%%%----------------------------------------------------------------------
%%% File    : ejabberd_db_serialize.erl
%%% Author  : Pawel Chmielowski <pawel@process-one.net>
%%% Purpose : DB data de/serialization
%%% Created : 20 Nov 2025 by Pawel Chmielowski <pawel@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module(ejabberd_db_serialize).

%% API
-export([export/2, export/3,
	 export_status/1,
	 export_abort/1,
	 import/2, import/3,
	 import_status/1,
	 import_abort/1]).
-export([iter_records/3, fmt/2, mnesia_iter/2]).


-callback serialize(binary(), non_neg_integer(), undefined | term()) ->
    {ok, [term()], term()} | {error, iolist()}.
-callback deserialize_start(binary()) -> ok | {error, iolist()}.
-callback deserialize(binary(), [term()]) -> ok | {error, iolist()}.

-define(BATCH_SIZE, 1000).

export(Host, Dir) ->
    export(Host, Dir, undefined).


export(Host0, Dir, undefined) ->
    Host = jid:nameprep(Host0),
    export(Host, Dir, [ejabberd_auth | gen_mod:loaded_modules(Host)]);
export(Host0, Dir, Mods) ->
    Host = jid:nameprep(Host0),
    ModsF = compatible_mods(Host, fun module_can_serialize/3, Mods),
    F = fun Job({[], _State}) ->
	MM = lists:map(fun({M, _}) -> atom_to_binary(M, latin1) end, ModsF),
	{ok, undefined,
	 0,
	 fmt(<<"Serialization finished for modules: ~s">>,
	     [lists:join(<<", ">>, MM)])};
	    Job({[{Mod2, DbMod2} | Rest] = Mods2, Key}) ->
		case write_batch(Host, Mod2, DbMod2, Dir, Key) of
		    {ok, undefined, _Count} ->
			Job({Rest, undefined});
		    {ok, NextKey, Count} ->
			{ok, {Mods2, NextKey},
			 Count,
			 <<"Serializing ", (atom_to_binary(Mod2))/binary>>};
		    {error, _} = E ->
			E
		end
	end,
    case ejabberd_batch:register_task({db_export, Host},
				      0,
				      infinity,
				      {ModsF, undefined},
				      F) of
	ok ->
	    {ok, <<"Export started">>};
	{error, in_progress} ->
	    {error, <<"Operation in progress">>}
    end.


export_status(Host0) ->
    Host = jid:nameprep(Host0),
    case ejabberd_batch:task_status({db_export, Host}) of
	not_started -> <<"Operation not started">>;
	{failed, Steps, Error} ->
	    fmt("Operation failed after exporting ~p records with error ~p",
		[Steps, misc:format_val(Error)]);
	{aborted, Steps, _} ->
	    fmt("Operation was aborted after exporting ~p records",
		[Steps]);
	{working, Steps, Msg} ->
	    fmt("Operation in progress: '~s', exported ~p records so far",
		[Msg, Steps]);
	{completed, Steps, Msg} ->
	    fmt("Operation was completed after exporting ~p records: '~s'",
		[Steps, Msg])
    end.


export_abort(Host0) ->
    Host = jid:nameprep(Host0),
    case ejabberd_batch:abort_task({db_export, Host}) of
	aborted -> <<"Operation aborted">>;
	not_started -> <<"No task running">>
    end.


import(Host0, Dir) ->
    import(Host0, Dir, undefined).


import(Host0, Dir, undefined) ->
    Host = jid:nameprep(Host0),
    import(Host, Dir, [ejabberd_auth | gen_mod:loaded_modules(Host)]);
import(Host0, Dir, Mods) ->
    Host = jid:nameprep(Host0),
    Serialized = get_serialized_mods(Host, Dir),
    ModsF = compatible_mods(Host, fun module_can_deserialize/3, Mods -- (Mods -- Serialized)),
    F = fun Job({[], _State}) ->
	MM = lists:map(fun({M, _}) -> atom_to_binary(M, latin1) end, ModsF),
	{ok, undefined,
	 0,
	 fmt(<<"Deserialization finished for modules: ~s">>,
	     [lists:join(<<", ">>, MM)])};
	    Job({[{Mod2, DbMod2} | Rest] = Mods2, Key}) ->
		case read_batch(Host, Mod2, DbMod2, Dir, Key) of
		    {ok, undefined, _Count} ->
			Job({Rest, undefined});
		    {ok, NextKey, Count} ->
			{ok, {Mods2, NextKey},
			 Count,
			 <<"Deserializing ", (atom_to_binary(Mod2))/binary>>};
		    {error, _} = E ->
			E
		end
	end,
    case ejabberd_batch:register_task({db_import, Host},
				      0,
				      infinity,
				      {ModsF, undefined},
				      F) of
	ok ->
	    {ok, <<"Import started">>};
	{error, in_progress} ->
	    {error, <<"Operation in progress">>}
    end.


import_status(Host0) ->
    Host = jid:nameprep(Host0),
    case ejabberd_batch:task_status({db_import, Host}) of
	not_started -> <<"Operation not started">>;
	{failed, Steps, Error} ->
	    fmt("Operation failed after importing ~p records with error ~p",
		[Steps, misc:format_val(Error)]);
	{aborted, Steps, _} ->
	    fmt("Operation was aborted after importing ~p records",
		[Steps]);
	{working, Steps, Msg} ->
	    fmt("Operation in progress: '~s', imported ~p records so far",
		[Msg, Steps]);
	{completed, Steps, Msg} ->
	    fmt("Operation was completed after importing ~p records: '~s'",
		[Steps, Msg])
    end.


import_abort(Host0) ->
    Host = jid:nameprep(Host0),
    case ejabberd_batch:abort_task({db_import, Host}) of
	aborted -> <<"Operation aborted">>;
	not_started -> <<"No task running">>
    end.


compatible_mods(Host, Fun, Mods) ->
    lists:filtermap(
	fun(ejabberd_auth) ->
	    lists:foldl(
		fun(Method, false) ->
		    DbMod = ejabberd:module_name([<<"auth">>, Method]),
		    case Fun(Host, ejabberd_auth, DbMod) of
			{true, _} -> {true, {ejabberd_auth, DbMod}};
			_ -> false
		    end;
		   (_, Mod) -> Mod
		end,
		false,
		ejabberd_option:auth_method(Host));
	   (Mod) ->
	       case Fun(Host, Mod, undefined) of
		   {true, DbMod} ->
		       {true, {Mod, DbMod}};
		   _ -> false
	       end
	end,
	Mods).


get_serialized_mods(Host, Dir) ->
    Wildcard = binary_to_list(filename:join([Dir, <<Host/binary, "_*.dbser">>])),
    lists:filtermap(
	fun(Path) ->
	    case string:prefix(filename:basename(Path, ".dbser"), <<Host/binary, "_">>) of
		nomatch -> false;
		Mod ->
		    try list_to_existing_atom(Mod) of
			Atom -> {true, Atom}
		    catch
			_:_ -> false
		    end
	    end
	end,
	filelib:wildcard(Wildcard)).


module_can_serialize(Host, Mod, DbMod) ->
    module_has_methods(Host, Mod, DbMod, [{serialize, 3}]).


module_can_deserialize(Host, Mod, DbMod) ->
    module_has_methods(Host, Mod, DbMod, [{deserialize, 2}, {deserialize_start, 1}]).


module_has_methods(Host, Mod, undefined, Methods) ->
    try {gen_mod:is_loaded(Host, Mod), gen_mod:db_mod(Host, Mod)} of
	{true, DbMod} ->
	    module_has_methods(Host, Mod, DbMod, Methods);
	_ ->
	    {error, not_loaded}
    catch
	_:_ -> false
    end;
module_has_methods(_Host, _Mod, DbMod, Methods) ->
    case lists:all(
	fun({Method, Arity}) ->
	    erlang:function_exported(DbMod, Method, Arity)
	end,
	Methods) of
	true ->
	    {true, DbMod};
	_ ->
	    false
    end.


write_batch(Host, Mod, DbMod, Dir, undefined) ->
    FN = <<Host/binary, "_", (atom_to_binary(Mod, latin1))/binary, ".dbser">>,
    Path = filename:join([Dir, FN]),
    case file:open(Path, [write, raw, binary]) of
	{ok, IO} ->
	    write_batch(Host, Mod, DbMod, Dir, {IO, Path, undefined});
	{error, Msg} ->
	    {error, iolist_to_binary(io_lib:format("Unable to open file ~s for write: ~p",
						   [Path, Msg]))}
    end;
write_batch(Host, _Mod, DbMod, _Dir, {IO, Path, Key}) ->
    case DbMod:serialize(Host, ?BATCH_SIZE, Key) of
	{ok, [], _} ->
	    file:close(IO),
	    {ok, undefined, 0};
	{ok, Data, NextKey} ->
	    Ser = term_to_binary(Data),
	    SerLen = byte_size(Ser),
	    maybe
		ok ?= file:write(IO, <<"dbdb", SerLen:32/unsigned-little-integer>>),
		ok ?= file:write(IO, Ser),
		{ok, {IO, Path, NextKey}, length(Data)}
	    else
		{error, Msg} ->
		    file:close(IO),
		    file:delete(Path),
		    {error, iolist_to_binary(io_lib:format("Error when writing to file ~s: ~p",
							   [Path, Msg]))}
	    end;
	{error, Error} ->
	    file:close(IO),
	    file:delete(Path),
	    {error, iolist_to_binary(Error)}
    end.


read_batch(Host, Mod, DbMod, Dir, undefined) ->
    FN = <<Host/binary, "_", (atom_to_binary(Mod, latin1))/binary, ".dbser">>,
    Path = filename:join([Dir, FN]),
    case file:open(Path, [read, raw, binary]) of
	{ok, IO} ->
	    DbMod:deserialize_start(Host),
	    read_batch(Host, Mod, DbMod, Dir, {IO, Path});
	{error, Msg} ->
	    {error, iolist_to_binary(io_lib:format("Unable to open file ~s for read: ~p",
						   [Path, Msg]))}
    end;
read_batch(Host, _Mod, DbMod, _Dir, {IO, Path}) ->
    case file:read(IO, 8) of
	{ok, <<"dbdb", Len:32/unsigned-little-integer>>} ->
	    case file:read(IO, Len) of
		{ok, Data} when byte_size(Data) == Len ->
		    try
			Decoded = erlang:binary_to_term(iolist_to_binary(Data)),
			case DbMod:deserialize(Host, Decoded) of
			    ok ->
				{ok, {IO, Path}, length(Decoded)};
			    Err -> Err
			end
		    catch
			E:M:S ->
			    {error, fmt("Unable to decode data from file ~s: ~p:~p:~p", [Path, E, M, S])}
		    end;
		{ok, _} ->
		    {error, fmt("File ~s is too short", [Path])};
		eof ->
		    {error, fmt("File ~s is too short", [Path])};
		{error, Msg} ->
		    {error, fmt("Error when reading file ~s: ~p", [Path, Msg])}
	    end;
	{ok, _} ->
	    {error, fmt("File ~s has wrong header", [Path])};
	eof ->
	    {ok, undefined, 0};
	{error, Msg} ->
	    {error, fmt("Error when reading file ~s: ~p", [Path, Msg])}
    end.


fmt(Msg, Args) ->
    iolist_to_binary(io_lib:format(Msg, Args)).


iter_records([], Output, _)  -> {ok, Output, fin};
iter_records(fin, Output, _) -> {ok, Output, fin};
iter_records(Key, Output, 0) -> {ok, Output, Key};
iter_records([{First, _, Process} | Rest] = Funs, Output, EntriesLeft) ->
    case First() of
	{ok, Value, NextKey} ->
	    case Process(Value) of
		{ok, Formated} ->
		    iter_records({Funs, NextKey}, [Formated | Output], EntriesLeft - 1);
		{error, Msg} ->
		    {error, Msg};
		skip ->
		    iter_records({Funs, NextKey}, Output, EntriesLeft)
	    end;
	fin ->
	    iter_records(Rest, Output, EntriesLeft);
	{error, Msg} ->
	    {error, Msg}
    end;
iter_records({[{_, Next, Process} | Rest] = Funs, Key}, Output, EntriesLeft) ->
    case Next(Key) of
	{ok, Value, NextKey} ->
	    case Process(Value) of
		{ok, Formated} ->
		    iter_records({Funs, NextKey}, [Formated | Output], EntriesLeft - 1);
		{error, Msg} ->
		    {error, Msg};
		skip ->
		    iter_records({Funs, NextKey}, Output, EntriesLeft)
	    end;
	fin ->
	    iter_records(Rest, Output, EntriesLeft);
	{error, Msg} ->
	    {error, Msg}
    end.

mnesia_iter(Tab, Conv) ->
    NV = fun ({key, '$end_of_table'}) -> fin;
	     ({key, Key}) ->
		 case mnesia:dirty_read(Tab, Key) of
		     [] ->
			 {ok, [], {key, mnesia:dirty_next(Tab, Key)}};
		     [Single] ->
			 {ok, [Single], {key, mnesia:dirty_next(Tab, Key)}};
		     [First | Rest] ->
			 {ok, [First], {bag, Rest, Key}}
		 end;
	     ({bag, [Only], Key}) ->
		 {ok, [Only], {key, mnesia:dirty_next(Tab, Key)}};
	     ({bag, [First | Rest], Key}) ->
		 {ok, [First], {bag, Rest, Key}}
	 end,
    {fun() ->
	NV({key, mnesia:dirty_first(Tab)})
     end,
     NV, Conv}.
