%%%----------------------------------------------------------------------
%%% File    : ejabberd_db_serialize.erl
%%% Author  : Pawel Chmielowski <pawel@process-one.net>
%%% Purpose : DB data de/serialization
%%% Created : 20 Nov 2025 by Pawel Chmielowski <pawel@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-export([serialize_module/3, deserialize_module/3]).

-callback serialize(binary(), non_neg_integer(), undefined | term()) ->
    {ok, [term()], term()} | {error, iolist()}.
-callback deserialize_start(binary()) -> ok | {error, iolist()}.
-callback deserialize(binary(), [term()]) -> ok | {error, iolist()}.

-define(BATCH_SIZE, 1000).

serialize_module(Host, ejabberd_auth, Dir) ->
    DbMod2 =
	lists:foldl(
	    fun(Method, undefined) ->
		DbMod = ejabberd:module_name([<<"auth">>, Method]),
		case erlang:function_exported(DbMod, serialize, 3) of
		    true -> DbMod;
		    _ -> undefined
		end;
	       (_, Mod) -> Mod
	    end, undefined, ejabberd_option:auth_method(Host)),
    case DbMod2 of
	undefined ->
	    {error, iolist_to_binary(io_lib:format("No auth module offer serialization",
						   []))};
	_ ->
	    serialize_db_module(Host, ejabberd_auth, DbMod2, Dir)
    end;
serialize_module(Host, Mod, Dir) ->
    case gen_mod:is_loaded(Host, Mod) of
	true ->
	    serialize_db_module(Host, Mod, gen_mod:db_mod(Host, Mod), Dir);
	_ ->
	    {error, iolist_to_binary(io_lib:format("Module ~s is not loaded on host ~s",
						   [Mod, Host]))}
    end.

serialize_db_module(Host, Mod, DbMod, Dir) ->
    case erlang:function_exported(DbMod, serialize, 3) of
	true ->
	    FN = <<(atom_to_binary(Mod, latin1))/binary, ".dbser">>,
	    Path = filename:join([Dir, FN]),
	    write_data(Path, fun(Next) -> DbMod:serialize(Host, ?BATCH_SIZE, Next) end);
	_ ->
	    {error, iolist_to_binary(io_lib:format("Module ~s doesn't offer serialization",
						   [Mod]))}
    end.

deserialize_module(Host, ejabberd_auth, Dir) ->
    DbMod2 =
	lists:foldl(
	    fun(Method, undefined) ->
		DbMod = ejabberd:module_name([<<"auth">>, Method]),
		case erlang:function_exported(DbMod, deserialize, 2) andalso
		     erlang:function_exported(DbMod, deserialize_start, 1) of
		    true -> DbMod;
		    _ -> undefined
		end;
	       (_, Mod) -> Mod
	    end, undefined, ejabberd_option:auth_method(Host)),
    case DbMod2 of
	undefined ->
	    {error, iolist_to_binary(io_lib:format("No auth module offer serialization",
						   []))};
	_ ->
	    deserialize_db_module(Host, ejabberd_auth, DbMod2, Dir)
    end;
deserialize_module(Host, Mod, Dir) ->
    case gen_mod:is_loaded(Host, Mod) of
	true ->
	    deserialize_db_module(Host, Mod, gen_mod:db_mod(Host, Mod), Dir);
	_ ->
	    {error, iolist_to_binary(io_lib:format("Module ~s is not loaded on host ~s",
						   [Mod, Host]))}
    end.

deserialize_db_module(Host, Mod, DbMod, Dir) ->
    case erlang:function_exported(DbMod, deserialize, 2) andalso
	 erlang:function_exported(DbMod, deserialize_start, 1)
    of
	true ->
	    FN = <<(atom_to_binary(Mod, latin1))/binary, ".dbser">>,
	    Path = filename:join([Dir, FN]),
	    DbMod:deserialize_start(Host),
	    read_data(Path, fun(Chunk) -> DbMod:deserialize(Host, Chunk) end);
	_ ->
	    {error, iolist_to_binary(io_lib:format("Module ~s doesn't offer serialization",
						   [Mod]))}
    end.

write_loop(Path, Io, Producer, Key) ->
    case Producer(Key) of
	{ok, [], _} ->
	    ok;
	{ok, Data, NextKey} ->
	    Ser = term_to_binary(Data),
	    SerLen = byte_size(Ser),
	    maybe
		ok ?= file:write(Io, <<"dbdb", SerLen:32/unsigned-little-integer>>),
		ok ?= file:write(Io, Ser),
		write_loop(Path, Io, Producer, NextKey)
	    else
		{error, Msg} ->
		    {error, io_lib:format("Error when writing to file ~s: ~p",
					  [Path, Msg])}
	    end;
	Error -> Error
    end.

write_data(Path, Producer) ->
    case file:open(Path, [write, raw, binary]) of
	{ok, IO} ->
	    case write_loop(Path, IO, Producer, undefined) of
		ok ->
		    file:close(IO),
		    ok;
		{error, Msg} ->
		    file:close(IO),
		    file:delete(Path),
		    {error, iolist_to_binary(Msg)}
	    end;
	{error, Msg} ->
	    {error, iolist_to_binary(io_lib:format("Unable to open file ~s for write: ~p",
						   [Path, Msg]))}
    end.

read_loop(Path, Io, Consumer) ->
    case file:read(Io, 8) of
	{ok, <<"dbdb", Len:32/unsigned-little-integer>>} ->
	    case file:read(Io, Len) of
		{ok, Data} when byte_size(Data) == Len ->
		    try
			Decoded = erlang:binary_to_term(iolist_to_binary(Data)),
			case Consumer(Decoded) of
			    ok ->
				read_loop(Path, Io, Consumer);
			    Err -> Err
			end
		    catch
			_:_ ->
			    {error, io_lib:format("Unable to decode data from file ~s", [Path])}
		    end;
		{ok, _} ->
		    {error, io_lib:format("File ~s is too short", [Path])};
		eof ->
		    {error, io_lib:format("File ~s is too short", [Path])};
		{error, Msg} ->
		    {error, io_lib:format("Error when reading file ~s: ~p", [Path, Msg])}
	    end;
	{ok, _} ->
	    {error, io_lib:format("File ~s has wrong header", [Path])};
	eof ->
	    ok;
	{error, Msg} ->
	    {error, io_lib:format("Error when reading file ~s: ~p", [Path, Msg])}
    end.

read_data(Path, Consumer) ->
    case file:open(Path, [raw, read, binary]) of
	{ok, IO} ->
	    case read_loop(Path, IO, Consumer) of
		ok ->
		    file:close(IO),
		    ok;
		{error, Msg} ->
		    file:close(IO),
		    {error, iolist_to_binary(Msg)}
	    end;
	{error, Msg} ->
	    {error, iolist_to_binary(io_lib:format("Unable to open file ~s for write: ~p",
						   [Path, Msg]))}
    end.
