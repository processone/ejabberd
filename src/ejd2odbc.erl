%%%----------------------------------------------------------------------
%%% File    : ejd2odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Export some mnesia tables to SQL DB
%%% Created : 22 Aug 2005 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(ejd2odbc).

-author('alexey@process-one.net').

-include("logger.hrl").

-export([export/2, export/3, import_file/2, import/2,
	 import/3]).

-define(MAX_RECORDS_PER_TRANSACTION, 100).

-record(dump, {fd, cont = start}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%% How to use:
%%% A table can be converted from Mnesia to an ODBC database by calling
%%% one of the API function with the following parameters:
%%% - Server is the server domain you want to convert
%%% - Output can be either odbc to export to the configured relational
%%%   database or "Filename" to export to text file.

modules() ->
    [ejabberd_auth,
     mod_announce,
     mod_irc,
     mod_last,
     mod_muc,
     mod_offline,
     mod_privacy,
     mod_private,
     %% mod_pubsub,
     mod_roster,
     mod_shared_roster,
     mod_vcard,
     mod_vcard_xupdate].

export(Server, Output) ->
    LServer = jid:nameprep(iolist_to_binary(Server)),
    Modules = modules(),
    IO = prepare_output(Output),
    lists:foreach(
      fun(Module) ->
              export(LServer, IO, Module)
      end, Modules),
    close_output(Output, IO).

export(Server, Output, Module) ->
    LServer = jid:nameprep(iolist_to_binary(Server)),
    IO = prepare_output(Output),
    lists:foreach(
      fun({Table, ConvertFun}) ->
              export(LServer, Table, IO, ConvertFun)
      end, Module:export(Server)),
    close_output(Output, IO).

import_file(Server, FileName) when is_binary(FileName) ->
    import(Server, binary_to_list(FileName));
import_file(Server, FileName) ->
    case disk_log:open([{name, make_ref()},
                        {file, FileName},
                        {mode, read_only}]) of
        {ok, Fd} ->
            LServer = jid:nameprep(Server),
            Mods = [{Mod, gen_mod:db_type(LServer, Mod)}
                    || Mod <- modules(), gen_mod:is_loaded(LServer, Mod)],
            AuthMods = case lists:member(ejabberd_auth_internal,
                                         ejabberd_auth:auth_modules(LServer)) of
                           true ->
                               [{ejabberd_auth, mnesia}];
                           false ->
                               []
                       end,
            import_dump(LServer, AuthMods ++ Mods, #dump{fd = Fd});
        Err ->
            exit(Err)
    end.

import(Server, Output) ->
    import(Server, Output, [{fast, true}]).

import(Server, Output, Opts) ->
    LServer = jid:nameprep(iolist_to_binary(Server)),
    Modules = modules(),
    IO = prepare_output(Output, disk_log),
    lists:foreach(
      fun(Module) ->
              import(LServer, IO, Opts, Module)
      end, Modules),
    close_output(Output, IO).

import(Server, Output, Opts, Module) ->
    LServer = jid:nameprep(iolist_to_binary(Server)),
    IO = prepare_output(Output, disk_log),
    lists:foreach(
      fun({SelectQuery, ConvertFun}) ->
              import(LServer, SelectQuery, IO, ConvertFun, Opts)
      end, Module:import(Server)),
    close_output(Output, IO).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
export(LServer, Table, IO, ConvertFun) ->
    F = fun () ->
                mnesia:read_lock_table(Table),
                {_N, SQLs} =
                    mnesia:foldl(
                      fun(R, {N, SQLs} = Acc) ->
                              case ConvertFun(LServer, R) of
                                  [] ->
                                      Acc;
                                  SQL ->
                                      if N < (?MAX_RECORDS_PER_TRANSACTION) - 1 ->
                                              {N + 1, [SQL | SQLs]};
                                         true ->
                                              output(LServer,
                                                     Table, IO,
                                                     flatten([SQL | SQLs])),
                                              {0, []}
                                      end
                              end
                      end,
                      {0, []}, Table),
                output(LServer, Table, IO, flatten(SQLs))
        end,
    mnesia:transaction(F).

output(_LServer, _Table, _IO, []) ->
    ok;
output(LServer, _Table, odbc, SQLs) ->
    ejabberd_odbc:sql_transaction(LServer, SQLs);
output(_LServer, Table, Fd, SQLs) ->
    file:write(Fd, ["-- \n-- Mnesia table: ", atom_to_list(Table),
                    "\n--\n", SQLs]).

import(LServer, SelectQuery, IO, ConvertFun, Opts) ->
    F = case proplists:get_bool(fast, Opts) of
            true ->
                fun() ->
                        case ejabberd_odbc:sql_query_t(SelectQuery) of
                            {selected, _, Rows} ->
                                lists:foldl(fun process_sql_row/2,
                                            {IO, ConvertFun, undefined}, Rows);
                            Err ->
                                erlang:error(Err)
                        end
                end;
            false ->
                fun() ->
                        ejabberd_odbc:sql_query_t(
                          [iolist_to_binary(
                             [<<"declare c cursor for ">>, SelectQuery])]),
                        fetch(IO, ConvertFun, undefined)
                end
        end,
    ejabberd_odbc:sql_transaction(LServer, F).

fetch(IO, ConvertFun, PrevRow) ->
    case ejabberd_odbc:sql_query_t([<<"fetch c;">>]) of
        {selected, _, [Row]} ->
            process_sql_row(Row, {IO, ConvertFun, PrevRow}),
            fetch(IO, ConvertFun, Row);
        {selected, _, []} ->
            ok;
        Err ->
            erlang:error(Err)
    end.

process_sql_row(Row, {IO, ConvertFun, PrevRow}) when Row == PrevRow ->
    %% Avoid calling ConvertFun with the same input
    {IO, ConvertFun, Row};
process_sql_row(Row, {IO, ConvertFun, _PrevRow}) ->
    case catch ConvertFun(Row) of
        {'EXIT', _} = Err ->
            ?ERROR_MSG("failed to convert ~p: ~p", [Row, Err]);
        Term ->
            ok = disk_log:log(IO#dump.fd, Term)
    end,
    {IO, ConvertFun, Row}.

import_dump(LServer, Mods, #dump{fd = Fd, cont = Cont}) ->
    case disk_log:chunk(Fd, Cont) of
        {NewCont, Terms} ->
            import_terms(LServer, Mods, Terms),
            import_dump(LServer, Mods, #dump{fd = Fd, cont = NewCont});
        eof ->
            ok;
        Err ->
            exit(Err)
    end.

import_terms(LServer, Mods, [Term|Terms]) ->
    import_term(LServer, Mods, Term),
    import_terms(LServer, Mods, Terms);
import_terms(_LServer, _Mods, []) ->
    ok.

import_term(LServer, [{Mod, DBType}|Mods], Term) ->
    case catch Mod:import(LServer, DBType, Term) of
        pass -> import_term(LServer, Mods, Term);
        ok -> ok;
        Err ->
            ?ERROR_MSG("failed to import ~p for module ~p: ~p",
                       [Term, Mod, Err])
    end;
import_term(_LServer, [], _Term) ->
    ok.

prepare_output(FileName) ->
    prepare_output(FileName, normal).

prepare_output(FileName, Type) when is_binary(FileName) ->
    prepare_output(binary_to_list(FileName), Type);
prepare_output(FileName, normal) when is_list(FileName) ->
    case file:open(FileName, [write, raw]) of
        {ok, Fd} ->
            Fd;
        Err ->
            exit(Err)
    end;
prepare_output(FileName, disk_log) when is_list(FileName) ->
    case disk_log:open([{name, make_ref()},
                        {repair, truncate},
			{file, FileName}]) of
        {ok, Fd} ->
            #dump{fd = Fd};
        Err ->
            exit(Err)
    end;
prepare_output(Output, _Type) ->
    Output.

close_output(FileName, Fd) when FileName /= Fd ->
    case Fd of
        #dump{} ->
            disk_log:close(Fd#dump.fd);
        _ ->
            file:close(Fd)
    end,
    ok;
close_output(_, _) ->
    ok.

flatten(SQLs) ->
    flatten(SQLs, []).

flatten([L|Ls], Acc) ->
    flatten(Ls, flatten1(lists:reverse(L), Acc));
flatten([], Acc) ->
    Acc.

flatten1([H|T], Acc) ->
    flatten1(T, [[H, $\n]|Acc]);
flatten1([], Acc) ->
    Acc.
