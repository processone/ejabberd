%%%----------------------------------------------------------------------
%%% File    : ejd2odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Export some mnesia tables to SQL DB
%%% Created : 22 Aug 2005 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejd2odbc).

-author('alexey@process-one.net').

-export([export/2, export/3]).

-define(MAX_RECORDS_PER_TRANSACTION, 100).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%% How to use:
%%% A table can be converted from Mnesia to an ODBC database by calling
%%% one of the API function with the following parameters:
%%% - Server is the server domain you want to convert
%%% - Output can be either odbc to export to the configured relational
%%%   database or "Filename" to export to text file.
export(Server, Output) ->
    LServer = jlib:nameprep(iolist_to_binary(Server)),
    Modules = [ejabberd_auth,
               mod_announce,
               mod_caps,
               mod_irc,
               mod_last,
               mod_muc,
               mod_offline,
               mod_privacy,
               mod_private,
               mod_roster,
               mod_shared_roster,
               mod_vcard,
               mod_vcard_xupdate],
    IO = prepare_output(Output),
    lists:foreach(
      fun(Module) ->
              export(LServer, IO, Module)
      end, Modules),
    close_output(Output, IO).

export(Server, Output, Module) ->
    LServer = jlib:nameprep(iolist_to_binary(Server)),
    IO = prepare_output(Output),
    lists:foreach(
      fun({Table, ConvertFun}) ->
              export(LServer, Table, IO, ConvertFun)
      end, Module:export(Server)),
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

prepare_output(FileName) when is_list(FileName); is_binary(FileName) ->
    case file:open(FileName, [write, raw]) of
        {ok, Fd} ->
            Fd;
        Err ->
            exit(Err)
    end;
prepare_output(Output) ->
    Output.

close_output(FileName, Fd) when FileName /= Fd ->
    file:close(Fd),
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
