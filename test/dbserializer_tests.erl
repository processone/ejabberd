%%%-------------------------------------------------------------------
%%% Author  : Pawel Chmielowski <pawel@process-one.net>
%%% Created : 15 Oct 2026 by Pawel Chmielowski <pawel@process-one.net>
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

-module(dbserializer_tests).

%% API
-compile(export_all).
-include("suite.hrl").
-include_lib("xmpp/include/scram.hrl").

%%%===================================================================
%%% API
%%%===================================================================
single_cases() ->
    {dbserializer_single, [sequence],
     [single_test(auth),
      single_test(roster)]}.

single_test(T) ->
    list_to_atom("dbserializer_" ++ atom_to_list(T)).

%%%===================================================================
%%% Single user tests
%%%===================================================================
serialize_loop(Host, Mod, Count, Cont, Acc) ->
    case Mod:serialize(Host, Count, Cont) of
	{ok, [], _} ->
	    {ok, Acc};
	{ok, Data, Cont2} ->
	    serialize_loop(Host, Mod, Count, Cont2, Data ++ Acc);
	{error, _} = Err ->
	    Err
    end.

auth(Config) ->
    Host = ?config(server, Config),
    ejabberd_auth:try_register(<<"bob">>, Host, <<"pass">>),
    ejabberd_auth:try_register(<<"henry">>, Host, <<"pass">>),
    ejabberd_auth:set_password_instance(<<"henry">>, Host,
					ejabberd_auth:password_to_scram(Host, <<"pass">>)),
    export_import(Config, hd(ejabberd_auth:auth_modules(Host))).

auth_clean(Config) ->
    Host = ?config(server, Config),
    ejabberd_auth:remove_user(<<"bob">>, Host),
    ejabberd_auth:remove_user(<<"henry">>, Host).

roster(Config) ->
    Host = ?config(server, Config),
    Items = [#roster_item{jid = jid:make(<<"ben">>, Host),
			  name = <<"ben">>,
			  groups = [],
			  subscription = from,
			  ask = subscribe,
			  approved = true},
	     #roster_item{jid = jid:make(<<"stan">>, Host),
			  name = <<"stan">>,
			  groups = [<<"one">>, <<"two">>],
			  subscription = both,
			  approved = false}],
    mod_roster:set_items(<<"bob">>, Host, #roster_query{items = Items}),
    export_import(Config, gen_mod:db_mod(Host, mod_roster)).

roster_clean(Config) ->
    Host = ?config(server, Config),
    mod_roster:remove_user(<<"bob">>, Host).


export_import(Config, Mod) ->
    Host = ?config(server, Config),
    case erlang:function_exported(Mod, serialize, 3) of
	false ->
	    {skip, "Module doesn't support serialize"};
	_ ->
	    case serialize_loop(Host, Mod, 1, undefined, []) of
		{ok, Data} ->
		    ?match(true, length(Data) > 0),
		    json_output_working(Config, Data),
		    case erlang:function_exported(Mod, deserialize, 2) of
			false ->
			    Config;
			_ ->
			    ?match(ok, Mod:deserialize_start(Host)),
			    ?match({ok, []}, serialize_loop(Host, Mod, 1000, undefined, [])),
			    ?match(ok, Mod:deserialize(Host, Data)),
			    Data2 = ?match({ok, Data2},
					   serialize_loop(Host, Mod, 1000, undefined, []),
					   Data2),
			    DataS = deep_sort(Data),
			    Data2S = deep_sort(Data2),
			    ct:pal("Compare~n~p~n~p", [DataS, Data2S]),
			    ?match(DataS, Data2S),
			    Config
		    end
	    end
    end.

deep_sort(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([deep_sort(E) || E <- tuple_to_list(Tuple)]);
deep_sort(List) when is_list(List) ->
    lists:sort([deep_sort(E) || E <- List]);
deep_sort(Other) -> Other.

json_output_working(Config, Data) ->
    ct:pal("Will serialize ~p", [Data]),
    [ejabberd_db_serialize:to_json(D) || D <- Data],
    Config.

