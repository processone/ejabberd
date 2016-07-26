%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_vcard_mnesia).

-behaviour(mod_vcard).

%% API
-export([init/2, import/2, get_vcard/2, set_vcard/4, search/4, remove_user/2]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("mod_vcard.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    mnesia:create_table(vcard,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, vcard)}]),
    mnesia:create_table(vcard_search,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, vcard_search)}]),
    update_tables(),
    mnesia:add_table_index(vcard_search, luser),
    mnesia:add_table_index(vcard_search, lfn),
    mnesia:add_table_index(vcard_search, lfamily),
    mnesia:add_table_index(vcard_search, lgiven),
    mnesia:add_table_index(vcard_search, lmiddle),
    mnesia:add_table_index(vcard_search, lnickname),
    mnesia:add_table_index(vcard_search, lbday),
    mnesia:add_table_index(vcard_search, lctry),
    mnesia:add_table_index(vcard_search, llocality),
    mnesia:add_table_index(vcard_search, lemail),
    mnesia:add_table_index(vcard_search, lorgname),
    mnesia:add_table_index(vcard_search, lorgunit).

get_vcard(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () -> mnesia:read({vcard, US}) end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:map(fun (R) -> R#vcard.vcard end, Rs);
	{aborted, _Reason} -> error
    end.

set_vcard(LUser, LServer, VCARD, VCardSearch) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:write(#vcard{us = US, vcard = VCARD}),
		mnesia:write(VCardSearch)
	end,
    mnesia:transaction(F).

search(LServer, Data, AllowReturnAll, MaxMatch) ->
    MatchSpec = make_matchspec(LServer, Data),
    if (MatchSpec == #vcard_search{_ = '_'}) and
       not AllowReturnAll ->
	    [];
       true ->
	    case catch mnesia:dirty_select(vcard_search,
					   [{MatchSpec, [], ['$_']}]) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]), [];
		Rs ->
		    case MaxMatch of
			infinity ->
			    Rs;
			Val ->
			    lists:sublist(Rs, Val)
		    end
	    end
    end.

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:delete({vcard, US}),
		mnesia:delete({vcard_search, US})
	end,
    mnesia:transaction(F).

import(_LServer, #vcard{} = VCard) ->
    mnesia:dirty_write(VCard);
import(_LServer, #vcard_search{} = S) ->
    mnesia:dirty_write(S).

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_tables() ->
    update_vcard_table(),
    update_vcard_search_table().

update_vcard_table() ->
    Fields = record_info(fields, vcard),
    case mnesia:table_info(vcard, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            vcard, Fields, set,
            fun(#vcard{us = {U, _}}) -> U end,
            fun(#vcard{us = {U, S}, vcard = El} = R) ->
                    R#vcard{us = {iolist_to_binary(U),
                                  iolist_to_binary(S)},
                            vcard = fxml:to_xmlel(El)}
            end);
      _ ->
	  ?INFO_MSG("Recreating vcard table", []),
	  mnesia:transform_table(vcard, ignore, Fields)
    end.

update_vcard_search_table() ->
    Fields = record_info(fields, vcard_search),
    case mnesia:table_info(vcard_search, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            vcard_search, Fields, set,
            fun(#vcard_search{us = {U, _}}) -> U end,
            fun(#vcard_search{} = VS) ->
                    [vcard_search | L] = tuple_to_list(VS),
                    NewL = lists:map(
                             fun({U, S}) ->
                                     {iolist_to_binary(U),
                                      iolist_to_binary(S)};
                                (Str) ->
                                     iolist_to_binary(Str)
                             end, L),
                    list_to_tuple([vcard_search | NewL])
            end);
      _ ->
	  ?INFO_MSG("Recreating vcard_search table", []),
	  mnesia:transform_table(vcard_search, ignore, Fields)
    end.

make_matchspec(LServer, Data) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch, LServer),
    Match.

filter_fields([], Match, _LServer) ->
    Match;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_binary(Val) and (Val /= <<"">>) ->
    LVal = mod_vcard:string2lower(Val),
    NewMatch = case SVar of
		   <<"user">> ->
		       case gen_mod:get_module_opt(LServer, ?MODULE,
						   search_all_hosts,
						   fun(B) when is_boolean(B) ->
							   B
						   end, true) of
			   true -> Match#vcard_search{luser = make_val(LVal)};
			   false ->
			       Host = find_my_host(LServer),
			       Match#vcard_search{us = {make_val(LVal), Host}}
		       end;
		   <<"fn">> -> Match#vcard_search{lfn = make_val(LVal)};
		   <<"last">> ->
		       Match#vcard_search{lfamily = make_val(LVal)};
		   <<"first">> ->
		       Match#vcard_search{lgiven = make_val(LVal)};
		   <<"middle">> ->
		       Match#vcard_search{lmiddle = make_val(LVal)};
		   <<"nick">> ->
		       Match#vcard_search{lnickname = make_val(LVal)};
		   <<"bday">> ->
		       Match#vcard_search{lbday = make_val(LVal)};
		   <<"ctry">> ->
		       Match#vcard_search{lctry = make_val(LVal)};
		   <<"locality">> ->
		       Match#vcard_search{llocality = make_val(LVal)};
		   <<"email">> ->
		       Match#vcard_search{lemail = make_val(LVal)};
		   <<"orgname">> ->
		       Match#vcard_search{lorgname = make_val(LVal)};
		 <<"orgunit">> ->
		       Match#vcard_search{lorgunit = make_val(LVal)};
		   _ -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

make_val(Val) ->
    case str:suffix(<<"*">>, Val) of
      true -> [str:substr(Val, 1, byte_size(Val) - 1)] ++ '_';
      _ -> Val
    end.

find_my_host(LServer) ->
    Parts = str:tokens(LServer, <<".">>),
    find_my_host(Parts, ?MYHOSTS).

find_my_host([], _Hosts) -> ?MYNAME;
find_my_host([_ | Tail] = Parts, Hosts) ->
    Domain = parts_to_string(Parts),
    case lists:member(Domain, Hosts) of
      true -> Domain;
      false -> find_my_host(Tail, Hosts)
    end.

parts_to_string(Parts) ->
    str:strip(list_to_binary(
                lists:map(fun (S) -> <<S/binary, $.>> end, Parts)),
              right, $.).
