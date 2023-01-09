%%%-------------------------------------------------------------------
%%% File    : mod_vcard_mnesia.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-module(mod_vcard_mnesia).

-behaviour(mod_vcard).

%% API
-export([init/2, stop/1, import/3, get_vcard/2, set_vcard/4, search/4,
	 search_fields/1, search_reported/1, remove_user/2]).
-export([is_search_supported/1]).
-export([need_transform/1, transform/1]).
-export([mod_opt_type/1, mod_options/1, mod_doc/0]).

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_vcard.hrl").
-include("logger.hrl").
-include("translate.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, vcard,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, vcard)}]),
    ejabberd_mnesia:create(?MODULE, vcard_search,
			[{disc_copies, [node()]},
			 {attributes,
			  record_info(fields, vcard_search)},
			 {index, [ luser, lfn, lfamily,
				   lgiven, lmiddle, lnickname,
				   lbday, lctry, llocality,
				   lemail, lorgname, lorgunit
				 ]}]).

stop(_Host) ->
    ok.

is_search_supported(_ServerHost) ->
    true.

get_vcard(LUser, LServer) ->
    US = {LUser, LServer},
    Rs = mnesia:dirty_read(vcard, US),
    {ok, lists:map(fun (R) -> R#vcard.vcard end, Rs)}.

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
		    Fields = lists:map(fun record_to_item/1, Rs),
		    case MaxMatch of
			infinity ->
			    Fields;
			Val ->
			    lists:sublist(Fields, Val)
		    end
	    end
    end.

search_fields(_LServer) ->
    [{?T("User"), <<"user">>},
     {?T("Full Name"), <<"fn">>},
     {?T("Name"), <<"first">>},
     {?T("Middle Name"), <<"middle">>},
     {?T("Family Name"), <<"last">>},
     {?T("Nickname"), <<"nick">>},
     {?T("Birthday"), <<"bday">>},
     {?T("Country"), <<"ctry">>},
     {?T("City"), <<"locality">>},
     {?T("Email"), <<"email">>},
     {?T("Organization Name"), <<"orgname">>},
     {?T("Organization Unit"), <<"orgunit">>}].

search_reported(_LServer) ->
    [{?T("Jabber ID"), <<"jid">>},
     {?T("Full Name"), <<"fn">>},
     {?T("Name"), <<"first">>},
     {?T("Middle Name"), <<"middle">>},
     {?T("Family Name"), <<"last">>},
     {?T("Nickname"), <<"nick">>},
     {?T("Birthday"), <<"bday">>},
     {?T("Country"), <<"ctry">>},
     {?T("City"), <<"locality">>},
     {?T("Email"), <<"email">>},
     {?T("Organization Name"), <<"orgname">>},
     {?T("Organization Unit"), <<"orgunit">>}].

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:delete({vcard, US}),
		mnesia:delete({vcard_search, US})
	end,
    mnesia:transaction(F).

import(LServer, <<"vcard">>, [LUser, XML, _TimeStamp]) ->
    #xmlel{} = El = fxml_stream:parse_element(XML),
    VCard = #vcard{us = {LUser, LServer}, vcard = El},
    mnesia:dirty_write(VCard);
import(LServer, <<"vcard_search">>,
       [User, LUser, FN, LFN,
        Family, LFamily, Given, LGiven,
        Middle, LMiddle, Nickname, LNickname,
        BDay, LBDay, CTRY, LCTRY, Locality, LLocality,
        EMail, LEMail, OrgName, LOrgName, OrgUnit, LOrgUnit]) ->
    mnesia:dirty_write(
      #vcard_search{us = {LUser, LServer},
                    user = {User, LServer}, luser = LUser,
                    fn = FN, lfn = LFN, family = Family,
                    lfamily = LFamily, given = Given,
                    lgiven = LGiven, middle = Middle,
                    lmiddle = LMiddle, nickname = Nickname,
                    lnickname = LNickname, bday = BDay,
                    lbday = LBDay, ctry = CTRY, lctry = LCTRY,
                    locality = Locality, llocality = LLocality,
                    email = EMail, lemail = LEMail,
                    orgname = OrgName, lorgname = LOrgName,
                    orgunit = OrgUnit, lorgunit = LOrgUnit}).

need_transform({vcard, {U, S}, _}) when is_list(U) orelse is_list(S) ->
    ?INFO_MSG("Mnesia table 'vcard' will be converted to binary", []),
    true;
need_transform(R) when element(1, R) == vcard_search ->
    case element(2, R) of
	{U, S} when is_list(U) orelse is_list(S) ->
	    ?INFO_MSG("Mnesia table 'vcard_search' will be converted to binary", []),
	    true;
	_ ->
	    false
    end;
need_transform(_) ->
    false.

transform(#vcard{us = {U, S}, vcard = El} = R) ->
    R#vcard{us = {iolist_to_binary(U), iolist_to_binary(S)},
	    vcard = fxml:to_xmlel(El)};
transform(#vcard_search{} = VS) ->
    [vcard_search | L] = tuple_to_list(VS),
    NewL = lists:map(
	     fun({U, S}) ->
		     {iolist_to_binary(U), iolist_to_binary(S)};
		(Str) ->
		     iolist_to_binary(Str)
	     end, L),
    list_to_tuple([vcard_search | NewL]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
		       case mod_vcard_mnesia_opt:search_all_hosts(LServer) of
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
    find_my_host(Parts, ejabberd_option:hosts()).

find_my_host([], _Hosts) -> ejabberd_config:get_myname();
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

-spec record_to_item(#vcard_search{}) -> [{binary(), binary()}].
record_to_item(R) ->
    {User, Server} = R#vcard_search.user,
    [{<<"jid">>, <<User/binary, "@", Server/binary>>},
     {<<"fn">>, (R#vcard_search.fn)},
     {<<"last">>, (R#vcard_search.family)},
     {<<"first">>, (R#vcard_search.given)},
     {<<"middle">>, (R#vcard_search.middle)},
     {<<"nick">>, (R#vcard_search.nickname)},
     {<<"bday">>, (R#vcard_search.bday)},
     {<<"ctry">>, (R#vcard_search.ctry)},
     {<<"locality">>, (R#vcard_search.locality)},
     {<<"email">>, (R#vcard_search.email)},
     {<<"orgname">>, (R#vcard_search.orgname)},
     {<<"orgunit">>, (R#vcard_search.orgunit)}].

mod_opt_type(search_all_hosts) ->
    econf:bool().

mod_options(_) ->
    [{search_all_hosts, true}].

mod_doc() ->
    #{opts =>
          [{search_all_hosts,
            #{value => "true | false",
              desc =>
                  ?T("Whether to perform search on all "
                     "virtual hosts or not. The default "
                     "value is 'true'.")}}]}.
