%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_vcard_riak).

-behaviour(mod_vcard).

%% API
-export([init/2, get_vcard/2, set_vcard/4, search/4, remove_user/2,
	 search_fields/1, search_reported/1, import/2, stop/1]).

-include("xmpp.hrl").
-include("mod_vcard.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

get_vcard(LUser, LServer) ->
    case ejabberd_riak:get(vcard, vcard_schema(), {LUser, LServer}) of
        {ok, R} ->
            [R#vcard.vcard];
        {error, notfound} ->
            [];
        _ ->
            error
    end.

set_vcard(LUser, LServer, VCARD,
	  #vcard_search{user = {User, _},
			fn = FN,
			lfn = LFN,
			family = Family,
			lfamily = LFamily,
			given = Given,
			lgiven = LGiven,
			middle = Middle,
			lmiddle = LMiddle,
			nickname = Nickname,
			lnickname = LNickname,
			bday = BDay,
			lbday = LBDay,
			ctry = CTRY,
			lctry = LCTRY,
			locality = Locality,
			llocality = LLocality,
			email = EMail,
			lemail = LEMail,
			orgname = OrgName,
			lorgname = LOrgName,
			orgunit = OrgUnit,
			lorgunit = LOrgUnit}) ->
    US = {LUser, LServer},
    {atomic,
     ejabberd_riak:put(#vcard{us = US, vcard = VCARD},
		       vcard_schema(),
		       [{'2i', [{<<"user">>, User},
				{<<"luser">>, LUser},
				{<<"fn">>, FN},
				{<<"lfn">>, LFN},
				{<<"family">>, Family},
				{<<"lfamily">>, LFamily},
				{<<"given">>, Given},
				{<<"lgiven">>, LGiven},
				{<<"middle">>, Middle},
				{<<"lmiddle">>, LMiddle},
				{<<"nickname">>, Nickname},
				{<<"lnickname">>, LNickname},
				{<<"bday">>, BDay},
				{<<"lbday">>, LBDay},
				{<<"ctry">>, CTRY},
				{<<"lctry">>, LCTRY},
				{<<"locality">>, Locality},
				{<<"llocality">>, LLocality},
				{<<"email">>, EMail},
				{<<"lemail">>, LEMail},
				{<<"orgname">>, OrgName},
				{<<"lorgname">>, LOrgName},
				{<<"orgunit">>, OrgUnit},
				{<<"lorgunit">>, LOrgUnit}]}])}.

search(_LServer, _Data, _AllowReturnAll, _MaxMatch) ->
    [].

search_fields(_LServer) ->
    [].

search_reported(_LServer) ->
    [].

remove_user(LUser, LServer) ->
    {atomic, ejabberd_riak:delete(vcard, {LUser, LServer})}.

import(_LServer, #vcard{us = {LUser, LServer}, vcard = El} = VCard) ->
    #vcard_search{fn = FN,
		  lfn = LFN,
		  family = Family,
		  lfamily = LFamily,
		  given = Given,
		  lgiven = LGiven,
		  middle = Middle,
		  lmiddle = LMiddle,
		  nickname = Nickname,
		  lnickname = LNickname,
		  bday = BDay,
		  lbday = LBDay,
		  ctry = CTRY,
		  lctry = LCTRY,
		  locality = Locality,
		  llocality = LLocality,
		  email = EMail,
		  lemail = LEMail,
		  orgname = OrgName,
		  lorgname = LOrgName,
		  orgunit = OrgUnit,
		  lorgunit = LOrgUnit} =
	mod_vcard:make_vcard_search(LUser, LUser, LServer, El),
    ejabberd_riak:put(VCard, vcard_schema(),
                      [{'2i', [{<<"user">>, LUser},
                               {<<"luser">>, LUser},
                               {<<"fn">>, FN},
                               {<<"lfn">>, LFN},
                               {<<"family">>, Family},
                               {<<"lfamily">>, LFamily},
                               {<<"given">>, Given},
                               {<<"lgiven">>, LGiven},
                               {<<"middle">>, Middle},
                               {<<"lmiddle">>, LMiddle},
                               {<<"nickname">>, Nickname},
                               {<<"lnickname">>, LNickname},
                               {<<"bday">>, BDay},
                               {<<"lbday">>, LBDay},
                               {<<"ctry">>, CTRY},
                               {<<"lctry">>, LCTRY},
                               {<<"locality">>, Locality},
                               {<<"llocality">>, LLocality},
                               {<<"email">>, EMail},
                               {<<"lemail">>, LEMail},
                               {<<"orgname">>, OrgName},
                               {<<"lorgname">>, LOrgName},
                               {<<"orgunit">>, OrgUnit},
                               {<<"lorgunit">>, LOrgUnit}]}]);
import(_LServer, #vcard_search{}) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
vcard_schema() ->
    {record_info(fields, vcard), #vcard{}}.
