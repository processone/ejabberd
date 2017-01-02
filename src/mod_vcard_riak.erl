%%%-------------------------------------------------------------------
%%% File    : mod_vcard_riak.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(mod_vcard_riak).

-behaviour(mod_vcard).

%% API
-export([init/2, get_vcard/2, set_vcard/4, search/4, remove_user/2,
	 search_fields/1, search_reported/1, import/3, stop/1]).
-export([is_search_supported/1]).

-include("xmpp.hrl").
-include("mod_vcard.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

is_search_supported(_LServer) ->
    false.

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

import(LServer, <<"vcard">>, [LUser, XML, _TimeStamp]) ->
    El = fxml_stream:parse_element(XML),
    VCard = #vcard{us = {LUser, LServer}, vcard = El},
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
import(_LServer, <<"vcard_search">>, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
vcard_schema() ->
    {record_info(fields, vcard), #vcard{}}.
