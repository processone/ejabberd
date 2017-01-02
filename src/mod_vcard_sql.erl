%%%-------------------------------------------------------------------
%%% File    : mod_vcard_sql.erl
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

-module(mod_vcard_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_vcard).

%% API
-export([init/2, stop/1, get_vcard/2, set_vcard/4, search/4, remove_user/2,
	 search_fields/1, search_reported/1, import/3, export/1]).
-export([is_search_supported/1]).

-include("xmpp.hrl").
-include("mod_vcard.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

is_search_supported(_LServer) ->
    true.

get_vcard(LUser, LServer) ->
    case catch sql_queries:get_vcard(LServer, LUser) of
	{selected, [{SVCARD}]} ->
	    case fxml_stream:parse_element(SVCARD) of
		{error, _Reason} -> error;
		VCARD -> [VCARD]
	    end;
	{selected, []} -> [];
	_ -> error
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
    SVCARD = fxml:element_to_binary(VCARD),
    sql_queries:set_vcard(LServer, LUser, BDay, CTRY,
			   EMail, FN, Family, Given, LBDay,
			   LCTRY, LEMail, LFN, LFamily,
			   LGiven, LLocality, LMiddle,
			   LNickname, LOrgName, LOrgUnit,
			   Locality, Middle, Nickname, OrgName,
			   OrgUnit, SVCARD, User).

search(LServer, Data, AllowReturnAll, MaxMatch) ->
    MatchSpec = make_matchspec(LServer, Data),
    if (MatchSpec == <<"">>) and not AllowReturnAll -> [];
       true ->
	    Limit = case MaxMatch of
			infinity ->
			    <<"">>;
			Val ->
			    [<<" LIMIT ">>, integer_to_binary(Val)]
		    end,
	   case catch ejabberd_sql:sql_query(
			LServer,
			[<<"select username, fn, family, given, "
			   "middle,        nickname, bday, ctry, "
			   "locality,        email, orgname, orgunit "
			   "from vcard_search ">>,
			 MatchSpec, Limit, <<";">>]) of
	       {selected,
		[<<"username">>, <<"fn">>, <<"family">>, <<"given">>,
		 <<"middle">>, <<"nickname">>, <<"bday">>, <<"ctry">>,
		 <<"locality">>, <<"email">>, <<"orgname">>,
		 <<"orgunit">>], Rs} when is_list(Rs) ->
		   [row_to_item(LServer, R) || R <- Rs];
	       Error ->
		   ?ERROR_MSG("~p", [Error]), []
	   end
    end.

search_fields(_LServer) ->
    [{<<"User">>, <<"user">>},
     {<<"Full Name">>, <<"fn">>},
     {<<"Name">>, <<"first">>},
     {<<"Middle Name">>, <<"middle">>},
     {<<"Family Name">>, <<"last">>},
     {<<"Nickname">>, <<"nick">>},
     {<<"Birthday">>, <<"bday">>},
     {<<"Country">>, <<"ctry">>},
     {<<"City">>, <<"locality">>},
     {<<"Email">>, <<"email">>},
     {<<"Organization Name">>, <<"orgname">>},
     {<<"Organization Unit">>, <<"orgunit">>}].

search_reported(_LServer) ->
    [{<<"Jabber ID">>, <<"jid">>},
     {<<"Full Name">>, <<"fn">>},
     {<<"Name">>, <<"first">>},
     {<<"Middle Name">>, <<"middle">>},
     {<<"Family Name">>, <<"last">>},
     {<<"Nickname">>, <<"nick">>},
     {<<"Birthday">>, <<"bday">>},
     {<<"Country">>, <<"ctry">>},
     {<<"City">>, <<"locality">>},
     {<<"Email">>, <<"email">>},
     {<<"Organization Name">>, <<"orgname">>},
     {<<"Organization Unit">>, <<"orgunit">>}].

remove_user(LUser, LServer) ->
    ejabberd_sql:sql_transaction(
      LServer,
      fun() ->
              ejabberd_sql:sql_query_t(
                ?SQL("delete from vcard where username=%(LUser)s")),
              ejabberd_sql:sql_query_t(
                ?SQL("delete from vcard_search where lusername=%(LUser)s"))
      end).

export(_Server) ->   
    [{vcard,
      fun(Host, #vcard{us = {LUser, LServer}, vcard = VCARD})
            when LServer == Host ->
              SVCARD = fxml:element_to_binary(VCARD),
              [?SQL("delete from vcard where username=%(LUser)s;"),
               ?SQL("insert into vcard(username, vcard) values ("
                    "%(LUser)s, %(SVCARD)s);")];
         (_Host, _R) ->
              []
      end},
     {vcard_search,
      fun(Host, #vcard_search{user = {User, LServer}, luser = LUser,
                              fn = FN, lfn = LFN, family = Family,
                              lfamily = LFamily, given = Given,
                              lgiven = LGiven, middle = Middle,
                              lmiddle = LMiddle, nickname = Nickname,
                              lnickname = LNickname, bday = BDay,
                              lbday = LBDay, ctry = CTRY, lctry = LCTRY,
                              locality = Locality, llocality = LLocality,
                              email = EMail, lemail = LEMail,
                              orgname = OrgName, lorgname = LOrgName,
                              orgunit = OrgUnit, lorgunit = LOrgUnit})
            when LServer == Host ->
              [?SQL("delete from vcard_search where lusername=%(LUser)s;"),
               ?SQL("insert into vcard_search(username,"
                    " lusername, fn, lfn, family, lfamily,"
                    " given, lgiven, middle, lmiddle,"
                    " nickname, lnickname, bday, lbday,"
                    " ctry, lctry, locality, llocality,"
                    " email, lemail, orgname, lorgname,"
                    " orgunit, lorgunit) values ("
                    " %(LUser)s, %(User)s,"
                    " %(FN)s, %(LFN)s,"
                    " %(Family)s, %(LFamily)s,"
                    " %(Given)s, %(LGiven)s,"
                    " %(Middle)s, %(LMiddle)s,"
                    " %(Nickname)s, %(LNickname)s,"
                    " %(BDay)s, %(LBDay)s,"
                    " %(CTRY)s, %(LCTRY)s,"
                    " %(Locality)s, %(LLocality)s,"
                    " %(EMail)s, %(LEMail)s,"
                    " %(OrgName)s, %(LOrgName)s,"
                    " %(OrgUnit)s, %(LOrgUnit)s);")];
         (_Host, _R) ->
              []
      end}].

import(_, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_matchspec(LServer, Data) ->
    filter_fields(Data, <<"">>, LServer).

filter_fields([], Match, _LServer) ->
    case Match of
	<<"">> -> <<"">>;
	_ -> [<<" where ">>, Match]
    end;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_binary(Val) and (Val /= <<"">>) ->
    LVal = mod_vcard:string2lower(Val),
    NewMatch = case SVar of
		   <<"user">> -> make_val(Match, <<"lusername">>, LVal);
		   <<"fn">> -> make_val(Match, <<"lfn">>, LVal);
		   <<"last">> -> make_val(Match, <<"lfamily">>, LVal);
		   <<"first">> -> make_val(Match, <<"lgiven">>, LVal);
		   <<"middle">> -> make_val(Match, <<"lmiddle">>, LVal);
		   <<"nick">> -> make_val(Match, <<"lnickname">>, LVal);
		   <<"bday">> -> make_val(Match, <<"lbday">>, LVal);
		   <<"ctry">> -> make_val(Match, <<"lctry">>, LVal);
		   <<"locality">> ->
		       make_val(Match, <<"llocality">>, LVal);
		   <<"email">> -> make_val(Match, <<"lemail">>, LVal);
		   <<"orgname">> -> make_val(Match, <<"lorgname">>, LVal);
		   <<"orgunit">> -> make_val(Match, <<"lorgunit">>, LVal);
		   _ -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

make_val(Match, Field, Val) ->
    Condition = case str:suffix(<<"*">>, Val) of
		  true ->
		      Val1 = str:substr(Val, 1, byte_size(Val) - 1),
		      SVal = <<(ejabberd_sql:escape(
                                  ejabberd_sql:escape_like_arg_circumflex(
                                    Val1)))/binary,
			       "%">>,
		      [Field, <<" LIKE '">>, SVal, <<"' ESCAPE '^'">>];
		  _ ->
		      SVal = ejabberd_sql:escape(Val),
		      [Field, <<" = '">>, SVal, <<"'">>]
		end,
    case Match of
      <<"">> -> Condition;
      _ -> [Match, <<" and ">>, Condition]
    end.

row_to_item(LServer, [Username, FN, Family, Given, Middle, Nickname, BDay,
		      CTRY, Locality, EMail, OrgName, OrgUnit]) ->
    [{<<"jid">>, <<Username/binary, $@, LServer/binary>>},
     {<<"fn">>, FN},
     {<<"last">>, Family},
     {<<"first">>, Given},
     {<<"middle">>, Middle},
     {<<"nick">>, Nickname},
     {<<"bday">>, BDay},
     {<<"ctry">>, CTRY},
     {<<"locality">>, Locality},
     {<<"email">>, EMail},
     {<<"orgname">>, OrgName},
     {<<"orgunit">>, OrgUnit}].
