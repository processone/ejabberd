%%%-------------------------------------------------------------------
%%% File    : mod_vcard_sql.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_vcard_sql).


-behaviour(mod_vcard).

%% API
-export([init/2, stop/1, get_vcard/2, set_vcard/4, search/4, remove_user/2,
	 search_fields/1, search_reported/1, import/3, export/1]).
-export([is_search_supported/1]).
-export([sql_schemas/0]).

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_vcard.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").
-include("translate.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(Host, _Opts) ->
    ejabberd_sql_schema:update_schema(Host, ?MODULE, sql_schemas()),
    ok.

sql_schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"vcard">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"vcard">>, type = {text, big}},
                     #sql_column{name = <<"created_at">>, type = timestamp,
                                 default = true}],
                indices = [#sql_index{
                              columns = [<<"server_host">>, <<"username">>],
                              unique = true}]},
             #sql_table{
                name = <<"vcard_search">>,
                columns =
                    [#sql_column{name = <<"username">>, type = text},
                     #sql_column{name = <<"lusername">>, type = text},
                     #sql_column{name = <<"server_host">>, type = text},
                     #sql_column{name = <<"fn">>, type = text},
                     #sql_column{name = <<"lfn">>, type = text},
                     #sql_column{name = <<"family">>, type = text},
                     #sql_column{name = <<"lfamily">>, type = text},
                     #sql_column{name = <<"given">>, type = text},
                     #sql_column{name = <<"lgiven">>, type = text},
                     #sql_column{name = <<"middle">>, type = text},
                     #sql_column{name = <<"lmiddle">>, type = text},
                     #sql_column{name = <<"nickname">>, type = text},
                     #sql_column{name = <<"lnickname">>, type = text},
                     #sql_column{name = <<"bday">>, type = text},
                     #sql_column{name = <<"lbday">>, type = text},
                     #sql_column{name = <<"ctry">>, type = text},
                     #sql_column{name = <<"lctry">>, type = text},
                     #sql_column{name = <<"locality">>, type = text},
                     #sql_column{name = <<"llocality">>, type = text},
                     #sql_column{name = <<"email">>, type = text},
                     #sql_column{name = <<"lemail">>, type = text},
                     #sql_column{name = <<"orgname">>, type = text},
                     #sql_column{name = <<"lorgname">>, type = text},
                     #sql_column{name = <<"orgunit">>, type = text},
                     #sql_column{name = <<"lorgunit">>, type = text}],
                indices = [#sql_index{
                              columns = [<<"server_host">>, <<"lusername">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lfn">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lfamily">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lgiven">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lmiddle">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lnickname">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lbday">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lctry">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"llocality">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lemail">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lorgname">>]},
                           #sql_index{
                              columns = [<<"server_host">>, <<"lorgunit">>]}]}]}].

stop(_Host) ->
    ok.

is_search_supported(_LServer) ->
    true.

get_vcard(LUser, LServer) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(vcard)s from vcard"
                " where username=%(LUser)s and %(LServer)H")) of
	{selected, [{SVCARD}]} ->
	    case fxml_stream:parse_element(SVCARD) of
		{error, _Reason} -> error;
		VCARD -> {ok, [VCARD]}
	    end;
	{selected, []} -> {ok, []};
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
    ejabberd_sql:sql_transaction(
      LServer,
      fun() ->
              ?SQL_UPSERT(LServer, "vcard",
                          ["!username=%(LUser)s",
                           "!server_host=%(LServer)s",
                           "vcard=%(SVCARD)s"]),
              ?SQL_UPSERT(LServer, "vcard_search",
                          ["username=%(User)s",
                           "!lusername=%(LUser)s",
                           "!server_host=%(LServer)s",
                           "fn=%(FN)s",
                           "lfn=%(LFN)s",
                           "family=%(Family)s",
                           "lfamily=%(LFamily)s",
                           "given=%(Given)s",
                           "lgiven=%(LGiven)s",
                           "middle=%(Middle)s",
                           "lmiddle=%(LMiddle)s",
                           "nickname=%(Nickname)s",
                           "lnickname=%(LNickname)s",
                           "bday=%(BDay)s",
                           "lbday=%(LBDay)s",
                           "ctry=%(CTRY)s",
                           "lctry=%(LCTRY)s",
                           "locality=%(Locality)s",
                           "llocality=%(LLocality)s",
                           "email=%(EMail)s",
                           "lemail=%(LEMail)s",
                           "orgname=%(OrgName)s",
                           "lorgname=%(LOrgName)s",
                           "orgunit=%(OrgUnit)s",
                           "lorgunit=%(LOrgUnit)s"])
      end).

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
    ejabberd_sql:sql_transaction(
      LServer,
      fun() ->
              ejabberd_sql:sql_query_t(
                ?SQL("delete from vcard"
                     " where username=%(LUser)s and %(LServer)H")),
              ejabberd_sql:sql_query_t(
                ?SQL("delete from vcard_search"
                     " where lusername=%(LUser)s and %(LServer)H"))
      end).

export(_Server) ->
    [{vcard,
      fun(Host, #vcard{us = {LUser, LServer}, vcard = VCARD})
            when LServer == Host ->
              SVCARD = fxml:element_to_binary(VCARD),
              [?SQL("delete from vcard"
                    " where username=%(LUser)s and %(LServer)H;"),
               ?SQL_INSERT("vcard",
                           ["username=%(LUser)s",
                            "server_host=%(LServer)s",
                            "vcard=%(SVCARD)s"])];
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
              [?SQL("delete from vcard_search"
                    " where lusername=%(LUser)s and %(LServer)H;"),
               ?SQL_INSERT("vcard_search",
                           ["username=%(User)s",
                            "lusername=%(LUser)s",
                            "server_host=%(LServer)s",
                            "fn=%(FN)s",
                            "lfn=%(LFN)s",
                            "family=%(Family)s",
                            "lfamily=%(LFamily)s",
                            "given=%(Given)s",
                            "lgiven=%(LGiven)s",
                            "middle=%(Middle)s",
                            "lmiddle=%(LMiddle)s",
                            "nickname=%(Nickname)s",
                            "lnickname=%(LNickname)s",
                            "bday=%(BDay)s",
                            "lbday=%(LBDay)s",
                            "ctry=%(CTRY)s",
                            "lctry=%(LCTRY)s",
                            "locality=%(Locality)s",
                            "llocality=%(LLocality)s",
                            "email=%(EMail)s",
                            "lemail=%(LEMail)s",
                            "orgname=%(OrgName)s",
                            "lorgname=%(LOrgName)s",
                            "orgunit=%(OrgUnit)s",
                            "lorgunit=%(LOrgUnit)s"])];
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

filter_fields([], Match, LServer) ->
    case ejabberd_sql:use_multihost_schema() of
        true ->
            SQLType = ejabberd_option:sql_type(LServer),
            SServer = ejabberd_sql:to_string_literal(SQLType, LServer),
            case Match of
                <<"">> -> [<<"where server_host=">>, SServer];
                _ -> [<<" where server_host=">>, SServer, <<" and ">>, Match]
            end;
        false ->
            case Match of
                <<"">> -> <<"">>;
                _ -> [<<" where ">>, Match]
            end
    end;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_binary(Val) and (Val /= <<"">>) ->
    LVal = mod_vcard:string2lower(Val),
    NewMatch = case SVar of
		   <<"user">> -> make_val(LServer, Match, <<"lusername">>, LVal);
		   <<"fn">> -> make_val(LServer, Match, <<"lfn">>, LVal);
		   <<"last">> -> make_val(LServer, Match, <<"lfamily">>, LVal);
		   <<"first">> -> make_val(LServer, Match, <<"lgiven">>, LVal);
		   <<"middle">> -> make_val(LServer, Match, <<"lmiddle">>, LVal);
		   <<"nick">> -> make_val(LServer, Match, <<"lnickname">>, LVal);
		   <<"bday">> -> make_val(LServer, Match, <<"lbday">>, LVal);
		   <<"ctry">> -> make_val(LServer, Match, <<"lctry">>, LVal);
		   <<"locality">> ->
		       make_val(LServer, Match, <<"llocality">>, LVal);
		   <<"email">> -> make_val(LServer, Match, <<"lemail">>, LVal);
		   <<"orgname">> -> make_val(LServer, Match, <<"lorgname">>, LVal);
		   <<"orgunit">> -> make_val(LServer, Match, <<"lorgunit">>, LVal);
		   _ -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

make_val(LServer, Match, Field, Val) ->
    Condition = case str:suffix(<<"*">>, Val) of
		  true ->
		      Val1 = str:substr(Val, 1, byte_size(Val) - 1),
		      SVal = <<(ejabberd_sql:escape(
                                  ejabberd_sql:escape_like_arg_circumflex(
                                    Val1)))/binary,
			       "%">>,
		      [Field, <<" LIKE '">>, SVal, <<"' ESCAPE '^'">>];
		  _ ->
                      SQLType = ejabberd_option:sql_type(LServer),
		      SVal = ejabberd_sql:to_string_literal(SQLType, Val),
		      [Field, <<" = ">>, SVal]
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
