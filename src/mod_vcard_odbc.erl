%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : vCard support via ODBC
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(mod_vcard_odbc).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, init/3, stop/1,
	 get_sm_features/5,
	 process_local_iq/3,
	 process_sm_iq/3,
	 %reindex_vcards/0,
	 remove_user/2]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").


-define(JUD_MATCHES, 30).
-define(PROCNAME, ejabberd_mod_vcard).

start(Host, Opts) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_VCARD,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_sm_features, HostB, ?MODULE, get_sm_features, 50),
    MyHost = gen_mod:get_opt_host(Host, Opts, "vjud.@HOST@"),
    Search = gen_mod:get_opt(search, Opts, true),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [MyHost, Host, Search])).


init(Host, ServerHost, Search) ->
    case Search of
	false ->
	    loop(Host, ServerHost);
	_ ->
	    ejabberd_router:register_route(Host),
	    loop(Host, ServerHost)
    end.

loop(Host, ServerHost) ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(ServerHost, From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop(Host, ServerHost);
	stop ->
	    ejabberd_router:unregister_route(Host),
	    ok;
	_ ->
	    loop(Host, ServerHost)
    end.

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, HostB, ?MODULE, get_sm_features, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! stop,
    {wait, Proc}.

get_sm_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_sm_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
	[] ->
	    case Acc of
		{result, Features} ->
		    {result, [?NS_VCARD_s | Features]};
		empty ->
		    {result, [?NS_VCARD_s]}
	    end;
 	_ ->
	    Acc
     end.

process_local_iq(_From, _To, #iq{type = get, lang = Lang} = IQ_Rec) ->
    Result = #xmlel{ns = ?NS_VCARD, name = 'vCard', children = [
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'FN'},
	  "ejabberd"),
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'URL'},
	  ?EJABBERD_URI),
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'DESC'},
	  translate:translate(Lang, "Erlang Jabber Server") ++
	  "\nCopyright (c) 2002-2008 ProcessOne"),
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'BDAY'},
	  "2002-11-16")
      ]},
    exmpp_iq:result(IQ_Rec, Result);
process_local_iq(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').


process_sm_iq(_From, To, #iq{type = get} = IQ_Rec) ->
    LUser = exmpp_jid:lnode_as_list(To),
    LServer = exmpp_jid:ldomain_as_list(To),
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_vcard(LServer, Username) of
        {selected, ["vcard"], [{SVCARD}]} ->
            try exmpp_xml:parse_document(SVCARD,
                         [names_as_atom, {check_elems, xmpp}, 
                          {check_nss,xmpp}, {check_attrs,xmpp}]) of
                [VCARD] ->
                    exmpp_iq:result(IQ_Rec, VCARD)
            catch
                _Type:_Error ->
                    ?ERROR_MSG("Error parsing vCard: ~s", [SVCARD]),
                    exmpp_iq:error(IQ_Rec, 'service-unavailable')
            end;
        {selected, ["vcard"], []} ->
            exmpp_iq:result(IQ_Rec);
        _ ->
            exmpp_iq:error(IQ_Rec, 'internal-server-error')
    end;
process_sm_iq(From, _To, #iq{type = set, payload = Request} = IQ_Rec) ->
    User = exmpp_jid:node_as_list(From),
    LServer = exmpp_jid:ldomain_as_list(From),
    case lists:member(LServer, ?MYHOSTS) of
        true ->
            set_vcard(User, LServer, Request),
            exmpp_iq:result(IQ_Rec);
        false ->
            exmpp_iq:error(IQ_Rec, 'not-allowed')
    end.

set_vcard(User, LServer, VCARD) ->
    FN       = exmpp_xml:get_path(VCARD,
      [{element, 'FN'},                         cdata_as_list]),
    Family   = exmpp_xml:get_path(VCARD,
      [{element, 'N'}, {element, 'FAMILY'},     cdata_as_list]),
    Given    = exmpp_xml:get_path(VCARD,
      [{element, 'N'}, {element, 'GIVEN'},      cdata_as_list]),
    Middle   = exmpp_xml:get_path(VCARD,
      [{element, 'N'}, {element, 'MIDDLE'},     cdata_as_list]),
    Nickname = exmpp_xml:get_path(VCARD,
      [{element, 'NICKNAME'},                   cdata_as_list]),
    BDay     = exmpp_xml:get_path(VCARD,
      [{element, 'BDAY'},                       cdata_as_list]),
    CTRY     = exmpp_xml:get_path(VCARD,
      [{element, 'ADR'}, {element, 'CTRY'},     cdata_as_list]),
    Locality = exmpp_xml:get_path(VCARD,
      [{element, 'ADR'}, {element, 'LOCALITY'}, cdata_as_list]),
    EMail1   = exmpp_xml:get_path(VCARD,
      [{element, 'EMAIL'}, {element, 'USERID'}, cdata_as_list]),
    EMail2   = exmpp_xml:get_path(VCARD,
      [{element, 'EMAIL'},                      cdata_as_list]),
    OrgName  = exmpp_xml:get_path(VCARD,
      [{element, 'ORG'}, {element, 'ORGNAME'},  cdata_as_list]),
    OrgUnit  = exmpp_xml:get_path(VCARD,
      [{element, 'ORG'}, {element, 'ORGUNIT'},  cdata_as_list]),
    EMail = case EMail1 of
		"" ->
		    EMail2;
		_ ->
		    EMail1
	    end,

    try
	LUser     = exmpp_stringprep:nodeprep(User),
	LFN       = exmpp_stringprep:to_lower(FN),
	LFamily   = exmpp_stringprep:to_lower(Family),
	LGiven    = exmpp_stringprep:to_lower(Given),
	LMiddle   = exmpp_stringprep:to_lower(Middle),
	LNickname = exmpp_stringprep:to_lower(Nickname),
	LBDay     = exmpp_stringprep:to_lower(BDay),
	LCTRY     = exmpp_stringprep:to_lower(CTRY),
	LLocality = exmpp_stringprep:to_lower(Locality),
	LEMail    = exmpp_stringprep:to_lower(EMail),
	LOrgName  = exmpp_stringprep:to_lower(OrgName),
	LOrgUnit  = exmpp_stringprep:to_lower(OrgUnit),

        Username = ejabberd_odbc:escape(User),
        LUsername = ejabberd_odbc:escape(LUser),
        SVCARD = ejabberd_odbc:escape(exmpp_xml:document_to_list(VCARD)),

        SFN = ejabberd_odbc:escape(FN),
        SLFN = ejabberd_odbc:escape(LFN),
        SFamily = ejabberd_odbc:escape(Family),
        SLFamily = ejabberd_odbc:escape(LFamily),
        SGiven = ejabberd_odbc:escape(Given),
        SLGiven = ejabberd_odbc:escape(LGiven),
        SMiddle = ejabberd_odbc:escape(Middle),
        SLMiddle = ejabberd_odbc:escape(LMiddle),
        SNickname = ejabberd_odbc:escape(Nickname),
        SLNickname = ejabberd_odbc:escape(LNickname),
        SBDay = ejabberd_odbc:escape(BDay),
        SLBDay = ejabberd_odbc:escape(LBDay),
        SCTRY = ejabberd_odbc:escape(CTRY),
        SLCTRY = ejabberd_odbc:escape(LCTRY),
        SLocality = ejabberd_odbc:escape(Locality),
        SLLocality = ejabberd_odbc:escape(LLocality),
        SEMail = ejabberd_odbc:escape(EMail),
        SLEMail = ejabberd_odbc:escape(LEMail),
        SOrgName = ejabberd_odbc:escape(OrgName),
        SLOrgName = ejabberd_odbc:escape(LOrgName),
        SOrgUnit = ejabberd_odbc:escape(OrgUnit),
        SLOrgUnit = ejabberd_odbc:escape(LOrgUnit),

        odbc_queries:set_vcard(LServer, LUsername, SBDay, SCTRY, SEMail,
                               SFN, SFamily, SGiven, SLBDay, SLCTRY,
                               SLEMail, SLFN, SLFamily, SLGiven,
                               SLLocality, SLMiddle, SLNickname,
                               SLOrgName, SLOrgUnit, SLocality,
                               SMiddle, SNickname, SOrgName,
                               SOrgUnit, SVCARD, Username)
    catch
	_ ->
	    {error, badarg}
    end.

-define(TLFIELD(Type, Label, Var),
	#xmlel{ns = ?NS_VCARD, name = 'field', attrs = [
	    #xmlattr{name = 'type', value = Type},
	    #xmlattr{name = 'label', value = list_to_binary(translate:translate(Lang, Label))},
	    #xmlattr{name = 'var', value = Var}]}).


-define(FORM(JID),
	[#xmlel{ns = ?NS_SEARCH, name = 'instructions', children =
	   [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "You need an x:data capable client to search"))}]},
	 #xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs =
	   [#xmlattr{name = 'type', value = <<"form">>}], children =
	   [#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	       [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "Search users in ") ++ exmpp_jid:jid_to_list(JID))}]},
	    #xmlel{ns = ?NS_SEARCH, name = 'instructions', children =
	       [#xmlcdata{cdata = list_to_binary(translate:translate(Lang,
		       "Fill in the form to search "
		       "for any matching Jabber User "
		       "(Add * to the end of field to "
		       "match substring)"))}]},
	   ?TLFIELD(<<"text-single">>, "User", <<"user">>),
	   ?TLFIELD(<<"text-single">>, "Full Name", <<"fn">>),
	   ?TLFIELD(<<"text-single">>, "Name", <<"first">>),
	   ?TLFIELD(<<"text-single">>, "Middle Name", <<"middle">>),
	   ?TLFIELD(<<"text-single">>, "Family Name", <<"last">>),
	   ?TLFIELD(<<"text-single">>, "Nickname", <<"nick">>),
	   ?TLFIELD(<<"text-single">>, "Birthday", <<"bday">>),
	   ?TLFIELD(<<"text-single">>, "Country", <<"ctry">>),
	   ?TLFIELD(<<"text-single">>, "City", <<"locality">>),
	   ?TLFIELD(<<"text-single">>, "Email", <<"email">>),
	   ?TLFIELD(<<"text-single">>, "Organization Name", <<"orgname">>),
	   ?TLFIELD(<<"text-single">>, "Organization Unit", <<"orgunit">>)
	  ]}]).

do_route(ServerHost, From, To, Packet) ->
    User = exmpp_jid:node(To),
    Resource = exmpp_jid:resource(To),
    if
	(User /= undefined) or (Resource /= undefined) ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'service-unavailable'),
	    ejabberd_router:route(To, From, Err);
	true ->
	    try
		Request = exmpp_iq:get_request(Packet),
		Type = exmpp_iq:get_type(Packet),
		Lang = exmpp_stanza:get_lang(Packet),
		case {Type, Request#xmlel.ns} of
		    {set, ?NS_SEARCH} ->
                        XDataEl = find_xdata_el(Request),
                        case XDataEl of
                            false ->
				Err = exmpp_iq:error(Packet, 'bad-request'),
                                ejabberd_router:route(To, From, Err);
                            _ ->
                                XData = jlib:parse_xdata_submit(XDataEl),
                                case XData of
                                    invalid ->
					Err = exmpp_iq:error(Packet,
					  'bad-request'),
                                        ejabberd_router:route(To, From,
                                                              Err);
                                    _ ->
					Result = #xmlel{
					  ns = ?NS_SEARCH,
					  name = 'query',
					  children = [
					    #xmlel{
					      ns = ?NS_DATA_FORMS,
					      name = 'x',
					      attrs = [#xmlattr{name = 'type',
						  value = <<"result">>}],
					      children = search_result(Lang,
						To, ServerHost, XData)}]},
					ResIQ = exmpp_iq:result(Packet,
					  Result),
                                        ejabberd_router:route(
                                          To, From, exmpp_iq:iq_to_xmlel(ResIQ))
                                end
                        end;
		    {get, ?NS_SEARCH} ->
			Result = #xmlel{ns = ?NS_SEARCH, name = 'query',
			  children = ?FORM(To)},
			ResIQ = exmpp_iq:result(Packet, Result),
                        ejabberd_router:route(To,
                                              From,
                                              ResIQ);
		    {set, ?NS_DISCO_INFO} ->
			Err = exmpp_iq:error(Packet, 'not-allowed'),
			ejabberd_router:route(To, From, Err);
		    {get, ?NS_DISCO_INFO} ->
			Result = #xmlel{ns = ?NS_DISCO_INFO, name = 'query',
			  children = [
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'identity',
			      attrs = [
				#xmlattr{name = 'category',
				  value = <<"directory">>},
				#xmlattr{name = 'type',
				  value = <<"user">>},
				#xmlattr{name = 'name',
				  value = list_to_binary(translate:translate(Lang,
				    "vCard User Search"))}]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				#xmlattr{name = 'var',
				  value = list_to_binary(?NS_SEARCH_s)}]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				#xmlattr{name = 'var',
				  value = list_to_binary(?NS_VCARD_s)}]}
			  ]},
			ResIQ = exmpp_iq:result(Packet, Result),
                        ejabberd_router:route(To,
                                              From,
                                              ResIQ);
		    {set, ?NS_DISCO_ITEMS} ->
			Err = exmpp_iq:error(Packet, 'not-allowed'),
			ejabberd_router:route(To, From, Err);
		    {get, ?NS_DISCO_ITEMS} ->
			Result = #xmlel{ns = ?NS_DISCO_ITEMS, name = 'query'},
			ResIQ = exmpp_iq:result(Packet, Result),
			ejabberd_router:route(To,
					      From,
					      ResIQ);
		    {get, ?NS_VCARD} ->
			Result = #xmlel{ns = ?NS_VCARD, name = 'vCard',
			  children = iq_get_vcard(Lang)},
			ResIQ = exmpp_iq:result(Packet, Result),
			ejabberd_router:route(To,
					      From,
					      ResIQ);
		    _ ->
			Err = exmpp_iq:error(Packet, 'service-unavailable'),
			ejabberd_router:route(To, From, Err)
                end
            catch
		_ ->
		    Err1 = exmpp_iq:error(Packet, 'service-unavailable'),
		    ejabberd_router:route(To, From, Err1)
	    end
    end.

iq_get_vcard(Lang) ->
    [
      #xmlel{ns = ?NS_SEARCH, name = 'FN', children = [
	  #xmlcdata{cdata = <<"ejabberd/mod_vcard">>}]},
      #xmlel{ns = ?NS_SEARCH, name = 'URL', children = [
	  #xmlcdata{cdata = list_to_binary(?EJABBERD_URI)}]},
      #xmlel{ns = ?NS_SEARCH, name ='DESC', children = [
	  #xmlcdata{cdata = list_to_binary(
	      translate:translate(Lang, "ejabberd vCard module") ++
	      "\nCopyright (c) 2003-2008 ProcessOne")}]}
    ].

find_xdata_el(#xmlel{children = SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) ->
    false;
find_xdata_el1([#xmlel{ns = ?NS_DATA_FORMS} = El | _Els]) ->
    El;
find_xdata_el1([_ | Els]) ->
    find_xdata_el1(Els).

search_result(Lang, JID, ServerHost, Data) ->
    [#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	[#xmlcdata{cdata = list_to_binary(
	      translate:translate(Lang, "Search Results for ") ++
	      exmpp_jid:jid_to_list(JID))}]},
     #xmlel{ns = ?NS_DATA_FORMS, name = 'reported', children =
      [?TLFIELD(<<"text-single">>, "Jabber ID", <<"jid">>),
       ?TLFIELD(<<"text-single">>, "Full Name", <<"fn">>),
       ?TLFIELD(<<"text-single">>, "Name", <<"first">>),
       ?TLFIELD(<<"text-single">>, "Middle Name", <<"middle">>),
       ?TLFIELD(<<"text-single">>, "Family Name", <<"last">>),
       ?TLFIELD(<<"text-single">>, "Nickname", <<"nick">>),
       ?TLFIELD(<<"text-single">>, "Birthday", <<"bday">>),
       ?TLFIELD(<<"text-single">>, "Country", <<"ctry">>),
       ?TLFIELD(<<"text-single">>, "City", <<"locality">>),
       ?TLFIELD(<<"text-single">>, "Email", <<"email">>),
       ?TLFIELD(<<"text-single">>, "Organization Name", <<"orgname">>),
       ?TLFIELD(<<"text-single">>, "Organization Unit", <<"orgunit">>)
      ]}] ++ lists:map(fun(R) -> record_to_item(ServerHost, R) end,
		       search(ServerHost, Data)).

-define(FIELD(Var, Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	  [#xmlattr{name = 'var', value = Var}], children =
	  [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
	      [#xmlcdata{cdata = Val}]}]}).


record_to_item(LServer, {Username, FN, Family, Given, Middle,
			 Nickname, BDay, CTRY, Locality,
			 EMail, OrgName, OrgUnit}) ->
    #xmlel{ns = ?NS_DATA_FORMS, name = 'item', children =
     [
       ?FIELD(<<"jid">>, list_to_binary(Username ++ "@" ++ LServer)),
       ?FIELD(<<"fn">>, list_to_binary(FN)),
       ?FIELD(<<"last">>, list_to_binary(Family)),
       ?FIELD(<<"first">>, list_to_binary(Given)),
       ?FIELD(<<"middle">>, list_to_binary(Middle)),
       ?FIELD(<<"nick">>, list_to_binary(Nickname)),
       ?FIELD(<<"bday">>, list_to_binary(BDay)),
       ?FIELD(<<"ctry">>, list_to_binary(CTRY)),
       ?FIELD(<<"locality">>, list_to_binary(Locality)),
       ?FIELD(<<"email">>, list_to_binary(EMail)),
       ?FIELD(<<"orgname">>, list_to_binary(OrgName)),
       ?FIELD(<<"orgunit">>, list_to_binary(OrgUnit))
      ]
     }.


search(LServer, Data) ->
    MatchSpec = make_matchspec(LServer, Data),
    AllowReturnAll = gen_mod:get_module_opt(LServer, ?MODULE,
					    allow_return_all, false),
    if
	(MatchSpec == "") and (not AllowReturnAll) ->
	    [];
	true ->
	    Limit = case gen_mod:get_module_opt(LServer, ?MODULE,
						matches, ?JUD_MATCHES) of
			infinity ->
			    "";
			Val when is_integer(Val) and (Val > 0) ->
			    [" LIMIT ", integer_to_list(Val)];
			Val ->
			    ?ERROR_MSG("Illegal option value ~p. "
				       "Default value ~p substituted.",
				       [{matches, Val}, ?JUD_MATCHES]),
			    [" LIMIT ", integer_to_list(?JUD_MATCHES)]
		    end,
	    case catch ejabberd_odbc:sql_query(
			 LServer,
			 ["select username, fn, family, given, middle, "
			  "       nickname, bday, ctry, locality, "
			  "       email, orgname, orgunit from vcard_search ",
			  MatchSpec, Limit, ";"]) of
		{selected, ["username", "fn", "family", "given", "middle",
			    "nickname", "bday", "ctry", "locality",
			    "email", "orgname", "orgunit"],
		 Rs} when is_list(Rs) ->
		    Rs;
		Error ->
		    ?ERROR_MSG("~p", [Error]),
		    []
	    end
    end.


make_matchspec(LServer, Data) ->
    filter_fields(Data, "", LServer).

filter_fields([], Match, _LServer) ->
    case Match of
	"" ->
	    "";
	_ ->
	    [" where ", Match]
    end;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_list(Val) and (Val /= "") ->
    LVal = exmpp_stringprep:to_lower(Val),
    NewMatch = case SVar of
                   "user"     -> make_val(Match, "lusername", LVal);
                   "fn"       -> make_val(Match, "lfn",       LVal);
                   "last"     -> make_val(Match, "lfamily",   LVal);
                   "first"    -> make_val(Match, "lgiven",    LVal);
                   "middle"   -> make_val(Match, "lmiddle",   LVal);
                   "nick"     -> make_val(Match, "lnickname", LVal);
                   "bday"     -> make_val(Match, "lbday",     LVal);
                   "ctry"     -> make_val(Match, "lctry",     LVal);
                   "locality" -> make_val(Match, "llocality", LVal);
                   "email"    -> make_val(Match, "lemail",    LVal);
                   "orgname"  -> make_val(Match, "lorgname",  LVal);
                   "orgunit"  -> make_val(Match, "lorgunit",  LVal);
		   _          -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

make_val(Match, Field, Val) ->
    Condition =
	case lists:suffix("*", Val) of
	    true ->
		Val1 = lists:sublist(Val, length(Val) - 1),
		SVal = ejabberd_odbc:escape_like(Val1) ++ "%",
		[Field, " LIKE '", SVal, "'"];
	    _ ->
		SVal = ejabberd_odbc:escape(Val),
		[Field, " = '", SVal, "'"]
	end,
    case Match of
	"" ->
	    Condition;
	_ ->
	    [Match, " and ", Condition]
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%set_vcard_t(R, _) ->
%    US = R#vcard.us,
%    User  = US,
%    VCARD = R#vcard.vcard,
%
%    FN       = xml:get_path_s(VCARD, [{elem, "FN"},                     cdata]),
%    Family   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "FAMILY"},    cdata]),
%    Given    = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "GIVEN"},     cdata]),
%    Middle   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "MIDDLE"},    cdata]),
%    Nickname = xml:get_path_s(VCARD, [{elem, "NICKNAME"},               cdata]),
%    BDay     = xml:get_path_s(VCARD, [{elem, "BDAY"},                   cdata]),
%    CTRY     = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "CTRY"},    cdata]),
%    Locality = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "LOCALITY"},cdata]),
%    EMail    = xml:get_path_s(VCARD, [{elem, "EMAIL"},                  cdata]),
%    OrgName  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGNAME"}, cdata]),
%    OrgUnit  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGUNIT"}, cdata]),
%
%    {LUser, _LServer} = US,
%    LFN       = stringprep:tolower(FN),
%    LFamily   = stringprep:tolower(Family),
%    LGiven    = stringprep:tolower(Given),
%    LMiddle   = stringprep:tolower(Middle),
%    LNickname = stringprep:tolower(Nickname),
%    LBDay     = stringprep:tolower(BDay),
%    LCTRY     = stringprep:tolower(CTRY),
%    LLocality = stringprep:tolower(Locality),
%    LEMail    = stringprep:tolower(EMail),
%    LOrgName  = stringprep:tolower(OrgName),
%    LOrgUnit  = stringprep:tolower(OrgUnit),
%
%    if
%	(LUser     == error) or
%	(LFN       == error) or
%	(LFamily   == error) or
%	(LGiven    == error) or
%	(LMiddle   == error) or
%	(LNickname == error) or
%	(LBDay     == error) or
%	(LCTRY     == error) or
%	(LLocality == error) or
%	(LEMail    == error) or
%	(LOrgName  == error) or
%	(LOrgUnit  == error) ->
%	    {error, badarg};
%	true ->
%	    mnesia:write(
%	      #vcard_search{us        = US,
%			    user      = User,     luser      = LUser,
%			    fn        = FN,       lfn        = LFN,
%			    family    = Family,   lfamily    = LFamily,
%			    given     = Given,    lgiven     = LGiven,
%			    middle    = Middle,   lmiddle    = LMiddle,
%			    nickname  = Nickname, lnickname  = LNickname,
%			    bday      = BDay,     lbday      = LBDay,
%			    ctry      = CTRY,     lctry      = LCTRY,
%			    locality  = Locality, llocality  = LLocality,
%			    email     = EMail,    lemail     = LEMail,
%			    orgname   = OrgName,  lorgname   = LOrgName,
%			    orgunit   = OrgUnit,  lorgunit   = LOrgUnit
%			   })
%    end.
%
%
%reindex_vcards() ->
%    F = fun() ->
%		mnesia:foldl(fun set_vcard_t/2, [], vcard)
%	end,
%    mnesia:transaction(F).


remove_user(User, Server) when is_binary(User), is_binary(server) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_transaction(
      LServer,
      [["delete from vcard where username='", Username, "';"],
       ["delete from vcard_search where lusername='", Username, "';"]]).
