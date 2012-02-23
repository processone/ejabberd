%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Vcard management in Mnesia
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

%%% Database schema (version / storage / table)
%%%
%%% 2.1.x / mnesia / vcard
%%%  us = {Username::string(), Host::string()}
%%%  vcard = xmlelement()
%%% 2.1.x / mnesia / vcard_search
%%%  us = {Username::string(), Host::string()}
%%%  user = {Username::string(), Host::string()}
%%%  luser = Username::string()
%%%  fn = string()
%%%  ... = string()
%%%
%%% 2.1.x / odbc / vcard
%%%  username = varchar250
%%%  vcard = text
%%%
%%% 3.0.0-prealpha / mnesia / vcard
%%%  us = {Username::string(), Host::string()}
%%%  vcard = xmlel()
%%%
%%% 3.0.0-prealpha / odbc / vcard
%%%  Same as 2.1.x
%%%
%%% 3.0.0-alpha / mnesia / vcard
%%%  user_host = {Username::binary(), Host::binary()}
%%%  vcard = xmlel()
%%% 3.0.0-alpha / mnesia / vcard_search
%%%  user_host = {Username::binary(), Host::binary()}
%%%  username = string()
%%%  lusername = string()
%%%  fn = string()
%%%  ... = string()
%%%
%%% 3.0.0-alpha / odbc / vcard
%%%  user = varchar150
%%%  host = varchar150
%%%  vcard = text

-module(mod_vcard).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, init/3, stop/1,
	 get_sm_features/5,
	 process_local_iq/3,
	 process_sm_iq/3,
	 reindex_vcards/0,
	 webadmin_page/3,
	 webadmin_user/4,
	 webadmin_user_parse_query/5,
	 remove_user/2]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").


-define(JUD_MATCHES, 30).

-record(vcard_search, {user_host,
		       username, lusername,
		       fn,	 lfn,
		       family,	 lfamily,
		       given,	 lgiven,
		       middle,	 lmiddle,
		       nickname, lnickname,
		       bday,	 lbday,
		       ctry,	 lctry,
		       locality, llocality,
		       email,	 lemail,
		       orgname,	 lorgname,
		       orgunit,	 lorgunit
		      }).
-record(vcard, {user_host, vcard}).

-define(PROCNAME, ejabberd_mod_vcard).

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    gen_storage:create_table(Backend, HostB, vcard,
			     [{disc_only_copies, [node()]},
			      {odbc_host, HostB},
			      {attributes, record_info(fields, vcard)},
			      {types, [{user_host, {text, text}}]}]),
    gen_storage:create_table(Backend, HostB, vcard_search,
			     [{disc_copies, [node()]},
			      {odbc_host, HostB},
			      {attributes, record_info(fields, vcard_search)},
			      {types, [{user_host, {text, text}}]}]),
    update_tables(HostB, Backend),
    gen_storage:add_table_index(HostB, vcard_search, lusername),
    gen_storage:add_table_index(HostB, vcard_search, lfn),
    gen_storage:add_table_index(HostB, vcard_search, lfamily),
    gen_storage:add_table_index(HostB, vcard_search, lgiven),
    gen_storage:add_table_index(HostB, vcard_search, lmiddle),
    gen_storage:add_table_index(HostB, vcard_search, lnickname),
    gen_storage:add_table_index(HostB, vcard_search, lbday),
    gen_storage:add_table_index(HostB, vcard_search, lctry),
    gen_storage:add_table_index(HostB, vcard_search, llocality),
    gen_storage:add_table_index(HostB, vcard_search, lemail),
    gen_storage:add_table_index(HostB, vcard_search, lorgname),
    gen_storage:add_table_index(HostB, vcard_search, lorgunit),

    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(webadmin_page_host, HostB,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, HostB,
		       ?MODULE, webadmin_user, 50),
    ejabberd_hooks:add(webadmin_user_parse_query, HostB,
                       ?MODULE, webadmin_user_parse_query, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_VCARD,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_sm_features, HostB, ?MODULE, get_sm_features, 50),
    MyHost = gen_mod:get_opt_host(HostB, Opts, "vjud.@HOST@"),
    Search = gen_mod:get_opt(search, Opts, true),
    register(gen_mod:get_module_proc(HostB, ?PROCNAME),
	     spawn(?MODULE, init, [MyHost, HostB, Search])).


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
    ejabberd_hooks:delete(anonymous_purge_hook, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(webadmin_page_host, HostB,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, HostB,
			  ?MODULE, webadmin_user, 50),
    ejabberd_hooks:delete(webadmin_user_parse_query, HostB,
                          ?MODULE, webadmin_user_parse_query, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB,
      ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB,
      ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, HostB, ?MODULE, get_sm_features, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! stop,
    {wait, Proc}.

get_sm_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
 
get_sm_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
	<<>> ->
	    case Acc of
		{result, Features} ->
		    {result, [?NS_DISCO_INFO_s, ?NS_VCARD_s | Features]};
		empty ->
		    {result, [?NS_DISCO_INFO_s, ?NS_VCARD_s]}
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
	  "\nCopyright (c) 2002-2012 ProcessOne"),
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'BDAY'},
	  "2002-11-16")
      ]},
    exmpp_iq:result(IQ_Rec, Result);
process_local_iq(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').


process_sm_iq(_From, To, #iq{type = get} = IQ_Rec) ->
    LUser = exmpp_jid:prep_node(To),
    LServer = exmpp_jid:prep_domain(To),
    case get_vcard(LUser, LServer) of
	{vcard, VCard} ->
	    exmpp_iq:result(IQ_Rec, VCard);
	novcard ->
	    exmpp_iq:result(IQ_Rec)
    end;
process_sm_iq(From, _To, #iq{type = set, payload = Request} = IQ_Rec) ->
    User = exmpp_jid:node(From),
    Server = exmpp_jid:prep_domain(From),
    ServerS = exmpp_jid:prep_domain_as_list(From),
    case ?IS_MY_HOST(ServerS) of
	true ->
	    set_vcard(User, Server, Request),
	    exmpp_iq:result(IQ_Rec);
	false ->
	    exmpp_iq:error(IQ_Rec, 'not-allowed')
    end.

%% @spec (User::binary(), Host::binary()) -> {vcard, xmlel()} | novcard
get_vcard(User, Host) ->
    US = {User, Host},
    case gen_storage:dirty_read(Host, {vcard, US}) of
		%% The vcard is stored as xmlel() in Mnesia:
		[#vcard{vcard = VCARD}] when is_tuple(VCARD) ->
		    {vcard, VCARD};
		%% The vcard is stored as a text string in ODBC:
		[#vcard{vcard = SVCARD}] when is_list(SVCARD) ->
		    [VCARD] = exmpp_xml:parse_document(SVCARD, [names_as_atom]),
		    {vcard, VCARD};
		[] ->
		    novcard
    end.


set_vcard(User, Server, VCARD) ->
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
	UserStr   = binary_to_list(User),
	LUser     = binary_to_list(exmpp_stringprep:nodeprep(User)),
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

	US = {User, Server},

	VcardToStore = case gen_storage:table_info(Server, vcard, backend) of
	    mnesia -> VCARD;
	    odbc -> lists:flatten(exmpp_xml:document_to_list(VCARD))
	end,

	F = fun() ->
		gen_storage:write(Server, #vcard{user_host = US, vcard = VcardToStore}),
		gen_storage:write(Server,
	      #vcard_search{user_host=US,
			    username  = UserStr,  lusername  = LUser,
			    fn        = FN,       lfn        = LFN,       
			    family    = Family,   lfamily    = LFamily,   
			    given     = Given,    lgiven     = LGiven,    
			    middle    = Middle,   lmiddle    = LMiddle,   
			    nickname  = Nickname, lnickname  = LNickname, 
			    bday      = BDay,     lbday      = LBDay,     
			    ctry      = CTRY,     lctry      = LCTRY,     
			    locality  = Locality, llocality  = LLocality, 
			    email     = EMail,    lemail     = LEMail,    
			    orgname   = OrgName,  lorgname   = LOrgName,  
			    orgunit   = OrgUnit,  lorgunit   = LOrgUnit   
			   })
	    end,
	    gen_storage:transaction(Server, vcard, F),
	    ejabberd_hooks:run(vcard_set, Server, [User, Server, VCARD])
    catch
	_ ->
	    {error, badarg}
    end.

-define(TLFIELD(Type, Label, Var),
	#xmlel{ns = ?NS_VCARD, name = 'field', attrs = [
	    ?XMLATTR(<<"type">>, Type),
	    ?XMLATTR(<<"label">>, translate:translate(Lang, Label)),
	    ?XMLATTR(<<"var">>, Var)]}).


-define(FORM(JID),
	[#xmlel{ns = ?NS_SEARCH, name = 'instructions', children =
	   [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "You need an x:data capable client to search"))}]},
	 #xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs =
	   [?XMLATTR(<<"type">>, <<"form">>)], children =
	   [#xmlel{ns = ?NS_DATA_FORMS, name = 'title', children =
	       [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "Search users in ") ++ exmpp_jid:to_list(JID))}]},
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


do_route(global, From, To, Packet) ->
    Host = exmpp_jid:prep_domain_as_list(To),
    ServerHost = ejabberd_global_router:server_host(Host, self()),
    do_route(ServerHost, From, To, Packet);
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
					  children =
					    [#xmlel{ns = ?NS_DATA_FORMS,
					      name = 'x',
					      attrs = [?XMLATTR(<<"type">>,
						  <<"result">>)],
					      children = search_result(Lang,
						To, ServerHost, XData)}]},
					ResIQ = exmpp_iq:result(Packet,
					  Result),
					ejabberd_router:route(
					  To, From, ResIQ)
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
			Info = ejabberd_hooks:run_fold(
				 disco_info, ServerHost, [],
				 [ServerHost, ?MODULE, <<>>, ""]),
			Result = #xmlel{ns = ?NS_DISCO_INFO, name = 'query',
			  children = Info ++ [
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'identity',
			      attrs = [
				?XMLATTR(<<"category">>, <<"directory">>),
				?XMLATTR(<<"type">>, <<"user">>),
				?XMLATTR(<<"name">>, translate:translate(Lang,
				    "vCard User Search"))]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				?XMLATTR(<<"var">>, ?NS_DISCO_INFO_s)]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				?XMLATTR(<<"var">>, ?NS_SEARCH_s)]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				?XMLATTR(<<"var">>, ?NS_VCARD_s)]}
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
	      "\nCopyright (c) 2002-2012 ProcessOne")}]}
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
	      exmpp_jid:to_list(JID))}]},
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
      ]}] ++ lists:map(fun record_to_item/1, search(ServerHost, Data)).

-define(FIELD(Var, Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	  [?XMLATTR(<<"var">>, Var)], children =
	  [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
	      [#xmlcdata{cdata = Val}]}]}).

record_to_item(R) ->
    {User, Server} = R#vcard_search.user_host,
    #xmlel{ns = ?NS_DATA_FORMS, name = 'item', children =
     [
       ?FIELD(<<"jid">>,      exmpp_jid:to_binary(User, Server)),
       ?FIELD(<<"fn">>,       list_to_binary(R#vcard_search.fn)),
       ?FIELD(<<"last">>,     list_to_binary(R#vcard_search.family)),
       ?FIELD(<<"first">>,    list_to_binary(R#vcard_search.given)),
       ?FIELD(<<"middle">>,   list_to_binary(R#vcard_search.middle)),
       ?FIELD(<<"nick">>,     list_to_binary(R#vcard_search.nickname)),
       ?FIELD(<<"bday">>,     list_to_binary(R#vcard_search.bday)),
       ?FIELD(<<"ctry">>,     list_to_binary(R#vcard_search.ctry)),
       ?FIELD(<<"locality">>, list_to_binary(R#vcard_search.locality)),
       ?FIELD(<<"email">>,    list_to_binary(R#vcard_search.email)),
       ?FIELD(<<"orgname">>,  list_to_binary(R#vcard_search.orgname)),
       ?FIELD(<<"orgunit">>,  list_to_binary(R#vcard_search.orgunit))
      ]
     }.


search(LServer, Data) ->
    MatchRules = make_matchrules(LServer, Data),
    AllowReturnAll = gen_mod:get_module_opt(LServer, ?MODULE,
					    allow_return_all, false),
    if
	(MatchRules =:= []) and (not AllowReturnAll) ->
	    [];
	true ->
	    %% TODO: pass ?JUD_MATCHES for smaller result sets
	    case catch gen_storage:dirty_select(LServer, vcard_search,
						MatchRules) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]),
		    [];
		Rs ->
		    case gen_mod:get_module_opt(LServer, ?MODULE,
						matches, ?JUD_MATCHES) of
			infinity ->
			    Rs;
			Val when is_integer(Val) and (Val > 0) ->
			    lists:sublist(Rs, Val);
			Val ->
			    ?ERROR_MSG("Illegal option value ~p. "
				       "Default value ~p substituted.",
				       [{matches, Val}, ?JUD_MATCHES]),
			    lists:sublist(Rs, ?JUD_MATCHES)
		    end
	    end
    end.


make_matchrules(LServer, Data) ->
    Match = filter_fields(Data, [], LServer),
    Match.




filter_fields([], Rules, _LServer) ->
    Rules;
filter_fields([{SVar, [Val]} | Ds], Rules, LServer)
  when is_list(Val) and (Val /= "") ->
    LVal = exmpp_stringprep:to_lower(Val),
    Rule = case SVar of
              "user" ->
                  case gen_mod:get_module_opt(LServer, ?MODULE,
                                              search_all_hosts, true) of
                      true ->
                          {like, lusername, make_val(LVal)};
                      false ->
                          Host = find_my_host(LServer),
                          {like, user_host, {make_val(LVal), Host}}
                  end;
              "fn"       -> {like, lfn, make_val(LVal)};
              "last"     -> {like, lfamily, make_val(LVal)};
              "first"    -> {like, lgiven, make_val(LVal)};
              "middle"   -> {like, lmiddle, make_val(LVal)};
              "nick"     -> {like, lnickname, make_val(LVal)};
              "bday"     -> {like, lbday, make_val(LVal)};
              "ctry"     -> {like, lctry, make_val(LVal)};
              "locality" -> {like, llocality, make_val(LVal)};
              "email"    -> {like, lemail, make_val(LVal)};
              "orgname"  -> {like, lorgname, make_val(LVal)};
              "orgunit"  -> {like, lorgunit, make_val(LVal)};
              _          -> false
          end,
    if
       Rule =/= false ->
           filter_fields(Ds, [Rule | Rules], LServer);
       true ->
           filter_fields(Ds, Rules, LServer)
    end;
filter_fields([_ | Ds], Rules, LServer) ->
    filter_fields(Ds, Rules, LServer).


make_val(Val) ->
    case lists:suffix("*", Val) of
	true ->
	    lists:sublist(Val, length(Val) - 1) ++ ['_'];
	_ ->
	    Val
    end.

find_my_host(LServer) ->
    Parts = string:tokens(LServer, "."),
    find_my_host(Parts, ?MYHOSTS).

find_my_host([], _Hosts) ->
    ?MYNAME;
find_my_host([_ | Tail] = Parts, Hosts) ->
    Domain = parts_to_string(Parts),
    case lists:member(Domain, Hosts) of
	true ->
	    Domain;
	false ->
	    find_my_host(Tail, Hosts)
    end.

parts_to_string(Parts) ->
    string:strip(lists:flatten(lists:map(fun(S) -> [S, $.] end, Parts)),
		 right, $.).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_vcard_t(R, _) ->
    US = R#vcard.user_host,
    User  = US,
    VCARD = R#vcard.vcard,

    FN       = exmpp_xml:get_path(VCARD,
      [{element, 'FN'},                        cdata_as_list]),
    Family   = exmpp_xml:get_path(VCARD,
      [{element, 'N'}, {element, 'FAMILY'},    cdata_as_list]),
    Given    = exmpp_xml:get_path(VCARD,
      [{element, 'N'}, {element, 'GIVEN'},     cdata_as_list]),
    Middle   = exmpp_xml:get_path(VCARD,
      [{element, 'N'}, {element, 'MIDDLE'},    cdata_as_list]),
    Nickname = exmpp_xml:get_path(VCARD,
      [{element, 'NICKNAME'},                  cdata_as_list]),
    BDay     = exmpp_xml:get_path(VCARD,
      [{element, 'BDAY'},                      cdata_as_list]),
    CTRY     = exmpp_xml:get_path(VCARD,
      [{element, 'ADR'}, {element, 'CTRY'},    cdata_as_list]),
    Locality = exmpp_xml:get_path(VCARD,
      [{element, 'ADR'}, {element, 'LOCALITY'},cdata_as_list]),
    EMail    = exmpp_xml:get_path(VCARD,
      [{element, 'EMAIL'},                     cdata_as_list]),
    OrgName  = exmpp_xml:get_path(VCARD,
      [{element, 'ORG'}, {element, 'ORGNAME'}, cdata_as_list]),
    OrgUnit  = exmpp_xml:get_path(VCARD,
      [{element, 'ORG'}, {element, 'ORGUNIT'}, cdata_as_list]),

    try
	{LUser, _LServer} = US,
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
	mnesia:write(
	  #vcard_search{user_host= US,
			username  = User,     lusername  = LUser,     
			fn        = FN,       lfn        = LFN,       
			family    = Family,   lfamily    = LFamily,   
			given     = Given,    lgiven     = LGiven,    
			middle    = Middle,   lmiddle    = LMiddle,   
			nickname  = Nickname, lnickname  = LNickname, 
			bday      = BDay,     lbday      = LBDay,     
			ctry      = CTRY,     lctry      = LCTRY,     
			locality  = Locality, llocality  = LLocality, 
			email     = EMail,    lemail     = LEMail,    
			orgname   = OrgName,  lorgname   = LOrgName,  
			orgunit   = OrgUnit,  lorgunit   = LOrgUnit   
		       })
    catch
	_ ->
	    {error, badarg}
    end.


reindex_vcards() ->
    F = fun() ->
		mnesia:foldl(fun set_vcard_t/2, [], vcard)
	end,
    mnesia:transaction(F).


remove_user(User, Server) when is_binary(User), is_binary(Server) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		gen_storage:delete(Server, {vcard, US}),
		gen_storage:delete(Server, {vcard_search, US})
	end,
    gen_storage:transaction(Server, vcard, F).


%%%
%%% Update tables
%%%

update_tables(global, Storage) ->
    [update_tables(HostB, Storage) || HostB <- ejabberd_hosts:get_hosts(ejabberd)];

update_tables(Host, mnesia) ->
    gen_storage_migration:migrate_mnesia(
      Host, vcard,
      [{vcard, [us, vcard],
	fun({vcard, {UserS, ServerS}, Vcard}) ->
		#vcard{user_host = {list_to_binary(UserS), list_to_binary(ServerS)},
		       vcard = convert_vcard_element(Vcard)}
	end}]),
    gen_storage_migration:migrate_mnesia(
      Host, vcard_search,
      [{vcard_search, [us,
		       user,     luser,
		       fn,	 lfn,
		       family,	 lfamily,
		       given,	 lgiven,
		       middle,	 lmiddle,
		       nickname, lnickname,
		       bday,	 lbday,
		       ctry,	 lctry,
		       locality, llocality,
		       email,	 lemail,
		       orgname,	 lorgname,
		       orgunit,	 lorgunit],
	fun(#vcard_search{user_host = {UserS, ServerS}} = Record) ->
		UserHost = {list_to_binary(UserS), list_to_binary(ServerS)},
		Record#vcard_search{user_host = UserHost, username = UserS}
	end}]);

update_tables(Host, odbc) ->
    gen_storage_migration:migrate_odbc(
      Host, [vcard],
      [{"vcard", ["username", "vcard"],
	fun(_, Username, Vcard) ->
		[#vcard{user_host = {Username, Host},
			vcard = Vcard}]
	end}]),
    gen_storage_migration:migrate_odbc(
      Host, [vcard_search],
      [{"vcard_search", ["username", "lusername",
			 "fn", "lfn",
			 "family", "lfamily",
			 "given", "lgiven",
			 "middle", "lmiddle",
			 "nickname", "lnickname",
			 "bday", "lbday",
			 "ctry", "lctry",
			 "locality", "llocality",
			 "email", "lemail",
			 "orgname", "lorgname",
			 "orgunit", "lorgunit"],
	fun(_, User, LUser,
	    FN, LFN,
	    Family, LFamily,
	    Given, LGiven,
	    Middle, LMiddle,
	    Nickname, LNickname,
	    BDay, LBDay,
	    CTRY, LCTRY,
	    Locality, LLocality,
	    EMail, LEMail,
	    OrgName, LOrgName,
	    OrgUnit, LOrgUnit) ->
		[#vcard_search{user_host = {LUser, Host},
			       username  = User,     lusername  = LUser,
			       fn        = FN,       lfn        = LFN,       
			       family    = Family,   lfamily    = LFamily,   
			       given     = Given,    lgiven     = LGiven,    
			       middle    = Middle,   lmiddle    = LMiddle,   
			       nickname  = Nickname, lnickname  = LNickname, 
			       bday      = BDay,     lbday      = LBDay,     
			       ctry      = CTRY,     lctry      = LCTRY,     
			       locality  = Locality, llocality  = LLocality, 
			       email     = EMail,    lemail     = LEMail,    
			       orgname   = OrgName,  lorgname   = LOrgName,  
			       orgunit   = OrgUnit,  lorgunit   = LOrgUnit}]
	end}]).

convert_vcard_element(Xmlelement) ->
    Xmlel = exmpp_xml:xmlelement_to_xmlel(Xmlelement, [?NS_VCARD], []),
    exmpp_xml:remove_whitespaces_deeply(Xmlel).

%%%
%%% WebAdmin
%%%

webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "vcard"],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = user_vcard(U, Host, Query, Lang),
    {stop, Res};

webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "vcard", "photo"],
		       lang = _Lang} = _Request) ->
    Res = get_user_photo(U, Host),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

user_vcard(User, Server, Query, Lang) ->
    US = {LUser, LServer} = {exmpp_stringprep:nodeprep(User), exmpp_stringprep:nameprep(Server)},
    Res = user_queue_parse_query(US, Query),
    VcardString = case get_vcard(LUser, LServer) of
	{vcard, Xml} -> 
	    XmlString = lists:flatten(exmpp_xml:document_to_list(Xml)),
	    Reduced = re:replace(XmlString, "<BINVAL>[^<]*", "<BINVAL>...", [global, {return, list}]),
	    try_indent(Reduced);
	novcard -> "no vcard"
    end,
    [?XE('h1', [?CT("vCard")]),
     ?XE('h2', [?AC("../", us_to_list(US))])
    ] ++
	case Res of
	    {ok, M} -> [?XREST(M)];
	    {error, M} -> [?XREST(M)];
	    nothing -> []
	end ++
	[?XAE('form', [?XMLATTR(<<"action">>, <<"">>), ?XMLATTR(<<"method">>, <<"post">>)],
	        [?XCT('h3', "vCard Photo:"),
		    ?XAE('img', [?XMLATTR(<<"src">>, <<"photo">>), ?XMLATTR(<<"border">>, <<"1px">>)], []),
                 ?XC('h3', ?T("vCard")++":"),
		 ?XE('pre', [?C(VcardString)]),
	       ?INPUTT("submit", "removevcard", "Remove vCard")
	      ])].

%% TODO: Is there a nice way to indent XML in Erlang/OTP or exmpp?
try_indent(String) ->
    try
	Indented = os:cmd("echo \""++String++"\" | xmlindent"),
	[$< | _] = Indented,
	Indented
    catch
	_:_ ->
	    String
    end.

get_user_photo(User, Host) ->
    case get_vcard(User, Host) of
	{vcard, VCard} -> 
	    case exmpp_xml:get_path(VCard, [{element, "PHOTO"}, {element, "BINVAL"}, cdata_as_list]) of
                [] -> "no avatar";
                BinVal -> case catch jlib:decode_base64(BinVal) of
                               {'EXIT', _} -> "error";
                               Decoded -> Decoded
                          end
           end;
	novcard -> "no vcard"
    end.

user_queue_parse_query(US, Query) ->
    {User, Server} = US,
    case lists:keysearch("removevcard", 1, Query) of
	{value, _} ->
	    case remove_user(list_to_binary(User), list_to_binary(Server)) of
		 {aborted, Reason} ->
		    ?ERROR_MSG("Failed to remove user's vCard: ~p", [Reason]),
		    {error, io_lib:format("Failed to remove user's vCard: ~p", [Reason])};
		 {atomic, ok} ->
		    ?INFO_MSG("Removed vCard of ~s@~s", [User, Server]),
		    {ok, io_lib:format("Removed vCard of ~s@~s", [User, Server])}
	    end;
	false ->
	    nothing
    end.

us_to_list({User, Server}) ->
    exmpp_jid:to_list(User, Server).

webadmin_user(Acc, User, Server, Lang) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    VcardSize = case get_vcard(LUser, LServer) of
	{vcard, Vcard} -> get_vcard_size(Vcard);
	novcard -> 0
    end,
    FVcardSize = case VcardSize > 0 of
	true -> [?AC("vcard/", integer_to_list(VcardSize))];
	false -> [?C(integer_to_list(VcardSize))]
    end,
    RemoveEl = case VcardSize > 0 of
	true -> [?INPUTT("submit", "removevcard", "Remove vCard")];
	false -> []
    end,
    Acc ++ [?XCT('h3', "vCard size (characters):")] ++ FVcardSize ++ RemoveEl.

get_vcard_size(Vcard) ->
    String = lists:flatten(exmpp_xml:document_to_list(Vcard)),
    length(String).

webadmin_user_parse_query(_, "removevcard", User, Server, _Query) ->
    case remove_user(list_to_binary(User), list_to_binary(Server)) of
         {aborted, Reason} ->
            ?ERROR_MSG("Failed to remove user's vCard: ~p", [Reason]),
            {stop, error};
         {atomic, ok} ->
            ?INFO_MSG("Removed vCard of ~s@~s", [User, Server]),
            {stop, ok}
    end;
webadmin_user_parse_query(Acc, _Action, _User, _Server, _Query) ->
    Acc.
