%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Vcard management in Mnesia
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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

-module(mod_vcard).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, init/3, stop/1,
	 get_sm_features/5,
	 process_local_iq/3,
	 process_sm_iq/3,
	 reindex_vcards/0,
	 remove_user/2]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").


-define(JUD_MATCHES, 30).

-record(vcard_search, {us,
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
		       orgunit,	 lorgunit
		      }).
-record(vcard, {us, vcard}).

-define(PROCNAME, ejabberd_mod_vcard).

start(Host, Opts) ->
    HostB = list_to_binary(Host),
    mnesia:create_table(vcard, [{disc_only_copies, [node()]},
				{attributes, record_info(fields, vcard)}]),
    mnesia:create_table(vcard_search,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, vcard_search)}]),
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
    mnesia:add_table_index(vcard_search, lorgunit),

    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_VCARD,
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
	[] ->
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
	  "\nCopyright (c) 2002-2009 ProcessOne"),
	exmpp_xml:set_cdata(#xmlel{ns = ?NS_VCARD, name = 'BDAY'},
	  "2002-11-16")
      ]},
    exmpp_iq:result(IQ_Rec, Result);
process_local_iq(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').


process_sm_iq(_From, To, #iq{type = get} = IQ_Rec) ->
    LUser = exmpp_jid:lnode_as_list(To),
    LServer = exmpp_jid:prep_domain_as_list(To),
    US = {LUser, LServer},
    F = fun() ->
		mnesia:read({vcard, US})
	end,
    Els = case mnesia:transaction(F) of
	      {atomic, Rs} ->
		  lists:map(fun(R) ->
				    R#vcard.vcard
			    end, Rs);
	      {aborted, _Reason} ->
		  []
	  end,
    case Els of
	[VCard | _] ->
	    exmpp_iq:result(IQ_Rec, VCard);
	_ ->
	    exmpp_iq:result(IQ_Rec)
    end;
process_sm_iq(From, _To, #iq{type = set, payload = Request} = IQ_Rec) ->
    User = exmpp_jid:node_as_list(From),
    LServer = exmpp_jid:prep_domain_as_list(From),
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

	US = {LUser, LServer},

	F = fun() ->
	    mnesia:write(#vcard{us = US, vcard = VCARD}),
	    mnesia:write(
	      #vcard_search{us        = US,
			    user      = {User, LServer},
			    luser     = LUser,
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
	    mnesia:transaction(F)
    catch
	_ ->
	    {error, badarg}
    end.

-define(TLFIELD(Type, Label, Var),
	#xmlel{ns = ?NS_VCARD, name = 'field', attrs = [
	    ?XMLATTR('type', Type),
	    ?XMLATTR('label', translate:translate(Lang, Label)),
	    ?XMLATTR('var', Var)]}).


-define(FORM(JID),
	[#xmlel{ns = ?NS_SEARCH, name = 'instructions', children =
	   [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "You need an x:data capable client to search"))}]},
	 #xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs =
	   [?XMLATTR('type', <<"form">>)], children =
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
	    ejabberd_router ! {route, To, From, Err};
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
					      attrs = [?XMLATTR('type',
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
			Result = #xmlel{ns = ?NS_DISCO_INFO, name = 'query',
			  children = [
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'identity',
			      attrs = [
				?XMLATTR('category', <<"directory">>),
				?XMLATTR('type', <<"user">>),
				?XMLATTR('name', translate:translate(Lang,
				    "vCard User Search"))]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				?XMLATTR('var', ?NS_DISCO_INFO_s)]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				?XMLATTR('var', ?NS_SEARCH_s)]},
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
			      attrs = [
				?XMLATTR('var', ?NS_VCARD_s)]}
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
	      "\nCopyright (c) 2003-2009 ProcessOne")}]}
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
      ]}] ++ lists:map(fun record_to_item/1, search(ServerHost, Data)).

-define(FIELD(Var, Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	  [?XMLATTR('var', Var)], children =
	  [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
	      [#xmlcdata{cdata = Val}]}]}).

record_to_item(R) ->
    {User, Server} = R#vcard_search.user,
    #xmlel{ns = ?NS_DATA_FORMS, name = 'item', children =
     [
       ?FIELD(<<"jid">>,      list_to_binary(User ++ "@" ++ Server)),
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
    MatchSpec = make_matchspec(LServer, Data),
    AllowReturnAll = gen_mod:get_module_opt(LServer, ?MODULE,
					    allow_return_all, false),
    if
	(MatchSpec == #vcard_search{_ = '_'}) and (not AllowReturnAll) ->
	    [];
	true ->
	    case catch mnesia:dirty_select(vcard_search,
					   [{MatchSpec, [], ['$_']}]) of
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


make_matchspec(LServer, Data) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch, LServer),
    Match.

filter_fields([], Match, _LServer) ->
    Match;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_list(Val) and (Val /= "") ->
    LVal = exmpp_stringprep:to_lower(Val),
    NewMatch = case SVar of
                   "user" ->
		       case gen_mod:get_module_opt(LServer, ?MODULE,
						   search_all_hosts, true) of
			   true ->
			       Match#vcard_search{luser = make_val(LVal)};
			   false ->
			       Host = find_my_host(LServer),
			       Match#vcard_search{us = {make_val(LVal), Host}}
		       end;
                   "fn"       -> Match#vcard_search{lfn       = make_val(LVal)};
                   "last"     -> Match#vcard_search{lfamily   = make_val(LVal)};
                   "first"    -> Match#vcard_search{lgiven    = make_val(LVal)};
                   "middle"   -> Match#vcard_search{lmiddle   = make_val(LVal)};
                   "nick"     -> Match#vcard_search{lnickname = make_val(LVal)};
                   "bday"     -> Match#vcard_search{lbday     = make_val(LVal)};
                   "ctry"     -> Match#vcard_search{lctry     = make_val(LVal)};
                   "locality" -> Match#vcard_search{llocality = make_val(LVal)};
                   "email"    -> Match#vcard_search{lemail    = make_val(LVal)};
                   "orgname"  -> Match#vcard_search{lorgname  = make_val(LVal)};
                   "orgunit"  -> Match#vcard_search{lorgunit  = make_val(LVal)};
		   _          -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

make_val(Val) ->
    case lists:suffix("*", Val) of
	true ->
	    lists:sublist(Val, length(Val) - 1) ++ '_';
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
    US = R#vcard.us,
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
	  #vcard_search{us        = US,
			user      = User,     luser      = LUser,     
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
		mnesia:delete({vcard, US}),
		mnesia:delete({vcard_search, US})
	end,
    mnesia:transaction(F).


update_tables() ->
    update_vcard_table(),
    update_vcard_search_table().

update_vcard_table() ->
    Fields = record_info(fields, vcard),
    case mnesia:table_info(vcard, attributes) of
	Fields ->
	    convert_to_exmpp();
	[user, vcard] ->
	    ?INFO_MSG("Converting vcard table from "
		      "{user, vcard} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_vcard_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, vcard},
			      {attributes, record_info(fields, vcard)}]),
	    mnesia:transform_table(vcard, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_vcard_tmp_table),
			 mnesia:foldl(
			   fun(#vcard{us = U} = R, _) ->
				   mnesia:dirty_write(
				     mod_vcard_tmp_table,
				     R#vcard{us = {U, Host}})
			   end, ok, vcard)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(vcard),
	    F2 = fun() ->
			 mnesia:write_lock_table(vcard),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_vcard_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_vcard_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating vcard table", []),
	    mnesia:transform_table(vcard, ignore, Fields)
    end.


convert_to_exmpp() ->
    Fun = fun() ->
	case mnesia:first(vcard) of
	    '$end_of_table' ->
		none;
	    Key ->
		case mnesia:read({vcard, Key}) of
		    [#vcard{vcard = #xmlel{}}] ->
			none;
		    [#vcard{vcard = #xmlelement{}}] ->
			mnesia:foldl(fun convert_to_exmpp2/2,
			  done, vcard, write)
		end
	end
    end,
    mnesia:transaction(Fun).

convert_to_exmpp2(#vcard{vcard = ElOld} = VCard, Acc) ->
    El0 = exmpp_xml:xmlelement_to_xmlel(ElOld, [?NS_VCARD], []),
    El = exmpp_xml:remove_whitespaces_deeply(El0),
    mnesia:write(VCard#vcard{vcard = El}),
    Acc.

update_vcard_search_table() ->
    Fields = record_info(fields, vcard_search),
    case mnesia:table_info(vcard_search, attributes) of
	Fields ->
	    ok;
	[user,     luser,
	 fn,       lfn,
	 family,   lfamily,
	 given,    lgiven,
	 middle,   lmiddle,
	 nickname, lnickname,
	 bday,	   lbday,
	 ctry,	   lctry,
	 locality, llocality,
	 email,	   lemail,
	 orgname,  lorgname,
	 orgunit,  lorgunit] ->
	    ?INFO_MSG("Converting vcard_search table from "
		      "{user, luser, fn, lfn, family, lfamily, given, lgiven, middle, lmiddle, nickname, lnickname, bday, lbday, ctry, lctry, locality, llocality, email, lemail, orgname, lorgname, orgunit, lorgunit} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_vcard_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, vcard_search},
			      {attributes, record_info(fields, vcard_search)}]),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_vcard_tmp_table),
			 mnesia:foldl(
			   fun({vcard_search,
				User,     LUser,     
				FN,       LFN,       
				Family,   LFamily,   
				Given,    LGiven,    
				Middle,   LMiddle,   
				Nickname, LNickname, 
				BDay,     LBDay,     
				CTRY,     LCTRY,     
				Locality, LLocality, 
				EMail,    LEMail,    
				OrgName,  LOrgName,  
				OrgUnit,  LOrgUnit   
			       }, _) ->
				   mnesia:dirty_write(
				     mod_vcard_tmp_table,
				     #vcard_search{
				       us        = {LUser, Host},
				       user      = {User, Host},
				       luser     = LUser,
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
			   end, ok, vcard_search)
		 end,
	    mnesia:transaction(F1),
	    lists:foreach(fun(I) ->
				  mnesia:del_table_index(
				    vcard_search,
				    element(I, {vcard_search,
						user,     luser,
						fn,       lfn,
						family,   lfamily,
						given,    lgiven,
						middle,   lmiddle,
						nickname, lnickname,
						bday,	   lbday,
						ctry,	   lctry,
						locality, llocality,
						email,	   lemail,
						orgname,  lorgname,
						orgunit,  lorgunit}))
			  end, mnesia:table_info(vcard_search, index)),
	    mnesia:clear_table(vcard_search),
	    mnesia:transform_table(vcard_search, ignore, Fields),
	    F2 = fun() ->
	        	 mnesia:write_lock_table(vcard_search),
	        	 mnesia:foldl(
	        	   fun(R, _) ->
	        		   mnesia:dirty_write(R)
	        	   end, ok, mod_vcard_tmp_table)
	         end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_vcard_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating vcard_search table", []),
	    mnesia:transform_table(vcard_search, ignore, Fields)
    end.

