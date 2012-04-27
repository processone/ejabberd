%%%----------------------------------------------------------------------
%%% File    : mod_vcard_ldap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for VCards from LDAP storage.
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

-module(mod_vcard_ldap).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(gen_mod).

%% gen_server callbacks.
-export([init/1,
	 handle_info/2,
	 handle_call/3,
	 handle_cast/2,
	 terminate/2,
	 code_change/3
	]).

-export([start/2,
	 start_link/2,
	 stop/1,
	 get_sm_features/5,
	 process_local_iq/3,
	 process_sm_iq/3,
	 remove_user/1,
	 route/4
	]).

-include("ejabberd.hrl").
-include("eldap/eldap.hrl").
-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_vcard_ldap).

-record(state, {serverhost,
		myhost,
		eldap_id,
		search,
		servers,
		backups,
		port,
		tls_options,
		dn,
		base,
		password,
		uids,
		vcard_map,
		vcard_map_attrs,
		user_filter,
		search_filter,
		search_fields,
		search_reported,
		search_reported_attrs,
                deref_aliases,
		matches
	       }).

-define(VCARD_MAP,
	[{"NICKNAME", "%u", []},
	 {"FN", "%s", ["displayName"]},
	 {"FAMILY", "%s", ["sn"]},
	 {"GIVEN", "%s", ["givenName"]},
	 {"MIDDLE", "%s", ["initials"]},
	 {"ORGNAME", "%s", ["o"]},
	 {"ORGUNIT", "%s", ["ou"]},
	 {"CTRY", "%s", ["c"]},
	 {"LOCALITY", "%s", ["l"]},
	 {"STREET", "%s", ["street"]},
	 {"REGION", "%s", ["st"]},
	 {"PCODE", "%s", ["postalCode"]},
	 {"TITLE", "%s", ["title"]},
	 {"URL", "%s", ["labeleduri"]},
	 {"DESC", "%s", ["description"]},
	 {"TEL", "%s", ["telephoneNumber"]},
	 {"EMAIL", "%s", ["mail"]},
	 {"BDAY", "%s", ["birthDay"]},
	 {"ROLE", "%s", ["employeeType"]},
	 {"PHOTO", "%s", ["jpegPhoto"]}
	]).

-define(SEARCH_FIELDS,
	[{"User", "%u"},
	 {"Full Name", "displayName"},
	 {"Given Name", "givenName"},
	 {"Middle Name", "initials"},
	 {"Family Name", "sn"},
	 {"Nickname", "%u"},
	 {"Birthday", "birthDay"},
	 {"Country", "c"},
	 {"City", "l"},
	 {"Email", "mail"},
	 {"Organization Name", "o"},
	 {"Organization Unit", "ou"}
	]).

-define(SEARCH_REPORTED,
	[{"Full Name", "FN"},
	 {"Given Name", "FIRST"},
	 {"Middle Name", "MIDDLE"},
	 {"Family Name", "LAST"},
	 {"Nickname", "NICK"},
	 {"Birthday", "BDAY"},
	 {"Country", "CTRY"},
	 {"City", "LOCALITY"},
	 {"Email", "EMAIL"},
	 {"Organization Name", "ORGNAME"},
	 {"Organization Unit", "ORGUNIT"}
	]).

%% Unused callbacks.
handle_cast(_Request, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% -----


start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {
      Proc, {?MODULE, start_link, [Host, Opts]},
      transient, 1000, worker, [?MODULE]
     },
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

terminate(_Reason, State) ->
    Host = State#state.serverhost,
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    case State#state.search of
	true ->
	    ejabberd_router:unregister_route(State#state.myhost);
	_ ->
	    ok
    end.

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

init([Host, Opts]) ->
    State = parse_options(Host, Opts),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_VCARD,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    eldap_pool:start_link(State#state.eldap_id,
		     State#state.servers,
		     State#state.backups,
		     State#state.port,
		     State#state.dn,
		     State#state.password,
		     State#state.tls_options),
    case State#state.search of
	true ->
	    ejabberd_router:register_route(State#state.myhost);
	_ ->
	    ok
    end,
    {ok, State}.

handle_info({route, From, To, Packet}, State) ->
    case catch do_route(State, From, To, Packet) of
	Pid when is_pid(Pid) ->
	    ok;
	_ ->
	    Err = jlib:make_error_reply(Packet, ?ERR_INTERNAL_SERVER_ERROR),
	    ejabberd_router:route(To, From, Err)
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

get_sm_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_sm_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
	[] ->
	    case Acc of
		{result, Features} ->
		    {result, [?NS_VCARD | Features]};
		empty ->
		    {result, [?NS_VCARD]}
	    end;
	_ ->
	    Acc
    end.

process_local_iq(_From, _To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "vCard",
			     [{"xmlns", ?NS_VCARD}],
			     [{xmlelement, "FN", [],
			       [{xmlcdata, "ejabberd"}]},
			      {xmlelement, "URL", [],
			       [{xmlcdata, ?EJABBERD_URI}]},
			      {xmlelement, "DESC", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang,
				   "Erlang Jabber Server") ++
				   "\nCopyright (c) 2002-2012 ProcessOne"}]},
			      {xmlelement, "BDAY", [],
			       [{xmlcdata, "2002-11-16"}]}
			     ]}]}
    end.

process_sm_iq(_From, #jid{lserver=LServer} = To, #iq{sub_el = SubEl} = IQ) ->
    case catch process_vcard_ldap(To, IQ, LServer) of
	{'EXIT', _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
	Other ->
	    Other
    end.

process_vcard_ldap(To, IQ, Server) ->
    {ok, State} = eldap_utils:get_state(Server, ?PROCNAME),
    #iq{type = Type, sub_el = SubEl} = IQ,
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    #jid{luser = LUser} = To,
	    LServer = State#state.serverhost,
	    case ejabberd_auth:is_user_exists(LUser, LServer) of
		true ->
		    VCardMap = State#state.vcard_map,
		    case find_ldap_user(LUser, State) of
			#eldap_entry{attributes = Attributes} ->
			    Vcard = ldap_attributes_to_vcard(Attributes, VCardMap, {LUser, LServer}),
			    IQ#iq{type = result, sub_el = Vcard};
			_ ->
			    IQ#iq{type = result, sub_el = []}
		    end;
		_ ->
		    IQ#iq{type = result, sub_el = []}
	    end
	end.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

find_ldap_user(User, State) ->
    Base = State#state.base,
    RFC2254_Filter = State#state.user_filter,
    Eldap_ID = State#state.eldap_id,
    VCardAttrs = State#state.vcard_map_attrs,
    case eldap_filter:parse(RFC2254_Filter, [{"%u", User}]) of
	{ok, EldapFilter} ->
	    case eldap_pool:search(Eldap_ID,
                                   [{base, Base},
                                    {filter, EldapFilter},
                                    {deref_aliases, State#state.deref_aliases},
                                    {attributes, VCardAttrs}]) of
		#eldap_search_result{entries = [E | _]} ->
		    E;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

ldap_attributes_to_vcard(Attributes, VCardMap, UD) ->
    Attrs = lists:map(
	      fun({VCardName, _, _}) ->
		      {stringprep:tolower(VCardName),
		       map_vcard_attr(VCardName, Attributes, VCardMap, UD)}
	      end, VCardMap),
    Elts = [ldap_attribute_to_vcard(vCard, Attr) || Attr <- Attrs],
    NElts = [ldap_attribute_to_vcard(vCardN, Attr) || Attr <- Attrs],
    OElts = [ldap_attribute_to_vcard(vCardO, Attr) || Attr <- Attrs],
    AElts = [ldap_attribute_to_vcard(vCardA, Attr) || Attr <- Attrs],
    [{xmlelement, "vCard", [{"xmlns", ?NS_VCARD}],
      lists:append([X || X <- Elts, X /= none],
		   [{xmlelement,"N",[],   [X || X <- NElts, X /= none]},
		    {xmlelement,"ORG",[], [X || X <- OElts, X /= none]},
		    {xmlelement,"ADR",[], [X || X <- AElts, X /= none]}])
     }].

ldap_attribute_to_vcard(vCard, {"fn", Value}) ->
    {xmlelement,"FN",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCard, {"nickname", Value}) ->
    {xmlelement,"NICKNAME",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCard, {"title", Value}) ->
    {xmlelement,"TITLE",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCard, {"bday", Value}) ->
    {xmlelement,"BDAY",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCard, {"url", Value}) ->
    {xmlelement,"URL",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCard, {"desc", Value}) ->
    {xmlelement,"DESC",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCard, {"role", Value}) ->
    {xmlelement,"ROLE",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCard, {"tel", Value}) ->
    {xmlelement,"TEL",[],[{xmlelement,"VOICE",[],[]},
			  {xmlelement,"WORK",[],[]},
			  {xmlelement,"NUMBER",[],[{xmlcdata,Value}]}]};

ldap_attribute_to_vcard(vCard, {"email", Value}) ->
    {xmlelement,"EMAIL",[],[{xmlelement,"INTERNET",[],[]},
			    {xmlelement,"PREF",[],[]},
			    {xmlelement,"USERID",[],[{xmlcdata,Value}]}]};

ldap_attribute_to_vcard(vCard, {"photo", Value}) ->
    {xmlelement,"PHOTO",[],[
			    {xmlelement,"TYPE",[],[{xmlcdata,"image/jpeg"}]},
			    {xmlelement,"BINVAL",[],[{xmlcdata, jlib:encode_base64(Value)}]}]};

ldap_attribute_to_vcard(vCardN, {"family", Value}) ->
    {xmlelement,"FAMILY",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCardN, {"given", Value}) ->
    {xmlelement,"GIVEN",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCardN, {"middle", Value}) ->
    {xmlelement,"MIDDLE",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCardO, {"orgname", Value}) ->
    {xmlelement,"ORGNAME",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCardO, {"orgunit", Value}) ->
    {xmlelement,"ORGUNIT",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCardA, {"locality", Value}) ->
    {xmlelement,"LOCALITY",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCardA, {"street", Value}) ->
    {xmlelement,"STREET",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCardA, {"ctry", Value}) ->
    {xmlelement,"CTRY",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCardA, {"region", Value}) ->
    {xmlelement,"REGION",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(vCardA, {"pcode", Value}) ->
    {xmlelement,"PCODE",[],[{xmlcdata,Value}]};

ldap_attribute_to_vcard(_, _) ->
    none.

-define(TLFIELD(Type, Label, Var),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).

-define(FORM(JID, SearchFields),
	[{xmlelement, "instructions", [],
	  [{xmlcdata, translate:translate(Lang, "You need an x:data capable client to search")}]},
	 {xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
	  [{xmlelement, "title", [],
	    [{xmlcdata, translate:translate(Lang, "Search users in ") ++
	      jlib:jid_to_string(JID)}]},
	   {xmlelement, "instructions", [],
	    [{xmlcdata, translate:translate(Lang, "Fill in fields to search "
					    "for any matching Jabber User")}]}
	  ] ++ lists:map(fun({X,Y}) -> ?TLFIELD("text-single", X, Y) end, SearchFields)}]).

do_route(State, From, To, Packet) ->
    spawn(?MODULE, route, [State, From, To, Packet]).

route(State, From, To, Packet) ->
    #jid{user = User, resource = Resource} = To,
    ServerHost = State#state.serverhost,
    if
	(User /= "") or (Resource /= "") ->
	    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
	    ejabberd_router:route(To, From, Err);
	true ->
	    IQ = jlib:iq_query_info(Packet),
	    case IQ of
		#iq{type = Type, xmlns = ?NS_SEARCH, lang = Lang, sub_el = SubEl} ->
		    case Type of
			set ->
			    XDataEl = find_xdata_el(SubEl),
			    case XDataEl of
				false ->
				    Err = jlib:make_error_reply(
					    Packet, ?ERR_BAD_REQUEST),
				    ejabberd_router:route(To, From, Err);
				_ ->
				    XData = jlib:parse_xdata_submit(XDataEl),
				    case XData of
					invalid ->
					    Err = jlib:make_error_reply(
						    Packet,
						    ?ERR_BAD_REQUEST),
					    ejabberd_router:route(To, From,
								  Err);
					_ ->
					    ResIQ =
						IQ#iq{
						  type = result,
						  sub_el =
						  [{xmlelement,
						    "query",
						    [{"xmlns", ?NS_SEARCH}],
						    [{xmlelement, "x",
						      [{"xmlns", ?NS_XDATA},
						       {"type", "result"}],
						      search_result(Lang, To, State, XData)
						     }]}]},
					    ejabberd_router:route(
					      To, From, jlib:iq_to_xml(ResIQ))
				    end
			    end;
			get ->
			    SearchFields = State#state.search_fields,
			    ResIQ = IQ#iq{type = result,
					  sub_el = [{xmlelement,
						     "query",
						     [{"xmlns", ?NS_SEARCH}],
						     ?FORM(To, SearchFields)
						    }]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = Type, xmlns = ?NS_DISCO_INFO, lang = Lang} ->
		    case Type of
			set ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_NOT_ALLOWED),
			    ejabberd_router:route(To, From, Err);
			get ->
			    Info = ejabberd_hooks:run_fold(
				     disco_info, ServerHost, [],
				     [ServerHost, ?MODULE, "", ""]),
			    ResIQ =
				IQ#iq{type = result,
				      sub_el = [{xmlelement,
						 "query",
						 [{"xmlns", ?NS_DISCO_INFO}],
						 [{xmlelement, "identity",
						   [{"category", "directory"},
						    {"type", "user"},
						    {"name",
						     translate:translate(Lang, "vCard User Search")}],
						   []},
						  {xmlelement, "feature",
						   [{"var", ?NS_SEARCH}], []},
						  {xmlelement, "feature",
						   [{"var", ?NS_VCARD}], []}
						 ] ++ Info
						}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = Type, xmlns = ?NS_DISCO_ITEMS} ->
		    case Type of
			set ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_NOT_ALLOWED),
			    ejabberd_router:route(To, From, Err);
			get ->
			    ResIQ = 
				IQ#iq{type = result,
				      sub_el = [{xmlelement,
						 "query",
						 [{"xmlns", ?NS_DISCO_ITEMS}],
						 []}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = get, xmlns = ?NS_VCARD, lang = Lang} ->
		    ResIQ = 
			IQ#iq{type = result,
			      sub_el = [{xmlelement,
					 "vCard",
					 [{"xmlns", ?NS_VCARD}],
					 iq_get_vcard(Lang)}]},
		    ejabberd_router:route(To,
					  From,
					  jlib:iq_to_xml(ResIQ));
		_ ->
		    Err = jlib:make_error_reply(Packet,
						?ERR_SERVICE_UNAVAILABLE),
		    ejabberd_router:route(To, From, Err)
	    end
    end.

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_vcard"}]},
     {xmlelement, "URL", [],
      [{xmlcdata, ?EJABBERD_URI}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, translate:translate(
		    Lang,
		    "ejabberd vCard module") ++
		    "\nCopyright (c) 2003-2012 ProcessOne"}]}].

-define(LFIELD(Label, Var),
	{xmlelement, "field", [{"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).

search_result(Lang, JID, State, Data) ->
    SearchReported = State#state.search_reported,
    Header = [{xmlelement, "title", [],
	       [{xmlcdata, translate:translate(Lang, "Search Results for ") ++
		 jlib:jid_to_string(JID)}]},
	      {xmlelement, "reported", [],
	       [?TLFIELD("text-single", "Jabber ID", "jid")] ++
	       lists:map(
		 fun({Name, Value}) -> ?TLFIELD("text-single", Name, Value) end,
		 SearchReported)
	      }],
    case search(State, Data) of
	error ->
	    Header;
	Result ->
	    Header ++ Result
    end.

-define(FIELD(Var, Val),
	{xmlelement, "field", [{"var", Var}],
	 [{xmlelement, "value", [],
	   [{xmlcdata, Val}]}]}).

search(State, Data) ->
    Base = State#state.base,
    SearchFilter = State#state.search_filter,
    Eldap_ID = State#state.eldap_id,
    UIDs = State#state.uids,
    Limit = State#state.matches,
    ReportedAttrs = State#state.search_reported_attrs,
    Filter = eldap:'and'([SearchFilter, eldap_utils:make_filter(Data, UIDs)]),
    case eldap_pool:search(Eldap_ID,
                           [{base, Base},
                            {filter, Filter},
                            {limit, Limit},
                            {deref_aliases, State#state.deref_aliases},
                            {attributes, ReportedAttrs}]) of
	#eldap_search_result{entries = E} ->
	    search_items(E, State);
	_ ->
	    error
    end.

search_items(Entries, State) ->
    LServer = State#state.serverhost,
    SearchReported = State#state.search_reported,
    VCardMap = State#state.vcard_map,
	UIDs = State#state.uids,
    Attributes = lists:map(
		   fun(E) ->
			   #eldap_entry{attributes = Attrs} = E,
			   Attrs
		   end, Entries),
    lists:flatmap(
      fun(Attrs) ->
	      case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
	      {U, UIDAttrFormat} ->
	          case eldap_utils:get_user_part(U, UIDAttrFormat) of
		      {ok, Username} ->
		          case ejabberd_auth:is_user_exists(Username, LServer) of
			      true ->
			        RFields = lists:map(
					  fun({_, VCardName}) ->
						  {VCardName,
						   map_vcard_attr(
						     VCardName,
						     Attrs,
						     VCardMap,
						     {Username, ?MYNAME})}
					  end, SearchReported),
			        Result = [?FIELD("jid", Username ++ "@" ++ LServer)] ++
				    [?FIELD(Name, Value) || {Name, Value} <- RFields],
			        [{xmlelement, "item", [], Result}];
			      _ ->
			          []
		          end;
		      _ ->
		         []
	          end;
          "" ->
		      []
		  end
      end, Attributes).

remove_user(_User) ->
    true.

%%%-----------------------
%%% Auxiliary functions.
%%%-----------------------

map_vcard_attr(VCardName, Attributes, Pattern, UD) ->
    Res = lists:filter(
	    fun({Name, _, _}) ->
		    eldap_utils:case_insensitive_match(Name, VCardName)
	    end, Pattern),
    case Res of
	[{_, Str, Attrs}] ->
	    process_pattern(Str, UD,
			    [eldap_utils:get_ldap_attr(X, Attributes) || X<-Attrs]);
	_ -> ""
    end.

process_pattern(Str, {User, Domain}, AttrValues) ->
    eldap_filter:do_sub(
      Str,
      [{"%u", User},{"%d", Domain}] ++
      [{"%s", V, 1} || V <- AttrValues]).

find_xdata_el({xmlelement, _Name, _Attrs, SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) ->
    false;
find_xdata_el1([{xmlelement, Name, Attrs, SubEls} | Els]) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_XDATA ->
	    {xmlelement, Name, Attrs, SubEls};
	_ ->
	    find_xdata_el1(Els)
    end;
find_xdata_el1([_ | Els]) ->
    find_xdata_el1(Els).

parse_options(Host, Opts) ->
    MyHost = gen_mod:get_opt_host(Host, Opts, "vjud.@HOST@"),
    Search = gen_mod:get_opt(search, Opts, true),
    Matches = case gen_mod:get_opt(matches, Opts, 30) of
		infinity  -> 0;
		N         -> N
	    end,
    Eldap_ID = atom_to_list(gen_mod:get_module_proc(Host, ?PROCNAME)),
    LDAPServers = case gen_mod:get_opt(ldap_servers, Opts, undefined) of
		      undefined ->
			  ejabberd_config:get_local_option({ldap_servers, Host});
		      S -> S
		  end,
    LDAPBackups = case gen_mod:get_opt(ldap_backups, Opts, undefined) of
		      undefined ->
			  ejabberd_config:get_local_option({ldap_servers, Host});
		      Backups -> Backups
		  end,
    LDAPEncrypt = case gen_mod:get_opt(ldap_encrypt, Opts, undefined) of
		      undefined ->
			  ejabberd_config:get_local_option({ldap_encrypt, Host});
		      E -> E
	          end,
    LDAPTLSVerify = case gen_mod:get_opt(ldap_tls_verify, Opts, undefined) of
			undefined ->
			    ejabberd_config:get_local_option({ldap_tls_verify, Host});
			Verify -> Verify
		    end,
    LDAPTLSCAFile = case gen_mod:get_opt(ldap_tls_cacertfile, Opts, undefined) of
                        undefined ->
                            ejabberd_config:get_local_option({ldap_tls_cacertfile, Host});
                        CAFile -> CAFile
                    end,
    LDAPTLSDepth = case gen_mod:get_opt(ldap_tls_depth, Opts, undefined) of
                       undefined ->
                           ejabberd_config:get_local_option({ldap_tls_depth, Host});
                       Depth ->
                           Depth
                   end,
    LDAPPortTemp = case gen_mod:get_opt(ldap_port, Opts, undefined) of
		       undefined ->
			   ejabberd_config:get_local_option({ldap_port, Host});
		       PT -> PT
	           end,
    LDAPPort = case LDAPPortTemp of
		   undefined ->
		       case LDAPEncrypt of
			   tls -> ?LDAPS_PORT;
			   starttls -> ?LDAP_PORT;
			   _ -> ?LDAP_PORT
		       end;
		   P -> P
	       end,
    LDAPBase = case gen_mod:get_opt(ldap_base, Opts, undefined) of
		   undefined ->
		       ejabberd_config:get_local_option({ldap_base, Host});
		   B -> B
	       end,
	UIDs = case gen_mod:get_opt(ldap_uids, Opts, undefined) of
	    undefined ->
		    case ejabberd_config:get_local_option({ldap_uids, Host}) of
			undefined -> [{"uid", "%u"}];
			UI -> eldap_utils:uids_domain_subst(Host, UI)
			end;
		UI -> eldap_utils:uids_domain_subst(Host, UI)
		end,
    RootDN = case gen_mod:get_opt(ldap_rootdn, Opts, undefined) of
		 undefined ->
		     case ejabberd_config:get_local_option({ldap_rootdn, Host}) of
			 undefined -> "";
			 RDN -> RDN
		     end;
		 RDN -> RDN
	     end,
    Password = case gen_mod:get_opt(ldap_password, Opts, undefined) of
		   undefined ->
		       case ejabberd_config:get_local_option({ldap_password, Host}) of
			   undefined -> "";
			   Pass -> Pass
		       end;
		   Pass -> Pass
	       end,
    SubFilter = lists:flatten(eldap_utils:generate_subfilter(UIDs)),
    UserFilter = case gen_mod:get_opt(ldap_filter, Opts, undefined) of
		     undefined ->
			 case ejabberd_config:get_local_option({ldap_filter, Host}) of
			     undefined -> SubFilter;
			     "" -> SubFilter;
			     F ->
                                 eldap_utils:check_filter(F),
                                 "(&" ++ SubFilter ++ F ++ ")"
			 end;
		     "" -> SubFilter;
		     F ->
                         eldap_utils:check_filter(F),
                         "(&" ++ SubFilter ++ F ++ ")"
		 end,
    {ok, SearchFilter} = eldap_filter:parse(
			   eldap_filter:do_sub(UserFilter, [{"%u","*"}])),
    VCardMap = gen_mod:get_opt(ldap_vcard_map, Opts, ?VCARD_MAP),
    SearchFields = gen_mod:get_opt(ldap_search_fields, Opts, ?SEARCH_FIELDS),
    SearchReported = gen_mod:get_opt(ldap_search_reported, Opts, ?SEARCH_REPORTED),
    %% In search requests we need to fetch only attributes defined
    %% in vcard-map and search-reported. In some cases,
    %% this will essentially reduce network traffic from an LDAP server.
	UIDAttrs = [UAttr || {UAttr, _} <- UIDs],
    VCardMapAttrs = lists:usort(
		      lists:append([A || {_, _, A} <- VCardMap]) ++ UIDAttrs),
    SearchReportedAttrs =
	lists:usort(lists:flatmap(
		      fun({_, N}) ->
			      case lists:keysearch(N, 1, VCardMap) of
				  {value, {_, _, L}} -> L;
				  _ -> []
			      end
		      end, SearchReported) ++ UIDAttrs),
    DerefAliases = case gen_mod:get_opt(deref_aliases, Opts, undefined) of
                       undefined ->
                           case ejabberd_config:get_local_option(
                                  {deref_aliases, Host}) of
                               undefined -> never;
                               D -> D
                           end;
                       D -> D
                   end,
    #state{serverhost = Host,
	   myhost = MyHost,
	   eldap_id = Eldap_ID,
	   search = Search,
	   servers = LDAPServers,
	   backups = LDAPBackups,
	   port = LDAPPort,
	   tls_options = [{encrypt, LDAPEncrypt},
			  {tls_verify, LDAPTLSVerify},
                          {tls_cacertfile, LDAPTLSCAFile},
                          {tls_depth, LDAPTLSDepth}],
	   dn = RootDN,
	   base = LDAPBase,
	   password = Password,
	   uids = UIDs,
	   vcard_map = VCardMap,
	   vcard_map_attrs = VCardMapAttrs,
	   user_filter = UserFilter,
	   search_filter = SearchFilter,
	   search_fields = SearchFields,
	   search_reported = SearchReported,
	   search_reported_attrs = SearchReportedAttrs,
           deref_aliases = DerefAliases,
	   matches = Matches
	  }.
