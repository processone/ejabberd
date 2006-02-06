
%%%----------------------------------------------------------------------
%%% File    : mod_vcard_ldap.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_vcard_ldap).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/2, init/3, stop/1,
 	 get_sm_features/5,
	 process_local_iq/3,
	 process_sm_iq/3,
	 remove_user/1]).

-include("ejabberd.hrl").
-include("eldap/eldap.hrl").
-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_vcard_ldap).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_VCARD,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    LDAPServers = ejabberd_config:get_local_option({ldap_servers, Host}),
    RootDN = ejabberd_config:get_local_option({ldap_rootdn, Host}),
    Password = ejabberd_config:get_local_option({ldap_password, Host}),
    eldap:start_link("mod_vcard_ldap", LDAPServers, 389, RootDN, Password),
    MyHost = gen_mod:get_opt(host, Opts, "vjud." ++ Host),
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
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
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
			       [{xmlcdata,
				 "http://ejabberd.jabberstudio.org/"}]},
			      {xmlelement, "DESC", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang,
				   "Erlang Jabber Server\n"
				   "Copyright (c) 2002-2006 Alexey Shchepin")}]},
			      {xmlelement, "BDAY", [],
			       [{xmlcdata, "2002-11-16"}]}
			     ]}]}
    end.

find_ldap_user(Host, User) ->
    Attr = ejabberd_config:get_local_option({ldap_uidattr, Host}),
    Filter = eldap:equalityMatch(Attr, User),
    Base = ejabberd_config:get_local_option({ldap_base, Host}),
    case eldap:search("mod_vcard_ldap", [{base, Base},
					 {filter, Filter},
					 {attributes, []}]) of
	#eldap_search_result{entries = [E | _]} ->
	    E;
	_ ->
	    false
    end.

is_attribute_read_allowed(Name,From,To) ->
    true.

ldap_attribute_to_vcard(Prefix,{Name,Values},From,To) ->
    case is_attribute_read_allowed(Name,From,To) of 
	true ->
	    ldap_lca_to_vcard(Prefix,stringprep:tolower(Name),Values);
	_ ->
	    none
    end.

ldap_lca_to_vcard(vCard,"displayname",[Value|_]) ->
    {xmlelement,"FN",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(vCard,"uid",[Value|_]) ->
    {xmlelement,"NICKNAME",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(vCard,"title",[Value|_]) ->
    {xmlelement,"TITLE",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(vCard,"labeleduri",[Value|_]) ->
    {xmlelement,"URL",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(vCard,"description",[Value|_]) ->
    {xmlelement,"DESC",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(vCard,"telephonenumber",[Value|_]) ->
    {xmlelement,"TEL",[],[{xmlelement,"VOICE",[],[]},
			  {xmlelement,"WORK",[],[]},
			  {xmlelement,"NUMBER",[],[{xmlcdata,Value}]}]};

ldap_lca_to_vcard(vCard,"mail",[Value|_]) ->
    {xmlelement,"EMAIL",[],[{xmlelement,"INTERNET",[],[]},
			    {xmlelement,"PREF",[],[]},
			    {xmlelement,"USERID",[],[{xmlcdata,Value}]}]};

ldap_lca_to_vcard(vCardN,"sn",[Value|_]) ->
    {xmlelement,"FAMILY",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(vCardN,"givenname",[Value|_]) ->
    {xmlelement,"GIVEN",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(vCardN,"initials",[Value|_]) ->
    {xmlelement,"MIDDLE",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(vCardO,"o",[Value|_]) ->
    {xmlelement,"ORGNAME",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(vCardO,"ou",[Value|_]) ->
    {xmlelement,"ORGUNIT",[],[{xmlcdata,Value}]};

ldap_lca_to_vcard(_,_,_) -> none.

ldap_attributes_to_vcard(Attributes,From,To) ->
    Elts = lists:map(fun(Attr) ->
			     ldap_attribute_to_vcard(vCard,Attr,From,To)
		     end,Attributes),
    FElts = [ X || X <- Elts, X /= none ],
    NElts = lists:map(fun(Attr) ->
			      ldap_attribute_to_vcard(vCardN,Attr,From,To)
		      end,Attributes),
    FNElts = [ X || X <- NElts, X /= none ],
    OElts = lists:map(fun(Attr) ->
			      ldap_attribute_to_vcard(vCardO,Attr,From,To)
		      end,Attributes),
    FOElts = [ X || X <- OElts, X /= none ],
    [{xmlelement, "vCard", [{"xmlns", ?NS_VCARD}],
      lists:append(FElts,
		   [{xmlelement,"N",[],FNElts},
		    {xmlelement,"ORG",[],FOElts}])
     }].

process_sm_iq(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    #jid{luser = LUser, lserver = LServer} = To,
	    case find_ldap_user(LServer, LUser) of
		#eldap_entry{attributes = Attributes} ->
		    Vcard = ldap_attributes_to_vcard(Attributes,From,To),
		    IQ#iq{type = result, sub_el = Vcard};
		_ ->
		    IQ#iq{type = result, sub_el = []}
	    end
	end.

-define(TLFIELD(Type, Label, Var),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).


-define(FORM(JID),
	[{xmlelement, "instructions", [],
	  [{xmlcdata, translate:translate(Lang, "You need an x:data capable client to search")}]},
	 {xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
	  [{xmlelement, "title", [],
	    [{xmlcdata, translate:translate(Lang, "Search users in ") ++
	      jlib:jid_to_string(JID)}]},
	   {xmlelement, "instructions", [],
	    [{xmlcdata, translate:translate(Lang, "Fill in fields to search "
					    "for any matching Jabber User")}]},
	   ?TLFIELD("text-single", "User", "user"),
	   ?TLFIELD("text-single", "Full Name", "fn"),
	   ?TLFIELD("text-single", "Given Name", "given"),
	   ?TLFIELD("text-single", "Middle Name", "middle"),
	   ?TLFIELD("text-single", "Family Name", "family"),
	   ?TLFIELD("text-single", "Nickname", "nickname"),
	   ?TLFIELD("text-single", "Birthday", "bday"),
	   ?TLFIELD("text-single", "Country", "ctry"),
	   ?TLFIELD("text-single", "City", "locality"),
	   ?TLFIELD("text-single", "email", "email"),
	   ?TLFIELD("text-single", "Organization Name", "orgname"),
	   ?TLFIELD("text-single", "Organization Unit", "orgunit")
	  ]}]).




do_route(ServerHost, From, To, Packet) ->
    #jid{user = User, resource = Resource} = To,
    if
	(User /= "") or (Resource /= "") ->
	    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
	    ejabberd_router ! {route, To, From, Err};
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
						      search_result(Lang, To, ServerHost, XData)
						     }]}]},
					    ejabberd_router:route(
					      To, From, jlib:iq_to_xml(ResIQ))
				    end
			    end;
			get ->
			    ResIQ = IQ#iq{type = result,
					  sub_el = [{xmlelement,
						     "query",
						     [{"xmlns", ?NS_SEARCH}],
						     ?FORM(To)
						    }]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = Type, xmlns = ?NS_DISCO_INFO, sub_el = SubEl} ->
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
						 [{"xmlns", ?NS_DISCO_INFO}],
						 [{xmlelement, "identity",
						   [{"category", "directory"},
						    {"type", "user"},
						    {"name",
						     "vCard User Search"}],
						   []},
						  {xmlelement, "feature",
						   [{"var", ?NS_SEARCH}], []},
						  {xmlelement, "feature",
						   [{"var", ?NS_VCARD}], []}
						 ]
						}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = Type, xmlns = ?NS_DISCO_ITEMS, sub_el = SubEl} ->
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
      [{xmlcdata,
        "http://ejabberd.jabberstudio.org/"}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, translate:translate(
		    Lang,
		    "ejabberd vCard module\n"
		    "Copyright (c) 2003-2006 Alexey Shchepin")}]}].

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

-define(LFIELD(Label, Var),
	{xmlelement, "field", [{"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).

search_result(Lang, JID, ServerHost, Data) ->
    [{xmlelement, "title", [],
      [{xmlcdata, translate:translate(Lang, "Results of search in ") ++
	jlib:jid_to_string(JID)}]},
     {xmlelement, "reported", [],
      [?LFIELD("JID", "jid"),
       ?LFIELD("Full Name", "fn"),
       ?LFIELD("Given Name", "given"),
       ?LFIELD("Middle Name", "middle"),
       ?LFIELD("Family Name", "family"),
       ?LFIELD("Nickname", "nickname"),
       ?LFIELD("Birthday", "bday"),
       ?LFIELD("Country", "ctry"),
       ?LFIELD("City", "locality"),
       ?LFIELD("email", "email"),
       ?LFIELD("Organization Name", "orgname"),
       ?LFIELD("Organization Unit", "orgunit")
      ]}] ++ lists:map(fun(E) -> 
			       record_to_item(E#eldap_entry.attributes)
		       end, search(ServerHost, Data)).

-define(FIELD(Var, Val),
	{xmlelement, "field", [{"var", Var}],
	 [{xmlelement, "value", [],
	   [{xmlcdata, Val}]}]}).

case_exact_compare(none,_) ->
    false;
case_exact_compare(_,none) ->
    false;
case_exact_compare(X,Y) ->
    X > Y.

ldap_sort_entries(L) ->
    lists:sort(fun(E1,E2) ->
		       case_exact_compare(ldap_get_value(E1,"cn"),ldap_get_value(E2,"cn"))
	       end,L).

ldap_get_value(E,Attribute) ->
    #eldap_entry{attributes = Attributes} = E,
    case lists:filter(fun({A,_}) ->
			      string:equal(A,Attribute)
		      end,Attributes) of
	[{Attr,[Value|_]}] ->
	    Value;
	_ -> 
	    none
    end.
    

ldap_attribute_to_item("uid",Value) ->
    [
     ?FIELD("jid",Value ++ "@" ++ ?MYNAME),
     ?FIELD("uid",Value),
     ?FIELD("nickname",Value)
    ];

ldap_attribute_to_item("displayname",Value) ->
    [
     ?FIELD("fn",Value)
    ];

ldap_attribute_to_item("sn",Value) ->
    [
     ?FIELD("family",Value)
    ];

ldap_attribute_to_item("displayname",Value) ->
    [
     ?FIELD("fn",Value)
    ];

ldap_attribute_to_item("givenname",Value) ->
    [
     ?FIELD("given",Value)
    ];

ldap_attribute_to_item("initials",Value) ->
    [
     ?FIELD("middle",Value)
    ];

ldap_attribute_to_item("mail",Value) ->
    [
     ?FIELD("email",Value)
    ];

ldap_attribute_to_item("o",Value) ->
    [
     ?FIELD("orgname",Value)
    ];

ldap_attribute_to_item("ou",Value) ->
    [
     ?FIELD("orgunit",Value)
    ];

ldap_attribute_to_item(_,_) ->
    [none].

record_to_item(Attributes) ->
    List = lists:append(lists:map(fun({Attr,[Value|_]}) -> 
					  ldap_attribute_to_item(stringprep:tolower(Attr),Value)
				  end,Attributes)),
    FList = [X || X <- List, X /= none],
    {xmlelement, "item", [],FList}.

search(LServer, Data) ->
    Filter = make_filter(Data),
    Base = ejabberd_config:get_local_option({ldap_base, LServer}),
    UIDAttr = ejabberd_config:get_local_option({ldap_uidattr, LServer}),
    case eldap:search("mod_vcard_ldap",[{base, Base},
					{filter, Filter},
					{attributes, []}]) of
	#eldap_search_result{entries = E} ->
	    [X || X <- E,
		  ejabberd_auth:is_user_exists(
		    ldap_get_value(X, UIDAttr), LServer)];
	Err ->
	    ?ERROR_MSG("Bad search: ~p", [[LServer, {base, Base},
					{filter, Filter},
					{attributes, []}]])
    end.


make_filter(Data) ->
    Filter = [X || X <- lists:map(fun(R) -> 
					  make_assertion(R)
				  end, Data),
		   X /= none ],
    case Filter of
	[F] -> 
	    F;
	_ ->
	    eldap:'and'(Filter)
    end.


make_assertion("givenName",Value) ->
    eldap:substrings("givenName",[{any,Value}]);

make_assertion("cn",Value) ->
    eldap:substrings("cn",[{any,Value}]);

make_assertion("sn",Value) ->
    eldap:substrings("sn",[{any,Value}]);

make_assertion(Attr, Value) ->
    eldap:equalityMatch(Attr,Value).

make_assertion({SVar, [Val]}) ->
    LAttr = ldap_attribute(SVar),
    case LAttr of
	none ->
	    none;
	_ ->
	    if 
		is_list(Val) and (Val /= "") ->
		    make_assertion(LAttr,Val);
		true ->
		    none
	    end
    end.

ldap_attribute("user") ->
    "uid";

ldap_attribute("fn") ->
    "cn";

ldap_attribute("family") ->
    "sn";

ldap_attribute("given") ->
    "givenName";

ldap_attribute("middle") ->
    "initials";

ldap_attribute("email") ->
    "mail";

ldap_attribute("orgname") ->
    "o";

ldap_attribute("orgunit") ->
    "ou";

ldap_attribute(_) ->
    none.

remove_user(User) ->
    true.

