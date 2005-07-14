%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Service Discovery (JEP-0030) support
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_disco).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_local_iq_items/3,
	 process_local_iq_info/3,
	 process_sm_iq_items/3,
	 process_sm_iq_info/3,
	 register_feature/2,
	 unregister_feature/2,
	 register_extra_domain/2,
	 unregister_extra_domain/2,
	 register_sm_feature/2,
	 unregister_sm_feature/2,
	 register_sm_node/4,
	 unregister_sm_node/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(EMPTY_INFO_RESULT,
	IQ#iq{type = result,
	      sub_el = [{xmlelement, "query",
			 [{"xmlns", ?NS_DISCO_INFO},
			  {"node", SNode}], []}]}).

start(Host, Opts) ->
    ejabberd_local:refresh_iq_handlers(),

    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
				  ?MODULE, process_local_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
				  ?MODULE, process_local_iq_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_DISCO_ITEMS,
				  ?MODULE, process_sm_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_DISCO_INFO,
				  ?MODULE, process_sm_iq_info, IQDisc),

    catch ets:new(disco_features, [named_table, ordered_set, public]),
    register_feature(Host, "iq"),
    register_feature(Host, "presence"),
    register_feature(Host, "presence-invisible"),

    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ExtraDomains = gen_mod:get_opt(extra_domains, Opts, []),
    lists:foreach(fun(Domain) -> register_extra_domain(Host, Domain) end,
		  ExtraDomains),
    catch ets:new(disco_sm_features, [named_table, ordered_set, public]),
    catch ets:new(disco_sm_nodes, [named_table, ordered_set, public]),
    ok.

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_INFO),
    catch ets:match_delete(disco_features, {{'_', Host}}),
    catch ets:match_delete(disco_extra_domains, {{'_', Host}}),
    ok.


register_feature(Host, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:insert(disco_features, {{Feature, Host}}).

unregister_feature(Host, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:delete(disco_features, {Feature, Host}).

register_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:insert(disco_extra_domains, {{Domain, Host}}).

unregister_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:delete(disco_extra_domains, {Domain, Host}).

process_local_iq_items(From, To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    SNode = xml:get_tag_attr_s("node", SubEl),
	    Node = string:tokens(SNode, "/"),
	    Host = To#jid.lserver,

	    case acl:match_rule(Host, configure, From) of
		deny when Node /= [] ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
		deny ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", ?NS_DISCO_ITEMS}],
				     get_services_only(Host)
				    }]};
		_ ->
		    case get_local_items(Host, Node,
					 jlib:jid_to_string(To), Lang) of
			{result, Res} ->
			    IQ#iq{type = result,
				  sub_el = [{xmlelement, "query",
					     [{"xmlns", ?NS_DISCO_ITEMS},
					      {"node", SNode}],
					     Res
					    }]};
			{error, Error} ->
			    IQ#iq{type = error, sub_el = [SubEl, Error]}
		    end
	    end
    end.


process_local_iq_info(From, To, #iq{type = Type, xmlns = XMLNS,
				     sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    LServer = To#jid.lserver,
	    SNode = xml:get_tag_attr_s("node", SubEl),
	    Node = string:tokens(SNode, "/"),
	    case {acl:match_rule(LServer, configure, From), Node} of
		{_, []} ->
		    Features = lists:map(
				 fun feature_to_xml/1,
				 ets:select(disco_features,
					    [{{{'$1', LServer}},
					      [],
					      ['$1']}])),
		    IQ#iq{type = result,
			  sub_el = [{xmlelement,
				     "query",
				     [{"xmlns", ?NS_DISCO_INFO}],
				     [{xmlelement, "identity",
				       [{"category", "server"},
					{"type", "im"},
					{"name", "ejabberd"}], []}] ++
				     Features
				    }]};
		{deny, _} ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
		{allow, ["config"]} -> ?EMPTY_INFO_RESULT;
		{allow, ["online users"]} -> ?EMPTY_INFO_RESULT;
		{allow, ["all users"]} -> ?EMPTY_INFO_RESULT;
		{allow, ["all users", [$@ | _]]} -> ?EMPTY_INFO_RESULT;
		{allow, ["outgoing s2s" | _]} -> ?EMPTY_INFO_RESULT;
		{allow, ["running nodes"]} -> ?EMPTY_INFO_RESULT;
		{allow, ["stopped nodes"]} -> ?EMPTY_INFO_RESULT;
		{allow, ["running nodes", ENode]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement,
				     "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [{xmlelement, "identity",
				       [{"category", "ejabberd"},
					{"type", "node"},
					{"name", ENode}], []},
				      feature_to_xml(?NS_STATS)
				     ]
				    }]};
		{allow, ["running nodes", ENode, "DB"]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement,
				     "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml(?NS_EJABBERD_CONFIG)]}]};
		{allow, ["running nodes", ENode, "modules"]} ->
		    ?EMPTY_INFO_RESULT;
		{allow, ["running nodes", ENode, "modules", _]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml(?NS_EJABBERD_CONFIG)]}]};
		{allow, ["running nodes", ENode, "backup"]} ->
		    ?EMPTY_INFO_RESULT;
		{allow, ["running nodes", ENode, "backup", _]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml(?NS_EJABBERD_CONFIG)]}]};
		{allow, ["running nodes", ENode, "import"]} ->
		    ?EMPTY_INFO_RESULT;
		{allow, ["running nodes", ENode, "import", _]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml(?NS_EJABBERD_CONFIG)]}]};
		{allow, ["config", _]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml(?NS_EJABBERD_CONFIG)]}]};
		_ ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
	    end
    end.


feature_to_xml({{Feature, _Host}}) ->
    feature_to_xml(Feature);
feature_to_xml(Feature) when is_list(Feature) ->
    {xmlelement, "feature", [{"var", Feature}], []}.

domain_to_xml({Domain}) ->
    {xmlelement, "item", [{"jid", Domain}], []};
domain_to_xml(Domain) ->
    {xmlelement, "item", [{"jid", Domain}], []}.

-define(NODE(Name, Node),
	{xmlelement, "item",
	 [{"jid", Server},
	  {"name", translate:translate(Lang, Name)},
	  {"node", Node}], []}).


get_services_only(Host) ->
    lists:map(fun domain_to_xml/1,
	      get_vh_services(Host)) ++
	lists:map(fun domain_to_xml/1,
		  ets:select(disco_extra_domains,
			     [{{{'$1', Host}},
			       [],
			       ['$1']}])).

get_local_items(Host, [], Server, Lang) ->
    Domains =
	lists:map(fun domain_to_xml/1,
		  get_vh_services(Host)) ++
	lists:map(fun domain_to_xml/1,
		  ets:select(disco_extra_domains,
			     [{{{'$1', Host}},
			       [],
			       ['$1']}])),
    {result,
     Domains ++
     [?NODE("Configuration",            "config"),
      ?NODE("Online Users",             "online users"),
      ?NODE("All Users",                "all users"),
      ?NODE("Outgoing S2S connections", "outgoing s2s"),
      ?NODE("Running Nodes",            "running nodes"),
      ?NODE("Stopped Nodes",            "stopped nodes")
     ]};

get_local_items(Host, ["config"], Server, Lang) ->
    {result,
     [?NODE("Host Name",            "config/hostname"),
      ?NODE("Access Control Lists", "config/acls"),
      ?NODE("Access Rules",         "config/access")
      % Too expensive on big hosts
      %?NODE("Remove Users",         "config/remusers")
     ]};

get_local_items(Host, ["config", _], Server, Lang) ->
    {result, []};

get_local_items(Host, ["online users"], Server, Lang) ->
    {result, get_online_vh_users(Host)};

get_local_items(Host, ["all users"], Server, Lang) ->
    {result, get_all_vh_users(Host)};

get_local_items(Host, ["all users", [$@ | Diap]], Server, Lang) ->
    case catch ejabberd_auth:dirty_get_registered_users() of
	{'EXIT', Reason} ->
	    ?ERR_INTERNAL_SERVER_ERROR;
	Users ->
	    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
	    case catch begin
			   {ok, [S1, S2]} = regexp:split(Diap, "-"),
			   N1 = list_to_integer(S1),
			   N2 = list_to_integer(S2),
			   Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
			   lists:map(fun({S, U}) ->
					     {xmlelement, "item",
					      [{"jid", U ++ "@" ++ S},
					       {"name", U ++ "@" ++ S}], []}
				     end, Sub)
		       end of
		{'EXIT', Reason} ->
		    ?ERR_NOT_ACCEPTABLE;
		Res ->
		    {result, Res}
	    end
    end;

get_local_items(Host, ["outgoing s2s"], Server, Lang) ->
    {result, get_outgoing_s2s(Host, Lang)};

get_local_items(Host, ["outgoing s2s", To], Server, Lang) ->
    {result, get_outgoing_s2s(Host, Lang, To)};

get_local_items(Host, ["running nodes"], Server, Lang) ->
    {result, get_running_nodes(Lang)};

get_local_items(Host, ["stopped nodes"], Server, Lang) ->
    {result, get_stopped_nodes(Lang)};

get_local_items(Host, ["running nodes", ENode], Server, Lang) ->
    {result,
     [?NODE("DB", "running nodes/" ++ ENode ++ "/DB"),
      ?NODE("Modules", "running nodes/" ++ ENode ++ "/modules"),
      ?NODE("Backup Management", "running nodes/" ++ ENode ++ "/backup"),
      ?NODE("Import users from jabberd1.4 spool files",
	    "running nodes/" ++ ENode ++ "/import")
     ]};

get_local_items(Host, ["running nodes", ENode, "DB"], Server, Lang) ->
    {result, []};

get_local_items(Host, ["running nodes", ENode, "modules"], Server, Lang) ->
    {result,
     [?NODE("Start Modules", "running nodes/" ++ ENode ++ "/modules/start"),
      ?NODE("Stop Modules",  "running nodes/" ++ ENode ++ "/modules/stop")
     ]};

get_local_items(Host, ["running nodes", ENode, "modules", _], Server, Lang) ->
    {result, []};

get_local_items(Host, ["running nodes", ENode, "backup"], Server, Lang) ->
    {result,
     [?NODE("Backup", "running nodes/" ++ ENode ++ "/backup/backup"),
      ?NODE("Restore", "running nodes/" ++ ENode ++ "/backup/restore"),
      ?NODE("Dump to Text File",
	    "running nodes/" ++ ENode ++ "/backup/textfile")
     ]};

get_local_items(Host, ["running nodes", ENode, "backup", _], Server, Lang) ->
    {result, []};

get_local_items(Host, ["running nodes", ENode, "import"], Server, Lang) ->
    {result,
     [?NODE("Import File", "running nodes/" ++ ENode ++ "/import/file"),
      ?NODE("Import Directory",  "running nodes/" ++ ENode ++ "/import/dir")
     ]};

get_local_items(Host, ["running nodes", ENode, "import", _], Server, Lang) ->
    {result, []};

get_local_items(_Host, _, _, _) ->
    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}.



get_vh_services(Host) ->
    Hosts = lists:sort(fun(H1, H2) -> length(H1) >= length(H2) end, ?MYHOSTS),
    lists:filter(fun(H) ->
			 case lists:dropwhile(
				fun(VH) ->
					not lists:suffix("." ++ VH, H)
				end, Hosts) of
			     [] ->
				 false;
			     [VH | _] ->
				 VH == Host
			 end
		 end, ejabberd_router:dirty_get_all_routes()).

get_online_vh_users(Host) ->
    case catch ejabberd_sm:get_vh_session_list(Host) of
	{'EXIT', Reason} ->
	    [];
	USRs ->
	    SURs = lists:sort([{S, U, R} || {U, S, R} <- USRs]),
	    lists:map(fun({S, U, R}) ->
			      {xmlelement, "item",
			       [{"jid", U ++ "@" ++ S ++ "/" ++ R},
				{"name", U ++ "@" ++ S}], []}
		      end, SURs)
    end.

get_all_vh_users(Host) ->
    case catch ejabberd_auth:get_vh_registered_users(Host) of
	{'EXIT', Reason} ->
	    [];
	Users ->
	    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
	    case length(SUsers) of
		N when N =< 100 ->
		    lists:map(fun({S, U}) ->
				      {xmlelement, "item",
				       [{"jid", U ++ "@" ++ S},
					{"name", U ++ "@" ++ S}], []}
			      end, SUsers);
		N ->
		    NParts = trunc(math:sqrt(N * 0.618)) + 1,
		    M = trunc(N / NParts) + 1,
		    lists:map(fun(K) ->
				      L = K + M - 1,
				      Node =
					  "@" ++ integer_to_list(K) ++
					  "-" ++ integer_to_list(L),
				      {FS, FU} = lists:nth(K, SUsers),
				      {LS, LU} =
					  if L < N -> lists:nth(L, SUsers);
					     true -> lists:last(SUsers)
					  end,
				      Name = 
					  FU ++ "@" ++ FS ++
					  " -- " ++
					  LU ++ "@" ++ LS,
				      {xmlelement, "item",
				       [{"jid", Host},
					{"node", "all users/" ++ Node},
					{"name", Name}], []}
			      end, lists:seq(1, N, M))
	    end
    end.

get_outgoing_s2s(Host, Lang) ->
    case catch ejabberd_s2s:dirty_get_connections() of
	{'EXIT', Reason} ->
	    [];
	Connections ->
	    DotHost = "." ++ Host,
	    TConns = [TH || {FH, TH} <- Connections,
			    Host == FH orelse lists:suffix(DotHost, FH)],
	    lists:map(
	      fun(T) ->
		      {xmlelement, "item",
		       [{"jid", Host},
			{"node", "outgoing s2s/" ++ T},
			{"name",
			 lists:flatten(
			   io_lib:format(
			     translate:translate(Lang, "To ~s"), [T]))}],
		       []}
	      end, lists:usort(TConns))
    end.

get_outgoing_s2s(Host, Lang, To) ->
    case catch ejabberd_s2s:dirty_get_connections() of
	{'EXIT', Reason} ->
	    [];
	Connections ->
	    lists:map(
	      fun({F, T}) ->
		      {xmlelement, "item",
		       [{"jid", Host},
			{"node", "outgoing s2s/" ++ To ++ "/" ++ F},
			{"name",
			 lists:flatten(
			   io_lib:format(
			     translate:translate(Lang, "From ~s"), [F]))}],
		       []}
	      end, lists:keysort(1, lists:filter(fun(E) ->
							 element(2, E) == To
						 end, Connections)))
    end.


get_running_nodes(Lang) ->
    case catch mnesia:system_info(running_db_nodes) of
	{'EXIT', Reason} ->
	    [];
	DBNodes ->
	    lists:map(
	      fun(N) ->
		      S = atom_to_list(N),
		      {xmlelement, "item",
		       [{"jid", ?MYNAME},
			{"node", "running nodes/" ++ S},
			{"name", S}],
		       []}
	      end, lists:sort(DBNodes))
    end.

get_stopped_nodes(Lang) ->
    case catch (lists:usort(mnesia:system_info(db_nodes) ++
			    mnesia:system_info(extra_db_nodes)) --
		mnesia:system_info(running_db_nodes)) of
	{'EXIT', Reason} ->
	    [];
	DBNodes ->
	    lists:map(
	      fun(N) ->
		      S = atom_to_list(N),
		      {xmlelement, "item",
		       [{"jid", ?MYNAME},
			{"node", "stopped nodes/" ++ S},
			{"name", S}],
		       []}
	      end, lists:sort(DBNodes))
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_sm_feature(Host, Feature) ->
    catch ets:new(disco_sm_features, [named_table, ordered_set, public]),
    ets:insert(disco_sm_features, {{Feature, Host}}).

unregister_sm_feature(Host, Feature) ->
    catch ets:new(disco_sm_features, [named_table, ordered_set, public]),
    ets:delete(disco_sm_features, {Feature, Host}).

register_sm_node(Node, Name, Module, Function) ->
    catch ets:new(disco_sm_nodes, [named_table, ordered_set, public]),
    ets:insert(disco_sm_nodes, {Node, Name, Module, Function}).

unregister_sm_node(Node) ->
    catch ets:new(disco_sm_nodes, [named_table, ordered_set, public]),
    ets:delete(disco_sm_nodes, Node).

process_sm_iq_items(From, To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    #jid{user = User, luser = LTo} = To,
    #jid{luser = LFrom, lserver = LServer} = From,
    Self = (LTo == LFrom) andalso (LServer == ?MYNAME),
    Node = xml:get_tag_attr_s("node", SubEl),
    case {acl:match_rule(To#jid.lserver, configure, From), Type, Self, Node} of
	{_, set, _, _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	{_, get, true, []}  ->
	    Nodes = lists:map(fun({Nod, Name, _, _}) ->
					node_to_xml(User,
					Nod,
					translate:translate(Lang, Name))
			      end, ets:tab2list(disco_sm_nodes)),
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_DISCO_ITEMS}],
			     get_user_resources(User) ++ Nodes}]};
	{allow, get, _, []} ->
	    Nodes = lists:map(fun({Nod, Name, _, _}) ->
					node_to_xml(User,
					Nod,
					translate:translate(Lang, Name))
			      end, ets:tab2list(disco_sm_nodes)),
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_DISCO_ITEMS}],
			     get_user_resources(User) ++ Nodes}]};
	{A, get, S, _} when (A == allow) or (S == true) ->
	    case ets:lookup(disco_sm_nodes, Node) of
		[] ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
		[{Node, _Name, Module, Function}] ->
		    case Module:Function(From, To, IQ) of
			{error, Err} ->
			    IQ#iq{type = error, sub_el = [SubEl, Err]};
			{result, Res} ->
			    IQ#iq{type = result,
				  sub_el = [{xmlelement, "query",
					     [{"xmlns", ?NS_DISCO_ITEMS},
					      {"node", Node}],
					     Res}]}
		    end
	    end;
	{_, get, _, _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]};
	_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.


process_sm_iq_info(From, To, #iq{type = Type, xmlns = XMLNS,
				 sub_el = SubEl} = IQ) ->
    #jid{luser = LTo} = To,
    #jid{luser = LFrom, lserver = LServer} = From,
    Self = (LTo == LFrom) andalso (LServer == ?MYNAME),
    Node = xml:get_tag_attr_s("node", SubEl),
    case {acl:match_rule(To#jid.lserver, configure, From), Type, Self, Node} of
	{_, set, _, _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	{allow, get, _, []} ->
	    Features = lists:map(fun feature_to_xml/1,
				 ets:tab2list(disco_sm_features)),
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query", [{"xmlns", XMLNS}],
			     [feature_to_xml(?NS_EJABBERD_CONFIG)] ++
			     Features}]};
	{_, get, _, []} ->
	    Features = lists:map(fun feature_to_xml/1,
				 ets:tab2list(disco_sm_features)),
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query", [{"xmlns", XMLNS}],
			    Features}]};
	{A, get, S, _} when (A == allow) or (S == true) ->
	    case ets:lookup(disco_sm_nodes, Node) of
		[] ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
		_ ->
		    IQ#iq{type = result, sub_el = [{xmlelement, "query",
						   [{"xmlns", XMLNS},
						    {"node", Node}], []}]}
	    end;
	{_, get, _, _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]};
	_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.



get_user_resources(User) ->
    Rs = ejabberd_sm:get_user_resources(User, 'TODO'),
    lists:map(fun(R) ->
		      {xmlelement, "item",
		       [{"jid", User ++ "@" ++ ?MYNAME ++ "/" ++ R},
			{"name", User}], []}
	      end, lists:sort(Rs)).

node_to_xml(User, Node, Name) ->
    {xmlelement, "item", [{"jid", User ++ "@" ++ ?MYNAME},
			  {"node", Node},
			  {"name", Name}], []}.

