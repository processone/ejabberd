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

-export([start/1,
	 stop/0,
	 process_local_iq_items/3,
	 process_local_iq_info/3,
	 process_sm_iq_items/3,
	 process_sm_iq_info/3,
	 register_feature/1,
	 unregister_feature/1,
	 register_extra_domain/1,
	 unregister_extra_domain/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(EMPTY_INFO_RESULT,
	IQ#iq{type = result,
	      sub_el = [{xmlelement, "query",
			 [{"xmlns", ?NS_DISCO_INFO},
			  {"node", SNode}], []}]}).

start(Opts) ->
    ejabberd_local:refresh_iq_handlers(),

    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_DISCO_ITEMS,
				  ?MODULE, process_local_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_DISCO_INFO,
				  ?MODULE, process_local_iq_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_DISCO_ITEMS,
				  ?MODULE, process_sm_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_DISCO_INFO,
				  ?MODULE, process_sm_iq_info, IQDisc),

    catch ets:new(disco_features, [named_table, ordered_set, public]),
    register_feature("iq"),
    register_feature("presence"),
    register_feature("presence-invisible"),

    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ExtraDomains = gen_mod:get_opt(extra_domains, Opts, []),
    lists:foreach(fun register_extra_domain/1, ExtraDomains),
    ok.

stop() ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ?NS_DISCO_INFO),
    catch ets:delete(disco_features),
    catch ets:delete(disco_extra_domains),
    ok.


register_feature(Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:insert(disco_features, {Feature}).

unregister_feature(Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:delete(disco_features, Feature).

register_extra_domain(Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:insert(disco_extra_domains, {Domain}).

unregister_extra_domain(Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:delete(disco_extra_domains, Domain).

process_local_iq_items(From, To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    SNode = xml:get_tag_attr_s("node", SubEl),
	    Node = string:tokens(SNode, "/"),

	    case acl:match_rule(configure, From) of
		deny when Node /= [] ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
		deny ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", ?NS_DISCO_ITEMS}],
				     get_services_only()
				    }]};
		_ ->
		    case get_local_items(Node, jlib:jid_to_string(To), Lang) of
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


process_local_iq_info(From, _To, #iq{type = Type, xmlns = XMLNS,
				     sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    SNode = xml:get_tag_attr_s("node", SubEl),
	    Node = string:tokens(SNode, "/"),
	    case {acl:match_rule(configure, From), Node} of
		{_, []} ->
		    Features = lists:map(fun feature_to_xml/1,
					 ets:tab2list(disco_features)),
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
				      feature_to_xml({?NS_STATS})
				     ]
				    }]};
		{allow, ["running nodes", ENode, "DB"]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement,
				     "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml({?NS_EJABBERD_CONFIG})]}]};
		{allow, ["running nodes", ENode, "modules"]} ->
		    ?EMPTY_INFO_RESULT;
		{allow, ["running nodes", ENode, "modules", _]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml({?NS_EJABBERD_CONFIG})]}]};
		{allow, ["running nodes", ENode, "backup"]} ->
		    ?EMPTY_INFO_RESULT;
		{allow, ["running nodes", ENode, "backup", _]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml({?NS_EJABBERD_CONFIG})]}]};
		{allow, ["running nodes", ENode, "import"]} ->
		    ?EMPTY_INFO_RESULT;
		{allow, ["running nodes", ENode, "import", _]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml({?NS_EJABBERD_CONFIG})]}]};
		{allow, ["config", _]} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS},
				      {"node", SNode}],
				     [feature_to_xml({?NS_EJABBERD_CONFIG})]}]};
		_ ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
	    end
    end.


feature_to_xml({Feature}) ->
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


get_services_only() ->
    lists:map(fun domain_to_xml/1,
	      ejabberd_router:dirty_get_all_routes()) ++
	lists:map(fun domain_to_xml/1, ets:tab2list(disco_extra_domains)).

get_local_items([], Server, Lang) ->
    Domains =
	lists:map(fun domain_to_xml/1,
		  ejabberd_router:dirty_get_all_routes()) ++
	lists:map(fun domain_to_xml/1, ets:tab2list(disco_extra_domains)),
    {result,
     Domains ++
     [?NODE("Configuration",            "config"),
      ?NODE("Online Users",             "online users"),
      ?NODE("All Users",                "all users"),
      ?NODE("Outgoing S2S connections", "outgoing s2s"),
      ?NODE("Running Nodes",            "running nodes"),
      ?NODE("Stopped Nodes",            "stopped nodes")
     ]};

get_local_items(["config"], Server, Lang) ->
    {result,
     [?NODE("Host Name",            "config/hostname"),
      ?NODE("Access Control Lists", "config/acls"),
      ?NODE("Access Rules",         "config/access"),
      ?NODE("Remove Users",         "config/remusers")
     ]};

get_local_items(["config", _], Server, Lang) ->
    {result, []};

get_local_items(["online users"], Server, Lang) ->
    {result, get_online_users()};

get_local_items(["all users"], Server, Lang) ->
    {result, get_all_users()};

get_local_items(["all users", [$@ | Diap]], Server, Lang) ->
    case catch ejabberd_auth:dirty_get_registered_users() of
	{'EXIT', Reason} ->
	    ?ERR_INTERNAL_SERVER_ERROR;
	Users ->
	    SUsers = lists:sort(Users),
	    case catch begin
			   {ok, [S1, S2]} = regexp:split(Diap, "-"),
			   N1 = list_to_integer(S1),
			   N2 = list_to_integer(S2),
			   Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
			   lists:map(fun(U) ->
					     {xmlelement, "item",
					      [{"jid", U ++ "@" ++ ?MYNAME},
					       {"name", U}], []}
				     end, Sub)
		       end of
		{'EXIT', Reason} ->
		    % TODO: must be "not acceptable"
		    ?ERR_BAD_REQUEST;
		Res ->
		    {result, Res}
	    end
    end;

get_local_items(["outgoing s2s"], Server, Lang) ->
    {result, get_outgoing_s2s(Lang)};

get_local_items(["outgoing s2s", To], Server, Lang) ->
    {result, get_outgoing_s2s(Lang, To)};

get_local_items(["running nodes"], Server, Lang) ->
    {result, get_running_nodes(Lang)};

get_local_items(["stopped nodes"], Server, Lang) ->
    {result, get_stopped_nodes(Lang)};

get_local_items(["running nodes", ENode], Server, Lang) ->
    {result,
     [?NODE("DB", "running nodes/" ++ ENode ++ "/DB"),
      ?NODE("Modules", "running nodes/" ++ ENode ++ "/modules"),
      ?NODE("Backup Management", "running nodes/" ++ ENode ++ "/backup"),
      ?NODE("Import users from jabberd1.4 spool files",
	    "running nodes/" ++ ENode ++ "/import")
     ]};

get_local_items(["running nodes", ENode, "DB"], Server, Lang) ->
    {result, []};

get_local_items(["running nodes", ENode, "modules"], Server, Lang) ->
    {result,
     [?NODE("Start Modules", "running nodes/" ++ ENode ++ "/modules/start"),
      ?NODE("Stop Modules",  "running nodes/" ++ ENode ++ "/modules/stop")
     ]};

get_local_items(["running nodes", ENode, "modules", _], Server, Lang) ->
    {result, []};

get_local_items(["running nodes", ENode, "backup"], Server, Lang) ->
    {result,
     [?NODE("Backup", "running nodes/" ++ ENode ++ "/backup/backup"),
      ?NODE("Restore", "running nodes/" ++ ENode ++ "/backup/restore"),
      ?NODE("Dump to Text File",
	    "running nodes/" ++ ENode ++ "/backup/textfile")
     ]};

get_local_items(["running nodes", ENode, "backup", _], Server, Lang) ->
    {result, []};

get_local_items(["running nodes", ENode, "import"], Server, Lang) ->
    {result,
     [?NODE("Import File", "running nodes/" ++ ENode ++ "/import/file"),
      ?NODE("Import Directory",  "running nodes/" ++ ENode ++ "/import/dir")
     ]};

get_local_items(["running nodes", ENode, "import", _], Server, Lang) ->
    {result, []};

get_local_items(_, _, _) ->
    {error, ?ERR_FEATURE_NOT_IMPLEMENTED}.





get_online_users() ->
    case catch ejabberd_sm:dirty_get_sessions_list() of
	{'EXIT', Reason} ->
	    [];
	URs ->
	    lists:map(fun({U, R}) ->
			      {xmlelement, "item",
			       [{"jid", U ++ "@" ++ ?MYNAME ++ "/" ++ R},
				{"name", U}], []}
		      end, lists:sort(URs))
    end.

get_all_users() ->
    case catch ejabberd_auth:dirty_get_registered_users() of
	{'EXIT', Reason} ->
	    [];
	Users ->
	    SUsers = lists:sort(Users),
	    case length(SUsers) of
		N when N =< 100 ->
		    lists:map(fun(U) ->
				      {xmlelement, "item",
				       [{"jid", U ++ "@" ++ ?MYNAME},
					{"name", U}], []}
			      end, SUsers);
		N ->
		    NParts = trunc(math:sqrt(N * 0.618)) + 1,
		    M = trunc(N / NParts) + 1,
		    lists:map(fun(K) ->
				      L = K + M - 1,
				      Node =
					  "@" ++ integer_to_list(K) ++
					  "-" ++ integer_to_list(L),
				      Last = if L < N -> lists:nth(L, SUsers);
						true -> lists:last(SUsers)
					     end,
				      Name = 
					  lists:nth(K, SUsers) ++ " -- " ++
					  Last,
				      {xmlelement, "item",
				       [{"jid", ?MYNAME},
					{"node", "all users/" ++ Node},
					{"name", Name}], []}
			      end, lists:seq(1, N, M))
	    end
    end.

get_outgoing_s2s(Lang) ->
    case catch ejabberd_s2s:dirty_get_connections() of
	{'EXIT', Reason} ->
	    [];
	Connections ->
	    TConns = [element(2, C) || C <- Connections],
	    lists:map(
	      fun(T) ->
		      {xmlelement, "item",
		       [{"jid", ?MYNAME},
			{"node", "outgoing s2s/" ++ T},
			{"name",
			 lists:flatten(
			   io_lib:format(
			     translate:translate(Lang, "To ~s"), [T]))}],
		       []}
	      end, lists:usort(TConns))
    end.

get_outgoing_s2s(Lang, To) ->
    case catch ejabberd_s2s:dirty_get_connections() of
	{'EXIT', Reason} ->
	    [];
	Connections ->
	    lists:map(
	      fun({F, T}) ->
		      {xmlelement, "item",
		       [{"jid", ?MYNAME},
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

process_sm_iq_items(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    #jid{user = User} = To,
    case {acl:match_rule(configure, From), Type} of
	{deny, _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	{allow, set} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	{allow, get} ->
	    case xml:get_tag_attr_s("node", SubEl) of
		"" ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", ?NS_DISCO_ITEMS}],
				     get_user_resources(User)
				    }]};
		_ ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
	    end
    end.


process_sm_iq_info(From, To, #iq{type = Type, xmlns = XMLNS,
				 sub_el = SubEl} = IQ) ->
    case {acl:match_rule(configure, From), Type} of
	{deny, _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	{allow, set} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	{allow, get} ->
	    case xml:get_tag_attr_s("node", SubEl) of
		"" ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query", [{"xmlns", XMLNS}],
				     [feature_to_xml({?NS_EJABBERD_CONFIG})]}]};
		_ ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
	    end
    end.



get_user_resources(User) ->
    Rs = ejabberd_sm:get_user_resources(User),
    lists:map(fun(R) ->
		      {xmlelement, "item",
		       [{"jid", User ++ "@" ++ ?MYNAME ++ "/" ++ R},
			{"name", User}], []}
	      end, lists:sort(Rs)).

