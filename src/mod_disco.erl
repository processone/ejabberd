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
	 unregister_feature/1]).

-include("ejabberd.hrl").
-include("namespaces.hrl").

-define(EMPTY_INFO_RESULT,
	{iq, ID, result, XMLNS, [{xmlelement, "query",
				  [{"xmlns", ?NS_DISCO_INFO}], []}]}).

start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_DISCO_ITEMS,
				  ?MODULE, process_local_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_DISCO_INFO,
				  ?MODULE, process_local_iq_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_DISCO_ITEMS,
				  ?MODULE, process_sm_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_DISCO_INFO,
				  ?MODULE, process_sm_iq_info, IQDisc),
    register_feature("iq"),
    register_feature("presence"),
    register_feature("presence-invisible"),
    ok.

stop() ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ?NS_DISCO_INFO).


register_feature(Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:insert(disco_features, {Feature}).

unregister_feature(Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:delete(disco_features, Feature).

process_local_iq_items(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    Lang = xml:get_tag_attr_s("xml:lang", SubEl),
    case Type of
	set ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	get ->
	    Node = string:tokens(xml:get_tag_attr_s("node", SubEl), "/"),

	    case get_local_items(Node, jlib:jid_to_string(To), Lang) of
		{result, Res} ->
		    {iq, ID, result, XMLNS,
		     [{xmlelement, "query", [{"xmlns", ?NS_DISCO_ITEMS}],
		       Res
		      }]};
		{error, Code, Desc} ->
		    {iq, ID, error, XMLNS,
		     [SubEl, {xmlelement, "error",
			      [{"code", Code}],
			      [{xmlcdata, Desc}]}]}
	    end
    end.


process_local_iq_info(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	get ->
	    case string:tokens(xml:get_tag_attr_s("node", SubEl), "/") of
		[] ->
		    Features = lists:map(fun feature_to_xml/1,
					 ets:tab2list(disco_features)),
		    {iq, ID, result, XMLNS, [{xmlelement,
					      "query",
					      [{"xmlns", ?NS_DISCO_INFO}],
					      [{xmlelement, "identity",
						[{"category", "service"},
						 {"type", "jabber"},
						 {"name", "ejabberd"}], []}] ++
					      Features
					     }]};
		["config"] -> ?EMPTY_INFO_RESULT;
		["online users"] -> ?EMPTY_INFO_RESULT;
		["all users"] -> ?EMPTY_INFO_RESULT;
		["all users", [$@ | _]] -> ?EMPTY_INFO_RESULT;
		["outgoing s2s" | _] -> ?EMPTY_INFO_RESULT;
		["running nodes"] -> ?EMPTY_INFO_RESULT;
		["stopped nodes"] -> ?EMPTY_INFO_RESULT;
		["running nodes", ENode] ->
		    {iq, ID, result, XMLNS, [{xmlelement,
					      "query",
					      [{"xmlns", XMLNS}],
					      [{xmlelement, "identity",
						[{"category", "ejabberd"},
						 {"type", "node"},
						 {"name", ENode}], []},
					       feature_to_xml({?NS_STATS})
					      ]
					     }]};
		["running nodes", ENode, "DB"] ->
		    {iq, ID, result, XMLNS, [{xmlelement,
					      "query",
					      [{"xmlns", XMLNS}],
					      [feature_to_xml({?NS_IQDATA})
					      ]
					     }]};
		["running nodes", ENode, "modules"] -> ?EMPTY_INFO_RESULT;
		["running nodes", ENode, "modules", _] ->
		    {iq, ID, result, XMLNS,
		     [{xmlelement, "query", [{"xmlns", XMLNS}],
		       [feature_to_xml({?NS_IQDATA})]}]};
		["running nodes", ENode, "backup"] -> ?EMPTY_INFO_RESULT;
		["running nodes", ENode, "backup", _] ->
		    {iq, ID, result, XMLNS,
		     [{xmlelement, "query", [{"xmlns", XMLNS}],
		       [feature_to_xml({?NS_IQDATA})]}]};
		["running nodes", ENode, "import"] -> ?EMPTY_INFO_RESULT;
		["running nodes", ENode, "import", _] ->
		    {iq, ID, result, XMLNS,
		     [{xmlelement, "query", [{"xmlns", XMLNS}],
		       [feature_to_xml({?NS_IQDATA})]}]};
		["config", _] ->
		    {iq, ID, result, XMLNS,
		     [{xmlelement, "query", [{"xmlns", XMLNS}],
		       [feature_to_xml({?NS_IQDATA})]}]};
		_ ->
		    {iq, ID, error, XMLNS,
		     [SubEl, {xmlelement, "error",
			      [{"code", "501"}],
			      [{xmlcdata, "Not Implemented"}]}]}
	    end
    end.


feature_to_xml({Feature}) ->
    {xmlelement, "feature", [{"var", Feature}], []}.

domain_to_xml(Domain) ->
    {xmlelement, "item", [{"jid", Domain}], []}.


-define(NODE(Name, Node),
	{xmlelement, "item",
	 [{"jid", Server},
	  {"name", translate:translate(Lang, Name)},
	  {"node", Node}], []}).


get_local_items([], Server, Lang) ->
    Domains =
	lists:map(fun domain_to_xml/1,
		  ejabberd_router:dirty_get_all_routes()),
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
     [?NODE("Host Name",      "config/hostname"),
      ?NODE("ACLs",           "config/acls"),
      ?NODE("Access Rules",   "config/access"),
      ?NODE("Remove Users",   "config/remusers")
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
	    {error, "500", "Internal Server Error"};
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
		    {error, "406", "Not Acceptable"};
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
    {error, "501", "Not Implemented"}.





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
	    lists:map(
	      fun({F, T}) ->
		      {xmlelement, "item",
		       [{"jid", ?MYNAME},
			{"node", "outgoing s2s/" ++ T},
			{"name",
			 lists:flatten(
			   io_lib:format(
			     translate:translate(Lang, "To ~s"), [T]))}],
		       []}
	      end, lists:keysort(2, Connections))
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

process_sm_iq_items(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    {User, _, _} = To,
    case Type of
	set ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	get ->
	    case xml:get_tag_attr_s("node", SubEl) of
		"" ->
		    {iq, ID, result, XMLNS,
		     [{xmlelement, "query", [{"xmlns", ?NS_DISCO_ITEMS}],
		       get_user_resources(User)
		      }]};
		_ ->
		    {iq, ID, error, XMLNS,
		     [SubEl, {xmlelement, "error",
			      [{"code", "501"}],
			      [{xmlcdata, "Not Implemented"}]}]}
	    end
    end.


process_sm_iq_info(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	get ->
	    case xml:get_tag_attr_s("node", SubEl) of
		"" ->
		    {iq, ID, result, XMLNS,
		     [{xmlelement, "query", [{"xmlns", XMLNS}],
		       [feature_to_xml({?NS_IQDATA})]}]};
		_ ->
		    {iq, ID, error, XMLNS,
		     [SubEl, {xmlelement, "error",
			      [{"code", "501"}],
			      [{xmlcdata, "Not Implemented"}]}]}
	    end
    end.



get_user_resources(User) ->
    Rs = ejabberd_sm:get_user_resources(User),
    lists:map(fun(R) ->
		      {xmlelement, "item",
		       [{"jid", User ++ "@" ++ ?MYNAME ++ "/" ++ R},
			{"name", User}], []}
	      end, lists:sort(Rs)).

