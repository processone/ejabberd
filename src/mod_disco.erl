%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_disco).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0, init/0,
	 process_local_iq_items/3,
	 process_local_iq_info/3,
	 process_sm_iq_items/3,
	 process_sm_iq_info/3,
	 register_feature/1]).

-include("ejabberd.hrl").
-include("namespaces.hrl").

-define(EMPTY_INFO_RESULT,
	{iq, ID, result, XMLNS, [{xmlelement, "query",
				  [{"xmlns", ?NS_DISCO_INFO}], []}]}).

start() ->
    ejabberd_local:register_iq_handler(?NS_DISCO_ITEMS,
				       ?MODULE, process_local_iq_items),
    ejabberd_local:register_iq_handler(?NS_DISCO_INFO,
				       ?MODULE, process_local_iq_info),
    ejabberd_sm:register_iq_handler(?NS_DISCO_ITEMS,
        			    ?MODULE, process_sm_iq_items),
    ejabberd_sm:register_iq_handler(?NS_DISCO_INFO,
        			    ?MODULE, process_sm_iq_info),
    register_feature("iq"),
    register_feature("presence"),
    register_feature("presence-invisible"),
    ok.

init() ->
    ok.

register_feature(Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:insert(disco_features, {Feature}).

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
		["online users"] -> ?EMPTY_INFO_RESULT;
		["all users"] -> ?EMPTY_INFO_RESULT;
		["outgoing s2s" | _] -> ?EMPTY_INFO_RESULT;
		["running nodes"] -> ?EMPTY_INFO_RESULT;
		["stopped nodes"] -> ?EMPTY_INFO_RESULT;
		["running nodes", ENode] ->
		    {iq, ID, result, XMLNS, [{xmlelement,
					      "query",
					      [{"xmlns", ?NS_DISCO_INFO}],
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
					      [{"xmlns", ?NS_DISCO_INFO}],
					      [feature_to_xml({?NS_XDATA})
					      ]
					     }]};
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
       [?NODE("Online Users",             "online users"),
	?NODE("All Users",                "all users"),
	?NODE("Outgoing S2S connections", "outgoing s2s"),
	?NODE("Running Nodes",            "running nodes"),
	?NODE("Stopped Nodes",            "stopped nodes")
       ]};

get_local_items(["online users"], Server, Lang) ->
    {result, get_online_users()};

get_local_items(["all users"], Server, Lang) ->
    {result, get_all_users()};

get_local_items(["outgoing s2s"], Server, Lang) ->
    {result, get_outgoing_s2s(Lang)};

get_local_items(["running nodes"], Server, Lang) ->
    {result, get_running_nodes(Lang)};

get_local_items(["stopped nodes"], Server, Lang) ->
    {result, get_stopped_nodes(Lang)};

get_local_items(["running nodes", ENode], Server, Lang) ->
    {result,
     [?NODE("DB", "running nodes/" ++ ENode ++ "/DB")]};

get_local_items(["running nodes", ENode, "DB"], Server, Lang) ->
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
	    lists:map(fun(U) ->
			      {xmlelement, "item",
			       [{"jid", U ++ "@" ++ ?MYNAME},
				{"name", U}], []}
		      end, lists:sort(Users))
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
		"" -> ?EMPTY_INFO_RESULT;
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

