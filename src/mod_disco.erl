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
    case Type of
	set ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	get ->
	    case xml:get_tag_attr_s("node", SubEl) of
		"" ->
		    Domains =
			lists:map(fun domain_to_xml/1,
				  ejabberd_router:dirty_get_all_routes()),
		    {iq, ID, result, XMLNS,
		     [{xmlelement,
		       "query",
		       [{"xmlns", ?NS_DISCO_ITEMS}],
		       Domains ++
		       [{xmlelement, "item",
			 [{"jid", jlib:jid_to_string(To)},
			  {"name", "Online Users"},
			  {"node", "online users"}], []},
			{xmlelement, "item",
			 [{"jid", jlib:jid_to_string(To)},
			  {"name", "All Users"},
			  {"node", "all users"}], []}]
		      }]};
		"online users" ->
		    {iq, ID, result, XMLNS,
		     [{xmlelement, "query", [{"xmlns", ?NS_DISCO_ITEMS}],
		       get_online_users()
		      }]};
		"all users" ->
		    {iq, ID, result, XMLNS,
		     [{xmlelement, "query", [{"xmlns", ?NS_DISCO_ITEMS}],
		       get_all_users()
		      }]};
		_ ->
		    {iq, ID, error, XMLNS,
		     [SubEl, {xmlelement, "error",
			      [{"code", "501"}],
			      [{xmlcdata, "Not Implemented"}]}]}
	    end
    end.


process_local_iq_info(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	get ->
	    case xml:get_tag_attr_s("node", SubEl) of
		"" ->
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
		"online users" -> ?EMPTY_INFO_RESULT;
		"all users" -> ?EMPTY_INFO_RESULT;
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

