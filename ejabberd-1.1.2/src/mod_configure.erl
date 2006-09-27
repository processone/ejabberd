%%%----------------------------------------------------------------------
%%% File    : mod_configure.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Support for online configuration of ejabberd
%%% Created : 19 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_configure).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 get_local_identity/5,
	 get_local_features/5,
	 get_local_items/5,
	 adhoc_local_items/4,
	 adhoc_local_commands/4,
	 get_sm_identity/5,
	 get_sm_features/5,
	 get_sm_items/5,
	 adhoc_sm_items/4,
	 adhoc_sm_commands/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").


start(Host, _Opts) ->
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_local_items, 50),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, get_local_features, 50),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, get_local_identity, 50),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE, get_sm_items, 50),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE, get_sm_identity, 50),
    ejabberd_hooks:add(adhoc_local_items, Host, ?MODULE, adhoc_local_items, 50),
    ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE, adhoc_local_commands, 50),
    ejabberd_hooks:add(adhoc_sm_items, Host, ?MODULE, adhoc_sm_items, 50),
    ejabberd_hooks:add(adhoc_sm_commands, Host, ?MODULE, adhoc_sm_commands, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(adhoc_sm_commands, Host, ?MODULE, adhoc_sm_commands, 50),
    ejabberd_hooks:delete(adhoc_sm_items, Host, ?MODULE, adhoc_sm_items, 50),
    ejabberd_hooks:delete(adhoc_local_commands, Host, ?MODULE, adhoc_local_commands, 50),
    ejabberd_hooks:delete(adhoc_local_items, Host, ?MODULE, adhoc_local_items, 50),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE, get_sm_identity, 50),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE, get_sm_items, 50),
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE, get_local_identity, 50),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_local_features, 50),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_local_items, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_COMMANDS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_COMMANDS).

%%%-----------------------------------------------------------------------

-define(INFO_IDENTITY(Category, Type, Name, Lang),
	[{xmlelement, "identity",
	  [{"category", Category},
	   {"type", Type},
	   {"name", translate:translate(Lang, Name)}], []}]).

-define(INFO_COMMAND(Name, Lang),
	?INFO_IDENTITY("automation", "command-node", Name, Lang)).

get_sm_identity(Acc, _From, _To, Node, Lang) ->
    case Node of
	"config" ->
	    ?INFO_COMMAND("Configuration", Lang);
	_ ->
	    Acc
    end.

get_local_identity(Acc, _From, _To, Node, Lang) ->
    LNode = string:tokens(Node, "/"),
    case LNode of
	["running nodes", ENode] ->
	    ?INFO_IDENTITY("ejabberd", "node", ENode, Lang);
	["running nodes", _ENode, "DB"] ->
	    ?INFO_COMMAND("Database", Lang);
	["running nodes", _ENode, "modules", "start"] ->
	    ?INFO_COMMAND("Start Modules", Lang);
	["running nodes", _ENode, "modules", "stop"] ->
	    ?INFO_COMMAND("Stop Modules", Lang);
	["running nodes", _ENode, "backup", "backup"] ->
	    ?INFO_COMMAND("Backup", Lang);
	["running nodes", _ENode, "backup", "restore"] ->
	    ?INFO_COMMAND("Restore", Lang);
	["running nodes", _ENode, "backup", "textfile"] ->
	    ?INFO_COMMAND("Dump to Text File", Lang);
	["running nodes", _ENode, "import", "file"] ->
	    ?INFO_COMMAND("Import File", Lang);
	["running nodes", _ENode, "import", "dir"] ->
	    ?INFO_COMMAND("Import Directory", Lang);
	["config", "hostname"] ->
	    ?INFO_COMMAND("Host Name", Lang);
	["config", "acls"] ->
	    ?INFO_COMMAND("Access Control Lists", Lang);
	["config", "access"] ->
	    ?INFO_COMMAND("Access Rules", Lang);
	_ ->
	    Acc
    end.

%%%-----------------------------------------------------------------------

-define(INFO_RESULT(Allow, Feats),
    case Allow of
	deny ->
	    {error, ?ERR_FORBIDDEN};
	allow ->
	    {result, Feats}
    end).

get_sm_features(Acc, From, #jid{lserver = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Allow = acl:match_rule(LServer, configure, From),
	    case Node of
		"config" ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		_ ->
		    Acc
	    end
    end.

get_local_features(Acc, From, #jid{lserver = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    LNode = string:tokens(Node, "/"),
	    Allow = acl:match_rule(LServer, configure, From),
	    case LNode of
		["config"] ->
		    ?INFO_RESULT(Allow, []);
		["online users"] ->
		    ?INFO_RESULT(Allow, []);
		["all users"] ->
		    ?INFO_RESULT(Allow, []);
		["all users", [$@ | _]] ->
		    ?INFO_RESULT(Allow, []);
		["outgoing s2s" | _] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes"] ->
		    ?INFO_RESULT(Allow, []);
		["stopped nodes"] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes", _ENode] ->
		    ?INFO_RESULT(Allow, [?NS_STATS]);
		["running nodes", _ENode, "DB"] ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		["running nodes", _ENode, "modules"] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes", _ENode, "modules", _] ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		["running nodes", _ENode, "backup"] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes", _ENode, "backup", _] ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		["running nodes", _ENode, "import"] ->
		    ?INFO_RESULT(Allow, []);
		["running nodes", _ENode, "import", _] ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		["config", _] ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		_ ->
		    Acc
	    end
    end.

%%%-----------------------------------------------------------------------

adhoc_sm_items(Acc, From, #jid{lserver = LServer} = To, Lang) ->
    case acl:match_rule(LServer, configure, From) of
	allow ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    Nodes = [{xmlelement, "item",
		      [{"jid", jlib:jid_to_string(To)},
		       {"name", translate:translate(Lang, "Configuration")},
		       {"node", "config"}], []}],
	    {result, Items ++ Nodes};
	_ ->
	    Acc
    end.

%%%-----------------------------------------------------------------------

get_sm_items(Acc, From,
	     #jid{user = User, server = Server, lserver = LServer} = To,
	     Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    case {acl:match_rule(LServer, configure, From), Node} of
		{allow, ""} ->
		    Nodes = [{xmlelement, "item",
			      [{"jid", jlib:jid_to_string(To)},
			       {"name", translate:translate(Lang, "Configuration")},
			       {"node", "config"}], []}],
		    {result, Items ++ Nodes ++ get_user_resources(User, Server)};
		{allow, "config"} ->
		    {result, []};
		{_, "config"} ->
		    {error, ?ERR_FORBIDDEN};
		_ ->
		    Acc
	    end
    end.

get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    lists:map(fun(R) ->
		      {xmlelement, "item",
		       [{"jid", User ++ "@" ++ Server ++ "/" ++ R},
			{"name", User}], []}
	      end, lists:sort(Rs)).

%%%-----------------------------------------------------------------------

adhoc_local_items(Acc, From, #jid{lserver = LServer, server = Server} = To,
		  Lang) ->
    case acl:match_rule(LServer, configure, From) of
	allow ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    %% Recursively get all configure commands
	    Nodes = recursively_get_local_items(LServer, "", Server, Lang),
	    Nodes1 = lists:filter(
		       fun(N) ->
				Nd = xml:get_tag_attr_s("node", N),
				F = get_local_features([], From, To, Nd, Lang),
				case F of
				    {result, [?NS_COMMANDS]} ->
					true;
				    _ ->
					false
				end
		       end, Nodes),
	    {result, Items ++ Nodes1};
	_ ->
	    Acc
    end.

recursively_get_local_items(_LServer, "online users", _Server, _Lang) ->
    [];

recursively_get_local_items(_LServer, "all users", _Server, _Lang) ->
    [];

recursively_get_local_items(LServer, Node, Server, Lang) ->
    LNode = string:tokens(Node, "/"),
    Items = case get_local_items(LServer, LNode, Server, Lang) of
		{result, Res} ->
		    Res;
		{error, _Error} ->
		    []
	    end,
    Nodes = lists:flatten(
      lists:map(
	fun(N) ->
		S = xml:get_tag_attr_s("jid", N),
		Nd = xml:get_tag_attr_s("node", N),
		if (S /= Server) or (Nd == "") ->
		    [];
		true ->
		    [N, recursively_get_local_items(
			  LServer, Nd, Server, Lang)]
		end
	end, Items)),
    Nodes.

%%%-----------------------------------------------------------------------

-define(ITEMS_RESULT(Allow, LNode, Fallback),
    case Allow of
	deny ->
	    Fallback;
	allow ->
	    case get_local_items(LServer, LNode,
				 jlib:jid_to_string(To), Lang) of
		{result, Res} ->
		    {result, Res};
		{error, Error} ->
		    {error, Error}
	    end
    end).

get_local_items(Acc, From, #jid{lserver = LServer} = To, "", Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, Its} -> Its;
			empty -> []
		    end,
	    Allow = acl:match_rule(LServer, configure, From),
	    case Allow of
		deny ->
		    {result, Items};
		allow ->
		    case get_local_items(LServer, [],
					 jlib:jid_to_string(To), Lang) of
			{result, Res} ->
			    {result, Items ++ Res};
			{error, _Error} ->
			    {result, Items}
		    end
	    end
    end;

get_local_items(Acc, From, #jid{lserver = LServer} = To, Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    LNode = string:tokens(Node, "/"),
	    Allow = acl:match_rule(LServer, configure, From),
	    case LNode of
		["config"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["online users"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["all users"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["all users", [$@ | _]] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["outgoing s2s" | _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["running nodes"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["stopped nodes"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["running nodes", _ENode] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["running nodes", _ENode, "DB"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["running nodes", _ENode, "modules"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["running nodes", _ENode, "modules", _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["running nodes", _ENode, "backup"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["running nodes", _ENode, "backup", _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["running nodes", _ENode, "import"] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["running nodes", _ENode, "import", _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		["config", _] ->
		    ?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
		_ ->
		    Acc
	    end
    end.

%%%-----------------------------------------------------------------------

-define(NODE(Name, Node),
	{xmlelement, "item",
	 [{"jid", Server},
	  {"name", translate:translate(Lang, Name)},
	  {"node", Node}], []}).

get_local_items(_Host, [], Server, Lang) ->
    {result,
     [?NODE("Configuration",            "config"),
      ?NODE("Online Users",             "online users"),
      ?NODE("All Users",                "all users"),
      ?NODE("Outgoing s2s Connections", "outgoing s2s"),
      ?NODE("Running Nodes",            "running nodes"),
      ?NODE("Stopped Nodes",            "stopped nodes")
     ]};

get_local_items(_Host, ["config"], Server, Lang) ->
    {result,
     [?NODE("Host Name",            "config/hostname"),
      ?NODE("Access Control Lists", "config/acls"),
      ?NODE("Access Rules",         "config/access")
      % Too expensive on big hosts
      %?NODE("Remove Users",         "config/remusers")
     ]};

get_local_items(_Host, ["config", _], _Server, _Lang) ->
    {result, []};

get_local_items(Host, ["online users"], _Server, _Lang) ->
    {result, get_online_vh_users(Host)};

get_local_items(Host, ["all users"], _Server, _Lang) ->
    {result, get_all_vh_users(Host)};

get_local_items(_Host, ["all users", [$@ | Diap]], _Server, _Lang) ->
    case catch ejabberd_auth:dirty_get_registered_users() of
	{'EXIT', _Reason} ->
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
		{'EXIT', _Reason} ->
		    ?ERR_NOT_ACCEPTABLE;
		Res ->
		    {result, Res}
	    end
    end;

get_local_items(Host, ["outgoing s2s"], _Server, Lang) ->
    {result, get_outgoing_s2s(Host, Lang)};

get_local_items(Host, ["outgoing s2s", To], _Server, Lang) ->
    {result, get_outgoing_s2s(Host, Lang, To)};

get_local_items(_Host, ["running nodes"], _Server, Lang) ->
    {result, get_running_nodes(Lang)};

get_local_items(_Host, ["stopped nodes"], _Server, Lang) ->
    {result, get_stopped_nodes(Lang)};

get_local_items(_Host, ["running nodes", ENode], Server, Lang) ->
    {result,
     [?NODE("Database", "running nodes/" ++ ENode ++ "/DB"),
      ?NODE("Modules", "running nodes/" ++ ENode ++ "/modules"),
      ?NODE("Backup Management", "running nodes/" ++ ENode ++ "/backup"),
      ?NODE("Import Users From jabberd 1.4 Spool Files",
	    "running nodes/" ++ ENode ++ "/import")
     ]};

get_local_items(_Host, ["running nodes", _ENode, "DB"], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, ["running nodes", ENode, "modules"], Server, Lang) ->
    {result,
     [?NODE("Start Modules", "running nodes/" ++ ENode ++ "/modules/start"),
      ?NODE("Stop Modules",  "running nodes/" ++ ENode ++ "/modules/stop")
     ]};

get_local_items(_Host, ["running nodes", _ENode, "modules", _], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, ["running nodes", ENode, "backup"], Server, Lang) ->
    {result,
     [?NODE("Backup", "running nodes/" ++ ENode ++ "/backup/backup"),
      ?NODE("Restore", "running nodes/" ++ ENode ++ "/backup/restore"),
      ?NODE("Dump to Text File",
	    "running nodes/" ++ ENode ++ "/backup/textfile")
     ]};

get_local_items(_Host, ["running nodes", _ENode, "backup", _], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, ["running nodes", ENode, "import"], Server, Lang) ->
    {result,
     [?NODE("Import File", "running nodes/" ++ ENode ++ "/import/file"),
      ?NODE("Import Directory",  "running nodes/" ++ ENode ++ "/import/dir")
     ]};

get_local_items(_Host, ["running nodes", _ENode, "import", _], _Server, _Lang) ->
    {result, []};

get_local_items(_Host, _, _Server, _Lang) ->
    {error, ?ERR_ITEM_NOT_FOUND}.


get_online_vh_users(Host) ->
    case catch ejabberd_sm:get_vh_session_list(Host) of
	{'EXIT', _Reason} ->
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
	{'EXIT', _Reason} ->
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
	{'EXIT', _Reason} ->
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
	{'EXIT', _Reason} ->
	    [];
	Connections ->
	    lists:map(
	      fun({F, _T}) ->
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


get_running_nodes(_Lang) ->
    case catch mnesia:system_info(running_db_nodes) of
	{'EXIT', _Reason} ->
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

get_stopped_nodes(_Lang) ->
    case catch (lists:usort(mnesia:system_info(db_nodes) ++
			    mnesia:system_info(extra_db_nodes)) --
		mnesia:system_info(running_db_nodes)) of
	{'EXIT', _Reason} ->
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

%-------------------------------------------------------------------------

-define(COMMANDS_RESULT(Allow, From, To, Request),
    case Allow of
	deny ->
	    {error, ?ERR_FORBIDDEN};
	allow ->
	    adhoc_local_commands(From, To, Request)
    end).

adhoc_local_commands(Acc, From, #jid{lserver = LServer} = To,
		     #adhoc_request{node = Node} = Request) ->
    LNode = string:tokens(Node, "/"),
    Allow = acl:match_rule(LServer, configure, From),
    case LNode of
	["running nodes", _ENode, "DB"] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["running nodes", _ENode, "modules", _] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["running nodes", _ENode, "backup", _] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["running nodes", _ENode, "import", _] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	["config", _] ->
	    ?COMMANDS_RESULT(Allow, From, To, Request);
	_ ->
	    Acc
    end.

adhoc_local_commands(_From, #jid{lserver = LServer} = _To,
		     #adhoc_request{lang = Lang,
				    node = Node,
				    sessionid = SessionID,
				    action = Action,
				    xdata = XData} = Request) ->
    LNode = string:tokens(Node, "/"),
    %% If the "action" attribute is not present, it is
    %% understood as "execute".  If there was no <actions/>
    %% element in the first response (which there isn't in our
    %% case), "execute" and "complete" are equivalent.
    ActionIsExecute = lists:member(Action,
				   ["", "execute", "complete"]),
    if	Action == "cancel" ->
	    %% User cancels request
	    adhoc:produce_response(
	      Request, 
	      #adhoc_response{status = canceled});
	XData == false, ActionIsExecute ->
	    %% User requests form
	    case get_form(LServer, LNode, Lang) of
		{result, Form} ->
		    adhoc:produce_response(
		      Request,
		      #adhoc_response{status = executing,
				      elements = Form});
		{error, Error} ->
		    {error, Error}
	    end;
	XData /= false, ActionIsExecute ->
	    %% User returns form.
	    case jlib:parse_xdata_submit(XData) of
		invalid ->
		    {error, ?ERR_BAD_REQUEST};
		Fields ->
		    case set_form(LServer, LNode, Lang, Fields) of
			{result, _Res} ->
			    adhoc:produce_response(
			      #adhoc_response{lang = Lang,
			                      node = Node,
					      sessionid = SessionID,
					      status = completed});
			{error, Error} ->
			    {error, Error}
		    end
	    end;
	true ->
	    {error, ?ERR_BAD_REQUEST}
    end.


-define(TLFIELD(Type, Label, Var),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).

-define(XFIELD(Type, Label, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

-define(TABLEFIELD(Table, Val),
	{xmlelement, "field", [{"type", "list-single"},
			       {"label", atom_to_list(Table)},
			       {"var", atom_to_list(Table)}],
	 [{xmlelement, "value", [], [{xmlcdata, atom_to_list(Val)}]},
	  {xmlelement, "option", [{"label",
				   translate:translate(Lang, "RAM copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "ram_copies"}]}]},
	  {xmlelement, "option", [{"label",
				   translate:translate(Lang,
						       "RAM and disc copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "disc_copies"}]}]},
	  {xmlelement, "option", [{"label",
				   translate:translate(Lang,
						       "Disc only copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "disc_only_copies"}]}]},
	  {xmlelement, "option", [{"label",
				   translate:translate(Lang, "Remote copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "unknown"}]}]}
	 ]}).



get_form(_Host, ["running nodes", ENode, "DB"], Lang) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    case rpc:call(Node, mnesia, system_info, [tables]) of
		{badrpc, _Reason} ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR};
		Tables ->
		    STables = lists:sort(Tables),
		    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
			       [{xmlelement, "title", [],
			         [{xmlcdata,
				   translate:translate(
				     Lang, "Database Tables Configuration at ") ++
				     ENode}]},
			        {xmlelement, "instructions", [],
			         [{xmlcdata,
				   translate:translate(
				     Lang, "Choose storage type of tables")}]} |
			        lists:map(
				  fun(Table) ->
					  case rpc:call(Node,
						        mnesia,
						        table_info,
						        [Table, storage_type]) of
					      {badrpc, _} ->
						  ?TABLEFIELD(Table, unknown);
					      Type ->
						  ?TABLEFIELD(Table, Type)
					  end
				  end, STables)
			     ]}]}
	    end
    end;

get_form(Host, ["running nodes", ENode, "modules", "stop"], Lang) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    case rpc:call(Node, gen_mod, loaded_modules, [Host]) of
		{badrpc, _Reason} ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR};
		Modules ->
		    SModules = lists:sort(Modules),
		    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
			       [{xmlelement, "title", [],
			         [{xmlcdata,
				   translate:translate(
				     Lang, "Stop Modules at ") ++ ENode}]},
			        {xmlelement, "instructions", [],
			         [{xmlcdata,
				   translate:translate(
				     Lang, "Choose modules to stop")}]} |
			        lists:map(fun(M) ->
						  S = atom_to_list(M),
						  ?XFIELD("boolean", S, S, "0")
					  end, SModules)
			     ]}]}
	    end
    end;

get_form(_Host, ["running nodes", ENode, "modules", "start"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Start Modules at ") ++ ENode}]},
	        {xmlelement, "instructions", [],
	         [{xmlcdata,
	           translate:translate(
	             Lang, "Enter list of {Module, [Options]}")}]},
	        {xmlelement, "field", [{"type", "text-multi"},
				       {"label",
				        translate:translate(
					  Lang, "List of modules to start")},
				       {"var", "modules"}],
	         [{xmlelement, "value", [], [{xmlcdata, "[]."}]}]
	        }
	     ]}]};

get_form(_Host, ["running nodes", ENode, "backup", "backup"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Backup to File at ") ++ ENode}]},
	        {xmlelement, "instructions", [],
	         [{xmlcdata,
	           translate:translate(
	             Lang, "Enter path to backup file")}]},
	        {xmlelement, "field", [{"type", "text-single"},
				       {"label",
				        translate:translate(
					  Lang, "Path to File")},
				       {"var", "path"}],
	         [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	        }
	     ]}]};

get_form(_Host, ["running nodes", ENode, "backup", "restore"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Restore Backup from File at ") ++ ENode}]},
	        {xmlelement, "instructions", [],
	         [{xmlcdata,
	           translate:translate(
	             Lang, "Enter path to backup file")}]},
	        {xmlelement, "field", [{"type", "text-single"},
				       {"label",
				        translate:translate(
					  Lang, "Path to File")},
				       {"var", "path"}],
	         [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	        }
	     ]}]};

get_form(_Host, ["running nodes", ENode, "backup", "textfile"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Dump Backup to Text File at ") ++ ENode}]},
	        {xmlelement, "instructions", [],
	         [{xmlcdata,
	           translate:translate(
	             Lang, "Enter path to text file")}]},
	        {xmlelement, "field", [{"type", "text-single"},
				       {"label",
				        translate:translate(
					  Lang, "Path to File")},
				       {"var", "path"}],
	         [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	        }
	     ]}]};

get_form(_Host, ["running nodes", ENode, "import", "file"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Import User from File at ") ++ ENode}]},
	        {xmlelement, "instructions", [],
	         [{xmlcdata,
	           translate:translate(
	             Lang, "Enter path to jabberd1.4 spool file")}]},
	        {xmlelement, "field", [{"type", "text-single"},
				       {"label",
				        translate:translate(
					  Lang, "Path to File")},
				       {"var", "path"}],
	         [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	        }
	     ]}]};

get_form(_Host, ["running nodes", ENode, "import", "dir"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Import Users from Dir at ") ++ ENode}]},
	        {xmlelement, "instructions", [],
	         [{xmlcdata,
	           translate:translate(
	             Lang, "Enter path to jabberd1.4 spool dir")}]},
	        {xmlelement, "field", [{"type", "text-single"},
				       {"label",
				        translate:translate(
					  Lang, "Path to Dir")},
				       {"var", "path"}],
	         [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	        }
	     ]}]};

get_form(_Host, ["config", "hostname"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
		 [{xmlcdata,
		   translate:translate(
		     Lang, "Hostname Configuration")}]},
	        {xmlelement, "instructions", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Choose host name")}]},
	        {xmlelement, "field", [{"type", "text-single"},
				       {"label",
				        translate:translate(Lang,
							    "Host name")},
				       {"var", "hostname"}],
	         [{xmlelement, "value", [], [{xmlcdata, ?MYNAME}]}]}
	     ]}]};

get_form(_Host, ["config", "acls"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Access Control List Configuration")}]},
	        %{xmlelement, "instructions", [],
	        % [{xmlcdata,
	        %   translate:translate(
	        %     Lang, "")}]},
	        {xmlelement, "field", [{"type", "text-multi"},
				       {"label",
				        translate:translate(
					  Lang, "Access control lists")},
				       {"var", "acls"}],
	         lists:map(fun(S) ->
			       {xmlelement, "value", [], [{xmlcdata, S}]}
			   end,
			   string:tokens(
			     lists:flatten(io_lib:format("~p.",
						         [ets:tab2list(acl)])),
			     "\n"))
	        }
	     ]}]};

get_form(_Host, ["config", "access"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Access Configuration")}]},
	        %{xmlelement, "instructions", [],
	        % [{xmlcdata,
	        %   translate:translate(
	        %     Lang, "")}]},
	        {xmlelement, "field", [{"type", "text-multi"},
				       {"label",
				        translate:translate(
					  Lang, "Access rules")},
				       {"var", "access"}],
	         lists:map(fun(S) ->
			       {xmlelement, "value", [], [{xmlcdata, S}]}
			   end,
			   string:tokens(
			     lists:flatten(
			       io_lib:format(
			         "~p.",
			         [ets:select(config,
					     [{{config, {access, '$1'}, '$2'},
					       [],
					       [{{access, '$1', '$2'}}]}])
			         ])),
			     "\n"))
	        }
	     ]}]};

get_form(Host, ["config", "remusers"], Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Remove Users")}]},
	        {xmlelement, "instructions", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Choose users to remove")}]}] ++
      case catch ejabberd_auth:get_vh_registered_users(Host) of
	  {'EXIT', _Reason} ->
	      [];
	  Users ->
	      lists:map(fun(U) ->
			    ?XFIELD("boolean", U, U, "0")
		        end, lists:sort(Users))
      end
    }]};

get_form(_Host, _, _Lang) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.



set_form(_Host, ["running nodes", ENode, "DB"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    lists:foreach(
	      fun({SVar, SVals}) ->
		      % We believe that this is allowed only for good people
		      Table = list_to_atom(SVar),
		      Type = case SVals of
				 ["unknown"] -> unknown;
				 ["ram_copies"] -> ram_copies;
				 ["disc_copies"] -> disc_copies;
				 ["disc_only_copies"] -> disc_only_copies;
				 _ -> false
			     end,
		      if
			  Type == false ->
			      ok;
			  Type == unknown ->
			      mnesia:del_table_copy(Table, Node);
			  true ->
			      case mnesia:add_table_copy(Table, Node, Type) of
				  {aborted, _} ->
				      mnesia:change_table_copy_type(
					Table, Node, Type);
				  _ ->
				      ok
			      end
		      end
	      end, XData),
	    {result, []}
    end;

set_form(Host, ["running nodes", ENode, "modules", "stop"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    lists:foreach(
	      fun({Var, Vals}) ->
		      case Vals of
			  ["1"] ->
			      Module = list_to_atom(Var),
			      rpc:call(Node, gen_mod, stop_module, [Host, Module]);
			  _ ->
			      ok
		      end
	      end, XData),
	    {result, []}
    end;

set_form(Host, ["running nodes", ENode, "modules", "start"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    case lists:keysearch("modules", 1, XData) of
		false ->
		    {error, ?ERR_BAD_REQUEST};
		{value, {_, Strings}} ->
		    String = lists:foldl(fun(S, Res) ->
						 Res ++ S ++ "\n"
					 end, "", Strings),
		    case erl_scan:string(String) of
			{ok, Tokens, _} ->
			    case erl_parse:parse_term(Tokens) of
				{ok, Modules} ->
				    lists:foreach(
				      fun({Module, Args}) ->
					      rpc:call(Node,
						       gen_mod,
						       start_module,
						       [Host, Module, Args])
				      end, Modules),
				    {result, []};
				_ ->
				    {error, ?ERR_BAD_REQUEST}
			    end;
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end
    end;


set_form(_Host, ["running nodes", ENode, "backup", "backup"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, ?ERR_BAD_REQUEST};
		{value, {_, [String]}} ->
		    case rpc:call(Node, mnesia, backup, [String]) of
			{badrpc, _Reason} ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR};
			{error, _Reason} ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR};
			_ ->
			    {result, []}
			end;
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end
    end;


set_form(_Host, ["running nodes", ENode, "backup", "restore"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, ?ERR_BAD_REQUEST};
		{value, {_, [String]}} ->
                    case rpc:call(Node, ejabberd_admin, restore, [String]) of
			{badrpc, _Reason} ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR};
			{error, _Reason} ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR};
			_ ->
			    {result, []}
			end;
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end
    end;


set_form(_Host, ["running nodes", ENode, "backup", "textfile"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, ?ERR_BAD_REQUEST};
		{value, {_, [String]}} ->
		    case rpc:call(Node, mnesia, dump_to_textfile, [String]) of
			{badrpc, _Reason} ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR};
			{error, _Reason} ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR};
			_ ->
			    {result, []}
			end;
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end
    end;


set_form(_Host, ["running nodes", ENode, "import", "file"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, ?ERR_BAD_REQUEST};
		{value, {_, [String]}} ->
		    rpc:call(Node, jd2ejd, import_file, [String]),
		    {result, []};
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end
    end;


set_form(_Host, ["running nodes", ENode, "import", "dir"], _Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, ?ERR_BAD_REQUEST};
		{value, {_, [String]}} ->
		    rpc:call(Node, jd2ejd, import_dir, [String]),
		    {result, []};
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end
    end;


set_form(_Host, ["config", "hostname"], _Lang, XData) ->
    case lists:keysearch("hostname", 1, XData) of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	{value, {_, [""]}} ->
	    {error, ?ERR_BAD_REQUEST};
	{value, {_, [NewName]}} ->
	    ejabberd_config:add_global_option(hostname, NewName),
	    {result, []};
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end;

set_form(_Host, ["config", "acls"], _Lang, XData) ->
    case lists:keysearch("acls", 1, XData) of
	{value, {_, Strings}} ->
	    String = lists:foldl(fun(S, Res) ->
					 Res ++ S ++ "\n"
				 end, "", Strings),
	    case erl_scan:string(String) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, ACLs} ->
			    case acl:add_list(ACLs, true) of
				ok ->
				    {result, []};
				_ ->
				    {error, ?ERR_BAD_REQUEST}
			    end;
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end;
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end;

set_form(_Host, ["config", "access"], _Lang, XData) ->
    SetAccess =
	fun(Rs) ->
		mnesia:transaction(
		  fun() ->
			  Os = mnesia:select(config,
					     [{{config, {access, '$1'}, '$2'},
					       [],
					       ['$_']}]),
			  lists:foreach(fun(O) ->
						mnesia:delete_object(O)
					end, Os),
			  lists:foreach(
			    fun({access, Name, Rules}) ->
				    mnesia:write({config,
						  {access, Name},
						  Rules})
			    end, Rs)
		  end)
	end,
    case lists:keysearch("access", 1, XData) of
	{value, {_, Strings}} ->
	    String = lists:foldl(fun(S, Res) ->
					 Res ++ S ++ "\n"
				 end, "", Strings),
	    case erl_scan:string(String) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, Rs} ->
			    case SetAccess(Rs) of
				{atomic, _} ->
				    {result, []};
				E ->
				    io:format("A: ~p~n", [E]),
				    {error, ?ERR_BAD_REQUEST}
			    end;
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end;
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end;

set_form(Host, ["config", "remusers"], _Lang, XData) ->
    lists:foreach(
      fun({Var, Vals}) ->
	      case Vals of
		  ["1"] ->
		      catch ejabberd_auth:remove_user(Var, Host);
		  _ ->
		      ok
	      end
      end, XData),
    {result, []};

set_form(_Host, _, _Lang, _XData) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.



search_running_node(SNode) ->
    search_running_node(SNode, mnesia:system_info(running_db_nodes)).

search_running_node(_, []) ->
    false;
search_running_node(SNode, [Node | Nodes]) ->
    case atom_to_list(Node) of
	SNode ->
	    Node;
	_ ->
	    search_running_node(SNode, Nodes)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adhoc_sm_commands(_Acc, From,
		  #jid{user = User, server = Server, lserver = LServer} = _To,
		  #adhoc_request{lang = Lang,
				 node = "config",
				 action = Action,
				 xdata = XData} = Request) ->
    case acl:match_rule(LServer, configure, From) of
	deny ->
	    {error, ?ERR_FORBIDDEN};
	allow ->
	    %% If the "action" attribute is not present, it is
	    %% understood as "execute".  If there was no <actions/>
	    %% element in the first response (which there isn't in our
	    %% case), "execute" and "complete" are equivalent.
	    ActionIsExecute = lists:member(Action,
					   ["", "execute", "complete"]),
	    if	Action == "cancel" ->
		    %% User cancels request
		    adhoc:produce_response(
		      Request, 
		      #adhoc_response{status = canceled});
		XData == false, ActionIsExecute ->
		    %% User requests form
		    case get_sm_form(User, Server, "config", Lang) of
			{result, Form} ->
			    adhoc:produce_response(
			      Request,
			      #adhoc_response{status = executing,
					      elements = Form});
			{error, Error} ->
			    {error, Error}
		    end;
		XData /= false, ActionIsExecute ->
		    %% User returns form.
		    case jlib:parse_xdata_submit(XData) of
			invalid ->
			    {error, ?ERR_BAD_REQUEST};
			Fields ->
			    set_sm_form(User, Server, "config", Request, Fields)
		    end;
		true ->
		    {error, ?ERR_BAD_REQUEST}
	    end
    end;

adhoc_sm_commands(Acc, _From, _To, _Request) ->
    Acc.

get_sm_form(User, Server, "config", Lang) ->
    {result, [{xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang, "Administration of ") ++ User}]},
	        {xmlelement, "field",
	         [{"type", "list-single"},
		  {"label", translate:translate(Lang, "Action on user")},
		  {"var", "action"}],
	         [{xmlelement, "value", [], [{xmlcdata, "edit"}]},
		  {xmlelement, "option",
		   [{"label", translate:translate(Lang, "Edit Properties")}],
		   [{xmlelement, "value", [], [{xmlcdata, "edit"}]}]},
		  {xmlelement, "option",
		   [{"label", translate:translate(Lang, "Remove User")}],
		   [{xmlelement, "value", [], [{xmlcdata, "remove"}]}]}
	         ]},
	        ?XFIELD("text-private", "Password", "password",
		        ejabberd_auth:get_password_s(User, Server))
	     ]}]};

get_sm_form(_User, _Server, _Node, _Lang) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.


set_sm_form(User, Server, "config",
	    #adhoc_request{lang = Lang,
	                   node = Node,
			   sessionid = SessionID}, XData) ->
    Response = #adhoc_response{lang = Lang,
                               node = Node,
			       sessionid = SessionID,
			       status = completed},
    case lists:keysearch("action", 1, XData) of
	{value, {_, ["edit"]}} ->
	    case lists:keysearch("password", 1, XData) of
		{value, {_, [Password]}} ->
		    ejabberd_auth:set_password(User, Server, Password),
		    adhoc:produce_response(Response);
		_ ->
		    {error, ?ERR_NOT_ACCEPTABLE}
	    end;
	{value, {_, ["remove"]}} ->
	    catch ejabberd_auth:remove_user(User, Server),
	    adhoc:produce_response(Response);
	_ ->
	    {error, ?ERR_NOT_ACCEPTABLE}
    end;

set_sm_form(_User, _Server, _Node, _Request, _Fields) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.

