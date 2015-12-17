%%%----------------------------------------------------------------------
%%% File    : mod_configure.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for online configuration of ejabberd
%%% Created : 19 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_configure).

-author('alexey@process-one.net').

-protocol({xep, 133, '1.1'}).

-behaviour(gen_mod).

-export([start/2, stop/1, get_local_identity/5,
	 get_local_features/5, get_local_items/5,
	 adhoc_local_items/4, adhoc_local_commands/4,
	 get_sm_identity/5, get_sm_features/5, get_sm_items/5,
	 adhoc_sm_items/4, adhoc_sm_commands/4, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("adhoc.hrl").

-define(T(Lang, Text), translate:translate(Lang, Text)).

%% Copied from ejabberd_sm.erl
-record(session, {sid, usr, us, priority, info}).

start(Host, _Opts) ->
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE,
		       get_local_items, 50),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
		       get_local_features, 50),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE,
		       get_local_identity, 50),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE,
		       get_sm_items, 50),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       get_sm_features, 50),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE,
		       get_sm_identity, 50),
    ejabberd_hooks:add(adhoc_local_items, Host, ?MODULE,
		       adhoc_local_items, 50),
    ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE,
		       adhoc_local_commands, 50),
    ejabberd_hooks:add(adhoc_sm_items, Host, ?MODULE,
		       adhoc_sm_items, 50),
    ejabberd_hooks:add(adhoc_sm_commands, Host, ?MODULE,
		       adhoc_sm_commands, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(adhoc_sm_commands, Host, ?MODULE,
			  adhoc_sm_commands, 50),
    ejabberd_hooks:delete(adhoc_sm_items, Host, ?MODULE,
			  adhoc_sm_items, 50),
    ejabberd_hooks:delete(adhoc_local_commands, Host,
			  ?MODULE, adhoc_local_commands, 50),
    ejabberd_hooks:delete(adhoc_local_items, Host, ?MODULE,
			  adhoc_local_items, 50),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE,
			  get_sm_identity, 50),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  get_sm_features, 50),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE,
			  get_sm_items, 50),
    ejabberd_hooks:delete(disco_local_identity, Host,
			  ?MODULE, get_local_identity, 50),
    ejabberd_hooks:delete(disco_local_features, Host,
			  ?MODULE, get_local_features, 50),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE,
			  get_local_items, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_COMMANDS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_COMMANDS).

%%%-----------------------------------------------------------------------

-define(INFO_IDENTITY(Category, Type, Name, Lang),
	[#xmlel{name = <<"identity">>,
		attrs =
		    [{<<"category">>, Category}, {<<"type">>, Type},
		     {<<"name">>, ?T(Lang, Name)}],
		children = []}]).

-define(INFO_COMMAND(Name, Lang),
	?INFO_IDENTITY(<<"automation">>, <<"command-node">>,
		       Name, Lang)).

-define(NODEJID(To, Name, Node),
	#xmlel{name = <<"item">>,
	       attrs =
		   [{<<"jid">>, jid:to_string(To)},
		    {<<"name">>, ?T(Lang, Name)}, {<<"node">>, Node}],
	       children = []}).

-define(NODE(Name, Node),
	#xmlel{name = <<"item">>,
	       attrs =
		   [{<<"jid">>, Server}, {<<"name">>, ?T(Lang, Name)},
		    {<<"node">>, Node}],
	       children = []}).

-define(NS_ADMINX(Sub),
	<<(?NS_ADMIN)/binary, "#", Sub/binary>>).

-define(NS_ADMINL(Sub),
	[<<"http:">>, <<"jabber.org">>, <<"protocol">>,
	 <<"admin">>, Sub]).

tokenize(Node) -> str:tokens(Node, <<"/#">>).

get_sm_identity(Acc, _From, _To, Node, Lang) ->
    case Node of
      <<"config">> ->
	  ?INFO_COMMAND(<<"Configuration">>, Lang);
      _ -> Acc
    end.

get_local_identity(Acc, _From, _To, Node, Lang) ->
    LNode = tokenize(Node),
    case LNode of
      [<<"running nodes">>, ENode] ->
	  ?INFO_IDENTITY(<<"ejabberd">>, <<"node">>, ENode, Lang);
      [<<"running nodes">>, _ENode, <<"DB">>] ->
	  ?INFO_COMMAND(<<"Database">>, Lang);
      [<<"running nodes">>, _ENode, <<"modules">>,
       <<"start">>] ->
	  ?INFO_COMMAND(<<"Start Modules">>, Lang);
      [<<"running nodes">>, _ENode, <<"modules">>,
       <<"stop">>] ->
	  ?INFO_COMMAND(<<"Stop Modules">>, Lang);
      [<<"running nodes">>, _ENode, <<"backup">>,
       <<"backup">>] ->
	  ?INFO_COMMAND(<<"Backup">>, Lang);
      [<<"running nodes">>, _ENode, <<"backup">>,
       <<"restore">>] ->
	  ?INFO_COMMAND(<<"Restore">>, Lang);
      [<<"running nodes">>, _ENode, <<"backup">>,
       <<"textfile">>] ->
	  ?INFO_COMMAND(<<"Dump to Text File">>, Lang);
      [<<"running nodes">>, _ENode, <<"import">>,
       <<"file">>] ->
	  ?INFO_COMMAND(<<"Import File">>, Lang);
      [<<"running nodes">>, _ENode, <<"import">>,
       <<"dir">>] ->
	  ?INFO_COMMAND(<<"Import Directory">>, Lang);
      [<<"running nodes">>, _ENode, <<"restart">>] ->
	  ?INFO_COMMAND(<<"Restart Service">>, Lang);
      [<<"running nodes">>, _ENode, <<"shutdown">>] ->
	  ?INFO_COMMAND(<<"Shut Down Service">>, Lang);
      ?NS_ADMINL(<<"add-user">>) ->
	  ?INFO_COMMAND(<<"Add User">>, Lang);
      ?NS_ADMINL(<<"delete-user">>) ->
	  ?INFO_COMMAND(<<"Delete User">>, Lang);
      ?NS_ADMINL(<<"end-user-session">>) ->
	  ?INFO_COMMAND(<<"End User Session">>, Lang);
      ?NS_ADMINL(<<"get-user-password">>) ->
	  ?INFO_COMMAND(<<"Get User Password">>, Lang);
      ?NS_ADMINL(<<"change-user-password">>) ->
	  ?INFO_COMMAND(<<"Change User Password">>, Lang);
      ?NS_ADMINL(<<"get-user-lastlogin">>) ->
	  ?INFO_COMMAND(<<"Get User Last Login Time">>, Lang);
      ?NS_ADMINL(<<"user-stats">>) ->
	  ?INFO_COMMAND(<<"Get User Statistics">>, Lang);
      ?NS_ADMINL(<<"get-registered-users-num">>) ->
	  ?INFO_COMMAND(<<"Get Number of Registered Users">>,
			Lang);
      ?NS_ADMINL(<<"get-online-users-num">>) ->
	  ?INFO_COMMAND(<<"Get Number of Online Users">>, Lang);
      [<<"config">>, <<"acls">>] ->
	  ?INFO_COMMAND(<<"Access Control Lists">>, Lang);
      [<<"config">>, <<"access">>] ->
	  ?INFO_COMMAND(<<"Access Rules">>, Lang);
      _ -> Acc
    end.

%%%-----------------------------------------------------------------------

-define(INFO_RESULT(Allow, Feats),
	case Allow of
	  deny -> {error, ?ERR_FORBIDDEN};
	  allow -> {result, Feats}
	end).

get_sm_features(Acc, From,
		#jid{lserver = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  Allow = acl:match_rule(LServer, configure, From),
	  case Node of
	    <<"config">> -> ?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    _ -> Acc
	  end
    end.

get_local_features(Acc, From,
		   #jid{lserver = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  LNode = tokenize(Node),
	  Allow = acl:match_rule(LServer, configure, From),
	  case LNode of
	    [<<"config">>] -> ?INFO_RESULT(Allow, []);
	    [<<"user">>] -> ?INFO_RESULT(Allow, []);
	    [<<"online users">>] -> ?INFO_RESULT(Allow, []);
	    [<<"all users">>] -> ?INFO_RESULT(Allow, []);
	    [<<"all users">>, <<$@, _/binary>>] ->
		?INFO_RESULT(Allow, []);
	    [<<"outgoing s2s">> | _] -> ?INFO_RESULT(Allow, []);
	    [<<"running nodes">>] -> ?INFO_RESULT(Allow, []);
	    [<<"stopped nodes">>] -> ?INFO_RESULT(Allow, []);
	    [<<"running nodes">>, _ENode] ->
		?INFO_RESULT(Allow, [?NS_STATS]);
	    [<<"running nodes">>, _ENode, <<"DB">>] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    [<<"running nodes">>, _ENode, <<"modules">>] ->
		?INFO_RESULT(Allow, []);
	    [<<"running nodes">>, _ENode, <<"modules">>, _] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    [<<"running nodes">>, _ENode, <<"backup">>] ->
		?INFO_RESULT(Allow, []);
	    [<<"running nodes">>, _ENode, <<"backup">>, _] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    [<<"running nodes">>, _ENode, <<"import">>] ->
		?INFO_RESULT(Allow, []);
	    [<<"running nodes">>, _ENode, <<"import">>, _] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    [<<"running nodes">>, _ENode, <<"restart">>] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    [<<"running nodes">>, _ENode, <<"shutdown">>] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    [<<"config">>, _] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    ?NS_ADMINL(<<"add-user">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    ?NS_ADMINL(<<"delete-user">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    ?NS_ADMINL(<<"end-user-session">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    ?NS_ADMINL(<<"get-user-password">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    ?NS_ADMINL(<<"change-user-password">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    ?NS_ADMINL(<<"get-user-lastlogin">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    ?NS_ADMINL(<<"user-stats">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    ?NS_ADMINL(<<"get-registered-users-num">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    ?NS_ADMINL(<<"get-online-users-num">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS]);
	    _ -> Acc
	  end
    end.

%%%-----------------------------------------------------------------------

adhoc_sm_items(Acc, From, #jid{lserver = LServer} = To,
	       Lang) ->
    case acl:match_rule(LServer, configure, From) of
      allow ->
	  Items = case Acc of
		    {result, Its} -> Its;
		    empty -> []
		  end,
	  Nodes = [#xmlel{name = <<"item">>,
			  attrs =
			      [{<<"jid">>, jid:to_string(To)},
			       {<<"name">>, ?T(Lang, <<"Configuration">>)},
			       {<<"node">>, <<"config">>}],
			  children = []}],
	  {result, Items ++ Nodes};
      _ -> Acc
    end.

%%%-----------------------------------------------------------------------

get_sm_items(Acc, From,
	     #jid{user = User, server = Server, lserver = LServer} =
		 To,
	     Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  Items = case Acc of
		    {result, Its} -> Its;
		    empty -> []
		  end,
	  case {acl:match_rule(LServer, configure, From), Node} of
	    {allow, <<"">>} ->
		Nodes = [?NODEJID(To, <<"Configuration">>,
				  <<"config">>),
			 ?NODEJID(To, <<"User Management">>, <<"user">>)],
		{result,
		 Items ++ Nodes ++ get_user_resources(User, Server)};
	    {allow, <<"config">>} -> {result, []};
	    {_, <<"config">>} -> {error, ?ERR_FORBIDDEN};
	    _ -> Acc
	  end
    end.

get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    lists:map(fun (R) ->
		      #xmlel{name = <<"item">>,
			     attrs =
				 [{<<"jid">>,
				   <<User/binary, "@", Server/binary, "/",
				     R/binary>>},
				  {<<"name">>, User}],
			     children = []}
	      end,
	      lists:sort(Rs)).

%%%-----------------------------------------------------------------------

adhoc_local_items(Acc, From,
		  #jid{lserver = LServer, server = Server} = To, Lang) ->
    case acl:match_rule(LServer, configure, From) of
      allow ->
	  Items = case Acc of
		    {result, Its} -> Its;
		    empty -> []
		  end,
	  PermLev = get_permission_level(From),
	  Nodes = recursively_get_local_items(PermLev, LServer,
					      <<"">>, Server, Lang),
	  Nodes1 = lists:filter(fun (N) ->
					Nd = xml:get_tag_attr_s(<<"node">>, N),
					F = get_local_features([], From, To, Nd,
							       Lang),
					case F of
					  {result, [?NS_COMMANDS]} -> true;
					  _ -> false
					end
				end,
				Nodes),
	  {result, Items ++ Nodes1};
      _ -> Acc
    end.

recursively_get_local_items(_PermLev, _LServer,
			    <<"online users">>, _Server, _Lang) ->
    [];
recursively_get_local_items(_PermLev, _LServer,
			    <<"all users">>, _Server, _Lang) ->
    [];
recursively_get_local_items(PermLev, LServer, Node,
			    Server, Lang) ->
    LNode = tokenize(Node),
    Items = case get_local_items({PermLev, LServer}, LNode,
				 Server, Lang)
		of
	      {result, Res} -> Res;
	      {error, _Error} -> []
	    end,
    Nodes = lists:flatten(lists:map(fun (N) ->
					    S = xml:get_tag_attr_s(<<"jid">>,
								   N),
					    Nd = xml:get_tag_attr_s(<<"node">>,
								    N),
					    if (S /= Server) or
						 (Nd == <<"">>) ->
						   [];
					       true ->
						   [N,
						    recursively_get_local_items(PermLev,
										LServer,
										Nd,
										Server,
										Lang)]
					    end
				    end,
				    Items)),
    Nodes.

get_permission_level(JID) ->
    case acl:match_rule(global, configure, JID) of
      allow -> global;
      deny -> vhost
    end.

%%%-----------------------------------------------------------------------

-define(ITEMS_RESULT(Allow, LNode, Fallback),
	case Allow of
	  deny -> Fallback;
	  allow ->
	      PermLev = get_permission_level(From),
	      case get_local_items({PermLev, LServer}, LNode,
				   jid:to_string(To), Lang)
		  of
		{result, Res} -> {result, Res};
		{error, Error} -> {error, Error}
	      end
	end).

get_local_items(Acc, From, #jid{lserver = LServer} = To,
		<<"">>, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  Items = case Acc of
		    {result, Its} -> Its;
		    empty -> []
		  end,
	  Allow = acl:match_rule(LServer, configure, From),
	  case Allow of
	    deny -> {result, Items};
	    allow ->
		PermLev = get_permission_level(From),
		case get_local_items({PermLev, LServer}, [],
				     jid:to_string(To), Lang)
		    of
		  {result, Res} -> {result, Items ++ Res};
		  {error, _Error} -> {result, Items}
		end
	  end
    end;
get_local_items(Acc, From, #jid{lserver = LServer} = To,
		Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  LNode = tokenize(Node),
	  Allow = acl:match_rule(LServer, configure, From),
	  case LNode of
	    [<<"config">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"user">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"online users">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"all users">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"all users">>, <<$@, _/binary>>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"outgoing s2s">> | _] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"stopped nodes">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode, <<"DB">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode, <<"modules">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode, <<"modules">>, _] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode, <<"backup">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode, <<"backup">>, _] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode, <<"import">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode, <<"import">>, _] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode, <<"restart">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"running nodes">>, _ENode, <<"shutdown">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    [<<"config">>, _] ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    ?NS_ADMINL(<<"add-user">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    ?NS_ADMINL(<<"delete-user">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    ?NS_ADMINL(<<"end-user-session">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    ?NS_ADMINL(<<"get-user-password">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    ?NS_ADMINL(<<"change-user-password">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    ?NS_ADMINL(<<"get-user-lastlogin">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    ?NS_ADMINL(<<"user-stats">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    ?NS_ADMINL(<<"get-registered-users-num">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    ?NS_ADMINL(<<"get-online-users-num">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, ?ERR_FORBIDDEN});
	    _ -> Acc
	  end
    end.

%%%-----------------------------------------------------------------------

%% @spec ({PermissionLevel, Host}, [string()], Server::string(), Lang)
%%              -> {result, [xmlelement()]}
%%       PermissionLevel = global | vhost
get_local_items(_Host, [], Server, Lang) ->
    {result,
     [?NODE(<<"Configuration">>, <<"config">>),
      ?NODE(<<"User Management">>, <<"user">>),
      ?NODE(<<"Online Users">>, <<"online users">>),
      ?NODE(<<"All Users">>, <<"all users">>),
      ?NODE(<<"Outgoing s2s Connections">>,
	    <<"outgoing s2s">>),
      ?NODE(<<"Running Nodes">>, <<"running nodes">>),
      ?NODE(<<"Stopped Nodes">>, <<"stopped nodes">>)]};
get_local_items(_Host, [<<"config">>], Server, Lang) ->
    {result,
     [?NODE(<<"Access Control Lists">>, <<"config/acls">>),
      ?NODE(<<"Access Rules">>, <<"config/access">>)]};
get_local_items(_Host, [<<"config">>, _], _Server,
		_Lang) ->
    {result, []};
get_local_items(_Host, [<<"user">>], Server, Lang) ->
    {result,
     [?NODE(<<"Add User">>, (?NS_ADMINX(<<"add-user">>))),
      ?NODE(<<"Delete User">>,
	    (?NS_ADMINX(<<"delete-user">>))),
      ?NODE(<<"End User Session">>,
	    (?NS_ADMINX(<<"end-user-session">>))),
      ?NODE(<<"Get User Password">>,
	    (?NS_ADMINX(<<"get-user-password">>))),
      ?NODE(<<"Change User Password">>,
	    (?NS_ADMINX(<<"change-user-password">>))),
      ?NODE(<<"Get User Last Login Time">>,
	    (?NS_ADMINX(<<"get-user-lastlogin">>))),
      ?NODE(<<"Get User Statistics">>,
	    (?NS_ADMINX(<<"user-stats">>))),
      ?NODE(<<"Get Number of Registered Users">>,
	    (?NS_ADMINX(<<"get-registered-users-num">>))),
      ?NODE(<<"Get Number of Online Users">>,
	    (?NS_ADMINX(<<"get-online-users-num">>)))]};
get_local_items(_Host, [<<"http:">> | _], _Server,
		_Lang) ->
    {result, []};
get_local_items({_, Host}, [<<"online users">>],
		_Server, _Lang) ->
    {result, get_online_vh_users(Host)};
get_local_items({_, Host}, [<<"all users">>], _Server,
		_Lang) ->
    {result, get_all_vh_users(Host)};
get_local_items({_, Host},
		[<<"all users">>, <<$@, Diap/binary>>], _Server,
		_Lang) ->
    case catch ejabberd_auth:get_vh_registered_users(Host)
	of
      {'EXIT', _Reason} -> ?ERR_INTERNAL_SERVER_ERROR;
      Users ->
	  SUsers = lists:sort([{S, U} || {U, S} <- Users]),
	  case catch begin
		       [S1, S2] = ejabberd_regexp:split(Diap, <<"-">>),
		       N1 = jlib:binary_to_integer(S1),
		       N2 = jlib:binary_to_integer(S2),
		       Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
		       lists:map(fun ({S, U}) ->
					 #xmlel{name = <<"item">>,
						attrs =
						    [{<<"jid">>,
						      <<U/binary, "@",
							S/binary>>},
						     {<<"name">>,
						      <<U/binary, "@",
							S/binary>>}],
						children = []}
				 end,
				 Sub)
		     end
	      of
	    {'EXIT', _Reason} -> ?ERR_NOT_ACCEPTABLE;
	    Res -> {result, Res}
	  end
    end;
get_local_items({_, Host}, [<<"outgoing s2s">>],
		_Server, Lang) ->
    {result, get_outgoing_s2s(Host, Lang)};
get_local_items({_, Host}, [<<"outgoing s2s">>, To],
		_Server, Lang) ->
    {result, get_outgoing_s2s(Host, Lang, To)};
get_local_items(_Host, [<<"running nodes">>], Server,
		Lang) ->
    {result, get_running_nodes(Server, Lang)};
get_local_items(_Host, [<<"stopped nodes">>], _Server,
		Lang) ->
    {result, get_stopped_nodes(Lang)};
get_local_items({global, _Host},
		[<<"running nodes">>, ENode], Server, Lang) ->
    {result,
     [?NODE(<<"Database">>,
	    <<"running nodes/", ENode/binary, "/DB">>),
      ?NODE(<<"Modules">>,
	    <<"running nodes/", ENode/binary, "/modules">>),
      ?NODE(<<"Backup Management">>,
	    <<"running nodes/", ENode/binary, "/backup">>),
      ?NODE(<<"Import Users From jabberd14 Spool Files">>,
	    <<"running nodes/", ENode/binary, "/import">>),
      ?NODE(<<"Restart Service">>,
	    <<"running nodes/", ENode/binary, "/restart">>),
      ?NODE(<<"Shut Down Service">>,
	    <<"running nodes/", ENode/binary, "/shutdown">>)]};
get_local_items({vhost, _Host},
		[<<"running nodes">>, ENode], Server, Lang) ->
    {result,
     [?NODE(<<"Modules">>,
	    <<"running nodes/", ENode/binary, "/modules">>)]};
get_local_items(_Host,
		[<<"running nodes">>, _ENode, <<"DB">>], _Server,
		_Lang) ->
    {result, []};
get_local_items(_Host,
		[<<"running nodes">>, ENode, <<"modules">>], Server,
		Lang) ->
    {result,
     [?NODE(<<"Start Modules">>,
	    <<"running nodes/", ENode/binary, "/modules/start">>),
      ?NODE(<<"Stop Modules">>,
	    <<"running nodes/", ENode/binary, "/modules/stop">>)]};
get_local_items(_Host,
		[<<"running nodes">>, _ENode, <<"modules">>, _],
		_Server, _Lang) ->
    {result, []};
get_local_items(_Host,
		[<<"running nodes">>, ENode, <<"backup">>], Server,
		Lang) ->
    {result,
     [?NODE(<<"Backup">>,
	    <<"running nodes/", ENode/binary, "/backup/backup">>),
      ?NODE(<<"Restore">>,
	    <<"running nodes/", ENode/binary, "/backup/restore">>),
      ?NODE(<<"Dump to Text File">>,
	    <<"running nodes/", ENode/binary,
	      "/backup/textfile">>)]};
get_local_items(_Host,
		[<<"running nodes">>, _ENode, <<"backup">>, _], _Server,
		_Lang) ->
    {result, []};
get_local_items(_Host,
		[<<"running nodes">>, ENode, <<"import">>], Server,
		Lang) ->
    {result,
     [?NODE(<<"Import File">>,
	    <<"running nodes/", ENode/binary, "/import/file">>),
      ?NODE(<<"Import Directory">>,
	    <<"running nodes/", ENode/binary, "/import/dir">>)]};
get_local_items(_Host,
		[<<"running nodes">>, _ENode, <<"import">>, _], _Server,
		_Lang) ->
    {result, []};
get_local_items(_Host,
		[<<"running nodes">>, _ENode, <<"restart">>], _Server,
		_Lang) ->
    {result, []};
get_local_items(_Host,
		[<<"running nodes">>, _ENode, <<"shutdown">>], _Server,
		_Lang) ->
    {result, []};
get_local_items(_Host, _, _Server, _Lang) ->
    {error, ?ERR_ITEM_NOT_FOUND}.

get_online_vh_users(Host) ->
    case catch ejabberd_sm:get_vh_session_list(Host) of
      {'EXIT', _Reason} -> [];
      USRs ->
	  SURs = lists:sort([{S, U, R} || {U, S, R} <- USRs]),
	  lists:map(fun ({S, U, R}) ->
			    #xmlel{name = <<"item">>,
				   attrs =
				       [{<<"jid">>,
					 <<U/binary, "@", S/binary, "/",
					   R/binary>>},
					{<<"name">>,
					 <<U/binary, "@", S/binary>>}],
				   children = []}
		    end,
		    SURs)
    end.

get_all_vh_users(Host) ->
    case catch ejabberd_auth:get_vh_registered_users(Host)
	of
      {'EXIT', _Reason} -> [];
      Users ->
	  SUsers = lists:sort([{S, U} || {U, S} <- Users]),
	  case length(SUsers) of
	    N when N =< 100 ->
		lists:map(fun ({S, U}) ->
				  #xmlel{name = <<"item">>,
					 attrs =
					     [{<<"jid">>,
					       <<U/binary, "@", S/binary>>},
					      {<<"name">>,
					       <<U/binary, "@", S/binary>>}],
					 children = []}
			  end,
			  SUsers);
	    N ->
		NParts = trunc(math:sqrt(N * 6.17999999999999993783e-1))
			   + 1,
		M = trunc(N / NParts) + 1,
		lists:map(fun (K) ->
				  L = K + M - 1,
				  Node = <<"@",
					   (iolist_to_binary(integer_to_list(K)))/binary,
					   "-",
					   (iolist_to_binary(integer_to_list(L)))/binary>>,
				  {FS, FU} = lists:nth(K, SUsers),
				  {LS, LU} = if L < N -> lists:nth(L, SUsers);
						true -> lists:last(SUsers)
					     end,
				  Name = <<FU/binary, "@", FS/binary, " -- ",
					   LU/binary, "@", LS/binary>>,
				  #xmlel{name = <<"item">>,
					 attrs =
					     [{<<"jid">>, Host},
					      {<<"node">>,
					       <<"all users/", Node/binary>>},
					      {<<"name">>, Name}],
					 children = []}
			  end,
			  lists:seq(1, N, M))
	  end
    end.

get_outgoing_s2s(Host, Lang) ->
    case catch ejabberd_s2s:dirty_get_connections() of
      {'EXIT', _Reason} -> [];
      Connections ->
	  DotHost = <<".", Host/binary>>,
	  TConns = [TH
		    || {FH, TH} <- Connections,
		       Host == FH orelse str:suffix(DotHost, FH)],
	  lists:map(fun (T) ->
			    #xmlel{name = <<"item">>,
				   attrs =
				       [{<<"jid">>, Host},
					{<<"node">>,
					 <<"outgoing s2s/", T/binary>>},
					{<<"name">>,
					 iolist_to_binary(io_lib:format(?T(Lang,
									   <<"To ~s">>),
									[T]))}],
				   children = []}
		    end,
		    lists:usort(TConns))
    end.

get_outgoing_s2s(Host, Lang, To) ->
    case catch ejabberd_s2s:dirty_get_connections() of
      {'EXIT', _Reason} -> [];
      Connections ->
	  lists:map(fun ({F, _T}) ->
			    #xmlel{name = <<"item">>,
				   attrs =
				       [{<<"jid">>, Host},
					{<<"node">>,
					 <<"outgoing s2s/", To/binary, "/",
					   F/binary>>},
					{<<"name">>,
					 iolist_to_binary(io_lib:format(?T(Lang,
									   <<"From ~s">>),
									[F]))}],
				   children = []}
		    end,
		    lists:keysort(1,
				  lists:filter(fun (E) -> element(2, E) == To
					       end,
					       Connections)))
    end.

get_running_nodes(Server, _Lang) ->
    case catch mnesia:system_info(running_db_nodes) of
      {'EXIT', _Reason} -> [];
      DBNodes ->
	  lists:map(fun (N) ->
			    S = iolist_to_binary(atom_to_list(N)),
			    #xmlel{name = <<"item">>,
				   attrs =
				       [{<<"jid">>, Server},
					{<<"node">>,
					 <<"running nodes/", S/binary>>},
					{<<"name">>, S}],
				   children = []}
		    end,
		    lists:sort(DBNodes))
    end.

get_stopped_nodes(_Lang) ->
    case catch lists:usort(mnesia:system_info(db_nodes) ++
			     mnesia:system_info(extra_db_nodes))
		 -- mnesia:system_info(running_db_nodes)
	of
      {'EXIT', _Reason} -> [];
      DBNodes ->
	  lists:map(fun (N) ->
			    S = iolist_to_binary(atom_to_list(N)),
			    #xmlel{name = <<"item">>,
				   attrs =
				       [{<<"jid">>, ?MYNAME},
					{<<"node">>,
					 <<"stopped nodes/", S/binary>>},
					{<<"name">>, S}],
				   children = []}
		    end,
		    lists:sort(DBNodes))
    end.

%%-------------------------------------------------------------------------

-define(COMMANDS_RESULT(LServerOrGlobal, From, To,
			Request),
	case acl:match_rule(LServerOrGlobal, configure, From) of
	  deny -> {error, ?ERR_FORBIDDEN};
	  allow -> adhoc_local_commands(From, To, Request)
	end).

adhoc_local_commands(Acc, From,
		     #jid{lserver = LServer} = To,
		     #adhoc_request{node = Node} = Request) ->
    LNode = tokenize(Node),
    case LNode of
      [<<"running nodes">>, _ENode, <<"DB">>] ->
	  ?COMMANDS_RESULT(global, From, To, Request);
      [<<"running nodes">>, _ENode, <<"modules">>, _] ->
	  ?COMMANDS_RESULT(LServer, From, To, Request);
      [<<"running nodes">>, _ENode, <<"backup">>, _] ->
	  ?COMMANDS_RESULT(global, From, To, Request);
      [<<"running nodes">>, _ENode, <<"import">>, _] ->
	  ?COMMANDS_RESULT(global, From, To, Request);
      [<<"running nodes">>, _ENode, <<"restart">>] ->
	  ?COMMANDS_RESULT(global, From, To, Request);
      [<<"running nodes">>, _ENode, <<"shutdown">>] ->
	  ?COMMANDS_RESULT(global, From, To, Request);
      [<<"config">>, _] ->
	  ?COMMANDS_RESULT(LServer, From, To, Request);
      ?NS_ADMINL(_) ->
	  ?COMMANDS_RESULT(LServer, From, To, Request);
      _ -> Acc
    end.

adhoc_local_commands(From,
		     #jid{lserver = LServer} = _To,
		     #adhoc_request{lang = Lang, node = Node,
				    sessionid = SessionID, action = Action,
				    xdata = XData} =
			 Request) ->
    LNode = tokenize(Node),
    ActionIsExecute = lists:member(Action,
				   [<<"">>, <<"execute">>, <<"complete">>]),
    if Action == <<"cancel">> ->
	   adhoc:produce_response(Request,
				  #adhoc_response{status = canceled});
       XData == false, ActionIsExecute ->
	   case get_form(LServer, LNode, Lang) of
	     {result, Form} ->
		 adhoc:produce_response(Request,
					#adhoc_response{status = executing,
							elements = Form});
	     {result, Status, Form} ->
		 adhoc:produce_response(Request,
					#adhoc_response{status = Status,
							elements = Form});
	     {error, Error} -> {error, Error}
	   end;
       XData /= false, ActionIsExecute ->
	   case jlib:parse_xdata_submit(XData) of
	     invalid -> {error, ?ERR_BAD_REQUEST};
	     Fields ->
		 case catch set_form(From, LServer, LNode, Lang, Fields)
		     of
		   {result, Res} ->
		       adhoc:produce_response(#adhoc_response{lang = Lang,
							      node = Node,
							      sessionid =
								  SessionID,
							      elements = Res,
							      status =
								  completed});
		   {'EXIT', _} -> {error, ?ERR_BAD_REQUEST};
		   {error, Error} -> {error, Error}
		 end
	   end;
       true -> {error, ?ERR_BAD_REQUEST}
    end.

-define(TVFIELD(Type, Var, Val),
	#xmlel{name = <<"field">>,
	       attrs = [{<<"type">>, Type}, {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

-define(HFIELD(),
	?TVFIELD(<<"hidden">>, <<"FORM_TYPE">>, (?NS_ADMIN))).

-define(TLFIELD(Type, Label, Var),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type}, {<<"label">>, ?T(Lang, Label)},
		    {<<"var">>, Var}],
	       children = []}).

-define(XFIELD(Type, Label, Var, Val),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type}, {<<"label">>, ?T(Lang, Label)},
		    {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

-define(XMFIELD(Type, Label, Var, Vals),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type}, {<<"label">>, ?T(Lang, Label)},
		    {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}
		    || Val <- Vals]}).

-define(TABLEFIELD(Table, Val),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, <<"list-single">>},
		    {<<"label">>, iolist_to_binary(atom_to_list(Table))},
		    {<<"var">>, iolist_to_binary(atom_to_list(Table))}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children =
			       [{xmlcdata,
				 iolist_to_binary(atom_to_list(Val))}]},
		    #xmlel{name = <<"option">>,
			   attrs = [{<<"label">>, ?T(Lang, <<"RAM copy">>)}],
			   children =
			       [#xmlel{name = <<"value">>, attrs = [],
				       children =
					   [{xmlcdata, <<"ram_copies">>}]}]},
		    #xmlel{name = <<"option">>,
			   attrs =
			       [{<<"label">>,
				 ?T(Lang, <<"RAM and disc copy">>)}],
			   children =
			       [#xmlel{name = <<"value">>, attrs = [],
				       children =
					   [{xmlcdata, <<"disc_copies">>}]}]},
		    #xmlel{name = <<"option">>,
			   attrs =
			       [{<<"label">>, ?T(Lang, <<"Disc only copy">>)}],
			   children =
			       [#xmlel{name = <<"value">>, attrs = [],
				       children =
					   [{xmlcdata,
					     <<"disc_only_copies">>}]}]},
		    #xmlel{name = <<"option">>,
			   attrs = [{<<"label">>, ?T(Lang, <<"Remote copy">>)}],
			   children =
			       [#xmlel{name = <<"value">>, attrs = [],
				       children =
					   [{xmlcdata, <<"unknown">>}]}]}]}).

get_form(_Host, [<<"running nodes">>, ENode, <<"DB">>],
	 Lang) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  case ejabberd_cluster:call(Node, mnesia, system_info, [tables]) of
	    {badrpc, _Reason} ->
		{error, ?ERR_INTERNAL_SERVER_ERROR};
	    Tables ->
		STables = lists:sort(Tables),
		{result,
		 [#xmlel{name = <<"x">>,
			 attrs = [{<<"xmlns">>, ?NS_XDATA}],
			 children =
			     [?HFIELD(),
			      #xmlel{name = <<"title">>, attrs = [],
				     children =
					 [{xmlcdata,
					   <<(?T(Lang,
						 <<"Database Tables Configuration at ">>))/binary,
					     ENode/binary>>}]},
			      #xmlel{name = <<"instructions">>, attrs = [],
				     children =
					 [{xmlcdata,
					   ?T(Lang,
					      <<"Choose storage type of tables">>)}]}
			      | lists:map(fun (Table) ->
						  case ejabberd_cluster:call(Node, mnesia,
								table_info,
								[Table,
								 storage_type])
						      of
						    {badrpc, _} ->
							?TABLEFIELD(Table,
								    unknown);
						    Type ->
							?TABLEFIELD(Table, Type)
						  end
					  end,
					  STables)]}]}
	  end
    end;
get_form(Host,
	 [<<"running nodes">>, ENode, <<"modules">>, <<"stop">>],
	 Lang) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  case ejabberd_cluster:call(Node, gen_mod, loaded_modules, [Host]) of
	    {badrpc, _Reason} ->
		{error, ?ERR_INTERNAL_SERVER_ERROR};
	    Modules ->
		SModules = lists:sort(Modules),
		{result,
		 [#xmlel{name = <<"x">>,
			 attrs = [{<<"xmlns">>, ?NS_XDATA}],
			 children =
			     [?HFIELD(),
			      #xmlel{name = <<"title">>, attrs = [],
				     children =
					 [{xmlcdata,
					   <<(?T(Lang,
						 <<"Stop Modules at ">>))/binary,
					     ENode/binary>>}]},
			      #xmlel{name = <<"instructions">>, attrs = [],
				     children =
					 [{xmlcdata,
					   ?T(Lang,
					      <<"Choose modules to stop">>)}]}
			      | lists:map(fun (M) ->
						  S = jlib:atom_to_binary(M),
						  ?XFIELD(<<"boolean">>, S, S,
							  <<"0">>)
					  end,
					  SModules)]}]}
	  end
    end;
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"modules">>,
	  <<"start">>],
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       <<(?T(Lang, <<"Start Modules at ">>))/binary,
				 ENode/binary>>}]},
		  #xmlel{name = <<"instructions">>, attrs = [],
			 children =
			     [{xmlcdata,
			       ?T(Lang,
				  <<"Enter list of {Module, [Options]}">>)}]},
		  ?XFIELD(<<"text-multi">>,
			  <<"List of modules to start">>, <<"modules">>,
			  <<"[].">>)]}]};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"backup">>],
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       <<(?T(Lang, <<"Backup to File at ">>))/binary,
				 ENode/binary>>}]},
		  #xmlel{name = <<"instructions">>, attrs = [],
			 children =
			     [{xmlcdata,
			       ?T(Lang, <<"Enter path to backup file">>)}]},
		  ?XFIELD(<<"text-single">>, <<"Path to File">>,
			  <<"path">>, <<"">>)]}]};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"restore">>],
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       <<(?T(Lang,
				     <<"Restore Backup from File at ">>))/binary,
				 ENode/binary>>}]},
		  #xmlel{name = <<"instructions">>, attrs = [],
			 children =
			     [{xmlcdata,
			       ?T(Lang, <<"Enter path to backup file">>)}]},
		  ?XFIELD(<<"text-single">>, <<"Path to File">>,
			  <<"path">>, <<"">>)]}]};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"textfile">>],
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       <<(?T(Lang,
				     <<"Dump Backup to Text File at ">>))/binary,
				 ENode/binary>>}]},
		  #xmlel{name = <<"instructions">>, attrs = [],
			 children =
			     [{xmlcdata,
			       ?T(Lang, <<"Enter path to text file">>)}]},
		  ?XFIELD(<<"text-single">>, <<"Path to File">>,
			  <<"path">>, <<"">>)]}]};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"file">>],
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       <<(?T(Lang,
				     <<"Import User from File at ">>))/binary,
				 ENode/binary>>}]},
		  #xmlel{name = <<"instructions">>, attrs = [],
			 children =
			     [{xmlcdata,
			       ?T(Lang,
				  <<"Enter path to jabberd14 spool file">>)}]},
		  ?XFIELD(<<"text-single">>, <<"Path to File">>,
			  <<"path">>, <<"">>)]}]};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"dir">>],
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       <<(?T(Lang,
				     <<"Import Users from Dir at ">>))/binary,
				 ENode/binary>>}]},
		  #xmlel{name = <<"instructions">>, attrs = [],
			 children =
			     [{xmlcdata,
			       ?T(Lang,
				  <<"Enter path to jabberd14 spool dir">>)}]},
		  ?XFIELD(<<"text-single">>, <<"Path to Dir">>,
			  <<"path">>, <<"">>)]}]};
get_form(_Host,
	 [<<"running nodes">>, _ENode, <<"restart">>], Lang) ->
    Make_option = fun (LabelNum, LabelUnit, Value) ->
			  #xmlel{name = <<"option">>,
				 attrs =
				     [{<<"label">>,
				       <<LabelNum/binary,
					 (?T(Lang, LabelUnit))/binary>>}],
				 children =
				     [#xmlel{name = <<"value">>, attrs = [],
					     children = [{xmlcdata, Value}]}]}
		  end,
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata, ?T(Lang, <<"Restart Service">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"list-single">>},
			      {<<"label">>, ?T(Lang, <<"Time delay">>)},
			      {<<"var">>, <<"delay">>}],
			 children =
			     [Make_option(<<"">>, <<"immediately">>, <<"1">>),
			      Make_option(<<"15 ">>, <<"seconds">>, <<"15">>),
			      Make_option(<<"30 ">>, <<"seconds">>, <<"30">>),
			      Make_option(<<"60 ">>, <<"seconds">>, <<"60">>),
			      Make_option(<<"90 ">>, <<"seconds">>, <<"90">>),
			      Make_option(<<"2 ">>, <<"minutes">>, <<"120">>),
			      Make_option(<<"3 ">>, <<"minutes">>, <<"180">>),
			      Make_option(<<"4 ">>, <<"minutes">>, <<"240">>),
			      Make_option(<<"5 ">>, <<"minutes">>, <<"300">>),
			      Make_option(<<"10 ">>, <<"minutes">>, <<"600">>),
			      Make_option(<<"15 ">>, <<"minutes">>, <<"900">>),
			      Make_option(<<"30 ">>, <<"minutes">>, <<"1800">>),
			      #xmlel{name = <<"required">>, attrs = [],
				     children = []}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"fixed">>},
			      {<<"label">>,
			       ?T(Lang,
				  <<"Send announcement to all online users "
				    "on all hosts">>)}],
			 children = []},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"var">>, <<"subject">>},
			      {<<"type">>, <<"text-single">>},
			      {<<"label">>, ?T(Lang, <<"Subject">>)}],
			 children = []},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"var">>, <<"announcement">>},
			      {<<"type">>, <<"text-multi">>},
			      {<<"label">>, ?T(Lang, <<"Message body">>)}],
			 children = []}]}]};
get_form(_Host,
	 [<<"running nodes">>, _ENode, <<"shutdown">>], Lang) ->
    Make_option = fun (LabelNum, LabelUnit, Value) ->
			  #xmlel{name = <<"option">>,
				 attrs =
				     [{<<"label">>,
				       <<LabelNum/binary,
					 (?T(Lang, LabelUnit))/binary>>}],
				 children =
				     [#xmlel{name = <<"value">>, attrs = [],
					     children = [{xmlcdata, Value}]}]}
		  end,
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata, ?T(Lang, <<"Shut Down Service">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"list-single">>},
			      {<<"label">>, ?T(Lang, <<"Time delay">>)},
			      {<<"var">>, <<"delay">>}],
			 children =
			     [Make_option(<<"">>, <<"immediately">>, <<"1">>),
			      Make_option(<<"15 ">>, <<"seconds">>, <<"15">>),
			      Make_option(<<"30 ">>, <<"seconds">>, <<"30">>),
			      Make_option(<<"60 ">>, <<"seconds">>, <<"60">>),
			      Make_option(<<"90 ">>, <<"seconds">>, <<"90">>),
			      Make_option(<<"2 ">>, <<"minutes">>, <<"120">>),
			      Make_option(<<"3 ">>, <<"minutes">>, <<"180">>),
			      Make_option(<<"4 ">>, <<"minutes">>, <<"240">>),
			      Make_option(<<"5 ">>, <<"minutes">>, <<"300">>),
			      Make_option(<<"10 ">>, <<"minutes">>, <<"600">>),
			      Make_option(<<"15 ">>, <<"minutes">>, <<"900">>),
			      Make_option(<<"30 ">>, <<"minutes">>, <<"1800">>),
			      #xmlel{name = <<"required">>, attrs = [],
				     children = []}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"fixed">>},
			      {<<"label">>,
			       ?T(Lang,
				  <<"Send announcement to all online users "
				    "on all hosts">>)}],
			 children = []},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"var">>, <<"subject">>},
			      {<<"type">>, <<"text-single">>},
			      {<<"label">>, ?T(Lang, <<"Subject">>)}],
			 children = []},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"var">>, <<"announcement">>},
			      {<<"type">>, <<"text-multi">>},
			      {<<"label">>, ?T(Lang, <<"Message body">>)}],
			 children = []}]}]};
get_form(Host, [<<"config">>, <<"acls">>], Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       ?T(Lang,
				  <<"Access Control List Configuration">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"text-multi">>},
			      {<<"label">>,
			       ?T(Lang, <<"Access control lists">>)},
			      {<<"var">>, <<"acls">>}],
			 children =
			     lists:map(fun (S) ->
					       #xmlel{name = <<"value">>,
						      attrs = [],
						      children =
							  [{xmlcdata, S}]}
				       end,
				       str:tokens(iolist_to_binary(io_lib:format("~p.",
										 [ets:select(acl,
											     [{{acl,
												{'$1',
												 '$2'},
												'$3'},
											       [{'==',
												 '$2',
												 Host}],
											       [{{acl,
												  '$1',
												  '$3'}}]}])])),
						  <<"\n">>))}]}]};
get_form(Host, [<<"config">>, <<"access">>], Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       ?T(Lang, <<"Access Configuration">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"text-multi">>},
			      {<<"label">>, ?T(Lang, <<"Access rules">>)},
			      {<<"var">>, <<"access">>}],
			 children =
			     lists:map(fun (S) ->
					       #xmlel{name = <<"value">>,
						      attrs = [],
						      children =
							  [{xmlcdata, S}]}
				       end,
				       str:tokens(iolist_to_binary(io_lib:format("~p.",
										 [ets:select(local_config,
											     [{{local_config,
												{access,
												 '$1',
												 '$2'},
												'$3'},
											       [{'==',
												 '$2',
												 Host}],
											       [{{access,
												  '$1',
												  '$3'}}]}])])),
						  <<"\n">>))}]}]};
get_form(_Host, ?NS_ADMINL(<<"add-user">>), Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children = [{xmlcdata, ?T(Lang, <<"Add User">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"jid-single">>},
			      {<<"label">>, ?T(Lang, <<"Jabber ID">>)},
			      {<<"var">>, <<"accountjid">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"text-private">>},
			      {<<"label">>, ?T(Lang, <<"Password">>)},
			      {<<"var">>, <<"password">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"text-private">>},
			      {<<"label">>,
			       ?T(Lang, <<"Password Verification">>)},
			      {<<"var">>, <<"password-verify">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]}]}]};
get_form(_Host, ?NS_ADMINL(<<"delete-user">>), Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children = [{xmlcdata, ?T(Lang, <<"Delete User">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"jid-multi">>},
			      {<<"label">>, ?T(Lang, <<"Jabber ID">>)},
			      {<<"var">>, <<"accountjids">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]}]}]};
get_form(_Host, ?NS_ADMINL(<<"end-user-session">>),
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata, ?T(Lang, <<"End User Session">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"jid-single">>},
			      {<<"label">>, ?T(Lang, <<"Jabber ID">>)},
			      {<<"var">>, <<"accountjid">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]}]}]};
get_form(_Host, ?NS_ADMINL(<<"get-user-password">>),
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata, ?T(Lang, <<"Get User Password">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"jid-single">>},
			      {<<"label">>, ?T(Lang, <<"Jabber ID">>)},
			      {<<"var">>, <<"accountjid">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]}]}]};
get_form(_Host, ?NS_ADMINL(<<"change-user-password">>),
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata, ?T(Lang, <<"Get User Password">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"jid-single">>},
			      {<<"label">>, ?T(Lang, <<"Jabber ID">>)},
			      {<<"var">>, <<"accountjid">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"text-private">>},
			      {<<"label">>, ?T(Lang, <<"Password">>)},
			      {<<"var">>, <<"password">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]}]}]};
get_form(_Host, ?NS_ADMINL(<<"get-user-lastlogin">>),
	 Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       ?T(Lang, <<"Get User Last Login Time">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"jid-single">>},
			      {<<"label">>, ?T(Lang, <<"Jabber ID">>)},
			      {<<"var">>, <<"accountjid">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]}]}]};
get_form(_Host, ?NS_ADMINL(<<"user-stats">>), Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata, ?T(Lang, <<"Get User Statistics">>)}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"jid-single">>},
			      {<<"label">>, ?T(Lang, <<"Jabber ID">>)},
			      {<<"var">>, <<"accountjid">>}],
			 children =
			     [#xmlel{name = <<"required">>, attrs = [],
				     children = []}]}]}]};
get_form(Host,
	 ?NS_ADMINL(<<"get-registered-users-num">>), Lang) ->
    Num = list_to_binary(
            io_lib:format("~p",
			  [ejabberd_auth:get_vh_registered_users_number(Host)])),
    {result, completed,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"text-single">>},
			      {<<"label">>,
			       ?T(Lang, <<"Number of registered users">>)},
			      {<<"var">>, <<"registeredusersnum">>}],
			 children =
			     [#xmlel{name = <<"value">>, attrs = [],
				     children = [{xmlcdata, Num}]}]}]}]};
get_form(Host, ?NS_ADMINL(<<"get-online-users-num">>),
	 Lang) ->
    Num = list_to_binary(
            io_lib:format("~p",
                          [length(ejabberd_sm:get_vh_session_list(Host))])),
    {result, completed,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"text-single">>},
			      {<<"label">>,
			       ?T(Lang, <<"Number of online users">>)},
			      {<<"var">>, <<"onlineusersnum">>}],
			 children =
			     [#xmlel{name = <<"value">>, attrs = [],
				     children = [{xmlcdata, Num}]}]}]}]};
get_form(_Host, _, _Lang) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.

set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"DB">>], _Lang, XData) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  lists:foreach(fun ({SVar, SVals}) ->
				Table = jlib:binary_to_atom(SVar),
				Type = case SVals of
					 [<<"unknown">>] -> unknown;
					 [<<"ram_copies">>] -> ram_copies;
					 [<<"disc_copies">>] -> disc_copies;
					 [<<"disc_only_copies">>] ->
					     disc_only_copies;
					 _ -> false
				       end,
				if Type == false -> ok;
				   Type == unknown ->
				       mnesia:del_table_copy(Table, Node);
				   true ->
				       case mnesia:add_table_copy(Table, Node,
								  Type)
					   of
					 {aborted, _} ->
					     mnesia:change_table_copy_type(Table,
									   Node,
									   Type);
					 _ -> ok
				       end
				end
			end,
			XData),
	  {result, []}
    end;
set_form(_From, Host,
	 [<<"running nodes">>, ENode, <<"modules">>, <<"stop">>],
	 _Lang, XData) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  lists:foreach(fun ({Var, Vals}) ->
				case Vals of
				  [<<"1">>] ->
				      Module = jlib:binary_to_atom(Var),
				      ejabberd_cluster:call(Node, gen_mod, stop_module,
					       [Host, Module]);
				  _ -> ok
				end
			end,
			XData),
	  {result, []}
    end;
set_form(_From, Host,
	 [<<"running nodes">>, ENode, <<"modules">>,
	  <<"start">>],
	 _Lang, XData) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  case lists:keysearch(<<"modules">>, 1, XData) of
	    false -> {error, ?ERR_BAD_REQUEST};
	    {value, {_, Strings}} ->
		String = lists:foldl(fun (S, Res) ->
					     <<Res/binary, S/binary, "\n">>
				     end,
				     <<"">>, Strings),
		case erl_scan:string(binary_to_list(String)) of
		  {ok, Tokens, _} ->
		      case erl_parse:parse_term(Tokens) of
			{ok, Modules} ->
			    lists:foreach(fun ({Module, Args}) ->
						  ejabberd_cluster:call(Node, gen_mod,
							   start_module,
							   [Host, Module, Args])
					  end,
					  Modules),
			    {result, []};
			_ -> {error, ?ERR_BAD_REQUEST}
		      end;
		  _ -> {error, ?ERR_BAD_REQUEST}
		end
	  end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"backup">>],
	 _Lang, XData) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  case lists:keysearch(<<"path">>, 1, XData) of
	    false -> {error, ?ERR_BAD_REQUEST};
	    {value, {_, [String]}} ->
		case ejabberd_cluster:call(Node, mnesia, backup, [String]) of
		  {badrpc, _Reason} ->
		      {error, ?ERR_INTERNAL_SERVER_ERROR};
		  {error, _Reason} -> {error, ?ERR_INTERNAL_SERVER_ERROR};
		  _ -> {result, []}
		end;
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"restore">>],
	 _Lang, XData) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  case lists:keysearch(<<"path">>, 1, XData) of
	    false -> {error, ?ERR_BAD_REQUEST};
	    {value, {_, [String]}} ->
		case ejabberd_cluster:call(Node, ejabberd_admin, restore, [String])
		    of
		  {badrpc, _Reason} ->
		      {error, ?ERR_INTERNAL_SERVER_ERROR};
		  {error, _Reason} -> {error, ?ERR_INTERNAL_SERVER_ERROR};
		  _ -> {result, []}
		end;
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"textfile">>],
	 _Lang, XData) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  case lists:keysearch(<<"path">>, 1, XData) of
	    false -> {error, ?ERR_BAD_REQUEST};
	    {value, {_, [String]}} ->
		case ejabberd_cluster:call(Node, ejabberd_admin, dump_to_textfile,
			      [String])
		    of
		  {badrpc, _Reason} ->
		      {error, ?ERR_INTERNAL_SERVER_ERROR};
		  {error, _Reason} -> {error, ?ERR_INTERNAL_SERVER_ERROR};
		  _ -> {result, []}
		end;
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"file">>],
	 _Lang, XData) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  case lists:keysearch(<<"path">>, 1, XData) of
	    false -> {error, ?ERR_BAD_REQUEST};
	    {value, {_, [String]}} ->
		ejabberd_cluster:call(Node, jd2ejd, import_file, [String]),
		{result, []};
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"dir">>],
	 _Lang, XData) ->
    case search_running_node(ENode) of
      false -> {error, ?ERR_ITEM_NOT_FOUND};
      Node ->
	  case lists:keysearch(<<"path">>, 1, XData) of
	    false -> {error, ?ERR_BAD_REQUEST};
	    {value, {_, [String]}} ->
		ejabberd_cluster:call(Node, jd2ejd, import_dir, [String]),
		{result, []};
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end
    end;
set_form(From, Host,
	 [<<"running nodes">>, ENode, <<"restart">>], _Lang,
	 XData) ->
    stop_node(From, Host, ENode, restart, XData);
set_form(From, Host,
	 [<<"running nodes">>, ENode, <<"shutdown">>], _Lang,
	 XData) ->
    stop_node(From, Host, ENode, stop, XData);
set_form(_From, Host, [<<"config">>, <<"acls">>], _Lang,
	 XData) ->
    case lists:keysearch(<<"acls">>, 1, XData) of
      {value, {_, Strings}} ->
	  String = lists:foldl(fun (S, Res) ->
				       <<Res/binary, S/binary, "\n">>
			       end,
			       <<"">>, Strings),
	  case erl_scan:string(binary_to_list(String)) of
	    {ok, Tokens, _} ->
		case erl_parse:parse_term(Tokens) of
		  {ok, ACLs} ->
		      acl:add_list(Host, ACLs, true),
                      {result, []};
		  _ -> {error, ?ERR_BAD_REQUEST}
		end;
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end;
      _ -> {error, ?ERR_BAD_REQUEST}
    end;
set_form(_From, Host, [<<"config">>, <<"access">>],
	 _Lang, XData) ->
    SetAccess = fun (Rs) ->
			mnesia:transaction(fun () ->
						   Os = mnesia:select(local_config,
								      [{{local_config,
									 {access,
									  '$1',
									  '$2'},
									 '$3'},
									[{'==',
									  '$2',
									  Host}],
									['$_']}]),
						   lists:foreach(fun (O) ->
									 mnesia:delete_object(O)
								 end,
								 Os),
						   lists:foreach(fun ({access,
								       Name,
								       Rules}) ->
									 mnesia:write({local_config,
										       {access,
											Name,
											Host},
										       Rules})
								 end,
								 Rs)
					   end)
		end,
    case lists:keysearch(<<"access">>, 1, XData) of
      {value, {_, Strings}} ->
	  String = lists:foldl(fun (S, Res) ->
				       <<Res/binary, S/binary, "\n">>
			       end,
			       <<"">>, Strings),
	  case erl_scan:string(binary_to_list(String)) of
	    {ok, Tokens, _} ->
		case erl_parse:parse_term(Tokens) of
		  {ok, Rs} ->
		      case SetAccess(Rs) of
			{atomic, _} -> {result, []};
			_ -> {error, ?ERR_BAD_REQUEST}
		      end;
		  _ -> {error, ?ERR_BAD_REQUEST}
		end;
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end;
      _ -> {error, ?ERR_BAD_REQUEST}
    end;
set_form(From, Host, ?NS_ADMINL(<<"add-user">>), _Lang,
	 XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    Password = get_value(<<"password">>, XData),
    Password = get_value(<<"password-verify">>, XData),
    AccountJID = jid:from_string(AccountString),
    User = AccountJID#jid.luser,
    Server = AccountJID#jid.lserver,
    true = lists:member(Server, ?MYHOSTS),
    true = Server == Host orelse
	     get_permission_level(From) == global,
    ejabberd_auth:try_register(User, Server, Password),
    {result, []};
set_form(From, Host, ?NS_ADMINL(<<"delete-user">>),
	 _Lang, XData) ->
    AccountStringList = get_values(<<"accountjids">>,
				   XData),
    [_ | _] = AccountStringList,
    ASL2 = lists:map(fun (AccountString) ->
			     JID = jid:from_string(AccountString),
			     User = JID#jid.luser,
			     Server = JID#jid.lserver,
			     true = Server == Host orelse
				      get_permission_level(From) == global,
			     true = ejabberd_auth:is_user_exists(User, Server),
			     {User, Server}
		     end,
		     AccountStringList),
    [ejabberd_auth:remove_user(User, Server)
     || {User, Server} <- ASL2],
    {result, []};
set_form(From, Host, ?NS_ADMINL(<<"end-user-session">>),
	 Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:from_string(AccountString),
    LUser = JID#jid.luser,
    LServer = JID#jid.lserver,
    true = LServer == Host orelse
	     get_permission_level(From) == global,
    Xmlelement = ?SERRT_POLICY_VIOLATION(Lang, <<"has been kicked">>),
    case JID#jid.lresource of
      <<>> ->
	  SIDs = mnesia:dirty_select(session,
				     [{#session{sid = '$1',
						usr = {LUser, LServer, '_'},
						_ = '_'},
				       [], ['$1']}]),
	  [Pid ! {kick, kicked_by_admin, Xmlelement} || {_, Pid} <- SIDs];
      R ->
	  [{_, Pid}] = mnesia:dirty_select(session,
					   [{#session{sid = '$1',
						      usr = {LUser, LServer, R},
						      _ = '_'},
					     [], ['$1']}]),
	  Pid ! {kick, kicked_by_admin, Xmlelement}
    end,
    {result, []};
set_form(From, Host,
	 ?NS_ADMINL(<<"get-user-password">>), Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:from_string(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From) == global,
    Password = ejabberd_auth:get_password(User, Server),
    true = is_binary(Password),
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  ?XFIELD(<<"jid-single">>, <<"Jabber ID">>,
			  <<"accountjid">>, AccountString),
		  ?XFIELD(<<"text-single">>, <<"Password">>,
			  <<"password">>, Password)]}]};
set_form(From, Host,
	 ?NS_ADMINL(<<"change-user-password">>), _Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    Password = get_value(<<"password">>, XData),
    JID = jid:from_string(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From) == global,
    true = ejabberd_auth:is_user_exists(User, Server),
    ejabberd_auth:set_password(User, Server, Password),
    {result, []};
set_form(From, Host,
	 ?NS_ADMINL(<<"get-user-lastlogin">>), Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:from_string(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From) == global,
    FLast = case ejabberd_sm:get_user_resources(User,
						Server)
		of
	      [] ->
		  case get_last_info(User, Server) of
		    not_found -> ?T(Lang, <<"Never">>);
		    {ok, Timestamp, _Status} ->
			Shift = Timestamp,
			TimeStamp = {Shift div 1000000, Shift rem 1000000, 0},
			{{Year, Month, Day}, {Hour, Minute, Second}} =
			    calendar:now_to_local_time(TimeStamp),
			iolist_to_binary(io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
						       [Year, Month, Day, Hour,
							Minute, Second]))
		  end;
	      _ -> ?T(Lang, <<"Online">>)
	    end,
    {result,
     [#xmlel{name = <<"x">>,
	     attrs =
		 [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
	     children =
		 [?HFIELD(),
		  ?XFIELD(<<"jid-single">>, <<"Jabber ID">>,
			  <<"accountjid">>, AccountString),
		  ?XFIELD(<<"text-single">>, <<"Last login">>,
			  <<"lastlogin">>, FLast)]}]};
set_form(From, Host, ?NS_ADMINL(<<"user-stats">>), Lang,
	 XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:from_string(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From) == global,
    Resources = ejabberd_sm:get_user_resources(User,
					       Server),
    IPs1 = [ejabberd_sm:get_user_ip(User, Server, Resource)
	    || Resource <- Resources],
    IPs = [<<(jlib:ip_to_list(IP))/binary, ":",
             (jlib:integer_to_binary(Port))/binary>>
               || {IP, Port} <- IPs1],
    Items = ejabberd_hooks:run_fold(roster_get, Server, [],
				    [{User, Server}]),
    Rostersize = jlib:integer_to_binary(erlang:length(Items)),
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  ?XFIELD(<<"jid-single">>, <<"Jabber ID">>,
			  <<"accountjid">>, AccountString),
		  ?XFIELD(<<"text-single">>, <<"Roster size">>,
			  <<"rostersize">>, Rostersize),
		  ?XMFIELD(<<"text-multi">>, <<"IP addresses">>,
			   <<"ipaddresses">>, IPs),
		  ?XMFIELD(<<"text-multi">>, <<"Resources">>,
			   <<"onlineresources">>, Resources)]}]};
set_form(_From, _Host, _, _Lang, _XData) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.

get_value(Field, XData) -> hd(get_values(Field, XData)).

get_values(Field, XData) ->
    {value, {_, ValueList}} = lists:keysearch(Field, 1,
					      XData),
    ValueList.

search_running_node(SNode) ->
    search_running_node(SNode,
			mnesia:system_info(running_db_nodes)).

search_running_node(_, []) -> false;
search_running_node(SNode, [Node | Nodes]) ->
    case iolist_to_binary(atom_to_list(Node)) of
      SNode -> Node;
      _ -> search_running_node(SNode, Nodes)
    end.

stop_node(From, Host, ENode, Action, XData) ->
    Delay = jlib:binary_to_integer(get_value(<<"delay">>,
					       XData)),
    Subject = case get_value(<<"subject">>, XData) of
		<<"">> -> [];
		S ->
		    [#xmlel{name = <<"field">>,
			    attrs = [{<<"var">>, <<"subject">>}],
			    children =
				[#xmlel{name = <<"value">>, attrs = [],
					children = [{xmlcdata, S}]}]}]
	      end,
    Announcement = case get_values(<<"announcement">>,
				   XData)
		       of
		     [] -> [];
		     As ->
			 [#xmlel{name = <<"field">>,
				 attrs = [{<<"var">>, <<"body">>}],
				 children =
				     [#xmlel{name = <<"value">>, attrs = [],
					     children = [{xmlcdata, Line}]}
				      || Line <- As]}]
		   end,
    case Subject ++ Announcement of
      [] -> ok;
      SubEls ->
	  Request = #adhoc_request{node =
				       ?NS_ADMINX(<<"announce-allhosts">>),
				   action = <<"complete">>,
				   xdata =
				       #xmlel{name = <<"x">>,
					      attrs =
						  [{<<"xmlns">>,
						    ?NS_XDATA},
						   {<<"type">>, <<"submit">>}],
					      children = SubEls},
				   others =
				       [#xmlel{name = <<"x">>,
					       attrs =
						   [{<<"xmlns">>,
						     ?NS_XDATA},
						    {<<"type">>, <<"submit">>}],
					       children = SubEls}]},
	  To = jid:make(<<"">>, Host, <<"">>),
	  mod_announce:announce_commands(empty, From, To, Request)
    end,
    Time = timer:seconds(Delay),
    Node = jlib:binary_to_atom(ENode),
    {ok, _} = timer:apply_after(Time, rpc, call,
				[Node, init, Action, []]),
    {result, []}.

get_last_info(User, Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
      true -> mod_last:get_last_info(User, Server);
      false -> not_found
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adhoc_sm_commands(_Acc, From,
		  #jid{user = User, server = Server, lserver = LServer} =
		      _To,
		  #adhoc_request{lang = Lang, node = <<"config">>,
				 action = Action, xdata = XData} =
		      Request) ->
    case acl:match_rule(LServer, configure, From) of
      deny -> {error, ?ERR_FORBIDDEN};
      allow ->
	  ActionIsExecute = lists:member(Action,
					 [<<"">>, <<"execute">>,
					  <<"complete">>]),
	  if Action == <<"cancel">> ->
		 adhoc:produce_response(Request,
					#adhoc_response{status = canceled});
	     XData == false, ActionIsExecute ->
		 case get_sm_form(User, Server, <<"config">>, Lang) of
		   {result, Form} ->
		       adhoc:produce_response(Request,
					      #adhoc_response{status =
								  executing,
							      elements = Form});
		   {error, Error} -> {error, Error}
		 end;
	     XData /= false, ActionIsExecute ->
		 case jlib:parse_xdata_submit(XData) of
		   invalid -> {error, ?ERR_BAD_REQUEST};
		   Fields ->
		       set_sm_form(User, Server, <<"config">>, Request, Fields)
		 end;
	     true -> {error, ?ERR_BAD_REQUEST}
	  end
    end;
adhoc_sm_commands(Acc, _From, _To, _Request) -> Acc.

get_sm_form(User, Server, <<"config">>, Lang) ->
    {result,
     [#xmlel{name = <<"x">>,
	     attrs = [{<<"xmlns">>, ?NS_XDATA}],
	     children =
		 [?HFIELD(),
		  #xmlel{name = <<"title">>, attrs = [],
			 children =
			     [{xmlcdata,
			       <<(?T(Lang, <<"Administration of ">>))/binary,
				 User/binary>>}]},
		  #xmlel{name = <<"field">>,
			 attrs =
			     [{<<"type">>, <<"list-single">>},
			      {<<"label">>, ?T(Lang, <<"Action on user">>)},
			      {<<"var">>, <<"action">>}],
			 children =
			     [#xmlel{name = <<"value">>, attrs = [],
				     children = [{xmlcdata, <<"edit">>}]},
			      #xmlel{name = <<"option">>,
				     attrs =
					 [{<<"label">>,
					   ?T(Lang, <<"Edit Properties">>)}],
				     children =
					 [#xmlel{name = <<"value">>, attrs = [],
						 children =
						     [{xmlcdata,
						       <<"edit">>}]}]},
			      #xmlel{name = <<"option">>,
				     attrs =
					 [{<<"label">>,
					   ?T(Lang, <<"Remove User">>)}],
				     children =
					 [#xmlel{name = <<"value">>, attrs = [],
						 children =
						     [{xmlcdata,
						       <<"remove">>}]}]}]},
		  ?XFIELD(<<"text-private">>, <<"Password">>,
			  <<"password">>,
			  (ejabberd_auth:get_password_s(User, Server)))]}]};
get_sm_form(_User, _Server, _Node, _Lang) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.

set_sm_form(User, Server, <<"config">>,
	    #adhoc_request{lang = Lang, node = Node,
			   sessionid = SessionID},
	    XData) ->
    Response = #adhoc_response{lang = Lang, node = Node,
			       sessionid = SessionID, status = completed},
    case lists:keysearch(<<"action">>, 1, XData) of
      {value, {_, [<<"edit">>]}} ->
	  case lists:keysearch(<<"password">>, 1, XData) of
	    {value, {_, [Password]}} ->
		ejabberd_auth:set_password(User, Server, Password),
		adhoc:produce_response(Response);
	    _ -> {error, ?ERR_NOT_ACCEPTABLE}
	  end;
      {value, {_, [<<"remove">>]}} ->
	  catch ejabberd_auth:remove_user(User, Server),
	  adhoc:produce_response(Response);
      _ -> {error, ?ERR_NOT_ACCEPTABLE}
    end;
set_sm_form(_User, _Server, _Node, _Request, _Fields) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.

mod_opt_type(_) -> [].
