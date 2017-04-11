%%%----------------------------------------------------------------------
%%% File    : mod_configure.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for online configuration of ejabberd
%%% Created : 19 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-export([start/2, stop/1, reload/3, get_local_identity/5,
	 get_local_features/5, get_local_items/5,
	 adhoc_local_items/4, adhoc_local_commands/4,
	 get_sm_identity/5, get_sm_features/5, get_sm_items/5,
	 adhoc_sm_items/4, adhoc_sm_commands/4, mod_opt_type/1,
	 depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include("ejabberd_sm.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(T(Lang, Text), translate:translate(Lang, Text)).

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
			  get_local_items, 50).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [{mod_adhoc, hard}, {mod_last, soft}].

%%%-----------------------------------------------------------------------

-define(INFO_IDENTITY(Category, Type, Name, Lang),
	[#identity{category = Category, type = Type, name = ?T(Lang, Name)}]).

-define(INFO_COMMAND(Name, Lang),
	?INFO_IDENTITY(<<"automation">>, <<"command-node">>,
		       Name, Lang)).

-define(NODEJID(To, Name, Node),
	#disco_item{jid = To, name = ?T(Lang, Name), node = Node}).

-define(NODE(Name, Node),
	#disco_item{jid = jid:make(Server),
		    node = Node,
		    name = ?T(Lang, Name)}).

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

-define(INFO_RESULT(Allow, Feats, Lang),
	case Allow of
	  deny -> {error, xmpp:err_forbidden(<<"Denied by ACL">>, Lang)};
	  allow -> {result, Feats}
	end).

get_sm_features(Acc, From,
		#jid{lserver = LServer} = _To, Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  Allow = acl:match_rule(LServer, configure, From),
	  case Node of
	    <<"config">> -> ?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    _ -> Acc
	  end
    end.

get_local_features(Acc, From,
		   #jid{lserver = LServer} = _To, Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  LNode = tokenize(Node),
	  Allow = acl:match_rule(LServer, configure, From),
	  case LNode of
	    [<<"config">>] -> ?INFO_RESULT(Allow, [], Lang);
	    [<<"user">>] -> ?INFO_RESULT(Allow, [], Lang);
	    [<<"online users">>] -> ?INFO_RESULT(Allow, [], Lang);
	    [<<"all users">>] -> ?INFO_RESULT(Allow, [], Lang);
	    [<<"all users">>, <<$@, _/binary>>] ->
		?INFO_RESULT(Allow, [], Lang);
	    [<<"outgoing s2s">> | _] -> ?INFO_RESULT(Allow, [], Lang);
	    [<<"running nodes">>] -> ?INFO_RESULT(Allow, [], Lang);
	    [<<"stopped nodes">>] -> ?INFO_RESULT(Allow, [], Lang);
	    [<<"running nodes">>, _ENode] ->
		?INFO_RESULT(Allow, [?NS_STATS], Lang);
	    [<<"running nodes">>, _ENode, <<"DB">>] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    [<<"running nodes">>, _ENode, <<"modules">>] ->
		?INFO_RESULT(Allow, [], Lang);
	    [<<"running nodes">>, _ENode, <<"modules">>, _] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    [<<"running nodes">>, _ENode, <<"backup">>] ->
		?INFO_RESULT(Allow, [], Lang);
	    [<<"running nodes">>, _ENode, <<"backup">>, _] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    [<<"running nodes">>, _ENode, <<"import">>] ->
		?INFO_RESULT(Allow, [], Lang);
	    [<<"running nodes">>, _ENode, <<"import">>, _] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    [<<"running nodes">>, _ENode, <<"restart">>] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    [<<"running nodes">>, _ENode, <<"shutdown">>] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    [<<"config">>, _] ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"add-user">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"delete-user">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"end-user-session">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-user-password">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"change-user-password">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-user-lastlogin">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"user-stats">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-registered-users-num">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-online-users-num">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    _ -> Acc
	  end
    end.

%%%-----------------------------------------------------------------------
-spec adhoc_sm_items(empty | {error, stanza_error()} | {result, [disco_item()]},
		     jid(), jid(), binary()) -> {error, stanza_error()} |
						{result, [disco_item()]} |
						empty.
adhoc_sm_items(Acc, From, #jid{lserver = LServer} = To,
	       Lang) ->
    case acl:match_rule(LServer, configure, From) of
      allow ->
	  Items = case Acc of
		    {result, Its} -> Its;
		    empty -> []
		  end,
	  Nodes = [#disco_item{jid = To, node = <<"config">>,
			       name = ?T(Lang, <<"Configuration">>)}],
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
	    {_, <<"config">>} ->
		  {error, xmpp:err_forbidden(<<"Denied by ACL">>, Lang)};
	    _ -> Acc
	  end
    end.

get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    lists:map(fun (R) ->
		      #disco_item{jid = jid:make(User, Server, R),
				  name = User}
	      end,
	      lists:sort(Rs)).

%%%-----------------------------------------------------------------------

-spec adhoc_local_items(empty | {error, stanza_error()} | {result, [disco_item()]},
			jid(), jid(), binary()) -> {error, stanza_error()} |
						   {result, [disco_item()]} |
						   empty.
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
	  Nodes1 = lists:filter(
		     fun (#disco_item{node = Nd}) ->
			     F = get_local_features([], From, To, Nd, Lang),
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
    lists:flatten(
      lists:map(
	fun(#disco_item{jid = #jid{server = S}, node = Nd} = Item) ->
		if (S /= Server) or
		   (Nd == <<"">>) ->
			[];
		   true ->
			[Item,
			 recursively_get_local_items(
			   PermLev, LServer, Nd, Server, Lang)]
		end
	end,
	Items)).

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
				   jid:encode(To), Lang)
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
				     jid:encode(To), Lang)
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
	  Err = xmpp:err_forbidden(<<"Denied by ACL">>, Lang),
	  case LNode of
	    [<<"config">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"user">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"online users">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"all users">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"all users">>, <<$@, _/binary>>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"outgoing s2s">> | _] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"stopped nodes">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode, <<"DB">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode, <<"modules">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode, <<"modules">>, _] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode, <<"backup">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode, <<"backup">>, _] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode, <<"import">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode, <<"import">>, _] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode, <<"restart">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"running nodes">>, _ENode, <<"shutdown">>] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    [<<"config">>, _] ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"add-user">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"delete-user">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"end-user-session">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-user-password">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"change-user-password">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-user-lastlogin">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"user-stats">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-registered-users-num">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-online-users-num">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
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
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
    try
	[S1, S2] = ejabberd_regexp:split(Diap, <<"-">>),
	N1 = binary_to_integer(S1),
	N2 = binary_to_integer(S2),
	Sub = lists:sublist(SUsers, N1, N2 - N1 + 1),
	{result, lists:map(
		   fun({S, U}) ->
			   #disco_item{jid = jid:make(U, S),
				       name = <<U/binary, $@, S/binary>>}
		   end, Sub)}
    catch _:_ ->
	    xmpp:err_not_acceptable()
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
    {error, xmpp:err_item_not_found()}.

get_online_vh_users(Host) ->
    case catch ejabberd_sm:get_vh_session_list(Host) of
      {'EXIT', _Reason} -> [];
      USRs ->
	  SURs = lists:sort([{S, U, R} || {U, S, R} <- USRs]),
	  lists:map(
	    fun({S, U, R}) ->
		    #disco_item{jid = jid:make(U, S, R),
				name = <<U/binary, "@", S/binary>>}
		    end, SURs)
    end.

get_all_vh_users(Host) ->
    case catch ejabberd_auth:get_vh_registered_users(Host)
	of
      {'EXIT', _Reason} -> [];
      Users ->
	  SUsers = lists:sort([{S, U} || {U, S} <- Users]),
	  case length(SUsers) of
	    N when N =< 100 ->
		lists:map(fun({S, U}) ->
				  #disco_item{jid = jid:make(U, S),
					      name = <<U/binary, $@, S/binary>>}
			  end, SUsers);
	    N ->
		NParts = trunc(math:sqrt(N * 6.17999999999999993783e-1))
			   + 1,
		M = trunc(N / NParts) + 1,
		lists:map(fun (K) ->
				  L = K + M - 1,
				  Node = <<"@",
					   (integer_to_binary(K))/binary,
					   "-",
					   (integer_to_binary(L))/binary>>,
				  {FS, FU} = lists:nth(K, SUsers),
				  {LS, LU} = if L < N -> lists:nth(L, SUsers);
						true -> lists:last(SUsers)
					     end,
				  Name = <<FU/binary, "@", FS/binary, " -- ",
					   LU/binary, "@", LS/binary>>,
				  #disco_item{jid = jid:make(Host),
					      node = <<"all users/", Node/binary>>,
					      name = Name}
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
	  lists:map(
	    fun (T) ->
		    Name = str:format(?T(Lang, <<"To ~s">>),[T]),
		    #disco_item{jid = jid:make(Host),
				node = <<"outgoing s2s/", T/binary>>,
				name = Name}
	    end, lists:usort(TConns))
    end.

get_outgoing_s2s(Host, Lang, To) ->
    case catch ejabberd_s2s:dirty_get_connections() of
      {'EXIT', _Reason} -> [];
      Connections ->
	  lists:map(
	    fun ({F, _T}) ->
		    Node = <<"outgoing s2s/", To/binary, "/", F/binary>>,
		    Name = str:format(?T(Lang, <<"From ~s">>), [F]),
		    #disco_item{jid = jid:make(Host), node = Node, name = Name}
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
	  lists:map(
	    fun (N) ->
		    S = iolist_to_binary(atom_to_list(N)),
		    #disco_item{jid = jid:make(Server),
				node = <<"running nodes/", S/binary>>,
				name = S}
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
	  lists:map(
	    fun (N) ->
		    S = iolist_to_binary(atom_to_list(N)),
		    #disco_item{jid = jid:make(?MYNAME),
				node = <<"stopped nodes/", S/binary>>,
				name = S}
	    end,
	    lists:sort(DBNodes))
    end.

%%-------------------------------------------------------------------------

-define(COMMANDS_RESULT(LServerOrGlobal, From, To,
			Request, Lang),
	case acl:match_rule(LServerOrGlobal, configure, From) of
	  deny -> {error, xmpp:err_forbidden(<<"Denied by ACL">>, Lang)};
	  allow -> adhoc_local_commands(From, To, Request)
	end).

-spec adhoc_local_commands(adhoc_command(), jid(), jid(), adhoc_command()) ->
				  adhoc_command() | {error, stanza_error()}.
adhoc_local_commands(Acc, From,
		     #jid{lserver = LServer} = To,
		     #adhoc_command{node = Node, lang = Lang} = Request) ->
    LNode = tokenize(Node),
    case LNode of
      [<<"running nodes">>, _ENode, <<"DB">>] ->
	  ?COMMANDS_RESULT(global, From, To, Request, Lang);
      [<<"running nodes">>, _ENode, <<"modules">>, _] ->
	  ?COMMANDS_RESULT(LServer, From, To, Request, Lang);
      [<<"running nodes">>, _ENode, <<"backup">>, _] ->
	  ?COMMANDS_RESULT(global, From, To, Request, Lang);
      [<<"running nodes">>, _ENode, <<"import">>, _] ->
	  ?COMMANDS_RESULT(global, From, To, Request, Lang);
      [<<"running nodes">>, _ENode, <<"restart">>] ->
	  ?COMMANDS_RESULT(global, From, To, Request, Lang);
      [<<"running nodes">>, _ENode, <<"shutdown">>] ->
	  ?COMMANDS_RESULT(global, From, To, Request, Lang);
      [<<"config">>, _] ->
	  ?COMMANDS_RESULT(LServer, From, To, Request, Lang);
      ?NS_ADMINL(_) ->
	  ?COMMANDS_RESULT(LServer, From, To, Request, Lang);
      _ -> Acc
    end.

adhoc_local_commands(From,
		     #jid{lserver = LServer} = _To,
		     #adhoc_command{lang = Lang, node = Node,
				    sid = SessionID, action = Action,
				    xdata = XData} = Request) ->
    LNode = tokenize(Node),
    ActionIsExecute = Action == execute orelse Action == complete,
    if Action == cancel ->
	    #adhoc_command{status = canceled, lang = Lang,
			   node = Node, sid = SessionID};
       XData == undefined, ActionIsExecute ->
	   case get_form(LServer, LNode, Lang) of
	     {result, Form} ->
		 xmpp_util:make_adhoc_response(
		   Request,
		   #adhoc_command{status = executing, xdata = Form});
	     {result, Status, Form} ->
		 xmpp_util:make_adhoc_response(
		   Request,
		   #adhoc_command{status = Status, xdata = Form});
	     {error, Error} -> {error, Error}
	   end;
       XData /= undefined, ActionIsExecute ->
	    case catch set_form(From, LServer, LNode, Lang, XData) of
		{result, Res} ->
		    xmpp_util:make_adhoc_response(
		      Request,
		      #adhoc_command{xdata = Res, status = completed});
		{'EXIT', _} -> {error, xmpp:err_bad_request()};
		{error, Error} -> {error, Error}
	    end;
       true ->
	  {error, xmpp:err_bad_request(<<"Unexpected action">>, Lang)}
    end.

-define(TVFIELD(Type, Var, Val),
	#xdata_field{type = Type, var = Var, values = [Val]}).

-define(HFIELD(),
	?TVFIELD(hidden, <<"FORM_TYPE">>, (?NS_ADMIN))).

-define(TLFIELD(Type, Label, Var),
	#xdata_field{type = Type, label = ?T(Lang, Label), var = Var}).

-define(XFIELD(Type, Label, Var, Val),
	#xdata_field{type = Type, label = ?T(Lang, Label),
		     var = Var, values = [Val]}).

-define(XMFIELD(Type, Label, Var, Vals),
	#xdata_field{type = Type, label = ?T(Lang, Label),
		     var = Var, values = Vals}).

-define(TABLEFIELD(Table, Val),
	#xdata_field{
	   type = 'list-single',
	   label = iolist_to_binary(atom_to_list(Table)),
	   var = iolist_to_binary(atom_to_list(Table)),
	   values = [iolist_to_binary(atom_to_list(Val))],
	   options = [#xdata_option{label = ?T(Lang, <<"RAM copy">>),
				    value = <<"ram_copies">>},
		      #xdata_option{label = ?T(Lang, <<"RAM and disc copy">>),
				    value = <<"disc_copies">>},
		      #xdata_option{label = ?T(Lang, <<"Disc only copy">>),
				    value =  <<"disc_only_copies">>},
		      #xdata_option{label = ?T(Lang, <<"Remote copy">>),
				    value = <<"unknown">>}]}).

get_form(_Host, [<<"running nodes">>, ENode, <<"DB">>],
	 Lang) ->
    case search_running_node(ENode) of
      false ->
	  Txt = <<"No running node found">>,
	  {error, xmpp:err_item_not_found(Txt, Lang)};
      Node ->
	  case ejabberd_cluster:call(Node, mnesia, system_info, [tables]) of
	    {badrpc, Reason} ->
		?ERROR_MSG("RPC call mnesia:system_info(tables) on node "
			   "~s failed: ~p", [Node, Reason]),
		{error, xmpp:err_internal_server_error()};
	    Tables ->
		STables = lists:sort(Tables),
		Title = <<(?T(Lang, <<"Database Tables Configuration at ">>))/binary,
			  ENode/binary>>,
		Instr = ?T(Lang, <<"Choose storage type of tables">>),
		try
		    Fs = lists:map(
			   fun(Table) ->
				   case ejabberd_cluster:call(
					  Node, mnesia, table_info,
					  [Table, storage_type]) of
				       Type when is_atom(Type) ->
					   ?TABLEFIELD(Table, Type)
				   end
			   end, STables),
		    {result, #xdata{title = Title,
				    type = form,
				    instructions = [Instr],
				    fields = [?HFIELD()|Fs]}}
		catch _:{case_clause, {badrpc, Reason}} ->
			?ERROR_MSG("RPC call mnesia:table_info/2 "
				   "on node ~s failed: ~p", [Node, Reason]),
			{error, xmpp:err_internal_server_error()}
		end
	  end
    end;
get_form(Host,
	 [<<"running nodes">>, ENode, <<"modules">>, <<"stop">>],
	 Lang) ->
    case search_running_node(ENode) of
      false ->
	  Txt = <<"No running node found">>,
	  {error, xmpp:err_item_not_found(Txt, Lang)};
      Node ->
	  case ejabberd_cluster:call(Node, gen_mod, loaded_modules, [Host]) of
	    {badrpc, Reason} ->
		?ERROR_MSG("RPC call gen_mod:loaded_modules(~s) on node "
			   "~s failed: ~p", [Host, Node, Reason]),
		{error, xmpp:err_internal_server_error()};
	    Modules ->
		SModules = lists:sort(Modules),
		Title = <<(?T(Lang, <<"Stop Modules at ">>))/binary,
			  ENode/binary>>,
		Instr = ?T(Lang, <<"Choose modules to stop">>),
		Fs = lists:map(fun(M) ->
				       S = misc:atom_to_binary(M),
				       ?XFIELD(boolean, S, S, <<"0">>)
			       end, SModules),
		{result, #xdata{title = Title,
				type = form,
				instructions = [Instr],
				fields = [?HFIELD()|Fs]}}
	  end
    end;
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"modules">>,
	  <<"start">>],
	 Lang) ->
    {result,
     #xdata{title = <<(?T(Lang, <<"Start Modules at ">>))/binary, ENode/binary>>,
	    type = form,
	    instructions = [?T(Lang, <<"Enter list of {Module, [Options]}">>)],
	    fields = [?HFIELD(),
		      ?XFIELD('text-multi',
			      <<"List of modules to start">>, <<"modules">>,
			      <<"[].">>)]}};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"backup">>],
	 Lang) ->
    {result,
     #xdata{title = <<(?T(Lang, <<"Backup to File at ">>))/binary, ENode/binary>>,
	    type = form,
	    instructions = [?T(Lang, <<"Enter path to backup file">>)],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', <<"Path to File">>,
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"restore">>],
	 Lang) ->
    {result,
     #xdata{title = <<(?T(Lang, <<"Restore Backup from File at ">>))/binary,
		      ENode/binary>>,
	    type = form,
	    instructions = [?T(Lang, <<"Enter path to backup file">>)],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', <<"Path to File">>,
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"textfile">>],
	 Lang) ->
    {result,
     #xdata{title = <<(?T(Lang, <<"Dump Backup to Text File at ">>))/binary,
		      ENode/binary>>,
	    type = form,
	    instructions = [?T(Lang, <<"Enter path to text file">>)],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', <<"Path to File">>,
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"file">>],
	 Lang) ->
    {result,
     #xdata{title = <<(?T(Lang, <<"Import User from File at ">>))/binary,
		      ENode/binary>>,
	    type = form,
	    instructions = [?T(Lang, <<"Enter path to jabberd14 spool file">>)],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', <<"Path to File">>,
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"dir">>],
	 Lang) ->
    {result,
     #xdata{title = <<(?T(Lang, <<"Import Users from Dir at ">>))/binary,
		      ENode/binary>>,
	    type = form,
	    instructions = [?T(Lang, <<"Enter path to jabberd14 spool dir">>)],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', <<"Path to Dir">>,
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, _ENode, <<"restart">>], Lang) ->
    Make_option =
	fun (LabelNum, LabelUnit, Value) ->
		#xdata_option{
		   label = <<LabelNum/binary, (?T(Lang, LabelUnit))/binary>>,
		   value = Value}
	end,
    {result,
     #xdata{title = ?T(Lang, <<"Restart Service">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{
			 type = 'list-single',
			 label = ?T(Lang, <<"Time delay">>),
			 var = <<"delay">>,
			 required = true,
			 options =
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
			      Make_option(<<"30 ">>, <<"minutes">>, <<"1800">>)]},
		      #xdata_field{type = fixed,
				   label = ?T(Lang,
					      <<"Send announcement to all online users "
						"on all hosts">>)},
		      #xdata_field{var = <<"subject">>,
				   type = 'text-single',
				   label = ?T(Lang, <<"Subject">>)},
		      #xdata_field{var = <<"announcement">>,
				   type = 'text-multi',
				   label = ?T(Lang, <<"Message body">>)}]}};
get_form(_Host,
	 [<<"running nodes">>, _ENode, <<"shutdown">>], Lang) ->
    Make_option =
	fun (LabelNum, LabelUnit, Value) ->
		#xdata_option{
		   label = <<LabelNum/binary, (?T(Lang, LabelUnit))/binary>>,
		   value = Value}
	end,
    {result,
     #xdata{title = ?T(Lang, <<"Shut Down Service">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{
			 type = 'list-single',
			 label = ?T(Lang, <<"Time delay">>),
			 var = <<"delay">>,
			 required = true,
			 options =
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
			      Make_option(<<"30 ">>, <<"minutes">>, <<"1800">>)]},
		      #xdata_field{type = fixed,
				   label = ?T(Lang,
					      <<"Send announcement to all online users "
						"on all hosts">>)},
		      #xdata_field{var = <<"subject">>,
				   type = 'text-single',
				   label = ?T(Lang, <<"Subject">>)},
		      #xdata_field{var = <<"announcement">>,
				   type = 'text-multi',
				   label = ?T(Lang, <<"Message body">>)}]}};
get_form(Host, [<<"config">>, <<"acls">>], Lang) ->
    ACLs = str:tokens(
	     str:format("~p.",
			[mnesia:dirty_select(
			   acl,
			   ets:fun2ms(
			     fun({acl, {Name, H}, Spec}) when H == Host ->
				     {acl, Name, Spec}
			     end))]),
	     <<"\n">>),
    {result,
     #xdata{title = ?T(Lang, <<"Access Control List Configuration">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'text-multi',
				   label = ?T(Lang, <<"Access control lists">>),
				   var = <<"acls">>,
				   values = ACLs}]}};
get_form(Host, [<<"config">>, <<"access">>], Lang) ->
    Accs = str:tokens(
	     str:format("~p.",
			[mnesia:dirty_select(
			   access,
			   ets:fun2ms(
			     fun({access, {Name, H}, Acc}) when H == Host ->
				     {access, Name, Acc}
			     end))]),
	     <<"\n">>),
    {result,
     #xdata{title = ?T(Lang, <<"Access Configuration">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'text-multi',
				   label = ?T(Lang, <<"Access rules">>),
				   var = <<"access">>,
				   values = Accs}]}};
get_form(_Host, ?NS_ADMINL(<<"add-user">>), Lang) ->
    {result,
     #xdata{title = ?T(Lang, <<"Add User">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = ?T(Lang, <<"Jabber ID">>),
				   required = true,
				   var = <<"accountjid">>},
		      #xdata_field{type = 'text-private',
				   label = ?T(Lang, <<"Password">>),
				   required = true,
				   var = <<"password">>},
		      #xdata_field{type = 'text-private',
				   label = ?T(Lang, <<"Password Verification">>),
				   required = true,
				   var = <<"password-verify">>}]}};
get_form(_Host, ?NS_ADMINL(<<"delete-user">>), Lang) ->
    {result,
     #xdata{title = ?T(Lang, <<"Delete User">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = ?T(Lang, <<"Jabber ID">>),
				   required = true,
				   var = <<"accountjids">>}]}};
get_form(_Host, ?NS_ADMINL(<<"end-user-session">>),
	 Lang) ->
    {result,
     #xdata{title = ?T(Lang, <<"End User Session">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = ?T(Lang, <<"Jabber ID">>),
				   required = true,
				   var = <<"accountjid">>}]}};
get_form(_Host, ?NS_ADMINL(<<"get-user-password">>),
	 Lang) ->
    {result,
     #xdata{title = ?T(Lang, <<"Get User Password">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = ?T(Lang, <<"Jabber ID">>),
				   var = <<"accountjid">>,
				   required = true}]}};
get_form(_Host, ?NS_ADMINL(<<"change-user-password">>),
	 Lang) ->
    {result,
     #xdata{title = ?T(Lang, <<"Get User Password">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = ?T(Lang, <<"Jabber ID">>),
				   required = true,
				   var = <<"accountjid">>},
		      #xdata_field{type = 'text-private',
				   label = ?T(Lang, <<"Password">>),
				   required = true,
				   var = <<"password">>}]}};
get_form(_Host, ?NS_ADMINL(<<"get-user-lastlogin">>),
	 Lang) ->
    {result,
     #xdata{title = ?T(Lang, <<"Get User Last Login Time">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = ?T(Lang, <<"Jabber ID">>),
				   var = <<"accountjid">>,
				   required = true}]}};
get_form(_Host, ?NS_ADMINL(<<"user-stats">>), Lang) ->
    {result,
     #xdata{title = ?T(Lang, <<"Get User Statistics">>),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = ?T(Lang, <<"Jabber ID">>),
				   var = <<"accountjid">>,
				   required = true}]}};
get_form(Host,
	 ?NS_ADMINL(<<"get-registered-users-num">>), Lang) ->
    Num = integer_to_binary(ejabberd_auth:get_vh_registered_users_number(Host)),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'text-single',
				   label = ?T(Lang, <<"Number of registered users">>),
				   var = <<"registeredusersnum">>,
				   values = [Num]}]}};
get_form(Host, ?NS_ADMINL(<<"get-online-users-num">>),
	 Lang) ->
    Num = integer_to_binary(ejabberd_sm:get_vh_session_number(Host)),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'text-single',
				   label = ?T(Lang, <<"Number of online users">>),
				   var = <<"onlineusersnum">>,
				   values = [Num]}]}};
get_form(_Host, _, _Lang) ->
    {error, xmpp:err_service_unavailable()}.

set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"DB">>], Lang, XData) ->
    case search_running_node(ENode) of
      false ->
	  Txt = <<"No running node found">>,
	  {error, xmpp:err_item_not_found(Txt, Lang)};
      Node ->
	  lists:foreach(
	    fun(#xdata_field{var = SVar, values = SVals}) ->
		    Table = misc:binary_to_atom(SVar),
		    Type = case SVals of
			       [<<"unknown">>] -> unknown;
			       [<<"ram_copies">>] -> ram_copies;
			       [<<"disc_copies">>] -> disc_copies;
			       [<<"disc_only_copies">>] -> disc_only_copies;
			       _ -> false
			   end,
		    if Type == false -> ok;
		       Type == unknown ->
			    mnesia:del_table_copy(Table, Node);
		       true ->
			    case mnesia:add_table_copy(Table, Node, Type) of
				{aborted, _} ->
				    mnesia:change_table_copy_type(
				      Table, Node, Type);
				_ -> ok
			    end
		    end
	    end, XData#xdata.fields),
	    {result, undefined}
    end;
set_form(_From, Host,
	 [<<"running nodes">>, ENode, <<"modules">>, <<"stop">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
      false ->
	  Txt = <<"No running node found">>,
	  {error, xmpp:err_item_not_found(Txt, Lang)};
      Node ->
	  lists:foreach(
	    fun(#xdata_field{var = Var, values = Vals}) ->
		    case Vals of
			[<<"1">>] ->
			    Module = misc:binary_to_atom(Var),
			    ejabberd_cluster:call(Node, gen_mod, stop_module,
						  [Host, Module]);
			_ -> ok
		    end
	    end, XData#xdata.fields),
	  {result, undefined}
    end;
set_form(_From, Host,
	 [<<"running nodes">>, ENode, <<"modules">>,
	  <<"start">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = <<"No running node found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"modules">>, XData) of
		[] ->
		    Txt = <<"No 'modules' found in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)};
		Strings ->
		    String = lists:foldl(fun (S, Res) ->
						 <<Res/binary, S/binary, "\n">>
					 end, <<"">>, Strings),
		    case erl_scan:string(binary_to_list(String)) of
			{ok, Tokens, _} ->
			    case erl_parse:parse_term(Tokens) of
				{ok, Modules} ->
				    lists:foreach(
				      fun ({Module, Args}) ->
					      ejabberd_cluster:call(
						Node, gen_mod, start_module,
						[Host, Module, Args])
				      end,
				      Modules),
				    {result, undefined};
				_ ->
				    Txt = <<"Parse failed">>,
				    {error, xmpp:err_bad_request(Txt, Lang)}
			    end;
			_ ->
			    Txt = <<"Scan failed">>,
			    {error, xmpp:err_bad_request(Txt, Lang)}
		    end
	    end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"backup">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = <<"No running node found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = <<"No 'path' found in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    case ejabberd_cluster:call(Node, mnesia, backup, [String]) of
			{badrpc, Reason} ->
			    ?ERROR_MSG("RPC call mnesia:backup(~s) to node ~s "
				       "failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			{error, Reason} ->
			    ?ERROR_MSG("RPC call mnesia:backup(~s) to node ~s "
				       "failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			_ ->
			    {result, undefined}
		    end;
		_ ->
		    Txt = <<"Incorrect value of 'path' in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"restore">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = <<"No running node found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = <<"No 'path' found in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    case ejabberd_cluster:call(Node, ejabberd_admin,
					       restore, [String]) of
			{badrpc, Reason} ->
			    ?ERROR_MSG("RPC call ejabberd_admin:restore(~s) to node "
				       "~s failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			{error, Reason} ->
			    ?ERROR_MSG("RPC call ejabberd_admin:restore(~s) to node "
				       "~s failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			_ ->
			    {result, undefined}
		    end;
		_ ->
		    Txt = <<"Incorrect value of 'path' in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"textfile">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = <<"No running node found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = <<"No 'path' found in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    case ejabberd_cluster:call(Node, ejabberd_admin,
					       dump_to_textfile, [String]) of
			{badrpc, Reason} ->
			    ?ERROR_MSG("RPC call ejabberd_admin:dump_to_textfile(~s) "
				       "to node ~s failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			{error, Reason} ->
			    ?ERROR_MSG("RPC call ejabberd_admin:dump_to_textfile(~s) "
				       "to node ~s failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			_ ->
			    {result, undefined}
		    end;
		_ ->
		    Txt = <<"Incorrect value of 'path' in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"file">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = <<"No running node found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = <<"No 'path' found in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    ejabberd_cluster:call(Node, jd2ejd, import_file, [String]),
		    {result, undefined};
		_ ->
		    Txt = <<"Incorrect value of 'path' in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"dir">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = <<"No running node found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = <<"No 'path' found in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    ejabberd_cluster:call(Node, jd2ejd, import_dir, [String]),
		    {result, undefined};
		_ ->
		    Txt = <<"Incorrect value of 'path' in data form">>,
		    {error, xmpp:err_bad_request(Txt, Lang)}
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
set_form(_From, Host, [<<"config">>, <<"acls">>], Lang,
	 XData) ->
    case xmpp_util:get_xdata_values(<<"acls">>, XData) of
	[] ->
	    Txt = <<"No 'acls' found in data form">>,
	    {error, xmpp:err_bad_request(Txt, Lang)};
	Strings ->
	    String = lists:foldl(fun (S, Res) ->
					 <<Res/binary, S/binary, "\n">>
				 end, <<"">>, Strings),
	    case erl_scan:string(binary_to_list(String)) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, ACLs} ->
			    acl:add_list(Host, ACLs, true),
			    {result, undefined};
			_ ->
			    Txt = <<"Parse failed">>,
			    {error, xmpp:err_bad_request(Txt, Lang)}
		    end;
		_ ->
		    {error, xmpp:err_bad_request(<<"Scan failed">>, Lang)}
	    end
    end;
set_form(_From, Host, [<<"config">>, <<"access">>],
	 Lang, XData) ->
    SetAccess =
	fun(Rs) ->
		mnesia:transaction(
		  fun () ->
			  Os = mnesia:select(
				 access,
				 ets:fun2ms(
				   fun({access, {_, H}, _} = O) when H == Host ->
					   O
				   end)),
			  lists:foreach(fun mnesia:delete_object/1, Os),
			  lists:foreach(
			    fun({access, Name, Rules}) ->
				    mnesia:write({access, {Name, Host}, Rules})
			    end, Rs)
		  end)
	end,
    case xmpp_util:get_xdata_values(<<"access">>, XData) of
	[] ->
	    Txt = <<"No 'access' found in data form">>,
	    {error, xmpp:err_bad_request(Txt, Lang)};
	Strings ->
	    String = lists:foldl(fun (S, Res) ->
					 <<Res/binary, S/binary, "\n">>
				 end, <<"">>, Strings),
	    case erl_scan:string(binary_to_list(String)) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, Rs} ->
			    case SetAccess(Rs) of
				{atomic, _} ->
				    {result, undefined};
				_ ->
				    {error, xmpp:err_bad_request()}
			    end;
			_ ->
			    Txt = <<"Parse failed">>,
			    {error, xmpp:err_bad_request(Txt, Lang)}
		    end;
		_ ->
		    {error, xmpp:err_bad_request(<<"Scan failed">>, Lang)}
	    end
    end;
set_form(From, Host, ?NS_ADMINL(<<"add-user">>), _Lang,
	 XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    Password = get_value(<<"password">>, XData),
    Password = get_value(<<"password-verify">>, XData),
    AccountJID = jid:decode(AccountString),
    User = AccountJID#jid.luser,
    Server = AccountJID#jid.lserver,
    true = lists:member(Server, ?MYHOSTS),
    true = Server == Host orelse
	     get_permission_level(From) == global,
    ejabberd_auth:try_register(User, Server, Password),
    {result, undefined};
set_form(From, Host, ?NS_ADMINL(<<"delete-user">>),
	 _Lang, XData) ->
    AccountStringList = get_values(<<"accountjids">>,
				   XData),
    [_ | _] = AccountStringList,
    ASL2 = lists:map(fun (AccountString) ->
			     JID = jid:decode(AccountString),
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
    {result, undefined};
set_form(From, Host, ?NS_ADMINL(<<"end-user-session">>),
	 Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:decode(AccountString),
    LUser = JID#jid.luser,
    LServer = JID#jid.lserver,
    true = LServer == Host orelse
	     get_permission_level(From) == global,
    Xmlelement = xmpp:serr_policy_violation(<<"has been kicked">>, Lang),
    case JID#jid.lresource of
      <<>> ->
	  SIs = mnesia:dirty_select(session,
				    [{#session{usr = {LUser, LServer, '_'},
					       sid = '$1',
					       info = '$2',
						_ = '_'},
				      [], [{{'$1', '$2'}}]}]),
	  Pids = [P || {{_, P}, Info} <- SIs,
		       not proplists:get_bool(offline, Info)],
	  lists:foreach(fun(Pid) ->
				Pid ! {kick, kicked_by_admin, Xmlelement}
			end, Pids);
      R ->
	  [{{_, Pid}, Info}] = mnesia:dirty_select(
				 session,
				 [{#session{usr = {LUser, LServer, R},
					    sid = '$1',
					    info = '$2',
						      _ = '_'},
				   [], [{{'$1', '$2'}}]}]),
	  case proplists:get_bool(offline, Info) of
	    true -> ok;
	    false -> Pid ! {kick, kicked_by_admin, Xmlelement}
	  end
    end,
    {result, undefined};
set_form(From, Host,
	 ?NS_ADMINL(<<"get-user-password">>), Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:decode(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From) == global,
    Password = ejabberd_auth:get_password(User, Server),
    true = is_binary(Password),
    {result,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      ?XFIELD('jid-single', <<"Jabber ID">>,
			      <<"accountjid">>, AccountString),
		      ?XFIELD('text-single', <<"Password">>,
			      <<"password">>, Password)]}};
set_form(From, Host,
	 ?NS_ADMINL(<<"change-user-password">>), _Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    Password = get_value(<<"password">>, XData),
    JID = jid:decode(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From) == global,
    true = ejabberd_auth:is_user_exists(User, Server),
    ejabberd_auth:set_password(User, Server, Password),
    {result, undefined};
set_form(From, Host,
	 ?NS_ADMINL(<<"get-user-lastlogin">>), Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:decode(AccountString),
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
			(str:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
						       [Year, Month, Day, Hour,
							Minute, Second]))
		  end;
	      _ -> ?T(Lang, <<"Online">>)
	    end,
    {result,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      ?XFIELD('jid-single', <<"Jabber ID">>,
			      <<"accountjid">>, AccountString),
		      ?XFIELD('text-single', <<"Last login">>,
			      <<"lastlogin">>, FLast)]}};
set_form(From, Host, ?NS_ADMINL(<<"user-stats">>), Lang,
	 XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:decode(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From) == global,
    Resources = ejabberd_sm:get_user_resources(User,
					       Server),
    IPs1 = [ejabberd_sm:get_user_ip(User, Server, Resource)
	    || Resource <- Resources],
    IPs = [<<(misc:ip_to_list(IP))/binary, ":",
             (integer_to_binary(Port))/binary>>
               || {IP, Port} <- IPs1],
    Items = ejabberd_hooks:run_fold(roster_get, Server, [],
				    [{User, Server}]),
    Rostersize = integer_to_binary(erlang:length(Items)),
    {result,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      ?XFIELD('jid-single', <<"Jabber ID">>,
			      <<"accountjid">>, AccountString),
		      ?XFIELD('text-single', <<"Roster size">>,
			      <<"rostersize">>, Rostersize),
		      ?XMFIELD('text-multi', <<"IP addresses">>,
			       <<"ipaddresses">>, IPs),
		      ?XMFIELD('text-multi', <<"Resources">>,
			       <<"onlineresources">>, Resources)]}};
set_form(_From, _Host, _, _Lang, _XData) ->
    {error, xmpp:err_service_unavailable()}.

get_value(Field, XData) -> hd(get_values(Field, XData)).

get_values(Field, XData) ->
    xmpp_util:get_xdata_values(Field, XData).

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
    Delay = binary_to_integer(get_value(<<"delay">>, XData)),
    Subject = case get_value(<<"subject">>, XData) of
		  <<"">> ->
		      [];
		  S ->
		      [#xdata_field{var = <<"subject">>, values = [S]}]
	      end,
    Announcement = case get_values(<<"announcement">>, XData) of
		       [] ->
			   [];
		       As ->
			   [#xdata_field{var = <<"body">>, values = As}]
		   end,
    case Subject ++ Announcement of
	[] ->
	    ok;
	Fields ->
	    Request = #adhoc_command{node = ?NS_ADMINX(<<"announce-allhosts">>),
				     action = complete,
				     xdata = #xdata{type = submit,
						    fields = Fields}},
	    To = jid:make(Host),
	    mod_announce:announce_commands(empty, From, To, Request)
    end,
    Time = timer:seconds(Delay),
    Node = misc:binary_to_atom(ENode),
    {ok, _} = timer:apply_after(Time, rpc, call, [Node, init, Action, []]),
    {result, undefined}.

get_last_info(User, Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
      true -> mod_last:get_last_info(User, Server);
      false -> not_found
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec adhoc_sm_commands(adhoc_command(), jid(), jid(), adhoc_command()) -> adhoc_command().
adhoc_sm_commands(_Acc, From,
		  #jid{user = User, server = Server, lserver = LServer},
		  #adhoc_command{lang = Lang, node = <<"config">>,
				 action = Action, xdata = XData} = Request) ->
    case acl:match_rule(LServer, configure, From) of
	deny ->
	    {error, xmpp:err_forbidden(<<"Denied by ACL">>, Lang)};
	allow ->
	    ActionIsExecute = Action == execute orelse Action == complete,
	    if Action == cancel ->
		    xmpp_util:make_adhoc_response(
		      Request, #adhoc_command{status = canceled});
	       XData == undefined, ActionIsExecute ->
		    case get_sm_form(User, Server, <<"config">>, Lang) of
			{result, Form} ->
			    xmpp_util:make_adhoc_response(
			      Request, #adhoc_command{status = executing,
						      xdata = Form});
			{error, Error} ->
			    {error, Error}
		    end;
	       XData /= undefined, ActionIsExecute ->
		    set_sm_form(User, Server, <<"config">>, Request);
	       true ->
		    Txt = <<"Unexpected action">>,
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
adhoc_sm_commands(Acc, _From, _To, _Request) -> Acc.

get_sm_form(User, Server, <<"config">>, Lang) ->
    {result,
     #xdata{type = form,
	    title = <<(?T(Lang, <<"Administration of ">>))/binary, User/binary>>,
	    fields =
		[?HFIELD(),
		 #xdata_field{
		    type = 'list-single',
		    label = ?T(Lang, <<"Action on user">>),
		    var = <<"action">>,
		    values = [<<"edit">>],
		    options = [#xdata_option{
				  label = ?T(Lang, <<"Edit Properties">>),
				  value = <<"edit">>},
			       #xdata_option{
				  label = ?T(Lang, <<"Remove User">>),
				  value = <<"remove">>}]},
		 ?XFIELD('text-private', <<"Password">>,
			 <<"password">>,
			 ejabberd_auth:get_password_s(User, Server))]}};
get_sm_form(_User, _Server, _Node, _Lang) ->
    {error, xmpp:err_service_unavailable()}.

set_sm_form(User, Server, <<"config">>,
	    #adhoc_command{lang = Lang, node = Node,
			   sid = SessionID, xdata = XData}) ->
    Response = #adhoc_command{lang = Lang, node = Node,
			      sid = SessionID, status = completed},
    case xmpp_util:get_xdata_values(<<"action">>, XData) of
	[<<"edit">>] ->
	    case xmpp_util:get_xdata_values(<<"password">>, XData) of
		[Password] ->
		    ejabberd_auth:set_password(User, Server, Password),
		    xmpp_util:make_adhoc_response(Response);
		_ ->
		    Txt = <<"No 'password' found in data form">>,
		    {error, xmpp:err_not_acceptable(Txt, Lang)}
	    end;
	[<<"remove">>] ->
	    catch ejabberd_auth:remove_user(User, Server),
	    xmpp_util:make_adhoc_response(Response);
	_ ->
	    Txt = <<"Incorrect value of 'action' in data form">>,
	    {error, xmpp:err_not_acceptable(Txt, Lang)}
    end;
set_sm_form(_User, _Server, _Node, _Request) ->
    {error, xmpp:err_service_unavailable()}.

mod_opt_type(_) -> [].
