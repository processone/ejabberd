%%%----------------------------------------------------------------------
%%% File    : mod_configure.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for online configuration of ejabberd using XEP-0050
%%% Created : 19 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-protocol({xep, 133, '1.3.1', '13.10', "partial", ""}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, get_local_identity/5,
	 get_local_features/5, get_local_items/5,
	 adhoc_local_items/4, adhoc_local_commands/4,
	 get_sm_identity/5, get_sm_features/5, get_sm_items/5,
	 adhoc_sm_items/4, adhoc_sm_commands/4, mod_options/1,
	 mod_opt_type/1,
	 depends/2, mod_doc/0]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_sm.hrl").
-include("translate.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

start(_Host, _Opts) ->
    {ok, [{hook, disco_local_items, get_local_items, 50},
          {hook, disco_local_features, get_local_features, 50},
          {hook, disco_local_identity, get_local_identity, 50},
          {hook, disco_sm_items, get_sm_items, 50},
          {hook, disco_sm_features, get_sm_features, 50},
          {hook, disco_sm_identity, get_sm_identity, 50},
          {hook, adhoc_local_items, adhoc_local_items, 50},
          {hook, adhoc_local_commands, adhoc_local_commands, 50},
          {hook, adhoc_sm_items, adhoc_sm_items, 50},
          {hook, adhoc_sm_commands, adhoc_sm_commands, 50}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [{mod_adhoc, hard}, {mod_last, soft}].

%%%-----------------------------------------------------------------------

-define(INFO_IDENTITY(Category, Type, Name, Lang),
	[#identity{category = Category, type = Type, name = tr(Lang, Name)}]).

-define(INFO_COMMAND(Name, Lang),
	?INFO_IDENTITY(<<"automation">>, <<"command-node">>,
		       Name, Lang)).

-define(NODEJID(To, Name, Node),
	#disco_item{jid = To, name = tr(Lang, Name), node = Node}).

-define(NODE(Name, Node),
	#disco_item{jid = jid:make(Server),
		    node = Node,
		    name = tr(Lang, Name)}).

-define(NS_ADMINX(Sub),
	<<(?NS_ADMIN)/binary, "#", Sub/binary>>).

-define(NS_ADMINL(Sub),
	[<<"http:">>, <<"jabber.org">>, <<"protocol">>,
	 <<"admin">>, Sub]).

-spec tokenize(binary()) -> [binary()].
tokenize(Node) -> str:tokens(Node, <<"/#">>).

acl_match_rule(Host, From) ->
    Access = mod_configure_opt:access(Host),
    acl:match_rule(Host, Access, From).

-spec get_sm_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
get_sm_identity(Acc, _From, _To, Node, Lang) ->
    case Node of
      <<"config">> ->
	  ?INFO_COMMAND(?T("Configuration"), Lang);
      _ -> Acc
    end.

-spec get_local_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
get_local_identity(Acc, _From, _To, Node, Lang) ->
    LNode = tokenize(Node),
    case LNode of
      [<<"running nodes">>, ENode] ->
	  ?INFO_IDENTITY(<<"ejabberd">>, <<"node">>, ENode, Lang);
      [<<"running nodes">>, _ENode, <<"DB">>] ->
	  ?INFO_COMMAND(?T("Database"), Lang);
      [<<"running nodes">>, _ENode, <<"backup">>,
       <<"backup">>] ->
	  ?INFO_COMMAND(?T("Backup"), Lang);
      [<<"running nodes">>, _ENode, <<"backup">>,
       <<"restore">>] ->
	  ?INFO_COMMAND(?T("Restore"), Lang);
      [<<"running nodes">>, _ENode, <<"backup">>,
       <<"textfile">>] ->
	  ?INFO_COMMAND(?T("Dump to Text File"), Lang);
      [<<"running nodes">>, _ENode, <<"import">>,
       <<"file">>] ->
	  ?INFO_COMMAND(?T("Import File"), Lang);
      [<<"running nodes">>, _ENode, <<"import">>,
       <<"dir">>] ->
	  ?INFO_COMMAND(?T("Import Directory"), Lang);
      [<<"running nodes">>, _ENode, <<"restart">>] ->
	  ?INFO_COMMAND(?T("Restart Service"), Lang);
      [<<"running nodes">>, _ENode, <<"shutdown">>] ->
	  ?INFO_COMMAND(?T("Shut Down Service"), Lang);
      ?NS_ADMINL(<<"add-user">>) ->
	  ?INFO_COMMAND(?T("Add User"), Lang);
      ?NS_ADMINL(<<"delete-user">>) ->
	  ?INFO_COMMAND(?T("Delete User"), Lang);
      ?NS_ADMINL(<<"disable-user">>) ->
	  ?INFO_COMMAND(?T("Disable User"), Lang);
      ?NS_ADMINL(<<"reenable-user">>) ->
	  ?INFO_COMMAND(?T("Re-Enable User"), Lang);
      ?NS_ADMINL(<<"end-user-session">>) ->
	  ?INFO_COMMAND(?T("End User Session"), Lang);
      ?NS_ADMINL(<<"change-user-password">>) ->
	  ?INFO_COMMAND(?T("Change User Password"), Lang);
      ?NS_ADMINL(<<"get-user-roster">>) ->
	  ?INFO_COMMAND(?T("Get User Roster"), Lang);
      ?NS_ADMINL(<<"get-user-lastlogin">>) ->
	  ?INFO_COMMAND(?T("Get User Last Login Time"), Lang);
      ?NS_ADMINL(<<"user-stats">>) ->
	  ?INFO_COMMAND(?T("Get User Statistics"), Lang);
      ?NS_ADMINL(<<"get-registered-users-num">>) ->
	  ?INFO_COMMAND(?T("Get Number of Registered Users"),
			Lang);
      ?NS_ADMINL(<<"get-disabled-users-num">>) ->
	  ?INFO_COMMAND(?T("Get Number of Disabled Users"),
			Lang);
      ?NS_ADMINL(<<"get-online-users-num">>) ->
	  ?INFO_COMMAND(?T("Get Number of Online Users"), Lang);
      ?NS_ADMINL(<<"get-active-users-num">>) ->
	  ?INFO_COMMAND(?T("Get Number of Active Users"), Lang);
      ?NS_ADMINL(<<"get-idle-users-num">>) ->
	  ?INFO_COMMAND(?T("Get Number of Idle Users"), Lang);
      ?NS_ADMINL(<<"get-registered-users-list">>) ->
	  ?INFO_COMMAND(?T("Get List of Registered Users"),
			Lang);
      ?NS_ADMINL(<<"get-disabled-users-list">>) ->
	  ?INFO_COMMAND(?T("Get List of Disabled Users"),
			Lang);
      ?NS_ADMINL(<<"get-online-users-list">>) ->
	  ?INFO_COMMAND(?T("Get List of Online Users"), Lang);
      ?NS_ADMINL(<<"get-active-users">>) ->
	  ?INFO_COMMAND(?T("Get List of Active Users"), Lang);
      ?NS_ADMINL(<<"get-idle-users">>) ->
	  ?INFO_COMMAND(?T("Get List of Idle Users"), Lang);
      ?NS_ADMINL(<<"restart">>) ->
	  ?INFO_COMMAND(?T("Restart Service"), Lang);
      ?NS_ADMINL(<<"shutdown">>) ->
	  ?INFO_COMMAND(?T("Shut Down Service"), Lang);
      _ -> Acc
    end.

%%%-----------------------------------------------------------------------

-define(INFO_RESULT(Allow, Feats, Lang),
	case Allow of
	  deny -> {error, xmpp:err_forbidden(?T("Access denied by service policy"), Lang)};
	  allow -> {result, Feats}
	end).

-spec get_sm_features(mod_disco:features_acc(), jid(), jid(),
		      binary(), binary()) -> mod_disco:features_acc().
get_sm_features(Acc, From,
		#jid{lserver = LServer} = _To, Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  Allow = acl_match_rule(LServer, From),
	  case Node of
	    <<"config">> -> ?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    _ -> Acc
	  end
    end.

-spec get_local_features(mod_disco:features_acc(), jid(), jid(),
			 binary(), binary()) -> mod_disco:features_acc().
get_local_features(Acc, From,
		   #jid{lserver = LServer} = _To, Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  LNode = tokenize(Node),
	  Allow = acl_match_rule(LServer, From),
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
	    ?NS_ADMINL(<<"disable-user">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"reenable-user">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"end-user-session">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"change-user-password">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-user-roster">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-user-lastlogin">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"user-stats">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-registered-users-num">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-disabled-users-num">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-online-users-num">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-active-users-num">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-idle-users-num">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-registered-users-list">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-disabled-users-list">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-online-users-list">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-active-users">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"get-idle-users">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"restart">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    ?NS_ADMINL(<<"shutdown">>) ->
		?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
	    _ -> Acc
	  end
    end.

%%%-----------------------------------------------------------------------
-spec adhoc_sm_items(mod_disco:items_acc(),
		     jid(), jid(), binary()) -> mod_disco:items_acc().
adhoc_sm_items(Acc, From, #jid{lserver = LServer} = To,
	       Lang) ->
    case acl_match_rule(LServer, From) of
      allow ->
	  Items = case Acc of
		    {result, Its} -> Its;
		    empty -> []
		  end,
	  Nodes = [#disco_item{jid = To, node = <<"config">>,
			       name = tr(Lang, ?T("Configuration"))}],
	  {result, Items ++ Nodes};
      _ -> Acc
    end.

%%%-----------------------------------------------------------------------
-spec get_sm_items(mod_disco:items_acc(), jid(), jid(),
		   binary(), binary()) -> mod_disco:items_acc().
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
	  case {acl_match_rule(LServer, From), Node} of
	    {allow, <<"">>} ->
		Nodes = [?NODEJID(To, ?T("Configuration"),
				  <<"config">>),
			 ?NODEJID(To, ?T("User Management"), <<"user">>)],
		{result,
		 Items ++ Nodes ++ get_user_resources(User, Server)};
	    {allow, <<"config">>} -> {result, []};
	    {_, <<"config">>} ->
		  {error, xmpp:err_forbidden(?T("Access denied by service policy"), Lang)};
	    _ -> Acc
	  end
    end.

-spec get_user_resources(binary(), binary()) -> [disco_item()].
get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    lists:map(fun (R) ->
		      #disco_item{jid = jid:make(User, Server, R),
				  name = User}
	      end,
	      lists:sort(Rs)).

%%%-----------------------------------------------------------------------

-spec adhoc_local_items(mod_disco:items_acc(),
			jid(), jid(), binary()) -> mod_disco:items_acc().
adhoc_local_items(Acc, From,
		  #jid{lserver = LServer, server = Server} = To, Lang) ->
    case acl_match_rule(LServer, From) of
      allow ->
	  Items = case Acc of
		    {result, Its} -> Its;
		    empty -> []
		  end,
	  PermLev = get_permission_level(From, LServer),
	  Nodes = recursively_get_local_items(PermLev, LServer,
					      <<"">>, Server, Lang),
	  Nodes1 = lists:filter(
		     fun (#disco_item{node = Nd}) ->
			     F = get_local_features(empty, From, To, Nd, Lang),
			     case F of
				 {result, [?NS_COMMANDS]} -> true;
				 _ -> false
			     end
		     end,
		     Nodes),
	  {result, Items ++ Nodes1};
      _ -> Acc
    end.

-spec recursively_get_local_items(global | vhost, binary(), binary(),
				  binary(), binary()) -> [disco_item()].
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

-spec get_permission_level(jid(), binary()) -> global | vhost.
get_permission_level(JID, Host) ->
    Access = mod_configure_opt:access(Host),
    case acl:match_rule(global, Access, JID) of
      allow -> global;
      deny -> vhost
    end.

%%%-----------------------------------------------------------------------

-define(ITEMS_RESULT(Allow, LNode, Fallback),
	case Allow of
	  deny -> Fallback;
	  allow ->
	      PermLev = get_permission_level(From, LServer),
	      case get_local_items({PermLev, LServer}, LNode,
				   jid:encode(To), Lang)
		  of
		{result, Res} -> {result, Res};
		{error, Error} -> {error, Error}
	      end
	end).

-spec get_local_items(mod_disco:items_acc(), jid(), jid(),
		      binary(), binary()) -> mod_disco:items_acc().
get_local_items(Acc, From, #jid{lserver = LServer} = To,
		<<"">>, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
      false -> Acc;
      _ ->
	  Items = case Acc of
		    {result, Its} -> Its;
		    empty -> []
		  end,
	  Allow = acl_match_rule(LServer, From),
	  case Allow of
	    deny -> {result, Items};
	    allow ->
		PermLev = get_permission_level(From, LServer),
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
	  Allow = acl_match_rule(LServer, From),
	  Err = xmpp:err_forbidden(?T("Access denied by service policy"), Lang),
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
	    ?NS_ADMINL(<<"disable-user">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"reenable-user">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"end-user-session">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"change-user-password">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-user-roster">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-user-lastlogin">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"user-stats">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-registered-users-num">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-disabled-users-num">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-online-users-num">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-active-users-num">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-idle-users-num">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-registered-users-list">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-disabled-users-list">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-online-users-list">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-active-users">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"get-idle-users">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"restart">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    ?NS_ADMINL(<<"shutdown">>) ->
		?ITEMS_RESULT(Allow, LNode, {error, Err});
	    _ -> Acc
	  end
    end.

%%%-----------------------------------------------------------------------
-spec get_local_items({global | vhost, binary()}, [binary()],
		      binary(), binary()) -> {result, [disco_item()]} | {error, stanza_error()}.
get_local_items(_Host, [], Server, Lang) ->
    {result,
     [?NODE(?T("Configuration"), <<"config">>),
      ?NODE(?T("User Management"), <<"user">>),
      ?NODE(?T("Online Users"), <<"online users">>),
      ?NODE(?T("All Users"), <<"all users">>),
      ?NODE(?T("Outgoing s2s Connections"),
	    <<"outgoing s2s">>),
      ?NODE(?T("Running Nodes"), <<"running nodes">>),
      ?NODE(?T("Stopped Nodes"), <<"stopped nodes">>)]};
get_local_items(_Host, [<<"config">>, _], _Server,
		_Lang) ->
    {result, []};
get_local_items(_Host, [<<"user">>], Server, Lang) ->
    {result,
     [?NODE(?T("Add User"), (?NS_ADMINX(<<"add-user">>))),
      ?NODE(?T("Delete User"),
	    (?NS_ADMINX(<<"delete-user">>))),
      ?NODE(?T("Disable User"),
	    (?NS_ADMINX(<<"disable-user">>))),
      ?NODE(?T("Re-Enable User"),
	    (?NS_ADMINX(<<"reenable-user">>))),
      ?NODE(?T("End User Session"),
	    (?NS_ADMINX(<<"end-user-session">>))),
      ?NODE(?T("Change User Password"),
	    (?NS_ADMINX(<<"change-user-password">>))),
      ?NODE(?T("Get User Roster"),
	    (?NS_ADMINX(<<"get-user-roster">>))),
      ?NODE(?T("Get User Last Login Time"),
	    (?NS_ADMINX(<<"get-user-lastlogin">>))),
      ?NODE(?T("Get User Statistics"),
	    (?NS_ADMINX(<<"user-stats">>))),
      ?NODE(?T("Get Number of Registered Users"),
	    (?NS_ADMINX(<<"get-registered-users-num">>))),
      ?NODE(?T("Get Number of Disabled Users"),
	    (?NS_ADMINX(<<"get-disabled-users-num">>))),
      ?NODE(?T("Get Number of Online Users"),
	    (?NS_ADMINX(<<"get-online-users-num">>))),
      ?NODE(?T("Get Number of Active Users"),
	    (?NS_ADMINX(<<"get-active-users-num">>))),
      ?NODE(?T("Get Number of Idle Users"),
	    (?NS_ADMINX(<<"get-idle-users-num">>))),
      ?NODE(?T("Get List of Registered Users"),
	    (?NS_ADMINX(<<"get-registered-users-list">>))),
      ?NODE(?T("Get List of Disabled Users"),
	    (?NS_ADMINX(<<"get-disabled-users-list">>))),
      ?NODE(?T("Get List of Online Users"),
	    (?NS_ADMINX(<<"get-online-users-list">>))),
      ?NODE(?T("Get List of Active Users"),
	    (?NS_ADMINX(<<"get-active-users">>))),
      ?NODE(?T("Get List of Idle Users"),
	    (?NS_ADMINX(<<"get-idle-users">>)))
     ]};
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
    Users = ejabberd_auth:get_users(Host),
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
	    {error, xmpp:err_not_acceptable()}
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
     [?NODE(?T("Database"),
	    <<"running nodes/", ENode/binary, "/DB">>),
      ?NODE(?T("Backup Management"),
	    <<"running nodes/", ENode/binary, "/backup">>),
      ?NODE(?T("Import Users From jabberd14 Spool Files"),
	    <<"running nodes/", ENode/binary, "/import">>),
      ?NODE(?T("Restart Service"),
	    (?NS_ADMINX(<<"restart">>))),
      ?NODE(?T("Shut Down Service"),
	    (?NS_ADMINX(<<"shutdown">>))),
      ?NODE(?T("Restart Service"),
	    <<"running nodes/", ENode/binary, "/restart">>),
      ?NODE(?T("Shut Down Service"),
	    <<"running nodes/", ENode/binary, "/shutdown">>)]};
get_local_items(_Host,
		[<<"running nodes">>, _ENode, <<"DB">>], _Server,
		_Lang) ->
    {result, []};
get_local_items(_Host,
		[<<"running nodes">>, ENode, <<"backup">>], Server,
		Lang) ->
    {result,
     [?NODE(?T("Backup"),
	    <<"running nodes/", ENode/binary, "/backup/backup">>),
      ?NODE(?T("Restore"),
	    <<"running nodes/", ENode/binary, "/backup/restore">>),
      ?NODE(?T("Dump to Text File"),
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
     [?NODE(?T("Import File"),
	    <<"running nodes/", ENode/binary, "/import/file">>),
      ?NODE(?T("Import Directory"),
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

-spec get_online_vh_users(binary()) -> [disco_item()].
get_online_vh_users(Host) ->
    USRs = ejabberd_sm:get_vh_session_list(Host),
    SURs = lists:sort([{S, U, R} || {U, S, R} <- USRs]),
    lists:map(
      fun({S, U, R}) ->
	      #disco_item{jid = jid:make(U, S, R),
			  name = <<U/binary, "@", S/binary>>}
      end, SURs).

-spec get_all_vh_users(binary()) -> [disco_item()].
get_all_vh_users(Host) ->
    Users = ejabberd_auth:get_users(Host),
    SUsers = lists:sort([{S, U} || {U, S} <- Users]),
    case length(SUsers) of
	N when N =< 100 ->
	    lists:map(fun({S, U}) ->
			      #disco_item{jid = jid:make(U, S),
					  name = <<U/binary, $@, S/binary>>}
		      end, SUsers);
	N ->
	    NParts = trunc(math:sqrt(N * 6.17999999999999993783e-1)) + 1,
	    M = trunc(N / NParts) + 1,
	    lists:map(
	      fun (K) ->
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
	      end, lists:seq(1, N, M))
    end.

-spec get_outgoing_s2s(binary(), binary()) -> [disco_item()].
get_outgoing_s2s(Host, Lang) ->
    Connections = ejabberd_s2s:dirty_get_connections(),
    DotHost = <<".", Host/binary>>,
    TConns = [TH || {FH, TH} <- Connections,
		    Host == FH orelse str:suffix(DotHost, FH)],
    lists:map(
      fun (T) ->
	      Name = str:translate_and_format(Lang, ?T("To ~ts"),[T]),
	      #disco_item{jid = jid:make(Host),
			  node = <<"outgoing s2s/", T/binary>>,
			  name = Name}
      end, lists:usort(TConns)).

-spec get_outgoing_s2s(binary(), binary(), binary()) -> [disco_item()].
get_outgoing_s2s(Host, Lang, To) ->
    Connections = ejabberd_s2s:dirty_get_connections(),
    lists:map(
      fun ({F, _T}) ->
	      Node = <<"outgoing s2s/", To/binary, "/", F/binary>>,
	      Name = str:translate_and_format(Lang, ?T("From ~ts"), [F]),
	      #disco_item{jid = jid:make(Host), node = Node, name = Name}
      end,
      lists:keysort(
	1,
	lists:filter(fun (E) -> element(2, E) == To end,
		     Connections))).

-spec get_running_nodes(binary(), binary()) -> [disco_item()].
get_running_nodes(Server, _Lang) ->
    DBNodes = mnesia:system_info(running_db_nodes),
    lists:map(
      fun (N) ->
	      S = iolist_to_binary(atom_to_list(N)),
	      #disco_item{jid = jid:make(Server),
			  node = <<"running nodes/", S/binary>>,
			  name = S}
      end, lists:sort(DBNodes)).

-spec get_stopped_nodes(binary()) -> [disco_item()].
get_stopped_nodes(_Lang) ->
    DBNodes = lists:usort(mnesia:system_info(db_nodes) ++
			      mnesia:system_info(extra_db_nodes))
	-- mnesia:system_info(running_db_nodes),
    lists:map(
      fun (N) ->
	      S = iolist_to_binary(atom_to_list(N)),
	      #disco_item{jid = jid:make(ejabberd_config:get_myname()),
			  node = <<"stopped nodes/", S/binary>>,
			  name = S}
      end, lists:sort(DBNodes)).

%%-------------------------------------------------------------------------

-define(COMMANDS_RESULT(LServerOrGlobal, From, To,
			Request, Lang),
	case acl_match_rule(LServerOrGlobal, From) of
	  deny -> {error, xmpp:err_forbidden(?T("Access denied by service policy"), Lang)};
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

-spec adhoc_local_commands(jid(), jid(), adhoc_command()) -> adhoc_command() | {error, stanza_error()}.
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
	    case set_form(From, LServer, LNode, Lang, XData) of
		{result, Res} ->
		    xmpp_util:make_adhoc_response(
		      Request,
		      #adhoc_command{xdata = Res, status = completed});
		%%{'EXIT', _} -> {error, xmpp:err_bad_request()};
		{error, Error} -> {error, Error}
	    end;
       true ->
	  {error, xmpp:err_bad_request(?T("Unexpected action"), Lang)}
    end.

-define(TVFIELD(Type, Var, Val),
	#xdata_field{type = Type, var = Var, values = [Val]}).

-define(HFIELD(),
	?TVFIELD(hidden, <<"FORM_TYPE">>, (?NS_ADMIN))).

-define(TLFIELD(Type, Label, Var),
	#xdata_field{type = Type, label = tr(Lang, Label), var = Var}).

-define(XFIELD(Type, Label, Var, Val),
	#xdata_field{type = Type, label = tr(Lang, Label),
		     var = Var, values = [Val]}).

-define(XMFIELD(Type, Label, Var, Vals),
	#xdata_field{type = Type, label = tr(Lang, Label),
		     var = Var, values = Vals}).

-define(TABLEFIELD(Table, Val),
	#xdata_field{
	   type = 'list-single',
	   label = iolist_to_binary(atom_to_list(Table)),
	   var = iolist_to_binary(atom_to_list(Table)),
	   values = [iolist_to_binary(atom_to_list(Val))],
	   options = [#xdata_option{label = tr(Lang, ?T("RAM copy")),
				    value = <<"ram_copies">>},
		      #xdata_option{label = tr(Lang, ?T("RAM and disc copy")),
				    value = <<"disc_copies">>},
		      #xdata_option{label = tr(Lang, ?T("Disc only copy")),
				    value =  <<"disc_only_copies">>},
		      #xdata_option{label = tr(Lang, ?T("Remote copy")),
				    value = <<"unknown">>}]}).

-spec get_form(binary(), [binary()], binary()) -> {result, xdata()} |
						  {result, completed, xdata()} |
						  {error, stanza_error()}.
get_form(_Host, [<<"running nodes">>, ENode, <<"DB">>],
	 Lang) ->
    case search_running_node(ENode) of
      false ->
	  Txt = ?T("No running node found"),
	  {error, xmpp:err_item_not_found(Txt, Lang)};
      Node ->
	  case ejabberd_cluster:call(Node, mnesia, system_info, [tables]) of
	    {badrpc, Reason} ->
		?ERROR_MSG("RPC call mnesia:system_info(tables) on node "
			   "~ts failed: ~p", [Node, Reason]),
		{error, xmpp:err_internal_server_error()};
	    Tables ->
		STables = lists:sort(Tables),
		Title = <<(tr(Lang, ?T("Database Tables Configuration at ")))/binary,
			  ENode/binary>>,
		Instr = tr(Lang, ?T("Choose storage type of tables")),
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
				   "on node ~ts failed: ~p", [Node, Reason]),
			{error, xmpp:err_internal_server_error()}
		end
	  end
    end;
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"backup">>],
	 Lang) ->
    {result,
     #xdata{title = <<(tr(Lang, ?T("Backup to File at ")))/binary, ENode/binary>>,
	    type = form,
	    instructions = [tr(Lang, ?T("Enter path to backup file"))],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', ?T("Path to File"),
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"restore">>],
	 Lang) ->
    {result,
     #xdata{title = <<(tr(Lang, ?T("Restore Backup from File at ")))/binary,
		      ENode/binary>>,
	    type = form,
	    instructions = [tr(Lang, ?T("Enter path to backup file"))],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', ?T("Path to File"),
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"textfile">>],
	 Lang) ->
    {result,
     #xdata{title = <<(tr(Lang, ?T("Dump Backup to Text File at ")))/binary,
		      ENode/binary>>,
	    type = form,
	    instructions = [tr(Lang, ?T("Enter path to text file"))],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', ?T("Path to File"),
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"file">>],
	 Lang) ->
    {result,
     #xdata{title = <<(tr(Lang, ?T("Import User from File at ")))/binary,
		      ENode/binary>>,
	    type = form,
	    instructions = [tr(Lang, ?T("Enter path to jabberd14 spool file"))],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', ?T("Path to File"),
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"dir">>],
	 Lang) ->
    {result,
     #xdata{title = <<(tr(Lang, ?T("Import Users from Dir at ")))/binary,
		      ENode/binary>>,
	    type = form,
	    instructions = [tr(Lang, ?T("Enter path to jabberd14 spool dir"))],
	    fields = [?HFIELD(),
		      ?XFIELD('text-single', ?T("Path to Dir"),
			      <<"path">>, <<"">>)]}};
get_form(_Host,
	 [<<"running nodes">>, _ENode, <<"restart">>], Lang) ->
    Make_option =
	fun (LabelNum, LabelUnit, Value) ->
		#xdata_option{
		   label = <<LabelNum/binary, (tr(Lang, LabelUnit))/binary>>,
		   value = Value}
	end,
    {result,
     #xdata{title = tr(Lang, ?T("Restart Service")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{
			 type = 'list-single',
			 label = tr(Lang, ?T("Time delay")),
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
				   label = tr(Lang,
					      ?T("Send announcement to all online users "
						 "on all hosts"))},
		      #xdata_field{var = <<"subject">>,
				   type = 'text-single',
				   label = tr(Lang, ?T("Subject"))},
		      #xdata_field{var = <<"announcement">>,
				   type = 'text-multi',
				   label = tr(Lang, ?T("Message body"))}]}};
get_form(_Host,
	 [<<"running nodes">>, _ENode, <<"shutdown">>], Lang) ->
    Make_option =
	fun (LabelNum, LabelUnit, Value) ->
		#xdata_option{
		   label = <<LabelNum/binary, (tr(Lang, LabelUnit))/binary>>,
		   value = Value}
	end,
    {result,
     #xdata{title = tr(Lang, ?T("Shut Down Service")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{
			 type = 'list-single',
			 label = tr(Lang, ?T("Time delay")),
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
				   label = tr(Lang,
					      ?T("Send announcement to all online users "
						 "on all hosts"))},
		      #xdata_field{var = <<"subject">>,
				   type = 'text-single',
				   label = tr(Lang, ?T("Subject"))},
		      #xdata_field{var = <<"announcement">>,
				   type = 'text-multi',
				   label = tr(Lang, ?T("Message body"))}]}};
get_form(_Host, ?NS_ADMINL(<<"add-user">>), Lang) ->
    {result,
     #xdata{title = tr(Lang, ?T("Add User")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = tr(Lang, ?T("Jabber ID")),
				   required = true,
				   var = <<"accountjid">>},
		      #xdata_field{type = 'text-private',
				   label = tr(Lang, ?T("Password")),
				   required = true,
				   var = <<"password">>},
		      #xdata_field{type = 'text-private',
				   label = tr(Lang, ?T("Password Verification")),
				   required = true,
				   var = <<"password-verify">>}]}};
get_form(_Host, ?NS_ADMINL(<<"delete-user">>), Lang) ->
    {result,
     #xdata{title = tr(Lang, ?T("Delete User")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = tr(Lang, ?T("Jabber ID")),
				   required = true,
				   var = <<"accountjids">>}]}};
get_form(_Host, ?NS_ADMINL(<<"disable-user">>), Lang) ->
    {result,
     #xdata{title = tr(Lang, ?T("Disable User")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = tr(Lang, ?T("Jabber ID")),
				   required = true,
				   var = <<"accountjids">>}]}};
get_form(_Host, ?NS_ADMINL(<<"reenable-user">>), Lang) ->
    {result,
     #xdata{title = tr(Lang, ?T("Re-Enable User")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = tr(Lang, ?T("Jabber ID")),
				   required = true,
				   var = <<"accountjids">>}]}};
get_form(_Host, ?NS_ADMINL(<<"end-user-session">>),
	 Lang) ->
    {result,
     #xdata{title = tr(Lang, ?T("End User Session")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = tr(Lang, ?T("Jabber ID")),
				   required = true,
				   var = <<"accountjid">>}]}};
get_form(_Host, ?NS_ADMINL(<<"change-user-password">>),
	 Lang) ->
    {result,
     #xdata{title = tr(Lang, ?T("Change User Password")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = tr(Lang, ?T("Jabber ID")),
				   required = true,
				   var = <<"accountjid">>},
		      #xdata_field{type = 'text-private',
				   label = tr(Lang, ?T("Password")),
				   required = true,
				   var = <<"password">>}]}};
get_form(_Host, ?NS_ADMINL(<<"get-user-roster">>),
	 Lang) ->
    {result,
     #xdata{title = tr(Lang, ?T("Get User Roster")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = tr(Lang, ?T("Jabber ID")),
				   required = true,
				   var = <<"accountjids">>}]}};
get_form(_Host, ?NS_ADMINL(<<"get-user-lastlogin">>),
	 Lang) ->
    {result,
     #xdata{title = tr(Lang, ?T("Get User Last Login Time")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = tr(Lang, ?T("Jabber ID")),
				   var = <<"accountjid">>,
				   required = true}]}};
get_form(_Host, ?NS_ADMINL(<<"user-stats">>), Lang) ->
    {result,
     #xdata{title = tr(Lang, ?T("Get User Statistics")),
	    type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-single',
				   label = tr(Lang, ?T("Jabber ID")),
				   var = <<"accountjid">>,
				   required = true}]}};
get_form(Host,
	 ?NS_ADMINL(<<"get-registered-users-num">>), Lang) ->
    Num = integer_to_binary(ejabberd_auth:count_users(Host)),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'text-single',
				   label = tr(Lang, ?T("Number of registered users")),
				   var = <<"registeredusersnum">>,
				   values = [Num]}]}};
get_form(Host,
	 ?NS_ADMINL(<<"get-disabled-users-num">>), Lang) ->
    Num = integer_to_binary(mod_admin_extra:count_banned(Host)),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'text-single',
				   label = tr(Lang, ?T("Number of disabled users")),
				   var = <<"disabledusersnum">>,
				   values = [Num]}]}};
get_form(Host, ?NS_ADMINL(<<"get-online-users-num">>),
	 Lang) ->
    Num = integer_to_binary(ejabberd_sm:get_vh_session_number(Host)),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'text-single',
				   label = tr(Lang, ?T("Number of online users")),
				   var = <<"onlineusersnum">>,
				   values = [Num]}]}};
get_form(Host, ?NS_ADMINL(<<"get-active-users-num">>),
	 Lang) ->
    Num = integer_to_binary(mod_admin_extra:status_num(Host, [<<"available">>,
                                                              <<"chat">>,
                                                              <<"dnd">>])),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'text-single',
				   label = tr(Lang, ?T("Number of active users")),
				   var = <<"activeusersnum">>,
				   values = [Num]}]}};
get_form(Host, ?NS_ADMINL(<<"get-idle-users-num">>),
	 Lang) ->
    Num = integer_to_binary(mod_admin_extra:status_num(Host, [<<"away">>,
                                                              <<"xa">>])),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'text-single',
				   label = tr(Lang, ?T("Number of idle users")),
				   var = <<"idleusersnum">>,
				   values = [Num]}]}};
get_form(Host, ?NS_ADMINL(<<"get-registered-users-list">>), Lang) ->
    Values = [jid:encode(jid:make(U, Host))
              || {U, _} <- ejabberd_auth:get_users(Host)],
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = tr(Lang, ?T("The list of all users")),
				   var = <<"registereduserjids">>,
				   values = Values}]}};
get_form(Host, ?NS_ADMINL(<<"get-disabled-users-list">>), Lang) ->
    Values = mod_admin_extra:list_banned(Host),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = tr(Lang, ?T("The list of all disabled users")),
				   var = <<"disableduserjids">>,
				   values = Values}]}};
get_form(Host, ?NS_ADMINL(<<"get-online-users-list">>), Lang) ->
    Accounts = [jid:encode(jid:make(U, Host))
              || {U, _, _} <- ejabberd_sm:get_vh_session_list(Host)],
    Values = lists:usort(Accounts),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = tr(Lang, ?T("The list of all online users")),
				   var = <<"onlineuserjids">>,
				   values = Values}]}};
get_form(Host, ?NS_ADMINL(<<"get-active-users">>), Lang) ->
    RR = mod_admin_extra:status_list(Host, [<<"available">>, <<"chat">>, <<"dnd">>]),
    Accounts = [jid:encode(jid:make(U, S))
              || {U, S, _Resource, _Priority, _StatusText} <- RR],
    Values = lists:usort(Accounts),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = tr(Lang, ?T("The list of all active users")),
				   var = <<"activeuserjids">>,
				   values = Values}]}};
get_form(Host, ?NS_ADMINL(<<"get-idle-users">>), Lang) ->
    RR = mod_admin_extra:status_list(Host, [<<"away">>, <<"xa">>]),
    Accounts = [jid:encode(jid:make(U, S))
              || {U, S, _Resource, _Priority, _StatusText} <- RR],
    Values = lists:usort(Accounts),
    {result, completed,
     #xdata{type = form,
	    fields = [?HFIELD(),
		      #xdata_field{type = 'jid-multi',
				   label = tr(Lang, ?T("The list of all idle users")),
				   var = <<"idleuserjids">>,
				   values = Values}]}};
get_form(Host, ?NS_ADMINL(<<"restart">>), Lang) ->
    get_form(Host,
	 [<<"running nodes">>, misc:atom_to_binary(node()), <<"restart">>], Lang);
get_form(Host, ?NS_ADMINL(<<"shutdown">>), Lang) ->
    get_form(Host,
	 [<<"running nodes">>, misc:atom_to_binary(node()), <<"shutdown">>], Lang);
get_form(_Host, _, _Lang) ->
    {error, xmpp:err_service_unavailable()}.

-spec set_form(jid(), binary(), [binary()], binary(), xdata()) -> {result, xdata() | undefined} |
								  {error, stanza_error()}.
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"DB">>], Lang, XData) ->
    case search_running_node(ENode) of
      false ->
	  Txt = ?T("No running node found"),
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
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"backup">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = ?T("No running node found"),
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = ?T("No 'path' found in data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    case ejabberd_cluster:call(
			   Node, mnesia, backup, [binary_to_list(String)],
			   timer:minutes(10)) of
			{badrpc, Reason} ->
			    ?ERROR_MSG("RPC call mnesia:backup(~ts) to node ~ts "
				       "failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			{error, Reason} ->
			    ?ERROR_MSG("RPC call mnesia:backup(~ts) to node ~ts "
				       "failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			_ ->
			    {result, undefined}
		    end;
		_ ->
		    Txt = ?T("Incorrect value of 'path' in data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"restore">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = ?T("No running node found"),
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = ?T("No 'path' found in data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    case ejabberd_cluster:call(
			   Node, ejabberd_admin, restore,
			   [String], timer:minutes(10)) of
			{badrpc, Reason} ->
			    ?ERROR_MSG("RPC call ejabberd_admin:restore(~ts) to node "
				       "~ts failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			{error, Reason} ->
			    ?ERROR_MSG("RPC call ejabberd_admin:restore(~ts) to node "
				       "~ts failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			_ ->
			    {result, undefined}
		    end;
		_ ->
		    Txt = ?T("Incorrect value of 'path' in data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"backup">>,
	  <<"textfile">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = ?T("No running node found"),
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = ?T("No 'path' found in data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    case ejabberd_cluster:call(
			   Node, ejabberd_admin, dump_to_textfile,
			   [String], timer:minutes(10)) of
			{badrpc, Reason} ->
			    ?ERROR_MSG("RPC call ejabberd_admin:dump_to_textfile(~ts) "
				       "to node ~ts failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			{error, Reason} ->
			    ?ERROR_MSG("RPC call ejabberd_admin:dump_to_textfile(~ts) "
				       "to node ~ts failed: ~p", [String, Node, Reason]),
			    {error, xmpp:err_internal_server_error()};
			_ ->
			    {result, undefined}
		    end;
		_ ->
		    Txt = ?T("Incorrect value of 'path' in data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"file">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = ?T("No running node found"),
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = ?T("No 'path' found in data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    ejabberd_cluster:call(Node, jd2ejd, import_file, [String]),
		    {result, undefined};
		_ ->
		    Txt = ?T("Incorrect value of 'path' in data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
set_form(_From, _Host,
	 [<<"running nodes">>, ENode, <<"import">>, <<"dir">>],
	 Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    Txt = ?T("No running node found"),
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Node ->
	    case xmpp_util:get_xdata_values(<<"path">>, XData) of
		[] ->
		    Txt = ?T("No 'path' found in data form"),
		    {error, xmpp:err_bad_request(Txt, Lang)};
		[String] ->
		    ejabberd_cluster:call(Node, jd2ejd, import_dir, [String]),
		    {result, undefined};
		_ ->
		    Txt = ?T("Incorrect value of 'path' in data form"),
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
set_form(From, Host, ?NS_ADMINL(<<"add-user">>), _Lang,
	 XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    Password = get_value(<<"password">>, XData),
    Password = get_value(<<"password-verify">>, XData),
    AccountJID = jid:decode(AccountString),
    User = AccountJID#jid.luser,
    Server = AccountJID#jid.lserver,
    true = lists:member(Server, ejabberd_option:hosts()),
    true = Server == Host orelse
	     get_permission_level(From, Host) == global,
    case ejabberd_auth:try_register(User, Server, Password) of
	ok -> {result, undefined};
	{error, exists} -> {error, xmpp:err_conflict()};
	{error, not_allowed} -> {error, xmpp:err_not_allowed()}
    end;
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
				      get_permission_level(From, Host) == global,
			     true = ejabberd_auth:user_exists(User, Server),
			     {User, Server}
		     end,
		     AccountStringList),
    [ejabberd_auth:remove_user(User, Server)
     || {User, Server} <- ASL2],
    {result, undefined};
set_form(From, Host, ?NS_ADMINL(<<"disable-user">>),
	 _Lang, XData) ->
    AccountStringList = get_values(<<"accountjids">>,
				   XData),
    [_ | _] = AccountStringList,
    ASL2 = lists:map(fun (AccountString) ->
			     JID = jid:decode(AccountString),
			     User = JID#jid.luser,
			     Server = JID#jid.lserver,
			     true = Server == Host orelse
				      get_permission_level(From, Host) == global,
			     true = ejabberd_auth:user_exists(User, Server),
			     {User, Server}
		     end,
		     AccountStringList),
    [mod_admin_extra:ban_account_v2(User, Server, <<"">>)
     || {User, Server} <- ASL2],
    {result, undefined};
set_form(From, Host, ?NS_ADMINL(<<"reenable-user">>),
	 _Lang, XData) ->
    AccountStringList = get_values(<<"accountjids">>,
				   XData),
    [_ | _] = AccountStringList,
    ASL2 = lists:map(fun (AccountString) ->
			     JID = jid:decode(AccountString),
			     User = JID#jid.luser,
			     Server = JID#jid.lserver,
			     true = Server == Host orelse
				      get_permission_level(From, Host) == global,
			     true = ejabberd_auth:user_exists(User, Server),
			     {User, Server}
		     end,
		     AccountStringList),
    [mod_admin_extra:unban_account(User, Server)
     || {User, Server} <- ASL2],
    {result, undefined};
set_form(From, Host, ?NS_ADMINL(<<"end-user-session">>),
	 _Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:decode(AccountString),
    LServer = JID#jid.lserver,
    true = LServer == Host orelse
	     get_permission_level(From, Host) == global,
    case JID#jid.lresource of
	<<>> ->
	    ejabberd_sm:kick_user(JID#jid.luser, JID#jid.lserver);
	R ->
	    ejabberd_sm:kick_user(JID#jid.luser, JID#jid.lserver, R)
    end,
    {result, undefined};
set_form(From, Host,
	 ?NS_ADMINL(<<"change-user-password">>), _Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    Password = get_value(<<"password">>, XData),
    JID = jid:decode(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From, Host) == global,
    true = ejabberd_auth:user_exists(User, Server),
    ejabberd_auth:set_password(User, Server, Password),
    {result, undefined};
set_form(From, Host,
	 ?NS_ADMINL(<<"get-user-roster">>), Lang, XData) ->
    AccountStringList = get_values(<<"accountjids">>,
				   XData),
    [_ | _] = AccountStringList,
    ASL2 = lists:map(fun (AccountString) ->
			     JID = jid:decode(AccountString),
			     User = JID#jid.luser,
			     Server = JID#jid.lserver,
			     true = Server == Host orelse
				      get_permission_level(From, Host) == global,
			     true = ejabberd_auth:user_exists(User, Server),
			     {User, Server}
		     end,
		     AccountStringList),
    Contacts = [mod_admin_extra:get_roster(User, Server) || {User, Server} <- ASL2],
    Jids = [lists:join(<<"; ">>, [Jid, Name, Subscription, lists:join(<<", ">>, Groups)])
            || {Jid, Name, Subscription, _, Groups} <- lists:flatten(Contacts)],
    {result,
     #xdata{type = result,
	    fields = [?HFIELD(),
		      ?XMFIELD('jid-multi', ?T("Jabber ID"),
			      <<"accountjids">>, AccountStringList),
		      ?XMFIELD('text-multi', ?T("Contacts"),
			       <<"contacts">>, Jids)]}};
set_form(From, Host,
	 ?NS_ADMINL(<<"get-user-lastlogin">>), Lang, XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:decode(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From, Host) == global,
    FLast = case ejabberd_sm:get_user_resources(User,
						Server)
		of
	      [] ->
		  case get_last_info(User, Server) of
		    not_found -> tr(Lang, ?T("Never"));
		    {ok, Timestamp, _Status} ->
			Shift = Timestamp,
			TimeStamp = {Shift div 1000000, Shift rem 1000000, 0},
			{{Year, Month, Day}, {Hour, Minute, Second}} =
			    calendar:now_to_local_time(TimeStamp),
			(str:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
						       [Year, Month, Day, Hour,
							Minute, Second]))
		  end;
	      _ -> tr(Lang, ?T("Online"))
	    end,
    {result,
     #xdata{type = result,
	    fields = [?HFIELD(),
		      ?XFIELD('jid-single', ?T("Jabber ID"),
			      <<"accountjid">>, AccountString),
		      ?XFIELD('text-single', ?T("Last login"),
			      <<"lastlogin">>, FLast)]}};
set_form(From, Host, ?NS_ADMINL(<<"user-stats">>), Lang,
	 XData) ->
    AccountString = get_value(<<"accountjid">>, XData),
    JID = jid:decode(AccountString),
    User = JID#jid.luser,
    Server = JID#jid.lserver,
    true = Server == Host orelse
	     get_permission_level(From, Host) == global,
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
     #xdata{type = result,
	    fields = [?HFIELD(),
		      ?XFIELD('jid-single', ?T("Jabber ID"),
			      <<"accountjid">>, AccountString),
		      ?XFIELD('text-single', ?T("Roster size"),
			      <<"rostersize">>, Rostersize),
		      ?XMFIELD('text-multi', ?T("IP addresses"),
			       <<"ipaddresses">>, IPs),
		      ?XMFIELD('text-multi', ?T("Resources"),
			       <<"onlineresources">>, Resources)]}};
set_form(From, Host, ?NS_ADMINL(<<"restart">>), Lang,
	 XData) ->
    set_form(From, Host,
	 [<<"running nodes">>, misc:atom_to_binary(node()), <<"restart">>], Lang, XData);
set_form(From, Host, ?NS_ADMINL(<<"shutdown">>), Lang,
	 XData) ->
    set_form(From, Host,
	 [<<"running nodes">>, misc:atom_to_binary(node()), <<"shutdown">>], Lang, XData);
set_form(_From, _Host, _, _Lang, _XData) ->
    {error, xmpp:err_service_unavailable()}.

-spec get_value(binary(), xdata()) -> binary().
get_value(Field, XData) ->
    hd(get_values(Field, XData)).

-spec get_values(binary(), xdata()) -> [binary()].
get_values(Field, XData) ->
    xmpp_util:get_xdata_values(Field, XData).

-spec search_running_node(binary()) -> false | node().
search_running_node(SNode) ->
    search_running_node(SNode,
			mnesia:system_info(running_db_nodes)).

-spec search_running_node(binary(), [node()]) -> false | node().
search_running_node(_, []) -> false;
search_running_node(SNode, [Node | Nodes]) ->
    case atom_to_binary(Node, utf8) of
	SNode -> Node;
	_ -> search_running_node(SNode, Nodes)
    end.

-spec stop_node(jid(), binary(), binary(), restart | stop, xdata()) -> {result, undefined}.
stop_node(From, Host, ENode, Action, XData) ->
    Delay = binary_to_integer(get_value(<<"delay">>, XData)),
    Subject = case get_values(<<"subject">>, XData) of
		  [] ->
		      [];
		  [S|_] ->
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
    {ok, _} = timer:apply_after(Time, ejabberd_cluster, call, [Node, init, Action, []]),
    {result, undefined}.

-spec get_last_info(binary(), binary()) -> {ok, non_neg_integer(), binary()} | not_found.
get_last_info(User, Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
      true -> mod_last:get_last_info(User, Server);
      false -> not_found
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec adhoc_sm_commands(adhoc_command(), jid(), jid(), adhoc_command()) -> adhoc_command() |
									   {error, stanza_error()}.
adhoc_sm_commands(_Acc, From,
		  #jid{user = User, server = Server, lserver = LServer},
		  #adhoc_command{lang = Lang, node = <<"config">>,
				 action = Action, xdata = XData} = Request) ->
    case acl_match_rule(LServer, From) of
	deny ->
	    {error, xmpp:err_forbidden(?T("Access denied by service policy"), Lang)};
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
		    Txt = ?T("Unexpected action"),
		    {error, xmpp:err_bad_request(Txt, Lang)}
	    end
    end;
adhoc_sm_commands(Acc, _From, _To, _Request) -> Acc.

-spec get_sm_form(binary(), binary(), binary(), binary()) -> {result, xdata()} |
							     {error, stanza_error()}.
get_sm_form(User, Server, <<"config">>, Lang) ->
    {result,
     #xdata{type = form,
	    title = <<(tr(Lang, ?T("Administration of ")))/binary, User/binary>>,
	    fields =
		[?HFIELD(),
		 #xdata_field{
		    type = 'list-single',
		    label = tr(Lang, ?T("Action on user")),
		    var = <<"action">>,
		    values = [<<"edit">>],
		    options = [#xdata_option{
				  label = tr(Lang, ?T("Edit Properties")),
				  value = <<"edit">>},
			       #xdata_option{
				  label = tr(Lang, ?T("Remove User")),
				  value = <<"remove">>}]},
		 ?XFIELD('text-private', ?T("Password"),
			 <<"password">>,
			 ejabberd_auth:get_password_s(User, Server))]}};
get_sm_form(_User, _Server, _Node, _Lang) ->
    {error, xmpp:err_service_unavailable()}.

-spec set_sm_form(binary(), binary(), binary(), adhoc_command()) -> adhoc_command() |
								    {error, stanza_error()}.
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
		    Txt = ?T("No 'password' found in data form"),
		    {error, xmpp:err_not_acceptable(Txt, Lang)}
	    end;
	[<<"remove">>] ->
	    ejabberd_auth:remove_user(User, Server),
	    xmpp_util:make_adhoc_response(Response);
	_ ->
	    Txt = ?T("Incorrect value of 'action' in data form"),
	    {error, xmpp:err_not_acceptable(Txt, Lang)}
    end;
set_sm_form(_User, _Server, _Node, _Request) ->
    {error, xmpp:err_service_unavailable()}.

-spec tr(binary(), binary()) -> binary().
tr(Lang, Text) ->
    translate:translate(Lang, Text).

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(access) ->
    econf:acl().

-spec mod_options(binary()) -> [{services, [tuple()]} | {atom(), any()}].
mod_options(_Host) ->
    [{access, configure}].

%% All ad-hoc commands implemented by mod_configure are available as API Commands:
%% - add-user                  -> register
%% - delete-user               -> unregister
%% - disable-user              -> ban_account
%% - reenable-user             -> unban_account
%% - end-user-session          -> kick_session / kick_user
%% - change-user-password      -> change_password
%% - get-user-lastlogin        -> get_last
%% - get-user-roster           -> get_roster
%% - get-user-lastlogin        -> get_last
%% - user-stats                -> user_sessions_info
%% - edit-blacklist            -> not ad-hoc or API command available !!!
%% - edit-whitelist            -> not ad-hoc or API command available !!!
%% - get-registered-users-num  -> stats
%% - get-disabled-users-num    -> count_banned
%% - get-online-users-num      -> stats
%% - get-active-users-num      -> status_num
%% - get-idle-users-num        -> status_num
%% - get-registered-users-list -> registered_users
%% - get-disabled-users-list   -> list_banned
%% - get-online-users-list     -> connected_users
%% - get-active-users          -> status_list
%% - get-idle-users            -> status_list
%% - stopped nodes             -> list_cluster_detailed
%% - DB                        -> mnesia_list_tables and mnesia_table_change_storage
%% - edit-admin                -> not ad-hoc or API command available !!!
%% - restart                   -> restart_kindly
%% - shutdown                  -> stop_kindly
%% - backup                    -> backup
%% - restore                   -> restore
%% - textfile                  -> dump
%% - import/file               -> import_file
%% - import/dir                -> import_dir

%%
%% An exclusive feature available only in this module is to list items and discover them:
%% - outgoing s2s
%% - online users
%% - all users

mod_doc() ->
    #{desc =>
          [?T("The module provides server configuration functionalities using "
              "https://xmpp.org/extensions/xep-0030.html[XEP-0030: Service Discovery] and "
              "https://xmpp.org/extensions/xep-0050.html[XEP-0050: Ad-Hoc Commands]:"),
           "",
           "- List and discover outgoing s2s, online client sessions and all registered accounts",
           "- Most of the ad-hoc commands defined in "
           "  https://xmpp.org/extensions/xep-0133.html[XEP-0133: Service Administration]",
           "- Additional custom ad-hoc commands specific to ejabberd",
           "",
           ?T("Ad-hoc commands from XEP-0133 that behave differently to the XEP:"),
           "",
           " - `get-user-roster`: returns standard fields instead of roster items that client cannot display",
           "",
           ?T("Those ad-hoc commands from XEP-0133 do not include in the response "
              "the client that executed the command:"),
           "",
           " - `get-active-users-num`",
           " - `get-idle-users-num`",
           " - `get-active-users`",
           " - `get-idle-users`",
           "",
           ?T("Those ad-hoc commands from XEP-0133 are not implemented:"),
           "",
           " - `edit-blacklist`",
           " - `edit-whitelist`",
           " - `edit-admin`",
           "",
           ?T("This module requires _`mod_adhoc`_ (to execute the commands), "
              "and recommends _`mod_disco`_ (to discover the commands). "),
           "",
           ?T("Please notice that all the ad-hoc commands implemented by this module "
              "have an equivalent "
              "https://docs.ejabberd.im/developer/ejabberd-api/[API Command] "
              "that you can execute using _`mod_adhoc_api`_ or any other API frontend.")],
      note => "improved in 25.10",
      opts =>
          [{access,
            #{value => ?T("AccessName"),
              note => "added in 25.03",
              desc =>
                  ?T("This option defines which access rule will be used to "
                     "control who is allowed to access the features provided by this module. "
                     "The default value is 'configure'.")}}],
      example =>
          ["acl:",
           "  admin:",
           "    user: sun@localhost",
           "",
           "access_rules:",
           "  configure:",
           "    allow: admin",
           "",
           "modules:",
           "  mod_configure:",
           "    access: configure"]}.
