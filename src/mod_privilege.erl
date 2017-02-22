%%%-------------------------------------------------------------------
%%% File    : mod_privilege.erl
%%% Author  : Anna Mukharram <amuhar3@gmail.com>
%%% Purpose : XEP-0356: Privileged Entity
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
%%%-------------------------------------------------------------------
-module(mod_privilege).

-author('amuhar3@gmail.com').

-protocol({xep, 0356, '0.2.1'}).

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3, mod_opt_type/1, depends/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([component_connected/1, component_disconnected/2,
	 roster_access/2, process_message/1,
	 process_presence_out/1, process_presence_in/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-record(state, {server_host = <<"">> :: binary(),
		permissions = dict:new() :: ?TDICT}).

%%%===================================================================
%%% API
%%%===================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

mod_opt_type(roster) -> v_roster();
mod_opt_type(message) -> v_message();
mod_opt_type(presence) -> v_presence();
mod_opt_type(_) ->
    [roster, message, presence].

depends(_, _) ->
    [].

-spec component_connected(binary()) -> ok.
component_connected(Host) ->
    lists:foreach(
      fun(ServerHost) ->
	      Proc = gen_mod:get_module_proc(ServerHost, ?MODULE),
	      gen_server:cast(Proc, {component_connected, Host})
      end, ?MYHOSTS).

-spec component_disconnected(binary(), binary()) -> ok.
component_disconnected(Host, _Reason) ->
    lists:foreach(
      fun(ServerHost) ->
	      Proc = gen_mod:get_module_proc(ServerHost, ?MODULE),
	      gen_server:cast(Proc, {component_disconnected, Host})
      end, ?MYHOSTS).

-spec process_message(stanza()) -> stop | ok.
process_message(#message{from = #jid{luser = <<"">>, lresource = <<"">>} = From,
			 to = #jid{lresource = <<"">>} = To,
			 lang = Lang, type = T} = Msg) when T /= error ->
    Host = From#jid.lserver,
    ServerHost = To#jid.lserver,
    Permissions = get_permissions(ServerHost),
    case dict:find(Host, Permissions) of
	{ok, Access} ->
	    case proplists:get_value(message, Access, none) of
		outgoing ->
		    forward_message(Msg);
		none ->
		    Txt = <<"Insufficient privilege">>,
		    Err = xmpp:err_forbidden(Txt, Lang),
		    ejabberd_router:route_error(Msg, Err)
	    end,
	    stop;
	error ->
	    %% Component is disconnected
	    ok
    end;
process_message(_Stanza) ->
    ok.

-spec roster_access(boolean(), iq()) -> boolean().
roster_access(true, _) ->
    true;
roster_access(false, #iq{from = From, to = To, type = Type}) ->
    Host = From#jid.lserver,
    ServerHost = To#jid.lserver,
    Permissions = get_permissions(ServerHost),
    case dict:find(Host, Permissions) of
	{ok, Access} ->
	    Permission = proplists:get_value(roster, Access, none),
	    (Permission == both)
		orelse (Permission == get andalso Type == get)
		orelse (Permission == set andalso Type == set);
	error ->
	    %% Component is disconnected
	    false
    end.

-spec process_presence_out({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
process_presence_out({#presence{
			 from = #jid{luser = LUser, lserver = LServer} = From,
			 to = #jid{luser = LUser, lserver = LServer, lresource = <<"">>},
			 type = Type} = Pres, C2SState})
  when Type == available; Type == unavailable ->
    %% Self-presence processing
    Permissions = get_permissions(LServer),
    lists:foreach(
      fun({Host, Access}) ->
	      Permission = proplists:get_value(presence, Access, none),
	      if Permission == roster; Permission == managed_entity ->
		      To = jid:make(Host),
		      ejabberd_router:route(
			xmpp:set_from_to(Pres, From, To));
		 true ->
		      ok
	      end
      end, dict:to_list(Permissions)),
    {Pres, C2SState};
process_presence_out(Acc) ->
    Acc.

-spec process_presence_in({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
process_presence_in({#presence{
			from = #jid{luser = U, lserver = S} = From,
			to = #jid{luser = LUser, lserver = LServer},
			type = Type} = Pres, C2SState})
  when {U, S} /= {LUser, LServer} andalso
       (Type == available orelse Type == unavailable) ->
    Permissions = get_permissions(LServer),
    lists:foreach(
      fun({Host, Access}) ->
	      case proplists:get_value(presence, Access, none) of
		  roster ->
		      Permission = proplists:get_value(roster, Access, none),
		      if Permission == both; Permission == get ->
			      To = jid:make(Host),
			      ejabberd_router:route(
				xmpp:set_from_to(Pres, From, To));
			 true ->
			      ok
		      end;
		 true ->
		      ok
	      end
      end, dict:to_list(Permissions)),
    {Pres, C2SState};
process_presence_in(Acc) ->
    Acc.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Host, _Opts]) ->
    process_flag(trap_exit, true),
    ejabberd_hooks:add(component_connected, ?MODULE,
                       component_connected, 50),
    ejabberd_hooks:add(component_disconnected, ?MODULE,
                       component_disconnected, 50),
    ejabberd_hooks:add(local_send_to_resource_hook, Host, ?MODULE,
		       process_message, 50),
    ejabberd_hooks:add(roster_remote_access, Host, ?MODULE,
		       roster_access, 50),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       process_presence_out, 50),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       process_presence_in, 50),
    {ok, #state{server_host = Host}}.

handle_call(get_permissions, _From, State) ->
    {reply, {ok, State#state.permissions}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({component_connected, Host}, State) ->
    ServerHost = State#state.server_host,
    From = jid:make(ServerHost),
    To = jid:make(Host),
    RosterPerm = get_roster_permission(ServerHost, Host),
    PresencePerm = get_presence_permission(ServerHost, Host),
    MessagePerm = get_message_permission(ServerHost, Host),
    if RosterPerm /= none, PresencePerm /= none, MessagePerm /= none ->
	    Priv = #privilege{perms = [#privilege_perm{access = message,
						       type = MessagePerm},
				       #privilege_perm{access = roster,
						       type = RosterPerm},
				       #privilege_perm{access = presence,
						       type = PresencePerm}]},
	    ?INFO_MSG("Granting permissions to external "
		      "component '~s': roster = ~s, presence = ~s, "
		      "message = ~s",
		      [Host, RosterPerm, PresencePerm, MessagePerm]),
	    Msg = #message{from = From, to = To,  sub_els = [Priv]},
	    ejabberd_router:route(Msg),
	    Permissions = dict:store(Host, [{roster, RosterPerm},
					    {presence, PresencePerm},
					    {message, MessagePerm}],
				     State#state.permissions),
	    {noreply, State#state{permissions = Permissions}};
       true ->
	    ?INFO_MSG("Granting no permissions to external component '~s'",
		      [Host]),
	    {noreply, State}
    end;
handle_cast({component_disconnected, Host}, State) ->
    Permissions = dict:erase(Host, State#state.permissions),
    {noreply, State#state{permissions = Permissions}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Note: we don't remove component_* hooks because they are global
    %% and might be registered within a module on another virtual host
    Host = State#state.server_host,
    ejabberd_hooks:delete(local_send_to_resource_hook, Host, ?MODULE,
			  process_message, 50),
    ejabberd_hooks:delete(roster_remote_access, Host, ?MODULE,
			  roster_access, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  process_presence_out, 50),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  process_presence_in, 50).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_permissions(ServerHost) ->
    Proc = gen_mod:get_module_proc(ServerHost, ?MODULE),
    try gen_server:call(Proc, get_permissions) of
	{ok, Permissions} ->
	    Permissions
    catch exit:{noproc, _} ->
	    %% No module is loaded for this virtual host
	    dict:new()
    end.

forward_message(#message{to = To} = Msg) ->
    ServerHost = To#jid.lserver,
    Lang = xmpp:get_lang(Msg),
    case xmpp:get_subtag(Msg, #privilege{}) of
	#privilege{forwarded = #forwarded{xml_els = [SubEl]}} ->
	    try xmpp:decode(SubEl, ?NS_CLIENT, [ignore_els]) of
		#message{} = NewMsg ->
		    case NewMsg#message.from of
			#jid{lresource = <<"">>, lserver = ServerHost} ->
			    ejabberd_router:route(NewMsg);
			_ ->
			    Lang = xmpp:get_lang(Msg),
			    Txt = <<"Invalid 'from' attribute in forwarded message">>,
			    Err = xmpp:err_forbidden(Txt, Lang),
			    ejabberd_router:route_error(Msg, Err)
		    end;
		_ ->
		    Txt = <<"Message not found in forwarded payload">>,
		    Err = xmpp:err_bad_request(Txt, Lang),
		    ejabberd_router:route_error(Msg, Err)
	    catch _:{xmpp_codec, Why} ->
		    Txt = xmpp:format_error(Why),
		    Err = xmpp:err_bad_request(Txt, Lang),
		    ejabberd_router:route_error(Msg, Err)
	    end;
	_ ->
	    Txt = <<"Invalid <forwarded/> element">>,
	    Err = xmpp:err_bad_request(Txt, Lang),
	    ejabberd_router:route_error(Msg, Err)
    end.

get_roster_permission(ServerHost, Host) ->
    Perms = gen_mod:get_module_opt(ServerHost, ?MODULE, roster,
				   v_roster(), []),
    case match_rule(ServerHost, Host, Perms, both) of
	allow ->
	    both;
	deny ->
	    Get = match_rule(ServerHost, Host, Perms, get),
	    Set = match_rule(ServerHost, Host, Perms, set),
	    if Get == allow, Set == allow -> both;
	       Get == allow -> get;
	       Set == allow -> set;
	       true -> none
	    end
    end.

get_message_permission(ServerHost, Host) ->
    Perms = gen_mod:get_module_opt(ServerHost, ?MODULE, message,
				   v_message(), []),
    case match_rule(ServerHost, Host, Perms, outgoing) of
	allow -> outgoing;
	deny -> none
    end.

get_presence_permission(ServerHost, Host) ->
    Perms = gen_mod:get_module_opt(ServerHost, ?MODULE, presence,
				   v_presence(), []),
    case match_rule(ServerHost, Host, Perms, roster) of
	allow ->
	    roster;
	deny ->
	    case match_rule(ServerHost, Host, Perms, managed_entity) of
		allow -> managed_entity;
		deny -> none
	    end
    end.

match_rule(ServerHost, Host, Perms, Type) ->
    Access = proplists:get_value(Type, Perms, none),
    acl:match_rule(ServerHost, Access, jid:make(Host)).

v_roster() ->
    fun(Props) ->
	    lists:map(
	      fun({both, ACL}) -> {both, acl:access_rules_validator(ACL)};
		 ({get, ACL}) -> {get, acl:access_rules_validator(ACL)};
		 ({set, ACL}) -> {set, acl:access_rules_validator(ACL)}
	      end, Props)
    end.

v_message() ->
    fun(Props) ->
	    lists:map(
	      fun({outgoing, ACL}) -> {outgoing, acl:access_rules_validator(ACL)}
	      end, Props)
    end.

v_presence() ->
    fun(Props) ->
	    lists:map(
	      fun({managed_entity, ACL}) ->
		      {managed_entity, acl:access_rules_validator(ACL)};
		 ({roster, ACL}) ->
		      {roster, acl:access_rules_validator(ACL)}
	      end, Props)
    end.
