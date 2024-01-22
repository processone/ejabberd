%%%-------------------------------------------------------------------
%%% File    : mod_privilege.erl
%%% Author  : Anna Mukharram <amuhar3@gmail.com>
%%% Purpose : XEP-0356: Privileged Entity
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

-protocol({xep, 356, '0.2.1', '16.09', "", ""}).

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2]).
-export([mod_doc/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([component_connected/1, component_disconnected/2,
	 roster_access/2, process_message/1,
	 process_presence_out/1, process_presence_in/1]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-type roster_permission() :: both | get | set.
-type presence_permission() :: managed_entity | roster.
-type message_permission() :: outgoing.
-type roster_permissions() :: [{roster_permission(), acl:acl()}].
-type presence_permissions() :: [{presence_permission(), acl:acl()}].
-type message_permissions() :: [{message_permission(), acl:acl()}].
-type access() :: [{roster, roster_permissions()} |
		   {presence, presence_permissions()} |
		   {message, message_permissions()}].
-type permissions() :: #{binary() => access()}.
-record(state, {server_host = <<"">> :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

mod_opt_type(roster) ->
    econf:options(
      #{both => econf:acl(), get => econf:acl(), set => econf:acl()});
mod_opt_type(message) ->
    econf:options(
      #{outgoing => econf:acl()});
mod_opt_type(presence) ->
    econf:options(
      #{managed_entity => econf:acl(), roster => econf:acl()}).

mod_options(_) ->
    [{roster, [{both, none}, {get, none}, {set, none}]},
     {presence, [{managed_entity, none}, {roster, none}]},
     {message, [{outgoing,none}]}].

mod_doc() ->
    #{desc =>
          [?T("This module is an implementation of "
              "https://xmpp.org/extensions/xep-0356.html"
              "[XEP-0356: Privileged Entity]. This extension "
              "allows components to have privileged access to "
              "other entity data (send messages on behalf of the "
              "server or on behalf of a user, get/set user roster, "
              "access presence information, etc.). This may be used "
              "to write powerful external components, for example "
              "implementing an external "
              "https://xmpp.org/extensions/xep-0163.html[PEP] or "
              "https://xmpp.org/extensions/xep-0313.html[MAM] service."), "",
           ?T("By default a component does not have any privileged access. "
              "It is worth noting that the permissions grant access to "
              "the component to a specific data type for all users of "
              "the virtual host on which 'mod_privilege' is loaded."), "",
	   ?T("Make sure you have a listener configured to connect your "
	      "component. Check the section about listening ports for more "
	      "information."), "",
	   ?T("WARNING: Security issue: Privileged access gives components "
	      "access to sensitive data, so permission should be granted "
	      "carefully, only if you trust a component."), "",
           ?T("NOTE: This module is complementary to _`mod_delegation`_, "
              "but can also be used separately.")],
      opts =>
          [{roster,
            #{value => ?T("Options"),
              desc =>
                  ?T("This option defines roster permissions. "
                     "By default no permissions are given. "
                     "The 'Options' are:")},
            [{both,
              #{value => ?T("AccessName"),
                desc =>
                    ?T("Sets read/write access to a user's roster. "
                       "The default value is 'none'.")}},
             {get,
              #{value => ?T("AccessName"),
                desc =>
                    ?T("Sets read access to a user's roster. "
                       "The default value is 'none'.")}},
             {set,
              #{value => ?T("AccessName"),
                desc =>
                    ?T("Sets write access to a user's roster. "
                       "The default value is 'none'.")}}]},
           {message,
            #{value => ?T("Options"),
              desc =>
                  ?T("This option defines permissions for messages. "
                     "By default no permissions are given. "
                     "The 'Options' are:")},
            [{outgoing,
              #{value => ?T("AccessName"),
                desc =>
                    ?T("The option defines an access rule for sending "
                       "outgoing messages by the component. "
                       "The default value is 'none'.")}}]},
           {presence,
            #{value => ?T("Options"),
              desc =>
                  ?T("This option defines permissions for presences. "
                     "By default no permissions are given. "
                     "The 'Options' are:")},
            [{managed_entity,
              #{value => ?T("AccessName"),
                desc =>
                    ?T("An access rule that gives permissions to "
                       "the component to receive server presences. "
                       "The default value is 'none'.")}},
             {roster,
              #{value => ?T("AccessName"),
                desc =>
                    ?T("An access rule that gives permissions to "
                       "the component to receive the presence of both "
                       "the users and the contacts in their roster. "
                       "The default value is 'none'.")}}]}],
      example =>
          ["modules:",
           "  ...",
           "  mod_privilege:",
           "    roster:",
           "      get: all",
           "    presence:",
           "      managed_entity: all",
           "    message:",
           "      outgoing: all",
           "  ..."]}.

depends(_, _) ->
    [].

-spec component_connected(binary()) -> ok.
component_connected(Host) ->
    lists:foreach(
      fun(ServerHost) ->
	      Proc = gen_mod:get_module_proc(ServerHost, ?MODULE),
	      gen_server:cast(Proc, {component_connected, Host})
      end, ejabberd_option:hosts()).

-spec component_disconnected(binary(), binary()) -> ok.
component_disconnected(Host, _Reason) ->
    lists:foreach(
      fun(ServerHost) ->
	      Proc = gen_mod:get_module_proc(ServerHost, ?MODULE),
	      gen_server:cast(Proc, {component_disconnected, Host})
      end, ejabberd_option:hosts()).

-spec process_message(stanza()) -> stop | ok.
process_message(#message{from = #jid{luser = <<"">>, lresource = <<"">>} = From,
			 to = #jid{lresource = <<"">>} = To,
			 lang = Lang, type = T} = Msg) when T /= error ->
    Host = From#jid.lserver,
    ServerHost = To#jid.lserver,
    Permissions = get_permissions(ServerHost),
    case maps:find(Host, Permissions) of
	{ok, Access} ->
	    case proplists:get_value(message, Access, none) of
		outgoing ->
		    forward_message(Msg);
		_ ->
		    Txt = ?T("Insufficient privilege"),
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

-spec roster_access({true, iq()} | false, iq()) -> {true, iq()} | false.
roster_access({true, _IQ} = Acc, _) ->
    Acc;
roster_access(false, #iq{from = From, to = To, type = Type} = IQ) ->
    Host = From#jid.lserver,
    ServerHost = To#jid.lserver,
    Permissions = get_permissions(ServerHost),
    case maps:find(Host, Permissions) of
	{ok, Access} ->
	    Permission = proplists:get_value(roster, Access, none),
	    case (Permission == both)
		     orelse (Permission == get andalso Type == get)
		     orelse (Permission == set andalso Type == set) of
		true ->
		    {true, xmpp:put_meta(IQ, privilege_from, To)};
		false ->
		    false
	    end;
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
      end, maps:to_list(Permissions)),
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
		 _ ->
		      ok
	      end
      end, maps:to_list(Permissions)),
    {Pres, C2SState};
process_presence_in(Acc) ->
    Acc.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Host|_]) ->
    process_flag(trap_exit, true),
    catch ets:new(?MODULE,
                  [named_table, public,
                   {heir, erlang:group_leader(), none}]),
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

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast({component_connected, Host}, State) ->
    ServerHost = State#state.server_host,
    From = jid:make(ServerHost),
    To = jid:make(Host),
    RosterPerm = get_roster_permission(ServerHost, Host),
    PresencePerm = get_presence_permission(ServerHost, Host),
    MessagePerm = get_message_permission(ServerHost, Host),
    if RosterPerm /= none; PresencePerm /= none; MessagePerm /= none ->
	    Priv = #privilege{perms = [#privilege_perm{access = message,
						       type = MessagePerm},
				       #privilege_perm{access = roster,
						       type = RosterPerm},
				       #privilege_perm{access = presence,
						       type = PresencePerm}]},
	    ?INFO_MSG("Granting permissions to external "
		      "component '~ts': roster = ~ts, presence = ~ts, "
		      "message = ~ts",
		      [Host, RosterPerm, PresencePerm, MessagePerm]),
	    Msg = #message{from = From, to = To,  sub_els = [Priv]},
	    ejabberd_router:route(Msg),
	    Permissions = maps:put(Host, [{roster, RosterPerm},
					  {presence, PresencePerm},
					  {message, MessagePerm}],
				   get_permissions(ServerHost)),
	    ets:insert(?MODULE, {ServerHost, Permissions}),
	    {noreply, State};
       true ->
	    ?INFO_MSG("Granting no permissions to external component '~ts'",
		      [Host]),
	    {noreply, State}
    end;
handle_cast({component_disconnected, Host}, State) ->
    ServerHost = State#state.server_host,
    Permissions = maps:remove(Host, get_permissions(ServerHost)),
    case maps:size(Permissions) of
	0 -> ets:delete(?MODULE, ServerHost);
	_ -> ets:insert(?MODULE, {ServerHost, Permissions})
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.server_host,
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	false ->
	    ejabberd_hooks:delete(component_connected, ?MODULE,
				  component_connected, 50),
	    ejabberd_hooks:delete(component_disconnected, ?MODULE,
				  component_disconnected, 50);
	true ->
	    ok
    end,
    ejabberd_hooks:delete(local_send_to_resource_hook, Host, ?MODULE,
			  process_message, 50),
    ejabberd_hooks:delete(roster_remote_access, Host, ?MODULE,
			  roster_access, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  process_presence_out, 50),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  process_presence_in, 50),
    ets:delete(?MODULE, Host).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_permissions(binary()) -> permissions().
get_permissions(ServerHost) ->
    try ets:lookup_element(?MODULE, ServerHost, 2)
    catch _:badarg -> #{}
    end.

-spec forward_message(message()) -> ok.
forward_message(#message{to = To} = Msg) ->
    ServerHost = To#jid.lserver,
    Lang = xmpp:get_lang(Msg),
    CodecOpts = ejabberd_config:codec_options(),
    try xmpp:try_subtag(Msg, #privilege{}) of
	#privilege{forwarded = #forwarded{sub_els = [SubEl]}} ->
	    try xmpp:decode(SubEl, ?NS_CLIENT, CodecOpts) of
		#message{} = NewMsg ->
		    case NewMsg#message.from of
			#jid{lresource = <<"">>, lserver = ServerHost} ->
                            FromJID = NewMsg#message.from,
                            State = #{jid => FromJID},
                            ejabberd_hooks:run_fold(user_send_packet, FromJID#jid.lserver, {NewMsg, State}, []),
			    ejabberd_router:route(NewMsg);
			_ ->
			    Lang = xmpp:get_lang(Msg),
			    Txt = ?T("Invalid 'from' attribute in forwarded message"),
			    Err = xmpp:err_forbidden(Txt, Lang),
			    ejabberd_router:route_error(Msg, Err)
		    end;
		_ ->
		    Txt = ?T("Message not found in forwarded payload"),
		    Err = xmpp:err_bad_request(Txt, Lang),
		    ejabberd_router:route_error(Msg, Err)
	    catch _:{xmpp_codec, Why} ->
		    Txt = xmpp:io_format_error(Why),
		    Err = xmpp:err_bad_request(Txt, Lang),
		    ejabberd_router:route_error(Msg, Err)
	    end;
	_ ->
	    Txt = ?T("No <forwarded/> element found"),
	    Err = xmpp:err_bad_request(Txt, Lang),
	    ejabberd_router:route_error(Msg, Err)
    catch _:{xmpp_codec, Why} ->
	    Txt = xmpp:io_format_error(Why),
	    Err = xmpp:err_bad_request(Txt, Lang),
	    ejabberd_router:route_error(Msg, Err)
    end.

-spec get_roster_permission(binary(), binary()) -> roster_permission() | none.
get_roster_permission(ServerHost, Host) ->
    Perms = mod_privilege_opt:roster(ServerHost),
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

-spec get_message_permission(binary(), binary()) -> message_permission() | none.
get_message_permission(ServerHost, Host) ->
    Perms = mod_privilege_opt:message(ServerHost),
    case match_rule(ServerHost, Host, Perms, outgoing) of
	allow -> outgoing;
	deny -> none
    end.

-spec get_presence_permission(binary(), binary()) -> presence_permission() | none.
get_presence_permission(ServerHost, Host) ->
    Perms = mod_privilege_opt:presence(ServerHost),
    case match_rule(ServerHost, Host, Perms, roster) of
	allow ->
	    roster;
	deny ->
	    case match_rule(ServerHost, Host, Perms, managed_entity) of
		allow -> managed_entity;
		deny -> none
	    end
    end.

-spec match_rule(binary(), binary(), roster_permissions(), roster_permission()) -> allow | deny;
		(binary(), binary(), presence_permissions(), presence_permission()) -> allow | deny;
		(binary(), binary(), message_permissions(), message_permission()) -> allow | deny.
match_rule(ServerHost, Host, Perms, Type) ->
    Access = proplists:get_value(Type, Perms, none),
    acl:match_rule(ServerHost, Access, jid:make(Host)).
