%%%----------------------------------------------------------------------
%%% File    : mod_push_keepalive.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Keep pending XEP-0198 sessions alive with XEP-0357
%%% Created : 15 Jul 2017 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2017-2025 ProcessOne
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

-module(mod_push_keepalive).
-author('holger@zedat.fu-berlin.de').

-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2]).
-export([mod_doc/0]).
%% ejabberd_hooks callbacks.
-export([ejabberd_started/0, c2s_session_pending/1, c2s_session_resumed/1,
	 c2s_copy_session/2, c2s_handle_cast/2, c2s_handle_info/2,
	 c2s_stanza/3]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-define(PUSH_BEFORE_TIMEOUT_PERIOD, 120000). % 2 minutes.

-type c2s_state() :: ejabberd_c2s:state().

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, _Opts) ->
    register_hooks(Host).

-spec stop(binary()) -> ok.
stop(Host) ->
    unregister_hooks(Host).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [{mod_push, hard},
     {mod_client_state, soft},
     {mod_stream_mgmt, soft}].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(resume_timeout) ->
    econf:either(
      econf:int(0, 0),
      econf:timeout(second));
mod_opt_type(wake_on_start) ->
    econf:bool();
mod_opt_type(wake_on_timeout) ->
    econf:bool().

mod_options(_Host) ->
    [{resume_timeout, timer:hours(72)},
     {wake_on_start, false},
     {wake_on_timeout, true}].

mod_doc() ->
    #{desc =>
          [?T("This module tries to keep the stream management "
              "session (see _`mod_stream_mgmt`_) of a disconnected "
              "mobile client alive if the client enabled push "
              "notifications for that session. However, the normal "
              "session resumption timeout is restored once a push "
              "notification is issued, so the session will be closed "
              "if the client doesn't respond to push notifications."), "",
           ?T("The module depends on _`mod_push`_.")],
      opts =>
          [{resume_timeout,
            #{value => "timeout()",
              desc =>
                  ?T("This option specifies the period of time until "
                     "the session of a disconnected push client times out. "
                     "This timeout is only in effect as long as no push "
                     "notification is issued. Once that happened, the "
                     "resumption timeout configured for _`mod_stream_mgmt`_ "
                     "is restored. "
                     "The default value is '72' hours.")}},
           {wake_on_start,
            #{value => "true | false",
              desc =>
                  ?T("If this option is set to 'true', notifications "
                     "are generated for **all** registered push clients "
                     "during server startup. This option should not be "
                     "enabled on servers with many push clients as it "
                     "can generate significant load on the involved push "
                     "services and the server itself. "
                     "The default value is 'false'.")}},
           {wake_on_timeout,
            #{value => "true | false",
              desc =>
                  ?T("If this option is set to 'true', a notification "
                     "is generated shortly before the session would time "
                     "out as per the 'resume_timeout' option. "
                     "The default value is 'true'.")}}]}.

%%--------------------------------------------------------------------
%% Register/unregister hooks.
%%--------------------------------------------------------------------
-spec register_hooks(binary()) -> ok.
register_hooks(Host) ->
    ejabberd_hooks:add(c2s_session_pending, Host, ?MODULE,
		       c2s_session_pending, 50),
    ejabberd_hooks:add(c2s_session_resumed, Host, ?MODULE,
		       c2s_session_resumed, 50),
    ejabberd_hooks:add(c2s_copy_session, Host, ?MODULE,
		       c2s_copy_session, 50),
    ejabberd_hooks:add(c2s_handle_cast, Host, ?MODULE,
		       c2s_handle_cast, 40),
    ejabberd_hooks:add(c2s_handle_info, Host, ?MODULE,
		       c2s_handle_info, 50),
    ejabberd_hooks:add(c2s_handle_send, Host, ?MODULE,
		       c2s_stanza, 50),
    % Wait for ejabberd_pkix before running our ejabberd_started/0, so that we
    % don't initiate s2s connections before certificates are loaded:
    ejabberd_hooks:add(ejabberd_started, ?MODULE, ejabberd_started, 90).

-spec unregister_hooks(binary()) -> ok.
unregister_hooks(Host) ->
    ejabberd_hooks:delete(c2s_session_pending, Host, ?MODULE,
			  c2s_session_pending, 50),
    ejabberd_hooks:delete(c2s_session_resumed, Host, ?MODULE,
			  c2s_session_resumed, 50),
    ejabberd_hooks:delete(c2s_copy_session, Host, ?MODULE,
			  c2s_copy_session, 50),
    ejabberd_hooks:delete(c2s_handle_cast, Host, ?MODULE,
			  c2s_handle_cast, 40),
    ejabberd_hooks:delete(c2s_handle_info, Host, ?MODULE,
			  c2s_handle_info, 50),
    ejabberd_hooks:delete(c2s_handle_send, Host, ?MODULE,
			  c2s_stanza, 50),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	false ->
	    ejabberd_hooks:delete(
	      ejabberd_started, ?MODULE, ejabberd_started, 90);
	true ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------
-spec c2s_stanza(c2s_state(), xmpp_element() | xmlel(), term()) -> c2s_state().
c2s_stanza(#{push_enabled := true, mgmt_state := pending} = State,
	   Pkt, _SendResult) ->
    case mod_push:is_incoming_chat_msg(Pkt) of
	true ->
	    maybe_restore_resume_timeout(State);
	false ->
	    State
    end;
c2s_stanza(State, _Pkt, _SendResult) ->
    State.

-spec c2s_session_pending(c2s_state()) -> c2s_state().
c2s_session_pending(#{push_enabled := true, mgmt_queue := Queue} = State) ->
    case mod_stream_mgmt:queue_find(fun mod_push:is_incoming_chat_msg/1,
				    Queue) of
	none ->
	    State1 = maybe_adjust_resume_timeout(State),
	    maybe_start_wakeup_timer(State1);
	_Msg ->
	    State
    end;
c2s_session_pending(State) ->
    State.

-spec c2s_session_resumed(c2s_state()) -> c2s_state().
c2s_session_resumed(#{push_enabled := true} = State) ->
    maybe_restore_resume_timeout(State);
c2s_session_resumed(State) ->
    State.

-spec c2s_copy_session(c2s_state(), c2s_state()) -> c2s_state().
c2s_copy_session(State, #{push_enabled := true,
			  push_resume_timeout := ResumeTimeout,
			  push_wake_on_timeout := WakeOnTimeout} = OldState) ->
    State1 = case maps:find(push_resume_timeout_orig, OldState) of
		 {ok, Val} ->
		     State#{push_resume_timeout_orig => Val};
		 error ->
		     State
	     end,
    State1#{push_resume_timeout => ResumeTimeout,
	    push_wake_on_timeout => WakeOnTimeout};
c2s_copy_session(State, _) ->
    State.

-spec c2s_handle_cast(c2s_state(), any()) -> c2s_state().
c2s_handle_cast(#{lserver := LServer} = State, {push_enable, _ID}) ->
    ResumeTimeout = mod_push_keepalive_opt:resume_timeout(LServer),
    WakeOnTimeout = mod_push_keepalive_opt:wake_on_timeout(LServer),
    State#{push_resume_timeout => ResumeTimeout,
	   push_wake_on_timeout => WakeOnTimeout};
c2s_handle_cast(State, push_disable) ->
    State1 = maps:remove(push_resume_timeout, State),
    maps:remove(push_wake_on_timeout, State1);
c2s_handle_cast(State, _Msg) ->
    State.

-spec c2s_handle_info(c2s_state(), any()) -> c2s_state() | {stop, c2s_state()}.
c2s_handle_info(#{push_enabled := true, mgmt_state := pending,
		  jid := JID} = State, {timeout, _, push_keepalive}) ->
    ?INFO_MSG("Waking ~ts before session times out", [jid:encode(JID)]),
    mod_push:notify(State, none, undefined),
    {stop, State};
c2s_handle_info(State, _) ->
    State.

-spec ejabberd_started() -> ok.
ejabberd_started() ->
    Pred = fun(Host) ->
		   gen_mod:is_loaded(Host, ?MODULE) andalso
		       mod_push_keepalive_opt:wake_on_start(Host)
	   end,
    [wake_all(Host) || Host <- ejabberd_config:get_option(hosts), Pred(Host)],
    ok.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec maybe_adjust_resume_timeout(c2s_state()) -> c2s_state().
maybe_adjust_resume_timeout(#{push_resume_timeout := undefined} = State) ->
    State;
maybe_adjust_resume_timeout(#{push_resume_timeout := Timeout} = State) ->
    OrigTimeout = mod_stream_mgmt:get_resume_timeout(State),
    ?DEBUG("Adjusting resume timeout to ~B seconds", [Timeout div 1000]),
    State1 = mod_stream_mgmt:set_resume_timeout(State, Timeout),
    State1#{push_resume_timeout_orig => OrigTimeout}.

-spec maybe_restore_resume_timeout(c2s_state()) -> c2s_state().
maybe_restore_resume_timeout(#{push_resume_timeout_orig := Timeout} = State) ->
    ?DEBUG("Restoring resume timeout to ~B seconds", [Timeout div 1000]),
    State1 = mod_stream_mgmt:set_resume_timeout(State, Timeout),
    maps:remove(push_resume_timeout_orig, State1);
maybe_restore_resume_timeout(State) ->
    State.

-spec maybe_start_wakeup_timer(c2s_state()) -> c2s_state().
maybe_start_wakeup_timer(#{push_wake_on_timeout := true,
			   push_resume_timeout := ResumeTimeout} = State)
  when is_integer(ResumeTimeout), ResumeTimeout > ?PUSH_BEFORE_TIMEOUT_PERIOD ->
    WakeTimeout = ResumeTimeout - ?PUSH_BEFORE_TIMEOUT_PERIOD,
    ?DEBUG("Scheduling wake-up timer to fire in ~B seconds", [WakeTimeout div 1000]),
    erlang:start_timer(WakeTimeout, self(), push_keepalive),
    State;
maybe_start_wakeup_timer(State) ->
    State.

-spec wake_all(binary()) -> ok.
wake_all(LServer) ->
    ?INFO_MSG("Waking all push clients on ~ts", [LServer]),
    Mod = gen_mod:db_mod(LServer, mod_push),
    case Mod:lookup_sessions(LServer) of
	{ok, Sessions} ->
	    IgnoreResponse = fun(_) -> ok end,
	    lists:foreach(fun({_, PushLJID, Node, XData}) ->
				  mod_push:notify(LServer, PushLJID, Node,
						  XData, none, undefined,
						  IgnoreResponse)
			  end, Sessions);
	error ->
	    ok
    end.
