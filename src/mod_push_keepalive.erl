%%%----------------------------------------------------------------------
%%% File    : mod_push_keepalive.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Keep pending XEP-0198 sessions alive with XEP-0357
%%% Created : 15 Jul 2017 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2017-2018 ProcessOne
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

%% ejabberd_hooks callbacks.
-export([c2s_session_pending/1, c2s_session_resumed/1, c2s_copy_session/2,
	 c2s_handle_cast/2, c2s_handle_info/2, c2s_stanza/3]).

-include("logger.hrl").
-include("xmpp.hrl").

-define(PUSH_BEFORE_TIMEOUT_SECS, 120).

-type c2s_state() :: ejabberd_c2s:state().

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, Opts) ->
    case gen_mod:get_opt(wake_on_start, Opts) of
	true ->
	    wake_all(Host);
	false ->
	    ok
    end,
    register_hooks(Host).

-spec stop(binary()) -> ok.
stop(Host) ->
    unregister_hooks(Host).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, OldOpts) ->
    case gen_mod:is_equal_opt(wake_on_start, NewOpts, OldOpts) of
	{false, true, _} ->
	    wake_all(Host);
	_ ->
	    ok
    end,
    ok.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [{mod_push, hard},
     {mod_client_state, soft},
     {mod_stream_mgmt, soft}].

-spec mod_opt_type(atom()) -> fun((term()) -> term()) | [atom()].
mod_opt_type(resume_timeout) ->
    fun(I) when is_integer(I), I >= 0 -> I;
       (undefined) -> undefined
    end;
mod_opt_type(wake_on_start) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(wake_on_timeout) ->
    fun (B) when is_boolean(B) -> B end.

mod_options(_Host) ->
    [{resume_timeout, 86400},
     {wake_on_start, false},
     {wake_on_timeout, true}].

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
		       c2s_stanza, 50).

-spec unregister_hooks(binary()) -> ok.
unregister_hooks(Host) ->
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  disco_sm_features, 50),
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
			  c2s_stanza, 50).

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------
-spec c2s_stanza(c2s_state(), xmpp_element() | xmlel(), term()) -> c2s_state().
c2s_stanza(#{push_enabled := true, mgmt_state := pending} = State,
	   Pkt, _SendResult) ->
    case mod_push:is_message_with_body(Pkt) of
	true ->
	    maybe_restore_resume_timeout(State);
	false ->
	    State
    end;
c2s_stanza(State, _Pkt, _SendResult) ->
    State.

-spec c2s_session_pending(c2s_state()) -> c2s_state().
c2s_session_pending(#{push_enabled := true, mgmt_queue := Queue} = State) ->
    case mod_stream_mgmt:queue_find(fun mod_push:is_message_with_body/1,
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
c2s_handle_cast(#{lserver := LServer} = State, push_enable) ->
    ResumeTimeout = gen_mod:get_module_opt(LServer, ?MODULE, resume_timeout),
    WakeOnTimeout = gen_mod:get_module_opt(LServer, ?MODULE, wake_on_timeout),
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
    ?INFO_MSG("Waking ~s before session times out", [jid:encode(JID)]),
    mod_push:notify(State, none),
    {stop, State};
c2s_handle_info(State, _) ->
    State.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec maybe_adjust_resume_timeout(c2s_state()) -> c2s_state().
maybe_adjust_resume_timeout(#{push_resume_timeout := undefined} = State) ->
    State;
maybe_adjust_resume_timeout(#{push_resume_timeout := Timeout} = State) ->
    OrigTimeout = mod_stream_mgmt:get_resume_timeout(State),
    ?DEBUG("Adjusting resume timeout to ~B seconds", [Timeout]),
    State1 = mod_stream_mgmt:set_resume_timeout(State, Timeout),
    State1#{push_resume_timeout_orig => OrigTimeout}.

-spec maybe_restore_resume_timeout(c2s_state()) -> c2s_state().
maybe_restore_resume_timeout(#{push_resume_timeout_orig := Timeout} = State) ->
    ?DEBUG("Restoring resume timeout to ~B seconds", [Timeout]),
    State1 = mod_stream_mgmt:set_resume_timeout(State, Timeout),
    maps:remove(push_resume_timeout_orig, State1);
maybe_restore_resume_timeout(State) ->
    State.

-spec maybe_start_wakeup_timer(c2s_state()) -> c2s_state().
maybe_start_wakeup_timer(#{push_wake_on_timeout := true,
			   push_resume_timeout := ResumeTimeout} = State)
  when is_integer(ResumeTimeout), ResumeTimeout > ?PUSH_BEFORE_TIMEOUT_SECS ->
    WakeTimeout = ResumeTimeout - ?PUSH_BEFORE_TIMEOUT_SECS,
    ?DEBUG("Scheduling wake-up timer to fire in ~B seconds", [WakeTimeout]),
    erlang:start_timer(timer:seconds(WakeTimeout), self(), push_keepalive),
    State;
maybe_start_wakeup_timer(State) ->
    State.

-spec wake_all(binary()) -> ok | error.
wake_all(LServer) ->
    ?INFO_MSG("Waking all push clients on ~s", [LServer]),
    Mod = gen_mod:db_mod(LServer, mod_push),
    case Mod:lookup_sessions(LServer) of
	{ok, Sessions} ->
	    IgnoreResponse = fun(_) -> ok end,
	    lists:foreach(fun({_, PushLJID, Node, XData}) ->
				  mod_push:notify(LServer, PushLJID, Node,
						  XData, none, IgnoreResponse)
			  end, Sessions);
	error ->
	    error
    end.
