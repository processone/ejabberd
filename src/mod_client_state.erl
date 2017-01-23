%%%----------------------------------------------------------------------
%%% File    : mod_client_state.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Filter stanzas sent to inactive clients (XEP-0352)
%%% Created : 11 Sep 2014 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2014-2017   ProcessOne
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

-module(mod_client_state).
-author('holger@zedat.fu-berlin.de').
-protocol({xep, 85, '2.1'}).
-protocol({xep, 352, '0.1'}).

-behavior(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, mod_opt_type/1, depends/2]).

%% ejabberd_hooks callbacks.
-export([filter_presence/1, filter_chat_states/1,
	 filter_pep/1, filter_other/1,
	 c2s_stream_started/2, add_stream_feature/2,
	 c2s_copy_session/2, c2s_authenticated_packet/2,
	 c2s_session_resumed/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(CSI_QUEUE_MAX, 100).

-type csi_type() :: presence | chatstate | {pep, binary()}.
-type csi_queue() :: {non_neg_integer(), non_neg_integer(), map()}.
-type csi_timestamp() :: {non_neg_integer(), erlang:timestamp()}.
-type c2s_state() :: ejabberd_c2s:state().
-type filter_acc() :: {stanza() | drop, c2s_state()}.

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------

-spec start(binary(), gen_mod:opts()) -> ok.

start(Host, Opts) ->
    QueuePresence =
	gen_mod:get_opt(queue_presence, Opts,
			fun(B) when is_boolean(B) -> B end,
			true),
    QueueChatStates =
	gen_mod:get_opt(queue_chat_states, Opts,
			fun(B) when is_boolean(B) -> B end,
			true),
    QueuePEP =
	gen_mod:get_opt(queue_pep, Opts,
			fun(B) when is_boolean(B) -> B end,
			true),
    if QueuePresence; QueueChatStates; QueuePEP ->
	   ejabberd_hooks:add(c2s_stream_started, Host, ?MODULE,
			      c2s_stream_started, 50),
	   ejabberd_hooks:add(c2s_post_auth_features, Host, ?MODULE,
			      add_stream_feature, 50),
	   ejabberd_hooks:add(c2s_authenticated_packet, Host, ?MODULE,
			      c2s_authenticated_packet, 50),
	   ejabberd_hooks:add(c2s_copy_session, Host, ?MODULE,
			      c2s_copy_session, 50),
	   ejabberd_hooks:add(c2s_session_resumed, Host, ?MODULE,
			      c2s_session_resumed, 50),
	   if QueuePresence ->
		  ejabberd_hooks:add(c2s_filter_send, Host, ?MODULE,
				     filter_presence, 50);
	      true -> ok
	   end,
	   if QueueChatStates ->
		  ejabberd_hooks:add(c2s_filter_send, Host, ?MODULE,
				     filter_chat_states, 50);
	      true -> ok
	   end,
	   if QueuePEP ->
		  ejabberd_hooks:add(c2s_filter_send, Host, ?MODULE,
				     filter_pep, 50);
	      true -> ok
	   end,
	   ejabberd_hooks:add(c2s_filter_send, Host, ?MODULE,
			      filter_other, 75);
       true -> ok
    end.

-spec stop(binary()) -> ok.

stop(Host) ->
    QueuePresence =
	gen_mod:get_module_opt(Host, ?MODULE, queue_presence,
			       fun(B) when is_boolean(B) -> B end,
			       true),
    QueueChatStates =
	gen_mod:get_module_opt(Host, ?MODULE, queue_chat_states,
			       fun(B) when is_boolean(B) -> B end,
			       true),
    QueuePEP =
	gen_mod:get_module_opt(Host, ?MODULE, queue_pep,
			       fun(B) when is_boolean(B) -> B end,
			       true),
    if QueuePresence; QueueChatStates; QueuePEP ->
	   ejabberd_hooks:delete(c2s_stream_started, Host, ?MODULE,
				 c2s_stream_started, 50),
	   ejabberd_hooks:delete(c2s_post_auth_features, Host, ?MODULE,
				 add_stream_feature, 50),
	   ejabberd_hooks:delete(c2s_authenticated_packet, Host, ?MODULE,
				 c2s_authenticated_packet, 50),
	   ejabberd_hooks:delete(c2s_copy_session, Host, ?MODULE,
				 c2s_copy_session, 50),
	   ejabberd_hooks:delete(c2s_session_resumed, Host, ?MODULE,
				 c2s_session_resumed, 50),
	   if QueuePresence ->
		  ejabberd_hooks:delete(c2s_filter_send, Host, ?MODULE,
					filter_presence, 50);
	      true -> ok
	   end,
	   if QueueChatStates ->
		  ejabberd_hooks:delete(c2s_filter_send, Host, ?MODULE,
					filter_chat_states, 50);
	      true -> ok
	   end,
	   if QueuePEP ->
		  ejabberd_hooks:delete(c2s_filter_send, Host, ?MODULE,
					filter_pep, 50);
	      true -> ok
	   end,
	   ejabberd_hooks:delete(c2s_filter_send, Host, ?MODULE,
				 filter_other, 75);
       true -> ok
    end.

-spec mod_opt_type(atom()) -> fun((term()) -> term()) | [atom()].

mod_opt_type(queue_presence) ->
    fun(B) when is_boolean(B) -> B end;
mod_opt_type(queue_chat_states) ->
    fun(B) when is_boolean(B) -> B end;
mod_opt_type(queue_pep) ->
    fun(B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [queue_presence, queue_chat_states, queue_pep].

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].

depends(_Host, _Opts) ->
    [].

%%--------------------------------------------------------------------
%% ejabberd_hooks callbacks.
%%--------------------------------------------------------------------
-spec c2s_stream_started(c2s_state(), stream_start()) -> c2s_state().
c2s_stream_started(State, _) ->
    State#{csi_state => active, csi_queue => queue_new()}.

-spec c2s_authenticated_packet(c2s_state(), xmpp_element()) -> c2s_state().
c2s_authenticated_packet(C2SState, #csi{type = active}) ->
    C2SState1 = C2SState#{csi_state => active},
    flush_queue(C2SState1);
c2s_authenticated_packet(C2SState, #csi{type = inactive}) ->
    C2SState#{csi_state => inactive};
c2s_authenticated_packet(C2SState, _) ->
    C2SState.

-spec c2s_copy_session(c2s_state(), c2s_state()) -> c2s_state().
c2s_copy_session(C2SState, #{csi_state := State, csi_queue := Q}) ->
    C2SState#{csi_state => State, csi_queue => Q};
c2s_copy_session(C2SState, _) ->
    C2SState.

-spec c2s_session_resumed(c2s_state()) -> c2s_state().
c2s_session_resumed(C2SState) ->
    flush_queue(C2SState).

-spec filter_presence(filter_acc()) -> filter_acc().
filter_presence({#presence{meta = #{csi_resend := true}}, _} = Acc) ->
    Acc;
filter_presence({#presence{to = To, type = Type} = Pres,
		 #{csi_state := inactive} = C2SState})
  when Type == available; Type == unavailable ->
    ?DEBUG("Got availability presence stanza for ~s", [jid:to_string(To)]),
    enqueue_stanza(presence, Pres, C2SState);
filter_presence(Acc) ->
    Acc.

-spec filter_chat_states(filter_acc()) -> filter_acc().
filter_chat_states({#message{meta = #{csi_resend := true}}, _} = Acc) ->
    Acc;
filter_chat_states({#message{from = From, to = To} = Msg,
		    #{csi_state := inactive} = C2SState} = Acc) ->
    case xmpp_util:is_standalone_chat_state(Msg) of
	true ->
	    case {From, To} of
		{#jid{luser = U, lserver = S}, #jid{luser = U, lserver = S}} ->
		    %% Don't queue (carbon copies of) chat states from other
		    %% resources, as they might be used to sync the state of
		    %% conversations across clients.
		    Acc;
		_ ->
		?DEBUG("Got standalone chat state notification for ~s",
		       [jid:to_string(To)]),
		    enqueue_stanza(chatstate, Msg, C2SState)
	    end;
	false ->
	    Acc
    end;
filter_chat_states(Acc) ->
    Acc.

-spec filter_pep(filter_acc()) -> filter_acc().
filter_pep({#message{meta = #{csi_resend := true}}, _} = Acc) ->
    Acc;
filter_pep({#message{to = To} = Msg,
	    #{csi_state := inactive} = C2SState} = Acc) ->
    case get_pep_node(Msg) of
	undefined ->
	    Acc;
	Node ->
	    ?DEBUG("Got PEP notification for ~s", [jid:to_string(To)]),
	    enqueue_stanza({pep, Node}, Msg, C2SState)
    end;
filter_pep(Acc) ->
    Acc.

-spec filter_other(filter_acc()) -> filter_acc().
filter_other({Stanza, #{jid := JID} = C2SState} = Acc) when ?is_stanza(Stanza) ->
    case xmpp:get_meta(Stanza) of
	#{csi_resend := true} ->
	    Acc;
	_ ->
	    ?DEBUG("Won't add stanza for ~s to CSI queue", [jid:to_string(JID)]),
	    From = xmpp:get_from(Stanza),
	    C2SState1 = dequeue_sender(From, C2SState),
	    {Stanza, C2SState1}
    end;
filter_other(Acc) ->
    Acc.

-spec add_stream_feature([xmpp_element()], binary) -> [xmpp_element()].
add_stream_feature(Features, Host) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
	true ->
	    [#feature_csi{xmlns = <<"urn:xmpp:csi:0">>} | Features];
	false ->
	    Features
    end.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec enqueue_stanza(csi_type(), stanza(), c2s_state()) -> filter_acc().
enqueue_stanza(Type, Stanza, #{csi_state := inactive,
			       csi_queue := Q} = C2SState) ->
    case queue_len(Q) >= ?CSI_QUEUE_MAX of
	true ->
	  ?DEBUG("CSI queue too large, going to flush it", []),
	    C2SState1 = flush_queue(C2SState),
	    enqueue_stanza(Type, Stanza, C2SState1);
	false ->
	    #jid{luser = U, lserver = S} = xmpp:get_from(Stanza),
	    Q1 = queue_in({U, S}, Type, Stanza, Q),
	    {stop, {drop, C2SState#{csi_queue => Q1}}}
    end;
enqueue_stanza(_Type, Stanza, State) ->
    {Stanza, State}.

-spec dequeue_sender(jid(), c2s_state()) -> c2s_state().
dequeue_sender(#jid{luser = U, lserver = S},
	       #{csi_queue := Q, jid := JID} = C2SState) ->
    ?DEBUG("Flushing packets of ~s@~s from CSI queue of ~s",
	   [U, S, jid:to_string(JID)]),
    case queue_take({U, S}, Q) of
	{Stanzas, Q1} ->
	    C2SState1 = flush_stanzas(C2SState, Stanzas),
	    C2SState1#{csi_queue => Q1};
      error ->
	    C2SState
    end.

-spec flush_queue(c2s_state()) -> c2s_state().
flush_queue(#{csi_queue := Q, jid := JID} = C2SState) ->
    ?DEBUG("Flushing CSI queue of ~s", [jid:to_string(JID)]),
    C2SState1 = flush_stanzas(C2SState, queue_to_list(Q)),
    C2SState1#{csi_queue => queue_new()}.

-spec flush_stanzas(c2s_state(),
		    [{csi_type(), csi_timestamp(), stanza()}]) -> c2s_state().
flush_stanzas(#{lserver := LServer} = C2SState, Elems) ->
    lists:foldl(
      fun({_Type, Time, Stanza}, AccState) ->
	      Stanza1 = add_delay_info(Stanza, LServer, Time),
	      ejabberd_c2s:send(AccState, Stanza1)
      end, C2SState, Elems).

-spec add_delay_info(stanza(), binary(), csi_timestamp()) -> stanza().
add_delay_info(Stanza, LServer, {_Seq, TimeStamp}) ->
    Stanza1 = xmpp_util:add_delay_info(
		Stanza, jid:make(LServer), TimeStamp,
		<<"Client Inactive">>),
    xmpp:put_meta(Stanza1, csi_resend, true).

-spec get_pep_node(message()) -> binary() | undefined.
get_pep_node(#message{from = #jid{luser = <<>>}}) ->
    %% It's not PEP.
    undefined;
get_pep_node(#message{} = Msg) ->
    case xmpp:get_subtag(Msg, #ps_event{}) of
	#ps_event{items = #ps_items{node = Node}} ->
	    Node;
	_ ->
	    undefined
    end.

%%--------------------------------------------------------------------
%% Queue interface
%%--------------------------------------------------------------------
-spec queue_new() -> csi_queue().
queue_new() ->
    {0, 0, #{}}.

-spec queue_in(term(), term(), term(), csi_queue()) -> csi_queue().
queue_in(Key, Type, Val, {N, Seq, Q}) ->
    Seq1 = Seq + 1,
    Time = {Seq1, p1_time_compat:timestamp()},
    case maps:get(Key, Q, error) of
	error ->
	    Q1 = maps:put(Key, [{Type, Time, Val}], Q),
	    {N + 1, Seq1, Q1};
	TypeVals ->
	    case lists:keymember(Type, 1, TypeVals) of
		true ->
		    TypeVals1 = lists:keyreplace(
				  Type, 1, TypeVals, {Type, Time, Val}),
		    Q1 = maps:put(Key, TypeVals1, Q),
		    {N, Seq1, Q1};
		false ->
		    TypeVals1 = [{Type, Time, Val}|TypeVals],
		    Q1 = maps:put(Key, TypeVals1, Q),
		    {N + 1, Seq1, Q1}
	    end
    end.

-spec queue_take(term(), csi_queue()) -> {list(), csi_queue()} | error.
queue_take(Key, {N, Seq, Q}) ->
    case maps:get(Key, Q, error) of
	error ->
	    error;
	TypeVals ->
	    Q1 = maps:remove(Key, Q),
	    {lists:keysort(2, TypeVals), {N-length(TypeVals), Seq, Q1}}
    end.

-spec queue_len(csi_queue()) -> non_neg_integer().
queue_len({N, _, _}) ->
    N.

-spec queue_to_list(csi_queue()) -> [term()].
queue_to_list({_, _, Q}) ->
    TypeVals = maps:fold(
		 fun(_, Vals, Acc) ->
			 Vals ++ Acc
		 end, [], Q),
    lists:keysort(2, TypeVals).
