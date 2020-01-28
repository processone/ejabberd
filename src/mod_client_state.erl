%%%----------------------------------------------------------------------
%%% File    : mod_client_state.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Filter stanzas sent to inactive clients (XEP-0352)
%%% Created : 11 Sep 2014 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2014-2020   ProcessOne
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

-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, depends/2, mod_options/1]).
-export([mod_doc/0]).

%% ejabberd_hooks callbacks.
-export([filter_presence/1, filter_chat_states/1,
	 filter_pep/1, filter_other/1,
	 c2s_stream_started/2, add_stream_feature/2,
	 c2s_authenticated_packet/2, csi_activity/2,
	 c2s_copy_session/2, c2s_session_resumed/1]).

-include("logger.hrl").
-include("xmpp.hrl").
-include("translate.hrl").

-define(CSI_QUEUE_MAX, 100).

-type csi_type() :: presence | chatstate | {pep, binary()}.
-type csi_queue() :: {non_neg_integer(), #{csi_key() => csi_element()}}.
-type csi_timestamp() :: {non_neg_integer(), erlang:timestamp()}.
-type csi_key() :: {ljid(), csi_type()}.
-type csi_element() :: {csi_timestamp(), stanza()}.
-type c2s_state() :: ejabberd_c2s:state().
-type filter_acc() :: {stanza() | drop, c2s_state()}.

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, Opts) ->
    QueuePresence = mod_client_state_opt:queue_presence(Opts),
    QueueChatStates = mod_client_state_opt:queue_chat_states(Opts),
    QueuePEP = mod_client_state_opt:queue_pep(Opts),
    if QueuePresence; QueueChatStates; QueuePEP ->
	   register_hooks(Host),
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
	   end;
       true -> ok
    end.

-spec stop(binary()) -> ok.
stop(Host) ->
    QueuePresence = mod_client_state_opt:queue_presence(Host),
    QueueChatStates = mod_client_state_opt:queue_chat_states(Host),
    QueuePEP = mod_client_state_opt:queue_pep(Host),
    if QueuePresence; QueueChatStates; QueuePEP ->
	   unregister_hooks(Host),
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
	   end;
       true -> ok
    end.

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, _OldOpts) ->
    QueuePresence = mod_client_state_opt:queue_presence(NewOpts),
    QueueChatStates = mod_client_state_opt:queue_chat_states(NewOpts),
    QueuePEP = mod_client_state_opt:queue_pep(NewOpts),
    if QueuePresence; QueueChatStates; QueuePEP ->
	    register_hooks(Host);
       true ->
	    unregister_hooks(Host)
    end,
    if QueuePresence ->
	    ejabberd_hooks:add(c2s_filter_send, Host, ?MODULE,
			       filter_presence, 50);
       true ->
	    ejabberd_hooks:delete(c2s_filter_send, Host, ?MODULE,
				  filter_presence, 50)
    end,
    if QueueChatStates ->
	    ejabberd_hooks:add(c2s_filter_send, Host, ?MODULE,
			       filter_chat_states, 50);
       true ->
	    ejabberd_hooks:delete(c2s_filter_send, Host, ?MODULE,
				  filter_chat_states, 50)
    end,
    if QueuePEP ->
	    ejabberd_hooks:add(c2s_filter_send, Host, ?MODULE,
			       filter_pep, 50);
       true ->
	    ejabberd_hooks:delete(c2s_filter_send, Host, ?MODULE,
				  filter_pep, 50)
    end.

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(queue_presence) ->
    econf:bool();
mod_opt_type(queue_chat_states) ->
    econf:bool();
mod_opt_type(queue_pep) ->
    econf:bool().

mod_options(_) ->
    [{queue_presence, true},
     {queue_chat_states, true},
     {queue_pep, true}].

mod_doc() ->
    #{desc =>
          [?T("This module allows for queueing certain types of stanzas "
              "when a client indicates that the user is not actively using "
              "the client right now (see https://xmpp.org/extensions/xep-0352.html"
              "[XEP-0352: Client State Indication]). This can save bandwidth and "
              "resources."), "",
           ?T("A stanza is dropped from the queue if it's effectively obsoleted "
              "by a new one (e.g., a new presence stanza would replace an old "
              "one from the same client). The queue is flushed if a stanza arrives "
              "that won't be queued, or if the queue size reaches a certain limit "
              "(currently 100 stanzas), or if the client becomes active again.")],
      opts =>
          [{queue_presence,
            #{value => "true | false",
              desc =>
                  ?T("While a client is inactive, queue presence stanzas "
                     "that indicate (un)availability. The default value is 'true'.")}},
           {queue_chat_states,
            #{value => "true | false",
              desc =>
                  ?T("Queue \"standalone\" chat state notifications (as defined in "
                     "https://xmpp.org/extensions/xep-0085.html"
                     "[XEP-0085: Chat State Notifications]) while a client "
                     "indicates inactivity. The default value is 'true'.")}},
           {queue_pep,
            #{value => "true | false",
              desc =>
                  ?T("Queue PEP notifications while a client is inactive. "
                     "When the queue is flushed, only the most recent notification "
                     "of a given PEP node is delivered. The default value is 'true'.")}}]}.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec register_hooks(binary()) -> ok.
register_hooks(Host) ->
    ejabberd_hooks:add(c2s_stream_started, Host, ?MODULE,
		       c2s_stream_started, 50),
    ejabberd_hooks:add(c2s_post_auth_features, Host, ?MODULE,
		       add_stream_feature, 50),
    ejabberd_hooks:add(c2s_authenticated_packet, Host, ?MODULE,
		       c2s_authenticated_packet, 50),
    ejabberd_hooks:add(csi_activity, Host, ?MODULE,
		       csi_activity, 50),
    ejabberd_hooks:add(c2s_copy_session, Host, ?MODULE,
		       c2s_copy_session, 50),
    ejabberd_hooks:add(c2s_session_resumed, Host, ?MODULE,
		       c2s_session_resumed, 50),
    ejabberd_hooks:add(c2s_filter_send, Host, ?MODULE,
		       filter_other, 75).

-spec unregister_hooks(binary()) -> ok.
unregister_hooks(Host) ->
    ejabberd_hooks:delete(c2s_stream_started, Host, ?MODULE,
			  c2s_stream_started, 50),
    ejabberd_hooks:delete(c2s_post_auth_features, Host, ?MODULE,
			  add_stream_feature, 50),
    ejabberd_hooks:delete(c2s_authenticated_packet, Host, ?MODULE,
			  c2s_authenticated_packet, 50),
    ejabberd_hooks:delete(csi_activity, Host, ?MODULE,
			  csi_activity, 50),
    ejabberd_hooks:delete(c2s_copy_session, Host, ?MODULE,
			  c2s_copy_session, 50),
    ejabberd_hooks:delete(c2s_session_resumed, Host, ?MODULE,
			  c2s_session_resumed, 50),
    ejabberd_hooks:delete(c2s_filter_send, Host, ?MODULE,
			  filter_other, 75).

%%--------------------------------------------------------------------
%% ejabberd_hooks callbacks.
%%--------------------------------------------------------------------
-spec c2s_stream_started(c2s_state(), stream_start()) -> c2s_state().
c2s_stream_started(State, _) ->
    init_csi_state(State).

-spec c2s_authenticated_packet(c2s_state(), xmpp_element()) -> c2s_state().
c2s_authenticated_packet(#{lserver := LServer} = C2SState, #csi{type = active}) ->
    ejabberd_hooks:run_fold(csi_activity, LServer, C2SState, [active]);
c2s_authenticated_packet(#{lserver := LServer} = C2SState, #csi{type = inactive}) ->
    ejabberd_hooks:run_fold(csi_activity, LServer, C2SState, [inactive]);
c2s_authenticated_packet(C2SState, _) ->
    C2SState.

-spec csi_activity(c2s_state(), active | inactive) -> c2s_state().
csi_activity(C2SState, active) ->
    C2SState1 = C2SState#{csi_state => active},
    flush_queue(C2SState1);
csi_activity(C2SState, inactive) ->
    C2SState#{csi_state => inactive}.

-spec c2s_copy_session(c2s_state(), c2s_state()) -> c2s_state().
c2s_copy_session(C2SState, #{csi_queue := Q}) ->
    C2SState#{csi_queue => Q};
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
    ?DEBUG("Got availability presence stanza for ~ts", [jid:encode(To)]),
    enqueue_stanza(presence, Pres, C2SState);
filter_presence(Acc) ->
    Acc.

-spec filter_chat_states(filter_acc()) -> filter_acc().
filter_chat_states({#message{meta = #{csi_resend := true}}, _} = Acc) ->
    Acc;
filter_chat_states({#message{from = From, to = To} = Msg,
		    #{csi_state := inactive} = C2SState} = Acc) ->
    case misc:is_standalone_chat_state(Msg) of
	true ->
	    case {From, To} of
		{#jid{luser = U, lserver = S}, #jid{luser = U, lserver = S}} ->
		    %% Don't queue (carbon copies of) chat states from other
		    %% resources, as they might be used to sync the state of
		    %% conversations across clients.
		    Acc;
		_ ->
		?DEBUG("Got standalone chat state notification for ~ts",
		       [jid:encode(To)]),
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
	    ?DEBUG("Got PEP notification for ~ts", [jid:encode(To)]),
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
	    ?DEBUG("Won't add stanza for ~ts to CSI queue", [jid:encode(JID)]),
	    From = case xmpp:get_from(Stanza) of
		       undefined -> JID;
		       F -> F
		   end,
	    C2SState1 = dequeue_sender(From, C2SState),
	    {Stanza, C2SState1}
    end;
filter_other(Acc) ->
    Acc.

-spec add_stream_feature([xmpp_element()], binary()) -> [xmpp_element()].
add_stream_feature(Features, Host) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
	true ->
	    [#feature_csi{} | Features];
	false ->
	    Features
    end.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec init_csi_state(c2s_state()) -> c2s_state().
init_csi_state(C2SState) ->
    C2SState#{csi_state => active, csi_queue => queue_new()}.

-spec enqueue_stanza(csi_type(), stanza(), c2s_state()) -> filter_acc().
enqueue_stanza(Type, Stanza, #{csi_state := inactive,
			       csi_queue := Q} = C2SState) ->
    case queue_len(Q) >= ?CSI_QUEUE_MAX of
	true ->
	  ?DEBUG("CSI queue too large, going to flush it", []),
	    C2SState1 = flush_queue(C2SState),
	    enqueue_stanza(Type, Stanza, C2SState1);
	false ->
	    From = jid:tolower(xmpp:get_from(Stanza)),
	    Q1 = queue_in({From, Type}, Stanza, Q),
	    {stop, {drop, C2SState#{csi_queue => Q1}}}
    end;
enqueue_stanza(_Type, Stanza, State) ->
    {Stanza, State}.

-spec dequeue_sender(jid(), c2s_state()) -> c2s_state().
dequeue_sender(#jid{luser = U, lserver = S} = Sender,
	       #{jid := JID} = C2SState) ->
    case maps:get(csi_queue, C2SState, undefined) of
	undefined ->
	    %% This may happen when the module is (re)loaded in runtime
	    init_csi_state(C2SState);
	Q ->
	    ?DEBUG("Flushing packets of ~ts@~ts from CSI queue of ~ts",
		   [U, S, jid:encode(JID)]),
	    {Elems, Q1} = queue_take(Sender, Q),
	    C2SState1 = flush_stanzas(C2SState, Elems),
	    C2SState1#{csi_queue => Q1}
    end.

-spec flush_queue(c2s_state()) -> c2s_state().
flush_queue(#{csi_queue := Q, jid := JID} = C2SState) ->
    ?DEBUG("Flushing CSI queue of ~ts", [jid:encode(JID)]),
    C2SState1 = flush_stanzas(C2SState, queue_to_list(Q)),
    C2SState1#{csi_queue => queue_new()}.

-spec flush_stanzas(c2s_state(),
		    [{csi_type(), csi_timestamp(), stanza()}]) -> c2s_state().
flush_stanzas(#{lserver := LServer} = C2SState, Elems) ->
    lists:foldl(
      fun({Time, Stanza}, AccState) ->
	      Stanza1 = add_delay_info(Stanza, LServer, Time),
	      ejabberd_c2s:send(AccState, Stanza1)
      end, C2SState, Elems).

-spec add_delay_info(stanza(), binary(), csi_timestamp()) -> stanza().
add_delay_info(Stanza, LServer, {_Seq, TimeStamp}) ->
    Stanza1 = misc:add_delay_info(
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
    {0, #{}}.

-spec queue_in(csi_key(), stanza(), csi_queue()) -> csi_queue().
queue_in(Key, Stanza, {Seq, Q}) ->
    Seq1 = Seq + 1,
    Time = {Seq1, erlang:timestamp()},
    Q1 = maps:put(Key, {Time, Stanza}, Q),
    {Seq1, Q1}.

-spec queue_take(jid(), csi_queue()) -> {[csi_element()], csi_queue()}.
queue_take(#jid{luser = LUser, lserver = LServer}, {Seq, Q}) ->
    {Vals, Q1} = maps:fold(fun({{U, S, _}, _} = Key, Val, {AccVals, AccQ})
				   when U == LUser, S == LServer ->
				   {[Val | AccVals], maps:remove(Key, AccQ)};
			       (_, _, Acc) ->
				   Acc
			    end, {[], Q}, Q),
    {lists:keysort(1, Vals), {Seq, Q1}}.

-spec queue_len(csi_queue()) -> non_neg_integer().
queue_len({_, Q}) ->
    maps:size(Q).

-spec queue_to_list(csi_queue()) -> [csi_element()].
queue_to_list({_, Q}) ->
    lists:keysort(1, maps:values(Q)).
