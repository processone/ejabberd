%%%----------------------------------------------------------------------
%%% File    : mod_client_state.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Filter stanzas sent to inactive clients (XEP-0352)
%%% Created : 11 Sep 2014 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2014-2016   ProcessOne
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
-export([filter_presence/3, filter_chat_states/3, filter_pep/3, filter_other/3,
	 flush_queue/2, add_stream_feature/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(CSI_QUEUE_MAX, 100).

-type csi_type() :: presence | chatstate | {pep, binary()}.
-type csi_key() :: {ljid(), csi_type()}.
-type csi_stanza() :: {csi_key(), erlang:timestamp(), xmlel()}.
-type csi_queue() :: [csi_stanza()].

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
	   ejabberd_hooks:add(c2s_post_auth_features, Host, ?MODULE,
			      add_stream_feature, 50),
	   if QueuePresence ->
		  ejabberd_hooks:add(csi_filter_stanza, Host, ?MODULE,
				     filter_presence, 50);
	      true -> ok
	   end,
	   if QueueChatStates ->
		  ejabberd_hooks:add(csi_filter_stanza, Host, ?MODULE,
				     filter_chat_states, 50);
	      true -> ok
	   end,
	   if QueuePEP ->
		  ejabberd_hooks:add(csi_filter_stanza, Host, ?MODULE,
				     filter_pep, 50);
	      true -> ok
	   end,
	   ejabberd_hooks:add(csi_filter_stanza, Host, ?MODULE,
			      filter_other, 100),
	   ejabberd_hooks:add(csi_flush_queue, Host, ?MODULE,
			      flush_queue, 50);
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
	   ejabberd_hooks:delete(c2s_post_auth_features, Host, ?MODULE,
				 add_stream_feature, 50),
	   if QueuePresence ->
		  ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
					filter_presence, 50);
	      true -> ok
	   end,
	   if QueueChatStates ->
		  ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
					filter_chat_states, 50);
	      true -> ok
	   end,
	   if QueuePEP ->
		  ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
					filter_pep, 50);
	      true -> ok
	   end,
	   ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
				 filter_other, 100),
	   ejabberd_hooks:delete(csi_flush_queue, Host, ?MODULE,
				 flush_queue, 50);
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

-spec filter_presence({ejabberd_c2s:state(), [stanza()]}, binary(), stanza())
      -> {ejabberd_c2s:state(), [stanza()]} |
	 {stop, {ejabberd_c2s:state(), [stanza()]}}.

filter_presence({C2SState, _OutStanzas} = Acc, Host,
		#presence{type = Type} = Stanza) ->
    if Type == available; Type == unavailable ->
	    ?DEBUG("Got availability presence stanza", []),
	    queue_add(presence, Stanza, Host, C2SState);
       true ->
	    Acc
    end;
filter_presence(Acc, _Host, _Stanza) -> Acc.

-spec filter_chat_states({ejabberd_c2s:state(), [stanza()]}, binary(), stanza())
      -> {ejabberd_c2s:state(), [stanza()]} |
	 {stop, {ejabberd_c2s:state(), [stanza()]}}.

filter_chat_states({C2SState, _OutStanzas} = Acc, Host,
		   #message{from = From, to = To} = Stanza) ->
    case xmpp_util:is_standalone_chat_state(Stanza) of
	true ->
	    case {From, To} of
		{#jid{luser = U, lserver = S}, #jid{luser = U, lserver = S}} ->
		    %% Don't queue (carbon copies of) chat states from other
		    %% resources, as they might be used to sync the state of
		    %% conversations across clients.
		    Acc;
		_ ->
		    ?DEBUG("Got standalone chat state notification", []),
		    queue_add(chatstate, Stanza, Host, C2SState)
	    end;
	false ->
	    Acc
    end;
filter_chat_states(Acc, _Host, _Stanza) -> Acc.

-spec filter_pep({ejabberd_c2s:state(), [stanza()]}, binary(), stanza())
      -> {ejabberd_c2s:state(), [stanza()]} |
	 {stop, {ejabberd_c2s:state(), [stanza()]}}.

filter_pep({C2SState, _OutStanzas} = Acc, Host, #message{} = Stanza) ->
    case get_pep_node(Stanza) of
	undefined ->
	    Acc;
	Node ->
	    ?DEBUG("Got PEP notification", []),
	    queue_add({pep, Node}, Stanza, Host, C2SState)
    end;
filter_pep(Acc, _Host, _Stanza) -> Acc.

-spec filter_other({ejabberd_c2s:state(), [stanza()]}, binary(), stanza())
      -> {stop, {ejabberd_c2s:state(), [stanza()]}}.

filter_other({C2SState, _OutStanzas}, Host, Stanza) ->
    ?DEBUG("Won't add stanza to CSI queue", []),
    queue_take(Stanza, Host, C2SState).

-spec flush_queue({ejabberd_c2s:state(), [stanza()]}, binary())
      -> {ejabberd_c2s:state(), [stanza()]}.

flush_queue({C2SState, _OutStanzas}, Host) ->
    ?DEBUG("Going to flush CSI queue", []),
    Queue = get_queue(C2SState),
    NewState = set_queue([], C2SState),
    {NewState, get_stanzas(Queue, Host)}.

-spec add_stream_feature([stanza()], binary) -> [stanza()].

add_stream_feature(Features, _Host) ->
    [#feature_csi{xmlns = <<"urn:xmpp:csi:0">>} | Features].

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------

-spec queue_add(csi_type(), stanza(), binary(), term())
      -> {stop, {term(), [stanza()]}}.

queue_add(Type, Stanza, Host, C2SState) ->
    case get_queue(C2SState) of
      Queue when length(Queue) >= ?CSI_QUEUE_MAX ->
	  ?DEBUG("CSI queue too large, going to flush it", []),
	  NewState = set_queue([], C2SState),
	  {stop, {NewState, get_stanzas(Queue, Host) ++ [Stanza]}};
      Queue ->
	  ?DEBUG("Adding stanza to CSI queue", []),
	  From = xmpp:get_from(Stanza),
	  Key = {jid:tolower(From), Type},
	  Entry = {Key, p1_time_compat:timestamp(), Stanza},
	  NewQueue = lists:keystore(Key, 1, Queue, Entry),
	  NewState = set_queue(NewQueue, C2SState),
	  {stop, {NewState, []}}
    end.

-spec queue_take(stanza(), binary(), term()) -> {stop, {term(), [stanza()]}}.

queue_take(Stanza, Host, C2SState) ->
    From = xmpp:get_from(Stanza),
    {LUser, LServer, _LResource} = jid:tolower(From),
    {Selected, Rest} = lists:partition(
			 fun({{{U, S, _R}, _Type}, _Time, _Stanza}) ->
				 U == LUser andalso S == LServer
			 end, get_queue(C2SState)),
    NewState = set_queue(Rest, C2SState),
    {stop, {NewState, get_stanzas(Selected, Host) ++ [Stanza]}}.

-spec set_queue(csi_queue(), term()) -> term().

set_queue(Queue, C2SState) ->
    ejabberd_c2s:set_aux_field(csi_queue, Queue, C2SState).

-spec get_queue(term()) -> csi_queue().

get_queue(C2SState) ->
    case ejabberd_c2s:get_aux_field(csi_queue, C2SState) of
      {ok, Queue} ->
	      Queue;
      error ->
	      []
    end.

-spec get_stanzas(csi_queue(), binary()) -> [stanza()].

get_stanzas(Queue, Host) ->
    lists:map(fun({_Key, Time, Stanza}) ->
		      xmpp_util:add_delay_info(Stanza, Host, Time,
					       <<"Client Inactive">>)
	      end, Queue).

-spec get_pep_node(message()) -> binary() | undefined.

get_pep_node(#message{from = #jid{luser = <<>>}}) ->
    %% It's not PEP.
    undefined;
get_pep_node(#message{} = Msg) ->
    case xmpp:get_subtag(Msg, #pubsub_event{}) of
	#pubsub_event{items = [#pubsub_event_items{node = Node}]} ->
	    Node;
	_ ->
	    undefined
    end.
