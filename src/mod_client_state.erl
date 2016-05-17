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

-export([start/2, stop/1, add_stream_feature/2,
	 filter_presence/3, filter_chat_states/3, filter_other/3, flush_queue/2,
	 mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(CSI_QUEUE_MAX, 100).

start(Host, Opts) ->
    QueuePresence = gen_mod:get_opt(queue_presence, Opts,
				    fun(B) when is_boolean(B) -> B end,
				    true),
    DropChatStates = gen_mod:get_opt(drop_chat_states, Opts,
				     fun(B) when is_boolean(B) -> B end,
				     true),
    if QueuePresence; DropChatStates ->
	   ejabberd_hooks:add(c2s_post_auth_features, Host, ?MODULE,
			      add_stream_feature, 50),
	   if QueuePresence ->
		  ejabberd_hooks:add(csi_filter_stanza, Host, ?MODULE,
				     filter_presence, 50);
	      true -> ok
	   end,
	   if DropChatStates ->
		  ejabberd_hooks:add(csi_filter_stanza, Host, ?MODULE,
				     filter_chat_states, 50);
	      true -> ok
	   end,
	   ejabberd_hooks:add(csi_filter_stanza, Host, ?MODULE,
			      filter_other, 100),
	   ejabberd_hooks:add(csi_flush_queue, Host, ?MODULE,
			      flush_queue, 50);
       true -> ok
    end.

stop(Host) ->
    QueuePresence = gen_mod:get_module_opt(Host, ?MODULE, queue_presence,
					   fun(B) when is_boolean(B) -> B end,
					   true),
    DropChatStates = gen_mod:get_module_opt(Host, ?MODULE, drop_chat_states,
					    fun(B) when is_boolean(B) -> B end,
					    true),
    if QueuePresence; DropChatStates ->
	   ejabberd_hooks:delete(c2s_post_auth_features, Host, ?MODULE,
				 add_stream_feature, 50),
	   if QueuePresence ->
		  ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
					filter_presence, 50);
	      true -> ok
	   end,
	   if DropChatStates ->
		  ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
					filter_chat_states, 50);
	      true -> ok
	   end,
	   ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
				 filter_other, 100),
	   ejabberd_hooks:delete(csi_flush_queue, Host, ?MODULE,
				 flush_queue, 50);
       true -> ok
    end.

add_stream_feature(Features, _Host) ->
    Feature = #xmlel{name = <<"csi">>,
		     attrs = [{<<"xmlns">>, ?NS_CLIENT_STATE}],
		     children = []},
    [Feature | Features].

filter_presence({C2SState, _OutStanzas} = Acc, Host,
		#xmlel{name = <<"presence">>, attrs = Attrs} = Stanza) ->
    case fxml:get_attr(<<"type">>, Attrs) of
      {value, Type} when Type /= <<"unavailable">> ->
	  Acc;
      _ ->
	  ?DEBUG("Got availability presence stanza", []),
	  queue_add(presence, Stanza, Host, C2SState)
    end;
filter_presence(Acc, _Host, _Stanza) -> Acc.

filter_chat_states({C2SState, _OutStanzas} = Acc, _Host,
		   #xmlel{name = <<"message">>} = Stanza) ->
    case jlib:is_standalone_chat_state(Stanza) of
      true -> % Drop the stanza.
	  ?DEBUG("Got standalone chat state notification", []),
	  {stop, {C2SState, []}};
      false ->
	  Acc
    end;
filter_chat_states(Acc, _Host, _Stanza) -> Acc.

filter_other({C2SState, _OutStanzas}, Host, Stanza) ->
    ?DEBUG("Won't add stanza to CSI queue", []),
    queue_take(Stanza, Host, C2SState).

flush_queue({C2SState, _OutStanzas}, Host) ->
    ?DEBUG("Going to flush CSI queue", []),
    Queue = get_queue(C2SState),
    NewState = set_queue([], C2SState),
    {stop, {NewState, get_stanzas(Queue, Host)}}.

queue_add(Type, Stanza, Host, C2SState) ->
    case get_queue(C2SState) of
      Queue when length(Queue) >= ?CSI_QUEUE_MAX ->
	  ?DEBUG("CSI queue too large, going to flush it", []),
	  NewState = set_queue([], C2SState),
	  {stop, {NewState, get_stanzas(Queue, Host) ++ [Stanza]}};
      Queue ->
	  ?DEBUG("Adding stanza to CSI queue", []),
	  From = fxml:get_tag_attr_s(<<"from">>, Stanza),
	  Key = {jid:tolower(jid:from_string(From)), Type},
	  Entry = {Key, p1_time_compat:timestamp(), Stanza},
	  NewQueue = lists:keystore(Key, 1, Queue, Entry),
	  NewState = set_queue(NewQueue, C2SState),
	  {stop, {NewState, []}}
    end.

queue_take(Stanza, Host, C2SState) ->
    From = fxml:get_tag_attr_s(<<"from">>, Stanza),
    {LUser, LServer, _LResource} = jid:tolower(jid:from_string(From)),
    {Selected, Rest} = lists:partition(
			 fun({{{U, S, _R}, _Type}, _Time, _Stanza}) ->
				 U == LUser andalso S == LServer
			 end, get_queue(C2SState)),
    NewState = set_queue(Rest, C2SState),
    {stop, {NewState, get_stanzas(Selected, Host) ++ [Stanza]}}.

set_queue(Queue, C2SState) ->
    ejabberd_c2s:set_aux_field(csi_queue, Queue, C2SState).

get_queue(C2SState) ->
    case ejabberd_c2s:get_aux_field(csi_queue, C2SState) of
      {ok, Queue} ->
	      Queue;
      error ->
	      []
    end.

get_stanzas(Queue, Host) ->
    lists:map(fun({_Key, Time, Stanza}) ->
		      jlib:add_delay_info(Stanza, Host, Time,
					  <<"Client Inactive">>)
	      end, Queue).

mod_opt_type(queue_presence) ->
    fun(B) when is_boolean(B) -> B end;
mod_opt_type(drop_chat_states) ->
    fun(B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [queue_presence, drop_chat_states].
