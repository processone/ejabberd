%%%----------------------------------------------------------------------
%%% File    : mod_client_state.erl
%%% Author  : Holger Weiss
%%% Purpose : Filter stanzas sent to inactive clients (XEP-0352)
%%% Created : 11 Sep 2014 by Holger Weiss
%%%
%%%
%%% ejabberd, Copyright (C) 2014-2015   ProcessOne
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

-behavior(gen_mod).

-export([start/2, stop/1, add_stream_feature/2, filter_presence/2,
	 filter_chat_states/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    QueuePresence = gen_mod:get_opt(queue_presence, Opts,
				    fun(true) -> true;
				       (false) -> false
				    end, false),
    DropChatStates = gen_mod:get_opt(drop_chat_states, Opts,
				     fun(true) -> true;
				        (false) -> false
				     end, false),
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
	   end;
       true -> ok
    end,
    ok.

stop(Host) ->
    ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
			  filter_presence, 50),
    ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
			  filter_chat_states, 50),
    ejabberd_hooks:delete(c2s_post_auth_features, Host, ?MODULE,
			  add_stream_feature, 50),
    ok.

add_stream_feature(Features, _Host) ->
    Feature = #xmlel{name = <<"csi">>,
		     attrs = [{<<"xmlns">>, ?NS_CLIENT_STATE}],
		     children = []},
    [Feature | Features].

filter_presence(_Action, #xmlel{name = <<"presence">>, attrs = Attrs}) ->
    case xml:get_attr(<<"type">>, Attrs) of
      {value, Type} when Type /= <<"unavailable">> ->
	  ?DEBUG("Got important presence stanza", []),
	  {stop, send};
      _ ->
	  ?DEBUG("Got availability presence stanza", []),
	  {stop, queue}
    end;
filter_presence(Action, _Stanza) -> Action.

filter_chat_states(_Action, #xmlel{name = <<"message">>} = Stanza) ->
    %% All XEP-0085 chat states except for <gone/>:
    ChatStates = [<<"active">>, <<"inactive">>, <<"composing">>, <<"paused">>],
    Stripped =
	lists:foldl(fun(ChatState, AccStanza) ->
			    xml:remove_subtags(AccStanza, ChatState,
					       {<<"xmlns">>, ?NS_CHATSTATES})
		    end, Stanza, ChatStates),
    case Stripped of
      #xmlel{children = [#xmlel{name = <<"thread">>}]} ->
	  ?DEBUG("Got standalone chat state notification", []),
	  {stop, drop};
      #xmlel{children = []} ->
	  ?DEBUG("Got standalone chat state notification", []),
	  {stop, drop};
      _ ->
	  ?DEBUG("Got message with chat state notification", []),
	  {stop, send}
    end;
filter_chat_states(Action, _Stanza) -> Action.
