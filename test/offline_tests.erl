%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  7 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%----------------------------------------------------------------------

-module(offline_tests).

%% API
-compile(export_all).
-import(suite, [send/2, disconnect/1, my_jid/1, send_recv/2, recv_message/1,
		get_features/1, recv/1, get_event/1, server_jid/1,
		wait_for_master/1, wait_for_slave/1]).
-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
single_cases() ->
    {offline_single, [sequence],
     [single_test(feature_enabled),
      single_test(check_identity),
      single_test(send_non_existent),
      single_test(view_non_existent),
      single_test(remove_non_existent),
      single_test(view_non_integer),
      single_test(remove_non_integer),
      single_test(malformed_iq),
      single_test(wrong_user),
      single_test(unsupported_iq)]}.

feature_enabled(Config) ->
    Features = get_features(Config),
    ct:comment("Checking if offline features are set"),
    true = lists:member(?NS_FEATURE_MSGOFFLINE, Features),
    true = lists:member(?NS_FLEX_OFFLINE, Features),
    disconnect(Config).

check_identity(Config) ->
    #iq{type = result,
	sub_els = [#disco_info{
		      node = ?NS_FLEX_OFFLINE,
		      identities = Ids}]} =
	send_recv(Config, #iq{type = get,
			      sub_els = [#disco_info{
					    node = ?NS_FLEX_OFFLINE}]}),
    true = lists:any(
	     fun(#identity{category = <<"automation">>,
			   type = <<"message-list">>}) -> true;
		(_) -> false
	     end, Ids),
    disconnect(Config).

send_non_existent(Config) ->
    Server = ?config(server, Config),
    To = jid:make(<<"non-existent">>, Server),
    #message{type = error} = Err = send_recv(Config, #message{to = To}),
    #stanza_error{reason = 'service-unavailable'} = xmpp:get_error(Err),
    disconnect(Config).

view_non_existent(Config) ->
    #stanza_error{reason = 'item-not-found'} = view(Config, [randoms:get_string()], false),
    disconnect(Config).

remove_non_existent(Config) ->
    ok = remove(Config, [randoms:get_string()]),
    disconnect(Config).

view_non_integer(Config) ->
    #stanza_error{reason = 'item-not-found'} = view(Config, [<<"foo">>], false),
    disconnect(Config).

remove_non_integer(Config) ->
    #stanza_error{reason = 'item-not-found'} = remove(Config, [<<"foo">>]),
    disconnect(Config).

malformed_iq(Config) ->
    Item = #offline_item{node = randoms:get_string()},
    Range = [{Type, SubEl} || Type <- [set, get],
			      SubEl <- [#offline{items = [], _ = false},
					#offline{items = [Item], _ = true}]]
	++ [{set, #offline{items = [], fetch = true, purge = false}},
	    {set, #offline{items = [Item], fetch = true, purge = false}},
	    {get, #offline{items = [], fetch = false, purge = true}},
	    {get, #offline{items = [Item], fetch = false, purge = true}}],
    lists:foreach(
      fun({Type, SubEl}) ->
	      #iq{type = error} = Err =
		  send_recv(Config, #iq{type = Type, sub_els = [SubEl]}),
	      #stanza_error{reason = 'bad-request'} = xmpp:get_error(Err)
      end, Range),
    disconnect(Config).

wrong_user(Config) ->
    Server = ?config(server, Config),
    To = jid:make(<<"foo">>, Server),
    Item = #offline_item{node = randoms:get_string()},
    Range = [{Type, Items, Purge, Fetch} ||
		Type <- [set, get],
		Items <- [[], [Item]],
		Purge <- [false, true],
		Fetch <- [false, true]],
    lists:foreach(
      fun({Type, Items, Purge, Fetch}) ->
	      #iq{type = error} = Err =
		  send_recv(Config, #iq{type = Type, to = To,
					sub_els = [#offline{items = Items,
							    purge = Purge,
							    fetch = Fetch}]}),
	      #stanza_error{reason = 'forbidden'} = xmpp:get_error(Err)
      end, Range),
    disconnect(Config).

unsupported_iq(Config) ->
    Item = #offline_item{node = randoms:get_string()},
    lists:foreach(
      fun(Type) ->
	      #iq{type = error} = Err =
		  send_recv(Config, #iq{type = Type, sub_els = [Item]}),
	      #stanza_error{reason = 'service-unavailable'} = xmpp:get_error(Err)
      end, [set, get]),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {offline_master_slave, [sequence],
     [master_slave_test(flex),
      master_slave_test(send_all)]}.

flex_master(Config) ->
    send_messages(Config, 5),
    disconnect(Config).

flex_slave(Config) ->
    wait_for_master(Config),
    peer_down = get_event(Config),
    5 = get_number(Config),
    Nodes = get_nodes(Config),
    %% Since headers are received we can send initial presence without a risk
    %% of getting offline messages flood
    #presence{} = send_recv(Config, #presence{}),
    ct:comment("Checking fetch"),
    Nodes = fetch(Config, lists:seq(1, 5)),
    ct:comment("Fetching 2nd and 4th message"),
    [2, 4] = view(Config, [lists:nth(2, Nodes), lists:nth(4, Nodes)]),
    ct:comment("Deleting 2nd and 4th message"),
    ok = remove(Config, [lists:nth(2, Nodes), lists:nth(4, Nodes)]),
    ct:comment("Checking if messages were deleted"),
    [1, 3, 5] = view(Config, [lists:nth(1, Nodes),
			      lists:nth(3, Nodes),
			      lists:nth(5, Nodes)]),
    ct:comment("Purging everything left"),
    ok = purge(Config),
    ct:comment("Checking if there are no offline messages"),
    0 = get_number(Config),
    clean(disconnect(Config)).

send_all_master(Config) ->
    wait_for_slave(Config),
    Peer = ?config(peer, Config),
    BarePeer = jid:remove_resource(Peer),
    {Deliver, Errors} = message_iterator(Config),
    N = lists:foldl(
    	  fun(#message{type = error} = Msg, Acc) ->
    		  send(Config, Msg#message{to = BarePeer}),
    		  Acc;
    	     (Msg, Acc) ->
    		  I = send(Config, Msg#message{to = BarePeer}),
    		  case xmpp:get_subtag(Msg, #xevent{}) of
    		      #xevent{offline = true, id = undefined} ->
    			  ct:comment("Receiving event-reply for:~n~s",
    				     [xmpp:pp(Msg)]),
    			  #message{} = Reply = recv_message(Config),
    			  #xevent{id = I} = xmpp:get_subtag(Reply, #xevent{});
    		      _ ->
    			  ok
    		  end,
    		  Acc + 1
    	  end, 0, Deliver),
    lists:foreach(
      fun(#message{type = headline} = Msg) ->
	      send(Config, Msg#message{to = BarePeer});
         (Msg) ->
	      #message{type = error} = Err =
		  send_recv(Config, Msg#message{to = BarePeer}),
	      #stanza_error{reason = 'service-unavailable'} = xmpp:get_error(Err)
      end, Errors),
    ok = wait_for_complete(Config, N),
    disconnect(Config).

send_all_slave(Config) ->
    ServerJID = server_jid(Config),
    Peer = ?config(peer, Config),
    wait_for_master(Config),
    peer_down = get_event(Config),
    #presence{} = send_recv(Config, #presence{}),
    {Deliver, _Errors} = message_iterator(Config),
    lists:foreach(
      fun(#message{type = error}) ->
	      ok;
	 (#message{type = Type, body = Body, subject = Subject} = Msg) ->
	      ct:comment("Receiving message:~n~s", [xmpp:pp(Msg)]),
	      #message{from = Peer,
		       type = Type,
		       body = Body,
		       subject = Subject} = RecvMsg = recv_message(Config),
	      ct:comment("Checking if delay tag is correctly set"),
	      #delay{from = ServerJID} = xmpp:get_subtag(RecvMsg, #delay{})
      end, Deliver),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("offline_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("offline_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("offline_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("offline_" ++ atom_to_list(T) ++ "_slave")]}.

clean(Config) ->
    {U, S, _} = jid:tolower(my_jid(Config)),
    mod_offline:remove_user(U, S),
    Config.

send_messages(Config, Num) ->
    send_messages(Config, Num, normal, []).

send_messages(Config, Num, Type, SubEls) ->
    wait_for_slave(Config),
    Peer = ?config(peer, Config),
    BarePeer = jid:remove_resource(Peer),
    lists:foreach(
      fun(I) ->
	      Body = integer_to_binary(I),
	      send(Config,
		   #message{to = BarePeer,
			    type = Type,
			    body = [#text{data = Body}],
			    subject = [#text{data = <<"subject">>}],
			    sub_els = SubEls})
      end, lists:seq(1, Num)),
    ct:comment("Waiting for all messages to be delivered to offline spool"),
    ok = wait_for_complete(Config, Num).

recv_messages(Config, Num) ->
    wait_for_master(Config),
    peer_down = get_event(Config),
    Peer = ?config(peer, Config),
    #presence{} = send_recv(Config, #presence{}),
    lists:foreach(
      fun(I) ->
	      Text = integer_to_binary(I),
	      #message{sub_els = SubEls,
		       from = Peer,
		       body = [#text{data = Text}],
		       subject = [#text{data = <<"subject">>}]} =
		  recv_message(Config),
	      true = lists:keymember(delay, 1, SubEls)
      end, lists:seq(1, Num)),
    clean(disconnect(Config)).

get_number(Config) ->
    ct:comment("Getting offline message number"),
    #iq{type = result,
	sub_els = [#disco_info{
		      node = ?NS_FLEX_OFFLINE,
		      xdata = [X]}]} =
	send_recv(Config, #iq{type = get,
			      sub_els = [#disco_info{
					    node = ?NS_FLEX_OFFLINE}]}),
    Form = flex_offline:decode(X#xdata.fields),
    proplists:get_value(number_of_messages, Form).

get_nodes(Config) ->
    MyJID = my_jid(Config),
    MyBareJID = jid:remove_resource(MyJID),
    Peer = ?config(peer, Config),
    Peer_s = jid:encode(Peer),
    ct:comment("Getting headers"), 
    #iq{type = result,
	sub_els = [#disco_items{
		      node = ?NS_FLEX_OFFLINE,
		      items = DiscoItems}]} =
	send_recv(Config, #iq{type = get,
			      sub_els = [#disco_items{
					    node = ?NS_FLEX_OFFLINE}]}),
    ct:comment("Checking if headers are correct"),
    lists:sort(
      lists:map(
	fun(#disco_item{jid = J, name = P, node = N})
	      when (J == MyBareJID) and (P == Peer_s) ->
		N
	end, DiscoItems)).

fetch(Config, Range) ->
    ID = send(Config, #iq{type = get, sub_els = [#offline{fetch = true}]}),
    Nodes = lists:map(
	      fun(I) ->
		      Text = integer_to_binary(I),
		      #message{body = Body, sub_els = SubEls} = recv(Config),
		      [#text{data = Text}] = Body,
		      #offline{items = [#offline_item{node = Node}]} =
			  lists:keyfind(offline, 1, SubEls),
		      #delay{} = lists:keyfind(delay, 1, SubEls),
		      Node
	      end, Range),
    #iq{id = ID, type = result, sub_els = []} = recv(Config),
    Nodes.

view(Config, Nodes) ->
    view(Config, Nodes, true).

view(Config, Nodes, NeedReceive) ->
    Items = lists:map(
	      fun(Node) ->
		      #offline_item{action = view, node = Node}
	      end, Nodes),
    I = send(Config,
	     #iq{type = get, sub_els = [#offline{items = Items}]}),
    Range = if NeedReceive ->
		    lists:map(
		      fun(Node) ->
			      #message{body = [#text{data = Text}],
				       sub_els = SubEls} = recv(Config),
			      #offline{items = [#offline_item{node = Node}]} =
				  lists:keyfind(offline, 1, SubEls),
			      binary_to_integer(Text)
		      end, Nodes);
	       true ->
		    []
	    end,
    case recv(Config) of
	#iq{id = I, type = result, sub_els = []} -> Range;
	#iq{id = I, type = error} = Err -> xmpp:get_error(Err)
    end.

remove(Config, Nodes) ->
    Items = lists:map(
	      fun(Node) ->
		      #offline_item{action = remove, node = Node}
	      end, Nodes),
    case send_recv(Config, #iq{type = set,
			       sub_els = [#offline{items = Items}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

purge(Config) ->
    case send_recv(Config, #iq{type = set,
			       sub_els = [#offline{purge = true}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

wait_for_complete(_Config, 0) ->
    ok;
wait_for_complete(Config, N) ->
    {U, S, _} = jid:tolower(?config(peer, Config)),
    lists:foldl(
      fun(_Time, ok) ->
	      ok;
	 (Time, Acc) ->
	      timer:sleep(Time),
	      case mod_offline:count_offline_messages(U, S) of
		  N -> ok;
		  _ -> Acc
	      end
      end, error, [0, 100, 200, 2000, 5000, 10000]).

message_iterator(Config) ->
    ServerJID = server_jid(Config),
    ChatStates = [[#chatstate{type = composing}]],
    Offline = [[#offline{}]],
    Hints = [[#hint{type = T}] || T <- [store, 'no-store']],
    XEvent = [[#xevent{id = ID, offline = OfflineFlag}]
	      || ID <- [undefined, randoms:get_string()],
		 OfflineFlag <- [false, true]],
    Delay = [[#delay{stamp = p1_time_compat:timestamp(), from = ServerJID}]],
    AllEls = [Els1 ++ Els2 || Els1 <- [[]] ++ ChatStates ++ Delay ++ Hints ++ Offline,
			      Els2 <- [[]] ++ XEvent],
    All = [#message{type = Type, body = Body, subject = Subject, sub_els = Els}
	   || %%Type <- [chat],
	      Type <- [error, chat, normal, groupchat, headline],
	      Body <- [[], xmpp:mk_text(<<"body">>)],
	      Subject <- [[], xmpp:mk_text(<<"subject">>)],
	      Els <- AllEls],
    lists:partition(
      fun(#message{type = error}) -> true;
	 (#message{type = groupchat}) -> false;
	 (#message{sub_els = [#offline{}|_]}) -> false;
	 (#message{sub_els = [_, #xevent{id = I}]}) when I /= undefined -> false;
	 (#message{sub_els = [#xevent{id = I}]}) when I /= undefined -> false;
	 (#message{sub_els = [#hint{type = store}|_]}) -> true;
	 (#message{sub_els = [#hint{type = 'no-store'}|_]}) -> false;
	 (#message{body = [], subject = []}) -> false;
	 (#message{type = Type}) -> (Type == chat) or (Type == normal);
	 (_) -> false
      end, All).
