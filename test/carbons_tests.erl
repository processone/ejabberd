%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 16 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(carbons_tests).

%% API
-compile(export_all).
-import(suite, [is_feature_advertised/2, disconnect/1, send_recv/2,
		recv_presence/1, send/2, get_event/1, recv_message/1,
		my_jid/1, wait_for_slave/1, wait_for_master/1,
		put_event/2]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {carbons_single, [sequence],
     [single_test(feature_enabled),
      single_test(unsupported_iq)]}.

feature_enabled(Config) ->
    true = is_feature_advertised(Config, ?NS_CARBONS_2),
    disconnect(Config).

unsupported_iq(Config) ->
    lists:foreach(
      fun({Type, SubEl}) ->
	      #iq{type = error} =
		  send_recv(Config, #iq{type = Type, sub_els = [SubEl]})
      end, [{Type, SubEl} ||
	       Type <- [get, set],
	       SubEl <- [#carbons_sent{forwarded = #forwarded{}},
			 #carbons_received{forwarded = #forwarded{}},
			 #carbons_private{}]] ++
	  [{get, SubEl} || SubEl <- [#carbons_enable{}, #carbons_disable{}]]),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {carbons_master_slave, [sequence],
     [master_slave_test(send_recv),
      master_slave_test(enable_disable)]}.

send_recv_master(Config) ->
    Peer = ?config(peer, Config),
    prepare_master(Config),
    ct:comment("Waiting for the peer to be ready"),
    ready = get_event(Config),
    send_messages(Config),
    ct:comment("Waiting for the peer to disconnect"),
    #presence{from = Peer, type = unavailable} = recv_presence(Config),
    disconnect(Config).

send_recv_slave(Config) ->
    prepare_slave(Config),
    ok = enable(Config),
    put_event(Config, ready),
    recv_carbons(Config),
    disconnect(Config).

enable_disable_master(Config) ->
    prepare_master(Config),
    ct:comment("Waiting for the peer to be ready"),
    ready = get_event(Config),
    send_messages(Config),
    disconnect(Config).

enable_disable_slave(Config) ->
    Peer = ?config(peer, Config),
    prepare_slave(Config),
    ok = enable(Config),
    ok = disable(Config),
    put_event(Config, ready),
    ct:comment("Waiting for the peer to disconnect"),
    #presence{from = Peer, type = unavailable} = recv_presence(Config),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("carbons_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("carbons_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("carbons_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("carbons_" ++ atom_to_list(T) ++ "_slave")]}.

prepare_master(Config) ->
    MyJID = my_jid(Config),
    Peer = ?config(peer, Config),
    #presence{from = MyJID} = send_recv(Config, #presence{priority = 10}),
    wait_for_slave(Config),
    ct:comment("Receiving initial presence from the peer"),
    #presence{from = Peer} = recv_presence(Config),
    Config.

prepare_slave(Config) ->
    Peer = ?config(peer, Config),
    MyJID = my_jid(Config),
    ok = enable(Config),
    wait_for_master(Config),
    #presence{from = MyJID} = send_recv(Config, #presence{priority = 5}),
    ct:comment("Receiving initial presence from the peer"),
    #presence{from = Peer} = recv_presence(Config),
    Config.

send_messages(Config) ->
    Server = ?config(server, Config),
    MyJID = my_jid(Config),
    JID = jid:make(randoms:get_string(), Server),
    lists:foreach(
      fun({send, #message{type = Type} = Msg}) ->
	      I = send(Config, Msg#message{to = JID}),
	      if Type /= error ->
		      #message{id = I, type = error} = recv_message(Config);
		 true ->
		      ok
	      end;
	 ({recv, #message{} = Msg}) ->
	      ejabberd_router:route(
		Msg#message{from = JID, to = MyJID}),
	      ct:comment("Receiving message ~s", [xmpp:pp(Msg)]),
	      #message{} = recv_message(Config)
      end, message_iterator(Config)).

recv_carbons(Config) ->
    Peer = ?config(peer, Config),
    BarePeer = jid:remove_resource(Peer),
    MyJID = my_jid(Config),
    lists:foreach(
      fun({_, #message{sub_els = [#hint{type = 'no-copy'}]}}) ->
	      ok;
	 ({_, #message{sub_els = [#carbons_private{}]}}) ->
	      ok;
	 ({_, #message{type = T}}) when T /= normal, T /= chat ->
	      ok;
	 ({Dir, #message{type = T, body = Body} = M})
	    when (T == chat) or (T == normal andalso Body /= []) ->
	      ct:comment("Receiving carbon ~s", [xmpp:pp(M)]),
	      #message{from = BarePeer, to = MyJID} = CarbonMsg =
		  recv_message(Config),
	      case Dir of
		  send ->
		      #carbons_sent{forwarded = #forwarded{xml_els = [El]}} =
			  xmpp:get_subtag(CarbonMsg, #carbons_sent{}),
		      #message{body = Body} = xmpp:decode(El);
		  recv ->
		      #carbons_received{forwarded = #forwarded{xml_els = [El]}}=
			  xmpp:get_subtag(CarbonMsg, #carbons_received{}),
		      #message{body = Body} = xmpp:decode(El)
	      end;
	 (_) ->
	      false
      end, message_iterator(Config)).

enable(Config) ->
    case send_recv(
	   Config, #iq{type = set,
		       sub_els = [#carbons_enable{}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

disable(Config) ->
    case send_recv(
	   Config, #iq{type = set,
		       sub_els = [#carbons_disable{}]}) of
	#iq{type = result, sub_els = []} ->
	    ok;
	#iq{type = error} = Err ->
	    xmpp:get_error(Err)
    end.

message_iterator(_Config) ->
    [{Dir, #message{type = Type, body = Body, sub_els = Els}}
     || Dir <- [send, recv],
	Type <- [error, chat, normal, groupchat, headline],
    	Body <- [[], xmpp:mk_text(<<"body">>)],
    	Els <- [[],
    		[#hint{type = 'no-copy'}],
		[#carbons_private{}]]].
