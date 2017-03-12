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

-module(proxy65_tests).

%% API
-compile(export_all).
-import(suite, [disconnect/1, is_feature_advertised/3, proxy_jid/1,
		my_jid/1, wait_for_slave/1, wait_for_master/1,
		send_recv/2, put_event/2, get_event/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {proxy65_single, [sequence],
     [single_test(feature_enabled),
      single_test(service_vcard)]}.

feature_enabled(Config) ->
    true = is_feature_advertised(Config, ?NS_BYTESTREAMS, proxy_jid(Config)),
    disconnect(Config).

service_vcard(Config) ->
    JID = proxy_jid(Config),
    ct:comment("Retreiving vCard from ~s", [jid:encode(JID)]),
    #iq{type = result, sub_els = [#vcard_temp{}]} =
	send_recv(Config, #iq{type = get, to = JID, sub_els = [#vcard_temp{}]}),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {proxy65_master_slave, [sequence],
     [master_slave_test(all)]}.

all_master(Config) ->
    Proxy = proxy_jid(Config),
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    wait_for_slave(Config),
    #presence{} = send_recv(Config, #presence{}),
    #iq{type = result, sub_els = [#bytestreams{hosts = [StreamHost]}]} =
        send_recv(
          Config,
          #iq{type = get, sub_els = [#bytestreams{}], to = Proxy}),
    SID = randoms:get_string(),
    Data = randoms:bytes(1024),
    put_event(Config, {StreamHost, SID, Data}),
    Socks5 = socks5_connect(StreamHost, {SID, MyJID, Peer}),
    wait_for_slave(Config),
    #iq{type = result, sub_els = []} =
        send_recv(Config,
                  #iq{type = set, to = Proxy,
                      sub_els = [#bytestreams{activate = Peer, sid = SID}]}),
    socks5_send(Socks5, Data),
    disconnect(Config).

all_slave(Config) ->
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    {StreamHost, SID, Data} = get_event(Config),
    Socks5 = socks5_connect(StreamHost, {SID, Peer, MyJID}),
    wait_for_master(Config),
    socks5_recv(Socks5, Data),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("proxy65_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("proxy65_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("proxy65_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("proxy65_" ++ atom_to_list(T) ++ "_slave")]}.

socks5_connect(#streamhost{host = Host, port = Port},
               {SID, JID1, JID2}) ->
    Hash = p1_sha:sha([SID, jid:encode(JID1), jid:encode(JID2)]),
    {ok, Sock} = gen_tcp:connect(binary_to_list(Host), Port,
                                 [binary, {active, false}]),
    Init = <<?VERSION_5, 1, ?AUTH_ANONYMOUS>>,
    InitAck = <<?VERSION_5, ?AUTH_ANONYMOUS>>,
    Req = <<?VERSION_5, ?CMD_CONNECT, 0,
            ?ATYP_DOMAINNAME, 40, Hash:40/binary, 0, 0>>,
    Resp = <<?VERSION_5, ?SUCCESS, 0, ?ATYP_DOMAINNAME,
             40, Hash:40/binary, 0, 0>>,
    gen_tcp:send(Sock, Init),
    {ok, InitAck} = gen_tcp:recv(Sock, size(InitAck)),
    gen_tcp:send(Sock, Req),
    {ok, Resp} = gen_tcp:recv(Sock, size(Resp)),
    Sock.

socks5_send(Sock, Data) ->
    ok = gen_tcp:send(Sock, Data).

socks5_recv(Sock, Data) ->
    {ok, Data} = gen_tcp:recv(Sock, size(Data)).
