%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 16 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(announce_tests).

%% API
-compile(export_all).
-import(suite, [server_jid/1, send_recv/2, recv_message/1, disconnect/1,
		send/2, wait_for_master/1, wait_for_slave/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {announce_single, [sequence], []}.

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {announce_master_slave, [sequence],
     [master_slave_test(set_motd)]}.

set_motd_master(Config) ->
    ServerJID = server_jid(Config),
    MotdJID = jid:replace_resource(ServerJID, <<"announce/motd">>),
    Body = xmpp:mk_text(<<"motd">>),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_slave(Config),
    send(Config, #message{to = MotdJID, body = Body}),
    #message{from = ServerJID, body = Body} = recv_message(Config),
    disconnect(Config).

set_motd_slave(Config) ->
    ServerJID = server_jid(Config),
    Body = xmpp:mk_text(<<"motd">>),
    #presence{} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    #message{from = ServerJID, body = Body} = recv_message(Config),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("announce_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("announce_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("announce_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("announce_" ++ atom_to_list(T) ++ "_slave")]}.
