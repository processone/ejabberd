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

-module(sm_tests).

%% API
-compile(export_all).
-import(suite, [send/2, recv/1, close_socket/1, set_opt/3, my_jid/1,
		recv_message/1, disconnect/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {sm_single, [sequence],
     [single_test(feature_enabled),
      single_test(enable),
      single_test(resume),
      single_test(resume_failed)]}.

feature_enabled(Config) ->
    true = ?config(sm, Config),
    disconnect(Config).

enable(Config) ->
    Server = ?config(server, Config),
    ServerJID = jid:make(<<"">>, Server, <<"">>),
    %% Send messages of type 'headline' so the server discards them silently
    Msg = #message{to = ServerJID, type = headline,
		   body = [#text{data = <<"body">>}]},
    %% Enable the session management with resumption enabled
    send(Config, #sm_enable{resume = true, xmlns = ?NS_STREAM_MGMT_3}),
    #sm_enabled{id = ID, resume = true} = recv(Config),
    %% Initial request; 'h' should be 0.
    send(Config, #sm_r{xmlns = ?NS_STREAM_MGMT_3}),
    #sm_a{h = 0} = recv(Config),
    %% sending two messages and requesting again; 'h' should be 3.
    send(Config, Msg),
    send(Config, Msg),
    send(Config, Msg),
    send(Config, #sm_r{xmlns = ?NS_STREAM_MGMT_3}),
    #sm_a{h = 3} = recv(Config),
    close_socket(Config),
    {save_config, set_opt(sm_previd, ID, Config)}.

resume(Config) ->
    {_, SMConfig} = ?config(saved_config, Config),
    ID = ?config(sm_previd, SMConfig),
    Server = ?config(server, Config),
    ServerJID = jid:make(<<"">>, Server, <<"">>),
    MyJID = my_jid(Config),
    Txt = #text{data = <<"body">>},
    Msg = #message{from = ServerJID, to = MyJID, body = [Txt]},
    %% Route message. The message should be queued by the C2S process.
    ejabberd_router:route(ServerJID, MyJID, Msg),
    send(Config, #sm_resume{previd = ID, h = 0, xmlns = ?NS_STREAM_MGMT_3}),
    #sm_resumed{previd = ID, h = 3} = recv(Config),
    #message{from = ServerJID, to = MyJID, body = [Txt]} = recv_message(Config),
    #sm_r{} = recv(Config),
    send(Config, #sm_a{h = 1, xmlns = ?NS_STREAM_MGMT_3}),
    %% Send another stanza to increment the server's 'h' for sm_resume_failed.
    send(Config, #presence{to = ServerJID}),
    close_socket(Config),
    {save_config, set_opt(sm_previd, ID, Config)}.

resume_failed(Config) ->
    {_, SMConfig} = ?config(saved_config, Config),
    ID = ?config(sm_previd, SMConfig),
    ct:sleep(5000), % Wait for session to time out.
    send(Config, #sm_resume{previd = ID, h = 1, xmlns = ?NS_STREAM_MGMT_3}),
    #sm_failed{reason = 'item-not-found', h = 4} = recv(Config),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {sm_master_slave, [sequence], []}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("sm_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("sm_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("sm_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("sm_" ++ atom_to_list(T) ++ "_slave")]}.
