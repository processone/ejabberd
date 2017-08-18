%%%-------------------------------------------------------------------
%%% Author  : Anna Mukharram
%%%-------------------------------------------------------------------

-module(sms2s_tests).

%% API
-compile(export_all).

-import(suite, [connect/1, send/2, recv/1, set_opt/3,
				close_socket/1, disconnect/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================

single_cases() ->
    {sms2s_single, [sequence],
     [single_test(enable),
      single_test(resume),
      single_test(resume_failed)]}.

enable(Config) ->
  Server = ?config(server, Config),
  ServerJID = jid:make(<<"">>, Server, <<"">>),
  From = ?config(stream_from, Config),
  FromJID = jid:make(<<"">>, From, <<"">>),
  Msg = #message{from = FromJID, to = ServerJID, type = headline,
                 body = [#text{data = <<"body">>}]},
  ct:comment("Stream management with resumption is enabled"),
  send(Config, #sm_enable{resume = true, xmlns = ?NS_STREAM_MGMT_3}),
  #sm_enabled{id = ID, resume = true} = recv(Config),
  ct:comment("Initial request; 'h' should be 0"),
  send(Config, #sm_r{xmlns = ?NS_STREAM_MGMT_3}),
  #sm_a{h = 0} = recv(Config),
  ct:comment("Sending three messages and requesting again; 'h' should be 3"),
  send(Config, Msg),
  send(Config, Msg),
  send(Config, Msg),
  send(Config, #sm_r{xmlns = ?NS_STREAM_MGMT_3}),
  #sm_a{h = 3} = recv(Config),
  ct:comment("Closing socket"),
  close_socket(Config),
  {save_config, set_opt(sm_previd, ID, Config)}.

resume(Config) ->
  {_, SMConfig} = ?config(saved_config, Config),
  ID = ?config(sm_previd, SMConfig),
  ct:comment("Resuming the session"),
  send(Config, #sm_resume{previd = ID, h = 0, xmlns = ?NS_STREAM_MGMT_3}),
  #sm_resumed{previd = ID, h = 3} = recv(Config),
  ct:comment("Checking if the server counts stanzas correctly"),
  Server = ?config(server, Config),
  ServerJID = jid:make(<<"">>, Server, <<"">>),
  From = ?config(stream_from, Config),
  FromJID = jid:make(<<"">>, From, <<"">>),
  Msg = #message{from = FromJID, to = ServerJID, type = headline,
                 body = [#text{data = <<"body">>}]},
  send(Config, Msg),
  send(Config, #sm_r{xmlns = ?NS_STREAM_MGMT_3}),
  #sm_a{h = 4} = recv(Config),
  ct:comment("Closing socket"),
  close_socket(Config),
  {save_config, set_opt(sm_previd, ID, Config)}.

resume_failed(Config) ->
  {_, SMConfig} = ?config(saved_config, Config),
  ID = ?config(sm_previd, SMConfig),
  ct:comment("Waiting for the session to time out"),
  ct:sleep(30000),
  ct:comment("Trying to resume timed out session"),
  send(Config, #sm_resume{previd = ID, h = 0, xmlns = ?NS_STREAM_MGMT_3}),
  #sm_failed{reason = 'item-not-found'} = recv(Config),
  disconnect(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("sms2s_" ++ atom_to_list(T)).


