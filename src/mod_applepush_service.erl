%%%----------------------------------------------------------------------
%%% File    : mod_applepush_service.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Central push infrastructure 
%%% Created :  5 Jun 2009 by Alexey Shchepin <alexey@process-one.net>
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
%%%----------------------------------------------------------------------

-module(mod_applepush_service).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {host = <<"">>             :: binary(),
		socket                    :: ssl:sslsocket(),
		gateway = ""              :: string(),
		port = 2195               :: inet:port_number(),
		feedback_socket           :: ssl:sslsocket(),
		feedback                  :: string(),
		feedback_port = 2196      :: inet:port_number(),
		feedback_buf = <<>>       :: binary(),
		certfile = ""             :: string(),
		certfile_mtime            :: file:date_time(),
		failure_script            :: string(),
		queue = {0, queue:new()}  :: {non_neg_integer(), queue()},
		soundfile = <<"">>        :: binary(),
		cmd_id = 0                :: non_neg_integer(),
		cmd_cache = dict:new()    :: dict(),
		device_cache = dict:new() :: dict()}).

-define(PROCNAME, ejabberd_mod_applepush_service).
-define(RECONNECT_TIMEOUT, 5000).
-define(FEEDBACK_RECONNECT_TIMEOUT, 30000).
-define(HANDSHAKE_TIMEOUT, 60000).
-define(SSL_TIMEOUT, 5000).
-define(MAX_QUEUE_SIZE, 1000).
-define(CACHE_SIZE, 4096).
-define(MAX_PAYLOAD_SIZE, 255).

-define(NS_P1_PUSH, <<"p1:push">>).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    ssl:start(),
    MyHosts = case catch gen_mod:get_opt(
                           hosts, Opts,
                           fun(L) when is_list(L) ->
                                   [{iolist_to_binary(H), O} || {H, O}<-L]
                           end, []) of
                  {'EXIT', _} ->
                      [{gen_mod:get_opt_host(Host, Opts,
                                             <<"applepush.@HOST@">>), Opts}];
                  Hs ->
                      Hs
              end,
    lists:foreach(
      fun({MyHost, MyOpts}) ->
	      Proc = gen_mod:get_module_proc(MyHost, ?PROCNAME),
	      ChildSpec =
		  {Proc,
		   {?MODULE, start_link, [MyHost, MyOpts]},
		   transient,
		   1000,
		   worker,
		   [?MODULE]},
	      supervisor:start_child(ejabberd_sup, ChildSpec)
      end, MyHosts).

stop(Host) ->
    MyHosts = case gen_mod:get_module_opt(
                     Host, ?MODULE, hosts,
                     fun(Hs) when is_list(Hs) ->
                             [iolist_to_binary(H) || {H, _} <- Hs]
                     end, []) of
                  [] ->
                      [gen_mod:get_module_opt_host(
                         Host, ?MODULE, <<"applepush.@HOST@">>)];
                  Hs ->
                      Hs
              end,
    lists:foreach(
      fun(MyHost) ->
	      Proc = gen_mod:get_module_proc(MyHost, ?PROCNAME),
	      gen_server:call(Proc, stop),
	      supervisor:terminate_child(ejabberd_sup, Proc),
	      supervisor:delete_child(ejabberd_sup, Proc)
      end, MyHosts).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([MyHost, Opts]) ->
    CertFile = gen_mod:get_opt(certfile, Opts,
                               fun iolist_to_string/1,
                               ""),
    SoundFile = gen_mod:get_opt(sound_file, Opts,
                                fun iolist_to_binary/1,
                                <<"pushalert.wav">>),
    Gateway = gen_mod:get_opt(gateway, Opts,
                              fun iolist_to_string/1,
                              "gateway.push.apple.com"),
    Feedback = gen_mod:get_opt(feedback, Opts,
                               fun iolist_to_string/1,
                               undefined),
    Port = gen_mod:get_opt(port, Opts,
                           fun(I) when is_integer(I), I>0, I<65536 -> I end,
                           2195),
    FeedbackPort = gen_mod:get_opt(feedback_port, Opts,
                                   fun(I) when is_integer(I), I>0, I<65536 -> I end,
                                   2196),
    FailureScript = gen_mod:get_opt(failure_script, Opts,
                                    fun iolist_to_string/1,
                                    undefined),
    self() ! connect,
    case Feedback of
	undefined ->
	    ok;
	_ ->
	    self() ! connect_feedback
    end,
    ejabberd_router:register_route(MyHost),
    {ok, #state{host = MyHost,
		gateway = Gateway,
		port = Port,
		feedback = Feedback,
		feedback_port = FeedbackPort,
		certfile = CertFile,
		failure_script = FailureScript,
		queue = {0, queue:new()},
		soundfile = SoundFile}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet, State) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    {noreply, State};
	Res ->
	    Res
    end;
handle_info(connect, State) ->
    connect(State);
handle_info(connect_feedback, #state{certfile_mtime = MTime} = State)
  when MTime /= undefined ->
    erlang:send_after(?FEEDBACK_RECONNECT_TIMEOUT, self(),
		      connect_feedback),
    {noreply, State};
handle_info(connect_feedback, State)
  when State#state.feedback /= undefined,
       State#state.feedback_socket == undefined ->
    Feedback = State#state.feedback,
    FeedbackPort = State#state.feedback_port,
    CertFile = State#state.certfile,
    case ssl:connect(Feedback, FeedbackPort,
		     [{certfile, CertFile},
		      {active, true},
                      binary], ?SSL_TIMEOUT) of
	{ok, Socket} ->
	    ?INFO_MSG("(~p) Connected to ~p:~p",
                      [State#state.host, Feedback, FeedbackPort]),
	    {noreply, State#state{feedback_socket = Socket}};
	{error, Reason} ->
	    ?ERROR_MSG("(~p) Connection to ~p:~p failed: ~p, "
		       "retrying after ~p seconds",
		       [State#state.host, Feedback, FeedbackPort,
			Reason, ?FEEDBACK_RECONNECT_TIMEOUT div 1000]),
	    erlang:send_after(?FEEDBACK_RECONNECT_TIMEOUT, self(),
			      connect_feedback),
	    {noreply, State}
    end;
handle_info({ssl, Socket, Packet}, State)
  when Socket == State#state.socket ->
    case Packet of
	<<8, Status, CmdID:32>> when Status /= 0 ->
	    case dict:find(CmdID, State#state.cmd_cache) of
		{ok, {JID, _DeviceID}} ->
                    ?ERROR_MSG("PUSH ERROR for ~p: ~p", [JID, Status]),
		    if
			Status == 8 ->
			    From = jlib:make_jid(<<"">>, State#state.host, <<"">>),
			    ejabberd_router:route(
			      From, JID,
			      #xmlel{name = <<"message">>, attrs = [],
                                     children =
                                     [#xmlel{name = <<"disable">>,
                                             attrs = [{<<"xmlns">>, ?NS_P1_PUSH},
                                                      {<<"status">>,
                                                       jlib:integer_to_binary(Status)}],
                                             children = []}]});
			true ->
			    ok
		    end,
                    ok;
		error ->
		    ?ERROR_MSG("Unknown cmd ID ~p~n", [CmdID]),
		    ok
	    end;
	_ ->
	    ?ERROR_MSG("Received unknown packet ~p~n", [Packet])
    end,
    {noreply, State};
handle_info({ssl, Socket, Packet}, State)
  when Socket == State#state.feedback_socket ->
    ?INFO_MSG("(~p) feedback: ~p", [State#state.host, Packet]),
    Buf = <<(State#state.feedback_buf)/binary, Packet/binary>>,
    Buf2 = parse_feedback_buf(Buf, State),
    {noreply, State#state{feedback_buf = Buf2}};
handle_info({ssl_closed, Socket}, State)
  when Socket == State#state.feedback_socket ->
    ssl:close(Socket),
    erlang:send_after(?FEEDBACK_RECONNECT_TIMEOUT, self(),
		      connect_feedback),
    {noreply, State#state{feedback_socket = undefined,
			  feedback_buf = <<>>}};
handle_info(_Info, State) ->
    %io:format("got info: ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_route(From, To, Packet, State) ->
    #jid{user = User, resource = Resource} = To,
    if
	(User /= <<"">>) or (Resource /= <<"">>) ->
	    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
	    ejabberd_router:route(To, From, Err),
	    {noreply, State};
	true ->
	    case Packet of
		#xmlel{name = <<"iq">>} ->
		    IQ = jlib:iq_query_info(Packet),
		    case IQ of
			#iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS,
			    sub_el = _SubEl, lang = Lang} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el =
                                        [#xmlel{name = <<"query">>,
                                                attrs = [{<<"xmlns">>, XMLNS}],
                                                children = iq_disco(Lang)}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res)),
			    {noreply, State};
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS = XMLNS} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el =
                                        [#xmlel{name = <<"query">>,
                                                attrs = [{<<"xmlns">>, XMLNS}],
                                                children = []}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res)),
			    {noreply, State};
			_ ->
			    Err = jlib:make_error_reply(Packet,
							?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err),
			    {noreply, State}
		    end;
		#xmlel{name = <<"message">>, children = Els} ->
		    case xml:remove_cdata(Els) of
			[#xmlel{name = <<"push">>}] ->
			    NewState = handle_message(From, To, Packet, State),
			    {noreply, NewState};
			[#xmlel{name = <<"disable">>}] ->
			    {noreply, State};
			_ ->
			    {noreply, State}
		    end;
		_ ->
		    {noreply, State}
	    end
    end.

get_custom_fields(Packet) ->
    case xml:get_subtag(xml:get_subtag(Packet, <<"push">>), <<"custom">>) of
        false -> [];
        #xmlel{name = <<"custom">>, attrs = [], children = Children} ->
            [ {xml:get_tag_attr_s(<<"name">>, C), xml:get_tag_cdata(C)} ||
                C <- xml:remove_cdata(Children) ]
    end.

handle_message(From, To, Packet, #state{socket = undefined} = State) ->
    queue_message(From, To, Packet, State);
handle_message(From, To, Packet, State) ->
    DeviceID =
	xml:get_path_s(Packet,
		       [{elem, <<"push">>}, {elem, <<"id">>}, cdata]),
    Msg =
	xml:get_path_s(Packet,
		       [{elem, <<"push">>}, {elem, <<"msg">>}, cdata]),
    Badge =
	xml:get_path_s(Packet,
		       [{elem, <<"push">>}, {elem, <<"badge">>}, cdata]),
    Sound =
	xml:get_path_s(Packet,
		       [{elem, <<"push">>}, {elem, <<"sound">>}, cdata]),
    Sender =
	xml:get_path_s(Packet,
		       [{elem, <<"push">>}, {elem, <<"from">>}, cdata]),
    Receiver =
	xml:get_path_s(Packet,
		       [{elem, <<"push">>}, {elem, <<"to">>}, cdata]),
    CustomFields  = get_custom_fields(Packet),
    Payload = make_payload(State, Msg, Badge, Sound, Sender, CustomFields),
    ID =
	case catch erlang:list_to_integer(binary_to_list(DeviceID), 16) of
	    ID1 when is_integer(ID1) ->
		ID1;
	    _ ->
		false
	end,
    if
	is_integer(ID) ->
	    Command = 1,
	    CmdID = State#state.cmd_id,
	    {MegaSecs, Secs, _MicroSecs} = now(),
	    Expiry = MegaSecs * 1000000 + Secs + 24 * 60 * 60,
	    BDeviceID = <<ID:256>>,
	    IDLen = size(BDeviceID),
	    PayloadLen = size(Payload),
	    Notification =
		<<Command:8,
		 CmdID:32,
		 Expiry:32,
		 IDLen:16,
		 BDeviceID/binary,
		 PayloadLen:16,
		 Payload/binary>>,
	    ?INFO_MSG("(~p) sending notification for ~s~n~p~npayload:~n~s~n"
		      "Sender: ~s~n"
		      "Receiver: ~s~n"
		      "Device ID: ~s~n",
		      [State#state.host, erlang:integer_to_list(ID, 16),
		       Notification, Payload,
		       Sender,
		       Receiver, DeviceID]),
	    case ssl:send(State#state.socket, Notification) of
		ok ->
		    cache(From, ID, State);
		{error, Reason} ->
		    ?INFO_MSG("(~p) Connection closed: ~p, reconnecting",
			      [State#state.host, Reason]),
		    ssl:close(State#state.socket),
		    self() ! connect,
		    queue_message(From, To, Packet,
				  State#state{socket = undefined})
	    end;
	true ->
	    State
    end.

make_payload(State, Msg, Badge, Sound, Sender, CustomFields) ->
    Msg2 = json_escape(Msg),
    AlertPayload =
	case Msg2 of
	    <<"">> -> <<"">>;
	    _ -> <<"\"alert\":\"", Msg2/binary, "\"">>
	end,
    BadgePayload =
	case catch jlib:binary_to_integer(Badge) of
	    B when is_integer(B) ->
		<<"\"badge\":", Badge/binary>>;
	    _ -> <<"">>
	end,
    SoundPayload = 
    	case Sound of
	    <<"true">> ->
		SoundFile = State#state.soundfile,
		<<"\"sound\":\"", (json_escape(SoundFile))/binary, "\"">>;
	    _ -> <<"">>
	end,
    Payloads = lists:filter(
                 fun(S) -> S /= <<"">> end,
                 [AlertPayload, BadgePayload, SoundPayload]),

    CustomPayloadFields =
        [<<"\"", (json_escape(Name))/binary, "\":\"",
           (json_escape(Value))/binary, "\"">>
             || {Name, Value} <- CustomFields] ++
        [<<"\"from\":\"", (json_escape(Sender))/binary, "\"">> || Sender /= <<"">>],

    Payload1 =
        [<<"{">>,
         str:join(
           [<<"\"aps\":{", (str:join(Payloads, <<",">>))/binary, "}">> | CustomPayloadFields],
           <<",">>),
         <<"}">>],
    Payload = list_to_binary(Payload1),
    PayloadLen = size(Payload),
    if
	PayloadLen > ?MAX_PAYLOAD_SIZE ->
	    Delta = PayloadLen - ?MAX_PAYLOAD_SIZE,
	    MsgLen = size(Msg),
	    if
		MsgLen /= 0 ->
		    CutMsg =
			if
			    MsgLen > Delta ->
                                {CMsg, _} = split_binary(Msg, MsgLen - Delta),
				CMsg;
			    true ->
				<<"">>
			end,
		    make_payload(State, CutMsg, Badge, Sound, Sender, CustomFields);
		true ->
		    Payload2 =
			<<"{\"aps\":{", (str:join(Payloads, <<",">>))/binary, "}}">>,
		    Payload2
	    end;
	true ->
	    Payload
    end.

connect(#state{socket = undefined, certfile_mtime = undefined} = State) ->
    Gateway = State#state.gateway,
    Port = State#state.port,
    CertFile = State#state.certfile,
    case ssl:connect(Gateway, Port, [{certfile, CertFile},
				     {active, true},
				     binary],
                     ?SSL_TIMEOUT) of
	{ok, Socket} ->
	    {noreply, resend_messages(State#state{socket = Socket})};
	{error, Reason} ->
	    {Timeout, State2} =
		case Reason of
		    esslconnect ->
			MTime = get_mtime(CertFile),
			case State#state.failure_script of
			    undefined ->
				ok;
			    FailureScript ->
				os:cmd(FailureScript ++ " " ++ Gateway)
			end,
			{?HANDSHAKE_TIMEOUT,
			 State#state{certfile_mtime = MTime}};
		    _ ->
			{?RECONNECT_TIMEOUT, State}
		end,
	    ?ERROR_MSG("(~p) Connection to ~p:~p failed: ~p, "
		       "retrying after ~p seconds",
		       [State2#state.host, Gateway, Port,
			Reason, Timeout div 1000]),
	    erlang:send_after(Timeout, self(), connect),
	    {noreply, State2}
    end;
connect(#state{socket = undefined, certfile_mtime = MTime} = State) ->
    CertFile = State#state.certfile,
    case get_mtime(CertFile) of
	MTime ->
	    Gateway = State#state.gateway,
	    Port = State#state.port,
	    Timeout = ?HANDSHAKE_TIMEOUT,
	    ?ERROR_MSG("(~p) Connection to ~p:~p postponed: "
		       "waiting for ~p update, "
		       "retrying after ~p seconds",
		       [State#state.host, Gateway, Port,
			CertFile, Timeout div 1000]),
	    erlang:send_after(Timeout, self(), connect),
	    {noreply, State};
	_ ->
	    connect(State#state{certfile_mtime = undefined})
    end;
connect(State) ->
    {noreply, State}.

get_mtime(File) ->
    case file:read_file_info(File) of
	{ok, FileInfo} ->
	    FileInfo#file_info.mtime;
	{error, _} ->
	    no_certfile
    end.

bounce_message(From, To, Packet, Reason) ->
    #xmlel{attrs = Attrs} = Packet,
    Type = xml:get_attr_s(<<"type">>, Attrs),
    if Type /= <<"error">>; Type /= <<"result">> ->
	    ejabberd_router:route(
	      To, From,
	      jlib:make_error_reply(
		Packet,
		?ERRT_INTERNAL_SERVER_ERROR(
		   xml:get_attr_s(<<"xml:lang">>, Attrs),
		   Reason)));
       true ->
	    ok
    end.

queue_message(From, To, Packet, State) ->
    case State#state.queue of
	{?MAX_QUEUE_SIZE, Queue} ->
	    {{value, {From1, To1, Packet1}}, Queue1} = queue:out(Queue),
	    bounce_message(From1, To1, Packet1,
			   <<"Unable to connect to push service">>),
	    Queue2 = queue:in({From, To, Packet}, Queue1),
	    State#state{queue = {?MAX_QUEUE_SIZE, Queue2}};
	{Size, Queue} ->
	    Queue1 = queue:in({From, To, Packet}, Queue),
	    State#state{queue = {Size+1, Queue1}}
    end.

resend_messages(#state{queue = {_, Queue}} = State) ->
    lists:foldl(
      fun({From, To, Packet}, AccState) ->
	      case catch handle_message(From, To, Packet, AccState) of
		  {'EXIT', _} = Err ->
		      ?ERROR_MSG("error while processing message:~n"
				 "** From: ~p~n"
				 "** To: ~p~n"
				 "** Packet: ~p~n"
				 "** Reason: ~p",
				 [From, To, Packet, Err]),
		      AccState;
		  NewAccState ->
		      NewAccState
	      end
      end, State#state{queue = {0, queue:new()}}, queue:to_list(Queue)).

cache(JID, DeviceID, State) ->
    CmdID = State#state.cmd_id,
    Key = CmdID rem ?CACHE_SIZE,
    C1 = State#state.cmd_cache,
    D1 = State#state.device_cache,
    D2 = case dict:find(Key, C1) of
	     {ok, {_, OldDeviceID}} ->
		 del_device_cache(D1, OldDeviceID);
	     error ->
		 D1
	 end,
    D3 = add_device_cache(D2, DeviceID, JID),
    C2 = dict:store(Key, {JID, DeviceID}, C1),
    State#state{cmd_id = CmdID + 1,
		cmd_cache = C2,
		device_cache = D3}.

add_device_cache(DeviceCache, DeviceID, JID) ->
    dict:update(
      DeviceID,
      fun({Counter, _}) -> {Counter + 1, JID} end,
      {1, JID},
      DeviceCache).

del_device_cache(DeviceCache, DeviceID) ->
    case dict:find(DeviceID, DeviceCache) of
	{ok, {Counter, JID}} ->
	    case Counter of
		1 ->
		    dict:erase(DeviceID, DeviceCache);
		_ ->
		    dict:store(DeviceID, {Counter - 1, JID}, DeviceCache)
	    end;
	error ->
	    DeviceCache
    end.

json_escape(S) ->
    json_escape(S, <<>>).

json_escape(<<>>, Res) ->
    Res;
json_escape(<<C, S/binary>>, Res) ->
    case C of
        $" -> json_escape(S, <<Res/binary, "\\\"">>);
        $\\ -> json_escape(S, <<Res/binary, "\\\\">>);
        _ when C < 16 ->
            B = list_to_binary(erlang:integer_to_list(C, 16)),
            json_escape(S, <<Res/binary, "\\u000", B/binary>>);
        _ when C < 32 ->
            B = list_to_binary(erlang:integer_to_list(C, 16)),
            json_escape(S, <<Res/binary, "\\u00", B/binary>>);
        _ -> json_escape(S, <<Res/binary, C>>)
    end.


iq_disco(Lang) ->
    [#xmlel{name = <<"identity">>,
            attrs = [{<<"category">>, <<"gateway">>},
                     {<<"type">>, <<"apple">>},
                     {<<"name">>, translate:translate(
                                    Lang, <<"Apple Push Service">>)}],
            children = []},
     #xmlel{name = <<"feature">>,
            attrs = [{<<"var">>, ?NS_DISCO_INFO}],
            children = []}].


parse_feedback_buf(Buf, State) ->
    case Buf of
	<<TimeStamp:32, IDLen:16, BDeviceID:IDLen/binary, Rest/binary>> ->
	    IDLen8 = IDLen * 8,
	    <<DeviceID:IDLen8>> = BDeviceID,
	    ?INFO_MSG("(~p) received feedback for ~s~n",
		      [State#state.host, erlang:integer_to_list(DeviceID, 16)]),
	    case dict:find(DeviceID, State#state.device_cache) of
		{ok, {_Counter, JID}} ->
                    ?INFO_MSG("(~p) sending feedback for ~s to ~s~n",
                              [State#state.host,
                               erlang:integer_to_list(DeviceID, 16),
                               jlib:jid_to_string(JID)]),
		    From = jlib:make_jid(<<"">>, State#state.host, <<"">>),
		    ejabberd_router:route(
		      From, JID,
		      #xmlel{name = <<"iq">>,
                             attrs = [{<<"id">>, <<"disable">>},
                                      {<<"type">>, <<"set">>}],
                             children =
                             [#xmlel{name = <<"disable">>,
                                     attrs =
                                     [{<<"xmlns">>, ?NS_P1_PUSH},
                                      {<<"status">>, <<"feedback">>},
                                      {<<"ts">>, jlib:integer_to_binary(TimeStamp)},
                                      {<<"id">>, jlib:integer_to_binary(DeviceID, 16)}],
                                     children = []}]});
		error ->
		    ok
	    end,
	    parse_feedback_buf(Rest, State);
	_ ->
	    Buf
    end.

iolist_to_string(S) ->
    binary_to_list(iolist_to_binary(S)).
