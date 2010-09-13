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

-record(state, {host,
		socket,
		gateway,
		port,
		feedback_socket,
		feedback,
		feedback_port,
		feedback_buf = <<>>,
		certfile,
		queue,
		soundfile,
		cmd_id = 0,
		cmd_cache = dict:new(),
		device_cache = dict:new()}).

-define(PROCNAME, ejabberd_mod_applepush_service).
-define(RECONNECT_TIMEOUT, 5000).
-define(FEEDBACK_RECONNECT_TIMEOUT, 30000).
-define(MAX_QUEUE_SIZE, 1000).
-define(CACHE_SIZE, 4096).
-define(MAX_PAYLOAD_SIZE, 255).

-define(NS_P1_PUSH, "p1:push").

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
    MyHosts =
	case catch gen_mod:get_opt(hosts, Opts) of
	    {'EXIT', _} ->
		[{gen_mod:get_opt_host(Host, Opts, "applepush.@HOST@"), Opts}];
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
    MyHosts =
	case catch gen_mod:get_module_opt(Host, ?MODULE, hosts, []) of
	    [] ->
		[gen_mod:get_module_opt_host(
		   Host, ?MODULE, "applepush.@HOST@")];
	    Hs ->
		[H || {H, _} <- Hs]
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
    CertFile = gen_mod:get_opt(certfile, Opts, ""),
    SoundFile = gen_mod:get_opt(sound_file, Opts, "pushalert.wav"),
    Gateway = gen_mod:get_opt(gateway, Opts, "gateway.push.apple.com"),
    Feedback = gen_mod:get_opt(feedback, Opts, undefined),
    Port = gen_mod:get_opt(port, Opts, 2195),
    FeedbackPort = gen_mod:get_opt(feedback_port, Opts, 2196),
    %MyHost = gen_mod:get_opt_host(Host, Opts, "applepush.@HOST@"),
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
handle_info(connect_feedback, State)
  when State#state.feedback /= undefined,
       State#state.feedback_socket == undefined ->
    Feedback = State#state.feedback,
    FeedbackPort = State#state.feedback_port,
    CertFile = State#state.certfile,
    case ssl:connect(Feedback, FeedbackPort,
		     [{certfile, CertFile},
		      {active, true},
		      binary]) of
	{ok, Socket} ->
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
                    %From = jlib:make_jid("", State#state.host, ""),
		    %ejabberd_router:route(
		    %  From, JID,
		    %  {xmlelement, "message", [],
		    %   [{xmlelement, "disable",
		    %	 [{"xmlns", ?NS_P1_PUSH},
		    %	  {"status", integer_to_list(Status)}],
		    %	  []}]});
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
	(User /= "") or (Resource /= "") ->
	    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
	    ejabberd_router:route(To, From, Err),
	    {noreply, State};
	true ->
	    case Packet of
		{xmlelement, "iq", _, _} ->
		    IQ = jlib:iq_query_info(Packet),
		    case IQ of
			#iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS,
			    sub_el = _SubEl, lang = Lang} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el = [{xmlelement, "query",
						   [{"xmlns", XMLNS}],
						   iq_disco(Lang)}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res)),
			    {noreply, State};
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS = XMLNS} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el = [{xmlelement, "query",
						   [{"xmlns", XMLNS}],
						   []}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res)),
			    {noreply, State};
			%%#iq{type = get, xmlns = ?NS_VCARD, lang = Lang} ->
			%%    ResIQ = 
			%%	IQ#iq{type = result,
			%%	      sub_el = [{xmlelement,
			%%			 "vCard",
			%%			 [{"xmlns", ?NS_VCARD}],
			%%			 iq_get_vcard(Lang)}]},
			%%    ejabberd_router:route(To,
			%%			  From,
			%%			  jlib:iq_to_xml(ResIQ));
			_ ->
			    Err = jlib:make_error_reply(Packet,
							?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err),
			    {noreply, State}
		    end;
		{xmlelement, "message", _, Els} ->
		    case xml:remove_cdata(Els) of
			[{xmlelement, "push", _, _}] ->
			    NewState = handle_message(From, To, Packet, State),
			    {noreply, NewState};
			[{xmlelement, "disable", _, _}] ->
			    {noreply, State};
			_ ->
			    {noreply, State}
		    end;
		_ ->
		    {noreply, State}
	    end
    end.


handle_message(From, To, Packet, #state{socket = undefined} = State) ->
    queue_message(From, To, Packet, State);
handle_message(From, To, Packet, State) ->
    DeviceID =
	xml:get_path_s(Packet,
		       [{elem, "push"}, {elem, "id"}, cdata]),
    Msg =
	xml:get_path_s(Packet,
		       [{elem, "push"}, {elem, "msg"}, cdata]),
    Badge =
	xml:get_path_s(Packet,
		       [{elem, "push"}, {elem, "badge"}, cdata]),
    Sound =
	xml:get_path_s(Packet,
		       [{elem, "push"}, {elem, "sound"}, cdata]),
    Sender =
	xml:get_path_s(Packet,
		       [{elem, "push"}, {elem, "from"}, cdata]),
    Receiver =
	xml:get_path_s(Packet,
		       [{elem, "push"}, {elem, "to"}, cdata]),
    Payload = make_payload(State, Msg, Badge, Sound, Sender),
    ID =
	case catch erlang:list_to_integer(DeviceID, 16) of
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
	    BPayload = list_to_binary(Payload),
	    IDLen = size(BDeviceID),
	    PayloadLen = size(BPayload),
	    Notification =
		<<Command:8,
		 CmdID:32,
		 Expiry:32,
		 IDLen:16,
		 BDeviceID/binary,
		 PayloadLen:16,
		 BPayload/binary>>,
	    ?INFO_MSG("(~p) sending notification for ~s~n~p~npayload:~n~s~n"
		      "Sender: ~s~n"
		      "Receiver: ~s~n"
		      "Device ID: ~s~n",
		      [State#state.host, erlang:integer_to_list(ID, 16),
		       Notification, Payload,
		       jlib:jid_to_string(From),
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

make_payload(State, Msg, Badge, Sound, Sender) ->
    Msg2 = json_escape(Msg),
    AlertPayload =
	case Msg2 of
	    "" -> "";
	    _ -> "\"alert\":\"" ++ Msg2 ++ "\""
	end,
    BadgePayload =
	case catch list_to_integer(Badge) of
	    B when is_integer(B) ->
		"\"badge\":" ++ Badge;
	    _ -> ""
	end,
    SoundPayload = 
    	case Sound of
	    "true" ->
		SoundFile = State#state.soundfile,
		"\"sound\":\"" ++ json_escape(SoundFile) ++ "\"";
	    _ -> ""
	end,
    Payloads = lists:filter(fun(S) -> S /= "" end,
			    [AlertPayload, BadgePayload, SoundPayload]),
    Payload =
	case Sender of
	    "" ->
		"{\"aps\":{" ++ join(Payloads, ",") ++ "}}";
	    _ ->
		"{\"aps\":{" ++ join(Payloads, ",") ++ "},"
		    "\"from\":\"" ++ json_escape(Sender) ++ "\"}"
	end,
    PayloadLen = length(Payload),
    if
	PayloadLen > ?MAX_PAYLOAD_SIZE ->
	    Delta = PayloadLen - ?MAX_PAYLOAD_SIZE,
	    MsgLen = length(Msg),
	    if
		MsgLen /= 0 ->
		    CutMsg =
			if
			    MsgLen > Delta ->
				lists:sublist(Msg, MsgLen - Delta);
			    true ->
				""
			end,
		    make_payload(State, CutMsg, Badge, Sound, Sender);
		true ->
		    Payload2 =
			"{\"aps\":{" ++ join(Payloads, ",") ++ "}}",
		    %PayloadLen2 = length(Payload2),
		    Payload2
	    end;
	true ->
	    Payload
    end.

connect(#state{socket = undefined} = State) ->
    Gateway = State#state.gateway,
    Port = State#state.port,
    CertFile = State#state.certfile,
    case ssl:connect(Gateway, Port, [{certfile, CertFile},
				     {active, true},
				     binary]) of
	{ok, Socket} ->
	    {noreply, resend_messages(State#state{socket = Socket})};
	{error, Reason} ->
	    ?ERROR_MSG("(~p) Connection to ~p:~p failed: ~p, "
		       "retrying after ~p seconds",
		       [State#state.host, Gateway, Port,
			Reason, ?RECONNECT_TIMEOUT div 1000]),
	    erlang:send_after(?RECONNECT_TIMEOUT, self(), connect),
	    {noreply, State}
    end;
connect(State) ->
    {noreply, State}.

bounce_message(From, To, Packet, Reason) ->
    {xmlelement, _, Attrs, _} = Packet,
    Type = xml:get_attr_s("type", Attrs),
    if Type /= "error"; Type /= "result" ->
	    ejabberd_router:route(
	      To, From,
	      jlib:make_error_reply(
		Packet,
		?ERRT_INTERNAL_SERVER_ERROR(
		   xml:get_attr_s("xml:lang", Attrs),
		   Reason)));
       true ->
	    ok
    end.

queue_message(From, To, Packet, State) ->
    case State#state.queue of
	{?MAX_QUEUE_SIZE, Queue} ->
	    {{value, {From1, To1, Packet1}}, Queue1} = queue:out(Queue),
	    bounce_message(From1, To1, Packet1,
			   "Unable to connect to push service"),
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
    [case C of
	 $" -> "\\\"";
	 $\\ -> "\\\\";
	 _ when C < 16 -> ["\\u000", erlang:integer_to_list(C, 16)];
	 _ when C < 32 -> ["\\u00", erlang:integer_to_list(C, 16)];
	 _ -> C
     end || C <- S].

join(List, Sep) ->
    lists:foldr(fun(A, "") -> A;
                   (A, Acc) -> A ++ Sep ++ Acc
                end, "", List).



iq_disco(Lang) ->
    [{xmlelement, "identity",
      [{"category", "gateway"},
       {"type", "apple"},
       {"name", translate:translate(Lang, "Apple Push Service")}], []},
     {xmlelement, "feature", [{"var", ?NS_DISCO_INFO}], []}].


parse_feedback_buf(Buf, State) ->
    case Buf of
	<<TimeStamp:32, IDLen:16, BDeviceID:IDLen/binary, Rest/binary>> ->
	    IDLen8 = IDLen * 8,
	    <<DeviceID:IDLen8>> = BDeviceID,
	    case dict:find(DeviceID, State#state.device_cache) of
		{ok, {_Counter, JID}} ->
		    From = jlib:make_jid("", State#state.host, ""),
		    ejabberd_router:route(
		      From, JID,
		      {xmlelement, "message", [],
		       [{xmlelement, "disable",
			 [{"xmlns", ?NS_P1_PUSH},
			  {"status", "feedback"},
			  {"ts", integer_to_list(TimeStamp)}],
			 []}]});
		error ->
		    ok
	    end,
	    parse_feedback_buf(Rest, State);
	_ ->
	    Buf
    end.
