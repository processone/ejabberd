%%%----------------------------------------------------------------------
%%% File    : ejabberd_http_bind.erl
%%% Author  : Stefan Strigler <steve@zeank.in-berlin.de>
%%% Purpose : Implements XMPP over BOSH (XEP-0205) (formerly known as 
%%%           HTTP Binding)
%%% Created : 21 Sep 2005 by Stefan Strigler <steve@zeank.in-berlin.de>
%%% Id      : $Id: ejabberd_http_bind.erl 408 2007-11-08 15:48:24Z badlop $
%%%----------------------------------------------------------------------

-module(ejabberd_http_bind).
-author('steve@zeank.in-berlin.de').
-vsn('$Rev: 408 $').

-behaviour(gen_fsm).

%% External exports
-export([start_link/2,
	 init/1,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3,
	 send/2,
	 setopts/2,
         sockname/1, 
         peername/1,
	 controlling_process/2,
	 close/1,
	 process_request/1]).

%%-define(ejabberd_debug, true).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

-record(http_bind, {id, pid, to, hold, wait, version}).

%% http binding request
-record(hbr, {rid,
	      key,
	      in,
	      out}).

-record(state, {id,
		rid = none,
		key,
		output = "",
		input = "",
		waiting_input = false,
		last_receiver,
		last_poll,
		ctime = 0,
		timer,
                pause=0,
		req_list = [] % list of requests
	       }).


%%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(BOSH_VERSION, "1.6").
-define(NS_CLIENT, "jabber:client").
-define(NS_BOSH, "urn:xmpp:xbosh").
-define(NS_HTTP_BIND, "http://jabber.org/protocol/httpbind").

-define(MAX_REQUESTS, 2).  % number of simultaneous requests
-define(MIN_POLLING, 2000000). % don't poll faster than that or we will
                               % shoot you (time in µsec)
-define(MAX_WAIT, 3600). % max num of secs to keep a request on hold
-define(MAX_INACTIVITY, 30000). % msecs to wait before terminating
                                % idle sessions
-define(MAX_PAUSE, 120). % may num of sec a client is allowed to pause 
                         % the session

-define(CT, {"Content-Type", "text/xml; charset=utf-8"}).
-define(HEADER, [?CT]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Sid, Key) ->
    mnesia:create_table(http_bind,
        		[{ram_copies, [node()]},
        		 {attributes, record_info(fields, http_bind)}]),
    supervisor:start_child(ejabberd_http_bind_sup, [Sid, Key]).

start_link(Sid, Key) ->
    gen_fsm:start_link(?MODULE, [Sid, Key], ?FSMOPTS).

send({http_bind, FsmRef}, Packet) ->
    gen_fsm:sync_send_all_state_event(FsmRef, {send, Packet}).

setopts({http_bind, FsmRef}, Opts) ->
    case lists:member({active, once}, Opts) of
	true ->
	    gen_fsm:send_all_state_event(FsmRef, {activate, self()});
	_ ->
	    ok
    end.

controlling_process(_Socket, _Pid) ->
    ok.

close({http_bind, FsmRef}) ->
    catch gen_fsm:sync_send_all_state_event(FsmRef, close).

sockname(_Socket) ->
    {ok, {{0, 0, 0, 0}, 0}}.

peername(_Socket) ->
    {ok, {{0, 0, 0, 0}, 0}}.

process_request(Data) ->
    case catch parse_request(Data) of
	{ok, {"", Rid, Attrs, Payload}} ->
	    case xml:get_attr_s("to",Attrs) of
                "" ->
		    {200, ?HEADER, "<body type='terminate' "
		     "condition='improper-addressing' "
		     "xmlns='" ++ ?NS_HTTP_BIND ++ "'/>"};
                XmppDomain ->
                    %% create new session
                    Sid = sha:sha(term_to_binary({now(), make_ref()})),
                    {ok, Pid} = start(Sid, ""),
                    ?DEBUG("got pid: ~p", [Pid]),
                    Wait = case
                               string:to_integer(xml:get_attr_s("wait",Attrs))
                               of
                               {error, _} ->
                                   ?MAX_WAIT;
                               {CWait, _} ->
                                   if 
                                       (CWait > ?MAX_WAIT) ->
                                           ?MAX_WAIT;
                                       true ->
                                           CWait
                                   end
                           end,
                    Hold = case
                               string:to_integer(
                                 xml:get_attr_s("hold",Attrs))
                               of
                               {error, _} ->
                                   (?MAX_REQUESTS - 1);
                               {CHold, _} ->
                                   if 
                                       (CHold > (?MAX_REQUESTS - 1)) ->
                                           (?MAX_REQUESTS - 1);
                                       true ->
                                           CHold
                                   end
                           end,
                    Version = 
                        case catch list_to_float(
                                     xml:get_attr_s("ver", Attrs)) of
                            {'EXIT', _} -> 0.0;
                            V -> V
                        end,
                    XmppVersion = xml:get_attr_s("xmpp:version", Attrs),
                    mnesia:transaction(
                      fun() ->
                              mnesia:write(
                                #http_bind{id = Sid,
                                           pid = Pid,
                                           to = {XmppDomain, 
                                                 XmppVersion},
                                           hold = Hold,
                                           wait = Wait,
                                           version = Version
                                          })
                      end),
                    handle_http_put(Sid, Rid, Attrs, Payload, true)
            end;
        {ok, {Sid, Rid, Attrs, Payload1}} ->
            %% old session
            StreamStart =  
                case xml:get_attr_s("xmpp:restart",Attrs) of
                    "true" ->
                        true;
                    _ ->
                        false
                end,
            Payload2 = case xml:get_attr_s("type",Attrs) of
                           "terminate" ->
                               %% close stream
                               Payload1 ++ "</stream:stream>";
                           _ ->
                               Payload1
                       end,
            handle_http_put(Sid, Rid, Attrs, Payload2, StreamStart);
        _ ->
            {400, ?HEADER, ""}
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}                   
%%----------------------------------------------------------------------
init([Sid, Key]) ->
    ?DEBUG("started: ~p", [{Sid, Key}]),

    %% Read c2s options from the first ejabberd_c2s configuration in
    %% the config file listen section
    %% TODO: We should have different access and shaper values for
    %% each connector. The default behaviour should be however to use
    %% the default c2s restrictions if not defined for the current
    %% connector.
    Opts = ejabberd_c2s_config:get_c2s_limits(),

    ejabberd_socket:start(ejabberd_c2s, ?MODULE, {http_bind, self()}, Opts),
%    {ok, C2SPid} = ejabberd_c2s:start({?MODULE, {http_bind, self()}}, Opts),
%    ejabberd_c2s:become_controller(C2SPid),
    Timer = erlang:start_timer(?MAX_INACTIVITY, self(), []),
    {ok, loop, #state{id = Sid,
		      key = Key,
		      timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event({activate, From}, StateName, StateData) ->
    case StateData#state.input of
	"" ->
	    {next_state, StateName, StateData#state{
				     waiting_input = {From, ok}}};
	Input ->
            Receiver = From,
	    Receiver ! {tcp, {http_bind, self()}, list_to_binary(Input)},
	    {next_state, StateName, StateData#state{
				     input = "",
				     waiting_input = false,
				     last_receiver = Receiver}}
    end;

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
handle_sync_event({send, Packet}, _From, StateName, StateData) ->
    Output = [StateData#state.output | Packet],
    Reply = ok,
    {reply, Reply, StateName, StateData#state{output = Output}};

handle_sync_event(stop, _From, _StateName, StateData) ->
    Reply = ok,
    {stop, normal, Reply, StateData};

handle_sync_event({http_put, Rid, Attrs, Payload, Hold, StreamTo},
		  _From, StateName, StateData) ->
    Key = xml:get_attr_s("key", Attrs),
    NewKey = xml:get_attr_s("newkey", Attrs),
    %% check if Rid valid
    RidAllow =  case StateData#state.rid of
                    none -> 
                        %% first request - nothing saved so far
                        {true, 0};
                    OldRid ->
                        ?DEBUG("state.rid/cur rid: ~p/~p", 
                               [OldRid, Rid]),
                        if 
                            (OldRid < Rid) and 
                            (Rid =< (OldRid + Hold + 1)) ->
                                case catch list_to_integer(
                                       xml:get_attr_s("pause", Attrs)) of
                                    {'EXIT', _} ->
                                        {true, 0};
                                    Pause1 when Pause1 =< ?MAX_PAUSE ->
                                        ?DEBUG("got pause: ~p", [Pause1]),
                                        {true, Pause1};
                                    _ ->
                                        {true, 0}
                                end;
                            (Rid =< OldRid) and 
                            (Rid > OldRid - Hold - 1) ->
                                repeat;
                            true ->
                                false
                        end
                end,
    %% check if key valid
    KeyAllow = case RidAllow of
		   repeat -> 
		       true;
		   false ->
		       false;
		   {true, _} ->
		       case StateData#state.key of
			   "" ->
			       true;
			   OldKey ->
			       NextKey = string:to_lower(
					   hex(binary_to_list(
						 crypto:sha(Key)))),
			       ?DEBUG("Key/OldKey/NextKey: ~s/~s/~s", 
				      [Key, OldKey, NextKey]),
			       if
				   OldKey == NextKey ->
				       true;
				   true ->
				       ?DEBUG("wrong key: ~s",[Key]),
				       false
			       end
		       end
	       end,
    {_,TSec,TMSec} = now(),
    TNow = TSec*1000*1000 + TMSec,
    LastPoll = if 
		   Payload == "" ->
		       TNow;
		   true ->
		       0
	       end,
    if
	(Payload == "") and 
        (Hold == 0) and
	(TNow - StateData#state.last_poll < ?MIN_POLLING) ->
	    Reply = {error, polling_too_frequently},
	    {reply, Reply, StateName, StateData};
	KeyAllow ->
	    case RidAllow of
		false ->
		    Reply = {error, not_exists},
		    {reply, Reply, StateName, StateData};
		repeat ->
		    ?DEBUG("REPEATING ~p", [Rid]),
		    [Out | _XS] = [El#hbr.out || 
				      El <- StateData#state.req_list, 
				      El#hbr.rid == Rid],
		    case Out of 
			[[] | OutPacket] ->
			    Reply = {repeat, OutPacket};
			_ ->
			    Reply = {repeat, Out}
		    end,
		    {reply, Reply, StateName, 
		     StateData#state{input = "cancel", last_poll = LastPoll}};
		{true, Pause} ->
		    SaveKey = if 
				  NewKey == "" ->
				      Key;
				  true ->
				      NewKey
			      end,
		    ?DEBUG(" -- SaveKey: ~s~n", [SaveKey]),

		    %% save request
		    ReqList = [#hbr{rid=Rid,
				    key=StateData#state.key,
				    in=StateData#state.input,
				    out=StateData#state.output
				   } | 
			       [El || El <- StateData#state.req_list, 
				      El#hbr.rid < Rid, 
				      El#hbr.rid > (Rid - 1 - Hold)]
			      ],
%%		    ?DEBUG("reqlist: ~p", [ReqList]),
                    
                    %% setup next timer
                    cancel_timer(StateData#state.timer),
                    if 
                        Pause > 0 ->
			    Timer = erlang:start_timer(
				      Pause*1000, self(), []);
                        true ->
			    Timer = erlang:start_timer(
				      ?MAX_INACTIVITY, self(), [])
                    end,
		    case StateData#state.waiting_input of
			false ->
			    Input = Payload ++ [StateData#state.input],
			    Reply = ok,
			    {reply, Reply, StateName, 
			     StateData#state{input = Input,
					     rid = Rid,
					     key = SaveKey,
					     ctime = TNow,
					     timer = Timer,
                                             pause = Pause,
					     last_poll = LastPoll,
					     req_list = ReqList
					    }};
			{Receiver, _Tag} ->
                            SendPacket = 
                                case StreamTo of
                                    {To, ""} ->
                                        ["<stream:stream to='", To, "' "
                                         "xmlns='"++?NS_CLIENT++"' "
                                         "xmlns:stream='"++?NS_STREAM++"'>"] 
                                            ++ Payload;
                                    {To, Version} ->
                                        ["<stream:stream to='", To, "' "
                                         "xmlns='"++?NS_CLIENT++"' "
                                         "version='", Version, "' "
                                         "xmlns:stream='"++?NS_STREAM++"'>"] 
                                            ++ Payload;
                                    _ ->
                                        Payload
                                end,
                            ?DEBUG("really sending now: ~s", [SendPacket]),
			    Receiver ! {tcp, {http_bind, self()},
					list_to_binary(SendPacket)},
			    Reply = ok,
			    {reply, Reply, StateName,
			     StateData#state{waiting_input = false,
					     last_receiver = Receiver,
					     input = "",
					     rid = Rid,
					     key = SaveKey,
					     ctime = TNow,
					     timer = Timer,
                                             pause = Pause,
					     last_poll = LastPoll,
					     req_list = ReqList
					    }}
		    end
	    end;
	true ->
	    Reply = {error, bad_key},
	    {reply, Reply, StateName, StateData}
    end;

handle_sync_event({http_get, Rid, Wait, Hold}, _From, StateName, StateData) ->
    %% setup timer
    cancel_timer(StateData#state.timer),
    if 
        StateData#state.pause > 0 ->
            Timer = erlang:start_timer(
                      StateData#state.pause*1000, self(), []);
        true ->
            Timer = erlang:start_timer(
                      ?MAX_INACTIVITY, self(), [])
    end,

    {_,TSec,TMSec} = now(),
    TNow = TSec*1000*1000 + TMSec,
    if 
	(Hold > 0) and 
	(StateData#state.output == "") and 
	((TNow - StateData#state.ctime) < (Wait*1000*1000)) and 
	(StateData#state.rid == Rid) and 
	(StateData#state.input /= "cancel") and
        (StateData#state.pause == 0) ->
	    Output = StateData#state.output,
	    ReqList = StateData#state.req_list,
	    Reply = {ok, keep_on_hold};
	(StateData#state.input == "cancel") ->
	    Output = StateData#state.output,
	    ReqList = StateData#state.req_list,
	    Reply = {ok, cancel};
	true ->
	    case StateData#state.output of
		[[]| OutPacket] ->
		    Reply = {ok, OutPacket};
		_ ->
		    Reply = {ok, StateData#state.output}
	    end,
	    %% save request
	    ReqList = [#hbr{rid=Rid,
			    key=StateData#state.key,
			    in=StateData#state.input,
			    out=StateData#state.output
			   } | 
		       [El || El <- StateData#state.req_list, 
			      El#hbr.rid /= Rid ] 
		      ],
	    Output = ""
    end,
    {reply, Reply, StateName, StateData#state{
				input = "",
				output = Output, 
				timer = Timer,
				req_list = ReqList}};

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info({timeout, Timer, _}, _StateName,
	    #state{timer = Timer} = StateData) ->
    ?DEBUG("ding dong", []),
    {stop, normal, StateData};

handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, StateData) ->
    ?DEBUG("terminate: deleting session ~s", [StateData#state.id]),
    mnesia:transaction(
      fun() ->
	      mnesia:delete({http_bind, StateData#state.id})
      end),
    case StateData#state.waiting_input of
	false ->
	    case StateData#state.last_receiver of
		undefined -> ok;
		Receiver -> Receiver ! {tcp_closed, {http_bind, self()}}
	    end;
	{Receiver, _Tag} -> Receiver ! {tcp_closed, {http_bind, self()}}
    end,
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

handle_http_put(Sid, Rid, Attrs, Payload, StreamStart) ->
    case http_put(Sid, Rid, Attrs, Payload, StreamStart) of
        {error, not_exists} ->
            ?DEBUG("no session associated with sid: ~p", [Sid]),
            {404, ?HEADER, ""};
        {{error, Reason}, Sess} ->
            handle_http_put_error(Reason, Sess);
        {{repeat, OutPacket}, Sess} ->
            ?DEBUG("http_put said 'repeat!' ...~nOutPacket: ~p", 
                   [OutPacket]),
            send_outpacket(Sess, OutPacket);
        {ok, Sess} ->
            receive_loop(Sess, Rid, Attrs, StreamStart)
    end.

http_put(Sid, Rid, Attrs, Payload, StreamStart) ->
    ?DEBUG("http-put",[]),
    case mnesia:dirty_read({http_bind, Sid}) of
	[] ->
            {error, not_exists};
	[#http_bind{pid = FsmRef, hold=Hold, to={To, StreamVersion}}=Sess] ->
            NewStream = 
                case StreamStart of
                    true ->
                        {To, StreamVersion};
                    _ ->
                        ""
                end,
            {gen_fsm:sync_send_all_state_event(
               FsmRef, {http_put, Rid, Attrs, Payload, Hold, NewStream}), Sess}
    end.

handle_http_put_error(Reason, #http_bind{pid=FsmRef, version=Version}) 
  when Version >= 0 ->
    gen_fsm:sync_send_all_state_event(FsmRef,stop),
    case Reason of
        not_exists ->
            {200, ?HEADER, 
             xml:element_to_string(
               {xmlelement, "body",
                [{"xmlns", ?NS_HTTP_BIND},
                 {"type", "terminate"},
                 {"condition", "item-not-found"}], []})};
        bad_key ->
            {200, ?HEADER, 
             xml:element_to_string(
               {xmlelement, "body",
                [{"xmlns", ?NS_HTTP_BIND},
                 {"type", "terminate"},
                 {"condition", "item-not-found"}], []})};
        polling_too_frequently ->
            {200, ?HEADER, 
             xml:element_to_string(
               {xmlelement, "body",
                [{"xmlns", ?NS_HTTP_BIND},
                 {"type", "terminate"},
                 {"condition", "policy-violation"}], []})}
    end;
handle_http_put_error(Reason, #http_bind{pid=FsmRef}) ->
    gen_fsm:sync_send_all_state_event(FsmRef,stop),
    case Reason of 
        not_exists -> %% bad rid
            {404, ?HEADER, ""};
        bad_key ->
            {404, ?HEADER, ""};
        polling_too_frequently ->
            {403, ?HEADER, ""}		    
    end.


receive_loop(Sess, Rid, Attrs, StreamStart) ->
    receive
	after 100 -> ok
	end,
    prepare_response(Sess, Rid, Attrs, StreamStart).

prepare_response(#http_bind{id=Sid, wait=Wait, hold=Hold}=Sess, 
                 Rid, Attrs, StreamStart) ->
    case http_get(Sess, Rid) of
	{ok, keep_on_hold} ->
	    receive_loop(Sess, Rid, Attrs, StreamStart);
	{ok, cancel} ->
	    %% actually it would be better if we could completely
	    %% cancel this request, but then we would have to hack
	    %% ejabberd_http and I'm too lazy now
            {200, ?HEADER, "<body type='error' xmlns='"++?NS_HTTP_BIND++"'/>"};
	{ok, OutPacket} ->
            ?DEBUG("OutPacket: ~s", [OutPacket]),
	    case StreamStart of
                false ->
		    send_outpacket(Sess, OutPacket);
		true ->
		    OutEls = 
                        case xml_stream:parse_element(
                               OutPacket++"</stream:stream>") of
                            El when element(1, El) == xmlelement ->
                                ?DEBUG("~p", [El]),
                                {xmlelement, _, OutAttrs, Els} = El,
                                AuthID = xml:get_attr_s("id", OutAttrs),
                                From = xml:get_attr_s("from", OutAttrs),
                                Version = xml:get_attr_s("version", OutAttrs),
                                StreamError = false,
                                case Els of
                                    [] ->
                                        [];
                                    [{xmlelement, "stream:features", 
                                      StreamAttribs, StreamEls} 
                                     | StreamTail] ->
                                        [{xmlelement, "stream:features", 
                                          [{"xmlns:stream",
                                            ?NS_STREAM}
                                          ] 
                                          ++ StreamAttribs, 
                                          StreamEls
                                         }] ++ StreamTail;
                                    Xml ->
                                        Xml
                                end;
                            {error, _} ->
                                AuthID = "",
                                From = "",
                                Version = "",
                                StreamError = true,
                                []
                        end,
		    if
			StreamError == true ->
			    {200, ?HEADER, "<body type='terminate' "
			     "condition='host-unknown' "
			     "xmlns='"++?NS_HTTP_BIND++"'/>"};
			true ->
                            BOSH_attribs = 
                                [{"authid", AuthID},
                                 {"xmlns:xmpp", ?NS_BOSH},
                                 {"xmlns:stream", ?NS_STREAM}] ++
                                case OutEls of 
                                    [] ->
                                        [];
                                    _ ->
                                        [{"xmpp:version", Version}]
                                end,
			    {200, ?HEADER,
			     xml:element_to_string(
			       {xmlelement,"body",
				[{"xmlns",
				  ?NS_HTTP_BIND},
				 {"sid", Sid},
				 {"wait", integer_to_list(Wait)},
				 {"requests", integer_to_list(Hold+1)},
				 {"inactivity", 
				  integer_to_list(
                                    trunc(?MAX_INACTIVITY/1000))},
                                 {"maxpause",
                                  integer_to_list(?MAX_PAUSE)},
				 {"polling", 
                                  integer_to_list(
                                    trunc(?MIN_POLLING/1000000))},
                                 {"ver", ?BOSH_VERSION},
                                 {"from", From},
                                 {"secure", "true"} %% we're always being secure
				] ++ BOSH_attribs,OutEls})}
		    end
	    end
    end.

http_get(#http_bind{pid = FsmRef, wait = Wait, hold = Hold}, Rid) ->
    gen_fsm:sync_send_all_state_event(FsmRef, 
                                      {http_get, Rid, Wait, Hold}).
    
send_outpacket(#http_bind{pid = FsmRef}, OutPacket) ->
    case OutPacket of
	"" ->
	    {200, ?HEADER, "<body xmlns='"++?NS_HTTP_BIND++"'/>"};
	"</stream:stream>" ->
            gen_fsm:sync_send_all_state_event(FsmRef,stop),
	    {200, ?HEADER, "<body xmlns='"++?NS_HTTP_BIND++"'/>"};
	_ ->
	    case xml_stream:parse_element("<body>" 
					  ++ OutPacket
					  ++ "</body>") 
		of
		El when element(1, El) == xmlelement ->
		    {xmlelement, _, _, OEls} = El,
		    TypedEls = [check_default_xmlns(OEl) ||
				   OEl <- OEls],
		    ?DEBUG(" --- outgoing data --- ~n~s~n --- END --- ~n",
			   [xml:element_to_string(
			      {xmlelement,"body",
			       [{"xmlns",
				 ?NS_HTTP_BIND}],
			       TypedEls})]
			  ),
		    {200, ?HEADER,
		     xml:element_to_string(
		       {xmlelement,"body",
			[{"xmlns",
			  ?NS_HTTP_BIND}],
			TypedEls})};
		{error, _E} ->
		    OutEls = case xml_stream:parse_element(
                                    OutPacket++"</stream:stream>") of
                                 SEl when element(1, SEl) == xmlelement ->
                                     {xmlelement, _, _OutAttrs, SEls} = SEl,
                                     StreamError = false,
                                     case SEls of
                                         [] ->
                                             [];
                                         [{xmlelement, 
                                           "stream:features", 
                                           StreamAttribs, StreamEls} | 
                                          StreamTail] ->
                                             TypedTail = 
                                                 [check_default_xmlns(OEl) ||
						     OEl <- StreamTail],
                                             [{xmlelement, 
                                               "stream:features", 
                                               [{"xmlns:stream",
                                                 ?NS_STREAM}] ++ 
                                               StreamAttribs, StreamEls}] ++ 
                                                 TypedTail;
                                         Xml ->
                                             Xml
                                     end;
                                 {error, _} ->
                                     StreamError = true,
                                     []
                             end,
                    if 
                        StreamError ->
                            StreamErrCond = 
                                case xml_stream:parse_element(
                                       "<stream:stream>"++OutPacket) of
                                    El when element(1, El) == xmlelement ->
										case xml:get_subtag(El, "stream:error") of
											false ->
												null;
											{xmlelement, _, _, Cond} ->
												Cond
										end;
                                    {error, _E} ->
                                        null
                                end,
                            gen_fsm:sync_send_all_state_event(FsmRef,
                                                              stop),
                            case StreamErrCond of
                                null ->
                                    {200, ?HEADER,
                                     "<body type='terminate' "
                                     "condition='internal-server-error' "
                                     "xmlns='"++?NS_HTTP_BIND++"'/>"};
                                _ ->
                                    {200, ?HEADER,
                                     "<body type='terminate' "
                                     "condition='remote-stream-error' "
                                     "xmlns='"++?NS_HTTP_BIND++"'>" ++
                                     elements_to_string(StreamErrCond) ++
                                     "</body>"}
                            end;
                        true ->
                            {200, ?HEADER,
                             xml:element_to_string(
                               {xmlelement,"body",
                                [{"xmlns",
                                  ?NS_HTTP_BIND}],
                                OutEls})}
                    end
	    end
    end.

parse_request(Data) ->
    ?DEBUG("--- incoming data --- ~n~s~n --- END --- ",
	   [Data]),
    case xml_stream:parse_element(Data) of
	El when element(1, El) == xmlelement ->
	    {xmlelement, Name, Attrs, Els} = El,
	    Xmlns = xml:get_attr_s("xmlns",Attrs),
	    if 
		Name /= "body" -> 
		    {error, bad_request};
		Xmlns /= ?NS_HTTP_BIND ->
		    {error, bad_request};
		true ->
                    case list_to_integer(xml:get_attr_s("rid", Attrs)) of
                        {'EXIT', _} ->
                            {error, bad_request};
                        Rid ->
                            FixedEls = 
                                lists:filter(
                                  fun(I) -> 
                                          case I of 
                                              {xmlelement, _, _, _} ->
                                                  true;
                                              _ ->
                                                  false
                                          end
                                  end, Els),
                            lists:map(
                              fun(E) ->
                                      EXmlns = xml:get_tag_attr_s("xmlns",E),
                                      if 
                                          EXmlns == ?NS_CLIENT ->
                                              remove_tag_attr("xmlns",E);
                                          true ->
                                              ok
                                      end
                              end, FixedEls),
                            Payload = [xml:element_to_string(E) || 
                                          E <- FixedEls],
                            Sid = xml:get_attr_s("sid",Attrs),
                            {ok, {Sid, Rid, Attrs, Payload}}
                    end
	    end;
	{error, _Reason} ->
	    {error, bad_request}
    end.

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive
	{timeout, Timer, _} ->
	    ok
    after 0 ->
	    ok
    end.

hex(Bin) when binary(Bin) -> hex(binary_to_list(Bin));
hex([]) -> "";
hex([H|T]) -> 
	[A,B] = if 
		H == 0 -> "00";
		H < 16 -> [$0,element(H,{$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f})];
		true   -> erlang:integer_to_list(H,16)
	end,
	[A,B|hex(T)].

elements_to_string([]) ->
    [];
elements_to_string([El | Els]) ->
    xml:element_to_string(El) ++ elements_to_string(Els).


remove_tag_attr(Attr, El) ->
    case El of
        {xmlelement, Name, Attrs, Els} ->
            Attrs1 = lists:keydelete(Attr, 1, Attrs),
            {xmlelement, Name, Attrs1, Els};
        _ ->
            El
    end.

check_default_xmlns({xmlelement, Name, Attrs, Els} = El) ->
    EXmlns = xml:get_tag_attr_s("xmlns", El),
    if 
	EXmlns == "" ->
	    {xmlelement, Name, [{"xmlns", ?NS_CLIENT} | Attrs], Els};
	true ->
	    El
    end.
