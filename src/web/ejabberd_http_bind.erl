%%%----------------------------------------------------------------------
%%% File    : ejabberd_http_bind.erl
%%% Author  : Stefan Strigler <steve@zeank.in-berlin.de>
%%% Purpose : HTTP Binding support (JEP-0124)
%%% Created : 21 Sep 2005 by Stefan Strigler <steve@zeank.in-berlin.de>
%%%----------------------------------------------------------------------

-module(ejabberd_http_bind).
-author('steve@zeank.in-berlin.de').
-vsn('1.9').

-behaviour(gen_fsm).

%% External exports
-export([start_link/3,
	 init/1,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3,
	 send/2,
	 setopts/2,
	 controlling_process/2,
	 close/1,
	 process_request/1]).

-define(ejabberd_debug, true).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

-record(http_bind, {id, pid, to, hold, wait}).

%% http binding request
-record(hbr, {rid, key, in, out}).

-record(state, {id,
		rid = error,
		key,
		output = "",
		input = "",
		waiting_input = false,
		last_receiver,
		last_poll,
		ctime = 0,
		timer,
		req_list = [] % list of requests
	       }).

-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(BOSH_VERSION, "1.6").

-define(MAX_REQUESTS, 2).  % number of simultaneous requests
-define(MIN_POLLING, "2"). % don't poll faster than that or we will shoot you
-define(MAX_WAIT, 3600).     % max num of secs to keep a request on hold
-define(MAX_INACTIVITY, 30000). % msecs to wait before terminating idle sessions
-define(CT, {"Content-Type", "text/xml; charset=utf-8"}).
-define(HEADER, [?CT,{"X-Sponsored-By", "http://mabber.com"}]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Sid, Rid, Key) ->
    mnesia:create_table(http_bind,
        		[{ram_copies, [node()]},
        		 {attributes, record_info(fields, http_bind)}]),
    supervisor:start_child(ejabberd_http_bind_sup, [Sid, Rid, Key]).

start_link(Sid, Rid, Key) ->
    gen_fsm:start_link(?MODULE, [Sid, Rid, Key], ?FSMOPTS).

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

process_request(Data) ->
    case catch parse_request(Data) of
        {ok, {[], Attrs, Packet}} -> %% no session id - create a new one!
            ?DEBUG("no sid given. create a new session?", []),
            case xml:get_attr_s("to",Attrs) of
                [] -> %% missing 'to' - can't proceed
                    ?DEBUG("missing 'to' attribute, can't create session", []),
                    {200, ?HEADER, "<body type='terminate' "
                     "condition='improper-addressing' "
                     "xmlns='http://jabber.org/protocol/httpbind'/>"};
                XmppDomain ->
                    create_session(XmppDomain, Attrs, Packet)
            end;
	{ok, {Sid, Attrs, Packet}} ->
            case check_request(Sid, Attrs, Packet) of
                {error, not_exists} ->
                    ?DEBUG("no session associated with sid: ~p", [Sid]),
                    {404, ?HEADER, ""};
                {error, bad_key} ->
                    %%?DEBUG("bad key: ~s", [Key]),
                    case mnesia:dirty_read({http_bind, Sid}) of
                        [] ->
                            {404, ?HEADER, ""};
                        [#http_bind{pid = FsmRef}] ->
                            gen_fsm:sync_send_all_state_event(FsmRef,stop),
                            {404, ?HEADER, ""}		    
                    end;
                {repeat, OutPacket} ->
                    ?DEBUG("http_put said 'repeat!' ...~nOutPacket: ~p", 
                           [OutPacket]),
                    send_outpacket(Sid, OutPacket);
                {ok, Rid} ->
                    case http_put(Sid, Attrs, Packet) of
                        {error, polling_too_frequently} ->
                            ?DEBUG("polling too frequently: ~p", [Sid]),
                            case mnesia:dirty_read({http_bind, Sid}) of
                                [] -> %% unlikely! (?)
                                    {404, ?HEADER, ""};
                                [#http_bind{pid = FsmRef}] ->
                                    gen_fsm:sync_send_all_state_event(FsmRef,stop),
                                    {403, ?HEADER, ""}		    
                            end;
                        ok ->
                            receive_loop(Sid, Rid);
                        _ ->
                            {400, ?HEADER, ""}
                    end
            end;
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
init([Sid, Rid, Key]) ->
    ?INFO_MSG("started: ~p", [{Sid, Key}]),
    Opts = [], % TODO
    ejabberd_socket:start(ejabberd_c2s, ?MODULE, {http_bind, self()}, Opts),
%    {ok, C2SPid} = ejabberd_c2s:start({?MODULE, {http_bind, self()}}, Opts),
%    ejabberd_c2s:become_controller(C2SPid),
    Timer = erlang:start_timer(?MAX_INACTIVITY, self(), []),
    {ok, loop, #state{id = Sid,
                      rid = Rid,
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

handle_sync_event({check_request, Attrs, Packet, _Hold},
                  _From, check_key, StateData) ->
    Key = xml:get_attr_s("key", Attrs),
    case StateData#state.key of
        "" ->
            NewKey = xml:get_attr_s("newkey", Attrs),
            {next_state, check_rid, StateData#state{key = NewKey}};
        StateKey ->
            case httpd_util:to_lower(
                   hex(binary_to_list(
                         crypto:sha(Key)))) of
                StateKey ->
                    case xml:get_attr_s("newkey", Attrs) of
                        "" ->
                            {next_state, check_rid, StateData#state{key = Key}};
                        NewKey ->
                            {next_state, check_rid, StateData#state{key = NewKey}}
                    end;
                _ ->
                    Reply = {error, bad_key},
                    {reply, Reply, check_key, StateData}
            end
    end;

handle_sync_event({check_request, Attrs, Packet, Hold},
                  _From, check_rid = StateName, StateData) ->
    case string:to_integer(xml:get_attr_s("rid", Attrs)) of
        {error, _} ->
            Reply = {error, not_exists},
            {reply, Reply, StateName, StateData};
        {Rid, _} ->
            case StateData#state.rid of
                error ->
                    {reply, ok, request_checked, StateData#state{rid = Rid}};
                StateRid -> 
                    if 
                        (StateRid < Rid) and 
                        (Rid =< StateRid + Hold + 1) ->
                            {reply, {ok, Rid}, request_checked, StateData#state{rid = Rid}};
                        ((StateRid-Hold-1) < Rid )and 
                        (Rid =< StateRid) ->
                            %% Repeat request
                            [Out | _XS] = [El#hbr.out || 
                                              El <- StateData#state.req_list, 
                                              El#hbr.rid == Rid],
                            Reply = case Out of 
                                        [[] | OutPacket] ->
                                            {repeat, OutPacket};
                                        _ ->
                                            {repeat, Out}
                                    end,
                            {reply, Reply, StateName, 
                             StateData#state{input = "cancel"}};
                        true ->
                            Reply = {error, not_exists},
                            {reply, Reply, StateName, StateData}
                    end
            end
    end;

handle_sync_event({check_request, Attrs, Packet, Hold},
                  _From, _StateName, StateData) ->
    {next_state, check_key, StateData};

handle_sync_event({http_put, _Rid, Attrs, Packet, Hold},
                  _From, check_activity, StateData) ->
    ?DEBUG("check activity", []),
    {_,TSec,TMSec} = now(),
    TNow = TSec*1000*1000 + TMSec,
    LastPoll = if 
		   Packet == "" ->
		       TNow;
		   true ->
		       0
	       end,
    {MinPoll, _} = string:to_integer(?MIN_POLLING),
    if
	(Packet == "") and 
	(TNow - StateData#state.last_poll < MinPoll*1000*1000) ->
	    Reply = {error, polling_too_frequently},
	    {reply, Reply, send2server, StateData};
        true ->
            {next_state, send2server, StateData#state{last_poll = LastPoll}}
    end;

handle_sync_event({http_put, Rid, Packet, StartTo, Hold},
		  _From, send2server = StateName, StateData) ->

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

    {_,TSec,TMSec} = now(),
    TNow = TSec*1000*1000 + TMSec,

    case StateData#state.waiting_input of
        false ->
            cancel_timer(StateData#state.timer),
            Timer = erlang:start_timer(
                      ?MAX_INACTIVITY, self(), []),
            Input = Packet ++ [StateData#state.input],
            Reply = ok,
            {reply, Reply, StateName, 
             StateData#state{input = Input,
                             ctime = TNow,
                             timer = Timer,
                             req_list = ReqList
                            }};
        {Receiver, _Tag} ->
            SendPacket = 
                if 
                    StartTo /= "" ->
                        ["<stream:stream to='",
                         StartTo, 
                         "' xmlns='jabber:client' "
                         %% version='1.0' "
                         "xmlns:stream='http://etherx.jabber.org/streams'>"] ++ Packet;
                    true ->
                        Packet
                end,
            ?DEBUG("really sending now: ~s", [SendPacket]),
            Receiver ! {tcp, {http_bind, self()},
                        list_to_binary(SendPacket)},
            cancel_timer(StateData#state.timer),
            Timer = erlang:start_timer(
                      ?MAX_INACTIVITY, self(), []),
            Reply = ok,
            {reply, Reply, StateName,
             StateData#state{waiting_input = false,
                             last_receiver = Receiver,
                             input = "",
                             ctime = TNow,
                             timer = Timer,
                             req_list = ReqList
                            }}
    end;
handle_sync_event({http_put, Rid, Packet, StartTo, Hold},
		  _From, StateName, StateData) ->
    ?DEBUG("http-put checking acitivtiy", []),
    {next_state, check_activity, StateData};


handle_sync_event({http_get, Rid, Wait, Hold}, _From, StateName, StateData) ->
    {_,TSec,TMSec} = now(),
    TNow = TSec*1000*1000 + TMSec,
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?MAX_INACTIVITY, self(), []),
    if 
	(Hold > 0) and 
	(StateData#state.output == "") and 
	((TNow - StateData#state.ctime) < (Wait*1000*1000)) and 
	(StateData#state.rid == Rid) and 
	(StateData#state.input /= "cancel") ->
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
limit_val_max(Val, Max) when is_list(Val) ->
    case string:to_integer(Val) of
        {error, _} -> Max;
        {IntVal, _} -> limit_val_max(IntVal, Max)
    end;
limit_val_max(Val, Max) when is_integer(Val) and (Val =< Max) ->
    Val;
limit_val_max(Val, Max) when is_integer(Val) and (Val > Max) -> 
    Max;
limit_val_max(_, Max) -> Max.

create_session(XmppDomain, Attrs, Packet) ->
    case string:to_integer(xml:get_attr_s("rid", Attrs)) of
        {error, _} ->
            ?DEBUG("'rid' invalid", []),
            {400, ?HEADER, ""};
        {Rid, _} ->
            Sid = sha:sha(term_to_binary({now(), make_ref()})),
            Key = xml:get_attr_s("key", Attrs),
            {ok, Pid} = start(Sid, Rid, Key),
            Wait = limit_val_max(xml:get_attr_s("wait",Attrs), ?MAX_WAIT),
            Hold = limit_val_max(xml:get_attr_s("hold",Attrs), (?MAX_REQUESTS-1)),
            F = fun() -> 
                        mnesia:write(
                          #http_bind{id   = Sid,
                                     pid  = Pid,
                                     to   = XmppDomain,
                                     wait = Wait,
                                     hold = Hold})
                end,
            case catch mnesia:transaction(F) of
                {atomic, ok} ->
                    ?DEBUG("created session with sid: ~s", [Sid]),
                    case http_put(Sid, Attrs, Packet) of
                        ok ->
                            receive_loop(Sid, Rid);
                        _ ->
                            {400, ?HEADER, ""}
                    end;
                _E ->
                    ?DEBUG("error creating session: ~p", [_E]),
                    close({http_bind, Pid}),
                    {200, ?HEADER,
                     "<body type='terminate' "
                     "condition='internal-server-error' "
                     "xmlns='http://jabber.org/protocol/httpbind'/>"}
            end
    end.

receive_loop(Sid, Rid) ->
    receive
	after 100 -> ok
	end,
    prepare_response(Sid, Rid).

prepare_response(Sid, Rid) ->
    case http_get(Sid,Rid) of
	{error, not_exists} ->
            ?DEBUG("no session associated with sid: ~s", Sid),
	    {404, ?HEADER, ""};
	{ok, keep_on_hold} ->
	    receive_loop(Sid, Rid);
	{ok, cancel} ->
	    %% actually it would be better if we could completely
	    %% cancel this request, but then we would have to hack
	    %% ejabberd_http and I'm too lazy now
	    {404, ?HEADER, ""}; 
	{ok, OutPacket} ->
            ?DEBUG("OutPacket: ~s", [OutPacket]),
            send_outpacket(Sid, OutPacket);
	{ok, stream_start, OutPacket} ->
            ?DEBUG("OutPacket: ~s", [OutPacket]),
            OutEls = case xml_stream:parse_element(
                            OutPacket++"</stream:stream>") of
                         El when element(1, El) == xmlelement ->
                             {xmlelement, _, OutAttrs, Els} = El,
                             AuthID = xml:get_attr_s("id", OutAttrs),
                             StreamError = false,
                             case Els of
                                 [] ->
                                     [];
                                 [{xmlelement, "stream:features", StreamAttribs, StreamEls} | StreamTail] ->
                                     [{xmlelement, "stream:features", [{"xmlns:stream","http://etherx.jabber.org/streams"}] ++ StreamAttribs, StreamEls}] ++ StreamTail;
                                 Xml ->
                                     Xml
                             end;
                         {error, _} ->
                             AuthID = "",
                             StreamError = true,
                             []
                     end,
%            To = xml:get_attr_s("to",Attrs),
            if
%                To == "" ->
%                    {200, ?HEADER, "<body type='terminate' "
%                     "condition='improper-addressing' "
%                     "xmlns='http://jabber.org/protocol/httpbind'/>"};
                StreamError == true ->
                    {200, ?HEADER, "<body type='terminate' "
                     "condition='host-unknown' "
                     "xmlns='http://jabber.org/protocol/httpbind'/>"};
                true ->
                    case mnesia:dirty_read({http_bind, Sid}) of 
                        [#http_bind{wait = Wait, hold = Hold}] ->
                            {200, ?HEADER,
                             xml:element_to_string(
                               {xmlelement,"body",
                                [{"xmlns",
                                  "http://jabber.org/protocol/httpbind"},
                                 {"sid",Sid},
                                 {"wait", integer_to_list(Wait)},
                                 {"requests", integer_to_list(Hold+1)},
                                 {"inactivity", 
                                  integer_to_list(trunc(?MAX_INACTIVITY/1000))},
                                 {"polling", ?MIN_POLLING},
                                 {"authid", AuthID}
                                ],OutEls})};
                        _ ->
                            {404, ?HEADER, ""}
                    end
            end
    end.
    
send_outpacket(Sid, []) ->
    {200, ?HEADER, "<body xmlns='http://jabber.org/protocol/httpbind'/>"};
send_outpacket(Sid, "</stream:stream>") ->
    case mnesia:dirty_read({http_bind, Sid}) of
        [#http_bind{pid = FsmRef}] ->
            gen_fsm:sync_send_all_state_event(FsmRef,stop)
    end,
    {200, ?HEADER, "<body xmlns='http://jabber.org/protocol/httpbind'/>"};
send_outpacket(Sid, OutPacket) ->
    case xml_stream:parse_element("<body>" 
                                  ++ OutPacket
                                  ++ "</body>") of
        El when element(1, El) == xmlelement ->
            {xmlelement, _, _, OEls} = El,
            TypedEls = [xml:replace_tag_attr("xmlns",
                                             "jabber:client",OEl) ||
                           OEl <- OEls],
            ?DEBUG(" --- outgoing data --- ~n~s~n --- END --- ~n",
                   [xml:element_to_string(
                      {xmlelement,"body",
                       [{"xmlns",
                         "http://jabber.org/protocol/httpbind"}],
                       TypedEls})]
                  ),
            {200, ?HEADER,
             xml:element_to_string(
               {xmlelement,"body",
                [{"xmlns",
                  "http://jabber.org/protocol/httpbind"}],
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
                                 [{xmlelement, "stream:features", StreamAttribs, StreamEls} | StreamTail] ->
                                     TypedTail = [xml:replace_tag_attr("xmlns",
                                                                       "jabber:client",OEl) ||
                                                     OEl <- StreamTail],
                                     [{xmlelement, "stream:features", [{"xmlns:stream","http://etherx.jabber.org/streams"}] ++ StreamAttribs, StreamEls}] ++ TypedTail;
                                 Xml ->
                                     Xml
                             end;
                         {error, _} ->
                             StreamError = true,
                             []
                     end,
            if 
                StreamError ->
                    StreamErrCond = case xml_stream:parse_element(
                                           "<stream:stream>"++OutPacket) of
                                        El when element(1, El) == xmlelement ->
                                            {xmlelement, _Tag, _Attr, Els} = El,
                                            [{xmlelement, SE, _, Cond} | _] = Els,
                                            if 
                                                SE == "stream:error" ->
                                                    Cond;
                                                true ->
                                                    null
                                            end;
                                        {error, _E} ->
                                            null
                                    end,
                    case mnesia:dirty_read({http_bind, Sid}) of
                        [#http_bind{pid = FsmRef}] ->
                            gen_fsm:sync_send_all_state_event(FsmRef,stop);
                        _ ->
                            err %% hu?
                    end,
                    case StreamErrCond of
                        null ->
                            {200, ?HEADER,
                             "<body type='terminate' "
                             "condition='internal-server-error' "
                             "xmlns='http://jabber.org/protocol/httpbind'/>"};
                        _ ->
                            {200, ?HEADER,
                             "<body type='terminate' "
                             "condition='remote-stream-error' "
                             "xmlns='http://jabber.org/protocol/httpbind'>" ++
                             elements_to_string(StreamErrCond) ++
                             "</body>"}
                    end;
                true ->
                    {200, ?HEADER,
                     xml:element_to_string(
                       {xmlelement,"body",
                        [{"xmlns",
                          "http://jabber.org/protocol/httpbind"}],
                        OutEls})}
            end
    end.

check_request(Sid, Attrs, Packet) ->
    case mnesia:dirty_read({http_bind, Sid}) of
	[] ->
            ?DEBUG("not found",[]),
	    {error, not_exists};
	[#http_bind{pid = FsmRef, hold = Hold}] ->
            gen_fsm:sync_send_all_state_event(
              FsmRef, {http_put, Attrs, Packet, Hold})
    end.
            

http_put(Sid, Attrs, Packet) ->
    ?DEBUG("http-put",[]),
    {Rid, _} = string:to_integer(xml:get_attr_s("rid", Attrs)),
    To = xml:get_attr_s("to", Attrs),
    case mnesia:dirty_read({http_bind, Sid}) of
	[] ->
            ?DEBUG("not found",[]),
	    {error, not_exists};
	[#http_bind{pid = FsmRef, to = To, hold = Hold}] ->
            gen_fsm:sync_send_all_state_event(
              FsmRef, {http_put, Rid, Packet, To, Hold})
    end.

http_get(Sid, Rid) ->
    case mnesia:dirty_read({http_bind, Sid}) of
	[] ->
	    {error, not_exists};
	[#http_bind{pid = FsmRef, wait = Wait, hold = Hold}] ->
	    gen_fsm:sync_send_all_state_event(FsmRef, 
					      {http_get, Rid, Wait, Hold})
    end.


parse_request(Data) ->
    ?DEBUG("--- incoming data --- ~n~s~n --- END --- ",
	   [Data]),
    case catch xml_stream:parse_element(Data) of
        {xmlelement, "body", Attrs, Els} ->
            case xml:get_attr_s("xmlns",Attrs) of
		"http://jabber.org/protocol/httpbind" ->
                    Sid = xml:get_attr_s("sid",Attrs),
                    %% normalize tree - actually not needed by XEP but
                    %% where playing nicely here
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
                    %% fix namespace of child element
                    lists:map(fun(E) ->
                                      case xml:get_tag_attr_s("xmlns",E) of
                                          "jabber:client" ->
                                              remove_tag_attr("xmlns",E);
                                          true ->
                                              ok
                                      end
                              end, FixedEls),
                    %% revert to string
                    Packet = [xml:element_to_string(E) || E <- FixedEls],
		    {ok, {Sid, Attrs, Packet}};
                _ -> %% bad namespace
		    {error, bad_request}
            end;
	_ ->
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
