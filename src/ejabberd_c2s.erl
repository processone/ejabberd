%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_c2s).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_fsm).

%% External exports
-export([start/1, receiver/2, sender/1, send_text/2]).

%% gen_fsm callbacks
%-export([init/1, state_name/2, state_name/3, handle_event/3,
%	 handle_sync_event/4, handle_info/3, terminate/3]).
%
-export([init/1, wait_for_stream/2, wait_for_auth/2, terminate/3]).

-record(state, {socket, sender, receiver}).

-include("ejabberd.hrl").

%start_old(Socket) ->
%    spawn(?MODULE, init, [Socket]).

%init_old(Socket) ->
%    SenderPid = spawn(?MODULE, sender, [Socket]),
%    ReceiverPid = spawn(?MODULE, receiver, [Socket, self()]),
%    loop_old(Socket, SenderPid, ReceiverPid).
%
%loop_old(Socket, SenderPid, ReceiverPid) ->
%    receive
%	{xmlstreamstart, Name, Attrs} ->
%	    ?DEBUG("Socket(~p) -> XML Stream start~n"
%		   "	Name: ~s~n"
%		   "	Attrs: ~p~n", [Socket, Name, Attrs]),
%	    loop_old(Socket, SenderPid, ReceiverPid);
%	{xmlstreamend, Name} ->
%	    ?DEBUG("Socket(~p) -> XML Stream end~n"
%		   "	Name: ~s~n", [Socket, Name]),
%	    loop_old(Socket, SenderPid, ReceiverPid);
%	{xmlstreamelement, El} ->
%	    ?DEBUG("Socket(~p) -> XML Stream element~n"
%		   "	Element: ~p~n", [Socket, El]),
%	    loop_old(Socket, SenderPid, ReceiverPid);
%	{xmlstreamerror, Err} ->
%	    ?DEBUG("Socket(~p) -> XML Stream error~n"
%		   "	Error: ~p~n", [Socket, Err]),
%	    loop_old(Socket, SenderPid, ReceiverPid)
%    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(STREAM_HEADER,
	"<?xml version='1.0'?>"
	"<stream:stream xmlns='jabber:client' "
	"xmlns:stream='http://etherx.jabber.org/streams' "
	"id='~s' from='~s'>"
       ).

-define(STREAM_TRAILER, "</stream:stream>").

-define(INVALID_NS_ERR, "<stream:error>Invalid Namespace</stream:error>").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Socket) ->
    gen_fsm:start(ejabberd_c2s, [Socket], ?FSMOPTS).
%start_old(Socket) ->
%    spawn(?MODULE, init, [Socket]).

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
init([Socket]) ->
    SenderPid = spawn(?MODULE, sender, [Socket]),
    ReceiverPid = spawn(?MODULE, receiver, [Socket, self()]),
    {ok, wait_for_stream, #state{socket = Socket,
				 receiver = ReceiverPid,
				 sender = SenderPid}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
state_name(Event, StateData) ->
    {next_state, state_name, StateData}.

wait_for_stream({xmlstreamstart, Name, Attrs}, StateData) ->
    % TODO
    Header = io_lib:format(?STREAM_HEADER, ["SID", "localhost"]),
    send_text(StateData#state.sender, Header),
    case lists:keysearch("xmlns:stream", 1, Attrs) of
	{value, {"xmlns:stream", "http://etherx.jabber.org/streams"}} ->
	    % TODO
	    {next_state, wait_for_auth, StateData};
	_ ->
	    send_text(StateData#state.sender, ?INVALID_NS_ERR),
	    send_text(StateData#state.sender, ?STREAM_TRAILER),
	    {stop, normal, StateData}
    end;

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.


wait_for_auth({xmlstreamelement, El}, StateData) ->
    % TODO
    {next_state, wait_for_auth, StateData};

wait_for_auth({xmlstreamend, Name}, StateData) ->
    % TODO
    {stop, normal, StateData};

wait_for_auth(closed, StateData) ->
    {stop, normal, StateData}.




%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
state_name(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
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
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    StateData#state.sender ! close,
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

receiver(Socket, C2SPid) ->
    XMLStreamPid = xml_stream:start(C2SPid),
    receiver(Socket, C2SPid, XMLStreamPid).

receiver(Socket, C2SPid, XMLStreamPid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Text} ->
	    xml_stream:send_text(XMLStreamPid, Text),
	    receiver(Socket, C2SPid, XMLStreamPid);
        {error, closed} ->
	    exit(XMLStreamPid, closed),
	    gen_fsm:send_event(C2SPid, closed),
	    ok
    end.

sender(Socket) ->
    receive
	{text, Text} ->
	    gen_tcp:send(Socket,Text),
	    sender(Socket);
	close ->
	    gen_tcp:close(Socket),
	    ok
    end.

send_text(Pid, Text) ->
    Pid ! {text, Text}.

