%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_in.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_s2s_in).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_fsm).

%% External exports
-export([start/2,
	 start_link/2,
	 send_text/2,
	 send_element/2]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_stream/2,
	 stream_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DICT, dict).

-record(state, {socket,
		receiver,
		streamid,
		shaper,
	        connections = ?DICT:new(),
		timer}).


%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(STREAM_HEADER,
	("<?xml version='1.0'?>"
	 "<stream:stream "
	 "xmlns:stream='http://etherx.jabber.org/streams' "
	 "xmlns='jabber:server' "
	 "xmlns:db='jabber:server:dialback' "
	 "id='" ++ StateData#state.streamid ++ "'>")
       ).

-define(STREAM_TRAILER, "</stream:stream>").

-define(INVALID_NAMESPACE_ERR,
	xml:element_to_string(?SERR_INVALID_NAMESPACE)).

-define(HOST_UNKNOWN_ERR,
	xml:element_to_string(?SERR_HOST_UNKNOWN)).

-define(INVALID_XML_ERR,
	xml:element_to_string(?SERR_XML_NOT_WELL_FORMED)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_s2s_in_sup, [SockData, Opts]).

start_link(SockData, Opts) ->
    gen_fsm:start_link(ejabberd_s2s_in, [SockData, Opts], ?FSMOPTS).

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
init([{SockMod, Socket}, Opts]) ->
    ?INFO_MSG("started: ~p", [{SockMod, Socket}]),
    ReceiverPid = ejabberd_receiver:start(Socket, SockMod, none),
    Shaper = case lists:keysearch(shaper, 1, Opts) of
		 {value, {_, S}} -> S;
		 _ -> none
	     end,
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {ok, wait_for_stream,
     #state{socket = Socket,
	    receiver = ReceiverPid,
	    streamid = new_id(),
	    shaper = Shaper,
	    timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------

wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    % TODO
    case {xml:get_attr_s("xmlns", Attrs), xml:get_attr_s("xmlns:db", Attrs)} of
	{"jabber:server", "jabber:server:dialback"} ->
	    send_text(StateData#state.socket, ?STREAM_HEADER),
	    {next_state, stream_established, StateData#state{}};
	_ ->
	    send_text(StateData#state.socket, ?INVALID_NAMESPACE_ERR),
	    {stop, normal, StateData}
    end;

wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData#state.socket,
	      ?STREAM_HEADER ++ ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};

wait_for_stream(closed, StateData) ->
    {stop, normal, StateData}.

stream_established({xmlstreamelement, El}, StateData) ->
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    case is_key_packet(El) of
	{key, To, From, Id, Key} ->
	    ?INFO_MSG("GET KEY: ~p", [{To, From, Id, Key}]),
	    LTo = jlib:nameprep(To),
	    LFrom = jlib:nameprep(From),
	    case lists:member(LTo, ejabberd_router:dirty_get_all_domains()) of
		true ->
		    ejabberd_s2s_out:start(To, From,
					   {verify, self(),
					    Key, StateData#state.streamid}),
		    Conns = ?DICT:store({LFrom, LTo}, wait_for_verification,
					StateData#state.connections),
		    change_shaper(StateData, jlib:make_jid("", LFrom, "")),
		    {next_state,
		     stream_established,
		     StateData#state{connections = Conns,
				     timer = Timer}};
		_ ->
		    send_text(StateData#state.socket, ?HOST_UNKNOWN_ERR),
		    {stop, normal, StateData}
	    end;
	{verify, To, From, Id, Key} ->
	    ?INFO_MSG("VERIFY KEY: ~p", [{To, From, Id, Key}]),
	    LTo = jlib:nameprep(To),
	    LFrom = jlib:nameprep(From),
	    Key1 = ejabberd_s2s:get_key({LTo, LFrom}),
	    Type = if Key == Key1 -> "valid";
		      true -> "invalid"
		   end,
	    send_element(StateData#state.socket,
			 {xmlelement,
			  "db:verify",
			  [{"from", To},
			   {"to", From},
			   {"id", Id},
			   {"type", Type}],
			  []}),
	    {next_state, stream_established, StateData#state{timer = Timer}};
	_ ->
	    {xmlelement, Name, Attrs, _Els} = El,
	    From_s = xml:get_attr_s("from", Attrs),
	    From = jlib:string_to_jid(From_s),
	    To_s = xml:get_attr_s("to", Attrs),
	    To = jlib:string_to_jid(To_s),
	    if
		(To /= error) and (From /= error) ->
		    LFrom = From#jid.lserver,
		    LTo = To#jid.lserver,
		    case ?DICT:find({LFrom, LTo},
				    StateData#state.connections) of
			{ok, established} ->
			    if ((Name == "iq") or
				(Name == "message") or
				(Name == "presence")) ->
				    ejabberd_router:route(From, To, El);
			       true ->
				    error
			    end;
			_ ->
			    error
		    end;
		true ->
		    error
	    end,
	    {next_state, stream_established, StateData#state{timer = Timer}}
    end;

stream_established({valid, From, To}, StateData) ->
    send_element(StateData#state.socket,
		 {xmlelement,
		  "db:result",
		  [{"from", To},
		   {"to", From},
		   {"type", "valid"}],
		  []}),
    LFrom = jlib:nameprep(From),
    LTo = jlib:nameprep(To),
    NSD = StateData#state{
	    connections = ?DICT:store({LFrom, LTo}, established,
				      StateData#state.connections)},
    {next_state, stream_established, NSD};

stream_established({invalid, From, To}, StateData) ->
    send_element(StateData#state.socket,
		 {xmlelement,
		  "db:result",
		  [{"from", To},
		   {"to", From},
		   {"type", "invalid"}],
		  []}),
    LFrom = jlib:nameprep(From),
    LTo = jlib:nameprep(To),
    NSD = StateData#state{
	    connections = ?DICT:erase({LFrom, LTo},
				      StateData#state.connections)},
    {next_state, stream_established, NSD};

stream_established({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};

stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData#state.socket,
	      ?STREAM_HEADER ++ ?INVALID_XML_ERR ++ ?STREAM_TRAILER),
    {stop, normal, StateData};

stream_established(timeout, StateData) ->
    {stop, normal, StateData};

stream_established(closed, StateData) ->
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
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
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
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData#state.socket, Text),
    {next_state, StateName, StateData};

handle_info({timeout, Timer, _}, StateName,
	    #state{timer = Timer} = StateData) ->
    {stop, normal, StateData};

handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, _StateName, StateData) ->
%    case StateData#state.user of
%	"" ->
%	    ok;
%	_ ->
%	    %ejabberd_sm:close_session(StateData#state.user,
%	    %    		      StateData#state.resource)
%    end,
    %ejabberd_s2s ! {closed_conection, StateData#state.server},
    ?INFO_MSG("terminated: ~p", [Reason]),
    gen_tcp:close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(Socket, Text) ->
    gen_tcp:send(Socket,Text).

send_element(Socket, El) ->
    send_text(Socket, xml:element_to_string(El)).


change_shaper(StateData, JID) ->
    Shaper = acl:match_rule(StateData#state.shaper, JID),
    ejabberd_receiver:change_shaper(StateData#state.receiver, Shaper).


new_id() ->
    randoms:get_string().

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive
	{timeout, Timer, _} ->
	    ok
    after 0 ->
	    ok
    end.


is_key_packet({xmlelement, Name, Attrs, Els}) when Name == "db:result" ->
    {key,
     xml:get_attr_s("to", Attrs),
     xml:get_attr_s("from", Attrs),
     xml:get_attr_s("id", Attrs),
     xml:get_cdata(Els)};
is_key_packet({xmlelement, Name, Attrs, Els}) when Name == "db:verify" ->
    {verify,
     xml:get_attr_s("to", Attrs),
     xml:get_attr_s("from", Attrs),
     xml:get_attr_s("id", Attrs),
     xml:get_cdata(Els)};
is_key_packet(_) ->
    false.



