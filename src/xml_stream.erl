%%%----------------------------------------------------------------------
%%% File    : xml_stream.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Parse XML streams
%%% Created : 17 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(xml_stream).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/1, start/2,
	 init/1, init/2,
	 send_text/2,
	 new/1,
	 parse/2,
	 close/1,
	 parse_element/1]).

-define(XML_START, 0).
-define(XML_END,   1).
-define(XML_CDATA, 2).
-define(XML_ERROR, 3).

-define(PARSE_COMMAND, 0).
-define(PARSE_FINAL_COMMAND, 1).

-record(xml_stream_state, {callback_pid, port, stack}).

start(CallbackPid) ->
    spawn(?MODULE, init, [CallbackPid]).

start(Receiver, CallbackPid) ->
    spawn(?MODULE, init, [Receiver, CallbackPid]).

init(CallbackPid) ->
    Port = open_port({spawn, expat_erl}, [binary]),
    loop(CallbackPid, Port, []).

init(Receiver, CallbackPid) ->
    erlang:monitor(process, Receiver),
    Port = open_port({spawn, expat_erl}, [binary]),
    loop(CallbackPid, Port, []).

loop(CallbackPid, Port, Stack) ->
    receive
	{Port, {data, Bin}} ->
	    Data = binary_to_term(Bin),
	    loop(CallbackPid, Port, process_data(CallbackPid, Stack, Data));
	{_From, {send, Str}} ->
	    Res = port_control(Port, ?PARSE_COMMAND, Str),
	    NewStack = lists:foldl(
			 fun(Data, St) ->
				 process_data(CallbackPid, St, Data)
			 end, Stack, binary_to_term(Res)),
	    loop(CallbackPid, Port, NewStack);
	{'DOWN', _Ref, _Type, _Object, _Info} ->
	    ok
    end.

process_data(CallbackPid, Stack, Data) ->
    case Data of
	{?XML_START, {Name, Attrs}} ->
	    if
		Stack == [] ->
		    catch gen_fsm:send_event(CallbackPid,
					     {xmlstreamstart, Name, Attrs});
		true ->
		    ok
	    end,
	    [{xmlelement, Name, Attrs, []} | Stack];
	{?XML_END, EndName} ->
	    case Stack of
		[{xmlelement, Name, Attrs, Els} | Tail] ->
		    NewEl = {xmlelement, Name, Attrs, lists:reverse(Els)},
		    case Tail of
			[] ->
			    catch gen_fsm:send_event(CallbackPid,
						     {xmlstreamend, EndName}),
			    Tail;
			[_] ->
			    catch gen_fsm:send_event(CallbackPid,
						     {xmlstreamelement, NewEl}),
			    Tail;
			[{xmlelement, Name1, Attrs1, Els1} | Tail1] ->
			    [{xmlelement, Name1, Attrs1, [NewEl | Els1]} |
			     Tail1]
		    end
	    end;
	{?XML_CDATA, CData} ->
	    case Stack of
		[El] ->
		    [El];
		[{xmlelement, Name, Attrs, Els} | Tail] ->
		    [{xmlelement, Name, Attrs, [{xmlcdata, CData} | Els]} |
		     Tail];
		[] -> []
	    end;
	{?XML_ERROR, Err} ->
	    catch gen_fsm:send_event(CallbackPid, {xmlstreamerror, Err})
    end.


send_text(Pid, Text) ->
    Pid ! {self(), {send, Text}}.


new(CallbackPid) ->
    Port = open_port({spawn, expat_erl}, [binary]),
    #xml_stream_state{callback_pid = CallbackPid,
		      port = Port,
		      stack = []}.


parse(#xml_stream_state{callback_pid = CallbackPid,
			port = Port,
			stack = Stack} = State, Str) ->
    Res = port_control(Port, ?PARSE_COMMAND, Str),
    NewStack = lists:foldl(
		 fun(Data, St) ->
			 process_data(CallbackPid, St, Data)
		 end, Stack, binary_to_term(Res)),
    State#xml_stream_state{stack = NewStack}.

close(#xml_stream_state{port = Port}) ->
    port_close(Port).


parse_element(Str) ->
    Port = open_port({spawn, expat_erl}, [binary]),
    Res = port_control(Port, ?PARSE_FINAL_COMMAND, Str),
    port_close(Port),
    process_element_events(binary_to_term(Res)).

process_element_events(Events) ->
    process_element_events(Events, []).

process_element_events([], _Stack) ->
    {error, parse_error};
process_element_events([Event | Events], Stack) ->
    case Event of
	{?XML_START, {Name, Attrs}} ->
	    process_element_events(
	      Events, [{xmlelement, Name, Attrs, []} | Stack]);
	{?XML_END, _EndName} ->
	    case Stack of
		[{xmlelement, Name, Attrs, Els} | Tail] ->
		    NewEl = {xmlelement, Name, Attrs, lists:reverse(Els)},
		    case Tail of
			[] ->
			    if
				Events == [] ->
				    NewEl;
				true ->
				    {error, parse_error}
			    end;
			[{xmlelement, Name1, Attrs1, Els1} | Tail1] ->
			    process_element_events(
			      Events,
			      [{xmlelement, Name1, Attrs1, [NewEl | Els1]} |
			       Tail1])
		    end
	    end;
	{?XML_CDATA, CData} ->
	    case Stack of
		[{xmlelement, Name, Attrs, Els} | Tail] ->
		    process_element_events(
		      Events, 
		      [{xmlelement, Name, Attrs, [{xmlcdata, CData} | Els]} |
		       Tail]);
		[] ->
		    process_element_events(Events, [])
	    end;
	{?XML_ERROR, Err} ->
	    {error, Err}
    end.

