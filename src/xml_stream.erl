%%%----------------------------------------------------------------------
%%% File    : xml_stream.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 17 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(xml_stream).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/1, init/1, send_text/2]).

-define(XML_START, 0).
-define(XML_END,   1).
-define(XML_CDATA, 2).
-define(XML_ERROR, 3).

start(CallbackPid) ->
    spawn(?MODULE, init, [CallbackPid]).

init(CallbackPid) ->
    Port = open_port({spawn, expat_erl}, [binary]),
    loop(CallbackPid, Port, []).

loop(CallbackPid, Port, Stack) ->
    receive
	{Port, {data, Bin}} ->
	    Data = binary_to_term(Bin),
	    loop(CallbackPid, Port, process_data(CallbackPid, Stack, Data));
	{_From, {send, Str}} ->
	    Port ! {self(), {command, Str}},
	    loop(CallbackPid, Port, Stack)
    end.

process_data(CallbackPid, Stack, Data) ->
    case Data of
	{?XML_START, {Name, Attrs}} ->
	    if Stack == [] ->
		    gen_fsm:send_event(CallbackPid,
				       {xmlstreamstart, Name, Attrs});
	       true -> true
	    end,
	    [{xmlelement, Name, Attrs, []} | Stack];
	{?XML_END, EndName} ->
	    case Stack of
		[{xmlelement, Name, Attrs, Els} | Tail] ->
		    NewEl = {xmlelement, Name, Attrs, lists:reverse(Els)},
		    Len = length(Tail),
		    if
			Len >  1 -> add_subelement(NewEl, Tail);
			Len == 1 ->
			    gen_fsm:send_event(CallbackPid,
					       {xmlstreamelement, NewEl}),
			    Tail;
			Len == 0 ->
			    gen_fsm:send_event(CallbackPid,
					       {xmlstreamend, EndName}),
			    Tail
		    end
	    end;
	{?XML_CDATA, CData} ->
	    add_subelement({xmlcdata, CData}, Stack);
	{?XML_ERROR, Err} -> gen_fsm:send_event(CallbackPid,
						{xmlstreamerror, Err})
    end.


add_subelement(El, Stack) ->
    case Stack of
	[{xmlelement, Name, Attrs, Els} | Tail] ->
	    [{xmlelement, Name, Attrs, [El | Els]} | Tail];
	[] -> []
    end.


send_text(Pid, Text) ->
    Pid ! {self(), {send, Text}}.

