%%%----------------------------------------------------------------------
%%% File    : ejabberd_receiver.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Socket receiver for C2S and S2S connections
%%% Created : 10 Nov 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_receiver).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/3,
	 receiver/4,
	 change_shaper/2,
	 reset_stream/1,
	 starttls/2]).

-include("ejabberd.hrl").


start(Socket, SockMod, Shaper) ->
    proc_lib:spawn(?MODULE, receiver, [Socket, SockMod, Shaper, self()]).


receiver(Socket, SockMod, Shaper, C2SPid) ->
    XMLStreamPid = xml_stream:start(self(), C2SPid),
    ShaperState = shaper:new(Shaper),
    Timeout = case SockMod of
		  ssl ->
		      20;
		  _ ->
		      infinity
	      end,
    receiver(Socket, SockMod, ShaperState, C2SPid, XMLStreamPid, Timeout).

receiver(Socket, SockMod, ShaperState, C2SPid, XMLStreamPid, Timeout) ->
    Res = (catch SockMod:recv(Socket, 0, Timeout)),
    case Res of
        {ok, Data} ->
	    receive
		{starttls, TLSSocket} ->
		    exit(XMLStreamPid, closed),
		    XMLStreamPid1 = xml_stream:start(self(), C2SPid),
		    TLSRes = tls:recv_data(TLSSocket, Data),
		    receiver1(TLSSocket, tls,
			      ShaperState, C2SPid, XMLStreamPid1, Timeout,
			      TLSRes)
	    after 0 ->
		    receiver1(Socket, SockMod,
			      ShaperState, C2SPid, XMLStreamPid, Timeout,
			      Res)
	    end;
	_ ->
	    receiver1(Socket, SockMod,
		      ShaperState, C2SPid, XMLStreamPid, Timeout, Res)
    end.


receiver1(Socket, SockMod, ShaperState, C2SPid, XMLStreamPid, Timeout, Res) ->
    case Res of
        {ok, Text} ->
	    ShaperSt1 = receive
			    {change_shaper, Shaper} ->
				shaper:new(Shaper)
			after 0 ->
				ShaperState
			end,
	    NewShaperState = shaper:update(ShaperSt1, size(Text)),
	    XMLStreamPid1 = receive
				reset_stream ->
				    exit(XMLStreamPid, closed),
				    xml_stream:start(self(), C2SPid)
			    after 0 ->
				    XMLStreamPid
			    end,
	    xml_stream:send_text(XMLStreamPid1, Text),
	    receiver(Socket, SockMod, NewShaperState, C2SPid, XMLStreamPid1,
		     Timeout);
	{error, timeout} ->
	    receiver(Socket, SockMod, ShaperState, C2SPid, XMLStreamPid,
		     Timeout);
        {error, Reason} ->
	    exit(XMLStreamPid, closed),
	    gen_fsm:send_event(C2SPid, closed),
	    ok;
	{'EXIT', Reason} ->
	    ?ERROR_MSG("(~w) abnormal ~w:recv termination:~n\t~p~n",
		       [Socket, SockMod, Reason]),
	    exit(XMLStreamPid, closed),
	    gen_fsm:send_event(C2SPid, closed),
	    ok
    end.


change_shaper(Pid, Shaper) ->
    Pid ! {change_shaper, Shaper}.

reset_stream(Pid) ->
    Pid ! reset_stream.

starttls(Pid, TLSSocket) ->
    Pid ! {starttls, TLSSocket}.


