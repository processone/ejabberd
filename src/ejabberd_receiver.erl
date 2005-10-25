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
    XMLStreamState = xml_stream:new(C2SPid),
    ShaperState = shaper:new(Shaper),
    Timeout = case SockMod of
		  ssl ->
		      20;
		  _ ->
		      infinity
	      end,
    receiver(Socket, SockMod, ShaperState, C2SPid, XMLStreamState, Timeout).

receiver(Socket, SockMod, ShaperState, C2SPid, XMLStreamState, Timeout) ->
    Res = (catch SockMod:recv(Socket, 0, Timeout)),
    receive
	{starttls, TLSSocket} ->
	    xml_stream:close(XMLStreamState),
	    XMLStreamState1 = xml_stream:new(C2SPid),
	    TLSRes = case Res of
			 {ok, Data} ->
			     tls:recv_data(TLSSocket, Data);
			 _ ->
			     tls:recv_data(TLSSocket, "")
		     end,
	    receiver1(TLSSocket, tls,
		      ShaperState, C2SPid, XMLStreamState1, Timeout,
		      TLSRes);
	{change_timeout, NewTimeout} -> % Dirty hack
	    receiver1(Socket, SockMod,
		      ShaperState, C2SPid, XMLStreamState, NewTimeout,
		      Res)
    after 0 ->
	    receiver1(Socket, SockMod,
		      ShaperState, C2SPid, XMLStreamState, Timeout,
		      Res)
    end.


receiver1(Socket, SockMod, ShaperState, C2SPid, XMLStreamState, Timeout, Res) ->
    case Res of
        {ok, Text} ->
	    ShaperSt1 = receive
			    {change_shaper, Shaper} ->
				shaper:new(Shaper)
			after 0 ->
				ShaperState
			end,
	    NewShaperState = shaper:update(ShaperSt1, size(Text)),
	    XMLStreamState1 = receive
				  reset_stream ->
				      xml_stream:close(XMLStreamState),
				      xml_stream:new(C2SPid)
			      after 0 ->
				      XMLStreamState
			      end,
	    XMLStreamState2 = xml_stream:parse(XMLStreamState1, Text),
	    receiver(Socket, SockMod, NewShaperState, C2SPid, XMLStreamState2,
		     Timeout);
	{error, timeout} ->
	    receiver(Socket, SockMod, ShaperState, C2SPid, XMLStreamState,
		     Timeout);
        {error, Reason} ->
	    xml_stream:close(XMLStreamState),
	    gen_fsm:send_event(C2SPid, closed),
	    ok;
	{'EXIT', Reason} ->
	    ?ERROR_MSG("(~w) abnormal ~w:recv termination:~n\t~p~n",
		       [Socket, SockMod, Reason]),
	    xml_stream:close(XMLStreamState),
	    gen_fsm:send_event(C2SPid, closed),
	    ok
    end.


change_shaper(Pid, Shaper) ->
    Pid ! {change_shaper, Shaper}.

reset_stream(Pid) ->
    Pid ! reset_stream.

starttls(Pid, TLSSocket) ->
    Pid ! {starttls, TLSSocket}.


