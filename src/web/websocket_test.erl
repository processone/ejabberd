-module (websocket_test).
-export([start/1, loop/1]).

% callback on received websockets data
start(Ws) ->
  spawn(?MODULE, loop, [Ws]).

loop(Ws) ->
	receive
		{browser, Data} ->
			Ws:send(["received '", Data, "'"]),
			loop(Ws);
		_Ignore ->
			loop(Ws)
	after 5000 ->
		Ws:send("pushing!"),
		loop(Ws)
	end.
