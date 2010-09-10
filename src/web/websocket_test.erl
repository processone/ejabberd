-module (websocket_test).
-export([handle/1]).

% callback on received websockets data
handle(Ws) ->
	receive
		{browser, Data} ->
			Ws:send(["received '", Data, "'"]),
			handle(Ws);
		_Ignore ->
			handle(Ws)
	after 5000 ->
		Ws:send("pushing!"),
		handle(Ws)
	end.
