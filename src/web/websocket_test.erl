-module (websocket_test).
-export([start_link/1, loop/1]).

% callback on received websockets data
start_link(Ws) ->
  Pid = spawn_link(?MODULE, loop, [Ws]),
  {ok, Pid}.

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
