-type local_hint() :: integer() | {apply, atom(), atom()}.

-record(route, {domain :: binary() | '_',
		server_host :: binary() | '_',
		pid :: undefined | pid(),
		local_hint :: local_hint() | undefined | '_'}).
