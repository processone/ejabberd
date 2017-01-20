-type local_hint() :: undefined | integer() | {apply, atom(), atom()}.

-record(route, {domain :: binary(),
		server_host :: binary(),
		pid :: undefined | pid(),
		local_hint :: local_hint()}).
