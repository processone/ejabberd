-define(ROUTES_CACHE, routes_cache).

-type local_hint() :: integer() | {apply, atom(), atom()}.

-record(route, {domain :: binary(),
		server_host :: binary(),
		pid :: undefined | pid(),
		local_hint :: local_hint() | undefined}).
