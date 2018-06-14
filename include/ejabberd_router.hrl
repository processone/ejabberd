-define(ROUTES_CACHE, routes_cache).

-type local_hint() :: integer() | {apply, atom(), atom()}.

-type balancing_algorithm() :: dynamic | fix_number | consistent_hashing.

-record(route, {domain :: binary() | '_',
		server_host :: binary() | '_',
		pid :: undefined | pid(),
		local_hint :: local_hint() | undefined | '_'}).
