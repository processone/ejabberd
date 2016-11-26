-record(offline_msg,
	{us = {<<"">>, <<"">>} :: {binary(), binary()},
	 timestamp = p1_time_compat:timestamp() :: erlang:timestamp() | '_',
	 expire = p1_time_compat:timestamp() :: erlang:timestamp() | never | '_',
	 from = #jid{}         :: jid() | '_',
	 to = #jid{}           :: jid() | '_',
	 packet = #xmlel{}     :: xmlel() | '_'}).

-record(state,
	{host = <<"">> :: binary(),
	 access_max_offline_messages}).
