-record(offline_msg,
	{us = {<<"">>, <<"">>} :: {binary(), binary()},
	 timestamp = now()     :: erlang:timestamp() | '_',
	 expire = now()        :: erlang:timestamp() | never | '_',
	 from = #jid{}         :: jid() | '_',
	 to = #jid{}           :: jid() | '_',
	 packet = #xmlel{}     :: xmlel() | '_'}).

-record(state,
	{host = <<"">> :: binary(),
	 access_max_offline_messages}).
