-record(archive_msg,
	{us = {<<"">>, <<"">>}                :: {binary(), binary()} | '$2',
	 id = <<>>                            :: binary() | '_',
	 timestamp = p1_time_compat:timestamp() :: erlang:timestamp() | '_' | '$1',
	 peer = {<<"">>, <<"">>, <<"">>}      :: ljid() | '_' | '$3' | undefined,
	 bare_peer = {<<"">>, <<"">>, <<"">>} :: ljid() | '_' | '$3',
	 packet = #xmlel{}                    :: xmlel() | message() | '_',
	 nick = <<"">>                        :: binary(),
	 type = chat                          :: chat | groupchat}).

-record(archive_prefs,
	{us = {<<"">>, <<"">>} :: {binary(), binary()},
	 default = never       :: never | always | roster,
	 always = []           :: [ljid()],
	 never = []            :: [ljid()]}).
