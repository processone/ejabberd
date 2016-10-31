-type conn_param() :: {binary(), binary(), inet:port_number(), binary()} |
                      {binary(), binary(), inet:port_number()} |
                      {binary(), binary()} |
                      {binary()}.

-type irc_data() :: [{username, binary()} | {connections_params, [conn_param()]}].

-record(irc_connection,
        {jid_server_host = {#jid{}, <<"">>, <<"">>} :: {jid(), binary(), binary()},
         pid = self()                               :: pid()}).

-record(irc_custom,
        {us_host = {{<<"">>, <<"">>}, <<"">>} :: {{binary(), binary()},
                                                  binary()},
         data = [] :: irc_data()}).
