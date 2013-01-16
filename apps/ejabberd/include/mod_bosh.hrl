-define(BOSH_BACKEND, (mod_bosh_dynamic:backend())).
-define(BOSH_SOCKET_SUP, ejabberd_mod_bosh_socket_sup).

-type bosh_sid() :: binary().

-record(bosh_session, {sid :: bosh_sid(),
                       socket :: pid()}).

-record(bosh_socket, {sid :: bosh_sid(),
                      pid :: pid(),
                      peer :: {inet:ip_address(), inet:port_number()}}).
