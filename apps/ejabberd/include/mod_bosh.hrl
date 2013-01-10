-define(BOSH_BACKEND, (mod_bosh_dynamic:backend())).
-define(BOSH_SOCKET_SUP, ejabberd_mod_bosh_socket_supervisor).

-type bosh_sid() :: binary().

-record(bosh_session, {sid :: bosh_sid(),
                       c2s_pid :: pid()}).

-record(bosh_socket, {pid :: pid(),
                      peer :: {inet:ip_address(), inet:port_number()}}).
