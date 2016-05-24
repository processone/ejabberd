-ifndef(EJABBERD_SM_HRL).
-define(EJABBERD_SM_HRL, true).

-record(session, {sid, usr, us, priority, info}).
-record(session_counter, {vhost, count}).
-type sid() :: {erlang:timestamp(), pid()} | {erlang:timestamp(), undefined}.
-type ip() :: {inet:ip_address(), inet:port_number()} | undefined.
-type info() :: [{conn, atom()} | {ip, ip()} | {node, atom()}
                 | {oor, boolean()} | {auth_module, atom()}
                 | {num_stanzas_in, non_neg_integer()}].
-type prio() :: undefined | integer().

-endif.
