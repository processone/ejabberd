-record(motd, {server = <<"">> :: binary(),
               packet = #xmlel{} :: xmlel()}).

-record(motd_users, {us = {<<"">>, <<"">>} :: {binary(), binary()} | '$1',
                     dummy = [] :: [] | '_'}).
