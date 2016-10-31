-record(sr_group, {group_host = {<<"">>, <<"">>} :: {'$1' | binary(), '$2' | binary()},
                   opts = [] :: list() | '_' | '$2'}).

-record(sr_user, {us = {<<"">>, <<"">>} :: {binary(), binary()},
                  group_host = {<<"">>, <<"">>} :: {binary(), binary()}}).
