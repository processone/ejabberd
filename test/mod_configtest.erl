-module(mod_configtest).
-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]).

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

mod_opt_type(mma) ->
    econf:atom();
mod_opt_type(mms) ->
    econf:binary();
mod_opt_type(mmsi) ->
    econf:binary();

mod_opt_type(kma) ->
    econf:atom();
mod_opt_type(kms) ->
    econf:binary();
mod_opt_type(kmsi) ->
    econf:binary();

mod_opt_type(predefined_keywords) ->
    econf:binary().

mod_options(_) ->
    [{mma, undefined},
     {mms, undefined},
     {mmsi, undefined},
     {kma, undefined},
     {kms, undefined},
     {kmsi, undefined},
     {predefined_keywords, undefined}].

mod_doc() ->
    #{}.
