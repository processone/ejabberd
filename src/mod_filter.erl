-module(mod_filter).
-author('n.shukla722@gmail.com').
behaviour(gen_mod).

export([start/2, stop/1, get_protected_names/0]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    ok.

stop(Host) ->
    ok.

get_protected_names() ->
    ["NamesTo", "FilterAway", "ToPrevent", "UnwantedSpoofing"].
