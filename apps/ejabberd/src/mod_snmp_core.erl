-module(mod_snmp_core).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("EJABBERD-MIB.hrl").
-include("mod_snmp.hrl").

-export([start/1,
         stop/0,
         is_started/0,
         increment_counter/1,
         decrement_counter/1,
         counter_value/1]).

start(Modules) ->
    ok.

stop() ->
    ok.

initialize_counters(_) ->
    ok.

-spec is_started() -> bool().
is_started() ->
    false.

increment_counter(Counter) ->
    ok.

decrement_counter(Counter) ->
    ok.

-spec counter_value(atom()) -> {value, term()}.
counter_value(Counter) ->
    {value, someVal}.
