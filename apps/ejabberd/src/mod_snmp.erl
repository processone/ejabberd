-module(mod_snmp).

-behaviour(gen_mod).

-export([start/2,
         stop/1,
         handle_entry/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("EJABBERD-MIB.hrl").


start(_Host, _Opts) ->
    SampleOID = lists:foldl(fun(E,A) -> A ++ "." ++ integer_to_list(E) end,
        integer_to_list(hd(?ejabberd)), tl(?ejabberd)),
    ?INFO_MSG("mod_snmp started. Sample OID is: ~s", [SampleOID]).

stop(_Host) ->
    ok.

handle_entry(_Args) ->
    ok.
