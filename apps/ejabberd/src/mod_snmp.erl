-module(mod_snmp).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("EJABBERD-MIB.hrl").

%% behaviour: gen_mod
-export([start/2, stop/1]).

%% snmp handlers
%-export([handle_entry/2, handle_entry/4]).

-define(CORE, ejabberd_snmp_core).

start(_Host, Opts) ->
    ?CORE:start([]),
    SampleOID = lists:foldl(fun(E,A) -> A ++ "." ++ integer_to_list(E) end,
        integer_to_list(hd(?ejabberd)), tl(?ejabberd)),
    %% These are the options from ejabberd.cfg
    SampleOpts = case Opts of
            [H|T] ->
                lists:foldl(fun(E,A) -> A ++ " " ++ atom_to_list(E) end,
                    atom_to_list(H), T);
            _ -> "none"
        end,
    ?INFO_MSG("mod_snmp started. Sample OID is: ~s~nSample opts: ~s", [SampleOID, SampleOpts]).

stop(_Host) ->
    ?CORE:stop().

%% The following is straight from ooVoo

%-spec(handle_entry/2 :: (get | new | delete, atom()) -> {value, term()} | ok).
%handle_entry(T, generalUptime)      -> ?UTILS:fun_value(T, getFun(generalUptime));
%%% Default for counters
%handle_entry(T, Counter) -> ?UTILS:counter_value(T, Counter).

%%% Tables
%-spec handle_entry(atom(), integer(), integer(), atom()) -> term().
%handle_entry(T, Row, Col, Table) -> ?UTILS:table(T, Row, Col, getTableData(Table)).
