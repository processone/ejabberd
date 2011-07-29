%%%------------------------------------------------------------------------
%%% File:   ejabberd_smnp_hooks.erl
%%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%%%         Radoslaw Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>
%%% Description: Hooks that needs to be registered for SNMP counters to work.
%%%
%%% Created: 29 July by <aleksandra.lipiec@erlang-solutions.com>
%%%------------------------------------------------------------------------
-module(ejabberd_snmp_hooks).

-export([get_hooks/1]).


-include("ejabberd.hrl").


%%-------------------
%% Implementation
%%-------------------

%% Here will be declared which hooks should be registered
get_hooks(Host) ->
    [].


%%------------------------------
%% SNMP specific hook callbacks
%%------------------------------



