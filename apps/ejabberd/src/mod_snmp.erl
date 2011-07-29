%%%------------------------------------------------------------------------
%%% File:   mod_snmp.erl
%%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%%%         Radoslaw Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>
%%% Description: Snmp module for ejabberd
%%%
%%% Created: 28 July by <radoslaw.szymczyszyn@erlang-solutions.com>
%%%------------------------------------------------------------------------
-module(mod_snmp).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("EJABBERD-MIB.hrl").

%% behaviour: gen_mod
-export([start/2, stop/1]).

%% snmp handlers
-export([handle_entry/2, handle_entry/4]).

-spec(start/2 :: (binary(), list()) -> ok).
start(Host, Opts) ->
    case ejabberd_snmp_core:is_started() of
        true ->
            ok;
        false ->
            init_snmp(Host, Opts)
    end.

-spec(stop/1 :: (binary()) -> ok).
stop(Host) ->
    %% snmp stop?
    ejabberd_snmp_core:stop(),
    snmp_hooks(delete, Host).

-spec(init_snmp/2 :: (binary(), list()) -> ok).
init_snmp(Host, Opts) ->
    Mods = case gen_mod:get_opt(modules, Opts, none) of
               none ->
                   [];
               List ->
                   List
           end,
    ok = ejabberd_snmp_core:start(Mods),
    snmp_hooks(add, Host),
    
    SampleOID = lists:foldl(fun(E,A) -> A ++ "." ++ integer_to_list(E) end,
        integer_to_list(hd(?ejabberd)), tl(?ejabberd)),
    %% These are the options from ejabberd.cfg
    SampleMods = case Mods of
             [H|T] ->
                 lists:foldl(fun(E,A) -> A ++ " " ++ atom_to_list(E) end,
                     atom_to_list(H), T);
             _ -> "none"
         end,
    
    ?INFO_MSG("mod_snmp started. Sample OID is: ~s~nSample opts: ~s",
              [SampleOID, SampleMods]),

    PrivDir = case code:priv_dir(ejabberd) of
                  {error, _} ->
                      ".";
                  Path ->
                      Path
              end,
    SNMPdir = filename:join([PrivDir, "snmp"]),
    MIBdir = filename:join([PrivDir, "mibs"]),

    case catch do_init_snmp(SNMPdir, MIBdir) of
        ok ->
            ok;
        {'EXIT', Error} ->
            snmp_error(Error)
    end.

-spec(snmp_hooks/2 :: (add | delete, string()) -> ok).
snmp_hooks(Op, Host) ->
    lists:foreach(fun(Hook) ->
                          apply(ejabberd_hooks, Op, Hook)
                  end, ejabberd_snmp_hooks:get_hooks(Host)).
    
-spec(do_init_snmp/2 :: (list(), list()) -> ok).
do_init_snmp(SNMPdir, MIBdir) ->
    code:add_patha(MIBdir),
    application:set_env(snmp, agent, [
                                     {config, [{dir, filename:join(SNMPdir, "conf")}]},
                                     {db_dir, filename:join(SNMPdir, "db")}]),
    ok = application:start(snmp, permanent),
    %% Deploy basic subagent
    Master = whereis(snmp_master_agent),
    {ok, _} = snmpa_supervisor:start_sub_agent(Master,
                                               ?ejabberdMIB,
                                               [code:where_is_file("EJABBERD-MIB.bin")]),
    ok.

-spec(snmp_error/1 :: (string()) -> ok).
snmp_error(Error) ->
    ?ERROR_MSG("Can't initialize SNMP. Error: ~p~nSNMP will not be active",
               [Error]).
    
    
-spec(handle_entry/2 :: (get | new | delete, atom()) -> {value, term()} | ok).
%handle_entry(T, generalUptime)      -> ?UTILS:fun_value(T, getFun(generalUptime));
%%% Default for counters
handle_entry(get, Counter) -> 
    ejabberd_snmp_core:counter_value(Counter);
handle_entry(_T, _Counter) ->
    ok.


%%% Tables
-spec(handle_entry/4 :: (atom(), integer(), integer(), atom()) -> term()).
handle_entry(T, Row, Col, Table) -> 
    ejabberd_snmp_core:table_value(T, Row, Col, Table).
