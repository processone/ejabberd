-module(snmp_helper).

-include_lib("common_test/include/ct.hrl").

-export([assert_counter/2,
         get_counter_value/1,
         get_table_value/3,
         get_next_table_value/3]).


%%--------------------------
%% Implementation
%%--------------------------

assert_counter(Value, Counter) ->
    {value, Value} = rpc:call(ct:get_config(ejabberd_node), 
                     mod_snmp, 
                     handle_entry, 
                     [get, Counter]).

get_counter_value(Counter) ->
    rpc:call(ct:get_config(ejabberd_node), 
             mod_snmp, 
             handle_entry, 
             [get, Counter]).

get_table_value(RowInd, Cols, Table) ->
    rpc:call(ct:get_config(ejabberd_node), 
             mod_snmp, 
             handle_entry, 
             [get, RowInd, Cols, Table]).

get_next_table_value(RowInd, Cols, Table) ->
    rpc:call(ct:get_config(ejabberd_node), 
             mod_snmp, 
             handle_entry, 
             [get_next, RowInd, Cols, Table]).
