%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 30 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------
-module(mod_proxy65_sql).
-behaviour(mod_proxy65).


%% API
-export([init/0, register_stream/2, unregister_stream/1, activate_stream/4]).
-export([sql_schemas/0]).

-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    ejabberd_sql_schema:update_schema(
      ejabberd_config:get_myname(), ?MODULE, sql_schemas()),
    NodeS = erlang:atom_to_binary(node(), latin1),
    ?DEBUG("Cleaning SQL 'proxy65' table...", []),
    case ejabberd_sql:sql_query(
	   ejabberd_config:get_myname(),
	   ?SQL("delete from proxy65 where "
		"node_i=%(NodeS)s or node_t=%(NodeS)s")) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("Failed to clean 'proxy65' table: ~p", [Err]),
	    Err
    end.

sql_schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"proxy65">>,
                columns =
                    [#sql_column{name = <<"sid">>, type = text},
                     #sql_column{name = <<"pid_t">>, type = text},
                     #sql_column{name = <<"pid_i">>, type = text},
                     #sql_column{name = <<"node_t">>, type = text},
                     #sql_column{name = <<"node_i">>, type = text},
                     #sql_column{name = <<"jid_i">>, type = text}],
                indices = [#sql_index{
                              columns = [<<"sid">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"jid_i">>]}]}]}].

register_stream(SID, Pid) ->
    PidS = misc:encode_pid(Pid),
    NodeS = erlang:atom_to_binary(node(Pid), latin1),
    F = fun() ->
		case ejabberd_sql:sql_query_t(
		       ?SQL("update proxy65 set pid_i=%(PidS)s, "
			    "node_i=%(NodeS)s where sid=%(SID)s")) of
		    {updated, 1} ->
			ok;
		    _ ->
			ejabberd_sql:sql_query_t(
			  ?SQL("insert into proxy65"
			       "(sid, pid_t, node_t, pid_i, node_i, jid_i) "
			       "values (%(SID)s, %(PidS)s, %(NodeS)s, '', '', '')"))
		end
	end,
    case ejabberd_sql:sql_transaction(ejabberd_config:get_myname(), F) of
	{atomic, _} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

unregister_stream(SID) ->
    F = fun() ->
		ejabberd_sql:sql_query_t(
		  ?SQL("delete from proxy65 where sid=%(SID)s"))
	end,
    case ejabberd_sql:sql_transaction(ejabberd_config:get_myname(), F) of
	{atomic, _} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

activate_stream(SID, IJID, MaxConnections, _Node) ->
    F = fun() ->
		case ejabberd_sql:sql_query_t(
		       ?SQL("select @(pid_t)s, @(node_t)s, @(pid_i)s, "
			    "@(node_i)s, @(jid_i)s from proxy65 where "
			    "sid=%(SID)s")) of
		    {selected, [{TPidS, TNodeS, IPidS, INodeS, <<"">>}]}
		      when IPidS /= <<"">> ->
			try {misc:decode_pid(TPidS, TNodeS),
			     misc:decode_pid(IPidS, INodeS)} of
			    {TPid, IPid} ->
				case ejabberd_sql:sql_query_t(
				       ?SQL("update proxy65 set jid_i=%(IJID)s "
					    "where sid=%(SID)s")) of
				    {updated, 1} when is_integer(MaxConnections) ->
					case ejabberd_sql:sql_query_t(
					       ?SQL("select @(count(*))d from proxy65 "
						    "where jid_i=%(IJID)s")) of
					    {selected, [{Num}]} when Num > MaxConnections ->
						ejabberd_sql:abort({limit, IPid, TPid});
					    {selected, _} ->
						{ok, IPid, TPid};
					    Err ->
						ejabberd_sql:abort(Err)
					end;
				    {updated, _} ->
					{ok, IPid, TPid};
				    Err ->
					ejabberd_sql:abort(Err)
				end
			catch _:{bad_node, _} ->
				{error, notfound}
			end;
		    {selected, [{_, _, _, _, JID}]} when JID /= <<"">> ->
			{error, conflict};
		    {selected, _} ->
			{error, notfound};
		    Err ->
			ejabberd_sql:abort(Err)
		end
	end,
    case ejabberd_sql:sql_transaction(ejabberd_config:get_myname(), F) of
	{atomic, Result} ->
	    Result;
	{aborted, {limit, _, _} = Limit} ->
	    {error, Limit};
	{aborted, Reason} ->
	    {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
