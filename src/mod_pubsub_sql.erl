%%%----------------------------------------------------------------------
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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
%%%----------------------------------------------------------------------
-module(mod_pubsub_sql).

-behaviour(ejabberd_db_serialize).

%% API
-export([init/3]).
-export([sql_schemas/0]).

%% Callbacks for `ejabberd_db_serialize`
-export([serialize/3, deserialize_start/1, deserialize/2]).

-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, ServerHost, _Opts) ->
    ejabberd_sql_schema:update_schema(ServerHost, ?MODULE, sql_schemas()),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sql_schemas() ->
    [#sql_schema{
        version = 1,
        tables =
            [#sql_table{
                name = <<"pubsub_node">>,
                columns =
                    [#sql_column{name = <<"host">>, type = text},
                     #sql_column{name = <<"node">>, type = text},
                     #sql_column{name = <<"parent">>, type = text,
                                 default = true},
                     #sql_column{name = <<"plugin">>, type = text},
                     #sql_column{name = <<"nodeid">>, type = bigserial}],
                indices = [#sql_index{
                              columns = [<<"nodeid">>],
                              unique = true},
                           #sql_index{
                              columns = [<<"parent">>]},
                           #sql_index{
                              columns = [<<"host">>, <<"node">>],
                              unique = true}]},
             #sql_table{
                name = <<"pubsub_node_option">>,
                columns =
                    [#sql_column{name = <<"nodeid">>, type = bigint,
                                 opts = [#sql_references{
                                            table = <<"pubsub_node">>,
                                            column = <<"nodeid">>}]},
                     #sql_column{name = <<"name">>, type = text},
                     #sql_column{name = <<"val">>, type = text}],
                indices = [#sql_index{columns = [<<"nodeid">>]}]},
             #sql_table{
                name = <<"pubsub_node_owner">>,
                columns =
                    [#sql_column{name = <<"nodeid">>, type = bigint,
                                 opts = [#sql_references{
                                            table = <<"pubsub_node">>,
                                            column = <<"nodeid">>}]},
                     #sql_column{name = <<"owner">>, type = text}],
                indices = [#sql_index{columns = [<<"nodeid">>]}]},
             #sql_table{
                name = <<"pubsub_state">>,
                columns =
                    [#sql_column{name = <<"nodeid">>, type = bigint,
                                 opts = [#sql_references{
                                            table = <<"pubsub_node">>,
                                            column = <<"nodeid">>}]},
                     #sql_column{name = <<"jid">>, type = text},
                     #sql_column{name = <<"affiliation">>, type = {char, 1}},
                     #sql_column{name = <<"subscriptions">>, type = text,
                                 default = true},
                     #sql_column{name = <<"stateid">>, type = bigserial}],
                indices = [#sql_index{columns = [<<"stateid">>],
                                      unique = true},
                           #sql_index{columns = [<<"jid">>]},
                           #sql_index{columns = [<<"nodeid">>, <<"jid">>],
                                      unique = true}]},
             #sql_table{
                name = <<"pubsub_item">>,
                columns =
                    [#sql_column{name = <<"nodeid">>, type = bigint,
                                 opts = [#sql_references{
                                            table = <<"pubsub_node">>,
                                            column = <<"nodeid">>}]},
                     #sql_column{name = <<"itemid">>, type = text},
                     #sql_column{name = <<"publisher">>, type = text},
                     #sql_column{name = <<"creation">>, type = {text, 32}},
                     #sql_column{name = <<"modification">>, type = {text, 32}},
                     #sql_column{name = <<"payload">>, type = {text, big},
                                 default = true}],
                indices = [#sql_index{columns = [<<"nodeid">>, <<"itemid">>],
                                      unique = true},
                           #sql_index{columns = [<<"itemid">>]}]},
             #sql_table{
                name = <<"pubsub_subscription_opt">>,
                columns =
                    [#sql_column{name = <<"subid">>, type = text},
                     #sql_column{name = <<"opt_name">>, type = {text, 32}},
                     #sql_column{name = <<"opt_value">>, type = text}],
                indices = [#sql_index{columns = [<<"subid">>, <<"opt_name">>],
                                      unique = true}]}]}].

-spec serialize(binary(), non_neg_integer(), undefined | term()) ->
              {ok, [term()], term()} | {error, iolist()}.
serialize(Host, BatchSize, Last) ->
    mod_pubsub:serialize_service(Host, BatchSize, Last).

-spec deserialize_start(binary()) -> ok | {error, iolist()}.
deserialize_start(Host) ->
    Service = mod_pubsub_opt:host(Host),
    Pattern = <<"%@", (ejabberd_sql:escape_like_arg(Host))/binary>>,
    ejabberd_sql:sql_query(Host, ?SQL("DELETE FROM pubsub_node where host=%(Host)s or "
        "host=%(Service)s or host like %(Pattern)s %ESCAPE")).

-spec deserialize(binary(), [term()]) -> ok | {error, iolist()}.
deserialize(Host, Data) ->
    mod_pubsub:deserialize_service(Host, Data).

