%%%----------------------------------------------------------------------
%%% File    : pubsub_db_sql.erl
%%% Author  : Pablo Polvorin <pablo.polvorin@process-one.net>
%%% Purpose : Provide helpers for PubSub ODBC backend
%%% Created :  7 Aug 2009 by Pablo Polvorin <pablo.polvorin@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-module(pubsub_db_sql).


-author("pablo.polvorin@process-one.net").

-include("pubsub.hrl").
-include("ejabberd_sql_pt.hrl").

-export([add_subscription/1, read_subscription/1,
    delete_subscription/1, update_subscription/1]).
-export([export/1]).

-spec read_subscription(SubID :: mod_pubsub:subId()) -> {ok, #pubsub_subscription{}} |  notfound.
read_subscription(SubID) ->
    case
	ejabberd_sql:sql_query_t(
          ?SQL("select @(opt_name)s, @(opt_value)s from pubsub_subscription_opt where subid = %(SubID)s"))
    of
	{selected, []} ->
	    notfound;
	{selected, Options} ->
	    {ok,
		#pubsub_subscription{subid = SubID,
		    options = lists:map(fun subscription_opt_from_sql/1, Options)}}
    end.

-spec delete_subscription(SubID :: mod_pubsub:subId()) -> ok.
delete_subscription(SubID) ->
    ejabberd_sql:sql_query_t(
      ?SQL("delete from pubsub_subscription_opt "
           "where subid = %(SubID)s")),
    ok.

-spec update_subscription(#pubsub_subscription{}) -> ok .
update_subscription(#pubsub_subscription{subid = SubId} = Sub) ->
    delete_subscription(SubId), add_subscription(Sub).

-spec add_subscription(#pubsub_subscription{}) -> ok.
add_subscription(#pubsub_subscription{subid = SubId, options = Opts}) ->
    lists:foreach(
      fun(Opt) ->
              {OdbcOptName, OdbcOptValue} = subscription_opt_to_sql(Opt),
              ejabberd_sql:sql_query_t(
                ?SQL("insert into pubsub_subscription_opt(subid, "
                     "opt_name, opt_value) values "
                     "(%(SubId)s, %(OdbcOptName)s, %(OdbcOptValue)s)"))
      end,
      Opts),
    ok.

subscription_opt_from_sql({<<"DELIVER">>, Value}) ->
    {deliver, sql_to_boolean(Value)};
subscription_opt_from_sql({<<"DIGEST">>, Value}) ->
    {digest, sql_to_boolean(Value)};
subscription_opt_from_sql({<<"DIGEST_FREQUENCY">>, Value}) ->
    {digest_frequency, sql_to_integer(Value)};
subscription_opt_from_sql({<<"EXPIRE">>, Value}) ->
    {expire, sql_to_timestamp(Value)};
subscription_opt_from_sql({<<"INCLUDE_BODY">>, Value}) ->
    {include_body, sql_to_boolean(Value)};
%%TODO: might be > than 1 show_values value??.
%%      need to use compact all in only 1 opt.
subscription_opt_from_sql({<<"SHOW_VALUES">>, Value}) ->
    {show_values, Value};
subscription_opt_from_sql({<<"SUBSCRIPTION_TYPE">>, Value}) ->
    {subscription_type,
	case Value of
	    <<"items">> -> items;
	    <<"nodes">> -> nodes
	end};
subscription_opt_from_sql({<<"SUBSCRIPTION_DEPTH">>, Value}) ->
    {subscription_depth,
	case Value of
	    <<"all">> -> all;
	    N -> sql_to_integer(N)
	end}.

subscription_opt_to_sql({deliver, Bool}) ->
    {<<"DELIVER">>, boolean_to_sql(Bool)};
subscription_opt_to_sql({digest, Bool}) ->
    {<<"DIGEST">>, boolean_to_sql(Bool)};
subscription_opt_to_sql({digest_frequency, Int}) ->
    {<<"DIGEST_FREQUENCY">>, integer_to_sql(Int)};
subscription_opt_to_sql({expire, Timestamp}) ->
    {<<"EXPIRE">>, timestamp_to_sql(Timestamp)};
subscription_opt_to_sql({include_body, Bool}) ->
    {<<"INCLUDE_BODY">>, boolean_to_sql(Bool)};
subscription_opt_to_sql({show_values, Values}) ->
    {<<"SHOW_VALUES">>, Values};
subscription_opt_to_sql({subscription_type, Type}) ->
    {<<"SUBSCRIPTION_TYPE">>,
	case Type of
	    items -> <<"items">>;
	    nodes -> <<"nodes">>
	end};
subscription_opt_to_sql({subscription_depth, Depth}) ->
    {<<"SUBSCRIPTION_DEPTH">>,
	case Depth of
	    all -> <<"all">>;
	    N -> integer_to_sql(N)
	end}.

integer_to_sql(N) -> integer_to_binary(N).

boolean_to_sql(true) -> <<"1">>;
boolean_to_sql(false) -> <<"0">>.

timestamp_to_sql(T) -> xmpp_util:encode_timestamp(T).

sql_to_integer(N) -> binary_to_integer(N).

sql_to_boolean(B) -> B == <<"1">>.

sql_to_timestamp(T) -> xmpp_util:decode_timestamp(T).

export(_Server) ->
      [{pubsub_node,
        fun(_Host, #pubsub_node{nodeid = {Host, Node}, id = Nidx,
                                parents = Parents, type = Type,
                                options = Options}) ->
            H = node_flat_sql:encode_host(Host),
            Parent = case Parents of
                       [] -> <<>>;
                       [First | _] -> First
                     end,
            [?SQL("delete from pubsub_node where nodeid=%(Nidx)d;"),
             ?SQL("delete from pubsub_node_option where nodeid=%(Nidx)d;"),
             ?SQL("delete from pubsub_node_owner where nodeid=%(Nidx)d;"),
             ?SQL("delete from pubsub_state where nodeid=%(Nidx)d;"),
             ?SQL("delete from pubsub_item where nodeid=%(Nidx)d;"),
             ?SQL("insert into pubsub_node(host,node,nodeid,parent,plugin)"
                  " values (%(H)s, %(Node)s, %(Nidx)d, %(Parent)s, %(Type)s);")]
            ++ lists:map(
                 fun ({Key, Value}) ->
                     SKey = iolist_to_binary(atom_to_list(Key)),
                     SValue = misc:term_to_expr(Value),
                     ?SQL("insert into pubsub_node_option(nodeid,name,val)"
                          " values (%(Nidx)d, %(SKey)s, %(SValue)s);")
                 end, Options);
           (_Host, _R) ->
            []
        end},
       {pubsub_state,
        fun(_Host, #pubsub_state{stateid = {JID, Nidx},
                                 affiliation = Affiliation,
                                 subscriptions = Subscriptions}) ->
            J = jid:encode(JID),
            S = node_flat_sql:encode_subscriptions(Subscriptions),
            A = node_flat_sql:encode_affiliation(Affiliation),
            [?SQL("insert into pubsub_state(nodeid,jid,affiliation,subscriptions)"
                  " values (%(Nidx)d, %(J)s, %(A)s, %(S)s);")];
           (_Host, _R) ->
            []
        end},
       {pubsub_item,
        fun(_Host, #pubsub_item{itemid = {ItemId, Nidx},
                                creation = {C, _},
                                modification = {M, JID},
                                payload = Payload}) ->
            P = jid:encode(JID),
            XML = str:join([fxml:element_to_binary(X) || X<-Payload], <<>>),
            SM = encode_now(M),
            SC = encode_now(C),
            [?SQL("insert into pubsub_item(itemid,nodeid,creation,modification,publisher,payload)"
                  " values (%(ItemId)s, %(Nidx)d, %(SC)s, %(SM)s, %(P)s, %(XML)s);")];
           (_Host, _R) ->
            []
        end}].

encode_now({T1, T2, T3}) ->
    <<(misc:i2l(T1, 6))/binary, ":",
      (misc:i2l(T2, 6))/binary, ":",
      (misc:i2l(T3, 6))/binary>>.
