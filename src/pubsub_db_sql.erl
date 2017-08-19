%%%----------------------------------------------------------------------
%%% File    : pubsub_db_sql.erl
%%% Author  : Pablo Polvorin <pablo.polvorin@process-one.net>
%%% Purpose : Provide helpers for PubSub ODBC backend
%%% Created :  7 Aug 2009 by Pablo Polvorin <pablo.polvorin@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-compile([{parse_transform, ejabberd_sql_pt}]).

-author("pablo.polvorin@process-one.net").

-include("pubsub.hrl").
-include("ejabberd_sql_pt.hrl").

-export([add_subscription/1, read_subscription/1,
    delete_subscription/1, update_subscription/1]).
-export([export/1]).

%% TODO: Those -spec lines produce errors in old Erlang versions.
%% They can be enabled again in ejabberd 3.0 because it uses R12B or higher.
%% -spec read_subscription(SubID :: string()) -> {ok, #pubsub_subscription{}} |  notfound.
read_subscription(SubID) ->
    case
	ejabberd_sql:sql_query_t([<<"select opt_name, opt_value from pubsub_subscr"
		    "iption_opt where subid = '">>,
		ejabberd_sql:escape(SubID), <<"'">>])
    of
	{selected, [<<"opt_name">>, <<"opt_value">>], []} ->
	    notfound;
	{selected, [<<"opt_name">>, <<"opt_value">>], Options} ->
	    {ok,
		#pubsub_subscription{subid = SubID,
		    options = lists:map(fun subscription_opt_from_sql/1, Options)}}
    end.

%% -spec delete_subscription(SubID :: string()) -> ok.
delete_subscription(SubID) ->
    %% -spec update_subscription(#pubsub_subscription{}) -> ok .
    %% -spec add_subscription(#pubsub_subscription{}) -> ok.
    %% -------------- Internal utilities -----------------------
    ejabberd_sql:sql_query_t([<<"delete from pubsub_subscription_opt "
		"where subid = '">>,
	    ejabberd_sql:escape(SubID), <<"'">>]),
    ok.

update_subscription(#pubsub_subscription{subid = SubId} = Sub) ->
    delete_subscription(SubId), add_subscription(Sub).

add_subscription(#pubsub_subscription{subid = SubId, options = Opts}) ->
    EscapedSubId = ejabberd_sql:escape(SubId),
    lists:foreach(fun (Opt) ->
		{OdbcOptName, OdbcOptValue} = subscription_opt_to_sql(Opt),
		ejabberd_sql:sql_query_t([<<"insert into pubsub_subscription_opt(subid, "
			    "opt_name, opt_value)values ('">>,
			EscapedSubId, <<"','">>,
			OdbcOptName, <<"','">>,
			OdbcOptValue, <<"')">>])
	end,
	Opts),
    ok.

subscription_opt_from_sql([<<"DELIVER">>, Value]) ->
    {deliver, sql_to_boolean(Value)};
subscription_opt_from_sql([<<"DIGEST">>, Value]) ->
    {digest, sql_to_boolean(Value)};
subscription_opt_from_sql([<<"DIGEST_FREQUENCY">>, Value]) ->
    {digest_frequency, sql_to_integer(Value)};
subscription_opt_from_sql([<<"EXPIRE">>, Value]) ->
    {expire, sql_to_timestamp(Value)};
subscription_opt_from_sql([<<"INCLUDE_BODY">>, Value]) ->
    {include_body, sql_to_boolean(Value)};
%%TODO: might be > than 1 show_values value??.
%%      need to use compact all in only 1 opt.
subscription_opt_from_sql([<<"SHOW_VALUES">>, Value]) ->
    {show_values, Value};
subscription_opt_from_sql([<<"SUBSCRIPTION_TYPE">>, Value]) ->
    {subscription_type,
	case Value of
	    <<"items">> -> items;
	    <<"nodes">> -> nodes
	end};
subscription_opt_from_sql([<<"SUBSCRIPTION_DEPTH">>, Value]) ->
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

%% REVIEW:
%% * this code takes NODEID from Itemid2, and forgets about Nodeidx
%% * this code assumes Payload only contains one xmlelement()
%% * PUBLISHER is taken from Creation
export(_Server) ->
    [{pubsub_item,
      fun(_Host, #pubsub_item{itemid = {Itemid1, NODEID},
                              %nodeidx = _Nodeidx,
                              creation = {{C1, C2, C3}, Cusr},
                              modification = {{M1, M2, M3}, _Musr},
                              payload = Payload}) ->
              ITEMID = ejabberd_sql:escape(Itemid1),
              CREATION = ejabberd_sql:escape(list_to_binary(
                string:join([string:right(integer_to_list(I),6,$0)||I<-[C1,C2,C3]],":"))),
              MODIFICATION = ejabberd_sql:escape(list_to_binary(
                string:join([string:right(integer_to_list(I),6,$0)||I<-[M1,M2,M3]],":"))),
              PUBLISHER = ejabberd_sql:escape(jid:encode(Cusr)),
              [PayloadEl] = [El || {xmlel,_,_,_} = El <- Payload],
              PAYLOAD = ejabberd_sql:escape(fxml:element_to_binary(PayloadEl)),
              [?SQL("delete from pubsub_item where itemid=%(ITEMID)s;"),
               ?SQL("insert into pubsub_item(itemid,nodeid,creation,modification,publisher,payload) \n"
               " values (%(ITEMID)s, %(NODEID)d, %(CREATION)s,
                 %(MODIFICATION)s, %(PUBLISHER)s, %(PAYLOAD)s);")];
         (_Host, _R) ->
              []
      end},
%% REVIEW:
%% * From the mnesia table, the #pubsub_state.items is not used in ODBC
%% * Right now AFFILIATION is the first letter of Affiliation
%% * Right now SUBSCRIPTIONS expects only one Subscription
%% * Right now SUBSCRIPTIONS letter is the first letter of Subscription
      {pubsub_state,
      fun(_Host, #pubsub_state{stateid = {Jid, Stateid},
                               %nodeidx = Nodeidx,
                               items = _Items,
                               affiliation = Affiliation,
                               subscriptions = Subscriptions}) ->
              STATEID = list_to_binary(integer_to_list(Stateid)),
              JID = ejabberd_sql:escape(jid:encode(Jid)),
              NODEID = <<"unknown">>, %% TODO: integer_to_list(Nodeidx),
              AFFILIATION = list_to_binary(string:substr(atom_to_list(Affiliation),1,1)),
              SUBSCRIPTIONS = list_to_binary(parse_subscriptions(Subscriptions)),
              [?SQL("delete from pubsub_state where stateid=%(STATEID)s;"),
               ?SQL("insert into pubsub_state(stateid,jid,nodeid,affiliation,subscriptions)\n"
               " values (%(STATEID)s, %(JID)s, %(NODEID)s, %(AFFILIATION)s, %(SUBSCRIPTIONS)s);")];
         (_Host, _R) ->
              []
      end},

%% REVIEW:
%% * Parents is not migrated to PARENTs
%% * Probably some option VALs are not correctly represented in mysql
      {pubsub_node,
      fun(_Host, #pubsub_node{nodeid = {Hostid, Nodeid},
                              id = Id,
                              parents = _Parents,
                              type = Type,
                              owners = Owners,
                              options = Options}) ->
              HOST = case Hostid of
                    {U,S,R} -> ejabberd_sql:escape(jid:encode({U,S,R}));
                    _ -> ejabberd_sql:escape(Hostid)
                    end,
              NODE = ejabberd_sql:escape(Nodeid),
              PARENT = <<"">>,
              IdB = integer_to_binary(Id),
              TYPE = ejabberd_sql:escape(<<Type/binary, "_odbc">>),
              [?SQL("delete from pubsub_node where nodeid=%(Id)d;"),
               ?SQL("insert into pubsub_node(host,node,nodeid,parent,type) \n"
               " values (%(HOST)s, %(NODE)s, %(Id)d, %(PARENT)s, %(TYPE)s);"),
               ?SQL("delete from pubsub_node_option where nodeid=%(Id)d;"),
               [["insert into pubsub_node_option(nodeid,name,val)\n"
                 " values (", IdB, ", '", atom_to_list(Name), "', '",
                           io_lib:format("~p", [Val]), "');\n"] || {Name,Val} <- Options],
               ?SQL("delete from pubsub_node_owner where nodeid=%(Id)d;"),
               [["insert into pubsub_node_owner(nodeid,owner)\n"
                 " values (", IdB, ", '", jid:encode(Usr), "');\n"] || Usr <- Owners],"\n"];
         (_Host, _R) ->
              []
      end}].

parse_subscriptions([]) ->
    "";
parse_subscriptions([{State, Item}]) ->
    STATE = case State of
        subscribed -> "s"
    end,
    string:join([STATE, Item],":").

