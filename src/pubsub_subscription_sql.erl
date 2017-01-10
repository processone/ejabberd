%%%----------------------------------------------------------------------
%%% File    : pubsub_subscription_sql.erl
%%% Author  : Pablo Polvorin <pablo.polvorin@process-one.net>
%%% Purpose : Handle pubsub subscriptions options with ODBC backend
%%%           based on pubsub_subscription.erl by Brian Cully <bjc@kublai.com>
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

-module(pubsub_subscription_sql).
-author("pablo.polvorin@process-one.net").

%% API
-export([init/3, subscribe_node/3, unsubscribe_node/3,
    get_subscription/3, set_subscription/4,
    make_subid/0,
    get_options_xform/2, parse_options_xform/1]).

-include("pubsub.hrl").

-include("xmpp.hrl").

-define(PUBSUB_DELIVER, <<"pubsub#deliver">>).
-define(PUBSUB_DIGEST, <<"pubsub#digest">>).
-define(PUBSUB_DIGEST_FREQUENCY, <<"pubsub#digest_frequency">>).
-define(PUBSUB_EXPIRE, <<"pubsub#expire">>).
-define(PUBSUB_INCLUDE_BODY, <<"pubsub#include_body">>).
-define(PUBSUB_SHOW_VALUES, <<"pubsub#show-values">>).
-define(PUBSUB_SUBSCRIPTION_TYPE, <<"pubsub#subscription_type">>).
-define(PUBSUB_SUBSCRIPTION_DEPTH, <<"pubsub#subscription_depth">>).
-define(DELIVER_LABEL, <<"Whether an entity wants to receive or disable notifications">>).
-define(DIGEST_LABEL, <<"Whether an entity wants to receive digests "
	"(aggregations) of notifications or all notifications individually">>).
-define(DIGEST_FREQUENCY_LABEL, <<"The minimum number of milliseconds between "
	"sending any two notification digests">>).
-define(EXPIRE_LABEL, <<"The DateTime at which a leased subscription will end or has ended">>).
-define(INCLUDE_BODY_LABEL, <<"Whether an entity wants to receive an "
	"XMPP message body in addition to the payload format">>).
-define(SHOW_VALUES_LABEL, <<"The presence states for which an entity wants to receive notifications">>).
-define(SUBSCRIPTION_TYPE_LABEL, <<"Type of notification to receive">>).
-define(SUBSCRIPTION_DEPTH_LABEL, <<"Depth from subscription for which to receive notifications">>).
-define(SHOW_VALUE_AWAY_LABEL, <<"XMPP Show Value of Away">>).
-define(SHOW_VALUE_CHAT_LABEL, <<"XMPP Show Value of Chat">>).
-define(SHOW_VALUE_DND_LABEL, <<"XMPP Show Value of DND (Do Not Disturb)">>).
-define(SHOW_VALUE_ONLINE_LABEL, <<"Mere Availability in XMPP (No Show Value)">>).
-define(SHOW_VALUE_XA_LABEL, <<"XMPP Show Value of XA (Extended Away)">>).
-define(SUBSCRIPTION_TYPE_VALUE_ITEMS_LABEL, <<"Receive notification of new items only">>).
-define(SUBSCRIPTION_TYPE_VALUE_NODES_LABEL, <<"Receive notification of new nodes only">>).
-define(SUBSCRIPTION_DEPTH_VALUE_ONE_LABEL, <<"Receive notification from direct child nodes only">>).
-define(SUBSCRIPTION_DEPTH_VALUE_ALL_LABEL, <<"Receive notification from all descendent nodes">>).

-define(DB_MOD, pubsub_db_sql).
%%====================================================================
%% API
%%====================================================================

init(_Host, _ServerHost, _Opts) -> ok = create_table().

-spec subscribe_node(_JID :: _, _NodeId :: _, Options :: [] | mod_pubsub:subOptions()) ->
			    {result, mod_pubsub:subId()}.

subscribe_node(_JID, _NodeId, Options) ->
    SubID = make_subid(),
    (?DB_MOD):add_subscription(#pubsub_subscription{subid = SubID, options = Options}),
    {result, SubID}.

-spec unsubscribe_node(_JID :: _, _NodeId :: _, SubID :: mod_pubsub:subId()) ->
			      {result, mod_pubsub:subscription()} | {error, notfound}.

unsubscribe_node(_JID, _NodeId, SubID) ->
    case (?DB_MOD):read_subscription(SubID) of
	{ok, Sub} -> (?DB_MOD):delete_subscription(SubID), {result, Sub};
	notfound -> {error, notfound}
    end.

-spec get_subscription(_JID :: _, _NodeId :: _, SubId :: mod_pubsub:subId()) ->
			      {result, mod_pubsub:subscription()} | {error, notfound}.

get_subscription(_JID, _NodeId, SubID) ->
    case (?DB_MOD):read_subscription(SubID) of
	{ok, Sub} -> {result, Sub};
	notfound -> {error, notfound}
    end.

-spec set_subscription(_JID :: _, _NodeId :: _, SubId :: mod_pubsub:subId(),
		       Options :: mod_pubsub:subOptions()) -> {result, ok}.
set_subscription(_JID, _NodeId, SubID, Options) ->
    case (?DB_MOD):read_subscription(SubID) of
	{ok, _} ->
	    (?DB_MOD):update_subscription(#pubsub_subscription{subid = SubID,
		    options = Options}),
	    {result, ok};
	notfound ->
	    (?DB_MOD):add_subscription(#pubsub_subscription{subid = SubID,
		    options = Options}),
	    {result, ok}
    end.

get_options_xform(Lang, Options) ->
    Keys = [deliver, show_values, subscription_type, subscription_depth],
    XFields = [get_option_xfield(Lang, Key, Options) || Key <- Keys],
    {result,
     #xdata{type = form,
	    fields = [#xdata_field{type = hidden,
				   var = <<"FORM_TYPE">>,
				   values = [?NS_PUBSUB_SUB_OPTIONS]}|
		      XFields]}}.

parse_options_xform(XFields) ->
    Opts = set_xoption(XFields, []),
    {result, Opts}.

%%====================================================================
%% Internal functions
%%====================================================================
create_table() -> ok.

-spec make_subid() -> mod_pubsub:subId().
make_subid() ->
    {T1, T2, T3} = p1_time_compat:timestamp(),
    (str:format("~.16B~.16B~.16B", [T1, T2, T3])).

%%
%% Subscription XForm processing.
%%

%% Return processed options, with types converted and so forth, using
%% Opts as defaults.
set_xoption([], Opts) -> Opts;
set_xoption([{Var, Value} | T], Opts) ->
    NewOpts = case var_xfield(Var) of
	{error, _} -> Opts;
	Key ->
	    Val = val_xfield(Key, Value),
	    lists:keystore(Key, 1, Opts, {Key, Val})
    end,
    set_xoption(T, NewOpts).

%% Return the options list's key for an XForm var.
%% Convert Values for option list's Key.
var_xfield(?PUBSUB_DELIVER) -> deliver;
var_xfield(?PUBSUB_DIGEST) -> digest;
var_xfield(?PUBSUB_DIGEST_FREQUENCY) -> digest_frequency;
var_xfield(?PUBSUB_EXPIRE) -> expire;
var_xfield(?PUBSUB_INCLUDE_BODY) -> include_body;
var_xfield(?PUBSUB_SHOW_VALUES) -> show_values;
var_xfield(?PUBSUB_SUBSCRIPTION_TYPE) -> subscription_type;
var_xfield(?PUBSUB_SUBSCRIPTION_DEPTH) -> subscription_depth;
var_xfield(_) -> {error, badarg}.

val_xfield(deliver = Opt, [Val]) -> xopt_to_bool(Opt, Val);
val_xfield(digest = Opt, [Val]) -> xopt_to_bool(Opt, Val);
val_xfield(digest_frequency = Opt, [Val]) ->
    case catch binary_to_integer(Val) of
	N when is_integer(N) -> N;
	_ ->
	    Txt = {<<"Value of '~s' should be integer">>, [Opt]},
	    {error, xmpp:err_not_acceptable(Txt, ?MYLANG)}
    end;
val_xfield(expire = Opt, [Val]) ->
    try xmpp_util:decode_timestamp(Val)
    catch _:{bad_timestamp, _} ->
	    Txt = {<<"Value of '~s' should be datetime string">>, [Opt]},
	    {error, xmpp:err_not_acceptable(Txt, ?MYLANG)}
    end;
val_xfield(include_body = Opt, [Val]) -> xopt_to_bool(Opt, Val);
val_xfield(show_values, Vals) -> Vals;
val_xfield(subscription_type, [<<"items">>]) -> items;
val_xfield(subscription_type, [<<"nodes">>]) -> nodes;
val_xfield(subscription_depth, [<<"all">>]) -> all;
val_xfield(subscription_depth = Opt, [Depth]) ->
    case catch binary_to_integer(Depth) of
	N when is_integer(N) -> N;
	_ ->
	    Txt = {<<"Value of '~s' should be integer">>, [Opt]},
	    {error, xmpp:err_not_acceptable(Txt, ?MYLANG)}
    end.

%% Convert XForm booleans to Erlang booleans.
xopt_to_bool(_, <<"0">>) -> false;
xopt_to_bool(_, <<"1">>) -> true;
xopt_to_bool(_, <<"false">>) -> false;
xopt_to_bool(_, <<"true">>) -> true;
xopt_to_bool(Option, _) ->
    Txt = {<<"Value of '~s' should be boolean">>, [Option]},
    {error, xmpp:err_not_acceptable(Txt, ?MYLANG)}.

%% Return a field for an XForm for Key, with data filled in, if
%% applicable, from Options.
get_option_xfield(Lang, Key, Options) ->
    Var = xfield_var(Key),
    Label = xfield_label(Key),
    {Type, OptEls} = type_and_options(xfield_type(Key), Lang),
    Vals = case lists:keysearch(Key, 1, Options) of
	       {value, {_, Val}} ->
		   [xfield_val(Key, Val)];
	       false ->
		   []
	   end,
    #xdata_field{type = Type, var = Var,
		 label = translate:translate(Lang, Label),
		 values = Vals,
		 options = OptEls}.

type_and_options({Type, Options}, Lang) ->
    {Type, [tr_xfield_options(O, Lang) || O <- Options]};
type_and_options(Type, _Lang) -> {Type, []}.

tr_xfield_options({Value, Label}, Lang) ->
    #xdata_option{label = translate:translate(Lang, Label),
		  value = Value}.

xfield_var(deliver) -> ?PUBSUB_DELIVER;
%xfield_var(digest) -> ?PUBSUB_DIGEST;
%xfield_var(digest_frequency) -> ?PUBSUB_DIGEST_FREQUENCY;
%xfield_var(expire) -> ?PUBSUB_EXPIRE;
%xfield_var(include_body) -> ?PUBSUB_INCLUDE_BODY;
xfield_var(show_values) -> ?PUBSUB_SHOW_VALUES;
xfield_var(subscription_type) -> ?PUBSUB_SUBSCRIPTION_TYPE;
xfield_var(subscription_depth) -> ?PUBSUB_SUBSCRIPTION_DEPTH.

xfield_type(deliver) -> boolean;
%xfield_type(digest) -> boolean;
%xfield_type(digest_frequency) -> 'text-single';
%xfield_type(expire) -> 'text-single';
%xfield_type(include_body) -> boolean;
xfield_type(show_values) ->
    {'list-multi',
     [{<<"away">>, ?SHOW_VALUE_AWAY_LABEL},
      {<<"chat">>, ?SHOW_VALUE_CHAT_LABEL},
      {<<"dnd">>, ?SHOW_VALUE_DND_LABEL},
      {<<"online">>, ?SHOW_VALUE_ONLINE_LABEL},
      {<<"xa">>, ?SHOW_VALUE_XA_LABEL}]};
xfield_type(subscription_type) ->
    {'list-single',
     [{<<"items">>, ?SUBSCRIPTION_TYPE_VALUE_ITEMS_LABEL},
      {<<"nodes">>, ?SUBSCRIPTION_TYPE_VALUE_NODES_LABEL}]};
xfield_type(subscription_depth) ->
    {'list-single',
     [{<<"1">>, ?SUBSCRIPTION_DEPTH_VALUE_ONE_LABEL},
      {<<"all">>, ?SUBSCRIPTION_DEPTH_VALUE_ALL_LABEL}]}.

%% Return the XForm variable label for a subscription option key.
xfield_label(deliver) -> ?DELIVER_LABEL;
%xfield_label(digest) -> ?DIGEST_LABEL;
%xfield_label(digest_frequency) -> ?DIGEST_FREQUENCY_LABEL;
%xfield_label(expire) -> ?EXPIRE_LABEL;
%xfield_label(include_body) -> ?INCLUDE_BODY_LABEL;
xfield_label(show_values) -> ?SHOW_VALUES_LABEL;
%% Return the XForm value for a subscription option key.
%% Convert erlang booleans to XForms.
xfield_label(subscription_type) -> ?SUBSCRIPTION_TYPE_LABEL;
xfield_label(subscription_depth) -> ?SUBSCRIPTION_DEPTH_LABEL.

xfield_val(deliver, Val) -> [bool_to_xopt(Val)];
%xfield_val(digest, Val) -> [bool_to_xopt(Val)];
%xfield_val(digest_frequency, Val) ->
%    [integer_to_binary(Val))];
%xfield_val(expire, Val) ->
%    [jlib:now_to_utc_string(Val)];
%xfield_val(include_body, Val) -> [bool_to_xopt(Val)];
xfield_val(show_values, Val) -> Val;
xfield_val(subscription_type, items) -> [<<"items">>];
xfield_val(subscription_type, nodes) -> [<<"nodes">>];
xfield_val(subscription_depth, all) -> [<<"all">>];
xfield_val(subscription_depth, N) ->
    [integer_to_binary(N)].

bool_to_xopt(false) -> <<"false">>;
bool_to_xopt(true) -> <<"true">>.
