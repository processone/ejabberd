%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%% @author Pablo Polvorin <pablo.polvorin@process-one.net>
%%% @author based on pubsub_subscription.erl by Brian Cully <bjc@kublai.com>
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(pubsub_subscription_odbc).

-author("pablo.polvorin@process-one.net").

%% API
-export([init/0, subscribe_node/3, unsubscribe_node/3,
	 get_subscription/3, set_subscription/4,
	 get_options_xform/2, parse_options_xform/1]).

-include("pubsub.hrl").

-include("jlib.hrl").

-define(PUBSUB_DELIVER, <<"pubsub#deliver">>).

-define(PUBSUB_DIGEST, <<"pubsub#digest">>).

-define(PUBSUB_DIGEST_FREQUENCY,
	<<"pubsub#digest_frequency">>).

-define(PUBSUB_EXPIRE, <<"pubsub#expire">>).

-define(PUBSUB_INCLUDE_BODY, <<"pubsub#include_body">>).

-define(PUBSUB_SHOW_VALUES, <<"pubsub#show-values">>).

-define(PUBSUB_SUBSCRIPTION_TYPE,
	<<"pubsub#subscription_type">>).

-define(PUBSUB_SUBSCRIPTION_DEPTH,
	<<"pubsub#subscription_depth">>).

-define(DELIVER_LABEL,
	<<"Whether an entity wants to receive or "
	  "disable notifications">>).

-define(DIGEST_LABEL,
	<<"Whether an entity wants to receive digests "
	  "(aggregations) of notifications or all "
	  "notifications individually">>).

-define(DIGEST_FREQUENCY_LABEL,
	<<"The minimum number of milliseconds between "
	  "sending any two notification digests">>).

-define(EXPIRE_LABEL,
	<<"The DateTime at which a leased subscription "
	  "will end or has ended">>).

-define(INCLUDE_BODY_LABEL,
	<<"Whether an entity wants to receive an "
	  "XMPP message body in addition to the "
	  "payload format">>).

-define(SHOW_VALUES_LABEL,
	<<"The presence states for which an entity "
	  "wants to receive notifications">>).

-define(SUBSCRIPTION_TYPE_LABEL,
	<<"Type of notification to receive">>).

-define(SUBSCRIPTION_DEPTH_LABEL,
	<<"Depth from subscription for which to "
	  "receive notifications">>).

-define(SHOW_VALUE_AWAY_LABEL,
	<<"XMPP Show Value of Away">>).

-define(SHOW_VALUE_CHAT_LABEL,
	<<"XMPP Show Value of Chat">>).

-define(SHOW_VALUE_DND_LABEL,
	<<"XMPP Show Value of DND (Do Not Disturb)">>).

-define(SHOW_VALUE_ONLINE_LABEL,
	<<"Mere Availability in XMPP (No Show Value)">>).

-define(SHOW_VALUE_XA_LABEL,
	<<"XMPP Show Value of XA (Extended Away)">>).

-define(SUBSCRIPTION_TYPE_VALUE_ITEMS_LABEL,
	<<"Receive notification of new items only">>).

-define(SUBSCRIPTION_TYPE_VALUE_NODES_LABEL,
	<<"Receive notification of new nodes only">>).

-define(SUBSCRIPTION_DEPTH_VALUE_ONE_LABEL,
	<<"Receive notification from direct child "
	  "nodes only">>).

-define(SUBSCRIPTION_DEPTH_VALUE_ALL_LABEL,
	<<"Receive notification from all descendent "
	  "nodes">>).

-define(DB_MOD, pubsub_db_odbc).
%%====================================================================
%% API
%%====================================================================

init() -> ok = create_table().

-spec(subscribe_node/3 ::
(
  _JID    :: _,
  _NodeID :: _,
  Options :: mod_pubsub:subOptions())
    -> {result, mod_pubsub:subId()}
).
subscribe_node(_JID, _NodeID, Options) ->
    SubID = make_subid(),
    (?DB_MOD):add_subscription(#pubsub_subscription{subid =
							SubID,
						    options = Options}),
    {result, SubID}.

-spec(unsubscribe_node/3 ::
(
  _JID :: _,
  _NodeID :: _,
  SubID :: mod_pubsub:subId())
    -> {result, mod_pubsub:subscription()}
     | {error, notfound}
).
unsubscribe_node(_JID, _NodeID, SubID) ->
    case (?DB_MOD):read_subscription(SubID) of
      {ok, Sub} ->
	  (?DB_MOD):delete_subscription(SubID), {result, Sub};
      notfound -> {error, notfound}
    end.

-spec(get_subscription/3 ::
(
  _JID    :: _,
  _NodeID :: _,
  SubId   :: mod_pubsub:subId())
    -> {result, mod_pubsub:subscription()}
     | {error, notfound}
).
get_subscription(_JID, _NodeID, SubID) ->
    case (?DB_MOD):read_subscription(SubID) of
      {ok, Sub} -> {result, Sub};
      notfound -> {error, notfound}
    end.

-spec(set_subscription/4 ::
(
  _JID    :: _,
  _NodeID :: _,
  SubId   :: mod_pubsub:subId(),
  Options :: mod_pubsub:subOptions())
    -> {result, ok}
).
set_subscription(_JID, _NodeID, SubID, Options) ->
    case (?DB_MOD):read_subscription(SubID) of
      {ok, _} ->
	  (?DB_MOD):update_subscription(#pubsub_subscription{subid
								 = SubID,
							     options =
								 Options}),
	  {result, ok};
      notfound ->
	  (?DB_MOD):add_subscription(#pubsub_subscription{subid =
							      SubID,
							  options = Options}),
	  {result, ok}
    end.

get_options_xform(Lang, Options) ->
    Keys = [deliver, show_values, subscription_type, subscription_depth],
    XFields = [get_option_xfield(Lang, Key, Options)
	       || Key <- Keys],
    {result,
     #xmlel{name = <<"x">>,
	    attrs = [{<<"xmlns">>, ?NS_XDATA}],
	    children =
		[#xmlel{name = <<"field">>,
			attrs =
			    [{<<"var">>, <<"FORM_TYPE">>},
			     {<<"type">>, <<"hidden">>}],
			children =
			    [#xmlel{name = <<"value">>, attrs = [],
				    children =
					[{xmlcdata, ?NS_PUBSUB_SUB_OPTIONS}]}]}]
		  ++ XFields}}.

parse_options_xform(XFields) ->
    case xml:remove_cdata(XFields) of
      [#xmlel{name = <<"x">>} = XEl] ->
	  case jlib:parse_xdata_submit(XEl) of
	    XData when is_list(XData) ->
	        Opts = set_xoption(XData, []),
	        {result, Opts};
	    Other -> Other
	  end;
      _ -> {result, []}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
create_table() -> ok.

-spec(make_subid/0 :: () -> mod_pubsub:subId()).
make_subid() ->
    {T1, T2, T3} = now(),
    iolist_to_binary(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3])).

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
%var_xfield(?PUBSUB_DIGEST) -> digest;
%var_xfield(?PUBSUB_DIGEST_FREQUENCY) ->
%    digest_frequency;
%var_xfield(?PUBSUB_EXPIRE) -> expire;
%var_xfield(?PUBSUB_INCLUDE_BODY) -> include_body;
var_xfield(?PUBSUB_SHOW_VALUES) -> show_values;
var_xfield(?PUBSUB_SUBSCRIPTION_TYPE) ->
    subscription_type;
var_xfield(?PUBSUB_SUBSCRIPTION_DEPTH) ->
    subscription_depth;
var_xfield(_) -> {error, badarg}.

val_xfield(deliver, [Val]) -> xopt_to_bool(Val);
%val_xfield(digest, [Val]) -> xopt_to_bool(Val);
%val_xfield(digest_frequency, [Val]) ->
%    jlib:binary_to_integer(Val);
%val_xfield(expire, [Val]) ->
%    jlib:datetime_string_to_timestamp(Val);
%val_xfield(include_body, [Val]) -> xopt_to_bool(Val);
val_xfield(show_values, Vals) -> Vals;
val_xfield(subscription_type, [<<"items">>]) -> items;
val_xfield(subscription_type, [<<"nodes">>]) -> nodes;
val_xfield(subscription_depth, [<<"all">>]) -> all;
val_xfield(subscription_depth, [Depth]) ->
    case catch jlib:binary_to_integer(Depth) of
      N when is_integer(N) -> N;
      _ -> {error, ?ERR_NOT_ACCEPTABLE}
    end.

%% Convert XForm booleans to Erlang booleans.
xopt_to_bool(<<"0">>) -> false;
xopt_to_bool(<<"1">>) -> true;
xopt_to_bool(<<"false">>) -> false;
xopt_to_bool(<<"true">>) -> true;
xopt_to_bool(_) -> {error, ?ERR_NOT_ACCEPTABLE}.

%% Return a field for an XForm for Key, with data filled in, if
%% applicable, from Options.
get_option_xfield(Lang, Key, Options) ->
    Var = xfield_var(Key),
    Label = xfield_label(Key),
    {Type, OptEls} = type_and_options(xfield_type(Key),
				      Lang),
    Vals = case lists:keysearch(Key, 1, Options) of
	     {value, {_, Val}} ->
		 [tr_xfield_values(Vals)
		  || Vals <- xfield_val(Key, Val)];
	     false -> []
	   end,
    #xmlel{name = <<"field">>,
	   attrs =
	       [{<<"var">>, Var}, {<<"type">>, Type},
		{<<"label">>, translate:translate(Lang, Label)}],
	   children = OptEls ++ Vals}.

type_and_options({Type, Options}, Lang) ->
    {Type, [tr_xfield_options(O, Lang) || O <- Options]};
type_and_options(Type, _Lang) -> {Type, []}.

tr_xfield_options({Value, Label}, Lang) ->
    #xmlel{name = <<"option">>,
	   attrs =
	       [{<<"label">>, translate:translate(Lang, Label)}],
	   children =
	       [#xmlel{name = <<"value">>, attrs = [],
		       children = [{xmlcdata, Value}]}]}.

tr_xfield_values(Value) ->
%% Return the XForm variable name for a subscription option key.
%% Return the XForm variable type for a subscription option key.
    #xmlel{name = <<"value">>, attrs = [],
	   children = [{xmlcdata, Value}]}.

xfield_var(deliver) -> ?PUBSUB_DELIVER;
%xfield_var(digest) -> ?PUBSUB_DIGEST;
%xfield_var(digest_frequency) ->
%    ?PUBSUB_DIGEST_FREQUENCY;
%xfield_var(expire) -> ?PUBSUB_EXPIRE;
%xfield_var(include_body) -> ?PUBSUB_INCLUDE_BODY;
xfield_var(show_values) -> ?PUBSUB_SHOW_VALUES;
xfield_var(subscription_type) ->
    ?PUBSUB_SUBSCRIPTION_TYPE;
xfield_var(subscription_depth) ->
    ?PUBSUB_SUBSCRIPTION_DEPTH.

xfield_type(deliver) -> <<"boolean">>;
%xfield_type(digest) -> <<"boolean">>;
%xfield_type(digest_frequency) -> <<"text-single">>;
%xfield_type(expire) -> <<"text-single">>;
%xfield_type(include_body) -> <<"boolean">>;
xfield_type(show_values) ->
    {<<"list-multi">>,
     [{<<"away">>, ?SHOW_VALUE_AWAY_LABEL},
      {<<"chat">>, ?SHOW_VALUE_CHAT_LABEL},
      {<<"dnd">>, ?SHOW_VALUE_DND_LABEL},
      {<<"online">>, ?SHOW_VALUE_ONLINE_LABEL},
      {<<"xa">>, ?SHOW_VALUE_XA_LABEL}]};
xfield_type(subscription_type) ->
    {<<"list-single">>,
     [{<<"items">>, ?SUBSCRIPTION_TYPE_VALUE_ITEMS_LABEL},
      {<<"nodes">>, ?SUBSCRIPTION_TYPE_VALUE_NODES_LABEL}]};
xfield_type(subscription_depth) ->
    {<<"list-single">>,
     [{<<"1">>, ?SUBSCRIPTION_DEPTH_VALUE_ONE_LABEL},
      {<<"all">>, ?SUBSCRIPTION_DEPTH_VALUE_ALL_LABEL}]}.

%% Return the XForm variable label for a subscription option key.
xfield_label(deliver) -> ?DELIVER_LABEL;
%xfield_label(digest) -> ?DIGEST_LABEL;
%xfield_label(digest_frequency) ->
%    ?DIGEST_FREQUENCY_LABEL;
%xfield_label(expire) -> ?EXPIRE_LABEL;
%xfield_label(include_body) -> ?INCLUDE_BODY_LABEL;
xfield_label(show_values) -> ?SHOW_VALUES_LABEL;
%% Return the XForm value for a subscription option key.
%% Convert erlang booleans to XForms.
xfield_label(subscription_type) ->
    ?SUBSCRIPTION_TYPE_LABEL;
xfield_label(subscription_depth) ->
    ?SUBSCRIPTION_DEPTH_LABEL.

xfield_val(deliver, Val) -> [bool_to_xopt(Val)];
%xfield_val(digest, Val) -> [bool_to_xopt(Val)];
%xfield_val(digest_frequency, Val) ->
%    [iolist_to_binary(integer_to_list(Val))];
%xfield_val(expire, Val) ->
%    [jlib:now_to_utc_string(Val)];
%xfield_val(include_body, Val) -> [bool_to_xopt(Val)];
xfield_val(show_values, Val) -> Val;
xfield_val(subscription_type, items) -> [<<"items">>];
xfield_val(subscription_type, nodes) -> [<<"nodes">>];
xfield_val(subscription_depth, all) -> [<<"all">>];
xfield_val(subscription_depth, N) ->
    [iolist_to_binary(integer_to_list(N))].

bool_to_xopt(false) -> <<"false">>;
bool_to_xopt(true) -> <<"true">>.
