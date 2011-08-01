-module(privacy_helper).

-include_lib("escalus/deps/exmpp/include/exmpp.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-export([config_list/2,
         set_and_activate/2,
         set_list/2,
         activate_list/2,
         privacy_list/2,
         privacy_list_item/1,
         is_privacy_list_push/1,
         is_presence_error/1,
         verify_result/1,
         verify_push/1,
         verify_presence_error/1]).

config_list(Name, Config) ->
    {Name, ?config(Name, Config)}.

%% Sets the list on server and makes it the active one.
set_and_activate(Client, {ListName, PrivacyList}) ->
    set_list(Client, {ListName, PrivacyList}),
    activate_list(Client, {ListName, PrivacyList}).

%% Sets the list on server.
set_list(Client, {_ListName, PrivacyList}) ->
    %% set list
    escalus_client:send(Client,
        escalus_stanza:privacy_set_one(Client, PrivacyList)),
    %% skip responses
    _ClientResponses = escalus_client:wait_for_stanzas(Client, 2).

%% Make the list the active one.
activate_list(Client, {ListName, _PrivacyList}) ->
    %% activate it
    escalus_client:send(Client,
        escalus_stanza:privacy_activate(Client, ListName)),
    true = exmpp_iq:is_result(escalus_client:wait_for_stanza(Client)).

%% Create empty list element with given name.
privacy_list(Name, Items) ->
    exmpp_xml:append_children(
        exmpp_xml:set_attribute(
            exmpp_xml:remove_attribute(
                exmpp_xml:element('list'),
                <<"xmlns">>),
            {<<"name">>, Name}),
        Items).

%% Create a privacy list item element, wrapping up arguments as attributes.
privacy_list_item(ItemDescription) ->
    Attrs = case ItemDescription of
        {Action, Order, Contents} ->
            [{<<"action">>, Action}, {<<"order">>, Order}];
        {Type, Value, Action, Order, Contents} ->
            [{<<"type">>, Type}, {<<"value">>, Value}, {<<"action">>, Action}, 
             {<<"order">>, Order}]
    end,
    ContentElements = [ exmpp_xml:element(Content) || Content <- Contents ],
    exmpp_xml:append_children(
        exmpp_xml:set_attributes(exmpp_xml:element('item'), Attrs),
        ContentElements).

%% Is this iq a notification about a privacy list being changed?
is_privacy_list_push(Iq) ->
    escalus_assert:is_iq('set', Iq),
    Query = exmpp_xml:get_element(Iq, ?NS_PRIVACY, 'query'),
    true = exmpp_xml:has_element(Query, 'list'),
    true.

is_presence_error(Stanza) ->
    true = exmpp_presence:is_presence(Stanza),
    error = exmpp_presence:get_type(Stanza),
    <<"modify">> = exmpp_stanza:get_error_type(Stanza),
    'not-acceptable' = exmpp_stanza:get_condition(Stanza),
    true.

verify_result(Stanza) ->
    try escalus_assert:is_iq('result', Stanza) of
        _ -> true
    catch
        _:_ -> false
    end.

verify_push(Stanza) ->
    try is_privacy_list_push(Stanza) of
        _ -> true
    catch
        _:_ -> false
    end.

verify_presence_error(Stanza) ->
    try is_presence_error(Stanza) of
        true -> true;
        _ -> false
    catch
        _:_ -> false
    end.
