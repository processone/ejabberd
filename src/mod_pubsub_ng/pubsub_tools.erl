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
%%% Portions created by ProcessOne are Copyright 2006-2011, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2011, ProcessOne.
%%%
%%% @copyright 2006-2011 ProcessOne
%%% @author Karim Gemayel <karim.gemayel@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @headerfile "pubsub_dev.hrl"

-module(pubsub_tools).
-author('karim.gemayel@process-one.net').

-compile(export_all).

-include("pubsub_dev.hrl").

jid_to_string({undefined, S, undefined}) ->
    jlib:jid_to_string({<<>>, S, <<>>});
jid_to_string({undefined, S, R}) ->
    jlib:jid_to_string({<<>>, S, R});
jid_to_string({U, S, undefined}) ->
    jlib:jid_to_string({U, S, <<>>});
jid_to_string(USR) ->
    jlib:jid_to_string(USR).

make_jid({undefined, S, undefined}) ->
    jlib:make_jid(<<>>, S, <<>>);
make_jid({U, S, undefined}) ->
    jlib:make_jid({U, S, <<>>});
make_jid(Entity) ->
    jlib:make_jid(Entity).

%% @doc Determine if data is a JID
-spec(is_jid/1 ::
(
  Data :: binary() | undefined)
    -> xmpp_jid:entity() | {error, 'jid-malformed'}
).

is_jid(Data) ->
    try jlib:string_to_jid(Data) of
        #jid{} = Jid -> Jid
    catch
        _Error -> {error, 'jid-malformed'}
    end.

%%%
%-spec(get_value/2 ::
%(
%  Options :: {Node_Options :: pubsub_options:options_node(),
%              Item_Options :: [] | pubsub_options:options_item()}
%           | pubsub_options:options_item()
%           | pubsub_options:options_node()
%           | pubsub_options:options_subscription()
%           | [],
%  Key     :: atom())
%    -> Value :: atom()
%              | binary()
%              | boolean()
%              | non_neg_integer()
%              | undefined
%              | []
%              | 'none'
%              | [atom(),...]
%              | [binary(),...]
%              | [boolean(),...]
%              | [non_neg_integer(),...]
%).

get_value({Node_Options, [] = _Item_Options}, Key) ->
    get_value(Node_Options, Key, 0);
%%
get_value({Node_Options, Item_Options}, Key) ->
    case get_value(Item_Options, Key, 'none') of
        'none' -> get_value(Node_Options, Key, false);
        Value -> Value
    end;
%%
get_value([], _Key) -> 'none';
%%
get_value(Options, Key) ->
    get_value(Options, Key, 'none').

%%%
%-spec(get_value/3 ::
%(
%  Options :: {Node_Options :: pubsub_options:options_node(),
%              Item_Options :: [] | pubsub_options:options_item()}
%           | pubsub_options:options_item()
%           | pubsub_options:options_node()
%           | pubsub_options:options_subscription(),
%  Key     :: atom(),
%  Default :: term())
%    -> Value :: atom()
%              | binary()
%              | boolean()
%              | non_neg_integer()
%              | undefined
%              | []
%              | [atom(),...]
%              | [binary(),...]
%              | [boolean(),...]
%              | [non_neg_integer(),...]
%).
get_value({Node_Options, [] = _Item_Options}, Key, Default) ->
    get_value(Node_Options, Key, Default);
%%
get_value({Node_Options, Item_Options}, Key, Default) ->
    case get_value(Item_Options, Key, 'none') of
        'none' -> get_value(Node_Options, Key, Default);
        Value  -> Value
    end;
%%
get_value(Options, Key, Default) ->
    case lists:keyfind(Key, 1, Options) of
        {_Key, Value} -> Value;
        false         -> Default
    end.

%%%
%-spec(set_value/3 ::
%(
%  Options :: pubsub_options:options_item()
%           | pubsub_options:options_node()
%           | pubsub_options:options_subscription(),
%  Key     :: atom(),
%  Value   :: atom()
%           | binary()
%           | boolean()
%           | non_neg_integer()
%           | undefined
%           | []
%           | [atom(),...]
%           | [binary(),...]
%           | [boolean(),...]
%           | [non_neg_integer(),...])
%    -> Options :: pubsub_options:options_item()
%                | pubsub_options:options_node()
%                | pubsub_options:options_subscription()
%).

set_value(Options, Key, Value) ->
    lists:keyreplace(Key, 1, Options, {Key, Value}).

%%

%-spec(get_option/2 ::
%(
%  Options :: {Node_Options :: pubsub_options:options_node(),
%              Item_Options :: [] | pubsub_options:options_item()}
%           | pubsub_options:options_item()
%           | pubsub_options:options_node()
%           | pubsub_options:options_subscription()
%           | [],
%  Key     :: atom())
%    -> Option :: pubsub_options:option_item()
%               | pubsub_options:option_node()
%               | pubsub_options:option_subscription()
%               | 'none'
%).

get_option({Node_Options, [] = _Item_Options}, Key) ->
    get_option(Node_Options, Key, 'none');
%%
get_option({Node_Options, Item_Options}, Key) ->
    case get_option(Item_Options, Key, 'none') of
        'none' -> get_option(Node_Options, Key, 'none');
        Option -> Option
    end;
%%
get_option([], _Key) -> 'none';
%%
get_option(Options, Key) ->
    get_option(Options, Key, 'none').

%%%
%-spec(get_option/3 ::
%(
%  Options :: {Node_Options :: pubsub_options:options_node(),
%              Item_Options :: [] | pubsub_options:options_item()}
%           | pubsub_options:options_item()
%           | pubsub_options:options_node()
%           | pubsub_options:options_subscription(),
%  Key     :: atom(),
%  Default :: term())
%    -> Option :: pubsub_options:option_item()
%               | pubsub_options:option_node()
%               | pubsub_options:option_subscription()
%               | term()
%).

get_option({Node_Options, [] = _Item_Options}, Key, Default) ->
    get_option(Node_Options, Key, Default);
%%
get_option({Node_Options, Item_Options}, Key, Default) ->
    case get_option(Item_Options, Key, 'none') of
        'none'  -> get_option(Node_Options, Key, Default);
        Option  -> Option
    end;
%%
get_option(Options, Key, Default) ->
    case lists:keyfind(Key, 1, Options) of
        {_Key, Value} -> _Option = {Key, Value};
        false         -> Default
    end.

%%
-spec(get_entity_roster/1 ::
(
  Entity :: xmpp_jid:usr_entity()
          | xmpp_jid:entity())
    ->  Roster::[Roster_Item::#roster{}]
).

get_entity_roster({U,S, _R} = _USR_Entity) ->
    _Roster = ejabberd_hooks:run_fold(roster_get, S, [], [{U,S}]);
get_entity_roster(#jid{luser = U, lserver = S} = _Jid_Entity) ->
    _Roster = get_entity_roster({U,S, undefined}).

%%
-spec(check_access_model/3 ::
(
  Host     :: xmpp_jid:raw_jid_component_bare(),
  Entity   :: xmpp_jid:usr_entity(),
  Criteria :: {Access_Model           :: pubsub_options:access_model(),
               Affiliation            :: exmpp_pubsub:affiliation(),
               Subscriptions          :: exmpp_pubsub:subscriptions(),
               Node_Owners            :: [Node_Owner::xmpp_jid:usr_bare(),...],
               Rosters_Groups_Allowed :: pubsub_options:rosters_groups_allowed()})
    -> ok
    %%%
     | {error, 'forbidden'}
).

check_access_model(_Host, _Entity,
  {_Access_Model, 'outcast' = _Affiliation, _Subscriptions, _Node_Owners,
   _Rosters_Groups_Allowed}) ->
    {error, 'forbidden'};
%%
check_access_model(_Host, _Entity,
  {'open' = _Access_Model, _Affiliation, _Subscriptions, _Node_Owners,
   _Rosters_Groups_Allowed}) ->
    ok;
%%
check_access_model(Host, Entity,
  {'presence' = _Access_Model, _Affiliation, _Subscriptions, Node_Owners,
   _Rosters_Groups_Allowed}) ->
    case is_contact_subscribed_to_node_owners(Host, Entity, Node_Owners) of
        false ->
            {error, 'forbidden'};
        _Node_Owner ->
            ok
    end;
%%
check_access_model(_Host, Entity,
  {'roster' = _Access_Model, _Affiliation, _Subscriptions, _Node_Owners,
   Rosters_Groups_Allowed}) ->
    case is_contact_in_allowed_roster_groups(Entity, Rosters_Groups_Allowed) of
        false ->
            {error, 'forbidden'};
        {_Node_Owner, _Roster_Group_Allowed} ->
            ok
    end;
%%
check_access_model(_Host, _Entity,
  {'whitelist' = _Access_Model, Affiliation, _Subscriptions, _Node_Owners,
   _Roster_Groups_Allowed})
  when Affiliation == 'member'
  %%orelse Affiliation == 'owner'
  orelse Affiliation == 'publish-only'
  orelse Affiliation == 'publisher' ->
    ok;
check_access_model(_Host, _Entity,
  {'whitelist' = _Access_Model, _Affiliation, _Subscriptions, _Node_Owners,
   _Roster_Groups_Allowed}) ->
    {error, 'forbidden'};
%%
check_access_model(_Host, _Entity,
  {'authorize' = _Access_Model, _Affiliation, Subscriptions, _Node_Owners,
   _Roster_Groups_Allowed}) ->
    case has_subscriptions(Subscriptions) of
        true  -> ok;
        false -> {error, 'forbidden'}
    end.



%%
-spec(check_access_model/7 ::
(
  Host                   :: xmpp_jid:raw_jid_component_bare(),
  Entity                 :: xmpp_jid:usr_entity(),
  Access_Model           :: pubsub_options:access_model(),
  Affiliation            :: exmpp_pubsub:affiliation(),
  Subscriptions          :: exmpp_pubsub:subscriptions(),
  Node_Owners            :: [Node_Owner::xmpp_jid:usr_bare(),...],
  Rosters_Groups_Allowed :: pubsub_options:rosters_groups_allowed())
    -> ok
    %%%
     | {error, 'forbidden'}
     | {error, 'item-not-found'}
).

check_access_model(_Host, _Entity, _Access_Model, 'owner' = _Affiliation,
  _Subscriptions, _Node_Owners, _Rosters_Groups_Allowed) ->
    ok;
%%
check_access_model(Host, Entity, Access_Model, 'outcast' = _Affiliation,
  _Subscriptions, Node_Owners, Rosters_Groups_Allowed) ->
    case Access_Model of
        %%
        'open'->
              {error, 'forbidden'};
        %%
        'presence' ->
            case
                is_contact_subscribed_to_node_owners(Host, Entity, Node_Owners)
            of
                false ->
                    {error, 'item-not-found'};
                _Node_Owner ->
                    {error, 'forbidden'}
            end;
        %%
        'roster' ->
            case
                is_contact_in_allowed_roster_groups(Entity,
                    Rosters_Groups_Allowed)
            of
                false ->
                    {error, 'item-not-found'};
                {_Node_Owner, _Roster_Group_Allowed} ->
                    {error, 'forbidden'}
            end;
        %%
        'authorize' ->
            {error, 'forbidden'};
        %%
        'whitelist' ->
            {error, 'item-not-found'}
    end;
%%
check_access_model(_Host, _Entity, 'open' = _Access_Model, _Affiliation,
  _Subscriptions, _Node_Owners, _Rosters_Groups_Allowed) ->
    ok;
%%
check_access_model(Host, Entity, 'presence' = _Access_Model, _Affiliation,
  _Subscriptions, Node_Owners, _Rosters_Groups_Allowed) ->
    case is_contact_subscribed_to_node_owners(Host, Entity, Node_Owners) of
        false ->
            {error, 'item-not-found'};
        _Node_Owner ->
            ok
    end;
%%
check_access_model(_Host, Entity, 'roster' = _Access_Model, _Affiliation,
  _Subscriptions, _Node_Owners, Rosters_Groups_Allowed) ->
    case is_contact_in_allowed_roster_groups(Entity, Rosters_Groups_Allowed) of
        false ->
            {error, 'item-not-found'};
        {_Node_Owner, _Roster_Group_Allowed} ->
            ok
    end;
%%
check_access_model(_Host, _Entity, 'whitelist' = _Access_Model, Affiliation,
  _Subscriptions, _Node_Owners, _Roster_Groups_Allowed) ->
    case Affiliation of
        Affiliation
          when   Affiliation == 'member'
          orelse Affiliation == 'publish-only'
          orelse Affiliation == 'publisher' ->
            ok;
        _Affiliation ->
            {error, 'item-not-found'}
    end;
%%
check_access_model(_Host, _Entity, 'authorize' = _Access_Model, _Affiliation,
  Subscriptions, _Node_Owners, _Roster_Groups_Allowed) ->
    case has_subscriptions(Subscriptions) of
        true  ->
            ok;
        false ->
            {error, 'item-not-found'}
    end.


-spec(check_publish_model/3 ::
(
  Publish_Model :: pubsub_options:publish_model(),
  Affiliation   :: exmpp_pubsub:affiliation(),
  Subscriptions :: exmpp_pubsub:subscriptions())
    -> ok
    %%%
     | {error, 'forbidden'}
).

check_publish_model('open' = _Publish_Model, _Affiliation, _Subscriptions) ->
    ok;
%%
check_publish_model('publishers' = _Publish_Model, Affiliation, _Subscriptions) ->
    case Affiliation of
        Affiliation
          when   Affiliation == 'owner'
          orelse Affiliation == 'publish-only'
          orelse Affiliation == 'publisher' ->
            ok;
        _Affiliation ->
            {error, 'forbidden'}
    end;
%%
check_publish_model('subscribers' = _Publish_Model, Affiliation, Subscriptions) ->
    case Affiliation of
        'owner' ->
            ok;
        _Affiliation ->
            case has_subscriptions(Subscriptions) of
                true ->
                    ok;
                false ->
                    {error, 'forbidden'}
            end
    end.

%%
-spec(has_subscriptions/1 ::
(
  Subscriptions :: exmpp_pubsub:subscriptions())
    -> Has_Subscriptions::boolean()
).

has_subscriptions([] = _Subscriptions) ->
    false;
has_subscriptions(
  [{'pending' = _Subscription_State, _SubId, _Resource, _Subscription_Options}]) ->
    false;
has_subscriptions(_Subscriptions) ->
    true.

%%
%% @doc Check if a contact is subscribed to at least one local node owner
%%      amongst a list of (remote and local) node owners
-spec(is_contact_subscribed_to_node_owners/3 ::
(
  Host        :: xmpp_jid:raw_jid_component_bare(),
  Contact     :: xmpp_jid:usr_entity(),
  Node_Owners :: [] | [Node_Owner::xmpp_jid:usr_bare(),...])
    -> Is_Contact_Subscribed_To_Node_Owners :: false | xmpp_jid:usr_bare()
).

is_contact_subscribed_to_node_owners(_Host, _Contact, [] = _Node_Owners) ->
    false;
%% The node owner is a local entity, check if the contact is subscribed to it
is_contact_subscribed_to_node_owners(Host, Contact,
  [{_U, Host, _R} = Local_Node_Owner | Node_Owners] = _Node_Owners) ->
    _Is_Contact_Subscribed_To_Node_Owners = case
        is_contact_subscribed_to_entity(Contact,
            _Local_Node_Owner_Roster = get_entity_roster(Local_Node_Owner))
    of
        true ->
            Local_Node_Owner;
        false ->
            is_contact_subscribed_to_node_owners(Host, Contact, Node_Owners)
    end;
%% The node owner is a remote entity, don't check if the contact is subscribed to it
is_contact_subscribed_to_node_owners(Host, Contact,
  [_Remote_Node_Owner | Node_Owners] = _Node_Owners) ->
    is_contact_subscribed_to_node_owners(Host, Contact, Node_Owners).

%%
%% @doc Check if an entity is in a #roster{}
%%      with #roster.subscription == 'from' or
%%      with #roster.subscription == 'both'
-spec(is_contact_subscribed_to_entity/2 ::
(
  Contact       :: xmpp_jid:usr_entity(),
  Entity_Roster :: [] | [Roster_Item::#roster{},...])
    -> Is_Contact_Subscribed::boolean()
).

is_contact_subscribed_to_entity(_Contact, [] = _Entity_Roster) ->
    _Is_Contact_Subscribed = false;
%% Contact is included in a #roster{} item with
%% a subscription of type 'from' and 'both' (i.e. is subscribed to it)
is_contact_subscribed_to_entity({U, S, _R} = _Contact,
  [#roster{jid = {U, S, _}, subscription = Subscription} = _Roster_Item
  | _Roster_Items] = _Entity_Roster)
   when Subscription == 'from' orelse Subscription == 'both' ->
    _Is_Contact_Subscribed = true;
%% Contact is not included in a #roster{} item
%% or has a subscription of type different than 'from' and 'both'
%% ( i.e. is not subscribed to it),
%% check next roster items
is_contact_subscribed_to_entity(Contact,
  [_Roster_Item | Roster_Items] = _Entity_Roster) ->
    _Is_Contact_Subscribed = is_contact_subscribed_to_entity(Contact,
        Roster_Items).

%%
-spec(is_contact_in_allowed_roster_groups/2 ::
(
  Contact                :: xmpp_jid:usr_entity(),
  Rosters_Groups_Allowed :: pubsub_options:rosters_groups_allowed())
    -> Roster_Group_Allowed :: false
                             | {Entity       :: xmpp_jid:usr_bare(),
                                Roster_Group :: pubsub_options:roster_group()}
).

is_contact_in_allowed_roster_groups(_Contact, [] = _Rosters_Groups_Allowed) ->
    false;
%%
is_contact_in_allowed_roster_groups(Contact,
  [{Entity, Entity_Roster_Groups_Allowed} | Rosters_Groups_Allowed]
  = _Rosters_Groups_Allowed) ->
    case
        is_contact_in_allowed_roster_group(Contact,
            _Entity_Roster = get_entity_roster(Entity),
            Entity_Roster_Groups_Allowed)
    of
        false ->
            is_contact_in_allowed_roster_groups(Contact, Rosters_Groups_Allowed);
        Roster_Group ->
            {Entity, Roster_Group}
    end.

%%
-spec(is_contact_in_allowed_roster_group/3 ::
(
  Contact               :: xmpp_jid:usr_entity(),
  Entity_Roster         :: [] | [Roster_Item::#roster{groups::pubsub_options:roster_groups()}],
  Roster_Groups_Allowed :: [Roster_Group_Allowed::pubsub_options:roster_group(),...])
    -> Roster_Group :: false | pubsub_options:roster_group()
).

is_contact_in_allowed_roster_group(_Contact, [] = _Entity_Roster,
  _Roster_Groups_Allowed) ->
    false;
%%
is_contact_in_allowed_roster_group({U, S, _R} = _Contact,
  [#roster{jid = {U, S, _}, subscription = Subscription, groups = Roster_Groups}
  | _Other_Roster_Items] = _Entity_Roster, Roster_Groups_Allowed)
  when Subscription == 'from' orelse Subscription == 'both' ->
    is_contact_in_allowed_roster_group(Roster_Groups, Roster_Groups_Allowed);
%%
is_contact_in_allowed_roster_group(Contact,
  [_Roster_Item | Roster_Items] = _Entity_Roster, Roster_Groups_Allowed) ->
    is_contact_in_allowed_roster_group(Contact, Roster_Items,
        Roster_Groups_Allowed).

%%
-spec(is_contact_in_allowed_roster_group/2 ::
(
  Roster_Groups         :: [] | [Roster_Group::pubsub_options:roster_group()],
  Roster_Groups_Allowed :: [Roster_Group_Allowed::pubsub_options:roster_group(),...])
    -> Roster_Group :: false | pubsub_options:roster_group()
).

is_contact_in_allowed_roster_group([] = _Roster_Groups, _Roster_Groups_Allowed) ->
    false;
%%
is_contact_in_allowed_roster_group([Roster_Group | Roster_Groups]
  = _Roster_Groups, Roster_Groups_Allowed) ->
    case lists:member(Roster_Group, Roster_Groups_Allowed) of
        true ->
            Roster_Group;
        false ->
            is_contact_in_allowed_roster_group(Roster_Groups,
                Roster_Groups_Allowed)
    end.

%%
-spec(get_user_resources/2 ::
(
  User   :: binary(),
  Server :: binary())
    -> Resources::[Resource::xmpp_jid:resource_jid()]
).

get_user_resources(User, Server) ->
    ejabberd_sm:get_user_resources(User, Server).

%%
-spec(get_resources_show/2 ::
(
  User   :: binary(),
  Server :: binary())
    -> Resources_Show :: [{Resource :: xmpp_jid:resource_jid(),
                           Show :: 'away' | 'chat' | 'dnd' | 'online' | 'xa' }]
).

get_resources_show(User, Server) ->
    _Resources_Show = lists:foldl(fun
        (Resource, Resources_Show) ->
            case ejabberd_sm:get_session_pid(User, Server, Resource) of
                C2SPid when is_pid(C2SPid) ->
                    case ejabberd_c2s:get_presence(C2SPid) of
                        {_User, _Resource, Show, _} ->
                            [{Resource, list_to_atom(Show)} | Resources_Show];
                        _ ->
                            Resources_Show
                    end;
                _  ->
                    Resources_Show
            end
    end, [], _Resources = ejabberd_sm:get_user_resources(User, Server)).


%%
-spec(rosters_groups_allowed_cache/2 ::
(
  Rosters_Groups_Allowed       :: pubsub_options:rosters_groups_allowed(),
  Rosters_Groups_Allowed_Cache :: pubsub_options:rosters_groups_allowed())
    -> Is_Roster_Groups_Cached::boolean()
).

rosters_groups_allowed_cache(_Rosters_Groups_Allowed,
  [] = _Rosters_Groups_Allowed_Cache) ->
    false;
%%
rosters_groups_allowed_cache([] = _Rosters_Groups_Allowed,
  _Rosters_Groups_Allowed_Cache) ->
    false;
%%
rosters_groups_allowed_cache(
  [{Entity, Roster_Groups_Allowed} | Rosters_Groups_Allowed],
  Rosters_Groups_Allowed_Cache) ->
    case lists:keyfind(Entity, 1, Rosters_Groups_Allowed_Cache) of
        {_Entity, Roster_Groups_Allowed_Cache} ->
            case
                roster_groups_allowed_cache(Roster_Groups_Allowed,
                    Roster_Groups_Allowed_Cache)
            of
                true ->
                    true;
                false ->
                    rosters_groups_allowed_cache(Rosters_Groups_Allowed,
                        Rosters_Groups_Allowed_Cache)
            end;
        false ->
            rosters_groups_allowed_cache(Rosters_Groups_Allowed,
                Rosters_Groups_Allowed_Cache)
    end.

%%
-spec(roster_groups_allowed_cache/2 ::
(
  Roster_Groups_Allowed       :: [Roster_Group_Allowed::pubsub_options:roster_group()],
  Roster_Groups_Allowed_Cache :: [Roster_Group_Allowed_Cache::pubsub_options:roster_group()])
    -> Is_Roster_Groups_Cached :: boolean()
).

roster_groups_allowed_cache(_Roster_Groups_Allowed,
  [] = _Roster_Groups_Allowed_Cache) ->
    false;
%%
roster_groups_allowed_cache([] = _Roster_Groups_Allowed,
  _Roster_Groups_Allowed_Cache) ->
    false;
%%
roster_groups_allowed_cache([Roster_Group_Allowed | Roster_Groups_Allowed],
  Roster_Groups_Allowed_Cache) ->
    case lists:member(Roster_Group_Allowed, Roster_Groups_Allowed_Cache) of
        true ->
            true;
        false ->
            roster_groups_allowed_cache(Roster_Groups_Allowed,
                Roster_Groups_Allowed_Cache)
    end.

