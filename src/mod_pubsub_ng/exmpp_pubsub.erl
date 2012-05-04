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
%%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2012, ProcessOne.
%%%
%%% @copyright 2006-2012 ProcessOne
%%% @author Karim Gemayel <karim.gemayel@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @headerfile "pubsub_dev.hrl"

-module(exmpp_pubsub).
-author('karim.gemayel@process-one.net').

-compile(export_all).

-include("pubsub_dev.hrl").


%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------



%-- Affiliations --%
-export_type([
  affiliation/0,
  %%
  affiliation_owner/0,
  affiliation_publisher/0,
  affiliation_publish_only/0,
  affiliation_member/0,
  affiliation_none/0,
  affiliation_outcast/0
]).

%% @type affiliation_owner() = 'owner'.
-type(affiliation_owner() :: 'owner').

%% @type affiliation_publisher() = 'publisher'.
-type(affiliation_publisher() :: 'publisher').

%% @type affiliation_publish_only() = 'publish-only'.
-type(affiliation_publish_only() :: 'publish-only').

%% @type affiliation_member() = 'member'.
-type(affiliation_member() :: 'member').

%% @type affiliation_none() = 'none'.
-type(affiliation_none() :: 'none').

%% @type affiliation_outcast() = 'outcast'.
-type(affiliation_outcast() :: 'outcast').

%% @type affiliation() = Owner | Publisher | Publish_Only | Member | None | Outcast
%%    Owner = exmpp_pubsub:affiliation_owner()
%%    Publisher = exmpp_pubsub:affiliation_publisher()
%%    Publish_Only = exmpp_pubsub:affiliation_publish_only()
%%    Member = exmpp_pubsub:affiliation_member()
%%    None = exmpp_pubsub:affiliation_none()
%%    Outcast = exmpp_pubsub:affiliation_outcast().
-type(affiliation() :: exmpp_pubsub:affiliation_owner()
                     | exmpp_pubsub:affiliation_publisher()
                     | exmpp_pubsub:affiliation_publish_only()
                     | exmpp_pubsub:affiliation_member()
                     | exmpp_pubsub:affiliation_none()
                     | exmpp_pubsub:affiliation_outcast()
).


%%

%-- Things identifiers --%
-export_type([
  index/0,
  itemId/0,
  itemIds/0,
  level/0,
  nodeId/0,
  nodeIdx/0,
  subId/0,
  plugin/0
]).

%% @type index() = 'node'.
-type(index() :: 'node' ).

%% @type itemId() = binary().
-type(itemId() :: binary()).

%% @type itemIds() = [ItemId::exmpp_pubsub:itemId(),...].
-type(itemIds() ::  [ItemId::exmpp_pubsub:itemId(),...]).

%% @type level() = non_neg_integer().
-type(level() :: non_neg_integer()).

%% @type nodeId() = binary().
-type(nodeId() :: binary()).

%% @type nodeIdx() = non_neg_integer().
-type(nodeIdx() :: non_neg_integer()).

%% @type plugin() = module().
-type(plugin() :: module()).

%% @type subId() = binary().
-type(subId() :: binary()).


-export_type([
  t_now/0
]).

%% Temporary -type, until calendar.erl -type is exported
-type(t_now()
  :: {MegaSec::non_neg_integer(),
      Sec::non_neg_integer(),
      MilliSec::non_neg_integer()}
).


%%
-export_type([
  feature/0,
  features/0,
  pubsub_feature/0,
  pubsub_features/0
]).

-type(feature() :: binary()).

-type(features() :: Features::[Feature::exmpp_pubsub:feature(),...]).

-type(pubsub_feature() :: exmpp_pubsub:feature()).

-type(pubsub_features() :: Pubsub_Features::[Pubsub_Feature::exmpp_pubsub:feature(),...]).

%%

%-- Payload types --%
-export_type([
    payload/0,
    %%
    payload_empty/0,
    payload_full/0
]).

%% @type payload_empty() = [].
-type(payload_empty() :: []).

%% @type payload_full() = #xmlel{}.
-type(payload_full() :: xmlel()).

%% @type payload() = Empty_Payload | Full_Payload
%%    Empty_Payload = exmpp_pubsub:payload_empty()
%%    Full_Payload = exmpp_pubsub:payload_full().
-type(payload() :: exmpp_pubsub:payload_empty()
                 | exmpp_pubsub:payload_full()
).


%%

%-- Pusbub Host --%
-export_type([
    host/0,
    %%
    host_pubsub/0,
    host_pep/0
]).

%% @type host_pubsub() = xmpp_jid:domain_jid().
%%    ```<<"pubsub.shakespeare.lit">>'''
%%    Identifier type of the Pubsub service
-type(host_pubsub() :: binary()).

%% @type host_pep() = xmpp_jid:usr_contact_bare().
%%    ```{<<"juliet">>, <<"capulet.lit">>, undefined}'''
%%    Identifier type of the Pubsub-On-Jid service
-type(host_pep() :: xmpp_jid:usr_contact_bare()).

%% @type host() = Pubsub_Host | Pubsub_On_Jid_Host
%%    Pubsub_Host = exmpp_pubsub:host_pubsub()
%%    Pubsub_On_Jid_Host = exmpp_pubsub:host_pep().
%%    Identifier types of the Pubsub and PEP (Pubsub-On-Jid) services
%%    ```* Pubsub_Host : <<"pubsub.shakespeare.lit">>
%%       * Pubsub_PEP  : {<<"juliet">>, <<"capulet.lit">>, undefined}'''
-type(host() :: exmpp_pubsub:host_pubsub()
              | exmpp_pubsub:host_pep()
).


%%

%-- Subscription States --%
-export_type([
  subscription_state/0,
  %%
  subscription_state_none/0,
  subscription_state_pending/0,
  subscription_state_unconfigured/0,
  subscription_state_subscribed/0
]).

%% @type subscription_state_none() = 'none'.
-type(subscription_state_none() :: 'none').

%% @type subscription_state_pending() = 'pending'.
-type(subscription_state_pending() :: 'pending').

%% @type subscription_state_unconfigured() = 'unconfigured'.
-type(subscription_state_unconfigured() :: 'unconfigured').

%% @type subscription_state_subscribed() = 'subscribed'.
-type(subscription_state_subscribed() :: 'subscribed').

%% @type subscription_state() = None | Pending | Unconfigured | Subscribed
%%    None = exmpp_pubsub:subscription_state_none()
%%    Pending = exmpp_pubsub:subscription_state_pending()
%%    Unconfigured = exmpp_pubsub:subscription_state_unconfigured()
%%    Subscribed = exmpp_pubsub:subscription_state_subscribed()
-type(subscription_state() :: exmpp_pubsub:subscription_state_pending()
                            | exmpp_pubsub:subscription_state_unconfigured()
                            | exmpp_pubsub:subscription_state_subscribed()
).

%%

%-- Subscription -- %%
-export_type([
  subscriptions/0,
  subscription/0,
  %%
  subscription_subscribed/0,
  subscription_unconfigured/0,
  subscription_pending/0
]).

-type(subscription_subscribed()
  :: {Subscription_State   :: exmpp_pubsub:subscription_state_subscribed(),
      SubId                :: exmpp_pubsub:subId(),
      Resource             :: undefined | xmpp_jid:resource_jid() | {'caps', xmpp_jid:resource_jid()},
      Subscription_Options :: [] | pubsub_options:options_subscription()}
).

-type(subscription_unconfigured()
  :: {Subscription_State   :: exmpp_pubsub:subscription_state_unconfigured(),
      SubId                :: exmpp_pubsub:subId(),
      Resource             :: undefined | xmpp_jid:resource_jid() | {'caps', xmpp_jid:resource_jid()},
      Subscription_Options :: []}
).

-type(subscription_pending()
  :: {Subscription_State   :: exmpp_pubsub:subscription_state_pending(),
      SubId                :: exmpp_pubsub:subId(),
      Resource             :: undefined | xmpp_jid:resource_jid() | {'caps', xmpp_jid:resource_jid()},
      Subscription_Options :: []}
).

-type(subscription()
  :: {Subscription_State   :: exmpp_pubsub:subscription_state(),
      SubId                :: exmpp_pubsub:subId(),
      Resource             :: undefined | xmpp_jid:resource_jid() | {'caps', xmpp_jid:resource_jid()},
      Subscription_Options :: [] | pubsub_options:options_subscription()}
%%  :: exmpp_pubsub:subscription_subscribed()
%%   | exmpp_pubsub:subscription_pending()
%%   | exmpp_pubsub:subscription_unconfigured()
).

-type(subscriptions()
    :: [exmpp_pubsub:subscription(),...]
%%  :: [exmpp_pubsub:subscription_subscribed(),...]
%%   | [exmpp_pubsub:subscription_unconfigured(),...]
%%   | [exmpp_pubsub:subscription_pending(),...]
).

%% TODO : commented -type unions return the following dialyzer error (bug ?) :
%% ...
%%  Adding information from /home/meta/otp-exmpp.plt to ejabberd.plt...
%%=ERROR REPORT==== 29-Jul-2011::15:14:42 ===
%Error in process <0.2604.0> with exit value: {function_clause,[{erl_types,inf_tuples_in_sets,4},{erl_types,inf_tuple_sets,4},{erl_types,inf_tuple_sets,3},{erl_types,t_inf,3},{erl_types,t_inf_lists_strict,4},{erl_types,t_inf,3},{dialyzer_typesig,solve_one_c... 
%%
%%
%%dialyzer: Analysis failed with error: {function_clause,[{erl_types,inf_tuples_in_sets,4},
%%                  {erl_types,inf_tuple_sets,4},
%%                  {erl_types,inf_tuple_sets,3},
%%                  {erl_types,t_inf,3},
%%                  {erl_types,t_inf_lists_strict,4},
%%                  {erl_types,t_inf,3},
%%                  {dialyzer_typesig,solve_one_c,...},
%%                  {dialyzer_typesig,...}]}
%%Last messages in the log cache:
%%  Typesig analysis for SCC: [{ejabberd_web_admin,make_xhtml,5}]
%%  Typesig analysis for SCC: [{ejabberd_web_admin,make_xhtml,4}]
%%  Typesig analysis for SCC: [{ejabberd_web_admin,process_admin,2}]
%%  Typesig analysis for SCC: [{ejabberd_web_admin,process,2}]
%%  Typesig analysis for SCC: [{mod_stats,get_local_stat,3}]
%%  Typesig analysis for SCC: [{mod_stats,get_local_stats,3}]
%%  Typesig analysis for SCC: [{mod_stats,process_local_iq,3}]
%%  Typesig analysis for SCC: [{mod_register_web,send_registration_notifications,2}]
%%  Typesig analysis for SCC: [{mod_register_web,process,2}]
%%  Typesig analysis for SCC: [{pubsub_db,create_node,5}]

%%


%% Pubsub #xmlel{} templates


%% --------------------------------------------------------------------
%% Functions.
%% --------------------------------------------------------------------




%% @spec nodeId() -> NodeId::exmpp_pubsub:nodeId()
-spec(nodeId/0 :: () -> NodeId::exmpp_pubsub:nodeId()).

nodeId() ->
    randoms:get_string().

%% @spec subId() -> SubId::exmpp_pubsub:subId()
-spec(subId/0 :: () -> SubId::exmpp_pubsub:subId()).

subId() ->
    {T1, T2, T3} = now(),
    list_to_binary(lists:flatten(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3]))).

%% @spec itemId() -> ItemId::exmpp_pubsub:itemId()
-spec(itemId/0 :: () -> ItemId::exmpp_pubsub:itemId()).

itemId() ->
    {T1, T2, T3} = now(),
    list_to_binary(lists:flatten(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3]))).

id() ->
    {T1, T2, T3} = now(),
    list_to_binary(lists:flatten(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3]))).



%%
-spec(subscription_subscribed/2 ::
(
  Resource             :: undefined
                        | xmpp_jid:resource_jid()
                        | {'caps', xmpp_jid:resource_jid()},
  Subscription_Options :: [] | pubsub_options:options_subscription())
    -> Subscription_Subscribed::exmpp_pubsub:subscription_subscribed()
).

subscription_subscribed(Resource, Subscription_Options) ->
    {'subscribed', subId(), Resource, Subscription_Options}.

%%
-spec(subscription_pending/2 ::
(
  Resource             :: undefined
                        | xmpp_jid:resource_jid()
                        | {'caps', xmpp_jid:resource_jid()},
  Subscription_Options :: [] | pubsub_options:options_subscription())
    -> Subscription_Pending::exmpp_pubsub:subscription_pending()
).

subscription_pending(Resource, Subscription_Options) ->
    {'pending', subId(), Resource, Subscription_Options}.



%% Pubsub Xmlel templates

xmlcdata(CData) ->
    {xmlcdata, CData}.

xmlattr(Name, Value) ->
    {Name, Value}.


xmlel('disco#info', Name, Attrs, Children) ->
    xmlel(?NS_DISCO_INFO, Name, Attrs, Children);
xmlel('disco#items', Name, Attrs, Children) ->
    xmlel(?NS_DISCO_ITEMS, Name, Attrs, Children);
xmlel('jabber:client', Name, Attrs, Children) ->
    xmlel(?NS_JABBER_CLIENT, Name, Attrs, Children);
xmlel('pubsub', Name, Attrs, Children) ->
    xmlel(?NS_PUBSUB, Name, Attrs, Children);
xmlel('pubsub#event', Name, Attrs, Children) ->
    xmlel(?NS_PUBSUB_EVENT, Name, Attrs, Children);
xmlel('pubsub#owner', Name, Attrs, Children) ->
    xmlel(?NS_PUBSUB_OWNER, Name, Attrs, Children);
xmlel('shim', Name, Attrs, Children) ->
    xmlel(?NS_SHIM, Name, Attrs, Children);
xmlel(NS, Name, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, NS} | Attrs], children = Children}.
%%
xmlattr_affiliation(Affiliation) ->
    xmlattr(<<"affiliation">>, list_to_binary(atom_to_list(Affiliation))).
%%
xmlattr_category(Category) ->
    xmlattr(<<"category">>, Category).
%%
xmlattr_id(ItemId) ->
    xmlattr(<<"id">>, ItemId).
%%
xmlattr_jid(Jid) ->
    xmlattr(<<"jid">>, Jid).
%%
xmlattr_name(Name) ->
    xmlattr(<<"name">>, Name).
%%
xmlattr_node(NodeId) ->
    xmlattr(<<"node">>, NodeId).
%%
xmlattr_publisher(Publisher) ->
    xmlattr(<<"publisher">>, Publisher).
%%
xmlattr_subid(SubId) ->
    xmlattr(<<"subid">>, SubId).
%%
xmlattr_subscription(Subscription_State) ->
    xmlattr(<<"subscription">>, list_to_binary(atom_to_list(Subscription_State))).
%%
xmlattr_type(Type) when is_atom(Type)->
    xmlattr(<<"type">>, list_to_binary(atom_to_list(Type)));
%%
xmlattr_type(Type) ->
    xmlattr(<<"type">>, Type).
%%
xmlattr_uri(URI) ->
    xmlattr(<<"uri">>, URI).
%%
xmlattr_var(Var) ->
    xmlattr(<<"var">>, Var).

%%
xmlel_affiliation('pubsub', NodeId, Affiliation) ->
    xmlel('pubsub', <<"affiliation">>,
        [xmlattr_node(NodeId),
         xmlattr_affiliation(Affiliation)],
        []);
xmlel_affiliation('pubsub#owner', Jid, Affiliation) ->
    xmlel('pubsub#owner', <<"affiliation">>,
        [xmlattr_jid(Jid),
         xmlattr_affiliation(Affiliation)],
        []). 
%%
xmlel_affiliations('pubsub', Xmlels) ->
    xmlel('pubsub', <<"affiliations">>, [], Xmlels).

xmlel_affiliations('pubsub#owner', NodeId, Xmlels) ->
    xmlel('pubsub#owner', <<"affiliations">>, [xmlattr_node(NodeId)], Xmlels).
%%
xmlel_create(NodeId) ->
    xmlel('pubsub', <<"create">>, [xmlattr_node(NodeId)], []).

%%
xmlel_configure('pubsub#owner', NodeId, Xmlels) ->
    xmlel('pubsub#owner', <<"configure">>, [xmlattr_node(NodeId)], Xmlels).

%%
xmlel_default('pubsub', Xmlels) ->
    xmlel('pubsub', <<"default">>, [], Xmlels);
%%
xmlel_default('pubsub#owner', Xmlels) ->
    xmlel('pubsub#owner', <<"default">>, [], Xmlels).

%%
xmlel_default('pubsub', NodeId, Xmlels) ->
    xmlel('pubsub', <<"default">>, [xmlattr_node(NodeId)], Xmlels).

%%
xmlel_delete(NodeId, RedirectURI) ->
    xmlel('pubsub#event', <<"delete">>,
        [xmlattr_node(NodeId)],
        case RedirectURI of
            undefined    -> [];
            _RedirectURI -> [xmlel_redirect(RedirectURI)]
        end).
%%
xmlel_event(Xmlels) ->
    xmlel('pubsub#event', <<"event">>, [], Xmlels).
%%
xmlel_feature('disco#info', Feature) ->
    xmlel('disco#info', <<"feature">>, [xmlattr_var(Feature)], []).
%%
xmlel_header(SubId) ->
    xmlel('shim', <<"header">>, [xmlattr_name(<<"SubID">>)], [xmlcdata(SubId)]).
%%
xmlel_headers(Xmlels_Header) ->
    xmlel('shim', <<"headers">>, [], Xmlels_Header).


xmlel_identity('disco#info', Category, Type, Name) ->
    xmlel('disco#info', <<"identity">>,
        case Name of
            undefined ->
                [xmlattr_category(Category),
                 xmlattr_type(Type)];
            _ ->
                [xmlattr_category(Category),
                 xmlattr_name(Name),
                 xmlattr_type(Type)]
        end,
        []).

%%
xmlel_item('pubsub', ItemId) ->
    xmlel('pubsub', <<"item">>, [xmlattr_id(ItemId)], []).

%%
xmlel_item('disco#items', ItemId, Jid) ->
    xmlel('disco#items', <<"item">>,
        [xmlattr_name(ItemId),
         xmlattr_jid(Jid)],
        []);
xmlel_item('pubsub', ItemId, Payload) ->
    xmlel('pubsub', <<"item">>, [xmlattr_id(ItemId)],
        case Payload of
            []       -> [];
            _Payload -> [Payload]
        end).

xmlel_item('disco#items', NodeId, Jid, Name) ->
    xmlel('disco#items', <<"item">>,
        case Name of
            undefined ->
                [xmlattr_jid(Jid),
                 xmlattr_node(NodeId)];
            _ ->
                [xmlattr_jid(Jid),
                 xmlattr_node(NodeId),
                 xmlattr_name(Name)]
        end,
        []);
%%
xmlel_item('pubsub#event', ItemId, Publisher, Payload) ->
    xmlel('pubsub#event', <<"item">>,
        case Publisher of
            undefined ->
                [xmlattr_id(ItemId)];
            _Publisher ->
                [xmlattr_id(ItemId),
                 xmlattr_publisher(Publisher)]
        end,
        case Payload of
            []       -> [];
            _Payload -> [Payload]
        end).
%%
xmlel_items('pubsub', NodeId, Xmlels) ->
    xmlel('pubsub', <<"items">>, [xmlattr_node(NodeId)], Xmlels);
%%
xmlel_items('pubsub#event', undefined = _NodeId, Xmlels) ->
    xmlel('pubsub#event', <<"items">>, [], Xmlels);
%%
xmlel_items('pubsub#event', NodeId, Xmlels) ->
    xmlel('pubsub#event', <<"items">>, [xmlattr_node(NodeId)], Xmlels).
%%
xmlel_items('pubsub#event', NodeId, undefined = _ItemId, _Publisher, _Payload) ->
    xmlel('pubsub#event', <<"items">>, [xmlattr_node(NodeId)], []);
xmlel_items('pubsub#event', NodeId, ItemId, Publisher, Payload) ->
    xmlel('pubsub#event', <<"items">>,
        [xmlattr_node(NodeId)],
        [xmlel_item('pubsub#event', ItemId, Publisher, Payload)]).
%%
xmlel_message(undefined = _Type, Xmlels) ->
    xmlel_message('normal', Xmlels);
xmlel_message(Type, Xmlels) ->
    xmlel('jabber:client', <<"message">>,
        [xmlattr_type(Type),
         xmlattr_id(_Id = id())],
        Xmlels).
%%
xmlel_options('pubsub', NodeId, Jid, SubId, Xmlels) ->
    xmlel('pubsub', <<"options">>,
        case SubId of
            undefined ->
                [xmlattr_node(NodeId),
                 xmlattr_jid(Jid)];
            _ ->
                [xmlattr_node(NodeId),
                 xmlattr_jid(Jid),
                 xmlattr_subid(SubId)]
        end,
        Xmlels).
%%
xmlel_publish('pubsub', NodeId, Xmlels) ->
    xmlel('pubsub', <<"publish">>, [xmlattr_node(NodeId)], Xmlels).
%%
xmlel_purge(NodeId) ->
    xmlel('pubsub#event', <<"purge">>, [xmlattr_node(NodeId)], []).

%%
xmlel_query('disco#info', Xmlels) ->
    xmlel('disco#info', <<"query">>, [], Xmlels);
%%
xmlel_query('disco#items', Xmlels) ->
    xmlel('disco#items', <<"query">>, [], Xmlels).

%%
xmlel_query('disco#info', NodeId, Xmlels) ->
    xmlel('disco#info', <<"query">>, [xmlattr_node(NodeId)], Xmlels);
xmlel_query('disco#items', NodeId, Xmlels) ->
    xmlel('disco#items', 'query', [xmlattr_node(NodeId)], Xmlels).

%%
xmlel_redirect(RedirectURI) ->
    xmlel('pubsub#event', <<"redirect">>, [xmlattr_uri(RedirectURI)], []).
%%
xmlel_retract(undefined = _ItemId) ->
    xmlel('pubsub#event', <<"retract">>, [], []);
xmlel_retract(ItemId) ->
    xmlel('pubsub#event', <<"retract">>, [xmlattr_id(ItemId)], []).
%%
xmlel_subscription('pubsub', NodeId, Jid, SubId, Subscription_State) ->
    xmlel('pubsub', <<"subscription">>,
        [xmlattr_node(NodeId),
         xmlattr_jid(Jid),
         xmlattr_subid(SubId),
         xmlattr_subscription(Subscription_State)],
        []);
xmlel_subscription('pubsub#event', NodeId, Jid, SubId, Subscription_State) ->
    xmlel('pubsub#event', <<"subscription">>,
        case SubId of
            undefined ->
                [xmlattr_node(NodeId),
                 xmlattr_jid(Jid),
                 xmlattr_subscription(Subscription_State)];
            _SubId ->
                [xmlattr_node(NodeId),
                 xmlattr_jid(Jid),
                 xmlattr_subid(SubId),
                 xmlattr_subscription(Subscription_State)]
        end,
        []).
%%
xmlel_subscription('pubsub#owner', Jid, SubId, Subscription_State) ->
    xmlel('pubsub#owner', <<"subscription">>,
        case SubId of
            undefined ->
                [xmlattr_jid(Jid),
                 xmlattr_subscription(Subscription_State)];
            _SubId ->
                [xmlattr_jid(Jid),
                 xmlattr_subid(SubId),
                 xmlattr_subscription(Subscription_State)]
        end,
        []).
%%
xmlel_subscriptions('pubsub', Xmlels) ->
    xmlel('pubsub', <<"subscriptions">>, [], Xmlels).

xmlel_subscriptions('pubsub#owner', NodeId, Xmlels) ->
    xmlel('pubsub#owner', <<"subscriptions">>, [xmlattr_node(NodeId)], Xmlels).
%%
xmlel_publish(NodeId, undefined = _ItemId) ->
    xmlel('pubsub', <<"publish">>, [xmlattr_node(NodeId)], []);
xmlel_publish(NodeId, ItemId) ->
    xmlel('pubsub', <<"publish">>,
        [xmlattr_node(NodeId)],
        [xmlel_item('pubsub', ItemId)]).
%%
xmlel_pubsub('pubsub', Children) ->
    xmlel('pubsub', <<"pubsub">>, [], Children);
%%
xmlel_pubsub('pubsub#owner', Children) ->
    xmlel('pubsub#owner', <<"pubsub">>, [], Children).

