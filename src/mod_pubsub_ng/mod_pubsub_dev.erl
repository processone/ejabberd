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
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @author Karim Gemayel <karim.gemayel@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @headerfile "pubsub_dev.hrl"

-module(mod_pubsub_dev).
-author('christophe.romain@process-one.net').
-author('karim.gemayel@process-one.net').
-version('1.13-0').

-behaviour(gen_server).
-behaviour(gen_mod).

-include("pubsub_dev.hrl").
-include("pubsub_api.hrl").

-compile(export_all).
%%% Export Functions

%% API and gen_server callbacks
-export([
  start_link/2,
  start/2,
  stop/1,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%
-export_type([
  resource_show/0,
  presence_cache/0
]).

-type(resource_show()
  :: {Resource :: xmpp_jid:resource_jid(),
      Show     :: 'away' | 'chat' | 'dnd' | 'online' | 'xa' }
).

-type(presence_cache() :: [Resource_Show::resource_show()]).

%%
-export_type([
  resource_subids/0,
  resources_subids/0
]).

-type(resource_subids()
  :: {Resource :: xmpp_jid:resource_jid(),
      SubIds   :: [SubId::exmpp_pubsub:subId(),...]}
).

-type(resources_subids() :: [Resource_SubIds::resource_subids(),...]).

%%%
-export_type([
  entity/0,
  n0de/0,
  cache/0,
  subids/0,
  event/0
]).

-type(entity() :: #entity{}).
%  id            :: xmpp_jid:usr_bare(),
%  affiliation   :: 'member' | 'owner' | 'publisher',
%  subscriptions :: []%[exmpp_pubsub:subscription(),...]
%}).

%%%
-type(n0de() :: #node{}).
%  id                      :: exmpp_pubsub:nodeId(),
%  owners                  :: [Node_Owner::xmpp_jid:usr_bare(),...],
%  access_model            :: pubsub_options:access_model(),
%  itemreply               :: 'owner' | 'publisher',
%  notification_type       :: 'headline' | 'normal',
%  presence_based_delivery :: boolean(),
%  rosters_groups_allowed  :: [] | pubsub_options:rosters_groups_allowed()
%}).

%%%
-type(cache() :: #cache{}).
%  presence               :: undefined | mod_pubsub_dev:presence_cache(),
%  presence_subscriptions :: undefined | boolean(),
%  rosters_groups         :: undefined | boolean()
%}).

%%%
-type(subids() :: #subids{}).
%  presence    :: undefined | [] | mod_pubsub_dev:resources_subids(),
%  no_presence :: undefined | [] | mod_pubsub_dev:resources_subids()
%}).

%%
-type(event() :: #event{
%  host      :: xmpp_jid:raw_jid_component_bare(),
%  component :: xmpp_jid:component_bare(),
%  entity    :: mod_pubsub_dev:entity(),
%  node      :: mod_pubsub_dev:n0de(),
%  cache     :: mod_pubsub_dev:cache(),
%  subids    :: mod_pubsub_dev:subids()
}).

%%%
-export_type([
  item/0,
  published_item/0,
  retracted_item/0
]).

-type(published_item() :: #item{
  access_model            :: undefined | pubsub_options:access_model(),
  presence_based_delivery :: undefined | boolean(),
  rosters_groups_allowed  :: [] | pubsub_options:rosters_groups_allowed(),
  stanza                  :: xmlel()
}).

-type(retracted_item() :: #item{
  access_model            :: undefined | pubsub_options:access_model(),
  presence_based_delivery :: undefined | boolean(),
  rosters_groups_allowed  :: [] | pubsub_options:rosters_groups_allowed(),
  stanza                  :: xmlel()
}).

-type(item() :: published_item() | retracted_item()).


%%

-export_type([
  pubsub_itemId/0,
  pubsub_nodeId/0,
  pubsub_stateId/0,
  item_owners/0,
  node_owners/0,
  item_creation/0,
  item_modification/0,
  node_creation/0
]).

-type(pubsub_itemId()
  :: {ItemId::exmpp_pubsub:itemId(), NodeIdx::exmpp_pubsub:nodeIdx()}
).

-type(pubsub_nodeId()
  :: {Pubsub_Host :: exmpp_pubsub:host(), NodeId :: exmpp_pubsub:nodeId()}
).

-type(pubsub_stateId()
  :: {Entity::xmpp_jid:usr_bare(), NodeIdx::exmpp_pubsub:nodeIdx()}
).

-type(item_owners() :: [Entity::xmpp_jid:usr_bare(),...]).
-type(node_owners() :: [Entity::xmpp_jid:usr_bare(),...]).

-type(item_creation()
  :: {DateTime::erlang:timestamp(), Entity::xmpp_jid:usr_entity()}
).

-type(item_modification()
  :: {DateTime::erlang:timestamp(), Entity::xmpp_jid:usr_entity()}
).

-type(node_creation()
  :: {DateTime::erlang:timestamp(), Entity::xmpp_jid:usr_entity()}
).



%%
-export_type([
  pubsub_state/0,
  pubsub_states/0,
  %
  pubsub_state_owner/0,
  pubsub_state_publisher/0,
  pubsub_state_publish_only/0,
  pubsub_state_member/0,
  pubsub_state_none/0,
  pubsub_state_outcast/0
]).

-type(pubsub_state_owner()
  :: #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'owner',
         access        :: undefined | 'presence' | 'roster',
         %subscriptions :: [exmpp_pubsub:subscription_subscribed()  |
         %                  exmpp_pubsub:subscription_unconfigured()],
         subscriptions :: [exmpp_pubsub:subscription()],
         itemids       :: [] | exmpp_pubsub:itemIds()
     }
).

%%
-type(pubsub_state_publisher()
  :: #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'publisher',
         access        :: undefined | 'presence' | 'roster',
         %subscriptions :: [exmpp_pubsub:subscription_subscribed()  |
         %                  exmpp_pubsub:subscription_unconfigured()],
         subscriptions :: [exmpp_pubsub:subscription()],
         itemids       :: [] | exmpp_pubsub:itemIds()
     }
).

%%
-type(pubsub_state_publish_only()
  :: #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'publish-only',
         access        :: undefined | 'presence' | 'roster',
         subscriptions :: [],
         itemids       :: [] | exmpp_pubsub:itemIds()
     }
).

%%
-type(pubsub_state_member()
  :: #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'member',
         access        :: undefined | 'pending' | 'presence' | 'roster',
         subscriptions :: [],
         itemids       :: [] | exmpp_pubsub:itemIds()
     }
  %%
   | #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'member',
         access        :: undefined | 'pending' | 'presence' | 'roster',
         %subscriptions :: [exmpp_pubsub:subscription_pending(),...],
         subscriptions :: [exmpp_pubsub:subscription()],
         itemids       :: [] | exmpp_pubsub:itemIds()
     }
  %%
   | #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'member',
         subscriptions :: [exmpp_pubsub:subscription()],
         %subscriptions :: [exmpp_pubsub:subscription_pending()     |
         %                  exmpp_pubsub:subscription_subscribed()  |
         %                  exmpp_pubsub:subscription_unconfigured()],
         itemids       :: [] | exmpp_pubsub:itemIds()
     }
).

%%
-type(pubsub_state_none()
  :: #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'none',
         access        :: undefined | 'presence' | 'roster',
         subscriptions :: [],
         itemids       :: []
     }
  %%
   | #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'none',
         access        :: undefined | 'presence' | 'roster',
         subscriptions :: [],
         itemids       :: exmpp_pubsub:itemIds()
     }
  %%
   | #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'none',
         access        :: undefined | 'presence' | 'roster',
         subscriptions :: [],
         itemids       :: exmpp_pubsub:itemIds()
     }
).

%%
-type(pubsub_state_outcast()
  :: #pubsub_state_dev{
         id            :: mod_pubsub_dev:pubsub_stateId(),
         nodeidx       :: exmpp_pubsub:nodeIdx(),
         affiliation   :: 'outcast',
         access        :: undefined | 'presence' | 'roster',
         subscriptions :: [exmpp_pubsub:subscription()],
         %subscriptions :: [exmpp_pubsub:subscription_pending()     |
         %                  exmpp_pubsub:subscription_subscribed()  |
         %                  exmpp_pubsub:subscription_unconfigured()],
         itemids       :: [] | exmpp_pubsub:itemIds()
     }
).


%%
-type(pubsub_state() :: mod_pubsub_dev:pubsub_state_owner()
                      | mod_pubsub_dev:pubsub_state_publisher()
                      | mod_pubsub_dev:pubsub_state_publish_only()
                      | mod_pubsub_dev:pubsub_state_member()
                      | mod_pubsub_dev:pubsub_state_none()
                      | mod_pubsub_dev:pubsub_state_outcast()
).

-type(pubsub_states() :: [Pubsub_State::mod_pubsub_dev:pubsub_state(),...]).


%%
-export_type([
  pubsub_node/0,
  pubsub_nodes/0
]).

-type(pubsub_node()
  :: #pubsub_node_dev{
         id       :: mod_pubsub_dev:pubsub_nodeId(),
         idx      :: exmpp_pubsub:nodeIdx(),
         creation :: mod_pubsub_dev:node_creation(),
         level    :: exmpp_pubsub:level(),
         owners   :: mod_pubsub_dev:node_owners(),
         itemids  :: [] | exmpp_pubsub:itemIds(),
         options  :: pubsub_options:options_node()
    }
).

-type(pubsub_nodes() :: [Pubsub_Node::mod_pubsub_dev:pubsub_node(),...]).


%%
-export_type([
  pubsub_item/0,
  pubsub_items/0
]).

-type(pubsub_item()
  :: #pubsub_item_dev{
         id           :: mod_pubsub_dev:pubsub_itemId(),
         nodeidx      :: exmpp_pubsub:nodeIdx(),
         owners       :: mod_pubsub_dev:item_owners(),
         creation     :: mod_pubsub_dev:item_creation(),
         modification :: mod_pubsub_dev:item_modification(),
         payload      :: exmpp_pubsub:payload(),
         options      :: [] | pubsub_options:options_item()
    }
).

-type(pubsub_items() :: [Pubsub_Item::mod_pubsub_dev:pubsub_item(),...]).

%%
-export_type([
  pubsub_last_item/0
]).

-type(pubsub_last_item()
  :: #pubsub_last_item_dev{
         nodeidx  :: exmpp_pubsub:nodeIdx(),
         id       :: exmpp_pubsub:itemId(),
         owners   :: mod_pubsub_dev:item_owners(),
         creation :: mod_pubsub_dev:item_creation(),
         payload  :: exmpp_pubsub:payload(),
         options  :: [] | pubsub_options:options_item()
     }
).

%%
-export_type([
  pubsub_subscription_pending/0
]).

-type(pubsub_subscription_pending()
  :: #pubsub_subscription_pending{
         id      :: {Entity::xmpp_jid:usr_bare(), NodeIdx::exmpp_pubsub:nodeIdx()},
         nodeidx :: exmpp_pubsub:nodeIdx(),
         subids  :: [SubId::exmpp_pubsub:subId(),...]
     }
).





-define(PROCNAME, ejabberd_mod_pubsub_dev).







-record(state,
{
  host           :: xmpp_jid:raw_jid_component_bare(),
  pubsub_host    :: xmpp_jid:raw_jid_component_bare(),
  pubsub         :: #capabilities{},
  pubsub_on_jid  :: #capabilities{}
}).

-type(state()
  :: #state{
         host           :: xmpp_jid:raw_jid_component_bare(),
         pubsub_host    :: xmpp_jid:raw_jid_component_bare(),
         pubsub         :: #capabilities{},
         pubsub_on_jid  :: #capabilities{}
     }
).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

%% @spec start_link(Host, Options) -> {'ok', pid()} | 'ignore' | {'error', atom()}
%%     Host = string()
%%     Options = {Option::atom(), Value::term()}
-spec(start_link/2 ::
(
  Host    :: string(),
  Options :: [Option::{Key::atom(), Value::term()}])
    -> {'ok', pid()} | 'ignore' | {'error', _}
).

start_link(Host, Options) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Options], []).

%% @spec start(Host, Options) -> {'error',atom()} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),any()}
%%     Host = string()
%%     Options = {Option::atom(), Value::term()}
-spec(start/2 ::
(
  Host    :: string(),
  Options :: [Option::{Key::atom(), Value::term()}])
    -> {'error',_} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}
).

start(Host, Options) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc,
		 {?MODULE, start_link, [Host, Options]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

%% @spec stop(Host) -> 'ok' | {'error','not_found' | 'running' | 'simple_one_for_one'}
%%     Host = string()
-spec(stop/1 ::
(
  Host :: string())
    -> 'ok' | {'error','not_found' | 'running' | 'simple_one_for_one'}
).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).




%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec(init/1 ::
(
  Args :: [binary() | [{Option::atom(), Value::term()}]])
    -> {'ok', State::state()}
).

init([Host, Options] = _Args) ->
    ?DEBUG("pubsub init ~p ~p",[Host, Options]),
    Pubsub_Host = gen_mod:expand_host_name(Host, Options, <<"pubsub">>),
    IQDisc = gen_mod:get_opt('iqdisc', Options, 'one_queue'),

   % mod_disco:register_feature(Server_HostB, ?NS_PUBSUB_s),
   %%
   %% Pubsub Hooks
   %%   - Service disco hooks
   %%     * disco_local_identity
   %%     * disco_local_features
   %%     * disco_local_items
   %%     * disco_local_forms
   %%   - Connectivity hooks
   %%     * presence_probe_hook
   %%     * sm_remove_connection_hook
   %%   - Roster hooks
   %%     * roster_in_subscription
   %%     * roster_out_subscription
   %%   - User managment hooks
   %%     * remove_user
   %%     * anonymous_purge_hook
   %%
   %% Pubsub On Jid hooks
   %%   - Service disco hooks
   %%     * disco_sm_identity
   %%     * disco_sm_features
   %%     * disco_sm_items
   %%     * disco_sm_forms
   %%   - Connectitivy
   %%     * caps_update


    Pubsub_Features = node_flat_dev:pubsub_features(),
   % mod_disco:register_feature(Server_HostB, ?NS_PUBSUB_s),
    pubsub_db:init('mnesia', 'dev'),
    %% Register hooks according to available Pubsub Features
    register_hooks(Host, Pubsub_Features, pubsub_hooks),
    %%
    ejabberd_router:register_route(Pubsub_Host),
    {ok,
     #state{
         host        = Host,
         pubsub_host = Pubsub_Host,
         pubsub      = node_flat_dev:capabilities()
     }
    }.

%%--------------------------------------------------------------------
%% Function:
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @private

handle_info({route, From, #jid{resource = undefined} = To, Stanza},
  #state{host = Host, pubsub = Capabilities} = State) ->
    do_route(Host, To, From, Stanza, Capabilities),
    {noreply, State};
%%
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%% @private
%% @todo : -spec
-spec(terminate/2 ::
(
  Reason :: _,
  State  :: state())
    -> 'ok'
).

terminate(_Reason, #state{host = Host, pubsub_host = Pubsub_Host}) ->
    ejabberd_router:unregister_route(Pubsub_Host),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
-define(Is_Stanza_IQ(Stanza),
(
  Stanza#xmlel.name == <<"iq">>
)).

-define(Is_Stanza_Message(Stanza),
(
  Stanza#xmlel.name == <<"message">>
)).

-define(Is_Stanza_Presence(Stanza),
(
  Stanza#xmlel.name == <<"presence">>
)).

-define(Is_IQ_Ignore(IQ),
(
  IQ#iq.type == 'result'
    orelse
  IQ#iq.type == 'error'
)).

-define(Is_IQ_Pubsub(IQ),
(
  (IQ#iq.type == 'set'
       orelse
   IQ#iq.type == 'get')
           andalso
  (IQ#iq.xmlns == ?NS_PUBSUB
       orelse
   IQ#iq.xmlns == ?NS_PUBSUB_OWNER)
          andalso
  IQ#iq.sub_el#xmlel.name == <<"pubsub">>
)).

-define(Is_IQ_Get_Disco_Info(IQ),
(
  IQ#iq.type                   == 'get'
    andalso
  IQ#iq.xmlns                  == ?NS_DISCO_INFO
    andalso
  IQ#iq.sub_el#xmlel.name      == <<"query">>
%    andalso
%  IQ#iq.payload#xmlel.children == []
)).

-define(Is_IQ_Get_Disco_Items(IQ),
(
  IQ#iq.type                   == 'get'
    andalso
  IQ#iq.xmlns                  == ?NS_DISCO_ITEMS
    andalso
  IQ#iq.sub_el#xmlel.name      == <<"query">>
%    andalso
%  IQ#iq.payload#xmlel.children == []
)).

%%
do_route(Host,
  #jid{lserver = Pubsub_Host} = Pubsub_Component,
  #jid{luser = U, lserver = S, lresource = R} = Entity, Stanza_IQ,
  #capabilities{privacy = Privacy, plugin = Plugin} = Capabilities)
  when ?Is_Stanza_IQ(Stanza_IQ) ->
    case IQ = jlib:iq_query_or_response_info(Stanza_IQ) of
        IQ when ?Is_IQ_Ignore(IQ) ->
            ok;
        IQ when ?Is_IQ_Get_Disco_Info(IQ) ->
            case
                pubsub_disco:iq_disco_info('dev', Host, Pubsub_Host,
                    Privacy, {U,S,undefined},
                    case S of
                        Host -> 'local';
                        _    -> 'remote'
                    end,
                    Plugin,
                    case xml:get_tag_attr_s(<<"node">>, IQ#iq.sub_el) of
                        <<>>   -> undefined;
                        NodeId -> NodeId
                    end,
                    xml:remove_cdata(IQ#iq.sub_el#xmlel.children))
            of
                {result, Xmlel_Query} ->
                    ejabberd_router:route(Pubsub_Component, Entity,
                        jlib:iq_to_xml(IQ#iq{
                            type   = result,
                            sub_el = [Xmlel_Query]
                        }));
                {error, Error} ->
                    ejabberd_router:route(Pubsub_Component, Entity,
                        jlib:make_error_reply(Stanza_IQ, Error))
            end;
        IQ when ?Is_IQ_Get_Disco_Items(IQ) ->
            case
                pubsub_disco:iq_disco_items('dev', Host, Pubsub_Host,
                    Privacy, {U,S,undefined},
                    case S of
                        Host -> 'local';
                        _    -> 'remote'
                    end,
                    Plugin,
                    case xml:get_tag_attr_s(<<"node">>, IQ#iq.sub_el) of
                        <<>>   -> undefined;
                        NodeId -> NodeId
                    end,
                    xml:remove_cdata(IQ#iq.sub_el#xmlel.children))
            of
                {result, Xmlel_Query} ->
                    ejabberd_router:route(Pubsub_Component, Entity,
                        jlib:iq_to_xml(IQ#iq{
                            type   = result,
                            sub_el = [Xmlel_Query]
                        }));
                {error, Error} ->
                    ejabberd_router:route(Pubsub_Component, Entity,
                        jlib:make_error_reply(Stanza_IQ, Error))
            end;
        IQ when ?Is_IQ_Pubsub(IQ) ->
            case
                iq_pubsub(Host, Pubsub_Host, {U,S,R}, IQ#iq.type,
                    _Rsm = lists:member(?NS_RSM, Plugin:features()),
                    xml:remove_cdata(IQ#iq.sub_el#xmlel.children),
                    IQ#iq.lang, Capabilities)
            of
                {result, []} ->
                    ejabberd_router:route(Pubsub_Component, Entity,
                        jlib:iq_to_xml(IQ#iq{
                            type   = result,
                            sub_el = []
                        }));
                {result, Xmlels} ->
                    ejabberd_router:route(Pubsub_Component, Entity,
                        jlib:iq_to_xml(IQ#iq{
                            type   = result,
                            sub_el = Xmlels
                        }));
                {error, Error} ->
                    ejabberd_router:route(Pubsub_Component, Entity,
                        jlib:make_error_reply(Stanza_IQ, Error))
            end
    end;
do_route(Host,
  #jid{lserver = Pubsub_Host} = Pubsub_Component,
  #jid{luser = U, lserver = S, lresource = R} = Entity, Stanza_Message,
  Capabilities)
  when ?Is_Stanza_Message(Stanza_Message) ->
    ok;
do_route(Host,
  #jid{lserver = Pubsub_Host} = Pubsub_Component,
  #jid{luser = U, lserver = S, lresource = R} = Entity, Stanza_Presence,
  Capabilities)
  when ?Is_Stanza_Presence(Stanza_Presence) ->
    ok.


iq_pubsub(Host, Pubsub_Host, Entity, IQ_Type, Rsm, Xmlels, _Lang,
  #capabilities{api = #api{parser = Parser, core = API_Core}} = Capabilities) ->
    case Parser:parse(IQ_Type, Xmlels, Rsm, API_Core) of
        {result, Module, Function, Parameters} ->
            Module:Function(Host, Pubsub_Host, Entity, Parameters, Capabilities);
        Error ->
            Error
    end.



register_hooks(Host, Pubsub_Features, Module) ->
    register_hooks('roster', Host, Pubsub_Features, Module),
    register_hooks('presence', Host, Pubsub_Features, Module).

    %ejabberd_hooks:add(roster_in_subscription, Host, pubsub_hooks, roster_in_subscription, 50),
    %ejabberd_hooks:add(roster_out_subscription, Host, ?MODULE, roster_out_subscription, 50),
    %ejabberd_hooks:add(roster_get, Host, ?MODULE, roster_get, 50),
    %ejabberd_hooks:add(roster_get_jid_info, Host, ?MODULE, roster_get_jid_info, 50),
    %ejabberd_hooks:add(roster_get_subscription_lists, Host, ?MODULE, roster_get_subscription_lists, 50),
    %%ejabberd_hooks:add(roster_process_item, Host, pubsub_hooks, roster_process_item, 50).


register_hooks('roster', Host, Pubsub_Features, Module) ->
    case lists:member("access-roster", Pubsub_Features) of
        true ->
            ejabberd_hooks:add(roster_process_item, Host, Module,
                roster_process_item, 50),
            ok;
        false ->
            ok
    end;
%%
register_hooks('presence', Host, Pubsub_Features, Module) ->
    case
        lists:member(<<"access-presence">>, Pubsub_Features)
            orelse
        lists:member(<<"last-published">>, Pubsub_Features)
            orelse
        lists:member(<<"leased-subscription">>, Pubsub_Features)
            orelse
        lists:member(<<"presence-notifications">>, Pubsub_Features)
    of
        true ->
            ejabberd_hooks:add(presence_probe_hook, Host, Module,
                presence_online, 80),
            ejabberd_hooks:add(sm_remove_connection_hook, Host, Module,
                presence_offline, 80),
            ok;
        false ->
            ok
    end.


roster_get(Acc, {User, Server}) ->
    ?INFO_MSG("ROSTER_GET Acc ~p ~nUser ~p ~nServer ~p ~n",
        [Acc, User, Server]).

roster_get_jid_info(Acc, User, Server, JID) ->
    ?INFO_MSG("ROSTER_GET_JID_INFO Acc ~p ~nUser ~p ~nServer ~p ~nJID ~p ~n",
        [Acc, User, Server, JID]).

roster_get_subscription_lists(Acc, User, Server) ->
    ?INFO_MSG("ROSTER_GET_SUBSCRIPTION_LISTS Acc ~p ~nUser ~p ~nServer ~p ~n",
        [Acc, User, Server]).

roster_in_subscription(User, Server, JID, SubscriptionType, Reason, A) ->
    ?INFO_MSG("ROSTER_IN_SUBSCRITION User  ~p ~n Server ~p ~n JID ~p ~n SubscriptionType ~p ~n, Reason ~p ~nA ~p ~n",
        [User, Server, JID, SubscriptionType, Reason, A]).

roster_out_subscription(User, Server, JID, SubscriptionType) ->
    ?INFO_MSG("ROSTER_OUT_SUBSCRITION User  ~p ~n Server ~p ~n JID ~p ~n SubscriptionType ~p ~n",
        [User, Server, JID, SubscriptionType]).

roster_process_item(RosterItem, Server) ->
        ?INFO_MSG("ROSTER_PROCESS_ITEM RosterItem ~p ~nServer ~p ~n",
        [RosterItem, Server]),
        RosterItem.

