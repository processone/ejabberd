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

-module(pubsub_parser).
-author('karim.gemayel@process-one.net').

-compile({no_auto_import,[node/1]}).

-compile(export_all).

-include("pubsub_dev.hrl").
-include("pubsub_api.hrl").

-import(xml,
[
  get_tag_attr_s/2,
  get_attr_s/2,
  remove_cdata/1
]).


%-type('xmlel_affiliations#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'affiliations', children::[]}
%).

%-type('xmlel_affiliations#owner'() ::
%  #xmlel{ns::?NS_PUBSUB_OWNER, name::'affiliations', children::[]}
%).

%-type('xmlel_configure#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'configure'}
%).

%-type('xmlel_configure#owner'() ::
%  #xmlel{ns::?NS_PUBSUB_OWNER, name::'configure', children::[]}
%).

%-type('xmlel_create#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'create', children::[]}
%).

%-type('xmlel_default#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'default'}
%).

%-type('xmlel_default#owner'() ::
%%  #xmlel{ns::?NS_PUBSUB_OWNER, name::'default', attrs::[], children::[]}
%  #xmlel{ns::?NS_PUBSUB_OWNER, name::'default', children::[]}
%).

%-type('xmlel_delete#owner'() ::
%  #xmlel{ns::?NS_PUBSUB_OWNER, name::'delete'}
%).

%-type('xmlel_items#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'items'}
%).

%-type('xmlel_options1#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'options', attrs::[]}
%).

%-type('xmlel_options2#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'options'}
%).

%-type('xmlel_publish#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'publish'}
%).

%-type('xmlel_publish-options#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'publish-options'}
%).

%-type('xmlel_purge#owner'() ::
%  #xmlel{ns::?NS_PUBSUB_OWNER, name::'purge', children::[]}
%).

%-type('xmlel_retract#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'retract'}
%).

%-type('xmlel_subscribe#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'subscribe', children::[]}
%).

%-type('xmlel_subscriptions#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'subscriptions', children::[]}
%).

%-type('xmlel_subscriptions#owner'() ::
%  #xmlel{ns::?NS_PUBSUB_OWNER, name::'subscriptions', children::[]}
%).

%-type('xmlel_unsubscribe#'() ::
%  #xmlel{ns::?NS_PUBSUB, name::'unsubscribe', children::[]}
%).


-define(Is_Xmlel_Affiliations(Xmlel),
(
  Xmlel#xmlel.name     == <<"affiliations">>    andalso
  Xmlel#xmlel.children == []
)).

-define(Is_Xmlel_Configure(Xmlel),
(
  Xmlel#xmlel.name     == <<"configure">>       andalso
  Xmlel#xmlel.attrs    == []
)).

-define(Is_Xmlel_Create(Xmlel),
(
  Xmlel#xmlel.name     == <<"create">>          andalso
  Xmlel#xmlel.children == []
)).

-define(Is_Xmlel_Default(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"default">>         andalso
  Xmlel#xmlel.children == []
)).

-define(Is_Xmlel_Delete(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB_OWNER  andalso
  Xmlel#xmlel.name     == <<"delete">>
)).

-define(Is_Xmlel_Item(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"item">>            andalso
  Xmlel#xmlel.children == []
)).

-define(Is_Xmlel_Items1(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"items">>           andalso
  Xmlel#xmlel.children == []
)).

-define(Is_Xmlel_Items2(Xmlel),
(
%Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"items">>
)).

-define(Is_Xmlel_Options1(Xmlel),
(
%Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"options">>         andalso
  Xmlel#xmlel.attrs    == []
)).

-define(Is_Xmlel_Options2(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"options">>
)).

-define(Is_Xmlel_Publish(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"publish">>
)).

-define(Is_Xmlel_Publish_Options(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"publish-options">> andalso
  Xmlel#xmlel.attrs    == []
)).

-define(Is_Xmlel_Purge(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB_OWNER  andalso
  Xmlel#xmlel.name     == <<"purge">>           andalso
  Xmlel#xmlel.children == []
)).

-define(Is_Xmlel_Redirect(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB_OWNER  andalso
  Xmlel#xmlel.name     == <<"redirect">>        andalso
  Xmlel#xmlel.children == []
)).

-define(Is_Xmlel_Retract(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"retract">>         andalso
  Xmlel#xmlel.children =/= []
)).

-define(Is_Xmlel_Subscribe(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"subscribe">>       andalso
  Xmlel#xmlel.children == []
)).

-define(Is_Xmlel_Subscriptions(Xmlel),
(
  %Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"subscriptions">>   andalso
  Xmlel#xmlel.children == []
)).

-define(Is_Xmlel_Unsubscribe(Xmlel),
(
%Xmlel#xmlel.ns       == ?NS_PUBSUB        andalso
  Xmlel#xmlel.name     == <<"unsubscribe">>     andalso
  Xmlel#xmlel.children == []
)).


%%
-spec(parse/4 ::
(
  IQ_Type  :: 'get' | 'set',
  Xmlels   :: %-- Create_Node --%
%              ['xmlel_create#'(),...]             |
%           %%
%              ['xmlel_create#'()
%              |'xmlel_configure#'(),...]          |
%           %% %-- Delete_Node --%
%              ['xmlel_delete#owner'(),...]        |
%           %% %-- Purge_Node --%
%              ['xmlel_purge#owner'(),...]         |
%           %% %-- Publish_Item --%
%              ['xmlel_publish#'(),...]            |
%           %%
%              ['xmlel_publish#'()
%              |'xmlel_configure#'(),...]          |
%           %%
%              ['xmlel_publish#'()
%              |'xmlel_publish-options#'(),...]    |
%           %%
%              ['xmlel_publish#'()
%              |'xmlel_configure#'()
%              |'xmlel_publish-options#'(),...]    |
%           %% %-- Retract_Item --%
%              ['xmlel_retract#'(),...]            |
%           %% %-- Subscribe_Node --%
%              ['xmlel_subscribe#'(),...]          |
%              ['xmlel_subscribe#'()
%              |'xmlel_options1#'(),...]           |
%           %% %-- Unsubscribe_Node --%
%              ['xmlel_unsubscribe#'(),...]        |
%           %% %-- Set_Configure_Subscription --%
%              ['xmlel_options2#'(),...]           |
%           %% %-- Get_Items --%
%              ['xmlel_items#'(),...]              |
%           %% %-- Get_Entity_Affiliations --%
%              ['xmlel_affiliations#'(),...]       |
%           %% %-- Get_Entity_Subscriptions --%
%              ['xmlel_subscriptions#'(),...]      |
%           %% %-- Get_Node_Affiliations --%
%              ['xmlel_affiliations#owner'(),...]  |
%           %% %-- Get_Node_Subscriptions --%
%              ['xmlel_subscriptions#owner'(),...] |
%           %% %-- Get_Configure_Subscription_Default --%
%              ['xmlel_default#'(),...]            |
%           %% %-- Get_Configure_Subscription --%
%              ['xmlel_options2#'(),...]           |
%           %% %-- Get_Configure_Node_Default --%
%              ['xmlel_default#owner'(),...]       |
%           %% %-- Get_Configure_Node --%
%              ['xmlel_configure#owner'(),...]     |
           %% %-- Other Stanza --%
              [xmlel()],
           %%
  Rsm      :: boolean(),
  API_Core :: #api_core{})
    -> %-- Create_Node --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'create_node',
        Parameters :: {
            NodeId      :: undefined | exmpp_pubsub:nodeId(),
            Node_Config :: [Xmlel::xmlel()]
        }
       }
     | %-- Delete_Node --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'delete_node',
        Parameters :: {
            NodeId      :: undefined | exmpp_pubsub:nodeId(),
            RedirectURI :: undefined | binary()
        }
       }
     | %-- Purge_Node --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'purge_node',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId()
        }
       }
     | %-- Publish_Item --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'publish_item',
        Parameters :: {
            NodeId          :: undefined | exmpp_pubsub:nodeId(),
            Item            :: 0,
            ItemId          :: undefined,
            Payload         :: exmpp_pubsub:payload_empty(),
            Node_Config     :: [Xmlel::xmlel()],
            Publish_Options :: [Xmlel::xmlel()]
        }
       }
     |
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'publish_item',
        Parameters :: {
            NodeId          :: undefined | exmpp_pubsub:nodeId(),
            Item            :: 1,
            ItemId          :: undefined | exmpp_pubsub:itemId(),
            Payload         :: exmpp_pubsub:payload() | [Xmlel::xmlel(),...],
            Node_Config     :: [Xmlel::xmlel()],
            Publish_Options :: [Xmlel::xmlel()]
        }
       }
     | %-- Retract_Item --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'retract_item',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId(),
            ItemId :: undefined | exmpp_pubsub:itemId(),
            Notify :: undefined | boolean()
        }
       }
     | %-- Subscribe_Node --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'subscribe_node',
        Parameters :: {
            NodeId            :: undefined | exmpp_pubsub:nodeId(),
            Jid               :: binary() | undefined,
            Subscribe_Options :: [Xmlel::xmlel()]
        }
       }
     | %-- Unsubscribe_Node --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'unsubscribe_node',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId(),
            Jid    :: binary() | undefined,
            SubId  :: undefined | exmpp_pubsub:subId()
        }
       }
     | %-- Set_Configure_Subscription --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'set_configure_subscription',
        Parameters :: {
            NodeId            :: undefined | exmpp_pubsub:nodeId(),
            Jid               :: binary() | undefined,
            SubId             :: undefined | exmpp_pubsub:subId(),
            Subscribe_Options :: [Xmlel::xmlel()]
        }
       }
     | %-- Get_Items --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_items',
        Parameters :: {
            NodeId    :: undefined | exmpp_pubsub:nodeId(),
            SubId     :: undefined | exmpp_pubsub:subId(),
            Max_Items :: non_neg_integer(),
            ItemIds   :: undefined
        }
       }
     |
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_items',
        Parameters :: {
            NodeId    :: undefined | exmpp_pubsub:nodeId(),
            SubId     :: undefined | exmpp_pubsub:subId(),
            Max_Items :: undefined,
            ItemIds   :: [] | exmpp_pubsub:itemIds()
        }
       }
     | %-- Get_Entity_Affiliations --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_entity_affiliations',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId()
        }
       }
     | %-- Get_Entity_Subscriptions --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_entity_subscriptions',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId()
        }
       }
     | %-- Get_Node_Affiliations --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_node_affiliations',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId()
        }
       }
     | %-- Get_Node_Subscriptions --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_node_subscriptions',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId()
        }
       }
     | %-- Get_Configure_Subscription_Default --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_configure_subscription_default',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId()
        }
       }
     | %-- Get_Configure_Subscription --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_configure_subscription',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId(),
            Jid    :: binary() | undefined,
            SubId  :: undefined | exmpp_pubsub:subId()
        }
       }
     | %-- Get_Configure_Node_Default --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_configure_node_default',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId()
        }
       }
     | %-- Get_Configure_Node --%
       {result,
        Module     :: 'pubsub_core' | module(),
        Function   :: 'get_configure_node',
        Parameters :: {
            NodeId :: undefined | exmpp_pubsub:nodeId()
        }
       }
    %%%
     | {error, 'invalid-payload'}
).

%-- Create_Node --%

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <create node='princely_musings'/>
%%  </pubsub>
%% </iq>

parse('set' = _IQ_Type, [Xmlel_Create], _Rsm, API_Core)
  when ?Is_Xmlel_Create(Xmlel_Create) ->
    case xmlns(Xmlel_Create) of
        ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.create_node,
             _Function = 'create_node',
             _Parameters = {
                 _NodeId      = node(Xmlel_Create),
                 _Node_Config = []
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <create node='princely_musings'/>
%%   <configure>
%%    <x xmlns='jabber:x:data' type='submit'>
%%     <field var='FORM_TYPE' type='hidden'>
%%      <value>http://jabber.org/protocol/pubsub#node_config</value>
%%     </field>
%%     <field var='pubsub#title'><value>Princely Musings (Atom)</value></field>
%%    </x>
%%   </configure>
%%  </pubsub>
%% </iq>

parse('set' = _IQ_Type, [Xmlel_Create, Xmlel_Configure], _Rsm, API_Core)
  when    ?Is_Xmlel_Create(Xmlel_Create)
  andalso ?Is_Xmlel_Configure(Xmlel_Configure) ->
    case {xmlns(Xmlel_Create), xmlns(Xmlel_Configure)} of
        {NS, NS} when NS == ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.create_node,
             _Function = 'create_node',
             _Parameters = {
                 _NodeId      = node(Xmlel_Create),
                 _Node_Config = remove_cdata(Xmlel_Configure#xmlel.children)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Delete_Node --%

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub#owner'>
%%   <delete node='princely_musings'/>
%%  </pubsub>
%% </iq>
%%
%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub#owner'>
%%   <delete node='princely_musings'>
%%    <redirect uri='xmpp:hamlet@denmark.lit?;node=blog'/>
%%   </delete>
%%  </pubsub>
%% </iq>

parse('set' = _IQ_Type, [Xmlel_Delete], _Rsm, API_Core)
  when ?Is_Xmlel_Delete(Xmlel_Delete) ->
    case {xmlns(Xmlel_Delete), remove_cdata(Xmlel_Delete#xmlel.children)} of
        {?NS_PUBSUB_OWNER, []} ->
            {result,
             _Module = API_Core#api_core.delete_node,
             _Function = 'delete_node',
             _Parameters = {
                 _NodeId      = node(Xmlel_Delete),
                 _RedirectURI = undefined
             }
            };
        {?NS_PUBSUB_OWNER, [Xmlel_Redirect]}
          when ?Is_Xmlel_Redirect(Xmlel_Redirect) ->
            case xmlns(Xmlel_Redirect) of
                ?NS_PUBSUB_OWNER ->
                    {result,
                     _Module = API_Core#api_core.delete_node,
                     _Function = 'delete_node',
                     _Parameters = {
                         _NodeId      = node(Xmlel_Delete),
                         _RedirectURI = uri(Xmlel_Redirect)
                     }
                    };
                _ ->
                    {error, 'invalid-payload'}
            end;
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Purge_Node --%

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub#owner'>
%%   <purge node='princely_musings'/>
%%  </pubsub>
%% </iq>

parse('set' = _IQ_Type, [Xmlel_Purge], _Rsm, API_Core)
  when ?Is_Xmlel_Purge(Xmlel_Purge) ->
    case xmlns(Xmlel_Purge) of
        ?NS_PUBSUB_OWNER ->
            {result,
             _Module = API_Core#api_core.purge_node,
             _Function = 'purge_node',
             _Parameters = {
                 _NodeId = node(Xmlel_Purge)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Publish_Item --%

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <publish node='princely_musings'/>
%%   <configure>
%%    <x xmlns='jabber:x:data' type='submit'>
%%     <field var='FORM_TYPE' type='hidden'>
%%      <value>http://jabber.org/protocol/pubsub#node_config</value>
%%     </field>
%%     <field var='pubsub#title'><value>Princely Musings (Atom)</value></field>
%%    </x>
%%   </configure>
%%   <publish-options>
%%    <x xmlns='jabber:x:data' type='submit'>
%%     <field var='FORM_TYPE' type='hidden'>
%%      <value>http://jabber.org/protocol/pubsub#publish-options</value>
%%     </field>
%%     <field var='pubsub#access_model'><value>presence</value></field>
%%    </x>
%%   </publish-options>
%%  </pubsub>
%% </iq>

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <publish node='princely_musings'>
%%    <item id='bnd81g37d61f49fgn581'/>
%%   </publish>
%%   <configure>
%%    <x xmlns='jabber:x:data' type='submit'>
%%     <field var='FORM_TYPE' type='hidden'>
%%      <value>http://jabber.org/protocol/pubsub#node_config</value>
%%     </field>
%%     <field var='pubsub#title'><value>Princely Musings (Atom)</value></field>
%%    </x>
%%   </configure>
%%   <publish-options>
%%    <x xmlns='jabber:x:data' type='submit'>
%%     <field var='FORM_TYPE' type='hidden'>
%%      <value>http://jabber.org/protocol/pubsub#publish-options</value>
%%     </field>
%%     <field var='pubsub#access_model'><value>presence</value></field>
%%    </x>
%%   </publish-options>
%%  </pubsub>
%% </iq>

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <publish node='princely_musings'>
%%    <item id='bnd81g37d61f49fgn581'>
%%     <entry xmlns='http://www.w3.org/2005/Atom'>
%%      <title>Soliloquy</title>
%%     </entry>
%%    </item>
%%   </publish>
%%   <publish-options>
%%    <x xmlns='jabber:x:data' type='submit'>
%%     <field var='FORM_TYPE' type='hidden'>
%%      <value>http://jabber.org/protocol/pubsub#publish-options</value>
%%     </field>
%%     <field var='pubsub#access_model'><value>presence</value></field>
%%    </x>
%%   </publish-options>
%%  </pubsub>
%% </iq>

%%
parse('set' = _IQ_Type, [Xmlel_Publish], _Rsm, API_Core)
  when ?Is_Xmlel_Publish(Xmlel_Publish) ->
    case {xmlns(Xmlel_Publish), remove_cdata(Xmlel_Publish#xmlel.children)} of
        %%
        {?NS_PUBSUB, []} ->
            {result,
             _Module = API_Core#api_core.publish_item,
             _Function = 'publish_item',
             _Parameters = {
                 _NodeId = node(Xmlel_Publish),
                 _Item = 0,
                 _ItemId = undefined,
                 _Payload = [],
                 _Node_Config = [],
                 _Publish_Options = []
             }
            };
        %%
%        [#xmlel{ns = ?NS_PUBSUB, name = 'item'} = Xmlel_Item] ->
        {?NS_PUBSUB, [#xmlel{name = <<"item">>} = Xmlel_Item]} ->
            {result,
             _Module = API_Core#api_core.publish_item,
             _Function = 'publish_item',
             _Parameters = {
                 _NodeId = node(Xmlel_Publish),
                 _Item = 1,
                 _ItemId = id(Xmlel_Item),
                 _Payload = remove_cdata(Xmlel_Item#xmlel.children),
                 _Node_Config = [],
                 _Publish_Options = []
             }
            };
        %%
        _ ->
            {error, 'invalid-payload'}
    end;
%%
parse('set' = _IQ_Type, [Xmlel_Publish, Xmlel_Configure], _Rsm, API_Core)
  when    ?Is_Xmlel_Publish(Xmlel_Publish)
  andalso ?Is_Xmlel_Configure(Xmlel_Configure) ->
    case {xmlns(Xmlel_Publish), xmlns(Xmlel_Configure)} of
        {NS, NS} when NS == ?NS_PUBSUB ->
            case parse('set', [Xmlel_Publish], _Rsm, API_Core) of
                {result, Module, Function, {NodeId, Item, ItemId, Payload, _, _}} ->
                    {result,
                     Module,
                     Function,
                     _Parameters = {
                         NodeId,
                         Item,
                         ItemId,
                         Payload,
                         _Node_Config = remove_cdata(Xmlel_Configure#xmlel.children),
                         _Publish_Options = []
                     }
                    };
                Error ->
                    Error
            end;
        _ ->
            {error, 'invalid-paylaod'}
    end;
%%
parse('set' = _IQ_Type, [Xmlel_Publish, Xmlel_Publish_Options], _Rsm, API_Core)
  when    ?Is_Xmlel_Publish(Xmlel_Publish)
  andalso ?Is_Xmlel_Publish_Options(Xmlel_Publish_Options) ->
    case {xmlns(Xmlel_Publish), xmlns(Xmlel_Publish_Options)} of
        {NS, NS} when NS == ?NS_PUBSUB ->
            case parse('set', [Xmlel_Publish], _Rsm, API_Core) of
                {result, Module, Function, {NodeId, Item, ItemId, Payload, _, _}} ->
                    {result,
                     Module,
                     Function,
                     _Parameters = {
                         NodeId,
                         Item,
                         ItemId,
                         Payload,
                         _Node_Config = [],
                         _Publish_Options = remove_cdata(
                             Xmlel_Publish_Options#xmlel.children)
                     }
                    };
                Error ->
                    Error
            end;
        _ ->
            {error, 'invalid-payload'}
    end;

%%
parse('set' = _IQ_Type, [Xmlel_Publish, Xmlel_Configure, Xmlel_Publish_Options],
  _Rsm, API_Core)
  when    ?Is_Xmlel_Publish(Xmlel_Publish)
  andalso ?Is_Xmlel_Configure(Xmlel_Configure)
  andalso ?Is_Xmlel_Publish_Options(Xmlel_Publish_Options) ->
    case {xmlns(Xmlel_Publish), xmlns(Xmlel_Configure), xmlns(Xmlel_Publish_Options)} of
        {NS, NS, NS} when NS == ?NS_PUBSUB ->
            case parse('set', [Xmlel_Publish, Xmlel_Configure], _Rsm, API_Core) of
                {result, Module, Function,
                 {NodeId, Item, ItemId, Payload, Node_Config, _}} ->
                    {result,
                     Module,
                     Function,
                     _Parameters = {
                         NodeId,
                         Item,
                         ItemId,
                         Payload,
                         Node_Config,
                         _Publish_Options = remove_cdata(
                             Xmlel_Publish_Options#xmlel.children)
                     }
                    };
                Error ->
                    Error
            end;
        _ ->
            {error, 'invalid-payload'}
    end;
%%
%%
parse('set' = _IQ_Type, [Xmlel_Publish, Xmlel_Publish_Options, Xmlel_Configure],
  Rsm, API_Core)
  when    ?Is_Xmlel_Publish(Xmlel_Publish)
  andalso ?Is_Xmlel_Publish_Options(Xmlel_Publish_Options)
  andalso ?Is_Xmlel_Configure(Xmlel_Configure) ->
    case {xmlns(Xmlel_Publish), xmlns(Xmlel_Publish_Options), xmlns(Xmlel_Configure)} of
        {NS, NS, NS} when NS == ?NS_PUBSUB ->
            parse('set', [Xmlel_Publish, Xmlel_Configure, Xmlel_Publish_Options],
                Rsm, API_Core);
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Get_Entity_Affiliations --%

%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <affiliations/>
%%  </pubsub>
%% </iq>
%%
%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <affiliations node='princely_musings'/>
%%  </pubsub>
%% </iq>

parse('get' = _IQ_Type, [Xmlel_Affiliations], _Rsm, API_Core)
  when ?Is_Xmlel_Affiliations(Xmlel_Affiliations) ->
    case xmlns(Xmlel_Affiliations) of
        ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.get_entity_affiliations,
             _Function = 'get_entity_affiliations',
             _Parameters = {
                 _NodeId = node(Xmlel_Affiliations)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%% TODO : RSM

%-- Get_Entity_Subscriptions --%

%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <subscriptions/>
%%  </pubsub>
%% </iq>
%%
%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <subscriptions node='princely_musings'/>
%%  </pubsub>
%% </iq>

parse('get' = _IQ_Type, [Xmlel_Subscriptions], _Rsm, API_Core)
  when ?Is_Xmlel_Subscriptions(Xmlel_Subscriptions) ->
    case xmlns(Xmlel_Subscriptions) of
        ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.get_entity_subscriptions,
             _Function = 'get_entity_subscriptions',
             _Parameters = {
                 _NodeId = node(Xmlel_Subscriptions)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%% TODO : RSM

%-- Get_Node_Affiliations --%

%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub#owner'>
%%   <affiliations node='princely_musings'/>
%%  </pubsub>
%% </iq>

parse('get' = _IQ_Type, [Xmlel_Affiliations], _Rsm, API_Core)
  when ?Is_Xmlel_Affiliations(Xmlel_Affiliations) ->
    case xmlns(Xmlel_Affiliations) of
        ?NS_PUBSUB_OWNER ->
            {result,
             _Module = API_Core#api_core.get_node_affiliations,
             _Function = 'get_node_affiliations',
             _Parameters = {
                 _NodeId = node(Xmlel_Affiliations)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%% TODO : RSM

%-- Get_Node_Subscriptions --%

%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub#owner'>
%%   <subscriptions node='princely_musings'/>
%%  </pubsub>
%% </iq>

parse('get' = _IQ_Type, [Xmlel_Subscriptions], _Rsm, API_Core)
  when ?Is_Xmlel_Subscriptions(Xmlel_Subscriptions) ->
    case xmlns(Xmlel_Subscriptions) of
        ?NS_PUBSUB_OWNER ->
            {result,
             _Module = API_Core#api_core.get_node_subscriptions,
             _Function = 'get_node_subscriptions',
             _Parameters = {
                 _NodeId = node(Xmlel_Subscriptions)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%% TODO : RSM

%-- Retract_Item --%

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <retract node='princely_musings'>
%%    <item id='ae890ac52d0df67ed7cfdf51b644e901'/>
%%   </retract>
%%  </pubsub>
%% </iq>
%%
%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <retract node='princely_musings' notify='true'>
%%    <item id='ae890ac52d0df67ed7cfdf51b644e901'/>
%%   </retract>
%%  </pubsub>
%% </iq>

parse('set' = _IQ_Type, [Xmlel_Retract], _Rsm, API_Core)
  when ?Is_Xmlel_Retract(Xmlel_Retract) ->
    case {xmlns(Xmlel_Retract), notify(Xmlel_Retract)} of
        {?NS_PUBSUB, undefined} ->
            {result,
             _Module = API_Core#api_core.retract_item,
             _Function = 'retract_item',
             _Parameters = {
                 _NodeId = node(Xmlel_Retract),
                 _ItemId = case remove_cdata(Xmlel_Retract#xmlel.children) of
                     [Xmlel_Item]
                       when ?Is_Xmlel_Item(Xmlel_Item) ->
                         case xmlns(Xmlel_Item) of
                            ?NS_PUBSUB -> id(Xmlel_Item);
                            _          -> undefined
                        end;
                     _ ->
                         undefined
                 
                 end,
                 _Notify = undefined
             }
            };
        {?NS_PUBSUB, False}
          when False == <<"false">> orelse False == <<"0">> ->
            {result,
             _Module = API_Core#api_core.retract_item,
             _Function = 'retract_item',
             _Parameters = {
                 _NodeId = node(Xmlel_Retract),
                 _ItemId = case remove_cdata(Xmlel_Retract#xmlel.children) of
                     [Xmlel_Item]
                       when ?Is_Xmlel_Item(Xmlel_Item) ->
                         case xmlns(Xmlel_Item) of
                            ?NS_PUBSUB -> id(Xmlel_Item);
                            _          -> undefined
                        end;
                     _ ->
                         undefined
                 
                 end,
                 _Notify = false
             }
            };
        {?NS_PUBSUB, True}
          when True == <<"true">> orelse True == <<"1">> ->
            {result,
             _Module = API_Core#api_core.retract_item,
             _Function = 'retract_item',
             _Parameters = {
                 _NodeId = node(Xmlel_Retract),
                 _ItemId = case remove_cdata(Xmlel_Retract#xmlel.children) of
                     [Xmlel_Item]
                       when ?Is_Xmlel_Item(Xmlel_Item) ->
                         case xmlns(Xmlel_Item) of
                            ?NS_PUBSUB -> id(Xmlel_Item);
                            _          -> undefined
                        end;
                     _ ->
                         undefined
                 
                 end,
                 _Notify = true
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Subscribe_Node --%

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <subscribe node='princely_musings' jid='francisco@denmark.lit'/>
%%  </pubsub>
%% </iq>
%%
%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <subscribe node='princely_musings' jid='francisco@denmark.lit'/>
%%   <options>
%%    <x xmlns='jabber:x:data' type='submit'>
%%     <field var='FORM_TYPE' type='hidden'>
%%      <value>http://jabber.org/protocol/pubsub#subscribe_options</value>
%%     </field>
%%     <field var='pubsub#deliver'><value>1</value></field>
%%     <field var='pubsub#show-values'>
%%      <value>chat</value>
%%      <value>online</value>
%%      <value>away</value>
%%     </field>
%%    </x>
%%   </options>
%%  </pubsub>
%% </iq>

parse('set' = _IQ_Type, [Xmlel_Subscribe], _Rsm, API_Core)
  when ?Is_Xmlel_Subscribe(Xmlel_Subscribe) ->
    case xmlns(Xmlel_Subscribe) of
        ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.subscribe_node,
             _Function = 'subscribe_node',
             _Parameters = {
                 _NodeId = node(Xmlel_Subscribe),
                 _Jid = id(Xmlel_Subscribe),
                 _Subscribe_Options = []
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;
%%
parse('set' = _IQ_Type, [Xmlel_Subscribe, Xmlel_Options], _Rsm, API_Core)
  when    ?Is_Xmlel_Subscribe(Xmlel_Subscribe)
  andalso ?Is_Xmlel_Options1(Xmlel_Options) ->
    case {xmlns(Xmlel_Subscribe), xmlns(Xmlel_Options)} of
        {NS, NS} when NS == ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.subscribe_node,
             _Function = 'subscribe_node',
             _Parameters = {
                 _NodeId = node(Xmlel_Subscribe),
                 _Jid = jid(Xmlel_Subscribe),
                 _Subscribe_Options = remove_cdata(Xmlel_Options#xmlel.children)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Unsubscribe_Node --%

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <unsubscribe node='princely_musings' jid='francisco@denmark.lit'/>
%%  </pubsub>
%% </iq>
%%
%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <unsubscribe node='princely_musings' jid='francisco@denmark.lit' subid='SubId'/>
%%  </pubsub>
%% </iq>

parse('set' = _IQ_Type, [Xmlel_Unsubscribe], _Rsm, API_Core)
  when ?Is_Xmlel_Unsubscribe(Xmlel_Unsubscribe) ->
    case xmlns(Xmlel_Unsubscribe) of
        ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.unsubscribe_node,
             _Function = 'unsubscribe_node',
             _Parameters = {
                 _NodeId = node(Xmlel_Unsubscribe),
                 _Jid = jid(Xmlel_Unsubscribe),
                 _SubId = subid(Xmlel_Unsubscribe)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Set_Configure_Subscription --%

%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <options node='princely_musings' jid='francisco@denmark.lit'>
%%    <x xmlns='jabber:x:data' type='submit'>
%%     <field var='FORM_TYPE' type='hidden'>
%%      <value>http://jabber.org/protocol/pubsub#subscribe_options</value>
%%     </field>
%%     <field var='pubsub#deliver'><value>1</value></field>
%%     <field var='pubsub#show-values'>
%%      <value>chat</value>
%%      <value>online</value>
%%      <value>away</value>
%%     </field>
%%    </x>
%%   </options>
%%  </pubsub>
%% </iq>
%%
%% <iq type='set'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <options node='princely_musings' jid='francisco@denmark.lit' subid='SubId'>
%%    <x xmlns='jabber:x:data' type='submit'>
%%     <field var='FORM_TYPE' type='hidden'>
%%      <value>http://jabber.org/protocol/pubsub#subscribe_options</value>
%%     </field>
%%     <field var='pubsub#deliver'><value>1</value></field>
%%     <field var='pubsub#show-values'>
%%      <value>chat</value>
%%      <value>online</value>
%%      <value>away</value>
%%     </field>
%%    </x>
%%   </options>
%%  </pubsub>
%% </iq>

parse('set' = _IQ_Type, [Xmlel_Options], _Rsm, API_Core)
  when ?Is_Xmlel_Options2(Xmlel_Options) ->
    case xmlns(Xmlel_Options) of
        ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.set_configure_subscription,
             _Function = 'set_configure_subscription',
             _Parameters = {
                 _NodeId = node(Xmlel_Options),
                 _Jid = jid(Xmlel_Options),
                 _SubId = subid(Xmlel_Options),
                 _Subscribe_Options = remove_cdata(Xmlel_Options#xmlel.children)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Get_Items --%

%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <items node='princely_musings'/>
%%  </pubsub>
%% </iq>
%%
%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <items node='princely_musings' max_items='2'/>
%%  </pubsub>
%% </iq>
%%
%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <items node='princely_musings'>
%%    <item id='368866411b877c30064a5f62b917cffe'/>
%%    <item id='4e30f35051b7b8b42abe083742187228'/>
%%   </items>
%%  </pubsub>
%% </iq>

parse('get' = _IQ_Type, [Xmlel_Items], _Rsm, API_Core)
  when ?Is_Xmlel_Items1(Xmlel_Items) ->
    case {xmlns(Xmlel_Items), max_items(Xmlel_Items)} of
        {?NS_PUBSUB, undefined} ->
            {result,
             _Module = API_Core#api_core.get_items,
             _Function = 'get_items',
             _Parameters = {
                 _NodeId = node(Xmlel_Items),
                 _SubId = subid(Xmlel_Items),
                 _Max_Items = undefined,
                 _ItemIds = []
             }
            };
        {?NS_PUBSUB, Binary} ->
            case catch list_to_integer(binary_to_list(Binary)) of
                Integer
                  when is_integer(Integer) andalso Integer >= 0 ->
                    {result,
                     _Module = API_Core#api_core.get_items,
                     _Function = 'get_items',
                     _Parameters = {
                         _NodeId = node(Xmlel_Items),
                         _SubId = subid(Xmlel_Items),
                         _Max_Items = Integer,
                         _ItemIds = undefined
                     }
                    };
                _ ->
                    {error, 'invalid-payload'}
            end;
        _ ->
            {error, 'invalid-payload'}
    end;
%%
parse('get' = _IQ_Type, [Xmlel_Items], _Rsm, API_Core)
  when ?Is_Xmlel_Items2(Xmlel_Items) ->
    case
        {xmlns(Xmlel_Items),
         parse_xmlels_item(remove_cdata(Xmlel_Items#xmlel.children))}
    of
        {?NS_PUBSUB, {ok, ItemIds}} ->
            {result,
             _Module = API_Core#api_core.get_items,
             _Function = 'get_items',
             _Parameters = {
                 _NodeId = node(Xmlel_Items),
                 _SubId = subid(Xmlel_Items),
                 _Max_Items = undefined,
                 ItemIds
             }
            };
        {_, _Error} ->
            {error, 'invalid-payload'}
    end;

%-- Get_Configure_Subscription_Default --%

%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <default/>
%%  </pubsub>
%% </iq>
%%
%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%   <default node='princely_musings'/>
%%  </pubsub>
%% </iq>

parse('get' = _IQ_Type, [Xmlel_Default], _Rsm, API_Core)
  when ?Is_Xmlel_Default(Xmlel_Default) ->
    case xmlns(Xmlel_Default) of
        ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.get_configure_subscription_default,
             _Function = 'get_configure_subscription_default',
             _Parameters = {
                 _NodeId = node(Xmlel_Default)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Get_Configure_Subscription --%

%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%    <options node='princely_musings' jid='francisco@denmark.lit'/>
%%  </pubsub>
%% </iq>
%%
%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
%%    <options node='princely_musings' jid='francisco@denmark.lit' subid='SubId'/>
%%  </pubsub>
%% </iq>

parse('get' = _IQ_Type, [Xmlel_Options], _Rsm, API_Core)
  when ?Is_Xmlel_Options2(Xmlel_Options) ->
    case xmlns(Xmlel_Options) of
        ?NS_PUBSUB ->
            {result,
             _Module = API_Core#api_core.get_configure_subscription,
             _Function = 'get_configure_subscription',
             _Parameters = {
                 _NodeId = node(Xmlel_Options),
                 _Jid = jid(Xmlel_Options),
                 _SubId = subid(Xmlel_Options)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Get_Configure_Node_Default --%

%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub#owner'>
%%   <default/>
%%  </pubsub>
%% </iq>

parse('get' = _IQ_Type, [Xmlel_Default], _Rsm, API_Core)
  when ?Is_Xmlel_Default(Xmlel_Default) ->
    case xmlns(Xmlel_Default) of
        ?NS_PUBSUB_OWNER ->
            {result,
             _Module = API_Core#api_core.get_configure_node_default,
             _Function = 'get_configure_node_default',
             _Parameters = {
                 _NodeId = node(Xmlel_Default)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%-- Get_Configure_Node --%

%% <iq type='get'>
%%  <pubsub xmlns='http://jabber.org/protocol/pubsub#owner'>
%%    <configure node='princely_musings'/>
%%  </pubsub>
%% </iq>

parse('get' = _IQ_Type, [Xmlel_Configure], _Rsm, API_Core)
  when ?Is_Xmlel_Configure(Xmlel_Configure) ->
    case xmlns(Xmlel_Configure) of
        ?NS_PUBSUB_OWNER ->
            {result,
             _Module = API_Core#api_core.get_configure_node,
             _Function = 'get_configure_node',
             _Parameters = {
                 _NodeId = node(Xmlel_Configure)
             }
            };
        _ ->
            {error, 'invalid-payload'}
    end;

%%
parse(_IQ_Type, _Xmlels, _Rsm, _API_Core) ->
    {error, 'invalid-payload'}.



%%%%%%%%%%%%%%%%%%%%%
%% Parsing Helpers %%
%%%%%%%%%%%%%%%%%%%%%


%%
-spec(parse_xmlels_item/1 ::
(
  Xmlels  :: [Xmlel_Item::xmlel(),...])
    -> {ok, ItemIds :: [] | exmpp_pubsub:itemIds()}
    %%%
     | {error, 'invalid-payload'}
).

parse_xmlels_item(Xmlels) ->
    parse_xmlels_item(Xmlels, []).

%%
-spec(parse_xmlels_item/2 ::
(
  Xmlels  :: [Xmlel_Item::xmlel(),...],
  ItemIds :: [] | exmpp_pubsub:itemIds())
    -> {ok, ItemIds :: [] | exmpp_pubsub:itemIds()}
    %%%
     | {error, 'invalid-payload'}
).


%%
parse_xmlels_item([Xmlel_Item | Xmlels], ItemIds)
  when ?Is_Xmlel_Item(Xmlel_Item) ->
    case {xmlns(Xmlel_Item), id(Xmlel_Item)} of
        {?NS_PUBSUB, ItemId}
          when ItemId /= undefined ->
            parse_xmlels_item(Xmlels, [ItemId | ItemIds]);
        _ ->
            {error, 'invalid-payload'}
    end;
parse_xmlels_item([] = _Xmlels_Item, ItemIds) ->
    {ok, ItemIds};
%%
parse_xmlels_item(_Xmlels, _ItemIds) ->
    {error, 'invalid-payload'}.


xmlns(Xmlel) ->
    get_tag_attr_s(<<"xmlns">>, Xmlel).

node(Xmlel) ->
    case get_tag_attr_s(<<"node">>, Xmlel) of
        <<>>   -> undefined;
        NodeId -> NodeId
    end.

id(Xmlel) ->
    case get_tag_attr_s(<<"id">>, Xmlel) of
        <<>>   -> undefined;
        ItemId -> ItemId
    end.

uri(Xmlel) ->
    case get_tag_attr_s(<<"uri">>, Xmlel) of
        <<>> -> undefined;
        URI  -> URI
    end.

jid(Xmlel) ->
    case get_tag_attr_s(<<"jid">>, Xmlel) of
        <<>> -> undefined;
        Jid  -> Jid
    end.

subid(Xmlel) ->
    case get_tag_attr_s(<<"subid">>, Xmlel) of
        <<>>  -> undefined;
        SubId -> SubId
    end.

notify(Xmlel) ->
    case get_tag_attr_s(<<"notify">>, Xmlel) of
        <<>>   -> undefined;
        Notify -> Notify
    end.

max_items(Xmlel) ->
    case get_tag_attr_s(<<"max_items">>, Xmlel) of
        <<>>      -> undefined;
        Max_Items -> Max_Items
    end.
