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

-module(pubsub_disco).
-author('karim.gemayel@process-one.net').

-compile(export_all).

-include("pubsub_dev.hrl").

-import(pubsub_tools,
[
  get_option/2,
  get_option/3,
  get_value/2,
  get_value/3,
  set_value/3,
  %%
  check_access_model/3,
  check_access_model/7,
  check_publish_model/3,
  is_contact_subscribed_to_node_owners/3,
  is_contact_in_allowed_roster_groups/2,
  has_subscriptions/1
]).


-import(pubsub_db_mnesia,
[
  table/2
]).

-import(xmpp_xdata,
[
  xmlel_field/3,
  xmlel_field_jid_multi/3,
  xmlel_field_jid_single/3,
  xmlel_field_list_single/4,
  xmlel_field_text_single/3,
  xmlel_x/2
]).

%%
-spec(iq_disco_info/9 ::
(
  Suffix      :: undefined | atom(),
  Host        :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host :: exmpp_pubsub:host_pubsub(),
  Privacy     :: boolean(),
  Entity      :: xmpp_jid:usr_bare(),
  Entity_Type :: 'local' | 'remote',
  Plugin      :: exmpp_pubsub:plugin(),
  NodeId      :: undefined | exmpp_pubsub:nodeId(),
  Xmlels      :: [] | [Xmlel_Set::xmlel(),...])
    -> {result, Xmlel_Query::xmlel()}
    %%%
%     | {error, 'feature-not-implemented', ?NS_DISCO_INFO}
%     | {error, 'feature-not-implemented', ?NS_PUBSUB_META_DATA}
%     %
%     | {error, 'item-not-found'}
).

iq_disco_info(_Suffix, Host, Pubsub_Host, _Privacy, _Entity, Entity_Type,
  Plugin, undefined = _NodeId, _Xmlels) ->
    Features = Plugin:features(),
    case lists:member(?NS_DISCO_INFO, Plugin:features()) of
        true ->
            {result,
             exmpp_pubsub:xmlel_query('disco#info',
                 lists:map(fun
                     ({Category, Name, Type}) ->
                         exmpp_pubsub:xmlel_identity('disco#info',
                             Category, Type, Name)
                 end, Plugin:identity())
                 ++
                 lists:map(fun
                    (Feature) ->
                        exmpp_pubsub:xmlel_feature('disco#info', Feature)
                 end, Features)
                 ++
                 lists:map(fun
                    (Pubsub_Feature) ->
                        exmpp_pubsub:xmlel_feature('disco#info',
                            <<?NS_PUBSUB/binary, "#", Pubsub_Feature/binary>>)
                 end, lists:sort(Plugin:pubsub_features(Entity_Type)))
                 )
             };
        false ->
            {error, 'feature-not-implemented', ?NS_DISCO_INFO}
    end;
%%
iq_disco_info(Suffix, Host, Pubsub_Host, false = _Privacy, {_,S,_} = _Entity,
  Entity_Type, Plugin, NodeId, _Xmlels) ->
    Features = Plugin:features(),
    case lists:member(?NS_DISCO_INFO, Features) of
        true ->
            case
                mnesia:dirty_read(table('pubsub_node', Suffix),
                    {Pubsub_Host, NodeId})
            of
                [#pubsub_node_dev{idx = NodeIdx, owners = Node_Owners,
                 options = Node_Options, creation = Creation}] ->
                Pubsub_Features = Plugin:pubsub_features(Entity_Type),
                {result,
                 exmpp_pubsub:xmlel_query('disco#info', NodeId,
                     [exmpp_pubsub:xmlel_identity('disco#info', <<"pubsub">>,
                          get_value(Node_Options, 'node_type'),
                          get_value(Node_Options, 'title', undefined))]
                     ++
                     [exmpp_pubsub:xmlel_identity('disco#info', Category, Type, Name)
                     || {Category, Name, Type} <- Plugin:identity()]
                     ++
                     [exmpp_pubsub:xmlel_feature('disco#info', Feature)
                     || Feature <- Features]
                     ++
                     [exmpp_pubsub:xmlel_feature('disco#info',
                          <<?NS_PUBSUB/binary, "#", Pubsub_Feature/binary>>)
                     || Pubsub_Feature <- Pubsub_Features]
                     ++
                     case lists:member(<<"meta-data">>, Pubsub_Features) of
                        true ->
                            [xmlel_x_metadata(table('pubsub_state', Suffix),
                                 NodeIdx, Node_Owners, Creation,
                                 Node_Options)];
                        false ->
                            []
                     end)};
                _ ->
                    {error, 'item-not-found'}
            end;
        false ->
            {error, 'feature-not-implemented', ?NS_DISCO_INFO}
    end.

%%
-spec(xmlel_x_metadata/5 ::
(
  Table_Pubsub_State :: atom(),
  NodeIdx      :: exmpp_pubsub:nodeIdx(),
  Node_Owners  :: mod_pubsub_dev:node_owners(),
  Creation     :: mod_pubsub_dev:node_creation(),
  Node_Options :: pubsub_options:options_node())
    -> Xmlel::xmlel()
).

%% <x xmlns='jabber:x:data' type='result'>
%%  <field var='FORM_TYPE' type='hidden'>
%%   <value>http://jabber.org/protocol/pubsub#meta-data</value>
%%  </field>
%%  <field var='pubsub#type' label='Payload type' type='text-single'>
%%   <value>http://www.w3.org/2005/Atom</value>
%%  </field>
%%  <field var='pubsub#creator' label='Node creator' type='jid-single'>
%%   <value>hamlet@denmark.lit</value>
%%  </field>
%%  <field var='pubsub#creation_date' label='Creation date' type='text-single'>
%%   <value>2003-07-29T22:56Z</value>
%%  </field>
%%  <field var='pubsub#title' label='A short name for the node' type='text-single'>
%%   <value>Princely Musings (Atom)</value>
%%  </field>
%%  <field var='pubsub#description' label='A description of the node' type='text-single'>
%%   <value>Updates for Hamlet&apos;s Princely Musings weblog.</value>
%%  </field>
%%  <field var='pubsub#language' label='Default language' type='list-single'>
%%   <value>en</value>
%%  </field>
%%  <field var='pubsub#contact' label='People to contact with questions' type='jid-multi'>
%%   <value>bard@shakespeare.lit</value>
%%  </field>
%%  <field var='pubsub#owner' label='Node owners' type='jid-multi'>
%%   <value>hamlet@denmark.lit</value>
%%  </field>
%%  <field var='pubsub#publisher' label='Publishers to this node' type='jid-multi'>
%%   <value>hamlet@denmark.lit</value>
%%  </field>
%%  <field var='pubsub#num_subscribers' label='Number of subscribers to this node' type='text-single'>
%%   <value>1066</value>
%%  </field>
%% </x>

xmlel_x_metadata(_Table_Pubsub_State, _NodeIdx, Node_Owners,
  {TimeStamp, Creator} = _Creation, Node_Options) ->
    {DateTime, _} = jlib:timestamp_to_iso(calendar:now_to_datetime(TimeStamp), utc),
    Creation_Date = list_to_binary(DateTime ++ "Z"),
    xmlel_x(<<"form">>, [
        xmlel_field(<<"FORM_TYPE">>, <<"result">>, [?NS_PUBSUB_META_DATA]),
        %
        xmlel_field_text_single(<<"pubsub#type">>, <<"Payload type">>,
            get_value(Node_Options, 'type', undefined)),
        %
        xmlel_field_jid_single(<<"pubsub#creator">>, <<"Node creator">>,
            pubsub_tools:jid_to_string(Creator)),
        %
        xmlel_field_text_single(<<"pubsub#creation_date">>, <<"Creation date">>,
            Creation_Date),
        %
        xmlel_field_text_single(<<"pubsub#title">>, <<"Node title">>,
            get_value(Node_Options, 'title', undefined)),
        %
        xmlel_field_text_single(<<"pubsub#description">>, <<"Node description">>,
            get_value(Node_Options, 'description', undefined)),
        %
        xmlel_field_list_single(<<"pubsub#language">>, <<"Default language">>,
            get_value(Node_Options, 'language', <<"en">>), []),
        %
        xmlel_field_jid_multi(<<"pubsub#contact">>,
            <<"People to contact with questions">>,
            get_value(Node_Options, 'contact', [])),
        %
        xmlel_field_jid_multi(<<"pubsub#owner">>, <<"Node owners">>,
            [pubsub_tools:jid_to_string(Node_Owner) || Node_Owner <- Node_Owners])
        %
%        xmlel_field_jid_multi(<<"pubsub#publisher">>,
%            <<"Publishers to this node">>,
%            [pubsub_tools:jid_to_string(Publisher)
%            || Publisher
%                   <- mnesia:dirty_select(Table_Pubsub_State,
%                          [{#pubsub_state_dev{
%                                id      = {'$1', '_'},
%                                nodeidx = NodeIdx,
%                                itemids = '$2',
%                               _       = '_'},
%                          [{'=/=', '$2', []}],
%                          ['$1']}])
%            ]
%           ),
%        %
%        xmlel_field_text_single(<<"pubsub#num_subscribers">>,
%            <<"Number of subscribers to this node">>,
%            list_to_binary(integer_to_list(length(
%                mnesia:dirty_select(Table_Pubsub_State,
%                    [{#pubsub_state_dev{
%                          id            = {'$1', '_'},
%                          nodeidx       = NodeIdx,
%                          access        = '$2',
%                          subscriptions = '$3',
%                          _             = '_'},
%                    [{'=/=', '$2', 'pending'},
%                     {'=/=', '$3', []}],
%                    ['$1']}])
%            )))
%           )
        ]).

%%
-spec(iq_disco_items/9 ::
(
  Suffix      :: undefined | atom(),
  Host        :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host :: exmpp_pubsub:host_pubsub(),
  Privacy     :: boolean(),
  Entity      :: xmpp_jid:usr_bare(),
  Entity_Type :: 'local' | 'remote',
  Plugin      :: exmpp_pubsub:plugin(),
  NodeId      :: undefined | exmpp_pubsub:nodeId(),
  Xmlels      :: [] | [Xmlel_Set::xmlel(),...])
    -> {result, Xmlel_Query::xmlel()}
    %%%
%     | {error, 'feature-not-implemented', ?NS_DISCO_ITEMS}
%     %
%     | {error, 'item-not-found'}
).

iq_disco_items(Suffix, Host, Pubsub_Host, Privacy, Entity, _Entity_Type,
  Plugin, NodeId, _Xmlels) ->
    case lists:member(?NS_DISCO_ITEMS, Plugin:features()) of
        true  -> disco_nodes(Privacy, NodeId, Suffix, Host, Pubsub_Host, Entity);
        false -> {error, 'feature-not-implemented', ?NS_DISCO_ITEMS}
    end.

%%
-spec(disco_nodes/6 ::
(
  Privacy     :: boolean(),
  NodeId      :: undefined | exmpp_pubsub:nodeId(),
  Suffix      :: undefined | atom(),
  Host        :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host :: exmpp_pubsub:host_pubsub(),
  Entity      :: xmpp_jid:usr_bare())
    -> {result, Xmlel_Query::xmlel()}
    %%%
     | {error, 'item-not-found'}
).

disco_nodes(false, undefined, Suffix, _, Pubsub_Host, _) ->
    {result,
     exmpp_pubsub:xmlel_query('disco#items',
         [exmpp_pubsub:xmlel_item('disco#items', NodeId, Pubsub_Host,
              get_value(Node_Options, 'title', undefined))
         || #pubsub_node_dev{id = {_, NodeId}, options = Node_Options}
                <- mnesia:dirty_match_object(table('pubsub_node', Suffix),
                       #pubsub_node_dev{
                           id    = {Pubsub_Host, '_'},
                           level = 1,
                           _     = '_'
                       })
         ])};
%%
disco_nodes(true, undefined, Suffix, Host, Pubsub_Host, Entity) ->
    {result,
     exmpp_pubsub:xmlel_query('disco#items',
         disco_nodes(
             mnesia:dirty_match_object(table('pubsub_node', Suffix),
                 #pubsub_node_dev{
                     id = {Pubsub_Host, '_'},
                     _  = '_'
                 }),
             Host, Entity, table('pubsub_state', Suffix), {[], [], []}))};
%%
disco_nodes(false, NodeId, Suffix, _Host, Pubsub_Host, _Entity) ->
    Table_Pubsub_Node = table('pubsub_node', Suffix),
    case mnesia:dirty_read(Table_Pubsub_Node, {Pubsub_Host, NodeId}) of
        [#pubsub_node_dev{idx = NodeIdx, options = Node_Options}] ->
            {result,
             exmpp_pubsub:xmlel_query('disco#items',
                case get_value(Node_Options, 'node_type') of
                    'leaf' ->
                        [exmpp_pubsub:xmlel_item('disco#items', ItemId,
                             Pubsub_Host)
                        || #pubsub_item_dev{id = {ItemId, _}}
                               <- mnesia:dirty_index_match_object(
                                      table('pubsub_item', Suffix),
                                      #pubsub_item_dev{
                                          id      = {'_', NodeIdx},
                                          nodeidx = NodeIdx,
                                           _      = '_'
                                      },
                                      nodeidx)
                        ];
                    'collection' ->
                        lists:foldr(fun
                            (_NodeId, Xmlels_Item) ->
                                case
                                    mnesia:dirty_read(Table_Pubsub_Node,
                                        {Pubsub_Host, _NodeId})
                                of
                                    [#pubsub_node_dev{options = _Node_Options}] ->
                                        [exmpp_pubsub:xmlel_item('disco#items',
                                             _NodeId, Pubsub_Host,
                                             get_value(_Node_Options, 'title',
                                                 undefined))
                                        |Xmlels_Item];
                                    _ ->
                                        Xmlels_Item
                                end
                        end, [], get_value(Node_Options, 'children', []))
                end)};
         _ ->
            {error, 'item_not-found'}
    end;
%%
disco_nodes(true, NodeId, Suffix, Host, Pubsub_Host, Entity) ->
    Table_Pubsub_Node = table('pubsub_node', Suffix),
    case mnesia:dirty_read(Table_Pubsub_Node, {Pubsub_Host, NodeId}) of
        [#pubsub_node_dev{idx = NodeIdx, owners = Node_Owners,
         options = Node_Options}] ->
            case lists:member(Entity, Node_Owners) of
                true ->
                    {result,
                     exmpp_pubsub:xmlel_query('disco#items',
                        case get_value(Node_Options, 'node_type') of
                            'leaf' ->
                                disco_items(
                                    mnesia:dirty_index_match_object(
                                        table('pubsub_item', Suffix),
                                        #pubsub_item_dev{
                                            id      = {'_', NodeIdx},
                                            nodeidx = NodeIdx,
                                            _      = '_'
                                        },
                                        nodeidx),
                                    Host, Pubsub_Host,
                                    get_value(Node_Options, 'access_model'),
                                    get_value(Node_Options, 'roster_groups_allowed', []),
                                    Node_Owners,
                                    Entity,
                                    'owner',
                                    undefined,
                                    {[], [], []});
                            'collection' ->
                                disco_nodes(
                                    lists:foldr(fun
                                        (_NodeId, Pubsub_Nodes) ->
                                        case
                                            mnesia:dirty_read(Table_Pubsub_Node,
                                                {Pubsub_Host, _NodeId})
                                        of
                                            [Pubsub_Node] ->
                                                [Pubsub_Node | Pubsub_Nodes];
                                            _ ->
                                                Pubsub_Nodes
                                        end
                                    end, [], get_value(Node_Options, 'children', [])),
                                    Host, Entity, table('pubsub_state', Suffix),
                                    {[], [], []})
                        end)};
                false ->
                    Node_Access_Model = get_value(Node_Options, 'access_model'),
                    case
                        mnesia:dirty_index_match_object(
                            table('pubsub_state', Suffix),
                            #pubsub_state_dev{
                                id      = {Entity, NodeIdx},
                                nodeidx = NodeIdx,
                                _       = '_'
                            },
                            nodeidx)
                    of
                        %%
                        [#pubsub_state_dev{affiliation = Affiliation}]
                          when    Node_Access_Model == 'open'
                          andalso (Affiliation == 'member'
                                       orelse
                                   Affiliation == 'none'
                                       orelse
                                   Affiliation == 'publisher') ->
                            {result,
                             exmpp_pubsub:xmlel_query('disco#items',
                            case get_value(Node_Options, 'node_type') of
                                'leaf' ->
                                    disco_items(
                                        mnesia:dirty_index_match_object(
                                            table('pubsub_item', Suffix),
                                            #pubsub_item_dev{
                                                id      = {'_', NodeIdx},
                                                nodeidx = NodeIdx,
                                                _      = '_'
                                            },
                                            nodeidx),
                                        Host, Pubsub_Host,
                                        get_value(Node_Options, 'access_model'),
                                        get_value(Node_Options,
                                            'roster_groups_allowed', []),
                                        Node_Owners,
                                        Entity,
                                        'owner',
                                        undefined,
                                        {[], [], []});
                                'collection' ->
                                    disco_nodes(
                                        lists:foldr(fun
                                            (_NodeId, Pubsub_Nodes) ->
                                            case
                                                mnesia:dirty_read(Table_Pubsub_Node,
                                                    {Pubsub_Host, _NodeId})
                                            of
                                                [Pubsub_Node] ->
                                                    [Pubsub_Node | Pubsub_Nodes];
                                                _ ->
                                                    Pubsub_Nodes
                                            end
                                        end, [],
                                            get_value(Node_Options, 'children', [])),
                                        Host, Entity, table('pubsub_state', Suffix),
                                        {[], [], []})
                            end)};
                        %%
                        []
                          when Node_Access_Model == 'open' ->
                            {result,
                             exmpp_pubsub:xmlel_query('disco#items',
                            case get_value(Node_Options, 'node_type') of
                                'leaf' ->
                                    disco_items(
                                        mnesia:dirty_index_match_object(
                                            table('pubsub_item', Suffix),
                                            #pubsub_item_dev{
                                                id      = {'_', NodeIdx},
                                                nodeidx = NodeIdx,
                                                _      = '_'
                                            },
                                            nodeidx),
                                        Host, Pubsub_Host,
                                        get_value(Node_Options, 'access_model'),
                                        get_value(Node_Options,
                                            'roster_groups_allowed', []),
                                        Node_Owners,
                                        Entity,
                                        'owner',
                                        undefined,
                                        {[], [], []});
                                'collection' ->
                                    disco_nodes(
                                        lists:foldr(fun
                                            (_NodeId, Pubsub_Nodes) ->
                                            case
                                                mnesia:dirty_read(Table_Pubsub_Node,
                                                    {Pubsub_Host, _NodeId})
                                            of
                                                [Pubsub_Node] ->
                                                    [Pubsub_Node | Pubsub_Nodes];
                                                _ ->
                                                    Pubsub_Nodes
                                            end
                                        end, [],
                                            get_value(Node_Options, 'children', [])),
                                        Host, Entity, table('pubsub_state', Suffix),
                                        {[], [], []})
                            end)};
                        %%
                        _
                          when Node_Access_Model == 'open' ->
                            {error, 'item-not-found'};
                        %%
                        [#pubsub_state_dev{affiliation = Affiliation, access = Access}]
                          when Node_Access_Model == 'presence'
                          andalso (Affiliation == 'member'
                                       orelse
                                   Affiliation == 'publisher'
                                       orelse
                                   Access      == 'presence'
                                       orelse
                                   Access      == 'roster') ->
                            {result,
                             exmpp_pubsub:xmlel_query('disco#items',
                            case get_value(Node_Options, 'node_type') of
                                'leaf' ->
                                    disco_items(
                                        mnesia:dirty_index_match_object(
                                            table('pubsub_item', Suffix),
                                            #pubsub_item_dev{
                                                id      = {'_', NodeIdx},
                                                nodeidx = NodeIdx,
                                                _      = '_'
                                            },
                                            nodeidx),
                                        Host, Pubsub_Host,
                                        get_value(Node_Options, 'access_model'),
                                        get_value(Node_Options,
                                            'roster_groups_allowed', []),
                                        Node_Owners,
                                        Entity,
                                        'owner',
                                        undefined,
                                        {[], [], []});
                                'collection' ->
                                    disco_nodes(
                                        lists:foldr(fun
                                            (_NodeId, Pubsub_Nodes) ->
                                            case
                                                mnesia:dirty_read(Table_Pubsub_Node,
                                                    {Pubsub_Host, _NodeId})
                                            of
                                                [Pubsub_Node] ->
                                                    [Pubsub_Node | Pubsub_Nodes];
                                                _ ->
                                                    Pubsub_Nodes
                                            end
                                        end, [],
                                            get_value(Node_Options, 'children', [])),
                                        Host, Entity, table('pubsub_state', Suffix),
                                        {[], [], []})
                            end)};
                        %%
                        [#pubsub_state_dev{affiliation = Affiliation}]
                          when    Node_Access_Model == 'presence'
                          andalso (Affiliation == 'outcast'
                                       orelse
                                   Affiliation == 'publish-only') ->
                            {error, 'item-not-found'};
                        %%
                        _
                          when    Node_Access_Model == 'presence' ->
                            case
                                is_contact_subscribed_to_node_owners(Host, Entity,
                                    Node_Owners)
                            of
                                false ->
                                    {error, 'item-not-found'};
                                Node_Owner ->
                                {result, exmpp_pubsub:xmlel_query('disco#items',
                                case get_value(Node_Options, 'node_type') of
                                    'leaf' ->
                                        disco_items(
                                            mnesia:dirty_index_match_object(
                                                table('pubsub_item', Suffix),
                                                #pubsub_item_dev{
                                                    id      = {'_', NodeIdx},
                                                    nodeidx = NodeIdx,
                                                    _      = '_'
                                                },
                                                nodeidx),
                                            Host, Pubsub_Host,
                                            get_value(Node_Options, 'access_model'),
                                            get_value(Node_Options,
                                                'roster_groups_allowed', []),
                                            Node_Owners,
                                            Entity,
                                            'owner',
                                            undefined,
                                            {[], [], []});
                                    'collection' ->
                                        disco_nodes(
                                            lists:foldr(fun
                                                (_NodeId, Pubsub_Nodes) ->
                                                case
                                                    mnesia:dirty_read(Table_Pubsub_Node,
                                                        {Pubsub_Host, _NodeId})
                                                of
                                                    [Pubsub_Node] ->
                                                        [Pubsub_Node | Pubsub_Nodes];
                                                    _ ->
                                                        Pubsub_Nodes
                                                end
                                            end, [],
                                                get_value(Node_Options, 'children', [])),
                                            Host, Entity, table('pubsub_state', Suffix),
                                            {[], [Node_Owner], []})
                                end)}
                            end;
                        %%
                        [#pubsub_state_dev{affiliation = Affiliation, access = Access}]
                          when Node_Access_Model == 'roster'
                          andalso (Affiliation == 'member'
                                       orelse
                                   Affiliation == 'publisher'
                                       orelse
                                   (Access      == 'roster'
                                        andalso
                                    Affiliation == 'none')) ->
                            {result,
                             exmpp_pubsub:xmlel_query('disco#items',
                            case get_value(Node_Options, 'node_type') of
                                'leaf' ->
                                    disco_items(
                                        mnesia:dirty_index_match_object(
                                            table('pubsub_item', Suffix),
                                            #pubsub_item_dev{
                                                id      = {'_', NodeIdx},
                                                nodeidx = NodeIdx,
                                                _      = '_'
                                            },
                                            nodeidx),
                                        Host, Pubsub_Host,
                                        get_value(Node_Options, 'access_model'),
                                        get_value(Node_Options,
                                            'roster_groups_allowed', []),
                                        Node_Owners,
                                        Entity,
                                        'owner',
                                        undefined,
                                        {[], [], []});
                                'collection' ->
                                    disco_nodes(
                                        lists:foldr(fun
                                            (_NodeId, Pubsub_Nodes) ->
                                            case
                                                mnesia:dirty_read(Table_Pubsub_Node,
                                                    {Pubsub_Host, _NodeId})
                                            of
                                                [Pubsub_Node] ->
                                                    [Pubsub_Node | Pubsub_Nodes];
                                                _ ->
                                                    Pubsub_Nodes
                                            end
                                        end, [],
                                            get_value(Node_Options, 'children', [])),
                                        Host, Entity, table('pubsub_state', Suffix),
                                        {[], [], []})
                            end)};
                        %%
                        [#pubsub_state_dev{affiliation = Affiliation}]
                          when    Node_Access_Model == 'roster'
                          andalso (Affiliation == 'outcast'
                                       orelse
                                   Affiliation == 'publish-only') ->
                            {error, 'item-not-found'};
                        %%
                        _
                          when    Node_Access_Model == 'roster' ->
                            Rosters_Groups_Allowed = get_value(Node_Options,
                                'roster_groups_allowed', []),
                            case
                                is_contact_in_allowed_roster_groups(Entity,
                                    Rosters_Groups_Allowed)
                            of
                                false ->
                                    {error, 'item-not-found'};
                                {Node_Owner, Roster_Group} ->
                                    {result,
                                     exmpp_pubsub:xmlel_query('disco#items',
                                    case get_value(Node_Options, 'node_type') of
                                        'leaf' ->
                                            disco_items(
                                                mnesia:dirty_index_match_object(
                                                    table('pubsub_item', Suffix),
                                                    #pubsub_item_dev{
                                                        id      = {'_', NodeIdx},
                                                        nodeidx = NodeIdx,
                                                        _      = '_'
                                                    },
                                                    nodeidx),
                                                Host, Pubsub_Host,
                                                get_value(Node_Options, 'access_model'),
                                                Rosters_Groups_Allowed,
                                                Node_Owners,
                                                Entity,
                                                'owner',
                                                undefined,
                                                {[], [], []});
                                        'collection' ->
                                            disco_nodes(
                                                lists:foldr(fun
                                                    (_NodeId, Pubsub_Nodes) ->
                                                    case
                                                        mnesia:dirty_read(
                                                            Table_Pubsub_Node,
                                                            {Pubsub_Host, _NodeId})
                                                    of
                                                        [Pubsub_Node] ->
                                                            [Pubsub_Node | Pubsub_Nodes];
                                                        _ ->
                                                            Pubsub_Nodes
                                                    end
                                                end, [],
                                                    get_value(Node_Options,
                                                        'children', [])),
                                                Host, Entity,
                                                table('pubsub_state', Suffix),
                                                {[], [], [{Node_Owner, [Roster_Group]}]})
                                    end)}
                            end;
                        %%
                        [#pubsub_state_dev{affiliation = Affiliation, access = Access}]
                          when    Node_Access_Model == 'authorize'
                          andalso ((Affiliation =/= 'publish-only'
                                        andalso
                                    Affiliation =/= 'outcast')
                                    orelse
                                  Access == 'authorize') ->
                            {result,
                             exmpp_pubsub:xmlel_query('disco#items',
                            case get_value(Node_Options, 'node_type') of
                                'leaf' ->
                                    disco_items(
                                        mnesia:dirty_index_match_object(
                                            table('pubsub_item', Suffix),
                                            #pubsub_item_dev{
                                                id      = {'_', NodeIdx},
                                                nodeidx = NodeIdx,
                                                _      = '_'
                                            },
                                            nodeidx),
                                        Host, Pubsub_Host,
                                        get_value(Node_Options, 'access_model'),
                                        get_value(Node_Options,
                                            'roster_groups_allowed', []),
                                        Node_Owners,
                                        Entity,
                                        'owner',
                                        undefined,
                                        {[], [], []});
                                'collection' ->
                                    disco_nodes(
                                        lists:foldr(fun
                                            (_NodeId, Pubsub_Nodes) ->
                                            case
                                                mnesia:dirty_read(Table_Pubsub_Node,
                                                    {Pubsub_Host, _NodeId})
                                            of
                                                [Pubsub_Node] ->
                                                    [Pubsub_Node | Pubsub_Nodes];
                                                _ ->
                                                    Pubsub_Nodes
                                            end
                                        end, [],
                                            get_value(Node_Options, 'children', [])),
                                        Host, Entity, table('pubsub_state', Suffix),
                                        {[], [], []})
                            end)};
                        %%
                        []
                          when Node_Access_Model == 'authorize' ->
                            {result,
                             exmpp_pubsub:xmlel_query('disco#items',
                            case get_value(Node_Options, 'node_type') of
                                'leaf' ->
                                    disco_items(
                                        mnesia:dirty_index_match_object(
                                            table('pubsub_item', Suffix),
                                            #pubsub_item_dev{
                                                id      = {'_', NodeIdx},
                                                nodeidx = NodeIdx,
                                                _      = '_'
                                            },
                                            nodeidx),
                                        Host, Pubsub_Host,
                                        get_value(Node_Options, 'access_model'),
                                        get_value(Node_Options,
                                            'roster_groups_allowed', []),
                                        Node_Owners,
                                        Entity,
                                        'owner',
                                        undefined,
                                        {[], [], []});
                                'collection' ->
                                    disco_nodes(
                                        lists:foldr(fun
                                            (_NodeId, Pubsub_Nodes) ->
                                            case
                                                mnesia:dirty_read(Table_Pubsub_Node,
                                                    {Pubsub_Host, _NodeId})
                                            of
                                                [Pubsub_Node] ->
                                                    [Pubsub_Node | Pubsub_Nodes];
                                                _ ->
                                                    Pubsub_Nodes
                                            end
                                        end, [],
                                            get_value(Node_Options, 'children', [])),
                                        Host, Entity, table('pubsub_state', Suffix),
                                        {[], [], []})
                            end)};
                        %%
                        _
                          when Node_Access_Model == 'authorize' ->
                            {error, 'item-not-found'};
                        %%
                        [#pubsub_state_dev{affiliation = Affiliation, access = Access}]
                          when    Node_Access_Model == 'whitelist'
                          andalso (Access == 'whitelist'
                                       orelse
                                  (Affiliation =/= 'none'
                                       andalso
                                   Affiliation =/= 'outcast'
                                       andalso
                                   Affiliation =/= 'publish-only')) ->
                            {result,
                             exmpp_pubsub:xmlel_query('disco#items',
                            case get_value(Node_Options, 'node_type') of
                                'leaf' ->
                                    disco_items(
                                        mnesia:dirty_index_match_object(
                                            table('pubsub_item', Suffix),
                                            #pubsub_item_dev{
                                                id      = {'_', NodeIdx},
                                                nodeidx = NodeIdx,
                                                _      = '_'
                                            },
                                            nodeidx),
                                        Host, Pubsub_Host,
                                        get_value(Node_Options, 'access_model'),
                                        get_value(Node_Options,
                                            'roster_groups_allowed', []),
                                        Node_Owners,
                                        Entity,
                                        'owner',
                                        undefined,
                                        {[], [], []});
                                'collection' ->
                                    disco_nodes(
                                        lists:foldr(fun
                                            (_NodeId, Pubsub_Nodes) ->
                                                case
                                                    mnesia:dirty_read(
                                                        Table_Pubsub_Node,
                                                    {Pubsub_Host, _NodeId})
                                                of
                                                    [Pubsub_Node] ->
                                                        [Pubsub_Node | Pubsub_Nodes];
                                                    _ ->
                                                        Pubsub_Nodes
                                                end
                                        end, [],
                                            get_value(Node_Options, 'children', [])),
                                        Host, Entity, table('pubsub_state', Suffix),
                                        {[], [], []})
                            end)};
                        %%
                        _ ->
                            {error, 'item-not-found'}
                    end
                end;
         _ ->
            {error, 'item_not-found'}
    end.

%%
-spec(disco_nodes/5 ::
(
  Pubsub_Nodes       :: [] | mod_pubsub_dev:pubsub_nodes(),
  Host               :: xmpp_jid:raw_jid_component_bare(),
  Entity             :: xmpp_jid:usr_bare(),
  Table_Pubsub_State :: atom(),
  Cache                  :: {
      Xmlels_Items   :: [Xmlel_Item::xmlel()],
      Presence_Cache :: [Entity::xmpp_jid:usr_bare()],
      Groups_Cache   :: [{Entity::xmpp_jid:usr_bare(), Groups::[Group::binary(),...]}]
  })
    -> Xmlels_Items::[Xmlel_Item::xmlel()]
).

disco_nodes([] = _Pubsub_Nodes, _Host, _Entity, _Table_Pubsub_State,
  {Xmlels_Item, _Presence_Cache, _Groups_Cache}) ->
    Xmlels_Item;
%%
disco_nodes([#pubsub_node_dev{id = {Pubsub_Host, NodeId},
  idx = NodeIdx, owners = Node_Owners, options = Node_Options} | Pubsub_Nodes],
  Host, Entity, Table_Pubsub_State,
  {Xmlels_Item, Presence_Cache, Groups_Cache}) ->
    disco_nodes(Pubsub_Nodes, Host, Entity, Table_Pubsub_State,
        case lists:member(Entity, Node_Owners) of
            true ->
                {[exmpp_pubsub:xmlel_item('disco#items', NodeId, Pubsub_Host,
                      get_value(Node_Options, 'title', undefined))
                 |Xmlels_Item],
                 Presence_Cache, Groups_Cache};
            false ->
                Node_Access_Model = get_value(Node_Options, 'access_model'),
                case
                    mnesia:dirty_index_match_object(Table_Pubsub_State,
                       #pubsub_state_dev{
                           id      = {Entity, NodeIdx},
                           nodeidx = NodeIdx,
                           _       = '_'
                        },
                        nodeidx)
                of
                    %%
                    [#pubsub_state_dev{affiliation = Affiliation}]
                      when    Node_Access_Model == 'open'
                      andalso (Affiliation == 'member'
                                   orelse
                               Affiliation == 'none'
                                   orelse
                               Affiliation == 'publisher') ->
                        {[exmpp_pubsub:xmlel_item('disco#items', NodeId,
                              Pubsub_Host,
                              get_value(Node_Options, 'title', undefined))
                         |Xmlels_Item],
                         Presence_Cache, Groups_Cache};
                    %%
                    []
                      when Node_Access_Model == 'open' ->
                        {[exmpp_pubsub:xmlel_item('disco#items', NodeId,
                              Pubsub_Host,
                              get_value(Node_Options, 'title', undefined))
                         |Xmlels_Item],
                         Presence_Cache, Groups_Cache};
                    %%
                    _
                      when Node_Access_Model == 'open' ->
                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                    %%
                    [#pubsub_state_dev{affiliation = Affiliation, access = Access}]
                      when Node_Access_Model == 'presence'
                      andalso (Affiliation == 'member'
                                   orelse
                               Affiliation == 'publisher'
                                   orelse
                               Access      == 'presence'
                                   orelse
                               Access      == 'roster') ->
                        {[exmpp_pubsub:xmlel_item('disco#items', NodeId, Pubsub_Host,
                              get_value(Node_Options, 'title', undefined))
                         |Xmlels_Item],
                          Presence_Cache, Groups_Cache};
                    %%
                    [#pubsub_state_dev{affiliation = Affiliation}]
                      when    Node_Access_Model == 'presence'
                      andalso (Affiliation == 'outcast'
                                   orelse
                               Affiliation == 'publish-only') ->
                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                    %%
                    _
                      when    Node_Access_Model == 'presence'
                      andalso Presence_Cache    == [] ->
                        case
                            is_contact_subscribed_to_node_owners(Host, Entity,
                                Node_Owners)
                        of
                            false ->
                                {Xmlels_Item, Presence_Cache, Groups_Cache};
                            Node_Owner ->
                                {[exmpp_pubsub:xmlel_item('disco#items', NodeId,
                                      Pubsub_Host,
                                  get_value(Node_Options, 'title', undefined))
                                 |Xmlels_Item],
                                 [Node_Owner | Presence_Cache], Groups_Cache}
                        end;
                    %%
                    _
                      when Node_Access_Model == 'presence' ->
                        case presence_cache(Presence_Cache, Node_Owners) of
                            true ->
                                {[exmpp_pubsub:xmlel_item('disco#items', NodeId,
                                      Pubsub_Host,
                                  get_value(Node_Options, 'title', undefined))
                                 |Xmlels_Item],
                                 Presence_Cache, Groups_Cache};
                            false ->
                                case
                                    is_contact_subscribed_to_node_owners(Host,
                                        Entity, Node_Owners)
                                of
                                    false ->
                                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                                    Node_Owner ->
                                        {[exmpp_pubsub:xmlel_item('disco#items',
                                              NodeId, Pubsub_Host,
                                          get_value(Node_Options, 'title', undefined))
                                         |Xmlels_Item],
                                         [Node_Owner | Presence_Cache], Groups_Cache}
                                end
                        end;
                    %%
                    [#pubsub_state_dev{affiliation = Affiliation, access = Access}]
                      when Node_Access_Model == 'roster'
                      andalso (Affiliation == 'member'
                                   orelse
                               Affiliation == 'publisher'
                                   orelse
                               (Access      == 'roster'
                                    andalso
                                Affiliation == 'none')) ->
                        {[exmpp_pubsub:xmlel_item('disco#items', NodeId, Pubsub_Host,
                              get_value(Node_Options, 'title', undefined))
                         |Xmlels_Item],
                          Presence_Cache, Groups_Cache};
                    %%
                    [#pubsub_state_dev{affiliation = Affiliation}]
                      when    Node_Access_Model == 'roster'
                      andalso (Affiliation == 'outcast'
                                   orelse
                               Affiliation == 'publish-only') ->
                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                    %%
                    _
                      when    Node_Access_Model == 'roster'
                      andalso Groups_Cache      == [] ->
                        case
                            is_contact_in_allowed_roster_groups(Entity,
                                get_value(Node_Options, 'roster_groups_allowed'))
                        of
                            false ->
                                {Xmlels_Item, Presence_Cache, Groups_Cache};
                            {Node_Owner, Roster_Group} ->
                                {[exmpp_pubsub:xmlel_item('disco#items', NodeId,
                                      Pubsub_Host,
                                      get_value(Node_Options, 'title', undefined))
                                 |Xmlels_Item],
                                  Presence_Cache,
                                  [{Node_Owner, [Roster_Group]} | Groups_Cache]}
                        end;
                    %%
                    _
                      when Node_Access_Model == 'roster' ->
                        Rosters_Groups_Allowed = get_value(Node_Options,
                            'roster_groups_allowed'),
                        case groups_cache(Groups_Cache, Rosters_Groups_Allowed) of
                            true ->
                                {[exmpp_pubsub:xmlel_item('disco#items', NodeId,
                                      Pubsub_Host,
                                  get_value(Node_Options, 'title', undefined))
                                 |Xmlels_Item],
                                 Presence_Cache, Groups_Cache};
                            false ->
                                case
                                    is_contact_in_allowed_roster_groups(Entity,
                                        Rosters_Groups_Allowed)
                                of
                                    false ->
                                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                                    {Node_Owner, Roster_Group} ->
                                        {[exmpp_pubsub:xmlel_item('disco#items',
                                              NodeId, Pubsub_Host,
                                              get_value(Node_Options, 'title',
                                                  undefined))
                                         |Xmlels_Item],
                                          Presence_Cache,
                                          case
                                              lists:keyfind(Node_Owner, 1,
                                                  Groups_Cache)
                                          of
                                              {_, Groups} ->
                                                  lists:keyreplace(Node_Owner, 1,
                                                      Groups_Cache,
                                                      {Node_Owner,
                                                       [Roster_Group | Groups]});
                                              false ->
                                                  [{Node_Owner, [Roster_Group]}
                                                  | Groups_Cache]
                                          end}
                                end
                        end;
                    %%
                    [#pubsub_state_dev{affiliation = Affiliation, access = Access}]
                      when    Node_Access_Model == 'authorize'
                      andalso ((Affiliation =/= 'publish-only'
                                    andalso
                                Affiliation =/= 'outcast')
                                orelse
                              Access == 'authorize') ->
                        {[exmpp_pubsub:xmlel_item('disco#items', NodeId,
                              Pubsub_Host,
                              get_value(Node_Options, 'title', undefined))
                         |Xmlels_Item],
                         Presence_Cache, Groups_Cache};
                    %%
                    []
                      when Node_Access_Model == 'authorize' ->
                        {[exmpp_pubsub:xmlel_item('disco#items', NodeId,
                              Pubsub_Host,
                              get_value(Node_Options, 'title', undefined))
                         |Xmlels_Item],
                         Presence_Cache, Groups_Cache};
                    %%
                    _
                      when Node_Access_Model == 'authorize' ->
                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                    %%
                    [#pubsub_state_dev{affiliation = Affiliation, access = Access}]
                      when    Node_Access_Model == 'whitelist'
                      andalso (Access == 'whitelist'
                                   orelse
                              (Affiliation =/= 'none'
                                   andalso
                               Affiliation =/= 'outcast'
                                   andalso
                               Affiliation =/= 'publish-only')) ->
                        {[exmpp_pubsub:xmlel_item('disco#items', NodeId,
                              Pubsub_Host,
                              get_value(Node_Options, 'title', undefined))
                         |Xmlels_Item],
                         Presence_Cache, Groups_Cache};
                    %%
                    _ ->
                        {Xmlels_Item, Presence_Cache, Groups_Cache}
            end
    end).

%%
-spec(disco_items/10 ::
(
  Pubsub_Items           :: [] | mod_pubsub_dev:pubsub_items(),
  Host                   :: xmpp_jid:raw_jid_component_bare(),
  Pubsub_Host            :: exmpp_pubsub:host_pubsub(),
  Node_Access_Model      :: pubsub_options:access_model(),
  Rosters_Groups_Allowed :: pubsub_options:rosters_groups_allowed(),
  Node_Owners            :: mod_pubsub_dev:node_owners(),
  Entity                 :: xmpp_jid:usr_bare(),
  Affiliation            :: 'member' | 'none' | 'owner' | 'publisher',
  Access                 :: undefined|'authorize'|'presence'|'roster'|'whitelist',
  Cache                  :: {
      Xmlels_Items   :: [Xmlel_Item::xmlel()],
      Presence_Cache :: [Entity::xmpp_jid:usr_bare()],
      Groups_Cache   :: [{Entity::xmpp_jid:usr_bare(), Groups::[Group::binary(),...]}]
  })
    -> Xmlels_Items::[Xmlel_Item::xmlel()]
).

disco_items([] = _Pubsub_Items, _Host, _Pubsub_Host, _Node_Access_Model,
  _Rosters_Groups_Allowed, _Node_Owners, _Entity, _Affiliation, _Access,
  {Xmlels_Items, _, _}) ->
    Xmlels_Items;
%%
disco_items([#pubsub_item_dev{id = {ItemId, _}, options = Item_Options} | Pubsub_Items],
  Host, Pubsub_Host, Node_Access_Model, Rosters_Groups_Allowed, Node_Owners, Entity,
  Affiliation, Access,
  {Xmlels_Item, Presence_Cache, Groups_Cache})
  when   Item_Options == []
  orelse Affiliation == 'owner'
  orelse Affiliation == 'publisher' ->
    disco_items(Pubsub_Items, Host, Pubsub_Host, Node_Access_Model,
        Rosters_Groups_Allowed, Node_Owners, Entity, Affiliation, Access,
        {[exmpp_pubsub:xmlel_item('disco#items', ItemId, Pubsub_Host)
         | Xmlels_Item], Presence_Cache, Groups_Cache});
%%
disco_items([#pubsub_item_dev{id = {ItemId, _}, options = Item_Options} | Pubsub_Items],
  Host, Pubsub_Host, Node_Access_Model, Rosters_Groups_Allowed, Node_Owners, Entity,
  Affiliation, Access,
  {Xmlels_Item, Presence_Cache, Groups_Cache}) ->
    disco_items(Pubsub_Items, Host, Pubsub_Host, Node_Access_Model,
        Rosters_Groups_Allowed, Node_Owners, Entity, Affiliation, Access,
        case get_value(Item_Options, 'access_model') of
            Item_Access_Model
              when   Item_Access_Model == 'none'
              orelse Item_Access_Model == 'open'
              orelse (Item_Access_Model =/= 'roster'
                           andalso
                      Item_Access_Model == Node_Access_Model)
              orelse (Item_Access_Model == 'presence'
                           andalso
                      (Node_Access_Model == 'roster'
                              orelse
                       Access == 'presence'
                              orelse
                       Access == 'roster'))
              orelse Item_Access_Model == 'authorize'
              orelse Item_Access_Model == 'whitelist' ->
                {[exmpp_pubsub:xmlel_item('disco#items', ItemId, Pubsub_Host)
                 | Xmlels_Item], Presence_Cache, Groups_Cache};
            %%
            'presence'
              when   Access == 'presence'
              orelse Access == 'roster' ->
                {[exmpp_pubsub:xmlel_item('disco#items', ItemId, Pubsub_Host)
                 | Xmlels_Item], Presence_Cache, Groups_Cache};
            'presence'
              when Presence_Cache == [] ->
                case
                    is_contact_subscribed_to_node_owners(Host, Entity, Node_Owners)
                of
                    false ->
                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                    Node_Owner ->
                        {[exmpp_pubsub:xmlel_item('disco#items', ItemId, Pubsub_Host)
                         |Xmlels_Item],
                         [Node_Owner | Presence_Cache], Groups_Cache}
                end;
            'presence' ->
                case presence_cache(Presence_Cache, Node_Owners) of
                    true ->
                        {[exmpp_pubsub:xmlel_item('disco#items', ItemId, Pubsub_Host)
                         |Xmlels_Item],
                         Presence_Cache, Groups_Cache};
                    false ->
                        case
                            is_contact_subscribed_to_node_owners(Host, Entity,
                                Node_Owners)
                        of
                            false ->
                                {Xmlels_Item, Presence_Cache, Groups_Cache};
                            Node_Owner ->
                                {[exmpp_pubsub:xmlel_item('disco#items', ItemId,
                                      Pubsub_Host)
                                 |Xmlels_Item],
                                 [Node_Owner | Presence_Cache], Groups_Cache}
                        end
                end;
            'roster'
              when Rosters_Groups_Allowed == [] ->
                case get_value(Item_Options, 'roster_groups_allowed') of
                    Item_Roster_Groups_Allowed
                      when   Item_Roster_Groups_Allowed == 'none'
                      orelse Item_Roster_Groups_Allowed == [] ->
                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                    Item_Roster_Groups_Allowed ->
                        case
                            groups_cache(Groups_Cache, Item_Roster_Groups_Allowed)
                       of
                            true ->
                                {[exmpp_pubsub:xmlel_item('disco#items', ItemId,
                                      Pubsub_Host)
                                 |Xmlels_Item],
                                 Presence_Cache, Groups_Cache};
                            false ->
                                case
                                    is_contact_in_allowed_roster_groups(Entity,
                                        Item_Roster_Groups_Allowed)
                                of
                                    false ->
                                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                                    {Node_Owner, Roster_Group} ->
                                        {[exmpp_pubsub:xmlel_item('disco#items',
                                              ItemId, Pubsub_Host)
                                         |Xmlels_Item],
                                          Presence_Cache,
                                          case
                                              lists:keyfind(Node_Owner, 1,
                                                  Groups_Cache)
                                          of
                                              {_, Groups} ->
                                                  lists:keyreplace(Node_Owner, 1,
                                                      Groups_Cache,
                                                      {Node_Owner,
                                                       [Roster_Group | Groups]});
                                              false ->
                                                  [{Node_Owner, [Roster_Group]}
                                                  | Groups_Cache]
                                          end}
                                end
                        end
                end;
            'roster' ->
                case get_value(Item_Options, 'roster_groups_allowed') of
                    Item_Rosters_Groups_Allowed
                      when   Item_Rosters_Groups_Allowed == 'none'
                      orelse Item_Rosters_Groups_Allowed == [] ->
                        case
                            groups_cache(Groups_Cache, Rosters_Groups_Allowed)
                        of
                            true ->
                                {[exmpp_pubsub:xmlel_item('disco#items', ItemId,
                                      Pubsub_Host)
                                 |Xmlels_Item],
                                 Presence_Cache, Groups_Cache};
                            false ->
                                case
                                    is_contact_in_allowed_roster_groups(Entity,
                                        Rosters_Groups_Allowed)
                                of
                                    false ->
                                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                                    {Node_Owner, Roster_Group} ->
                                        {[exmpp_pubsub:xmlel_item('disco#items',
                                              ItemId, Pubsub_Host)
                                         |Xmlels_Item],
                                          Presence_Cache,
                                          case
                                              lists:keyfind(Node_Owner, 1,
                                                  Groups_Cache)
                                          of
                                              {_, Groups} ->
                                                  lists:keyreplace(Node_Owner, 1,
                                                      Groups_Cache,
                                                      {Node_Owner,
                                                       [Roster_Group | Groups]});
                                              false ->
                                                  [{Node_Owner, [Roster_Group]}
                                                  | Groups_Cache]
                                          end}
                                end
                        end;
                    Item_Rosters_Groups_Allowed ->
                        case
                            groups_cache(Groups_Cache, Item_Rosters_Groups_Allowed)
                        of
                            true ->
                                {[exmpp_pubsub:xmlel_item('disco#items', ItemId,
                                      Pubsub_Host)
                                 |Xmlels_Item],
                                 Presence_Cache, Groups_Cache};
                            false ->
                                case
                                    is_contact_in_allowed_roster_groups(Entity,
                                        Item_Rosters_Groups_Allowed)
                                of
                                    false ->
                                        {Xmlels_Item, Presence_Cache, Groups_Cache};
                                    {Node_Owner, Roster_Group} ->
                                        {[exmpp_pubsub:xmlel_item('disco#items',
                                              ItemId, Pubsub_Host)
                                         |Xmlels_Item],
                                          Presence_Cache,
                                          case
                                              lists:keyfind(Node_Owner, 1,
                                                  Groups_Cache)
                                          of
                                              {_, Groups} ->
                                                  lists:keyreplace(Node_Owner, 1,
                                                      Groups_Cache,
                                                      {Node_Owner,
                                                       [Roster_Group | Groups]});
                                              false ->
                                                  [{Node_Owner, [Roster_Group]}
                                                  | Groups_Cache]
                                          end}
                                end
                        end
                end;
            _ ->
                {Xmlels_Item, Presence_Cache, Groups_Cache}
        end).


%%
-spec(presence_cache/2 ::
(
  Presence_Cache :: [Entity::xmpp_jid:usr_bare()],
  Node_Owners    :: [] | [Entity::xmpp_jid:usr_bare(),...])
    -> Presence_Cache::boolean()
).

presence_cache([], _) ->
    false;
%%
presence_cache([Entity | Presence_Cache], Node_Owners) ->
    case lists:member(Entity, Node_Owners) of
        true  -> true;
        false -> presence_cache(Presence_Cache, Node_Owners)
    end.

%%
-spec(groups_cache/2 ::
(
  Groups_Cache   :: [Group_Cache :: {
                         Entity::xmpp_jid:usr_bare(),
                         Groups::[Group::binary(),...]
                     }],
  Rosters_Groups :: [Roster_Groups :: {
                         Entity::xmpp_jid:usr_bare(),
                         Groups::[Group::binary(),...]
                     },...])
    -> Groups_Cache::boolean()
).

groups_cache([], _) ->
    false;
%%
groups_cache([{Entity, Groups} | Groups_Cache], Rosters_Groups) ->
    case lists:keyfind(Entity, 1, Rosters_Groups) of
        {_, Roster_Groups} -> group_cache(Groups, Roster_Groups);
        false              -> groups_cache(Groups_Cache, Rosters_Groups)
    end.

%%
-spec(group_cache/2 ::
(
  Groups        :: [] | [Group::binary(),...],
  Roster_Groups :: [Group::binary(),...])
    -> Group_Cache::boolean()
).

group_cache([], _) ->
    false;
%%
group_cache([Group | Groups], Roster_Groups) ->
    case lists:member(Group, Roster_Groups) of
        true  -> true;
        false -> group_cache(Groups, Roster_Groups)
    end.
