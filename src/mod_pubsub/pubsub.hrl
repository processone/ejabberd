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
%%%
%%% copyright 2006-2012 ProcessOne
%%%
%%% This file contains pubsub types definition.
%%% ====================================================================

%% -------------------------------
%% Pubsub constants
-define(ERR_EXTENDED(E,C), mod_pubsub:extended_error(E,C)).

%% The actual limit can be configured with mod_pubsub's option max_items_node
-define(MAXITEMS, 10).

%% this is currently a hard limit.
%% Would be nice to have it configurable. 
-define(MAX_PAYLOAD_SIZE, 60000).

%% -------------------------------
%% Pubsub types

%% @type hostPubsub() = string().
%% <p><tt>hostPubsub</tt> is the name of the PubSub service. For example, it can be
%% <tt>"pubsub.localhost"</tt>.</p>

%% @type hostPEP() = {User, Server, Resource}
%%     User     = string()
%%     Server   = string()
%%     Resource = [].
%% <p>For example, it can be :
%% ```{"bob", "example.org", []}'''.</p>

%% @type host() = hostPubsub() | hostPEP().

%% @type nodeId() = binary().
%% <p>A node is defined by a list of its ancestors. The last element is the name
%% of the current node. For example: 
%% ```<<"/home/localhost/user">>'''</p>

%% @type nodeIdx() = integer().

%% @type itemId() = string().

%% @type subId() = string().

%% @type payload() = [#xmlelement{} | #xmlcdata{}].

%% @type stanzaError() = #xmlelement{}.
%% Example: 
%%    ```{xmlelement, "error",
%%        [{"code", Code}, {"type", Type}],
%%        [{xmlelement, Condition, [{"xmlns", ?NS_STANZAS}], []}]}'''
%% @type pubsubIQResponse() = #xmlelement{}.
%% Example:
%%    ```{xmlelement, "pubsub",
%%               [{"xmlns", ?NS_PUBSUB_EVENT}],
%%               [{xmlelement, "affiliations", [],
%%               []}]}'''

%% @type nodeOption() = {Option, Value}
%%    Option = atom()
%%    Value = term().
%% Example:
%% ```{deliver_payloads, true}'''

%% @type nodeType() = string().
%% <p>The <tt>nodeType</tt> is a string containing the name of the PubSub
%% plugin to use to manage a given node. For example, it can be
%% <tt>"flat"</tt>, <tt>"hometree"</tt> or <tt>"blog"</tt>.</p>

%% @type jid() = {jid, User, Server, Resource, LUser, LServer, LResource}
%%    User      = string()
%%    Server    = string()
%%    Resource  = string()
%%    LUser     = string()
%%    LServer   = string()
%%    LResource = string().

%% @type ljid() = {User, Server, Resource}
%%     User     = string()
%%     Server   = string()
%%     Resource = string().

%% @type affiliation() = 'none' | 'owner' | 'publisher' | 'publish-only' | 'member' | 'outcast'.

%% @type subscription() = 'none' | 'pending' | 'unconfigured' | 'subscribed'.

%% @type accessModel() = 'open' | 'presence' | 'roster' | 'authorize' | 'whitelist'.

%% @type pubsubIndex() = {pubsub_index, Index, Last, Free}
%%    Index = atom()
%%    Last  = integer()
%%    Free  = [integer()].
%% internal pubsub index table
-record(pubsub_index,
{
  index,
  last,
  free
}).

%% @type pubsubNode() = {pubsub_node, NodeId, Id, Parents, Type, Owners, Options}
%%    NodeId  = {host() | ljid(), nodeId()}
%%    Id      = nodeIdx()
%%    Parents = [nodeId()]
%%    Type    = nodeType()
%%    Owners  = [ljid()]
%%    Options = [nodeOption()].
%% <p>This is the format of the <tt>nodes</tt> table. The type of the table
%% is: <tt>set</tt>,<tt>ram/disc</tt>.</p>
%% <p>The <tt>Parents</tt> and <tt>type</tt> fields are indexed.</p>
%% <tt>id</tt> can be anything you want.
-record(pubsub_node,
{
  nodeid,
  id,
  parents = [],
  type    = "flat",
  owners  = [],
  options = []
}).

%% @type pubsubState() = {pubsub_state, StateId, Items, Affiliation, Subscriptions}
%%    StateId       = {ljid(), nodeIdx()}
%%    Items         = [itemId()]
%%    Affiliation   = affiliation()
%%    Subscriptions = [{subscription(), subId()}].
%% <p>This is the format of the <tt>affiliations</tt> table. The type of the
%% table is: <tt>set</tt>,<tt>ram/disc</tt>.</p>
-record(pubsub_state,
{
  stateid,
  items         = [],
  affiliation   = 'none',
  subscriptions = []
}).

%% @type pubsubItem() = {pubsub_item, ItemId, Creation, Modification, Payload}
%%    ItemId       = {itemId(), nodeIdx()}
%%    Creation     = {now(), ljid()}
%%    Modification = {now(), ljid()}
%%    Payload      = payload().
%% <p>This is the format of the <tt>published items</tt> table. The type of the
%% table is: <tt>set</tt>,<tt>disc</tt>,<tt>fragmented</tt>.</p>
-record(pubsub_item,
{
  itemid,
  creation     = {'unknown','unknown'},
  modification = {'unknown','unknown'},
  payload      = []
}).

%% @type pubsubSubscription() = {pubsub_subscription, SubId, Options}
%%     SubId     = subId()
%%     Options   = [nodeOption()].
%% <p>This is the format of the <tt>subscriptions</tt> table. The type of the
%% table is: <tt>set</tt>,<tt>ram/disc</tt>.</p>
-record(pubsub_subscription,
{
  subid,
  options
}).

%% @type pubsubLastItem() = {pubsub_last_item, NodeId, ItemId, Creation, Payload}
%%    NodeId   = nodeIdx()
%%    ItemId   = itemId()
%%    Creation = {now(),ljid()}
%%    Payload  = payload().
%% <p>This is the format of the <tt>last items</tt> table. it stores last item payload
%% for every node</p>
-record(pubsub_last_item,
{
  nodeid,
  itemid,
  creation,
  payload
}).
