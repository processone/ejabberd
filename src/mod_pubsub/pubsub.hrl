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
%%% Portions created by ProcessOne are Copyright 2006-2009, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2009, ProcessOne.
%%%
%%%
%%% copyright 2006-2009 ProcessOne
%%%
%%% This file contains pubsub types definition.
%%% ====================================================================

%% -------------------------------
%% Pubsub constants
-define(ERR_EXTENDED(E,C), mod_pubsub:extended_error(E,C)).

%% this is currently a hard limit.
%% Would be nice to have it configurable. 
-define(MAXITEMS, 20).
-define(MAX_PAYLOAD_SIZE, 60000).

%% -------------------------------
%% Pubsub types

%%% @type host() = string().
%%% <p><tt>host</tt> is the name of the PubSub service. For example, it can be
%%% <tt>"pubsub.localhost"</tt>.</p>

%%% @type pubsubNode() = [string()]. 
%%% <p>A node is defined by a list of its ancestors. The last element is the name
%%% of the current node. For example: 
%%% ```["home", "localhost", "cromain", "node1"]'''</p>

%%% @type stanzaError() = #xmlelement{}.
%%% Example: 
%%%    ```{xmlelement, "error",
%%%        [{"code", Code}, {"type", Type}],
%%%        [{xmlelement, Condition, [{"xmlns", ?NS_STANZAS}], []}]}'''

%%% @type pubsubIQResponse() = #xmlelement{}.
%%% Example:
%%%    ```{xmlelement, "pubsub",
%%%               [{"xmlns", ?NS_PUBSUB_EVENT}],
%%%               [{xmlelement, "affiliations", [],
%%%               []}]}'''

%%% @type nodeOption() = {Option::atom(), Value::term()}.
%%% Example:
%%% ```{deliver_payloads, true}'''

%%% @type nodeType() = string().
%%% <p>The <tt>nodeType</tt> is a string containing the name of the PubSub
%%% plugin to use to manage a given node. For example, it can be
%%% <tt>"flat"</tt>, <tt>"hometree"</tt> or <tt>"blog"</tt>.</p>

%%% @type jid() = #jid{
%%%    user = string(),
%%%    server = string(),
%%%    resource = string(),
%%%    luser = string(),
%%%    lserver = string(),
%%%    lresource = string()}.

%%% @type ljid() = {User::string(), Server::string(), Resource::string()}.

%%% @type affiliation() = none | owner | publisher | outcast.
%%% @type subscription() = none | pending | unconfigured | subscribed.

%%% internal pubsub index table
-record(pubsub_index, {index, last, free}).

%%% @type pubsubNode() = #pubsub_node{
%%%    nodeid = {Host::host(), Node::pubsubNode()},
%%%    parentid = Node::pubsubNode(),
%%%    nodeidx = int(). % can be anything you want
%%%    type = nodeType(),
%%%    options = [nodeOption()]}
%%% <p>This is the format of the <tt>nodes</tt> table. The type of the table
%%% is: <tt>set</tt>,<tt>ram/disc</tt>.</p>
%%% <p>The <tt>parentid</tt> and <tt>type</tt> fields are indexed.</p>
-record(pubsub_node, {nodeid,
		      id,
		      parents = [],
		      type = "flat",
		      owners = [],
		      options = []
		     }).

%%% @type pubsubState() = #pubsub_state{
%%%    stateid = {ljid(), nodeidx()}},
%%%    items = [ItemId::string()],
%%%    affiliation = affiliation(),
%%%    subscriptions = [subscription()]}.
%%% <p>This is the format of the <tt>affiliations</tt> table. The type of the
%%% table is: <tt>set</tt>,<tt>ram/disc</tt>.</p>
-record(pubsub_state, {stateid,
		       items = [],
		       affiliation = none,
		       subscriptions = []
}).

%%% @type pubsubItem() = #pubsub_item{
%%%    itemid = {ItemId::string(), nodeidx()}},
%%%    creation = {now(), ljid()},
%%%    modification = {now(), ljid()},
%%%    payload = XMLContent::string()}.
%%% <p>This is the format of the <tt>published items</tt> table. The type of the
%%% table is: <tt>set</tt>,<tt>disc</tt>,<tt>fragmented</tt>.</p>
-record(pubsub_item, {itemid,
		      creation = {unknown,unknown},
		      modification = {unknown,unknown},
		      payload = []
		     }).

%% @type pubsubSubscription() = #pubsub_subscription{
%%     subid     = string(),
%%     state_key = {ljid(), pubsubNodeId()},
%%     options   = [{atom(), term()}]
%% }.
%% <p>This is the format of the <tt>subscriptions</tt> table. The type of the
%% table is: <tt>set</tt>,<tt>ram/disc</tt>.</p>
-record(pubsub_subscription, {subid, options}).
