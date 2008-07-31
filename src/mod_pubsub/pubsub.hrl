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
%%% Portions created by ProcessOne are Copyright 2006-2008, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2008, ProcessOne.
%%%
%%%
%%% copyright 2006-2008 ProcessOne
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
%%% <tt>"default"</tt>, <tt>"collection"</tt> or <tt>"blog"</tt>.</p>

%%% @type jid() = #jid{
%%%    user = string(),
%%%    server = string(),
%%%    resource = string(),
%%%    luser = string(),
%%%    lserver = string(),
%%%    lresource = string()}.

%%% @type usr() = {User::string(), Server::string(), Resource::string()}.

%%% @type affiliation() = none | owner | publisher | outcast.
%%% @type subscription() = none | pending | unconfigured | subscribed.

%%% @type pubsubNode() = #pubsub_node{
%%%    nodeid = {Host::host(), Node::pubsubNode()},
%%%    parentid = {Host::host(), Node::pubsubNode()},
%%%    type = nodeType(),
%%%    owners = [usr()],
%%%    options = [nodeOption()]}.
%%% <p>This is the format of the <tt>nodes</tt> table. The type of the table
%%% is: <tt>set</tt>,<tt>ram/disc</tt>.</p>
%%% <p>The <tt>parentid</tt> and <tt>type</tt> fields are indexed.</p>
-record(pubsub_node, {nodeid,
		      parentid = {},
		      type = "",
		      owners = [],
		      options = []
		     }).

%%% @type pubsubState() = #pubsub_state{
%%%    stateid = {jid(), {Host::host(), Node::pubsubNode()}},
%%%    items = [ItemId::string()],
%%%    affiliation = affiliation(),
%%%    subscription = subscription()}.
%%% <p>This is the format of the <tt>affiliations</tt> table. The type of the
%%% table is: <tt>set</tt>,<tt>ram/disc</tt>.</p>
-record(pubsub_state, {stateid,
		       items = [],
		       affiliation = none,
		       subscription = none
}).

%% @type pubsubItem() = #pubsub_item{
%%        itemid = {ItemId::string(), {Host::host(),Node::pubsubNode()}},
%%     creation = {JID::jid(), now()},
%%     modification = {JID::jid(), now()},
%%     payload = XMLContent::string()}.
%%% <p>This is the format of the <tt>published items</tt> table. The type of the
%%% table is: <tt>set</tt>,<tt>disc</tt>,<tt>fragmented</tt>.</p>
-record(pubsub_item, {itemid,
		      creation = {unknown,unknown},
		      modification = {unknown,unknown},
		      payload = []
		     }).


%% @type pubsubPresence() = #pubsub_presence{
%%     key = {Host::host(), User::string(), Server::string()},
%%     presence = list()}.
%%% <p>This is the format of the <tt>published presence</tt> table. The type of the
%%% table is: <tt>set</tt>,<tt>ram</tt>.</p>
-record(pubsub_presence, {key,
			  resource
			 }).

