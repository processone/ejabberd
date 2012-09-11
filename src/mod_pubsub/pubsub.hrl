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

-define(ERR_EXTENDED(E, C),
	mod_pubsub:extended_error(E, C)).

-define(MAXITEMS, 10).

-define(MAX_PAYLOAD_SIZE, 60000).

%% -------------------------------
%% Pubsub types

-type(hostPubsub() :: binary()).
%% <p><tt>hostPubsub</tt> is the name of the PubSub service. For example, it can be
%% <tt>"pubsub.localhost"</tt>.</p>

-type(hostPEP() :: {binary(), binary(), <<>>}).
%% @type hostPEP() = {User, Server, Resource}
%%     User     = string()
%%     Server   = string()
%%     Resource = [].
%% <p>For example, it can be :
%% ```{"bob", "example.org", []}'''.</p>

-type(host() :: hostPubsub() | hostPEP()).
%% @type host() = hostPubsub() | hostPEP().

-type(nodeId() :: binary()).
%% @type nodeId() = binary().
%% <p>A node is defined by a list of its ancestors. The last element is the name
%% of the current node. For example:
%% ```<<"/home/localhost/user">>'''</p>

-type(nodeIdx() :: pos_integer()).
%% @type nodeIdx() = integer().

-type(itemId() :: binary()).
%% @type itemId() = string().

-type(subId() :: binary()).
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

-type(nodeOption() ::
    {Option::atom(),
     Value::binary() | [binary()] | boolean() | non_neg_integer()
}).

-type(nodeOptions() :: [NodeOption::mod_pubsub:nodeOption(),...]).

%% @type nodeOption() = {Option, Value}
%%    Option = atom()
%%    Value = term().
%% Example:
%% ```{deliver_payloads, true}'''

-type(subOption() ::
    {Option::atom(),
     Value::binary() | [binary()] | boolean()
}).

-type(subOptions() :: [SubOption::mod_pubsub:subOption(),...]).

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

%-type(ljid() :: {binary(), binary(), binary()}).
%% @type ljid() = {User, Server, Resource}
%%     User     = string()
%%     Server   = string()
%%     Resource = string().

-type(affiliation() :: 'none'
                     | 'owner'
                     | 'publisher'
                    %| 'publish-only'
                     | 'member'
                     | 'outcast'
).
%% @type affiliation() = 'none' | 'owner' | 'publisher' | 'publish-only' | 'member' | 'outcast'.

-type(subscription() :: 'none'
                      | 'pending'
                      | 'unconfigured'
                      | 'subscribed'
).
%% @type subscription() = 'none' | 'pending' | 'unconfigured' | 'subscribed'.

-type(accessModel() :: 'open'
                     | 'presence'
                     | 'roster'
                     | 'authorize'
                     | 'whitelist'
).
%% @type accessModel() = 'open' | 'presence' | 'roster' | 'authorize' | 'whitelist'.

-type(publishModel() :: 'publishers'
                      | 'subscribers'
                      | 'open'
).


-record(pubsub_index,
{
    index :: atom(),
    last  :: mod_pubsub:nodeIdx(),
    free  :: [mod_pubsub:nodeIdx()]
}).

-record(pubsub_node,
{
    nodeid               ,%:: {Host::mod_pubsub:host(), NodeId::mod_pubsub:nodeId()},
    id                   ,%:: mod_pubsub:nodeIdx(),
    parents = []         ,%:: [Parent_NodeId::mod_pubsub:nodeId()],
    type    = <<"flat">> ,%:: binary(),
    owners  = []         ,%:: [Owner::ljid(),...],
    options = []          %:: mod_pubsub:nodeOptions()
}).


%-record(pubsub_state,
%	{stateid, nodeidx, items = [], affiliation = none,
%	 subscriptions = []}).
-record(pubsub_state,
{
    stateid                ,%:: {Entity::ljid(), NodeIdx::mod_pubsub:nodeIdx()},
    nodeidx                ,%:: mod_pubsub:nodeIdx(),
    items         = []     ,%:: [ItemId::mod_pubsub:itemId()],
    affiliation   = 'none' ,%:: mod_pubsub:affiliation(),
    subscriptions = []      %:: [{mod_pubsub:subscription(), mod_pubsub:subId()}]
}).

%-record(pubsub_item,
%	{itemid, nodeidx, creation = {unknown, unknown},
%	 modification = {unknown, unknown}, payload = []}).

-record(pubsub_item,
{
    itemid                            ,%:: {mod_pubsub:itemId(), mod_pubsub:nodeIdx()},
    nodeidx                           ,%:: mod_pubsub_nodeIdx(),
    creation     = {unknown, unknown} ,%:: {erlang:timestamp(), ljid()},
    modification = {unknown, unknown} ,%:: {erlang:timestamp(), ljid()},
    payload      = []                  %:: mod_pubsub:payload()
}).

%-record(pubsub_subscription, {subid, options}).
-record(pubsub_subscription,
{
    subid   ,%:: mod_pubsub:subId(),
    options  %:: [] | mod_pubsub:subOptions()
}).

%-record(pubsub_last_item,
%	{nodeid, itemid, creation, payload}).

-record(pubsub_last_item,
{
    nodeid   ,%:: mod_pubsub:nodeIdx(),
    itemid   ,%:: mod_pubsub:itemId(),
    creation ,%:: {erlang:timestamp(), ljid()},
    payload   %:: mod_pubsub:payload()
}).
