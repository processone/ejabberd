%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-include("ejabberd.hrl").

%% -------------------------------
%% Pubsub constants
-define(ERR_EXTENDED(E, C), mod_pubsub:extended_error(E, C)).

%% The actual limit can be configured with mod_pubsub's option max_items_node
-define(MAXITEMS, 10).

%% this is currently a hard limit.
%% Would be nice to have it configurable. 
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

-type(nodeIdx() :: pos_integer() | binary()).
%% @type nodeIdx() = integer() | binary().
%% note: pos_integer() should always be used, but we allow anything else coded
%% as binary, so one can have a custom implementation of nodetree with custom
%% indexing (see nodetree_virtual). this also allows to use any kind of key for
%% indexing nodes, as this can be usefull with external backends such as sql.

-type(itemId() :: binary()).
%% @type itemId() = string().

-type(subId() :: binary()).
%% @type subId() = string().

-type(nodeOption() ::
    {Option::atom(),
     Value::atom() | [binary()] | boolean() | non_neg_integer()
}).

-type(nodeOptions() :: [mod_pubsub:nodeOption(),...]).

%% @type nodeOption() = {Option, Value}
%%    Option = atom()
%%    Value = term().
%% Example:
%% ```{deliver_payloads, true}'''

-type(subOption() ::
    {Option::atom(),
     Value::binary() | [binary()] | boolean()
}).

-type(subOptions() :: [mod_pubsub:subOption(),...]).

-type(pubOption() ::
    {Option::binary(),
     Values::[binary()]
}).

-type(pubOptions() :: [mod_pubsub:pubOption()]).

-type(affiliation() :: 'none'
                     | 'owner'
                     | 'publisher'
                     | 'publish_only'
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
%% @type publishModel() = 'publishers' | 'subscribers' | 'open'

-record(pubsub_index,
{
    index :: atom(),
    last  :: mod_pubsub:nodeIdx(),
    free  :: [mod_pubsub:nodeIdx()]
}).

-record(pubsub_node,
{
    nodeid              ,% :: {mod_pubsub:host(), mod_pubsub:nodeId()},
    id                  ,% :: mod_pubsub:nodeIdx(),
    parents = []        ,% :: [mod_pubsub:nodeId(),...],
    type    = <<"flat">>,% :: binary(),
    owners  = []        ,% :: [jlib:ljid(),...],
    options = []        % :: mod_pubsub:nodeOptions()
}).

-record(pubsub_state,
{
    stateid               ,% :: {jlib:ljid(), mod_pubsub:nodeIdx()},
    nodeidx               ,% :: mod_pubsub:nodeIdx(),
    items         = []    ,% :: [mod_pubsub:itemId(),...],
    affiliation   = 'none',% :: mod_pubsub:affiliation(),
    subscriptions = []    % :: [{mod_pubsub:subscription(), mod_pubsub:subId()}]
}).

-record(pubsub_item,
{
    itemid                           ,% :: {mod_pubsub:itemId(), mod_pubsub:nodeIdx()},
    nodeidx                          ,% :: mod_pubsub:nodeIdx(),
    creation     = {unknown, unknown},% :: {erlang:timestamp(), jlib:ljid()},
    modification = {unknown, unknown},% :: {erlang:timestamp(), jlib:ljid()},
    payload      = []                % :: mod_pubsub:payload()
}).

-record(pubsub_subscription,
{
    subid        ,% :: mod_pubsub:subId(),
    options = [] % :: mod_pubsub:subOptions()
}).

-record(pubsub_last_item,
{
    nodeid   ,% :: {binary(), mod_pubsub:nodeIdx()},
    itemid   ,% :: mod_pubsub:itemId(),
    creation ,% :: {erlang:timestamp(), jlib:ljid()},
    payload  % :: mod_pubsub:payload()
}).

-record(pubsub_orphan,
{
    nodeid    ,% :: mod_pubsub:nodeIdx(),
    items = [] % :: list()
}).
