%%%-------------------------------------------------------------------
%%% Created :  1 Dec 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-module(mod_mix_mnesia).
-behaviour(mod_mix).

%% API
-export([init/2]).
-export([set_channel/6, get_channels/2, get_channel/3, del_channel/3]).
-export([set_participant/6, get_participant/4, get_participants/3, del_participant/4]).
-export([subscribe/5, unsubscribe/4, unsubscribe/5, get_subscribed/4]).

-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

-record(mix_channel,
	{chan_serv :: {binary(), binary()},
	 service :: binary(),
	 creator :: jid:jid(),
	 hidden  :: boolean(),
	 hmac_key :: binary(),
	 created_at :: erlang:timestamp()}).

-record(mix_participant,
	{user_chan :: {binary(), binary(), binary(), binary()},
	 chan_serv :: {binary(), binary()},
	 jid :: jid:jid(),
	 id :: binary(),
	 nick :: binary(),
	 created_at :: erlang:timestamp()}).

-record(mix_subscription,
	{user_chan_node :: {binary(), binary(), binary(), binary(), binary()},
	 user_chan :: {binary(), binary(), binary(), binary()},
	 chan_serv_node :: {binary(), binary(), binary()},
	 chan_serv :: {binary(), binary()},
	 jid :: jid:jid()}).

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    try
	{atomic, _} = ejabberd_mnesia:create(
			?MODULE, mix_channel,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, mix_channel)},
			 {index, [service]}]),
	{atomic, _} = ejabberd_mnesia:create(
			?MODULE, mix_participant,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, mix_participant)},
			 {index, [chan_serv]}]),
	{atomic, _} = ejabberd_mnesia:create(
			?MODULE, mix_subscription,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, mix_subscription)},
			 {index, [user_chan, chan_serv_node, chan_serv]}]),
	ok
    catch _:{badmatch, _} ->
	    {error, db_failure}
    end.

set_channel(_LServer, Channel, Service, CreatorJID, Hidden, Key) ->
    mnesia:dirty_write(
      #mix_channel{chan_serv = {Channel, Service},
		   service = Service,
		   creator = jid:remove_resource(CreatorJID),
		   hidden = Hidden,
		   hmac_key = Key,
		   created_at = erlang:timestamp()}).

get_channels(_LServer, Service) ->
    Ret = mnesia:dirty_index_read(mix_channel, Service, #mix_channel.service),
    {ok, lists:filtermap(
	   fun(#mix_channel{chan_serv = {Channel, _},
			    hidden = false}) ->
		   {true, Channel};
	      (_) ->
		   false
	   end, Ret)}.

get_channel(_LServer, Channel, Service) ->
    case mnesia:dirty_read(mix_channel, {Channel, Service}) of
	[#mix_channel{creator = JID,
		      hidden = Hidden,
		      hmac_key = Key}] ->
	    {ok, {JID, Hidden, Key}};
	[] ->
	    {error, notfound}
    end.

del_channel(_LServer, Channel, Service) ->
    Key = {Channel, Service},
    L1 = mnesia:dirty_read(mix_channel, Key),
    L2 = mnesia:dirty_index_read(mix_participant, Key,
				 #mix_participant.chan_serv),
    L3 = mnesia:dirty_index_read(mix_subscription, Key,
				 #mix_subscription.chan_serv),
    lists:foreach(fun mnesia:dirty_delete_object/1, L1++L2++L3).

set_participant(_LServer, Channel, Service, JID, ID, Nick) ->
    {User, Domain, _} = jid:tolower(JID),
    mnesia:dirty_write(
      #mix_participant{
	 user_chan = {User, Domain, Channel, Service},
	 chan_serv = {Channel, Service},
	 jid = jid:remove_resource(JID),
	 id = ID,
	 nick = Nick,
	 created_at = erlang:timestamp()}).

get_participant(_LServer, Channel, Service, JID) ->
    {User, Domain, _} = jid:tolower(JID),
    case mnesia:dirty_read(mix_participant, {User, Domain, Channel, Service}) of
	[#mix_participant{id = ID, nick = Nick}] -> {ok, {ID, Nick}};
	[] -> {error, notfound}
    end.

get_participants(_LServer, Channel, Service) ->
    Ret = mnesia:dirty_index_read(mix_participant,
				  {Channel, Service},
				  #mix_participant.chan_serv),
    {ok, lists:map(
	   fun(#mix_participant{jid = JID, id = ID, nick = Nick}) ->
		   {JID, ID, Nick}
	   end, Ret)}.

del_participant(_LServer, Channel, Service, JID) ->
    {User, Domain, _} = jid:tolower(JID),
    mnesia:dirty_delete(mix_participant, {User, Domain, Channel, Service}).

subscribe(_LServer, Channel, Service, JID, Nodes) ->
    {User, Domain, _} = jid:tolower(JID),
    BJID = jid:remove_resource(JID),
    lists:foreach(
      fun(Node) ->
	      mnesia:dirty_write(
		#mix_subscription{
		   user_chan_node = {User, Domain, Channel, Service, Node},
		   user_chan = {User, Domain, Channel, Service},
		   chan_serv_node = {Channel, Service, Node},
		   chan_serv = {Channel, Service},
		   jid = BJID})
      end, Nodes).

get_subscribed(_LServer, Channel, Service, Node) ->
    Ret = mnesia:dirty_index_read(mix_subscription,
				  {Channel, Service, Node},
				  #mix_subscription.chan_serv_node),
    {ok, [JID || #mix_subscription{jid = JID} <- Ret]}.

unsubscribe(_LServer, Channel, Service, JID) ->
    {User, Domain, _} = jid:tolower(JID),
    Ret = mnesia:dirty_index_read(mix_subscription,
				  {User, Domain, Channel, Service},
				  #mix_subscription.user_chan),
    lists:foreach(fun mnesia:dirty_delete_object/1, Ret).

unsubscribe(_LServer, Channel, Service, JID, Nodes) ->
    {User, Domain, _} = jid:tolower(JID),
    lists:foreach(
      fun(Node) ->
	      mnesia:dirty_delete(mix_subscription,
				  {User, Domain, Channel, Service, Node})
      end, Nodes).

%%%===================================================================
%%% Internal functions
%%%===================================================================
