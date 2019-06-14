%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  4 Dec 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(mod_mix_pam_mnesia).
-behaviour(mod_mix_pam).

%% API
-export([init/2, add_channel/3, get_channel/2,
	 get_channels/1, del_channel/2, del_channels/1,
	 use_cache/1]).

-record(mix_pam, {user_channel :: {binary(), binary(), binary(), binary()},
		  user :: {binary(), binary()},
		  id :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    case ejabberd_mnesia:create(?MODULE, mix_pam,
				[{disc_only_copies, [node()]},
				 {attributes, record_info(fields, mix_pam)},
				 {index, [user]}]) of
	{atomic, _} -> ok;
	_ -> {error, db_failure}
    end.

use_cache(Host) ->
    case mnesia:table_info(mix_pam, storage_type) of
        disc_only_copies ->
            mod_mix_pam_opt:use_cache(Host);
        _ ->
            false
    end.

add_channel(User, Channel, ID) ->
    {LUser, LServer, _} = jid:tolower(User),
    {Chan, Service, _} = jid:tolower(Channel),
    mnesia:dirty_write(#mix_pam{user_channel = {LUser, LServer, Chan, Service},
				user = {LUser, LServer},
				id = ID}).

get_channel(User, Channel) ->
    {LUser, LServer, _} = jid:tolower(User),
    {Chan, Service, _} = jid:tolower(Channel),
    case mnesia:dirty_read(mix_pam, {LUser, LServer, Chan, Service}) of
	[#mix_pam{id = ID}] -> {ok, ID};
	[] -> {error, notfound}
    end.

get_channels(User) ->
    {LUser, LServer, _} = jid:tolower(User),
    Ret = mnesia:dirty_index_read(mix_pam, {LUser, LServer}, #mix_pam.user),
    {ok, lists:map(
	   fun(#mix_pam{user_channel = {_, _, Chan, Service},
			id = ID}) ->
		   {jid:make(Chan, Service), ID}
	   end, Ret)}.

del_channel(User, Channel) ->
    {LUser, LServer, _} = jid:tolower(User),
    {Chan, Service, _} = jid:tolower(Channel),
    mnesia:dirty_delete(mix_pam, {LUser, LServer, Chan, Service}).

del_channels(User) ->
    {LUser, LServer, _} = jid:tolower(User),
    Ret = mnesia:dirty_index_read(mix_pam, {LUser, LServer}, #mix_pam.user),
    lists:foreach(fun mnesia:dirty_delete_object/1, Ret).

%%%===================================================================
%%% Internal functions
%%%===================================================================
