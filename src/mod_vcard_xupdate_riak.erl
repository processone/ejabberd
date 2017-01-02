%%%-------------------------------------------------------------------
%%% File    : mod_vcard_xupdate_riak.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
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

-module(mod_vcard_xupdate_riak).

-behaviour(mod_vcard_xupdate).

%% API
-export([init/2, import/3, add_xupdate/3, get_xupdate/2, remove_xupdate/2]).

-include("mod_vcard_xupdate.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

add_xupdate(LUser, LServer, Hash) ->
    {atomic, ejabberd_riak:put(#vcard_xupdate{us = {LUser, LServer},
                                              hash = Hash},
			       vcard_xupdate_schema())}.

get_xupdate(LUser, LServer) ->
    case ejabberd_riak:get(vcard_xupdate, vcard_xupdate_schema(),
			   {LUser, LServer}) of
        {ok, #vcard_xupdate{hash = Hash}} -> Hash;
        _ -> undefined
    end.

remove_xupdate(LUser, LServer) ->
    {atomic, ejabberd_riak:delete(vcard_xupdate, {LUser, LServer})}.

import(LServer, <<"vcard_xupdate">>, [LUser, Hash, _TimeStamp]) ->
    ejabberd_riak:put(
      #vcard_xupdate{us = {LUser, LServer}, hash = Hash},
      vcard_xupdate_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
vcard_xupdate_schema() ->
    {record_info(fields, vcard_xupdate), #vcard_xupdate{}}.
