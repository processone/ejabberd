%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_vcard_xupdate_riak).

%% API
-export([init/2, import/2, add_xupdate/3, get_xupdate/2, remove_xupdate/2]).

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

import(_LServer, #vcard_xupdate{} = R) ->
    ejabberd_riak:put(R, vcard_xupdate_schema()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
vcard_xupdate_schema() ->
    {record_info(fields, vcard_xupdate), #vcard_xupdate{}}.
