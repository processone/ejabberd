%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for private storage.
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-module(mod_private).

-author('alexey@process-one.net').

-protocol({xep, 49, '1.2'}).
-protocol({xep, 411, '0.2.0', '18.12', "", ""}).
-protocol({xep, 402, '1.1.3', '23.10', "", ""}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_sm_iq/1, import_info/0,
	 remove_user/2, get_data/2, get_data/3, export/1, mod_doc/0,
	 import/5, import_start/2, mod_opt_type/1, set_data/2,
	 mod_options/1, depends/2, get_sm_features/5, pubsub_publish_item/6,
	 pubsub_delete_item/5, pubsub_tree_call/4]).

-export([get_commands_spec/0, bookmarks_to_pep/2]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("mod_private.hrl").
-include("ejabberd_commands.hrl").
-include("translate.hrl").
-include("pubsub.hrl").

-define(PRIVATE_CACHE, private_cache).

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), [binary()]) -> ok.
-callback set_data(binary(), binary(), [{binary(), xmlel()}]) -> ok | {error, any()}.
-callback get_data(binary(), binary(), binary()) -> {ok, xmlel()} | error | {error, any()}.
-callback get_all_data(binary(), binary()) -> {ok, [xmlel()]} | error | {error, any()}.
-callback del_data(binary(), binary()) -> ok | {error, any()}.
-callback use_cache(binary()) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-optional_callbacks([use_cache/1, cache_nodes/1]).

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    Mod:init(Host, Opts),
    init_cache(Mod, Host, Opts),
    ejabberd_commands:register_commands(?MODULE, get_commands_spec()),
    {ok, [{hook, remove_user, remove_user, 50},
          {hook, disco_sm_features, get_sm_features, 50},
          {hook, pubsub_publish_item, pubsub_publish_item, 50},
          {hook, pubsub_delete_item, pubsub_delete_item, 50},
	  {hook, pubsub_tree_call, pubsub_tree_call, 50},
          {iq_handler, ejabberd_sm, ?NS_PRIVATE, process_sm_iq}]}.

stop(Host) ->
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	false ->
	    ejabberd_commands:unregister_commands(get_commands_spec());
	true ->
	    ok
    end.

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    init_cache(NewMod, Host, NewOpts).

depends(_Host, _Opts) ->
    [{mod_pubsub, soft}].

mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(use_cache) ->
    econf:bool();
mod_opt_type(cache_size) ->
    econf:pos_int(infinity);
mod_opt_type(cache_missed) ->
    econf:bool();
mod_opt_type(cache_life_time) ->
    econf:timeout(second, infinity).

mod_options(Host) ->
    [{db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {use_cache, ejabberd_option:use_cache(Host)},
     {cache_size, ejabberd_option:cache_size(Host)},
     {cache_missed, ejabberd_option:cache_missed(Host)},
     {cache_life_time, ejabberd_option:cache_life_time(Host)}].

mod_doc() ->
    #{desc =>
          [?T("This module adds support for "
              "https://xmpp.org/extensions/xep-0049.html"
              "[XEP-0049: Private XML Storage]."), "",
           ?T("Using this method, XMPP entities can store "
              "private data on the server, retrieve it "
              "whenever necessary and share it between multiple "
              "connected clients of the same user. The data stored "
              "might be anything, as long as it is a valid XML. "
              "One typical usage is storing a bookmark of all user's conferences "
              "(https://xmpp.org/extensions/xep-0048.html"
              "[XEP-0048: Bookmarks])."), "",
           ?T("It also implements the bookmark conversion described in "
              "https://xmpp.org/extensions/xep-0402.html[XEP-0402: PEP Native Bookmarks]"
              ", see the command "
              "https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#bookmarks-to-pep[bookmarks_to_pep].")],
      opts =>
          [{db_type,
            #{value => "mnesia | sql",
              desc =>
                  ?T("Same as top-level _`default_db`_ option, but applied to this module only.")}},
           {use_cache,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`use_cache`_ option, but applied to this module only.")}},
           {cache_size,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Same as top-level _`cache_size`_ option, but applied to this module only.")}},
           {cache_missed,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`cache_missed`_ option, but applied to this module only.")}},
           {cache_life_time,
            #{value => "timeout()",
              desc =>
                  ?T("Same as top-level _`cache_life_time`_ option, but applied to this module only.")}}]}.

-spec get_sm_features({error, stanza_error()} | empty | {result, [binary()]},
		      jid(), jid(), binary(), binary()) ->
			     {error, stanza_error()} | empty | {result, [binary()]}.
get_sm_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_sm_features(Acc, _From, To, <<"">>, _Lang) ->
    case gen_mod:is_loaded(To#jid.lserver, mod_pubsub) of
	true ->
	    {result, [?NS_BOOKMARKS_CONVERSION_0, ?NS_PEP_BOOKMARKS_COMPAT, ?NS_PEP_BOOKMARKS_COMPAT_PEP |
		      case Acc of
			  {result, Features} -> Features;
			  empty -> []
		      end]};
	false ->
	    Acc
    end;
get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec process_sm_iq(iq()) -> iq().
process_sm_iq(#iq{type = Type, lang = Lang,
		  from = #jid{luser = LUser, lserver = LServer} = From,
		  to = #jid{luser = LUser, lserver = LServer},
		  sub_els = [#private{sub_els = Els0}]} = IQ) ->
    case filter_xmlels(Els0) of
	[] ->
	    Txt = ?T("No private data found in this query"),
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	Data when Type == set ->
	    case set_data(From, Data) of
		ok ->
		    xmpp:make_iq_result(IQ);
		{error, #stanza_error{} = Err} ->
		    xmpp:make_error(IQ, Err);
		{error, _} ->
		    Txt = ?T("Database failure"),
		    Err = xmpp:err_internal_server_error(Txt, Lang),
		    xmpp:make_error(IQ, Err)
	    end;
	Data when Type == get ->
	    case get_data(LUser, LServer, Data) of
		{error, _} ->
		    Txt = ?T("Database failure"),
		    Err = xmpp:err_internal_server_error(Txt, Lang),
		    xmpp:make_error(IQ, Err);
		Els ->
		    xmpp:make_iq_result(IQ, #private{sub_els = Els})
	    end
    end;
process_sm_iq(#iq{lang = Lang} = IQ) ->
    Txt = ?T("Query to another users is forbidden"),
    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang)).

-spec filter_xmlels([xmlel()]) -> [{binary(), xmlel()}].
filter_xmlels(Els) ->
    lists:flatmap(
      fun(#xmlel{} = El) ->
	      case fxml:get_tag_attr_s(<<"xmlns">>, El) of
		  <<"">> -> [];
		  NS -> [{NS, El}]
	      end
      end, Els).

-spec set_data(jid(), [{binary(), xmlel()}]) -> ok | {error, _}.
set_data(JID, Data) ->
    set_data(JID, Data, true, true).

-spec set_data(jid(), [{binary(), xmlel()}], boolean(), boolean()) -> ok | {error, _}.
set_data(JID, Data, PublishPepStorageBookmarks, PublishPepXmppBookmarks) ->
    {LUser, LServer, _} = jid:tolower(JID),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:set_data(LUser, LServer, Data) of
	ok ->
	    delete_cache(Mod, LUser, LServer, Data),
	    case PublishPepStorageBookmarks of
		true -> publish_pep_storage_bookmarks(JID, Data);
		false -> ok
	    end,
	    case PublishPepXmppBookmarks of
		true -> publish_pep_native_bookmarks(JID, Data);
		false -> ok
	    end;
	{error, _} = Err ->
	    Err
    end.

-spec get_data(binary(), binary(), [{binary(), xmlel()}]) -> [xmlel()] | {error, _}.
get_data(LUser, LServer, Data) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    lists:foldr(
      fun(_, {error, _} = Err) ->
	      Err;
	 ({NS, El}, Els) ->
	      Res = case use_cache(Mod, LServer) of
			true ->
			    ets_cache:lookup(
			      ?PRIVATE_CACHE, {LUser, LServer, NS},
			      fun() -> Mod:get_data(LUser, LServer, NS) end);
			false ->
			    Mod:get_data(LUser, LServer, NS)
		    end,
	      case Res of
		  {ok, StorageEl} ->
		      [StorageEl|Els];
		  error ->
		      [El|Els];
		  {error, _} = Err ->
		      Err
	      end
      end, [], Data).

-spec get_data(binary(), binary()) -> [xmlel()] | {error, _}.
get_data(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:get_all_data(LUser, LServer) of
	{ok, Els} -> Els;
	error -> [];
	{error, _} = Err -> Err
    end.

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(Server, ?MODULE),
    Data = case use_cache(Mod, LServer) of
	       true ->
		   case Mod:get_all_data(LUser, LServer) of
		       {ok, Els} -> filter_xmlels(Els);
		       _ -> []
		   end;
	       false ->
		   []
	   end,
    Mod:del_data(LUser, LServer),
    delete_cache(Mod, LUser, LServer, Data).

%%%===================================================================
%%% Pubsub
%%%===================================================================
-spec publish_pep_storage_bookmarks(jid(), [{binary(), xmlel()}]) -> ok | {error, stanza_error()}.
publish_pep_storage_bookmarks(JID, Data) ->
    {_, LServer, _} = LBJID = jid:remove_resource(jid:tolower(JID)),
    case gen_mod:is_loaded(LServer, mod_pubsub) of
	true ->
	    case lists:keyfind(?NS_STORAGE_BOOKMARKS, 1, Data) of
		false -> ok;
		{_, El} ->
		    case mod_pubsub:get_items(LBJID, ?NS_STORAGE_BOOKMARKS) of
			{error, #stanza_error{reason = 'item-not-found'}} ->
			    PubOpts = [{persist_items, true},
				       {access_model, whitelist}],
			    case mod_pubsub:publish_item(
				LBJID, LServer, ?NS_STORAGE_BOOKMARKS, JID,
				<<"current">>, [El], PubOpts, all) of
				{result, _} -> ok;
				{error, _} = Err -> Err
			    end;
			_ ->
			    case mod_pubsub:publish_item(
				LBJID, LServer, ?NS_STORAGE_BOOKMARKS, JID,
				<<"current">>, [El], [], all) of
				{result, _} -> ok;
				{error, _} = Err -> Err
			    end
		    end
	    end;
	false ->
	    ok
    end.

-spec publish_pep_native_bookmarks(jid(), [{binary(), xmlel()}]) -> ok | {error, stanza_error()}.
publish_pep_native_bookmarks(JID, Data) ->
    {_, LServer, _} = LBJID = jid:remove_resource(jid:tolower(JID)),
    case gen_mod:is_loaded(LServer, mod_pubsub) of
	true ->
	    case lists:keyfind(?NS_STORAGE_BOOKMARKS, 1, Data) of
		{_, Bookmarks0} ->
		    Bookmarks = case xmpp:decode(Bookmarks0) of
				    #bookmark_storage{conference = C} -> C;
				    _ -> []
				end,
		    PubOpts = [{persist_items, true}, {access_model, whitelist}, {max_items, max}, {notify_retract,true}, {notify_delete,true}, {send_last_published_item, never}],
		    case mod_pubsub:get_items(LBJID, ?NS_PEP_BOOKMARKS) of
			PepBookmarks when is_list(PepBookmarks) ->
			    put(mod_private_pep_update, true),
			    PepBookmarksMap = lists:foldl(fun pubsub_item_to_map/2, #{}, PepBookmarks),
			    {ToDelete, Ret} =
			    lists:foldl(
				fun(#bookmark_conference{jid = BookmarkJID} = Bookmark, {Map2, Ret2}) ->
				    PB = storage_bookmark_to_xmpp_bookmark(Bookmark),
				    case maps:take(jid:tolower(BookmarkJID), Map2) of
					{StoredBookmark, Map3} when StoredBookmark == PB ->
					    {Map3, Ret2};
					{_, Map4} ->
					    {Map4,
					     err_ret(Ret2, mod_pubsub:publish_item(
						 LBJID, LServer, ?NS_PEP_BOOKMARKS, JID,
						 jid:encode(BookmarkJID), [xmpp:encode(PB)], [], all))};
					_ ->
					    {Map2,
					     err_ret(Ret2, mod_pubsub:publish_item(
						 LBJID, LServer, ?NS_PEP_BOOKMARKS, JID,
						 jid:encode(BookmarkJID), [xmpp:encode(PB)], [], all))}
				    end
				end, {PepBookmarksMap, ok}, Bookmarks),
			    Ret4 =
			    maps:fold(
				fun(DeleteJid, _, Ret3) ->
				    err_ret(Ret3, mod_pubsub:delete_item(LBJID, ?NS_PEP_BOOKMARKS,
									 JID, jid:encode(DeleteJid)))
				end, Ret, ToDelete),
			    erase(mod_private_pep_update),
			    Ret4;
			{error, #stanza_error{reason = 'item-not-found'}} ->
			    put(mod_private_pep_update, true),
			    Ret7 =
			    lists:foldl(
				fun(#bookmark_conference{jid = BookmarkJID} = Bookmark, Ret5) ->
				    PB = storage_bookmark_to_xmpp_bookmark(Bookmark),
				    err_ret(Ret5, mod_pubsub:publish_item(
					LBJID, LServer, ?NS_PEP_BOOKMARKS, JID,
					jid:encode(BookmarkJID), [xmpp:encode(PB)], PubOpts, all))
				end, ok, Bookmarks),
			    erase(mod_private_pep_update),
			    Ret7;
			_ ->
			    ok
		    end;
		_ ->
		    ok
	    end;
	false ->
	    ok
    end.

err_ret({error, _} = E, _) ->
    E;
err_ret(ok, {error, _} = E) ->
    E;
err_ret(_, _) ->
    ok.

-spec pubsub_publish_item(binary(), binary(), jid(), jid(),
			  binary(), [xmlel()]) -> any().
pubsub_publish_item(LServer, ?NS_STORAGE_BOOKMARKS,
		    #jid{luser = LUser, lserver = LServer} = From,
		    #jid{luser = LUser, lserver = LServer},
		    _ItemId, [Payload|_]) ->
    set_data(From, [{?NS_STORAGE_BOOKMARKS, Payload}], false, true);
pubsub_publish_item(LServer, ?NS_PEP_BOOKMARKS,
		    #jid{luser = LUser, lserver = LServer} = From,
		    #jid{luser = LUser, lserver = LServer},
		    _ItemId, _Payload) ->
    NotRecursion = get(mod_private_pep_update) == undefined,
    case mod_pubsub:get_items({LUser, LServer, <<>>}, ?NS_PEP_BOOKMARKS) of
	Bookmarks when is_list(Bookmarks), NotRecursion ->
	    Bookmarks2 = lists:filtermap(fun pubsub_item_to_storage_bookmark/1, Bookmarks),
	    Payload = xmpp:encode(#bookmark_storage{conference = Bookmarks2}),
	    set_data(From, [{?NS_STORAGE_BOOKMARKS, Payload}], true, false);
	_ ->
	    ok
    end;
pubsub_publish_item(_, _, _, _, _, _) ->
    ok.

-spec pubsub_delete_item(binary(), binary(), jid(), jid(), binary()) -> any().
pubsub_delete_item(LServer, ?NS_PEP_BOOKMARKS,
		    #jid{luser = LUser, lserver = LServer} = From,
		    #jid{luser = LUser, lserver = LServer},
		    _ItemId) ->
    NotRecursion = get(mod_private_pep_update) == undefined,
    case mod_pubsub:get_items({LUser, LServer, <<>>}, ?NS_PEP_BOOKMARKS) of
	Bookmarks when is_list(Bookmarks), NotRecursion ->
	    Bookmarks2 = lists:filtermap(fun pubsub_item_to_storage_bookmark/1, Bookmarks),
	    Payload = xmpp:encode(#bookmark_storage{conference = Bookmarks2}),
	    set_data(From, [{?NS_STORAGE_BOOKMARKS, Payload}], true, false);
	_ ->
	    ok
    end;
pubsub_delete_item(_, _, _, _, _) ->
    ok.

-spec pubsub_item_to_storage_bookmark(#pubsub_item{}) -> {true, bookmark_conference()} | false.
pubsub_item_to_storage_bookmark(#pubsub_item{itemid = {Id, _}, payload = [#xmlel{} = B | _]}) ->
    case xmpp:decode(B) of
	#pep_bookmarks_conference{name = Name, autojoin = AutoJoin, nick = Nick,
				  password = Password} ->
	    try jid:decode(Id) of
		#jid{} = Jid ->
		    {true, #bookmark_conference{jid = Jid, name = Name, autojoin = AutoJoin, nick = Nick,
						password = Password}}
	    catch _:_ ->
		false
	    end;
	_ ->
	    false
    end;
pubsub_item_to_storage_bookmark(_) ->
    false.

-spec pubsub_tree_call(Res :: any(), _Tree::any(), atom(), any()) -> any().
pubsub_tree_call({error, #stanza_error{reason = 'item-not-found'}} = Res, Tree, get_node,
		 [{User, Server, _}, ?NS_PEP_BOOKMARKS] = Args) ->
    case get(mod_private_in_pubsub_tree_call) of
	undefined ->
	    put(mod_private_in_pubsub_tree_call, true),
	    bookmarks_to_pep(User, Server),
	    Res2 = apply(Tree, get_node, Args),
	    erase(mod_private_in_pubsub_tree_call),
	    Res2;
	_ ->
	    Res
    end;
pubsub_tree_call(Res, _Tree, _Function, _Args) ->
    Res.

-spec storage_bookmark_to_xmpp_bookmark(bookmark_conference()) -> pep_bookmarks_conference().
storage_bookmark_to_xmpp_bookmark(#bookmark_conference{name = Name, autojoin = AutoJoin, nick = Nick,
						       password = Password}) ->
    #pep_bookmarks_conference{name = Name, autojoin = AutoJoin, nick = Nick,
			      password = Password}.

-spec pubsub_item_to_map(#pubsub_item{}, map()) -> map().
pubsub_item_to_map(#pubsub_item{itemid = {Id, _}, payload = [#xmlel{} = B | _]}, Map) ->
    case xmpp:decode(B) of
	#pep_bookmarks_conference{} = B2 ->
	    try jid:decode(Id) of
		#jid{} = Jid ->
		    maps:put(jid:tolower(Jid), B2#pep_bookmarks_conference{extensions = undefined}, Map)
	    catch _:_ ->
		Map
	    end;
	_ ->
	    Map
    end;
pubsub_item_to_map(_, Map) ->
    Map.

%%%===================================================================
%%% Commands
%%%===================================================================
-spec get_commands_spec() -> [ejabberd_commands()].
get_commands_spec() ->
    [#ejabberd_commands{name = bookmarks_to_pep, tags = [private],
			desc = "Export private XML storage bookmarks to PEP",
			module = ?MODULE, function = bookmarks_to_pep,
			args = [{user, binary}, {host, binary}],
			args_rename = [{server, host}],
			args_desc = ["Username", "Server"],
			args_example = [<<"bob">>, <<"example.com">>],
			result = {res, restuple},
			result_desc = "Result tuple",
			result_example = {ok, <<"Bookmarks exported">>}}].

-spec bookmarks_to_pep(binary(), binary())
      -> {ok, binary()} | {error, binary()}.
bookmarks_to_pep(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Res = case use_cache(Mod, LServer) of
	      true ->
		  ets_cache:lookup(
		    ?PRIVATE_CACHE, {LUser, LServer, ?NS_STORAGE_BOOKMARKS},
		    fun() ->
			    Mod:get_data(LUser, LServer, ?NS_STORAGE_BOOKMARKS)
		    end);
	      false ->
		  Mod:get_data(LUser, LServer, ?NS_STORAGE_BOOKMARKS)
	end,
    case Res of
	{ok, El} ->
	    Data = [{?NS_STORAGE_BOOKMARKS, El}],
	    case publish_pep_storage_bookmarks(jid:make(User, Server), Data) of
		ok ->
		    case publish_pep_native_bookmarks(jid:make(User, Server), Data) of
			ok ->
			    {ok, <<"Bookmarks exported to PEP node">>};
			{error, Err} ->
			    {error, xmpp:format_stanza_error(Err)}
		    end;
		{error, Err} ->
		    {error, xmpp:format_stanza_error(Err)}

	    end;
	_ ->
	    {error, <<"Cannot retrieve bookmarks from private XML storage">>}
    end.

%%%===================================================================
%%% Cache
%%%===================================================================
-spec delete_cache(module(), binary(), binary(), [{binary(), xmlel()}]) -> ok.
delete_cache(Mod, LUser, LServer, Data) ->
    case use_cache(Mod, LServer) of
	true ->
	    Nodes = cache_nodes(Mod, LServer),
	    lists:foreach(
	      fun({NS, _}) ->
		      ets_cache:delete(?PRIVATE_CACHE,
				       {LUser, LServer, NS},
				       Nodes)
	      end, Data);
	false ->
	    ok
    end.

-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
    case use_cache(Mod, Host) of
	true ->
	    CacheOpts = cache_opts(Opts),
	    ets_cache:new(?PRIVATE_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?PRIVATE_CACHE)
    end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = mod_private_opt:cache_size(Opts),
    CacheMissed = mod_private_opt:cache_missed(Opts),
    LifeTime = mod_private_opt:cache_life_time(Opts),
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 1) of
	true -> Mod:use_cache(Host);
	false -> mod_private_opt:use_cache(Host)
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
	true -> Mod:cache_nodes(Host);
	false -> ejabberd_cluster:get_nodes()
    end.

%%%===================================================================
%%% Import/Export
%%%===================================================================
import_info() ->
    [{<<"private_storage">>, 4}].

import_start(LServer, DBType) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:init(LServer, []).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer, {sql, _}, DBType, Tab, L) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Tab, L).
