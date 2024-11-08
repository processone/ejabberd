%%%----------------------------------------------------------------------
%%% File    : mod_caps.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Request and cache Entity Capabilities (XEP-0115)
%%% Created : 7 Oct 2006 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
%%% 2009, improvements from ProcessOne to support correct PEP handling
%%% through s2s, use less memory, and speedup global caps handling
%%%----------------------------------------------------------------------

-module(mod_caps).

-author('henoch@dtek.chalmers.se').

-protocol({xep, 115, '1.5', '2.1.4', "complete", ""}).

-behaviour(gen_server).

-behaviour(gen_mod).

-export([read_caps/1, list_features/1, caps_stream_features/2,
	 disco_features/5, disco_identity/5, disco_info/5,
	 get_features/2, export/1, import_info/0, import/5,
         get_user_caps/2, import_start/2, import_stop/2,
	 compute_disco_hash/2, is_valid_node/1]).

%% gen_mod callbacks
-export([start/2, stop/1, reload/3, depends/2]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

-export([user_send_packet/1, user_receive_packet/1,
	 c2s_presence_in/2, c2s_copy_session/2,
	 mod_opt_type/1, mod_options/1, mod_doc/0]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_caps.hrl").
-include("translate.hrl").

-define(BAD_HASH_LIFETIME, 600).

-record(state, {host = <<"">> :: binary()}).

-type digest_type() :: md5 | sha | sha224 | sha256 | sha384 | sha512.

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), {binary(), binary()}, [binary() | pos_integer()]) -> ok.
-callback caps_read(binary(), {binary(), binary()}) ->
    {ok, non_neg_integer() | [binary()]} | error.
-callback caps_write(binary(), {binary(), binary()},
		     non_neg_integer() | [binary()]) -> any().
-callback use_cache(binary()) -> boolean().

-optional_callbacks([use_cache/1]).

start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

-spec get_features(binary(), nothing | caps()) -> [binary()].
get_features(_Host, nothing) -> [];
get_features(Host, #caps{node = Node, version = Version,
		   exts = Exts}) ->
    SubNodes = [Version | Exts],
    Mod = gen_mod:db_mod(Host, ?MODULE),
    lists:foldl(
      fun(SubNode, Acc) ->
	      NodePair = {Node, SubNode},
	      Res = case use_cache(Mod, Host) of
			true ->
			    ets_cache:lookup(caps_features_cache, NodePair,
					     caps_read_fun(Host, NodePair));
			false ->
			    Mod:caps_read(Host, NodePair)
		    end,
	      case Res of
		  {ok, Features} when is_list(Features) ->
		      Features ++ Acc;
		  _ -> Acc
	      end
      end, [], SubNodes).

-spec list_features(ejabberd_c2s:state()) -> [{ljid(), caps()}].
list_features(C2SState) ->
    Rs = maps:get(caps_resources, C2SState, gb_trees:empty()),
    gb_trees:to_list(Rs).

-spec get_user_caps(jid() | ljid(), ejabberd_c2s:state()) -> {ok, caps()} | error.
get_user_caps(JID, C2SState) ->
    Rs = maps:get(caps_resources, C2SState, gb_trees:empty()),
    LJID = jid:tolower(JID),
    case gb_trees:lookup(LJID, Rs) of
	{value, Caps} ->
	    {ok, Caps};
	none ->
	    error
    end.

-spec read_caps(#presence{}) -> nothing | caps().
read_caps(Presence) ->
    case xmpp:get_subtag(Presence, #caps{}) of
	false -> nothing;
	Caps -> Caps
    end.

-spec user_send_packet({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_send_packet({#presence{type = available,
			    from = #jid{luser = U, lserver = LServer} = From,
			    to = #jid{luser = U, lserver = LServer,
				      lresource = <<"">>}} = Pkt,
		  #{jid := To} = State}) ->
    case read_caps(Pkt) of
	nothing -> ok;
	#caps{version = Version, exts = Exts} = Caps ->
	    feature_request(LServer, From, To, Caps, [Version | Exts])
    end,
    {Pkt, State};
user_send_packet(Acc) ->
    Acc.

-spec user_receive_packet({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_receive_packet({#presence{from = From, type = available} = Pkt,
		     #{lserver := LServer, jid := To} = State}) ->
    IsRemote = case From#jid.lresource of
		   % Don't store caps for presences sent by our muc rooms
		   <<>> ->
		       try ejabberd_router:host_of_route(From#jid.lserver) of
			   MaybeMuc ->
			       not lists:member(From#jid.lserver,
						gen_mod:get_module_opt_hosts(MaybeMuc, mod_muc))
		       catch error:{unregistered_route, _} ->
			   true
		       end;
		   _ ->
		       not ejabberd_router:is_my_host(From#jid.lserver)
	       end,
    if IsRemote ->
	   case read_caps(Pkt) of
	     nothing -> ok;
	     #caps{version = Version, exts = Exts} = Caps ->
		    feature_request(LServer, To, From, Caps, [Version | Exts])
	   end;
       true -> ok
    end,
    {Pkt, State};
user_receive_packet(Acc) ->
    Acc.

-spec caps_stream_features([xmpp_element()], binary()) -> [xmpp_element()].
caps_stream_features(Acc, MyHost) ->
    case gen_mod:is_loaded(MyHost, ?MODULE) of
	true ->
	    case make_my_disco_hash(MyHost) of
		<<"">> ->
		    Acc;
		Hash ->
		    [#caps{hash = <<"sha-1">>, node = ejabberd_config:get_uri(),
			   version = Hash} | Acc]
	    end;
	false ->
	    Acc
    end.

-spec disco_features({error, stanza_error()} | {result, [binary()]} | empty,
		     jid(), jid(),
		     binary(), binary()) ->
			    {error, stanza_error()} | {result, [binary()]} | empty.
disco_features(Acc, From, To, Node, Lang) ->
    case is_valid_node(Node) of
        true ->
            ejabberd_hooks:run_fold(disco_local_features,
                                    To#jid.lserver, empty,
                                    [From, To, <<"">>, Lang]);
        false ->
            Acc
    end.

-spec disco_identity([identity()], jid(), jid(),
		     binary(), binary()) ->
			    [identity()].
disco_identity(Acc, From, To, Node, Lang) ->
    case is_valid_node(Node) of
        true ->
            ejabberd_hooks:run_fold(disco_local_identity,
                                    To#jid.lserver, [],
                                    [From, To, <<"">>, Lang]);
        false ->
            Acc
    end.

-spec disco_info([xdata()], binary(), module(), binary(), binary()) -> [xdata()];
		([xdata()], jid(), jid(), binary(), binary()) -> [xdata()].
disco_info(Acc, Host, Module, Node, Lang) when is_atom(Module) ->
    case is_valid_node(Node) of
        true ->
            ejabberd_hooks:run_fold(disco_info, Host, [],
                                    [Host, Module, <<"">>, Lang]);
        false ->
            Acc
    end;
disco_info(Acc, _, _, _Node, _Lang) ->
    Acc.

-spec c2s_presence_in(ejabberd_c2s:state(), presence()) -> ejabberd_c2s:state().
c2s_presence_in(C2SState,
		#presence{from = From, to = To, type = Type} = Presence) ->
    ToSelf = (From#jid.luser == To#jid.luser)
	       andalso (From#jid.lserver == To#jid.lserver),
    Caps = read_caps(Presence),
    Operation =
    case {Type, ToSelf, Caps} of
	{unavailable, _, _} -> delete;
	{error, _, _} -> delete;
	{available, _, nothing} -> skip;
	{available, true, _} -> insert;
	{available, _, _} ->
	    {Subscription, _, _} = ejabberd_hooks:run_fold(
		roster_get_jid_info, To#jid.lserver,
		{none, none, []},
		[To#jid.luser, To#jid.lserver, From]),
	    case Subscription of
		from -> insert;
		both -> insert;
		_ -> skip
	    end;
	_ ->
	    skip
    end,
    case Operation of
	skip ->
	    C2SState;
	delete ->
	    LFrom = jid:tolower(From),
	    Rs = maps:get(caps_resources, C2SState, gb_trees:empty()),
	    C2SState#{caps_resources => gb_trees:delete_any(LFrom, Rs)};
	insert ->
	    LFrom = jid:tolower(From),
	    Rs = maps:get(caps_resources, C2SState, gb_trees:empty()),
	    NewRs = case gb_trees:lookup(LFrom, Rs) of
			{value, Caps} -> Rs;
			none ->
			    ejabberd_hooks:run(caps_add, To#jid.lserver,
					       [From, To,
						get_features(To#jid.lserver, Caps)]),
			    gb_trees:insert(LFrom, Caps, Rs);
			_ ->
			    ejabberd_hooks:run(caps_update, To#jid.lserver,
					       [From, To,
						get_features(To#jid.lserver, Caps)]),
			    gb_trees:update(LFrom, Caps, Rs)
		    end,
	    C2SState#{caps_resources => NewRs}
    end.

-spec c2s_copy_session(ejabberd_c2s:state(), ejabberd_c2s:state())
      -> ejabberd_c2s:state().
c2s_copy_session(C2SState, #{caps_resources := Rs}) ->
    C2SState#{caps_resources => Rs};
c2s_copy_session(C2SState, _) ->
    C2SState.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(OldOpts, ?MODULE),
    if OldMod /= NewMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    init_cache(NewMod, Host, NewOpts).

init([Host|_]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(Host, ?MODULE),
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    init_cache(Mod, Host, Opts),
    Mod:init(Host, Opts),
    ejabberd_hooks:add(c2s_presence_in, Host, ?MODULE,
		       c2s_presence_in, 75),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 75),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       user_receive_packet, 75),
    ejabberd_hooks:add(c2s_post_auth_features, Host, ?MODULE,
		       caps_stream_features, 75),
    ejabberd_hooks:add(s2s_in_post_auth_features, Host, ?MODULE,
		       caps_stream_features, 75),
    ejabberd_hooks:add(c2s_copy_session, Host, ?MODULE,
		       c2s_copy_session, 75),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
		       disco_features, 75),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE,
		       disco_identity, 75),
    ejabberd_hooks:add(disco_info, Host, ?MODULE,
		       disco_info, 75),
    {ok, #state{host = Host}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({iq_reply, IQReply, {Host, From, To, Caps, SubNodes}}, State) ->
    feature_response(IQReply, Host, From, To, Caps, SubNodes),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(c2s_presence_in, Host, ?MODULE,
			  c2s_presence_in, 75),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet, 75),
    ejabberd_hooks:delete(user_receive_packet, Host,
			  ?MODULE, user_receive_packet, 75),
    ejabberd_hooks:delete(c2s_post_auth_features, Host,
			  ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:delete(s2s_in_post_auth_features, Host,
			  ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:delete(c2s_copy_session, Host, ?MODULE,
			  c2s_copy_session, 75),
    ejabberd_hooks:delete(disco_local_features, Host,
			  ?MODULE, disco_features, 75),
    ejabberd_hooks:delete(disco_local_identity, Host,
			  ?MODULE, disco_identity, 75),
    ejabberd_hooks:delete(disco_info, Host, ?MODULE,
			  disco_info, 75),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec feature_request(binary(), jid(), jid(), caps(), [binary()]) -> any().
feature_request(Host, From, To, Caps,
		[SubNode | Tail] = SubNodes) ->
    Node = Caps#caps.node,
    NodePair = {Node, SubNode},
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Res = case use_cache(Mod, Host) of
	      true ->
		  ets_cache:lookup(caps_features_cache, NodePair,
				   caps_read_fun(Host, NodePair));
	      false ->
		  Mod:caps_read(Host, NodePair)
	  end,
    case Res of
	{ok, Fs} when is_list(Fs) ->
	    feature_request(Host, From, To, Caps, Tail);
	_ ->
	    LTo = jid:tolower(To),
	    case ets_cache:insert_new(caps_requests_cache, {LTo, NodePair}, ok) of
		true ->
		    IQ = #iq{type = get,
			     from = From,
			     to = To,
			     sub_els = [#disco_info{node = <<Node/binary, "#",
							     SubNode/binary>>}]},
		    ejabberd_router:route_iq(
		      IQ, {Host, From, To, Caps, SubNodes},
		      gen_mod:get_module_proc(Host, ?MODULE));
		false ->
		    ok
	    end,
	    feature_request(Host, From, To, Caps, Tail)
    end;
feature_request(_Host, _From, _To, _Caps, []) -> ok.

-spec feature_response(iq(), binary(), jid(), jid(), caps(), [binary()]) -> any().
feature_response(#iq{type = result, sub_els = [El]},
		 Host, From, To, Caps, [SubNode | SubNodes]) ->
    NodePair = {Caps#caps.node, SubNode},
    try
	DiscoInfo = xmpp:decode(El),
	case check_hash(Caps, DiscoInfo) of
	    true ->
		Features = DiscoInfo#disco_info.features,
		LServer = jid:nameprep(Host),
		Mod = gen_mod:db_mod(LServer, ?MODULE),
		case Mod:caps_write(LServer, NodePair, Features) of
		    ok ->
			case use_cache(Mod, LServer) of
			    true ->
				ets_cache:delete(caps_features_cache, NodePair);
			    false ->
				ok
			end;
		    {error, _} ->
			ok
		end;
	    false -> ok
	end
    catch _:{xmpp_codec, _Why} ->
	    ok
    end,
    feature_request(Host, From, To, Caps, SubNodes);
feature_response(_IQResult, Host, From, To, Caps,
		 [_SubNode | SubNodes]) ->
    feature_request(Host, From, To, Caps, SubNodes).

-spec caps_read_fun(binary(), {binary(), binary()})
      -> fun(() -> {ok, [binary()] | non_neg_integer()} | error).
caps_read_fun(Host, Node) ->
    LServer = jid:nameprep(Host),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    fun() -> Mod:caps_read(LServer, Node) end.

-spec make_my_disco_hash(binary()) -> binary().
make_my_disco_hash(Host) ->
    JID = jid:make(Host),
    case {ejabberd_hooks:run_fold(disco_local_features,
				  Host, empty, [JID, JID, <<"">>, <<"">>]),
	  ejabberd_hooks:run_fold(disco_local_identity, Host, [],
				  [JID, JID, <<"">>, <<"">>]),
	  ejabberd_hooks:run_fold(disco_info, Host, [],
				  [Host, undefined, <<"">>, <<"">>])}
	of
      {{result, Features}, Identities, Info} ->
	  Feats = lists:map(fun ({{Feat, _Host}}) -> Feat;
				(Feat) -> Feat
			    end,
			    Features),
	  DiscoInfo = #disco_info{identities = Identities,
				  features = Feats,
				  xdata = Info},
	  compute_disco_hash(DiscoInfo, sha);
      _Err -> <<"">>
    end.

-spec compute_disco_hash(disco_info(), digest_type()) -> binary().
compute_disco_hash(DiscoInfo, Algo) ->
    Concat = list_to_binary([concat_identities(DiscoInfo),
                             concat_features(DiscoInfo), concat_info(DiscoInfo)]),
    base64:encode(case Algo of
		      md5 -> erlang:md5(Concat);
		      sha -> crypto:hash(sha, Concat);
		      sha224 -> crypto:hash(sha224, Concat);
		      sha256 -> crypto:hash(sha256, Concat);
		      sha384 -> crypto:hash(sha384, Concat);
		      sha512 -> crypto:hash(sha512, Concat)
		  end).

-spec check_hash(caps(), disco_info()) -> boolean().
check_hash(Caps, DiscoInfo) ->
    case Caps#caps.hash of
      <<"md5">> ->
	  Caps#caps.version == compute_disco_hash(DiscoInfo, md5);
      <<"sha-1">> ->
	  Caps#caps.version == compute_disco_hash(DiscoInfo, sha);
      <<"sha-224">> ->
	  Caps#caps.version == compute_disco_hash(DiscoInfo, sha224);
      <<"sha-256">> ->
	  Caps#caps.version == compute_disco_hash(DiscoInfo, sha256);
      <<"sha-384">> ->
	  Caps#caps.version == compute_disco_hash(DiscoInfo, sha384);
      <<"sha-512">> ->
	  Caps#caps.version == compute_disco_hash(DiscoInfo, sha512);
      _ -> true
    end.

-spec concat_features(disco_info()) -> iolist().
concat_features(#disco_info{features = Features}) ->
    lists:usort([[Feat, $<] || Feat <- Features]).

-spec concat_identities(disco_info()) -> iolist().
concat_identities(#disco_info{identities = Identities}) ->
    lists:sort(
      [[Cat, $/, T, $/, Lang, $/, Name, $<] ||
	  #identity{category = Cat, type = T,
		    lang = Lang, name = Name} <- Identities]).

-spec concat_info(disco_info()) -> iolist().
concat_info(#disco_info{xdata = Xs}) ->
    lists:sort(
      [concat_xdata_fields(X) || #xdata{type = result} = X <- Xs]).

-spec concat_xdata_fields(xdata()) -> iolist().
concat_xdata_fields(#xdata{fields = Fields} = X) ->
    Form = xmpp_util:get_xdata_values(<<"FORM_TYPE">>, X),
    Res = [[Var, $<, lists:sort([[Val, $<] || Val <- Values])]
	   || #xdata_field{var = Var, values = Values} <- Fields,
	      is_binary(Var), Var /= <<"FORM_TYPE">>],
    [Form, $<, lists:sort(Res)].

-spec is_valid_node(binary()) -> boolean().
is_valid_node(Node) ->
    case str:tokens(Node, <<"#">>) of
	[H|_] ->
	    H == ejabberd_config:get_uri();
        [] ->
            false
    end.

init_cache(Mod, Host, Opts) ->
    CacheOpts = cache_opts(Opts),
    case use_cache(Mod, Host) of
	true ->
	    ets_cache:new(caps_features_cache, CacheOpts);
	false ->
	    ets_cache:delete(caps_features_cache)
    end,
    CacheSize = proplists:get_value(max_size, CacheOpts),
    ets_cache:new(caps_requests_cache,
		  [{max_size, CacheSize},
		   {life_time, timer:seconds(?BAD_HASH_LIFETIME)}]).

use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 1) of
	true -> Mod:use_cache(Host);
	false -> mod_caps_opt:use_cache(Host)
    end.

cache_opts(Opts) ->
    MaxSize = mod_caps_opt:cache_size(Opts),
    CacheMissed = mod_caps_opt:cache_missed(Opts),
    LifeTime = mod_caps_opt:cache_life_time(Opts),
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import_info() ->
    [{<<"caps_features">>, 4}].

import_start(LServer, DBType) ->
    ets:new(caps_features_tmp, [private, named_table, bag]),
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:init(LServer, []),
    ok.

import(_LServer, {sql, _}, _DBType, <<"caps_features">>,
       [Node, SubNode, Feature, _TimeStamp]) ->
    Feature1 = case catch binary_to_integer(Feature) of
                   I when is_integer(I), I>0 -> I;
                   _ -> Feature
               end,
    ets:insert(caps_features_tmp, {{Node, SubNode}, Feature1}),
    ok.

import_stop(LServer, DBType) ->
    import_next(LServer, DBType, ets:first(caps_features_tmp)),
    ets:delete(caps_features_tmp),
    ok.

import_next(_LServer, _DBType, '$end_of_table') ->
    ok;
import_next(LServer, DBType, NodePair) ->
    Features = [F || {_, F} <- ets:lookup(caps_features_tmp, NodePair)],
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, NodePair, Features),
    import_next(LServer, DBType, ets:next(caps_features_tmp, NodePair)).

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
          [?T("This module implements "
              "https://xmpp.org/extensions/xep-0115.html"
              "[XEP-0115: Entity Capabilities]."),
           ?T("The main purpose of the module is to provide "
              "PEP functionality (see _`mod_pubsub`_).")],
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
