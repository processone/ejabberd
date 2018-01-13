%%%-------------------------------------------------------------------
%%% File    : mod_bosh.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : This module acts as a bridge to ejabberd_bosh which implements
%%%           the real stuff, this is to handle the new pluggable architecture
%%%           for extending ejabberd's http service.
%%% Created : 20 Jul 2011 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(mod_bosh).

-author('steve@zeank.in-berlin.de').

%%-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([start_link/0]).
-export([start/2, stop/1, reload/3, process/2, open_session/2,
	 close_session/1, find_session/1, clean_cache/1]).

-export([depends/2, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("xmpp.hrl").
-include("ejabberd_http.hrl").
-include("bosh.hrl").

-callback init() -> any().
-callback open_session(binary(), pid()) -> ok | {error, any()}.
-callback close_session(binary()) -> ok | {error, any()}.
-callback find_session(binary()) -> {ok, pid()} | {error, any()}.
-callback use_cache() -> boolean().
-callback cache_nodes() -> [node()].

-optional_callbacks([use_cache/0, cache_nodes/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process([], #request{method = 'POST', data = <<>>}) ->
    ?DEBUG("Bad Request: no data", []),
    {400, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
	    children = [{xmlcdata, <<"400 Bad Request">>}]}};
process([],
	#request{method = 'POST', data = Data, ip = IP, headers = Hdrs}) ->
    ?DEBUG("Incoming data: ~p", [Data]),
    Type = get_type(Hdrs),
    ejabberd_bosh:process_request(Data, IP, Type);
process([], #request{method = 'GET', data = <<>>}) ->
    {200, ?HEADER(?CT_XML), get_human_html_xmlel()};
process([], #request{method = 'OPTIONS', data = <<>>}) ->
    {200, ?OPTIONS_HEADER, []};
process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
	    children = [{xmlcdata, <<"400 Bad Request">>}]}}.

-spec open_session(binary(), pid()) -> ok | {error, any()}.
open_session(SID, Pid) ->
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    case Mod:open_session(SID, Pid) of
	ok ->
	    delete_cache(Mod, SID);
	{error, _} = Err ->
	    Err
    end.

-spec close_session(binary()) -> ok.
close_session(SID) ->
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    Mod:close_session(SID),
    delete_cache(Mod, SID).

-spec find_session(binary()) -> {ok, pid()} | error.
find_session(SID) ->
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    case use_cache(Mod) of
	true ->
	    ets_cache:lookup(
	      ?BOSH_CACHE, SID,
	      fun() ->
		      case Mod:find_session(SID) of
			  {ok, Pid} -> {ok, Pid};
			  {error, _} -> error
		      end
	      end);
	false ->
	    case Mod:find_session(SID) of
		{ok, Pid} -> {ok, Pid};
		{error, _} -> error
	    end
    end.

start(Host, Opts) ->
    start_jiffy(Opts),
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    init_cache(Mod),
    Mod:init(),
    clean_cache(),
    TmpSup = gen_mod:get_module_proc(Host, ?MODULE),
    TmpSupSpec = {TmpSup,
		  {ejabberd_tmp_sup, start_link, [TmpSup, ejabberd_bosh]},
		  permanent, infinity, supervisor, [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_gen_mod_sup, TmpSupSpec).

stop(Host) ->
    TmpSup = gen_mod:get_module_proc(Host, ?MODULE),
    supervisor:terminate_child(ejabberd_gen_mod_sup, TmpSup),
    supervisor:delete_child(ejabberd_gen_mod_sup, TmpSup).

reload(_Host, NewOpts, _OldOpts) ->
    start_jiffy(NewOpts),
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    init_cache(Mod),
    Mod:init(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_jiffy(Opts) ->
    case gen_mod:get_opt(json, Opts, false) of
        false ->
            ok;
        true ->
            case catch ejabberd:start_app(jiffy) of
                ok ->
                    ok;
                Err ->
                    ?WARNING_MSG("Failed to start JSON codec (jiffy): ~p. "
                                 "JSON support will be disabled", [Err])
            end
    end.

get_type(Hdrs) ->
    try
        {_, S} = lists:keyfind('Content-Type', 1, Hdrs),
        [T|_] = str:tokens(S, <<";">>),
        [_, <<"json">>] = str:tokens(T, <<"/">>),
        json
    catch _:_ ->
            xml
    end.

depends(_Host, _Opts) ->
    [].

mod_opt_type(json) ->
    fun (false) -> false;
	(true) -> true
    end;
mod_opt_type(max_concat) ->
    fun (unlimited) -> unlimited;
	(N) when is_integer(N), N > 0 -> N
    end;
mod_opt_type(max_inactivity) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(max_pause) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(prebind) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(ram_db_type) ->
    fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(queue_type) ->
    fun(ram) -> ram; (file) -> file end;
mod_opt_type(O) when O == use_cache; O == cache_missed ->
    fun(B) when is_boolean(B) -> B end;
mod_opt_type(O) when O == cache_size; O == cache_life_time ->
    fun(I) when is_integer(I), I>0 -> I;
       (unlimited) -> infinity;
       (infinity) -> infinity
    end;
mod_opt_type(_) ->
    [json, max_concat, max_inactivity, max_pause, prebind, ram_db_type,
     queue_type, use_cache, cache_size, cache_missed, cache_life_time].

%%%----------------------------------------------------------------------
%%% Cache stuff
%%%----------------------------------------------------------------------
-spec init_cache(module()) -> ok.
init_cache(Mod) ->
    case use_cache(Mod) of
	true ->
	    ets_cache:new(?BOSH_CACHE, cache_opts());
	false ->
	    ets_cache:delete(?BOSH_CACHE)
    end.

-spec use_cache(module()) -> boolean().
use_cache(Mod) ->
    case erlang:function_exported(Mod, use_cache, 0) of
	true -> Mod:use_cache();
	false ->
	    gen_mod:get_module_opt(
	      global, ?MODULE, use_cache,
	      ejabberd_config:use_cache(global))
    end.

-spec cache_nodes(module()) -> [node()].
cache_nodes(Mod) ->
    case erlang:function_exported(Mod, cache_nodes, 0) of
	true -> Mod:cache_nodes();
	false -> ejabberd_cluster:get_nodes()
    end.

-spec delete_cache(module(), binary()) -> ok.
delete_cache(Mod, SID) ->
    case use_cache(Mod) of
	true ->
	    ets_cache:delete(?BOSH_CACHE, SID, cache_nodes(Mod));
	false ->
	    ok
    end.

-spec cache_opts() -> [proplists:property()].
cache_opts() ->
    MaxSize = gen_mod:get_module_opt(
		global, ?MODULE, cache_size,
		ejabberd_config:cache_size(global)),
    CacheMissed = gen_mod:get_module_opt(
		    global, ?MODULE, cache_missed,
		    ejabberd_config:cache_missed(global)),
    LifeTime = case gen_mod:get_module_opt(
		      global, ?MODULE, cache_life_time,
		      ejabberd_config:cache_life_time(global)) of
		   infinity -> infinity;
		   I -> timer:seconds(I)
	       end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec clean_cache(node()) -> ok.
clean_cache(Node) ->
    ets_cache:filter(
      ?BOSH_CACHE,
      fun(_, error) ->
	      false;
	 (_, {ok, Pid}) ->
	      node(Pid) /= Node
      end).

-spec clean_cache() -> ok.
clean_cache() ->
    ejabberd_cluster:eval_everywhere(?MODULE, clean_cache, [node()]).

%%%----------------------------------------------------------------------
%%% Help Web Page
%%%----------------------------------------------------------------------

get_human_html_xmlel() ->
    Heading = <<"ejabberd ",
		(iolist_to_binary(atom_to_list(?MODULE)))/binary>>,
    #xmlel{name = <<"html">>,
	   attrs =
	       [{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>}],
	   children =
	       [#xmlel{name = <<"head">>,
		       children =
			   [#xmlel{name = <<"title">>,
				   children = [{xmlcdata, Heading}]},
			    #xmlel{name = <<"style">>,
				   children = [{xmlcdata, get_style_cdata()}]}]},
		#xmlel{name = <<"body">>,
		       children =
			   [#xmlel{name = <<"div">>,
				   attrs = [{<<"class">>, <<"container">>}],
				   children = get_container_children(Heading)}]}]}.

get_container_children(Heading) ->
    [#xmlel{name = <<"div">>,
	    attrs = [{<<"class">>, <<"section">>}],
	    children =
		[#xmlel{name = <<"div">>,
			attrs = [{<<"class">>, <<"block">>}],
			children =
			    [#xmlel{name = <<"a">>,
				    attrs = [{<<"href">>, <<"https://www.ejabberd.im">>}],
				    children =
					[#xmlel{name = <<"img">>,
						attrs = [{<<"height">>, <<"32">>},
							 {<<"src">>, get_image_src()}]}]}]}]},
     #xmlel{name = <<"div">>,
	    attrs = [{<<"class">>, <<"white section">>}],
	    children =
		[#xmlel{name = <<"div">>,
			attrs = [{<<"class">>, <<"block">>}],
			children =
			    [#xmlel{name = <<"h1">>, children = [{xmlcdata, Heading}]},
			     #xmlel{name = <<"p">>, children =
					[{xmlcdata, <<"An implementation of ">>},
					 #xmlel{name = <<"a">>,
						attrs = [{<<"href">>, <<"http://xmpp.org/extensions/xep-0206.html">>}],
						children = [{xmlcdata, <<"XMPP over BOSH (XEP-0206)">>}]}]},
			     #xmlel{name = <<"p">>, children =
					[{xmlcdata, <<"This web page is only informative. To "
						      "use HTTP-Bind you need a Jabber/XMPP "
						      "client that supports it.">>}]}]}]},
     #xmlel{name = <<"div">>,
	    attrs = [{<<"class">>, <<"section">>}],
	    children =
		[#xmlel{name = <<"div">>,
			attrs = [{<<"class">>, <<"block">>}],
			children =
			    [#xmlel{name = <<"a">>,
				    attrs = [{<<"href">>, <<"https://www.ejabberd.im">>},
					     {<<"title">>, <<"ejabberd XMPP server">>}],
				    children = [{xmlcdata, <<"ejabberd">>}]},
			     {xmlcdata, <<" is maintained by ">>},
			     #xmlel{name = <<"a">>,
				    attrs = [{<<"href">>, <<"https://www.process-one.net">>},
					     {<<"title">>, <<"ProcessOne - Leader in Instant Messaging and Push Solutions">>}],
				    children = [{xmlcdata, <<"ProcessOne">>}]} ]}]}
    ].

get_style_cdata() ->
    case misc:read_css("bosh.css") of
	{ok, Data} -> Data;
	{error, _} -> <<>>
    end.

get_image_src() ->
    case misc:read_img("bosh-logo.png") of
	{ok, Img} ->
	    B64Img = base64:encode(Img),
	    <<"data:image/png;base64,", B64Img/binary>>;
	{error, _} ->
	    <<>>
    end.
