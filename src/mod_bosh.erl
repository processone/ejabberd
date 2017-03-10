%%%-------------------------------------------------------------------
%%% File    : mod_bosh.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : This module acts as a bridge to ejabberd_bosh which implements
%%%           the real stuff, this is to handle the new pluggable architecture
%%%           for extending ejabberd's http service.
%%% Created : 20 Jul 2011 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(mod_bosh).

-author('steve@zeank.in-berlin.de').

%%-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([start_link/0]).
-export([start/2, stop/1, reload/3, process/2, open_session/2,
	 close_session/1, find_session/1]).

-export([depends/2, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("xmpp.hrl").
-include("ejabberd_http.hrl").
-include("bosh.hrl").

-callback init() -> any().
-callback open_session(binary(), pid()) -> any().
-callback close_session(binary()) -> any().
-callback find_session(binary()) -> {ok, pid()} | error.

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

open_session(SID, Pid) ->
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    Mod:open_session(SID, Pid).

close_session(SID) ->
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    Mod:close_session(SID).

find_session(SID) ->
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    Mod:find_session(SID).

start(Host, Opts) ->
    start_jiffy(Opts),
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    Mod:init(),
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
    Mod:init(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_jiffy(Opts) ->
    case gen_mod:get_opt(json, Opts,
                         fun(false) -> false;
                            (true) -> true
                         end, false) of
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
mod_opt_type(_) ->
    [json, max_concat, max_inactivity, max_pause, prebind, ram_db_type,
     queue_type].

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
    <<"
      body {
	margin: 0;
	padding: 0;
	font-family: sans-serif;
	color: #fff;
      }
      h1 {
	font-size: 3em;
	color: #444;
      }
      p {
	line-height: 1.5em;
	color: #888;
      }
      a {
	color: #fff;
      }
      a:hover,
      a:active {
	text-decoration: underline;
      }
      .container {
	position: absolute;
	top: 0;
	left: 0;
	right: 0;
	bottom: 0;
	background: #424A55;
	background-image: -webkit-linear-gradient(270deg, rgba(48,52,62,0) 24%, #30353e 100%);
	background-image: linear-gradient(-180deg, rgba(48,52,62,0) 24%, #30353e 100%);
      }
      .section {
	padding: 3em;
      }
      .white.section {
	background: #fff;
	border-bottom: 4px solid #41AFCA;
      }
      .white.section a {
	text-decoration: none;
	color: #41AFCA;
      }
      .white.section a:hover,
      .white.section a:active {
	 text-decoration: underline;
      }
      .block {
	margin: 0 auto;
	max-width: 900px;
	width: 100%;
      }">>.

get_image_src() ->
    <<"data:image/png;base64,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">>.
