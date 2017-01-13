%%%-------------------------------------------------------------------
%%% File    : mod_bosh.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : This module acts as a bridge to ejabberd_bosh which implements
%%%           the real stuff, this is to handle the new pluggable architecture
%%%           for extending ejabberd's http service.
%%% Created : 20 Jul 2011 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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
-export([start/2, stop/1, process/2, open_session/2,
	 close_session/1, find_session/1]).

-export([depends/2, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("jlib.hrl").
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

get_human_html_xmlel() ->
    Heading = <<"ejabberd ", (jlib:atom_to_binary(?MODULE))/binary>>,
    #xmlel{name = <<"html">>,
	   attrs =
	       [{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>}],
	   children =
	       [#xmlel{name = <<"head">>, attrs = [],
		       children =
			   [#xmlel{name = <<"title">>, attrs = [],
				   children = [{xmlcdata, Heading}]}]},
		#xmlel{name = <<"body">>, attrs = [],
		       children =
			   [#xmlel{name = <<"h1">>, attrs = [],
				   children = [{xmlcdata, Heading}]},
			    #xmlel{name = <<"p">>, attrs = [],
				   children =
				       [{xmlcdata, <<"An implementation of ">>},
					#xmlel{name = <<"a">>,
					       attrs =
						   [{<<"href">>,
						     <<"http://xmpp.org/extensions/xep-0206.html">>}],
					       children =
						   [{xmlcdata,
						     <<"XMPP over BOSH (XEP-0206)">>}]}]},
			    #xmlel{name = <<"p">>, attrs = [],
				   children =
				       [{xmlcdata,
					 <<"This web page is only informative. To "
					   "use HTTP-Bind you need a Jabber/XMPP "
					   "client that supports it.">>}]}]}]}.

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
    TmpSup = gen_mod:get_module_proc(Host, ?PROCNAME),
    TmpSupSpec = {TmpSup,
		  {ejabberd_tmp_sup, start_link, [TmpSup, ejabberd_bosh]},
		  permanent, infinity, supervisor, [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, TmpSupSpec),
    Mod = gen_mod:ram_db_mod(global, ?MODULE),
    Mod:init().

stop(Host) ->
    TmpSup = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:terminate_child(ejabberd_sup, TmpSup),
    supervisor:delete_child(ejabberd_sup, TmpSup).

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
mod_opt_type(_) ->
    [json, max_concat, max_inactivity, max_pause, prebind, ram_db_type].
