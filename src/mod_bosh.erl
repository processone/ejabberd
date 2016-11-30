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

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start_link/0]).
-export([start/2, stop/1, process/2, open_session/2,
	 close_session/1, find_session/1]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 depends/2, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("jlib.hrl").

-include("ejabberd_http.hrl").

-include("bosh.hrl").

-record(bosh, {sid = <<"">>      :: binary() | '_',
               timestamp = p1_time_compat:timestamp() :: erlang:timestamp() | '_',
               pid = self()      :: pid() | '$1'}).

-record(state, {}).

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
    Session = #bosh{sid = SID, timestamp = p1_time_compat:timestamp(), pid = Pid},
    lists:foreach(
      fun(Node) when Node == node() ->
	      gen_server:call(?MODULE, {write, Session});
	 (Node) ->
	      cluster_send({?MODULE, Node}, {write, Session})
      end, ejabberd_cluster:get_nodes()).

close_session(SID) ->
    case mnesia:dirty_read(bosh, SID) of
	[Session] ->
	    lists:foreach(
	      fun(Node) when Node == node() ->
		      gen_server:call(?MODULE, {delete, Session});
		 (Node) ->
		      cluster_send({?MODULE, Node}, {delete, Session})
	      end, ejabberd_cluster:get_nodes());
	[] ->
	    ok
    end.

write_session(#bosh{pid = Pid1, sid = SID, timestamp = T1} = S1) ->
    case mnesia:dirty_read(bosh, SID) of
	[#bosh{pid = Pid2, timestamp = T2} = S2] ->
	    if Pid1 == Pid2 ->
		    mnesia:dirty_write(S1);
	       T1 < T2 ->
		    cluster_send(Pid2, replaced),
		    mnesia:dirty_write(S1);
	       true ->
		    cluster_send(Pid1, replaced),
		    mnesia:dirty_write(S2)
	    end;
	[] ->
	    mnesia:dirty_write(S1)
    end.

delete_session(#bosh{sid = SID, pid = Pid1}) ->
    case mnesia:dirty_read(bosh, SID) of
	[#bosh{pid = Pid2}] ->
	    if Pid1 == Pid2 ->
		    mnesia:dirty_delete(bosh, SID);
	       true ->
		    ok
	    end;
	[] ->
	    ok
    end.

find_session(SID) ->
    case mnesia:dirty_read(bosh, SID) of
        [#bosh{pid = Pid}] ->
            {ok, Pid};
        [] ->
            error
    end.

start(Host, Opts) ->
    setup_database(),
    start_jiffy(Opts),
    TmpSup = gen_mod:get_module_proc(Host, ?PROCNAME),
    TmpSupSpec = {TmpSup,
		  {ejabberd_tmp_sup, start_link, [TmpSup, ejabberd_bosh]},
		  permanent, infinity, supervisor, [ejabberd_tmp_sup]},
    ProcSpec = {?MODULE,
		{?MODULE, start_link, []},
		transient, 2000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_sup, ProcSpec) of
	{ok, _} ->
	    supervisor:start_child(ejabberd_sup, TmpSupSpec);
	{error, {already_started, _}} ->
	    supervisor:start_child(ejabberd_sup, TmpSupSpec);
	Err ->
	    Err
    end.

stop(Host) ->
    TmpSup = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:terminate_child(ejabberd_sup, TmpSup),
    supervisor:delete_child(ejabberd_sup, TmpSup).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({write, Session}, _From, State) ->
    Res = write_session(Session),
    {reply, Res, State};
handle_call({delete, Session}, _From, State) ->
    Res = delete_session(Session),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({write, Session}, State) ->
    write_session(Session),
    {noreply, State};
handle_info({delete, Session}, State) ->
    delete_session(Session),
    {noreply, State};
handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
setup_database() ->
    case catch mnesia:table_info(bosh, attributes) of
        [sid, pid] ->
            mnesia:delete_table(bosh);
        _ ->
            ok
    end,
    ejabberd_mnesia:create(?MODULE, bosh,
			[{ram_copies, [node()]}, {local_content, true},
			 {attributes, record_info(fields, bosh)}]),
    mnesia:add_table_copy(bosh, node(), ram_copies).

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

cluster_send(NodePid, Msg) ->
    erlang:send(NodePid, Msg, [noconnect, nosuspend]).

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
mod_opt_type(_) ->
    [json, max_concat, max_inactivity, max_pause, prebind].
