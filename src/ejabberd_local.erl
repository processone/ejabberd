%%%----------------------------------------------------------------------
%%% File    : ejabberd_local.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Route local packets
%%% Created : 30 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(ejabberd_local).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).

-export([route/1,
	 get_features/1,
	 bounce_resource_packet/1,
	 host_up/1, host_down/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

%% deprecated functions: use ejabberd_router:route_iq/3,4
-export([route_iq/2, route_iq/3]).
-deprecated([{route_iq, 2}, {route_iq, 3}]).

-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_stacktrace.hrl").
-include("translate.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    ChildSpec = {?MODULE, {?MODULE, start_link, []},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec route(stanza()) -> ok.
route(Packet) ->
    ?DEBUG("Local route:~n~ts", [xmpp:pp(Packet)]),
    Type = xmpp:get_type(Packet),
    To = xmpp:get_to(Packet),
    if To#jid.luser /= <<"">> ->
	    ejabberd_sm:route(Packet);
       is_record(Packet, iq), To#jid.lresource == <<"">> ->
	    gen_iq_handler:handle(?MODULE, Packet);
       Type == result; Type == error ->
	    ok;
       true ->
	    ejabberd_hooks:run(local_send_to_resource_hook,
			       To#jid.lserver, [Packet])
    end.

-spec route_iq(iq(), function()) -> ok.
route_iq(IQ, Fun) ->
    route_iq(IQ, Fun, undefined).

-spec route_iq(iq(), function(), undefined | non_neg_integer()) -> ok.
route_iq(IQ, Fun, Timeout) ->
    ejabberd_router:route_iq(IQ, Fun, undefined, Timeout).

-spec bounce_resource_packet(stanza()) -> ok | stop.
bounce_resource_packet(#presence{to = #jid{lresource = <<"">>}}) ->
    ok;
bounce_resource_packet(#message{to = #jid{lresource = <<"">>}, type = headline}) ->
    ok;
bounce_resource_packet(Packet) ->
    Lang = xmpp:get_lang(Packet),
    Txt = ?T("No available resource found"),
    Err = xmpp:err_item_not_found(Txt, Lang),
    ejabberd_router:route_error(Packet, Err),
    stop.

-spec get_features(binary()) -> [binary()].
get_features(Host) ->
    gen_iq_handler:get_features(?MODULE, Host).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    lists:foreach(fun host_up/1, ejabberd_option:hosts()),
    ejabberd_hooks:add(host_up, ?MODULE, host_up, 10),
    ejabberd_hooks:add(host_down, ?MODULE, host_down, 100),
    gen_iq_handler:start(?MODULE),
    update_table(),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({route, Packet}, State) ->
    route(Packet),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    lists:foreach(fun host_down/1, ejabberd_option:hosts()),
    ejabberd_hooks:delete(host_up, ?MODULE, host_up, 10),
    ejabberd_hooks:delete(host_down, ?MODULE, host_down, 100),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec update_table() -> ok.
update_table() ->
    catch mnesia:delete_table(iq_response),
    ok.

host_up(Host) ->
    Owner = case whereis(?MODULE) of
		undefined -> self();
		Pid -> Pid
	    end,
    ejabberd_router:register_route(Host, Host, {apply, ?MODULE, route}, Owner),
    ejabberd_hooks:add(local_send_to_resource_hook, Host,
		       ?MODULE, bounce_resource_packet, 100).

host_down(Host) ->
    Owner = case whereis(?MODULE) of
		undefined -> self();
		Pid -> Pid
	    end,
    ejabberd_router:unregister_route(Host, Owner),
    ejabberd_hooks:delete(local_send_to_resource_hook, Host,
			  ?MODULE, bounce_resource_packet, 100).
