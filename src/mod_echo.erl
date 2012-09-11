%%%----------------------------------------------------------------------
%%% File    : mod_echo.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Simple ejabberd module.
%%% Created : 15 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_echo).

-author('alexey@process-one.net').

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start_link/2, start/2, stop/1,
	 do_client_version/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").

-include("jlib.hrl").

-record(state, {host = <<"">> :: binary()}).

-define(PROCNAME, ejabberd_mod_echo).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 temporary, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Opts]) ->
    MyHost = gen_mod:get_opt_host(Host, Opts,
				  <<"echo.@HOST@">>),
    ejabberd_router:register_route(MyHost),
    {ok, #state{host = MyHost}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, From, To, Packet}, State) ->
    Packet2 = case From#jid.user of
		<<"">> ->
		    jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST);
		_ -> Packet
	      end,
    do_client_version(disabled, To, From),
    ejabberd_router:route(To, From, Packet2),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%% Example of routing XMPP packets using Erlang's message passing
%%--------------------------------------------------------------------

%% To enable this educational example, edit the function handle_info:
%% replace the argument 'disabled' with 'enabled' in the call to the
%% function do_client_version.

do_client_version(disabled, _From, _To) -> ok;
do_client_version(enabled, From, To) ->
    ToS = jlib:jid_to_string(To),
    Random_resource =
	iolist_to_binary(integer_to_list(random:uniform(100000))),
    From2 = From#jid{resource = Random_resource,
		     lresource = Random_resource},
    Packet = #xmlel{name = <<"iq">>,
		    attrs = [{<<"to">>, ToS}, {<<"type">>, <<"get">>}],
		    children =
			[#xmlel{name = <<"query">>,
				attrs = [{<<"xmlns">>, ?NS_VERSION}],
				children = []}]},
    ejabberd_router:route(From2, To, Packet),
    Els = receive
	    {route, To, From2, IQ} ->
		#xmlel{name = <<"query">>, children = List} =
		    xml:get_subtag(IQ, <<"query">>),
		List
	    after 5000 -> % Timeout in miliseconds: 5 seconds
		      []
	  end,
    Values = [{Name, Value}
	      || #xmlel{name = Name, attrs = [],
			children = [{xmlcdata, Value}]}
		     <- Els],
    Values_string1 = [io_lib:format("~n~s: ~p", [N, V])
		      || {N, V} <- Values],
    Values_string2 = iolist_to_binary(Values_string1),
    ?INFO_MSG("Information of the client: ~s~s",
	      [ToS, Values_string2]).
