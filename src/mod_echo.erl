%%%----------------------------------------------------------------------
%%% File    : mod_echo.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Simple ejabberd module.
%%% Created : 15 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(mod_echo).

-author('alexey@process-one.net').

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start_link/2, start/2, stop/1,
	 do_client_version/3]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-record(state, {host = <<"">> :: binary()}).

-define(PROCNAME, ejabberd_mod_echo).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    MyHost = gen_mod:get_opt_host(Host, Opts,
				  <<"echo.@HOST@">>),
    ejabberd_router:register_route(MyHost),
    {ok, #state{host = MyHost}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host), ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%% Example of routing XMPP packets using Erlang's message passing
%%--------------------------------------------------------------------

%% To enable this educational example, edit the function handle_info:
%% replace the argument 'disabled' with 'enabled' in the call to the
%% function do_client_version.

%% ejabberd provides a method to receive XMPP packets using Erlang's 
%% message passing mechanism. 
%%
%% The packets received by ejabberd are sent
%% to the local destination process by sending an Erlang message.
%% This means that you can receive XMPP stanzas in an Erlang process 
%% using Erlang's Receive, as long as this process is registered in
%% ejabberd as the process which handles the destination JID.
%%
%% This example function is called when a client queries the echo service.
%% This function then sends a query to the client, and waits 5 seconds to
%% receive an answer. The answer will only be accepted if it was sent
%% using exactly the same JID. We add a (mostly) random resource to
%% try to guarantee that the received response matches the request sent.
%% Finally, the received response is printed in the ejabberd log file.
do_client_version(disabled, _From, _To) -> ok;
do_client_version(enabled, From, To) ->
    ToS = jid:to_string(To),
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

mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(_) -> [host].
