%%%----------------------------------------------------------------------
%%% File    : gen_iq_handler.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : IQ handler support
%%% Created : 22 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(gen_iq_handler).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/3, add_iq_handler/6,
	 remove_iq_handler/3, stop_iq_handler/3, handle/7,
	 process_iq/6, check_type/1, transform_module_options/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-record(state, {host, module, function}).

-type component() :: ejabberd_sm | ejabberd_local.
-type type() :: no_queue | one_queue | pos_integer() | parallel.
-type opts() :: no_queue | {one_queue, pid()} | {queues, [pid()]} | parallel.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Module, Function) ->
    gen_server:start_link(?MODULE, [Host, Module, Function],
			  []).

-spec add_iq_handler(module(), binary(), binary(), module(), atom(), type()) -> any().

add_iq_handler(Component, Host, NS, Module, Function,
	       Type) ->
    case Type of
	no_queue ->
	    Component:register_iq_handler(Host, NS, Module,
		Function, no_queue);
	one_queue ->
	    {ok, Pid} = supervisor:start_child(ejabberd_iq_sup,
		    [Host, Module, Function]),
	    Component:register_iq_handler(Host, NS, Module,
		Function, {one_queue, Pid});
	N when is_integer(N) ->
	    Pids = lists:map(fun (_) ->
			    {ok, Pid} =
				supervisor:start_child(ejabberd_iq_sup,
				    [Host, Module,
					Function]),
			    Pid
		    end,
		    lists:seq(1, N)),
	    Component:register_iq_handler(Host, NS, Module,
		Function, {queues, Pids});
	parallel ->
	    Component:register_iq_handler(Host, NS, Module,
		Function, parallel)
    end.

-spec remove_iq_handler(component(), binary(), binary()) -> any().

remove_iq_handler(Component, Host, NS) ->
    Component:unregister_iq_handler(Host, NS).

-spec stop_iq_handler(atom(), atom(), [pid()]) -> any().

stop_iq_handler(_Module, _Function, Opts) ->
    case Opts of
      {one_queue, Pid} -> gen_server:call(Pid, stop);
      {queues, Pids} ->
	  lists:foreach(fun (Pid) ->
				catch gen_server:call(Pid, stop)
			end,
			Pids);
      _ -> ok
    end.

-spec handle(binary(), atom(), atom(), opts(), jid(), jid(), iq()) -> any().

handle(Host, Module, Function, Opts, From, To, IQ) ->
    case Opts of
	no_queue ->
	    process_iq(Host, Module, Function, From, To, IQ);
	{one_queue, Pid} ->
	    Pid ! {process_iq, From, To, IQ};
	{queues, Pids} ->
	    Pid = lists:nth(erlang:phash(p1_time_compat:unique_integer(),
                                         length(Pids)), Pids),
	    Pid ! {process_iq, From, To, IQ};
	parallel ->
	    spawn(?MODULE, process_iq,
		[Host, Module, Function, From, To, IQ]);
	_ -> todo
    end.

-spec process_iq(binary(), atom(), atom(), jid(), jid(), iq()) -> any().

process_iq(_Host, Module, Function, From, To, IQ0) ->
    IQ = xmpp:set_from_to(IQ0, From, To),
    try
	ResIQ = case erlang:function_exported(Module, Function, 1) of
		    true ->
			process_iq(Module, Function, IQ);
		    false ->
			process_iq(Module, Function, From, To,
				   jlib:iq_query_info(xmpp:encode(IQ)))
		end,
	if ResIQ /= ignore ->
		ejabberd_router:route(To, From, ResIQ);
	   true ->
		ok
	end
    catch E:R ->
	    ?ERROR_MSG("failed to process iq:~n~s~nReason = ~p",
		       [xmpp_codec:pp(IQ), {E, {R, erlang:get_stacktrace()}}]),
	    Txt = <<"Module failed to handle the query">>,
	    Err = xmpp:err_internal_server_error(Txt, IQ#iq.lang),
	    ejabberd_router:route(To, From, xmpp:make_error(IQ, Err))
    end.

-spec process_iq(module(), atom(), iq()) -> ignore | iq().
process_iq(Module, Function, #iq{lang = Lang, sub_els = [El]} = IQ) ->
    try
	%% TODO: move this 'conditional' decoding somewhere
	%% IQ handler should know *nothing* about vCards.
	Pkt = case xmpp:get_ns(El) of
		  ?NS_VCARD -> El;
		  _ -> xmpp:decode(El)
	      end,
	Module:Function(IQ#iq{sub_els = [Pkt]})
    catch error:{xmpp_codec, Why} ->
	    Txt = xmpp:format_error(Why),
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end.

-spec process_iq(module(), atom(), jid(), jid(), term()) -> iq().
process_iq(Module, Function, From, To, IQ) ->
    case Module:Function(From, To, IQ) of
	ignore -> ignore;
	ResIQ -> xmpp:decode(jlib:iq_to_xml(ResIQ), [ignore_els])
    end.

-spec check_type(type()) -> type().

check_type(no_queue) -> no_queue;
check_type(one_queue) -> one_queue;
check_type(N) when is_integer(N), N>0 -> N;
check_type(parallel) -> parallel.

-spec transform_module_options([{atom(), any()}]) -> [{atom(), any()}].

transform_module_options(Opts) ->
    lists:map(
      fun({iqdisc, {queues, N}}) ->
              {iqdisc, N};
         (Opt) ->
              Opt
      end, Opts).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Module, Function]) ->
    {ok,
     #state{host = Host, module = Module,
	    function = Function}}.

handle_call(stop, _From, State) ->
    Reply = ok, {stop, normal, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({process_iq, From, To, IQ},
	    #state{host = Host, module = Module,
		   function = Function} =
		State) ->
    process_iq(Host, Module, Function, From, To, IQ),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
