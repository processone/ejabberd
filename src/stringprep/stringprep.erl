%%%----------------------------------------------------------------------
%%% File    : stringprep.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Interface to stringprep_drv
%%% Created : 16 Feb 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(stringprep).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_server).

-export([start/0, start_link/0,
	 tolower/1,
	 nameprep/1,
	 nodeprep/1,
	 resourceprep/1]).

%% Internal exports, call-back functions.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-define(NAMEPREP_COMMAND, 1).
-define(NODEPREP_COMMAND, 2).
-define(RESOURCEPREP_COMMAND, 3).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = erl_ddll:load_driver(ejabberd:get_so_path(), stringprep_drv),
    Port = open_port({spawn, stringprep_drv}, []),
    ets:new(stringprep_table, [set, public, named_table]),
    ets:insert(stringprep_table, {port, Port}),
    {ok, Port}.


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, Port) ->
    {noreply, Port};

handle_info({'EXIT', Port, Reason}, Port) ->
    {stop, {port_died, Reason}, Port};
handle_info(_, State) ->
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(_Reason, Port) ->
    Port ! {self, close},
    ok.



tolower(String) ->
    control(0, String).

nameprep(String) ->
    control(?NAMEPREP_COMMAND, String).

nodeprep(String) ->
    control(?NODEPREP_COMMAND, String).

resourceprep(String) ->
    control(?RESOURCEPREP_COMMAND, String).

control(Command, String) ->
    [{port, Port} | _] = ets:lookup(stringprep_table, port),
    case port_control(Port, Command, String) of
	[0 | _] -> error;
	[1 | Res] -> Res
    end.



