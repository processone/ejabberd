%%%-------------------------------------------------------------------
%%% File    : stringprep_sup.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Description : Supervisor for the Stringprep worker.
%%%
%%% Created : 29 Jun 2007 by Mickael Remond <mremond@process-one.net>
%%%-------------------------------------------------------------------
-module(stringprep_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    StringPrep = {stringprep,
	      {stringprep, start_link, []},
	      permanent,
	      brutal_kill,
	      worker,
	      [stringprep]},
    {ok,{{one_for_all,10,1}, [StringPrep]}}.
