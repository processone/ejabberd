%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 24 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(gen_mod).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0,
	 start_module/2,
	 stop_module/1,
	 get_opt/2,
	 get_opt/3,
	 loaded_modules/0]).

-export([behaviour_info/1]).

-include("ejabberd.hrl").

-record(ejabberd_module, {module, opts}).

behaviour_info(callbacks) ->
    [{start, 1},
     {stop, 0}];
behaviour_info(Other) ->
    undefined.

start() ->
    ets:new(ejabberd_modules, [named_table,
			       public,
			       {keypos, #ejabberd_module.module}]),
    ok.


start_module(Module, Opts) ->
    case catch Module:start(Opts) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ets:insert(ejabberd_modules, #ejabberd_module{module = Module,
							  opts = Opts}),
	    ok
    end.

stop_module(Module) ->
    case catch Module:stop() of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ets:delete(ejabberd_modules, Module),
	    ok
    end.


get_opt(Opt, Opts) ->
    case lists:keysearch(Opt, 1, Opts) of
	false ->
	    % TODO: replace with more appropriate function
	    [] = {undefined_option, Opt};
	{value, {_, Val}} ->
	    Val
    end.

get_opt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	false ->
	    Default;
	{value, {_, Val}} ->
	    Val
    end.

loaded_modules() ->
    ets:select(ejabberd_modules,
	       [{#ejabberd_module{_ = '_', module = '$1'}, [],['$1']}]).
