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

-export([start_module/2,
	 stop_module/1,
	 get_opt/2,
	 get_opt/3]).

-export([behaviour_info/1]).

-include("ejabberd.hrl").


behaviour_info(callbacks) ->
    [{start, 1},
     {stop, 0}];
behaviour_info(Other) ->
    undefined.


start_module(Module, Opts) ->
    case catch Module:start(Opts) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ok
    end.

stop_module(Module) ->
    Module:stop().


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

