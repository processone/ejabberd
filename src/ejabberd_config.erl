%%%----------------------------------------------------------------------
%%% File    : ejabberd_config.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 14 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_config).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0, load_file/1, get_option/1]).

-include("ejabberd.hrl").

start() ->
    ets:new(ejabberd_config, [named_table, public]),
    load_file(?CONFIG_PATH).


load_file(File) ->
    case file:consult(File) of
	{ok, Terms} ->
	    lists:foreach(fun({Opt, Val}) ->
				  ets:insert(ejabberd_config, {Opt, Val})
			  end, Terms);
	{error, Reason} ->
	    exit(file:format_error(Reason))
    end.


get_option(Opt) ->
    case ets:lookup(ejabberd_config, Opt) of
	[{_, Val}] ->
	    Val;
	_ ->
	    undefined
    end.



