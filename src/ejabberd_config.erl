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
	    lists:foreach(fun process_term/1, Terms);
	{error, Reason} ->
	    exit(file:format_error(Reason))
    end.

process_term(Term) ->
    case Term of
	{acl, ACLName, ACLData} ->
	    acl:add(ACLName, ACLData);
	{Opt, Val} ->
	    ets:insert(ejabberd_config, {Opt, Val})
    end.


get_option(Opt) ->
    case ets:lookup(ejabberd_config, Opt) of
	[{_, Val}] ->
	    Val;
	_ ->
	    undefined
    end.



