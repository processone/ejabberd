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

-export([start/0, load_file/1,
	 add_global_option/2, add_local_option/2,
	 get_global_option/1, get_local_option/1]).

-include("ejabberd.hrl").

-record(config, {key, value}).
-record(local_config, {key, value}).

start() ->
    %ets:new(ejabberd_config, [named_table, public]),
    mnesia:create_table(config,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    mnesia:create_table(local_config,
			[{disc_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, local_config)}]),
    mnesia:add_table_copy(local_config, node(), ram_copies),
    load_file(?CONFIG_PATH).


load_file(File) ->
    case file:consult(File) of
	{ok, Terms} ->
	    lists:foreach(fun process_term/1, Terms);
	{error, Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    exit(file:format_error(Reason))
    end.

process_term(Term) ->
    case Term of
	{acl, ACLName, ACLData} ->
	    acl:add(ACLName, ACLData);
	{access, RuleName, Rules} ->
	    add_global_option({access, RuleName}, Rules);
	{Opt, Val} ->
	    add_option(Opt, Val)
    end.

add_option(Opt, Val) ->
    Table = case Opt of
		host ->
		    config;
		_ ->
		    local_config
	    end,
    case Table of
	config ->
	    add_global_option(Opt, Val);
	local_config ->
	    add_local_option(Opt, Val)
    end.

add_global_option(Opt, Val) ->
    mnesia:transaction(fun() ->
			       mnesia:write(#config{key = Opt,
						    value = Val})
		       end).

add_local_option(Opt, Val) ->
    mnesia:transaction(fun() ->
			       mnesia:write(#local_config{key = Opt,
							  value = Val})
		       end).


get_global_option(Opt) ->
    case ets:lookup(config, Opt) of
	[#config{value = Val}] ->
	    Val;
	_ ->
	    undefined
    end.

get_local_option(Opt) ->
    case ets:lookup(local_config, Opt) of
	[#local_config{value = Val}] ->
	    Val;
	_ ->
	    undefined
    end.



