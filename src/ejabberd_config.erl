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
-record(state, {opts = [],
		override_local = false,
		override_global = false,
		override_acls = false}).

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

    %% mremond: Config file can be configured from the command line
    Config = case application:get_env(config) of
		 {ok, Path} -> Path;
		 undefined -> 
		     case os:getenv("EJABBERD_CONFIG_PATH") of
			 false ->
			     ?CONFIG_PATH;
			 Path ->
			     Path
		     end
	     end,
    load_file(Config).


load_file(File) ->
    case file:consult(File) of
	{ok, Terms} ->
	    Res = lists:foldl(fun process_term/2, #state{}, Terms),
	    set_opts(Res);
	{error, Reason} ->
	    ?ERROR_MSG("Can't load config file ~p: ~p", [File, Reason]),
	    exit(file:format_error(Reason))
    end.

process_term(Term, State) ->
    case Term of
	override_global ->
	    State#state{override_global = true};
	override_local ->
	    State#state{override_local = true};
	override_acls ->
	    State#state{override_acls = true};
	{acl, ACLName, ACLData} ->
	    State#state{opts =
		   [acl:to_record(ACLName, ACLData) | State#state.opts]};
	{access, RuleName, Rules} ->
	    State#state{opts = [#config{key = {access, RuleName},
					value = Rules} |
				State#state.opts]};
	{shaper, Name, Data} ->
	    State#state{opts = [#config{key = {shaper, Name},
					value = Data} |
				State#state.opts]};
	{Opt, Val} ->
	    add_option(Opt, Val, State)
    end.

add_option(Opt, Val, State) ->
    Table = case Opt of
		host ->
		    config;
		language ->
		    config;
		_ ->
		    local_config
	    end,
    case Table of
	config ->
	    State#state{opts = [#config{key = Opt, value = Val} |
				State#state.opts]};
	local_config ->
	    State#state{opts = [#local_config{key = Opt, value = Val} |
				State#state.opts]}
    end.


set_opts(State) ->
    Opts = lists:reverse(State#state.opts),
    F = fun() ->
		if
		    State#state.override_global ->
			Ksg = mnesia:all_keys(config),
			lists:foreach(fun(K) ->
					      mnesia:delete({config, K})
				      end, Ksg);
		    true ->
			ok
		end,
		if
		    State#state.override_local ->
			Ksl = mnesia:all_keys(local_config),
			lists:foreach(fun(K) ->
					      mnesia:delete({local_config, K})
				      end, Ksl);
		    true ->
			ok
		end,
		if
		    State#state.override_acls ->
			Ksa = mnesia:all_keys(acl),
			lists:foreach(fun(K) ->
					      mnesia:delete({acl, K})
				      end, Ksa);
		    true ->
			ok
		end,
		lists:foreach(fun(R) ->
				      mnesia:write(R)
			      end, Opts)
	end,
    {atomic, _} = mnesia:transaction(F).


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


