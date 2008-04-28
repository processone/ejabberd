%%%----------------------------------------------------------------------
%%% File    : ejabberd_config.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Load config file
%%% Created : 14 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_config).
-author('alexey@process-one.net').

-export([start/0, load_file/1,
	 add_global_option/2, add_local_option/2,
	 get_global_option/1, get_local_option/1]).
-export([get_vh_by_auth_method/1]).

-include("ejabberd.hrl").
-include("ejabberd_config.hrl").

start() ->
    mnesia:create_table(config,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    mnesia:create_table(local_config,
			[{disc_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, local_config)}]),
    mnesia:add_table_copy(local_config, node(), ram_copies),
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
	    State = lists:foldl(fun search_hosts/2, #state{}, Terms),
	    Res = lists:foldl(fun process_term/2, State, Terms),
	    set_opts(Res);
	{error, Reason} ->
	    ?ERROR_MSG("Can't load config file ~p: ~p", [File, Reason]),
	    exit(File ++ ": " ++ file:format_error(Reason))
    end.

search_hosts(Term, State) ->
    case Term of
	{host, Host} ->
	    if
		State#state.hosts == [] ->
		    add_hosts_to_option([Host], State);
		true ->
		    ?ERROR_MSG("Can't load config file: "
			       "too many hosts definitions", []),
		    exit("too many hosts definitions")
	    end;
	{hosts, Hosts} ->
	    if
		State#state.hosts == [] ->
		    add_hosts_to_option(Hosts, State);
		true ->
		    ?ERROR_MSG("Can't load config file: "
			       "too many hosts definitions", []),
		    exit("too many hosts definitions")
	    end;
	_ ->
	    State
    end.

add_hosts_to_option(Hosts, State) ->
    PrepHosts = normalize_hosts(Hosts),
    add_option(hosts, PrepHosts, State#state{hosts = PrepHosts}).

normalize_hosts(Hosts) ->
    normalize_hosts(Hosts,[]).
normalize_hosts([], PrepHosts) ->
    lists:reverse(PrepHosts);
normalize_hosts([Host|Hosts], PrepHosts) ->
    case jlib:nodeprep(Host) of
	error ->
	    ?ERROR_MSG("Can't load config file: "
		       "invalid host name [~p]", [Host]),
	    exit("invalid hostname");
	PrepHost ->
	    normalize_hosts(Hosts, [PrepHost|PrepHosts])
    end.

process_term(Term, State) ->
    case Term of
	override_global ->
	    State#state{override_global = true};
	override_local ->
	    State#state{override_local = true};
	override_acls ->
	    State#state{override_acls = true};
	{acl, _ACLName, _ACLData} ->
	    process_host_term(Term, global, State);
	{access, _RuleName, _Rules} ->
	    process_host_term(Term, global, State);
	{shaper, _Name, _Data} ->
	    %%lists:foldl(fun(Host, S) -> process_host_term(Term, Host, S) end,
	    %%    	State, State#state.hosts);
	    process_host_term(Term, global, State);
	{host, _Host} ->
	    State;
	{hosts, _Hosts} ->
	    State;
	{host_config, Host, Terms} ->
	    lists:foldl(fun(T, S) -> process_host_term(T, Host, S) end,
			State, Terms);
	{listen, Val} ->
	    add_option(listen, Val, State);
	{language, Val} ->
	    add_option(language, Val, State);
	{outgoing_s2s_port, Port} ->
	    add_option(outgoing_s2s_port, Port, State);
	{s2s_use_starttls, Port} ->
	    add_option(s2s_use_starttls, Port, State);
	{s2s_certfile, CertFile} ->
	    add_option(s2s_certfile, CertFile, State);
	{domain_certfile, Domain, CertFile} ->
	    add_option({domain_certfile, Domain}, CertFile, State);
	{node_type, NodeType} ->
	    add_option(node_type, NodeType, State);
	{cluster_nodes, Nodes} ->
	    add_option(cluster_nodes, Nodes, State);
	{domain_balancing, Domain, Balancing} ->
	    add_option({domain_balancing, Domain}, Balancing, State);
	{domain_balancing_component_number, Domain, N} ->
	    add_option({domain_balancing_component_number, Domain}, N, State);
	{watchdog_admins, Admins} ->
	    add_option(watchdog_admins, Admins, State);
	{registration_timeout, Timeout} ->
	    add_option(registration_timeout, Timeout, State);
	{loglevel, Loglevel} ->
	    ejabberd_loglevel:set(Loglevel),
	    State;
	{_Opt, _Val} ->
	    lists:foldl(fun(Host, S) -> process_host_term(Term, Host, S) end,
			State, State#state.hosts)
    end.

process_host_term(Term, Host, State) ->
    case Term of
	{acl, ACLName, ACLData} ->
	    State#state{opts =
			[acl:to_record(Host, ACLName, ACLData) | State#state.opts]};
	{access, RuleName, Rules} ->
	    State#state{opts = [#config{key = {access, RuleName, Host},
					value = Rules} |
				State#state.opts]};
	{shaper, Name, Data} ->
	    State#state{opts = [#config{key = {shaper, Name, Host},
					value = Data} |
				State#state.opts]};
	{host, Host} ->
	    State;
	{hosts, _Hosts} ->
	    State;
	{odbc_server, ODBC_server} ->
	    add_option({odbc_server, Host}, ODBC_server, State);
	{Opt, Val} ->
	    add_option({Opt, Host}, Val, State)
    end.

add_option(Opt, Val, State) ->
    Table = case Opt of
		hosts ->
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
	    case Opt of
		{{add, OptName}, Host} ->
		    State#state{opts = compact({OptName, Host}, Val,
					       State#state.opts, [])};
		_ ->
		    State#state{opts = [#local_config{key = Opt, value = Val} |
					State#state.opts]}
	    end
    end.

compact({OptName, Host} = Opt, Val, [], Os) ->
    ?WARNING_MSG("The option '~p' is defined for the host ~p using host_config "
    "before the global '~p' option. This host_config option may get overwritten.", [OptName, Host, OptName]),
    [#local_config{key = Opt, value = Val}] ++ Os;
%% Traverse the list of the options already parsed
compact(Opt, Val, [O | Os1], Os2) ->
    case catch O#local_config.key of
	%% If the key of a local_config matches the Opt that wants to be added
	Opt ->
	    %% Then prepend the new value to the list of old values
	    Os2 ++ [#local_config{key = Opt,
				  value = Val++O#local_config.value}
		   ] ++ Os1;
	_ ->
	    compact(Opt, Val, Os1, Os2++[O])
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
    case mnesia:transaction(F) of
	{atomic, _} -> ok;
	{aborted,{no_exists,Table}} ->
	    MnesiaDirectory = mnesia:system_info(directory),
	    ?ERROR_MSG("Error reading Mnesia database spool files:~n"
		       "The Mnesia database couldn't read the spool file for the table '~p'.~n"
		       "ejabberd needs read and write access in the directory:~n   ~s~n"
		       "Maybe the problem is a change in the computer hostname,~n"
		       "or a change in the Erlang node name, which is currently:~n   ~p~n"
		       "Check the ejabberd guide for details about changing the~n"
		       "computer hostname or Erlang node name.~n",
		       [Table, MnesiaDirectory, node()]),
	    exit("Error reading Mnesia database")
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

%% Return the list of hosts handled by a given module
get_vh_by_auth_method(AuthMethod) ->
    mnesia:dirty_select(local_config,
			[{#local_config{key = {auth_method, '$1'},
					value=AuthMethod},[],['$1']}]).
