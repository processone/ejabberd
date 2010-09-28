%%%-------------------------------------------------------------------
%%% File    : mod_antiflood.erl
%%% Author  : Christophe Romain <cromain@process-one.net>
%%% Description : 
%%% Created : 12 Sep 2008 by Christophe Romain <cromain@process-one.net>
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
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
%%%-------------------------------------------------------------------
-module(mod_antiflood).
-author('cromain@process-one.net').

-behaviour(gen_mod).

%% API
-export([start/2, stop/1]).
-export([handler/4]).

-include("ejabberd.hrl").

%%====================================================================
%% API
%%====================================================================
start(Host, Opts) ->
    floodcheck:start_link(),
    lists:foreach(fun
		      ({listeners, Info, Op, Val}) ->
			  lists:foreach(fun({Name, Pid}) ->
						Id = {Name, Info},
						floodcheck:monitor(Id, Pid, Info, {Op, Val}, {?MODULE, handler})
					end, supervised_processes(listeners));
		      ({Module, Info, Op, Val}) ->
			  case module_pid(Host, Module) of
			      Pid when is_pid(Pid) ->
				  Id = {Host, Module, Info},
				  floodcheck:monitor(Id, Pid, Info, {Op, Val}, {?MODULE, handler});
			      Error ->
				  ?INFO_MSG("can not monitor ~s (~p)", [Module, Error])
			  end;
		      (Arg) ->
			  ?INFO_MSG("invalid argument: ~p", [Arg])
		  end, Opts).

stop(Host) ->
    MList = gen_mod:loaded_modules_with_opts(Host),
    case lists:keysearch(?MODULE, 1, MList) of
	{value, {?MODULE, Opts}} ->
	    lists:foreach(fun
			      ({Type, Info, _, _}) ->
				  case supervised_processes(Type) of
				      [] ->
					  Id = {Host, Type, Info},
					  floodcheck:demonitor(Id);
				      Childs ->
					  lists:foreach(fun({Name, _}) ->
								Id = {Name, Info},
								floodcheck:demonitor(Id)
						       end, Childs)
				  end;
			      (_) ->
				  ok
			  end, Opts);
	false ->
	    ok
    end,
    case floodcheck:check() of
	[] -> floodcheck:stop(), ok;
	_ -> ok
    end.
 
handler({Host, Module, Info}, Pid, Info, Value) ->
    ?WARNING_MSG("Flood alert on Process ~p (~s on ~s): ~s=~p", [Pid, Module, Host, Info, Value]),
    restart_module(Host, Module),
    remonitor({Host, Module, Info});
handler({Name, Info}, Pid, Info, Value) ->
    ?WARNING_MSG("Flood alert on Process ~p (~s): ~s=~p", [Pid, Name, Info, Value]),
    kill_process(Name, Pid),
    remonitor({Name, Info});
handler(Id, Pid, Info, Value) ->
    ?WARNING_MSG("Flood alert on Process ~p (~s): ~s=~p~nUnknown id, alert ignored", [Pid, Id, Info, Value]).

    
%%====================================================================
%% Internal functions
%%====================================================================

process_pid(Name) -> whereis(Name).
server_pid(Host, Name) -> process_pid(gen_mod:get_module_proc(Host, Name)).

module_pid(Host, mod_caps) -> server_pid(Host, ejabberd_mod_caps);
module_pid(Host, mod_ip_blacklist) -> server_pid(Host, mod_ip_blacklist);
module_pid(Host, mod_offline) -> server_pid(Host, ejabberd_offline);
module_pid(Host, mod_offline_odbc) -> server_pid(Host, ejabberd_offline);
module_pid(Host, mod_vcard) -> server_pid(Host, ejabberd_mod_vcard);
module_pid(Host, mod_vcard_odbc) -> server_pid(Host, ejabberd_mod_vcard);
module_pid(Host, mod_vcard_ldap) -> server_pid(Host, ejabberd_mod_vcard_ldap);
module_pid(Host, mod_irc) -> server_pid(Host, ejabberd_mod_irc);
module_pid(Host, mod_muc) -> server_pid(Host, ejabberd_mod_muc);
module_pid(Host, mod_muc_log) -> server_pid(Host, ejabberd_mod_muc_log);
module_pid(Host, mod_proxy65) -> server_pid(Host, ejabberd_mod_proxy65);
module_pid(Host, mod_proxy65_service) -> server_pid(Host, ejabberd_mod_proxy65_service);
module_pid(Host, mod_proxy65_sm) -> server_pid(Host, ejabberd_mod_proxy65_sm);
module_pid(Host, mod_pubsub) -> server_pid(Host, ejabberd_mod_pubsub);
module_pid(_, _) -> unsupported.

supervised_processes(listeners) ->
    {links, Links} = process_info(whereis(ejabberd_listeners), links),
    lists:map(fun(Pid) ->
		      {dictionary, Dict} = process_info(Pid, dictionary),
		      {_, _, [Port|_]} = proplists:get_value('$initial_call', Dict),
		      Name = list_to_atom("listener_"++integer_to_list(Port)),
		      {Name, Pid}
	      end, Links);
supervised_processes(_) -> [].

remonitor({Host, Module, Info}) ->
    MList = gen_mod:loaded_modules_with_opts(Host),
    case lists:keysearch(?MODULE, 1, MList) of
	{value, {?MODULE, Opts}} ->
	    lists:foreach(fun
			      ({M, I, Op, Val}) when M =:= Module, I =:= Info ->
				  case module_pid(Host, Module) of
				      Pid when is_pid(Pid) ->
					  Id = {Host, Module, Info},
					  floodcheck:monitor(Id, Pid, Info, {Op, Val}, {?MODULE, handler});
				      Error ->
					  ?INFO_MSG("can not monitor ~s (~p)", [Module, Error])
				  end;
			      (_) ->
				  ok
			  end, Opts);
	_ ->
	    ok
    end;
remonitor({Name, Info}) ->
    [Host|_] = ejabberd_config:get_global_option(hosts),
    MList = gen_mod:loaded_modules_with_opts(Host),
    case lists:keysearch(?MODULE, 1, MList) of
	{value, {?MODULE, Opts}} ->
	    lists:foreach(fun
			      ({Type, I, Op, Val}) when I =:= Info ->
				  lists:foreach(fun
						    ({N, Pid}) when N =:= Name ->
							Id = {Name, Info},
							floodcheck:monitor(Id, Pid, Info, {Op, Val}, {?MODULE, handler});
						    (_) ->
							ok
						end, supervised_processes(Type));
			      (_) ->
				  ok
			  end, Opts);
	_ ->
	    ok
    end;
remonitor(Id) ->
    ?INFO_MSG("can not monitor ~s", [Id]).

restart_module(Host, Module) ->
    MList = gen_mod:loaded_modules_with_opts(Host),
    case lists:keysearch(Module, 1, MList) of
	{value, {Module, Opts}} ->
	    ?WARNING_MSG("restarting module ~s on ~s", [Module, Host]),
	    gen_mod:stop_module(Host, Module),
	    gen_mod:start_module(Host, Module, Opts);
	_ ->
	    not_running
    end.

kill_process(Name, Pid) ->
    ?WARNING_MSG("killing process ~s(~p)", [Name, Pid]),
    exit(Pid, kill).
