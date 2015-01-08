%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Purpose :
%%% Created : 24 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(gen_mod).

-author('alexey@process-one.net').

-export([start/0, start_module/2, start_module/3, stop_module/2,
	 stop_module_keep_config/2, get_opt/3, get_opt/4,
	 get_opt_host/3, db_type/1, db_type/2, get_module_opt/5,
	 get_module_opt_host/3, loaded_modules/1,
	 loaded_modules_with_opts/1, get_hosts/2,
	 get_module_proc/2, is_loaded/2]).

%%-export([behaviour_info/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(ejabberd_module,
        {module_host = {undefined, <<"">>} :: {atom(), binary()},
         opts = [] :: opts() | '_' | '$2'}).

-type opts() :: [{atom(), any()}].

-callback start(binary(), opts()) -> any().
-callback stop(binary()) -> any().

-export_type([opts/0]).

%%behaviour_info(callbacks) -> [{start, 2}, {stop, 1}];
%%behaviour_info(_Other) -> undefined.

start() ->
    ets:new(ejabberd_modules,
	    [named_table, public,
	     {keypos, #ejabberd_module.module_host}]),
    ok.

-spec start_module(binary(), atom()) -> any().

start_module(Host, Module) ->
    Modules = ejabberd_config:get_option(
		{modules, Host},
		fun(L) when is_list(L) -> L end, []),
    case lists:keyfind(Module, 1, Modules) of
	{_, Opts} ->
	    start_module(Host, Module, Opts);
	false ->
	    {error, not_found_in_config}
    end.

-spec start_module(binary(), atom(), opts()) -> any().

start_module(Host, Module, Opts) ->
    ets:insert(ejabberd_modules,
	       #ejabberd_module{module_host = {Module, Host},
				opts = Opts}),
    try Module:start(Host, Opts) catch
      Class:Reason ->
	  ets:delete(ejabberd_modules, {Module, Host}),
	  ErrorText =
	      io_lib:format("Problem starting the module ~p for host "
			    "~p ~n options: ~p~n ~p: ~p~n~p",
			    [Module, Host, Opts, Class, Reason,
			     erlang:get_stacktrace()]),
	  ?CRITICAL_MSG(ErrorText, []),
	  case is_app_running(ejabberd) of
	    true ->
		erlang:raise(Class, Reason, erlang:get_stacktrace());
	    false ->
		?CRITICAL_MSG("ejabberd initialization was aborted "
			      "because a module start failed.",
			      []),
		timer:sleep(3000),
		erlang:halt(string:substr(lists:flatten(ErrorText), 1, 199))
	  end
    end.

is_app_running(AppName) ->
    Timeout = 15000,
    lists:keymember(AppName, 1,
		    application:which_applications(Timeout)).

-spec stop_module(binary(), atom()) -> error | {aborted, any()} | {atomic, any()}.

%% @doc Stop the module in a host, and forget its configuration.
stop_module(Host, Module) ->
    case stop_module_keep_config(Host, Module) of
      error -> error;
      ok -> ok
    end.

%% @doc Stop the module in a host, but keep its configuration.
%% As the module configuration is kept in the Mnesia local_config table,
%% when ejabberd is restarted the module will be started again.
%% This function is useful when ejabberd is being stopped
%% and it stops all modules.
-spec stop_module_keep_config(binary(), atom()) -> error | ok.

stop_module_keep_config(Host, Module) ->
    case catch Module:stop(Host) of
      {'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]), error;
      {wait, ProcList} when is_list(ProcList) ->
	  lists:foreach(fun wait_for_process/1, ProcList),
	  ets:delete(ejabberd_modules, {Module, Host}),
	  ok;
      {wait, Process} ->
	  wait_for_process(Process),
	  ets:delete(ejabberd_modules, {Module, Host}),
	  ok;
      _ -> ets:delete(ejabberd_modules, {Module, Host}), ok
    end.

wait_for_process(Process) ->
    MonitorReference = erlang:monitor(process, Process),
    wait_for_stop(Process, MonitorReference).

wait_for_stop(Process, MonitorReference) ->
    receive
      {'DOWN', MonitorReference, _Type, _Object, _Info} -> ok
      after 5000 ->
		catch exit(whereis(Process), kill),
		wait_for_stop1(MonitorReference)
    end.

wait_for_stop1(MonitorReference) ->
    receive
      {'DOWN', MonitorReference, _Type, _Object, _Info} -> ok
      after 5000 -> ok
    end.

-type check_fun() :: fun((any()) -> any()) | {module(), atom()}.

-spec get_opt(atom(), opts(), check_fun()) -> any().

get_opt(Opt, Opts, F) ->
    get_opt(Opt, Opts, F, undefined).

-spec get_opt(atom(), opts(), check_fun(), any()) -> any().

get_opt(Opt, Opts, F, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
        false ->
            Default;
        {value, {_, Val}} ->
            ejabberd_config:prepare_opt_val(Opt, Val, F, Default)
    end.

-spec get_module_opt(global | binary(), atom(), atom(), check_fun(), any()) -> any().

get_module_opt(global, Module, Opt, F, Default) ->
    Hosts = (?MYHOSTS),
    [Value | Values] = lists:map(fun (Host) ->
					 get_module_opt(Host, Module, Opt,
							F, Default)
				 end,
				 Hosts),
    Same_all = lists:all(fun (Other_value) ->
				 Other_value == Value
			 end,
			 Values),
    case Same_all of
      true -> Value;
      false -> Default
    end;
get_module_opt(Host, Module, Opt, F, Default) ->
    OptsList = ets:lookup(ejabberd_modules, {Module, Host}),
    case OptsList of
      [] -> Default;
      [#ejabberd_module{opts = Opts} | _] ->
	  get_opt(Opt, Opts, F, Default)
    end.

-spec get_module_opt_host(global | binary(), atom(), binary()) -> binary().

get_module_opt_host(Host, Module, Default) ->
    Val = get_module_opt(Host, Module, host,
                         fun iolist_to_binary/1,
                         Default),
    ejabberd_regexp:greplace(Val, <<"@HOST@">>, Host).

-spec get_opt_host(binary(), opts(), binary()) -> binary().

get_opt_host(Host, Opts, Default) ->
    Val = get_opt(host, Opts, fun iolist_to_binary/1, Default),
    ejabberd_regexp:greplace(Val, <<"@HOST@">>, Host).

-spec db_type(opts()) -> odbc | mnesia | riak.

db_type(Opts) ->
    get_opt(db_type, Opts,
            fun(odbc) -> odbc;
               (internal) -> mnesia;
               (mnesia) -> mnesia;
               (riak) -> riak
            end,
            mnesia).

-spec db_type(binary(), atom()) -> odbc | mnesia | riak.

db_type(Host, Module) ->
    get_module_opt(Host, Module, db_type,
                   fun(odbc) -> odbc;
                      (internal) -> mnesia;
                      (mnesia) -> mnesia;
                      (riak) -> riak
                   end,
                   mnesia).

-spec loaded_modules(binary()) -> [atom()].

loaded_modules(Host) ->
    ets:select(ejabberd_modules,
	       [{#ejabberd_module{_ = '_', module_host = {'$1', Host}},
		 [], ['$1']}]).

-spec loaded_modules_with_opts(binary()) -> [{atom(), opts()}].

loaded_modules_with_opts(Host) ->
    ets:select(ejabberd_modules,
	       [{#ejabberd_module{_ = '_', module_host = {'$1', Host},
				  opts = '$2'},
		 [], [{{'$1', '$2'}}]}]).

-spec get_hosts(opts(), binary()) -> [binary()].

get_hosts(Opts, Prefix) ->
    case get_opt(hosts, Opts,
                 fun(Hs) -> [iolist_to_binary(H) || H <- Hs] end) of
        undefined ->
            case get_opt(host, Opts,
                         fun iolist_to_binary/1) of
                undefined ->
                    [<<Prefix/binary, Host/binary>> || Host <- ?MYHOSTS];
                Host ->
                    [Host]
            end;
        Hosts ->
            Hosts
    end.

-spec get_module_proc(binary(), {frontend, atom()} | atom()) -> atom().

get_module_proc(Host, {frontend, Base}) ->
    get_module_proc(<<"frontend_", Host/binary>>, Base);
get_module_proc(Host, Base) ->
    binary_to_atom(
      <<(erlang:atom_to_binary(Base, latin1))/binary, "_", Host/binary>>,
      latin1).

-spec is_loaded(binary(), atom()) -> boolean().

is_loaded(Host, Module) ->
    ets:member(ejabberd_modules, {Module, Host}).
