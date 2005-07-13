%%%----------------------------------------------------------------------
%%% File    : ejabberd_app.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_app).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(application).

-export([start/2, stop/1, init/0]).

-export([dump_ports/0]).

-include("ejabberd.hrl").

start(normal, _Args) ->
    application:start(sasl),
    randoms:start(),
    db_init(),
    sha:start(),
    catch ssl:start(),
    translate:start(),
    acl:start(),
    gen_mod:start(),
    ejabberd_config:start(),
    Sup = ejabberd_sup:start_link(),
    ejabberd_auth:start(),
    cyrsasl:start(),
    % Profiling
    %eprof:start(),
    %eprof:profile([self()]),
    %fprof:trace(start, "/tmp/fprof"),
    start(),
    load_modules(),
    Sup;
start(_, _) ->
    {error, badarg}.

stop(_StartArgs) ->
    ok.

start() ->
    spawn_link(?MODULE, init, []).

init() ->
    register(ejabberd, self()),
    %erlang:system_flag(fullsweep_after, 0),
    %error_logger:logfile({open, ?LOG_PATH}),
    LogPath =
	case application:get_env(log_path) of
            {ok, Path} ->
		Path;
	    undefined ->
		case os:getenv("EJABBERD_LOG_PATH") of
		    false ->
			?LOG_PATH;
		    Path ->
			Path
		end
	end,
    error_logger:add_report_handler(ejabberd_logger_h, LogPath),
    %timer:apply_interval(3600000, ?MODULE, dump_ports, []),
    ok = erl_ddll:load_driver(ejabberd:get_so_path(), expat_erl),
    Port = open_port({spawn, expat_erl}, [binary]),
    loop(Port).


loop(Port) ->
    receive
	_ ->
	    loop(Port)
    end.

db_init() ->
    case mnesia:system_info(extra_db_nodes) of
	[] ->
	    mnesia:create_schema([node()]);
	_ ->
	    ok
    end,
    mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

load_modules() ->
    lists:foreach(
      fun(Host) ->
	      case ejabberd_config:get_local_option({modules, Host}) of
		  undefined ->
		      ok;
		  Modules ->
		      lists:foreach(
			fun({Module, Args}) ->
				gen_mod:start_module(Host, Module, Args)
			end, Modules)
	      end
      end, ?MYHOSTS).


dump_ports() ->
    ?INFO_MSG("ports:~n ~p",
	      [lists:map(fun(P) -> erlang:port_info(P) end, erlang:ports())]).

