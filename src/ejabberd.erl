%%%----------------------------------------------------------------------
%%% File    : ejabberd.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0, init/0]).

-include("ejabberd.hrl").

start() ->
    spawn(?MODULE, init, []).

init() ->
    register(ejabberd, self()),
    %erlang:system_flag(fullsweep_after, 0),
    error_logger:logfile({open, ?ERROR_LOG_PATH}),
    randoms:start(),
    ok = erl_ddll:load_driver(".", expat_erl),
    Port = open_port({spawn, expat_erl}, [binary]),
    db_init(),
    sha:start(),
    translate:start(),
    acl:start(),
    ejabberd_config:start(),
    ejabberd_auth:start(),
    ejabberd_router:start(),
    ejabberd_sm:start(),
    ejabberd_s2s:start(),
    ejabberd_local:start(),
    ejabberd_listener:start(),
    load_modules(),
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
    case ejabberd_config:get_local_option(modules) of
	undefined ->
	    ok;
	Modules ->
	    lists:foreach(fun({Module, Args}) ->
				  gen_mod:start_module(Module, Args)
			  end, Modules)
    end.

