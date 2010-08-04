%%%-------------------------------------------------------------------
%%% File    : ejabberd_hosts.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Description : Synchronises running VHosts with the hosts table in the Mnesia database.
%%% Created : 16 Nov 2007 by Alexey Shchepin <alexey@process-one.net>
%%%-------------------------------------------------------------------

%%% Database schema (version / storage / table)
%%%
%%% 3.0.0-alpha-x / mnesia / hosts
%%%  host = string()
%%%  clusterid = integer()
%%%  config = string() 
%%%
%%% 3.0.0-alpha-x / odbc / hosts
%%%  host = varchar150
%%%  clusterid = integer
%%%  config = text 

-module(ejabberd_hosts).

-behaviour(gen_server).

%% External API
-export([start_link/0
         ,reload/0
        ]).

%% Host Registration API
-export([register/1
         ,register/2
         ,registered/1
         ,running/1
         ,registered/0
         ,remove/1
        ]).

%% Host control API
-export([start_host/1
         ,start_hosts/1
         ,stop_host/1
         ,stop_hosts/1
         ,load_host_cert/2
         ]).

%% Private utility functions
-export([get_hosts/1
         ,config_from_string/2
         ,get_host_config/2
         ,diff_hosts/0
         ,diff_hosts/1
         ,diff_hosts/2
         ,reload_hosts/0
         ,delete_host_config/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("ejabberd_config.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {state=wait_odbc,
                backend=mnesia,
                odbc_wait_time=120}).
-record(hosts, {host, clusterid, config}).

-define(RELOAD_INTERVAL, timer:seconds(60)).
-define(ODBC_STARTUP_TIME, 120). % 2minute limit for ODBC startup.

%%====================================================================
%% API
%%====================================================================

reload() ->
    ?MODULE ! reload.

%% Creates a vhost in the system.
register(Host) when is_list(Host) -> ?MODULE:register(Host, "").
register(Host, Config) when is_list(Host), is_list(Config) ->
    true = exmpp_stringprep:is_node(Host),
    ID = get_clusterid(),
    H = #hosts{host = Host, clusterid = ID, config = Config},
    ok = gen_storage:dirty_write(Host, H),
    reload(),
    ok.

%% Removes a vhost from the system,
%% XXX deleting all ODBC data.
remove(Host) when is_list(Host) ->
    true = exmpp_stringprep:is_node(Host),
    ID = get_clusterid(),
    gen_storage:dirty_delete_where(
	Host, hosts,
	[{'andalso',
	    {'==', clusterid, ID},
	    {'==', host, Host}}]),
    reload(),
    ok.

registered() ->
    mnesia:dirty_select(local_config,
                        ets:fun2ms(fun (#local_config{key={Host, host}}) ->
                                           Host
                                   end)).

registered(Host) when is_list(Host) ->
    case mnesia:dirty_read({local_config, {Host, host}}) of
        [{local_config, {Host, host}, _}] -> true;
        [] -> false
    end.

running(global) -> true;
running(Host) ->
    Routes = [H
              || {H, _, {apply, ejabberd_local, route}} <- ejabberd_router:read_route(Host),
                 H =:= Host],
    Routes =/= [].


load_host_cert(Host, PemData) ->
    File = cert_filename(Host),
    ok = file:write_file(File, PemData),
    configure_host_cert(Host, File).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Backend = mnesia, %%+++ TODO: allow to configure this in ejabberd.cfg
    configure_static_hosts(),
    get_clusterid(), %% this is to report an error if the option wasn't configured
    %% Wait up to 120 seconds for odbc to start
    {ok, #state{state=wait_odbc,backend=Backend,odbc_wait_time=?ODBC_STARTUP_TIME}, timer:seconds(1)}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% Wait for odbc to start.
handle_info(timeout, State = #state{state=wait_odbc,backend=Backend,odbc_wait_time=N}) when N > 0 ->
    case (Backend /= odbc) orelse ejabberd_odbc:running(?MYNAME) of
        true ->
            ?DEBUG("ejabberd_hosts: odbc now running.",[]),

	    %% The table 'hosts' is defined in gen_storage as being for the "localhost" vhost
	    Host = ?MYNAME,
	    HostB = list_to_binary(Host),
	    gen_storage:create_table(Backend, HostB, hosts,
				     [{disc_only_copies, [node()]},
				      {odbc_host, Host},
				      {attributes, record_info(fields, hosts)},
				      {types, [{host, text},
					       {clusterid, int},
					       {config, text}]}]),

	    %% Now let's add the default vhost: "localhost"
	    gen_storage:dirty_write(Host, #hosts{host = Host,
							clusterid = 1,
							config = ""}),

            self() ! reload,
            timer:send_interval(?RELOAD_INTERVAL, reload),
            {noreply, State#state{state=running,odbc_wait_time=0}};
        false ->
            {noreply,State#state{odbc_wait_time=N-1},timer:seconds(1)}
    end;
handle_info(timeout, State=#state{state=running}) ->
    ?WARNING_MSG("Spurious timeout message when odbc is already running.", []),
    {noreply, State};

handle_info(reload, State = #state{state=running}) ->
    try reload_hosts()
    catch
        Class:Error ->
            StackTrace = erlang:get_stacktrace(),
            ?ERROR_MSG("~p while synchonising running vhosts with database: ~p~n~p", [Class, Error, StackTrace])
    end,
    {noreply, State};
handle_info(reload, State = #state{state=wait_odbc}) ->
    ?ERROR_MSG("Tried to reload vhosts while waiting for odbc startup.", []),
    handle_info(timeout, State);

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

reload_hosts() ->
    reload_hosts(get_hosts(odbc)).

reload_hosts(NewHosts) ->
    {AddedHosts,RemovedHosts} = diff_hosts(NewHosts),
    %% Avoid removing permanent hosts (staticly configured hosts)
    DeletedHosts = [H || H <- RemovedHosts,
                         ejabberd_config:get_host_option(H, permanent) =/= true],
    AddHostConfig = lists:map(fun (Host) ->
                                      {Host, get_host_config(odbc,Host)}
                              end, AddedHosts),
    update_config(AddHostConfig,DeletedHosts),
    ejabberd_config:add_global_option(hosts, NewHosts), % overwrite hosts list
    stop_hosts(DeletedHosts),
    start_hosts(AddedHosts),
    ejabberd_local:refresh_iq_handlers(),
    {DeletedHosts, AddedHosts}.

%% Apply the vhost changes (new, removed vhosts, configuration) to mnesia
update_config(AddHostConfig, RemoveHosts) ->
    F = fun() ->
                mnesia:write_lock_table(local_config),
                lists:foreach(fun configure_new_host/1, AddHostConfig),
                lists:foreach(fun delete_host_config/1, RemoveHosts),
                ok
        end,
    {atomic, ok} = mnesia:transaction(F).

%% Write the configuration data for a new vhost to mnesia (currently only modules)
configure_new_host({H, Config}) when is_list(Config) ->
    reconfigure_host_cert(H),
    ejabberd_config:configure_host(H, Config).

%% Delete the mnesia config data for a vhost.
%% Needs to mirror any data we insert in configure_new_host (currently only modules)
delete_host_config(Host) ->
    ejabberd_config:delete_host(Host).

%% Startup a list of vhosts
start_hosts([]) -> ok;
start_hosts(AddHosts) when is_list(AddHosts) ->
    ?DEBUG("ejabberd_hosts adding hosts: ~P", [AddHosts, 10]),
    lists:foreach(fun start_host/1, AddHosts).

%% Start a single vhost (route, modules)
start_host(Host) ->
    ?DEBUG("Starting host ~p", [Host]),
    ejabberd_router:register_route(Host, {apply, ejabberd_local, route}),
    case ejabberd_config:get_local_option({modules, Host}) of
        undefined -> ok;
        Modules when is_list(Modules) ->
            lists:foreach(
              fun({Module, Args}) ->
                      gen_mod:start_module(Host, Module, Args)
              end, Modules)
    end,
    case auth_method(Host) of
        {host_method, HostMethod} ->
            ejabberd_auth:start_method(Host, HostMethod);
        _ -> ignore
    end,
    ok.


%% Shut down a list of vhosts.
stop_hosts([]) -> ok;
stop_hosts(RemoveHosts) when is_list(RemoveHosts)->
    ?DEBUG("ejabberd_hosts removing hosts: ~p", [RemoveHosts]),
    lists:foreach(fun stop_host/1, RemoveHosts).

%% Shut down a single vhost. (Routes, modules)
stop_host(Host) ->
    ?DEBUG("Stopping host ~p", [Host]),
    ejabberd_router:force_unregister_route(Host),
    lists:foreach(fun(Module) ->
                          gen_mod:stop_module(Host, Module)
                  end, gen_mod:loaded_modules(Host)),
    case auth_method(Host) of
        use_global_auth_method -> ok;
        {host_method, Method} ->
            ejabberd_auth:stop_method(Host, Method)
    end.

%% Get the current vhost list from a variety of sources (ODBC, internal)
get_hosts(ejabberd) -> ?MYHOSTS;
get_hosts(odbc) ->
    ClusterID = get_clusterid(),
    case gen_storage:dirty_select(?MYNAME, hosts, [{'=', clusterid, ClusterID}]) of
            Hosts when is_list(Hosts) ->
		lists:map(fun (#hosts{host = Host}) ->
                              case exmpp_stringprep:nameprep(Host) of
                                  error ->
                                      erlang:error({bad_vhost_name, Host});
                                  Name ->
                                      Name
                              end
                      end, Hosts);
            E ->
		erlang:error({get_hosts_error, E})
    end.

%% Retreive the text format config for host Host from ODBC and covert
%% it into a {host, Host, Config} tuple.
get_host_config(odbc, Host) ->
    case gen_storage:dirty_read(?MYNAME, hosts, Host) of
        [] ->
            erlang:error({no_such_host, Host});
        [H] ->
            config_from_string(Host, H#hosts.config);
        E ->
            erlang:error({host_config_error, E})
    end.

%% Convert a plaintext string into a host config tuple.
config_from_string(_Host, "") -> [];
config_from_string(_Host, Config) ->
    {ok, Tokens, _} = erl_scan:string(Config),
    case erl_parse:parse_term(Tokens) of
        {ok, List} when is_list(List) ->
            List;
        E ->
            erlang:error({bad_host_config, Config, E})
    end.

diff_hosts() ->
    diff_hosts(get_hosts(odbc)).

diff_hosts(NewHosts) ->
    diff_hosts(NewHosts, get_hosts(ejabberd)).

%% Given the new list of vhosts and the old list, return the list of
%% hosts added since last time and the list of hosts that have been
%% removed.
diff_hosts(NewHosts, OldHosts) ->
    RemoveHosts = OldHosts -- NewHosts,
    AddHosts = NewHosts -- OldHosts,
    {AddHosts,RemoveHosts}.    

%% XXX - this should be part of auth, not hosts.
auth_method(Host) ->
    case ejabberd_config:get_host_option(Host, auth_method) of
        undefined ->
            use_global_auth_method;
        Other ->
            {host_method, Other}
    end.

configure_static_hosts() ->
    ?DEBUG("Node startup - configuring hosts: ~p", [?MYHOSTS]),
    %% Add a null configuration for all MYHOSTS - this ensures
    %% the 'I'm a host' term gets written to the config table.
    %% We don't need any configuration options because these are
    %% statically configured hosts already configured by ejabberd_config.
    F = fun () ->
                lists:foreach(fun (H) -> ejabberd_config:configure_host(H, [{permanent, true}]) end,
                              ?MYHOSTS)
        end,
    mnesia:transaction(F).

cert_filename(Host) ->
    Dir = ejabberd_config:get_local_option({domain_certdir, global}),
    filename:join(Dir, Host ++ ".pem").

configure_host_cert(Host, File) ->
    ejabberd_config:add_local_option({domain_certfile, Host}, File),
    ok.

reconfigure_host_cert(Host) ->
    File = cert_filename(Host),
    case ejabberd_config:is_file_readable(File) of
        true ->
            ejabberd_config:mne_add_local_option({domain_certfile, Host}, File),
            ok;
        false ->
            no_cert
    end.

get_clusterid() ->
    case ejabberd_config:get_local_option(clusterid) of
	ID when is_integer(ID) ->
	    ID;
	undefined ->
	    ?ERROR_MSG("Please add to your ejabberd.cfg the line: ~n  {clusterid, 1}.", []),
	    1;
	Other ->
	    ?ERROR_MSG("Change your misconfigured {clusterid, ~p} to the value: ~p", [Other, 1]),
	    1
    end.
