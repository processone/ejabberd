%%%
%%%  Copyright © IDEALX S.A.S. 2003
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 04 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%%-------------------------------------------------------------------
%%% File    : ts_config_server.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description :
%%%
%%% Created :  4 Dec 2003 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------

-module(ts_config_server).

-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("ts_profile.hrl").
-include("ts_config.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1, read_config/1, get_req/2, get_next_session/1,
         get_client_config/1, newbeam/1, newbeam/2, start_slave/5,
         get_monitor_hosts/0, encode_filename/1, decode_filename/1,
         endlaunching/1, status/0, start_file_server/1, get_user_agents/0,
         get_client_config/2, get_user_param/1 ]).

%%debug
-export([choose_client_ip/1, choose_session/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {config,
                logdir,
                client_static_users = 0, % number of clients that already have their static users
                static_users = 0, % static users not yet given to a client
                ports,            % dict, used if we need to choose the client port
                users=1,          % userid (incremental counter)
                start_date,       %
                hostname,         % controller hostname
                last_beam_id = 0, % last tsung beam id (used to set nodenames)
                ending_beams = 0, % number of beams with no new users to start
                lastips,          % store next ip to choose for each client host
                total_weight      % total weight of client machines
               }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(LogDir) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [LogDir], []).

status() ->
    gen_server:call({global, ?MODULE}, {status}).

%%--------------------------------------------------------------------
%% Function: newbeam/1
%% Description: start a new beam
%%--------------------------------------------------------------------
newbeam(Host)->
    gen_server:cast({global, ?MODULE},{newbeam, Host, [] }).
%%--------------------------------------------------------------------
%% Function: newbeam/2
%% Description: start a new beam with given config. Use by launcher
%%  when maxclient is reached. In this case, the arrival rate is known
%%--------------------------------------------------------------------
newbeam(Host, {Arrivals, MaxUsers})->
    gen_server:cast({global, ?MODULE},{newbeam, Host, {Arrivals, MaxUsers} }).

%%--------------------------------------------------------------------
%% Function: get_req/2
%% Description: get Nth request from given session Id
%% Returns: #message | {error, Reason}
%%--------------------------------------------------------------------
get_req(Id, Count)->
    gen_server:call({global, ?MODULE},{get_req, Id, Count}).

%%--------------------------------------------------------------------
%% Function: get_user_agents/0
%% Description:
%% Returns: List
%%--------------------------------------------------------------------
get_user_agents()->
    gen_server:call({global, ?MODULE},{get_user_agents}).

%%--------------------------------------------------------------------
%% Function: read_config/1
%% Description: Read Config file
%% Returns: ok | {error, Reason}
%%--------------------------------------------------------------------
read_config(ConfigFile)->
    gen_server:call({global,?MODULE},{read_config, ConfigFile},?config_timeout).

%%--------------------------------------------------------------------
%% Function: get_client_config/1
%% Description: get client machine setup (for the launcher)
%% Returns: {ok, {ArrivalList, StartDate, MaxUsers}} | {error, notfound}
%%--------------------------------------------------------------------
get_client_config(Host)->
    gen_server:call({global,?MODULE},{get_client_config, Host}, ?config_timeout).

get_client_config(Type, Host)->
    gen_server:call({global,?MODULE},{get_client_config, Type, Host}, ?config_timeout).

%%--------------------------------------------------------------------
%% Function: get_monitor_hosts/0
%% Returns: [Hosts]
%%--------------------------------------------------------------------
get_monitor_hosts()->
        gen_server:call({global,?MODULE},{get_monitor_hosts}).

%%--------------------------------------------------------------------
%% @spec get_next_session(Host::string())-> {ok, SessionId::integer(),
%%              SessionSize::integer(),IP::tuple(), UserId::integer()}
%% @doc Choose randomly a session
%% @end
%%--------------------------------------------------------------------
get_next_session(Host)->
    gen_server:call({global, ?MODULE},{get_next_session, Host}).

get_user_param(Host)->
    gen_server:call({global, ?MODULE},{get_user_param, Host}).

endlaunching(Node) ->
    gen_server:cast({global, ?MODULE},{end_launching, Node}).


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([LogDir]) ->
    process_flag(trap_exit,true),
    {ok, MyHostName} = ts_utils:node_to_hostname(node()),
    ?LOGF("Config server started, logdir is ~p~n ",[LogDir],?NOTICE),
    {ok, #state{logdir=LogDir, hostname=list_to_atom(MyHostName)}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({read_config, ConfigFile}, _From, State=#state{logdir=LogDir}) ->
    case catch ts_config:read(ConfigFile, LogDir) of
        {ok, Config=#config{curid=LastReqId,sessions=[LastSess| Sessions]}} ->
            case check_config(Config) of
                ok ->
                    ts_utils:init_seed(Config#config.seed),
                    ts_user_server:init_seed(Config#config.seed),
                    application:set_env(tsung_controller, clients, Config#config.clients),
                    application:set_env(tsung_controller, dump, Config#config.dump),
                    application:set_env(tsung_controller, stats_backend, Config#config.stats_backend),
                    application:set_env(tsung_controller, debug_level, Config#config.loglevel),
                    SumWeights = fun(X, Sum) -> X#client.weight + Sum end,
                    Sum = lists:foldl(SumWeights, 0, Config#config.clients),
                    %% we only know now the size of last session from the file: add it
                    %% in the table
                    print_info(),
                    NewLast=LastSess#session{size = LastReqId, type=Config#config.main_sess_type},
                    %% start the file server (if defined) using a separate process (it can be long)
                    spawn(?MODULE, start_file_server, [Config]),
                    NewConfig=loop_load(sort_static(Config#config{sessions=[NewLast]++Sessions})),
                    set_max_duration(Config#config.duration),
                    {reply, ok, State#state{config=NewConfig, static_users=NewConfig#config.static_users,total_weight = Sum}};
                {error, Reason} ->
                    ?LOGF("Error while checking config: ~p~n",[Reason],?EMERG),
                    {reply, {error, Reason}, State}
            end;
        {error, {{case_clause, {error, enoent}},
                  [{xmerl_scan, fetch_DTD, 2}|_]}} ->
            ?LOG("Error while parsing XML: DTD not found !~n",?EMERG),
            {reply, {error, dtd_not_found}, State};
        {error, Reason} ->
            ?LOGF("Error while parsing XML config file: ~p~n",[Reason],?EMERG),
            {reply, {error, Reason}, State};
        {'EXIT', Reason} ->
            ?LOGF("Error while parsing XML config file: ~p~n",[Reason],?EMERG),
            {reply, {error, Reason}, State}
    end;

%% get Nth request from given session Id
handle_call({get_req, Id, N}, _From, State) ->
    Config = State#state.config,
    Tab    = Config#config.session_tab,
    ?DebugF("look for ~p th request in session ~p for ~p~n",[N,Id,_From]),
    case ets:lookup(Tab, {Id, N}) of
        [{_, Session}] ->
            ?DebugF("ok, found ~p for ~p~n",[Session,_From]),
            {reply, Session, State};
        Other ->
            {reply, {error, Other}, State}
    end;

%%
handle_call({get_user_agents}, _From, State) ->
    Config = State#state.config,
    case ets:lookup(Config#config.session_tab, {http_user_agent, value}) of
        [] ->
            {reply, empty, State};
        [{_Key, UserAgents}] ->
            {reply, UserAgents, State}
    end;

%% get  user parameters (static user: the session id is already known)
handle_call({get_user_param, HostName}, _From, State=#state{users=UserId,ports=Ports}) ->
    Config = State#state.config,
    {value, Client} = lists:keysearch(HostName, #client.host, Config#config.clients),
    {IPParam, Server, NewPorts} = get_user_param(Client,Config,Ports),
    ts_mon:newclient({static,now()}),
    {reply, {ok, { IPParam, Server, UserId}}, State#state{users=UserId+1,ports=NewPorts}};

%% get a new session id and user parameters for the given node
handle_call({get_next_session, HostName}, _From, State=#state{users=Users,ports=Ports}) ->
    Config = State#state.config,
    {value, Client} = lists:keysearch(HostName, #client.host, Config#config.clients),
    ?DebugF("get new session for ~p~n",[_From]),
    case choose_session(Config#config.sessions) of
        {ok, Session=#session{id=Id}} ->
            ?LOGF("Session ~p choosen~n",[Id],?INFO),
            ts_mon:newclient({Id,now()}),
            {IPParam, Server, NewPorts} = get_user_param(Client,Config,Ports),
            {reply, {ok, {Session, IPParam, Server, Users}},
             State#state{users=Users+1,ports=NewPorts}};
        Other ->
            {reply, {error, Other}, State}
    end;

handle_call({get_client_config, static, Host}, _From, State=#state{config=Config}) ->
%% static users (eg. each user started once at fixed time)
%% we must spread this list of fixed users to each beam
%% If we have N users and M client beams
    Clients=Config#config.clients,
    StaticUsers=State#state.static_users,
    Done=State#state.client_static_users, % number of clients that already have their static users
    {value, Client} = lists:keysearch(Host, #client.host, Clients),
    StartDate = set_start_date(State#state.start_date),
    case Done == length(Clients)+1 of
        true -> % last client, give him all pending users
            {reply,{ok,StaticUsers,StartDate},State#state{start_date=StartDate,static_users=[]}};
        false ->
            Weight = Client#client.weight,
            Number=round(length(StaticUsers)*Weight/State#state.total_weight),
            {NewUsers,Tail}=lists:split(Number,StaticUsers),
            {reply,{ok,NewUsers,StartDate},State#state{start_date=StartDate,static_users=Tail}}
    end;
%% get randomly generated users
handle_call({get_client_config, Host}, _From, State) ->
    ?DebugF("get_client_config from ~p~n",[Host]),
    Config = State#state.config,
    %% set start date if not done yet
    StartDate = set_start_date(State#state.start_date),
    case get_client_cfg(Config#config.arrivalphases,
                        Config#config.clients, State#state.total_weight,
                        Host) of
        {ok,List,Max} ->
            {reply,{ok,{List,StartDate,Max}},State#state{start_date=StartDate}};
        _ ->
            {reply,{error, notfound}, State}
    end;

%%
handle_call({get_monitor_hosts}, _From, State) ->
    Config = State#state.config,
    {reply, Config#config.monitor_hosts, State};

% get status: send the number of actives nodes
handle_call({status}, _From, State) ->
    Config = State#state.config,
    Reply = {ok, length(Config#config.clients), State#state.ending_beams},
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    ?LOGF("Unknown call ~p !~n",[Request],?ERR),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% start the launcher on the current beam
handle_cast({newbeam, Host, []}, State=#state{last_beam_id = NodeId,
                                              hostname = LocalHost,
                                              config   = Config})
  when Config#config.use_controller_vm and ( ( LocalHost == Host ) or ( Host == 'localhost' )) ->
    ?LOGF("Start a launcher on the controller beam ~p~n", [LocalHost], ?NOTICE),
    LogDir = encode_filename(State#state.logdir),
    %% set the application spec (read the app file and update some env. var.)
    {ok, {_,_,AppSpec}} = load_app(tsung),
    {value, {env, OldEnv}} = lists:keysearch(env, 1, AppSpec),
    NewEnv = [ {dump,atom_to_list(?config(dump))},
               {debug_level,integer_to_list(?config(debug_level))},
               {log_file,LogDir}],

    RepKeyFun = fun(Tuple, List) ->  lists:keyreplace(element(1, Tuple), 1, List, Tuple) end,
    Env = lists:foldl(RepKeyFun, OldEnv, NewEnv),
    NewAppSpec = lists:keyreplace(env, 1, AppSpec, {env, Env}),

    ok = application:load({application, tsung, NewAppSpec}),
    case application:start(tsung) of
        ok ->
            ?LOG("Application started, activate launcher, ~n", ?INFO),
            application:set_env(tsung, debug_level, Config#config.loglevel),
            ts_launcher_static:launch({node(), Host, []}),
            ts_launcher:launch({node(), Host, [], Config#config.seed}),
            {noreply, State#state{last_beam_id = NodeId +1}};
        {error, Reason} ->
            ?LOGF("Can't start launcher application ~p (reason: ~p) ! Aborting!~n",[Host, Reason],?EMERG),
            ts_mon:abort(),
            {stop, normal, State}
    end;

%% use_controller_vm and max number of concurrent users reached , big trouble !
handle_cast({newbeam, Host, _}, State=#state{ hostname=LocalHost,config=Config})
  when Config#config.use_controller_vm and ( ( LocalHost == Host ) or ( Host == 'localhost' )) ->
    Msg ="Maximum number of concurrent users in a single VM reached and 'use_controller_vm' is true, can't start new beam !!! Check 'maxusers' value in <client> configuration.~n",
    ?LOG(Msg, ?EMERG),
    erlang:display(Msg),
    {noreply, State};

%% start a launcher on a new beam with slave module
handle_cast({newbeam, Host, Arrivals}, State=#state{last_beam_id = NodeId}) ->
    Name = set_nodename(NodeId),
    {ok, [[BootController]]}    = init:get_argument(boot),
    ?DebugF("BootController ~p~n", [BootController]),
    {ok, [[?TSUNGPATH,PathVar]]}    = init:get_argument(boot_var),
    ?DebugF("BootPathVar ~p~n", [PathVar]),
    {ok, PAList}    = init:get_argument(pa),
    PA = lists:flatmap(fun(A) -> [" -pa "] ++A end,PAList),
    ?DebugF("PA list ~p ~n", [PA]),
    {ok, Boot, _} = regexp:gsub(BootController,"tsung_controller","tsung"),
    ?DebugF("Boot ~p~n", [Boot]),
    Sys_Args= ts_utils:erl_system_args(),
    LogDir = encode_filename(State#state.logdir),
    Args = lists:flatten([ Sys_Args," -boot ", Boot,
        " -boot_var ", ?TSUNGPATH, " ",PathVar, PA,
        " +K true ",
        " -tsung debug_level ", integer_to_list(?config(debug_level)),
        " -tsung dump ", atom_to_list(?config(dump)),
        " -tsung log_file ", LogDir
        ]),
    ?LOGF("starting newbeam on host ~p from ~p with Args ~p~n", [Host, State#state.hostname, Args], ?INFO),
    Seed=(State#state.config)#config.seed,
    spawn_link(?MODULE, start_slave, [Host, Name, Args, Arrivals, Seed]),
    {noreply, State#state{last_beam_id = NodeId +1}};

handle_cast({end_launching, _Node}, State=#state{ending_beams=Beams}) ->
    {noreply, State#state{ending_beams = Beams+1}};

handle_cast(Msg, State) ->
    ?LOGF("Unknown cast ~p ! ~n",[Msg],?WARN),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({timeout, _Ref, end_tsung}, State) ->
    ts_mon:abort(),
    ?LOG("Tsung test max duration reached, exits ! ~n",?EMERG),
    {stop, normal, State};
handle_info({'EXIT', _Pid, {slave_failure,timeout}}, State) ->
    ts_mon:abort(),
    ?LOG("Abort ! ~n",?EMERG),
    {stop, normal, State};
handle_info({'EXIT', Pid, normal}, State) ->
    ?LOGF("spawned process termination (~p) ~n",[Pid],?INFO),
    {noreply, State};
handle_info(Info, State) ->
    ?LOGF("Unknown info ~p ! ~n",[Info],?WARN),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

set_start_date(undefined)->
     ts_utils:add_time(now(), ?config(warm_time));
set_start_date(Date) -> Date.

get_user_param(Client,Config,Ports)->
    {ok,IP} = choose_client_ip(Client),
    {ok, Server} = choose_server(Config#config.servers),
    {NewPorts,CPort}   = choose_port(IP, Ports,Config#config.ports_range),
    { {IP, CPort}, Server, NewPorts}.

%%----------------------------------------------------------------------
%% Func: choose_client_ip/1
%% Args: #client, Dict
%% Purpose: choose an IP for a client
%% Returns: {ok, IP, NewDict} IP=IPv4 address {A1,A2,A3,A4}
%%----------------------------------------------------------------------
%% FIXME: and for IPV6 ?
choose_client_ip(#client{ip = IPList, host=Host}) ->
    choose_rr(IPList, Host, {0,0,0,0}).

%%----------------------------------------------------------------------
%% Func: choose_server/1
%% Args: List
%% Purpose: choose a server for a new client
%% Returns: {ok, #server}
%%----------------------------------------------------------------------
choose_server(ServerList) ->
    choose_rr(ServerList, server, #server{}).

%%----------------------------------------------------------------------
%% Func: choose_rr/3
%% Args: List, Key, Default
%% Purpose: choose an value in list in a round robin way. Use last
%%          value stored in the process dictionnary
%           Return Default if list is empty
%% Returns: {ok, Val}
%%----------------------------------------------------------------------
choose_rr([],_, Def) -> % no val, return default
    {ok, Def};
choose_rr([Val],_,_) -> % only one value
    {ok, Val};
choose_rr(List, Key, _) ->
    I = case get({rr,Key}) of
          undefined -> 1 ; % first use of this key, init index to 1
          Val when is_integer(Val) ->
            (Val rem length(List))+1 % round robin
    end,
    put({rr, Key},I),
    {ok, lists:nth(I, List)}.

%%----------------------------------------------------------------------
%% Func: choose_session/1
%% Args: List of #session
%% Purpose: choose an session randomly
%% Returns: #session
%%----------------------------------------------------------------------
choose_session([Session]) -> %% only one Session
    {ok, Session};
choose_session(Sessions) ->
    choose_session(Sessions, random:uniform() * 100,0).

choose_session([S=#session{popularity=P} | _],Rand,Cur) when Rand =< P+Cur->
    {ok, S};
choose_session([#session{popularity=P} | SList], Rand, Cur) ->
    choose_session(SList, Rand, Cur+P).


%%----------------------------------------------------------------------
%% Func: get_client_cfg/4
%% Args: list of #arrivalphase, list of #client, String
%% Purpose: set parameters for given host client
%% Returns: {ok, {Intensity = float, Users=integer, StartDate = tuple,
%%                Max = MaxNumber of clients to start}}
%%          | {error, Reason}
%%----------------------------------------------------------------------
get_client_cfg(Arrival, Clients, TotalWeight, Host) ->
    SortedPhases=lists:keysort(#arrivalphase.phase, Arrival),
    get_client_cfg(SortedPhases, Clients, TotalWeight,Host, []).

%% get_client_cfg/5
%% all phases scanned, look for max and return config.
get_client_cfg([], Clients, _TotalWeight, Host, Cur) ->
    {value, Client} = lists:keysearch(Host, #client.host, Clients),
    Max = Client#client.maxusers,
    {ok, lists:reverse(Cur), Max};
get_client_cfg([Arrival=#arrivalphase{duration = Duration,
                                      intensity= PhaseIntensity,
                                      maxnumber= MaxNumber } | AList],
               Clients, TotalWeight, Host, Cur) ->
    {value, Client} = lists:keysearch(Host, #client.host, Clients),
    Weight = Client#client.weight,
    ClientIntensity = PhaseIntensity * Weight / TotalWeight,
    NUsers = case MaxNumber of
                 infinity -> %% only use the duration to set the number of users
                     Duration * 1000 * ClientIntensity;
                 _ ->
                     ClientMaxNumber = trunc(MaxNumber * Weight / TotalWeight),
                     lists:min([ClientMaxNumber, Duration*1000*ClientIntensity])
             end,
    %% TODO: store the max number of clients
    ?LOGF("New arrival phase ~p for client ~p: will start ~p users~n",
          [Arrival#arrivalphase.phase, Host, NUsers],?NOTICE),
    get_client_cfg(AList, Clients, TotalWeight, Host,
                   [{ClientIntensity, round(NUsers)} | Cur]).

%%----------------------------------------------------------------------
%% Func: encode_filename/1
%% Purpose: kludge: the command line erl doesn't like special characters
%%   in strings when setting up environnement variables for application,
%%   so we encode these characters !
%%----------------------------------------------------------------------
encode_filename(String) when is_list(String)->
    Transform=[{"\\.","_46"},{"\/","_47"},{"\-","_45"}, {"\:","_58"}, {",","_44"}],
    lists:foldl(fun replace_str/2, "ts_encoded" ++ String, Transform);
encode_filename(Term) -> Term.


%%----------------------------------------------------------------------
%% Func: decode_filename/1
%%----------------------------------------------------------------------
decode_filename("ts_encoded" ++ String)->
    Transform=[{"_46","."},{"_47","\/"},{"_45","\-"}, {"_58","\:"}, {"_44",","}],
    lists:foldl(fun replace_str/2, String, Transform).

replace_str({A,B},X) ->
    {ok, Str, _} = regexp:gsub(X,A,B),
    Str.

%%----------------------------------------------------------------------
%% Func: print_info/0 Print system info
%%----------------------------------------------------------------------
print_info() ->
    ?LOGF("SYSINFO:Erlang version: ~s~n",[erlang:system_info(system_version)],?NOTICE),
    ?LOGF("SYSINFO:System architecture ~s~n",[erlang:system_info(system_architecture)],?NOTICE),
    ?LOGF("SYSINFO:Current path: ~s~n",[code:which(tsung)],?NOTICE).

%%----------------------------------------------------------------------
%% Func: start_file_server/1
%%----------------------------------------------------------------------
start_file_server(#config{file_server=[]}) ->
    ?LOG("No File server defined, skip~n",?DEB);
start_file_server(Config=#config{file_server=Filenames}) ->
    ?LOG("Starting File server~n",?INFO),
    FileSrv  = {ts_file_server, {ts_file_server, start, []}, transient, 2000,
                worker, [ts_msg_server]},
    supervisor:start_child(ts_controller_sup, FileSrv),
    ts_file_server:read(Filenames),
    ?LOG("Starting user servers if needed~n",?INFO),
    setup_user_servers(Config#config.vhost_file,Config#config.user_server_maxuid).


%%----------------------------------------------------------------------
%% Func: setup_user_servers/2
%%----------------------------------------------------------------------
setup_user_servers(_,none) ->
    ?LOG("Don't start any user server, as user_server_maxuid not defined~n",?DEB),
    ok;
setup_user_servers(none,Val) when is_integer(Val) ->
    ts_user_server:reset(Val);
setup_user_servers(FileId,Val) when is_atom(FileId), is_integer(Val) ->
    ?LOGF("Starting user servers with params ~p ~p~n",[FileId,Val],?DEB),
    {ok,Domains} = ts_file_server:get_all_lines(FileId),
    ?LOGF("Domains:~p~n",[Domains],?DEB),
    lists:foreach(fun(Domain) ->
                    {ok,_} = ts_user_server_sup:start_user_server(list_to_atom("us_" ++Domain))
                  end, Domains),
    ts_user_server:reset_all(Val).




%%----------------------------------------------------------------------
%% Func: check_config/1
%% Returns: ok | {error, ErrorList}
%%----------------------------------------------------------------------
check_config(Config)->
    Pop= ts_utils:check_sum(Config#config.sessions, #session.popularity, ?SESSION_POP_ERROR_MSG),
    %% FIXME: we should not depend on a protocol specific feature here
    Agents = ts_config_http:check_user_agent_sum(Config#config.session_tab),
    case lists:filter(fun(X)-> X /= ok  end, [Pop, Agents]) of
        []        -> ok;
        ErrorList -> {error, ErrorList}
    end.


load_app(Name) when is_atom(Name) ->
    FName = atom_to_list(Name) ++ ".app",
    case code:where_is_file(FName) of
    non_existing ->
        {error, {file:format_error(error_enoent), FName}};
    FullName ->
        case file:consult(FullName) of
        {ok, [Application]} ->
            {ok, Application};
        {error, Reason} ->
            {error, {file:format_error(Reason), FName}}
        end
    end.

%%----------------------------------------------------------------------
%% Func: loop_load/1
%% Args: #config
%% Returns: #config
%% Purpose: duplicate phases 'load_loop' times.
%%----------------------------------------------------------------------
loop_load(Config=#config{load_loop=0}) -> Config;
loop_load(Config=#config{load_loop=Loop,arrivalphases=Arrival}) when is_integer(Loop) ->
    loop_load(Config, ts_utils:keymax(#arrivalphase.phase, Arrival), Arrival ).

%% We have a list of n phases: duplicate the list and increase by the
%% max to get a new unique id for all phases. Here we don't care about
%% the order, so we start with the last iteration (Loop* Max)
loop_load(Config=#config{load_loop=0},_,Current) ->
    Config#config{arrivalphases = Current};
loop_load(Config=#config{load_loop=Loop, arrivalphases=Arrival},Max,Current) ->
    Fun= fun(Phase) -> Phase+Max*Loop end,
    NewArrival = lists:keymap(Fun,#arrivalphase.phase,Arrival),
    loop_load(Config#config{load_loop=Loop-1},Max,lists:append(Current, NewArrival)).

%% @doc sort static users by start time
sort_static(Config=#config{static_users=S})->
    ?LOGF("sort static users: ~p ~n", [S], ?DEB),
    SortedL= lists:keysort(1,S),
    Config#config{static_users=static_name_to_session(Config#config.sessions,SortedL)}.

%%
%% @doc start a remote beam
%%
start_slave(Host, Name, Args, Arrivals, Seed)->
    case slave:start(Host, Name, Args) of
        {ok, Node} ->
            ?LOGF("started newbeam on node ~p ~n", [Node], ?NOTICE),
            Res = net_adm:ping(Node),
            ?LOGF("ping ~p ~p~n", [Node,Res], ?NOTICE),
            case Arrivals of
                [] -> ts_launcher_static:launch({Node,[]});
                _  -> ok  %no static launcher needed in this case
            end,
            ts_launcher:launch({Node, Arrivals, Seed});
        {error, Reason} ->
            ?LOGF("Can't start newbeam on host ~p (reason: ~p) ! Aborting!~n",[Host, Reason],?EMERG),
            exit({slave_failure, Reason})
    end.

choose_port(_,_, undefined) ->
    {[],0};
choose_port(Client,undefined, Range) ->
    choose_port(Client,dict:new(), Range);
choose_port(ClientIp,Ports, {Min, Max}) ->
    case dict:find(ClientIp,Ports) of
        {ok, Val} when Val =< Max ->
            NewPorts=dict:update_counter(ClientIp,1,Ports),
            {NewPorts,Val};
        _ -> % Max Reached or new entry
            NewPorts=dict:store(ClientIp,Min+1,Ports),
            {NewPorts,Min}
    end.

%% @spec session_name_to_session(Sessions::list(), Static::list() ) -> StaticUsers::list()
%% @doc convert session name to session id in static users list
static_name_to_session(Sessions, Static) ->
    ?LOGF("Static users with session id ~p~n",[Static],?DEB),
    Search = fun({Delay,Name})->
                     {value, Session} = lists:keysearch(Name, #session.name, Sessions),
                     {Delay, Session}
             end,
    Res=lists:map(Search, Static),
    ?LOGF("Static users with session id ~p~n",[Res],?DEB),
    Res.

%% @spec set_nodename(NodeId::integer()) -> string()
%% @doc set slave node name: check if controller node name has an id,
%%      and put it in the slave name
set_nodename(NodeId) when is_integer(NodeId)->
    CId = case atom_to_list(node()) of
              "tsung_controller@"++_ ->
                  "";
              "tsung_controller"++Tail ->
                  [Id|_] = string:tokens(Tail,"@"),
                  Id++"_"
          end,
    "tsung"++ CId++ integer_to_list(NodeId).

%% @spec set_max_duration(integer()) -> ok
%% @doc start a timer for the maximum duration of the load test. The
%% maximum duration is 49 days
set_max_duration(0) -> ok; % nothing to do
set_max_duration(Duration) when Duration =< 4294967 ->
    ?LOGF("Set max duration of test: ~p s ~n",[Duration],?NOTICE),
    erlang:start_timer(Duration*1000, self(), end_tsung ).

