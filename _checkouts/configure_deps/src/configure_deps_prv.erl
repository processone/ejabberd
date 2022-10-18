-module(configure_deps_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'configure-deps').
-define(DEPS, [install_deps]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
	        {namespace, default},
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 configure-deps"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Explicitly run ./configure for dependencies"},
            {desc, "A rebar plugin to allow explicitly running ./configure on dependencies. Useful if dependencies might change prior to compilation when configure is run."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = rebar_state:project_apps(State) ++ lists:usort(rebar_state:all_deps(State)),
    lists:foreach(fun do_app/1, Apps),
    {ok, State}.

exec_configure({'configure-deps', Cmd}, Dir) ->
	rebar_utils:sh(Cmd, [{cd, Dir}, {use_stdout, true}]);
exec_configure(_, Acc) -> Acc.

parse_pre_hooks({pre_hooks, PreHooks}, Acc) ->
    lists:foldl(fun exec_configure/2, Acc, PreHooks);
parse_pre_hooks(_, Acc) -> Acc.

parse_additions({add, App, Additions}, {MyApp, Dir}) when App == MyApp ->
    lists:foldl(fun parse_pre_hooks/2, Dir, Additions),
	{MyApp, Dir};
parse_additions(_, Acc) -> Acc.

do_app(App) ->
    Dir = rebar_app_info:dir(App),
	Opts = rebar_app_info:opts(App),
	Overrides = rebar_opts:get(Opts, overrides),
    lists:foldl(fun parse_additions/2, {binary_to_atom(rebar_app_info:name(App), utf8), Dir}, Overrides).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
