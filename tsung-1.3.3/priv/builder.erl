%%%----------------------------------------------------------------------
%%% File    : builder.erl
%%% Author  : Mats Cronqvist <mats.cronquist@etx.ericsson.se>
%%%	    : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Purpose : Simplify build of OTP applications & boot scripts
%%% Created : 2 Jan 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------
%%%  @doc OTP release script builder.
%%%
%%% <p>This program compiles .rel, .script, .boot and sys.config files
%%%  for erlang applications. It supports incremental and recursive builds,
%%%  and is intended to be (much) easier and safer to use than doing it all
%%%  manually. This program does not generate beam code from Erlang source
%%%  files.</p>
%%%
%%% <p>The program makes some assumptions:</p>
%%%   <ul><li>The application is store in a directory whose name is
%%%           composed with the name of the application '-' the version
%%%           number of the application (i.e. myapp-1.0)</li>
%%%       <li>The release and app file used in input are simplified subset
%%%           of the final Erlang release and app file. Some differences: 
%%%           Versions are handled automatically; You do not need to add
%%%           the erts version tuple in the rel.src file; the application,
%%%           currently being built is mandatory in the list of apps in the
%%%           rel.src file; etc.</li>
%%%   </ul>
%%%
%%% <p>The program (henceforth called 'builder') can be customized
%%%  using a number of options. The options are prioritized in 
%%% the following order: (1) those given as arguments to the 
%%% <code>builder:go/1</code> function, those in the BUILD_OPTIONS 
%%% file, and (3) the hard-coded defaults.</p>
%%%
%%% <p>Valid options are</p>
%%% <dl>
%%% <dt>{app_dir, dirname()}</dt>
%%%   <dd>The application directory of the application being built.
%%%     The default value is Current Working Directory.</dd>
%%%
%%% <dt>{build_options, filename()}</dt>
%%%   <dd>A filename pointing to a BUILD_OPTIONS file. This is a file
%%%     containing erlang expressions, each terminated by "." -- just
%%%     like a file processed using file:eval/1. The last expression
%%%     in the file should result in a list of
%%%     <code>{Key,Value}</code> options.
%%%     If this option is not given, builder will look in [AppDir] and 
%%%     <code>[AppDir]/src</code> for a file called BUILD_OPTIONS.<br/>
%%%     Pre-bound variables are:
%%%     <ul>
%%%      <li><code>ScriptName  : filename()</code>,
%%%       the name of the script file.</li>
%%%     </ul></dd>
%%% <dt>{report, Level : atom()}</dt>
%%%   <dd>Specifies the reporting level. The following levels are recognized:
%%%     none, progress, verbose, debug.</dd>
%%%
%%% <dt>{out_dir, dirname()}</dt>
%%%   <dd>Specifies where [AppName].script, [AppName].boot, and sys.config
%%%     should be written. Default is <code>[AppDir]/priv</code>.</dd>
%%%
%%% <dt>{sys_config, filename()}</dt>
%%%   <dd>Specifies the location of the sys.config file. Default is 
%%%     <code>[OutDir]/sys.config</code>.</dd>
%%%
%%% <dt>{rel_file, filename()}</dt>
%%%   <dd>Specifies which .rel file should be used as input for the 
%%%     build process. Default is 
%%%     <code>[AppDir]/src/[AppName].rel.src</code>.</dd>
%%%
%%% <dt>{rel_name, string()}</dt>
%%%   <dd>This option can be used if the release should be called something
%%%   other than <code>[AppName]</code> or whatever is in the 
%%%   <code>.rel</code> file.</dd>
%%%
%%% <dt>{app_vsn, string()}</dt>
%%%   <dd>This option can be used to assign a version to the current 
%%%    application. If the application directory has a version suffix,
%%%    the version suffix will override this option. If the directory
%%%    has no suffix, the default vsn will be "BLDR", unless 'app_vsn'
%%%    has been specified.</dd>
%%%
%%% <dt>{apps, [App : AppName | {AppName,Vsn} | {AppName,Vsn,EbinDir}]}</dt>
%%%   <dd>This is a way to identify which applications should be included
%%%     in the build. The way builder determines which applications should
%%%     be included is this:<ul>
%%%     <li>If there is a <code>.rel.src</code> file, the applications
%%%      listed in this file are included first, in the order in which 
%%%      they are listed.</li>
%%%     <li>After this, all applications listed in the 'applications'
%%%      attribute of the <code>.app.src</code> file are added (if not
%%%      already listed), also in the order in which they are listed.</li>
%%%     <li>Then, all remaining applications in the 'apps' list are added
%%%      in order.</li>
%%%     <li>Finally, any included applications listed in the
%%%      <code>.rel.src</code> file, not already listed, are added.
%%%      (note that applications listed in 'included_applications' of 
%%%       the <code>.app.src</code> file are not included here. The 
%%%       <code>.rel</code> file can be used to override this definition,
%%%       so it would be an error to include it at this point.)</li>
%%%     </ul>
%%%   </dd>
%%%
%%% <dt>{path, [PathExpr]}</dt>
%%%   <dd>Specifies the search path when locating applications. Default is
%%%     code:get_path(). PathExpr can contain wildcards, e.g.
%%%     <code>"/OTP/lib/kernel*/ebin"</code> (basically anything that 
%%%     <code>regexp:sh_to_awk/1</code> understands.)</dd>
%%%
%%% <dt>{skip, [ModuleName : atom()]}</dt>
%%%   <dd>Lists modules that should not be included in the generated .app
%%%     file for the current application. If not specified, builder will
%%%     locate all modules under <code>[AppDir]/src</code>, extract 
%%%     their module names, and include these module names in the
%%%     <code>'modules'</code> list of the <code>.app</code> file.</dd>
%%%
%%% <dt>{make_app, true | false | auto}</dt>
%%%   <dd>If true, builder will generate an .app file from an .app.src
%%%    file; if false, it will try to use an existing .app file; if
%%%    auto, it will build an .app file if no .app file exists
%%%    Default is 'auto'.</dd>
%%%
%%% <dt>{make_rel, true | false}</dt>
%%%   <dd>If true, builder will generate a .rel file from a .rel.src file;
%%%   if false, it will assume that there is a working .rel file.
%%%   Default: false if the rel_file option has been specified; true 
%%%   otherwise.</dd>
%%%
%%% <dt>{make_boot, true | false}</dt>
%%%   <dd>If true, builder will run <code>systools:make_script()</code>
%%%   to generate .script and .boot files in [OutDir]. If false, it 
%%%   will assume that there is a working .boot file. Default is true.</dd>
%%% </dl>
%%%
%%% <dt>{systools, Options : [Opt]}</dt>
%%%   <dd>If specified, <code>Options</code> will be passed to the 
%%%   <code>systools:make_script()</code> command for building the
%%%   <code>.boot</code> script. Note that the <code>'path'</code> option
%%%   is generated by builder. It cannot be overridden.
%%%   See <code>erl -man systools</code> for available options.</dd>
%%%
%%% <dt>{sh_script, auto | none}</dt>
%%%   <dd>If <code>'auto'</code>, builder will generate a small sh script 
%%%   that makes it easier to start the system; If <code>'none'</code>,
%%%   no script will be generated. Default is <code>'none'</code> on 
%%%   non-UNIX systems and <code>'auto'</code> on UNIX systems.</dd>
%%%
%%% <dt>{erl_opts, Opts : string()}</dt>
%%%   <dd>If specified, and if a sh_script is generated (see above),
%%%    <code>Opts</code> will be appended to the <code>erl</code>
%%%    command line. The builder will automatically put in options to
%%%    identify the boot script, sys.config, and possible boot variables.</dd>
%%%
%%% <dt>{config, Config : {file, filename()} | {M,F,A} | Data}</dt>
%%%   <dd>If present, this option should either point to a file that
%%%   contains a subset of data for a sys.config file (same format as
%%%   sys.config), or give a <code>{M,F,A}</code> tuple, where
%%%   <code>apply(M,F,A)</code> results in a list, 
%%%   <code>[{AppName, [{Key, Value}]}]</code>, or finally just specify
%%%   the data in place. The builder will look for similar <code>config</code>
%%%   directives in any BUILD_OPTIONS files of other applications that 
%%%   are part of the build, and all inputs will be merged
%%%   into a common <code>sys.config</code>.</dd>
%%%
%%% <dt>{{config,OtherApp}, Config}</dt>
%%%   <dd>This is a way to specify environment variables for another
%%%   application in the build options. <code>Config</code> in this case
%%%   is the same as <code>Config</code> above. The difference between
%%%   this option and specifying environment variables for the other 
%%%   application using the above option, is that with a simple 
%%%   <code>{config, ...}</code> directive, builder will also check
%%%   for a similar directive in the other application, and all 
%%%   different inputs will be merged. Using a <code>{{config,App},...}</code>
%%%   option, builder will not look into that application further.</dd>

%%% @end
%%% =====================================================================

-module(builder).
-author('mats.cronquist@etx.ericsson.se').
-author('ulf.wiger@ericsson.com').

-export([go/0, go/1, find_app/2]).
-export([early_debug_on/0]).
-compile(export_all).

-include_lib("kernel/include/file.hrl").
-import(dict, [fetch/2]).

-define(report(Level, Format, Args),
	case do_report(Level) of 
	    true ->
		io:format("[~p:~p] " ++ Format, [?MODULE,?LINE|Args]);
	    false ->
		ok
	end).

-define(f(D, Expr), fun(D) -> Expr end).

-define(herewith(D), fun(D) ->
			     ?report(debug, "herewith~n", []),
			     D
		     end).
		      
early_debug_on() ->
    put(builder_debug, true).
    

go() ->
    go([]).

go(Options) ->
    %% We use a dictionary for the options. The options are prioritized in 
    %% the following order: (1) those given as arguments to the go/1 function,
    %% those in the BUILD_OPTIONS file, and (3) the hard-coded defaults.
    %% The options stored last in the dictionary take precedence, but since
    %% we want to allow for the Options list to guide us in finding the 
    %% BUILD_OPTIONS file (under certain circumstances), we store the defaults
    %% and Options list in the dictionary, then locate and read the file, then
    %% store the Options list again.
    Dict0 = mk_dict(default_options() ++ Options),
    Dict = with(Dict0,
		[fun(D) -> read_options_file(D) end,
		 ?herewith(D),   % to help debugging
		 fun(D) -> store_options(Options, D) end,
		 ?herewith(D),
		 fun(D) -> post_process_options(D) end,
		 ?herewith(D),
		 fun(D) ->
			 {Path, [App, Vsn]} = get_app(D),
			 store_options([{app_name, list_to_atom(App)},
					{app_vsn, Vsn}], D)
		 end,
		 ?herewith(D),
		 fun(D) ->
			 [Descr, Id, Reg, Apps, Env, Mod, Phases] =
			     read_app_file(D),
			 Vsn = fetch(app_vsn, D),
			 store_options([{app, [{vsn,Vsn}, Descr, Id, Reg, 
					       Apps, Env, Mod, Phases]}], D)
		 end,
		 ?herewith(D),
		 fun(D) -> case dict:find(rel_name, D) of
			       {ok,_} -> D;
			       error ->
				   RelName = atom_to_list(
					       fetch(app_name, D)),
				   dict:store(rel_name, RelName, D)
			   end
		 end,
		 ?herewith(D),
		 fun(D) -> 
			 RelFname = get_rel_filename(D),
			 case file:consult(RelFname) of
			     {ok, [{release,{_Name,_RelVsn},
				    _RelApps}= Rel]} ->
				 ?report(debug,"rel_src = ~p~n",[Rel]),
				 dict:store(rel_src,Rel, D);
			     _ ->
				 D
			 end
		 end,
		 ?herewith(D),
		 fun(D) -> AppInfo = find_apps(D),
			   ?report(debug, "find_apps(D) -> ~p~n", [AppInfo]),
			   dict:store(app_info, AppInfo, D)
		 end,
		 ?herewith(D),
		 fun(D) ->
			 BootVars = boot_vars(D),
			 dict:store(boot_vars, BootVars, D)
		 end]),
    case lists:member({ok,true}, [dict:find(make_boot,Dict),
				  dict:find(make_rel,Dict),
				  dict:find(make_config, Dict)]) of
	true ->
	    verify_dir(fetch(out_dir,Dict));
	false ->
	    ok
    end,
    Dict1 = make_rel(Dict),
    make_app(Dict1),
    Res = make_boot(Dict1),
    make_config(Dict1),
    make_sh_script(Dict1),
    cleanup(Dict1),
    Res.


find_app(App, Path) ->
    {Found,_} = expand_path(Path, [App], [], []),
    Found.


post_process_options(Dict) ->
    D = with(Dict,
	     [fun(D) -> case dict:find(out_dir, D) of
			    {ok,_} -> D;
			    error -> dict:store(out_dir, out_dir(D), D)
			end
	      end,
	      fun(D) -> case dict:find(rel_file, D) of
			    {ok,_} -> dict:store(make_rel, false);
			    error -> dict:store(make_rel, true, D)
			end
	      end,
	      fun(D) -> case dict:find(config, D) of
			    {ok,_} -> dict:store(make_config, true, D);
			    _ -> D
			end
	      end,
	      fun(D) -> case dict:find(make_boot, D) of
			   {ok, _} -> D;
			   error -> dict:store(make_boot, true, D)
			end
	      end,
	      fun(D) -> case dict:find(make_app, D) of
			    {ok,_} -> D;
			    error -> dict:store(make_app, auto, D)
			end
	      end,
	      fun(D) -> case dict:find(sh_script, D) of
			    {ok, _} -> D;
			    error ->
				case os:type() of
				    {unix,_} ->
					dict:store(sh_script,auto,D);
				    _ ->
					dict:store(sh_script,none,D)
				end
			end
	      end,
	      fun(D) -> case dict:find(systools, D) of
			   {ok, _} -> D;
			   error -> dict:store(systools, [], D)
			end
	      end]),
    ?report(debug, "Options = ~p~n", [dict:to_list(D)]),
    D.


with(Dict, Actions) ->
    lists:foldl(fun(F,D) ->
			F(D)
		end, Dict, Actions).
			


cleanup(Dict) ->
    pop_report_level().

default_options() ->
    io:format("default app_dir is ~p~n",[cwd()]),

    [{app_dir, cwd()}, {skip, []},
     {path, code:get_path()}].


read_options_file(Dict) ->
    case dict:find(build_options, Dict) of
	{ok, BuildOptsF} ->
	    case script(BuildOptsF) of
		{error, empty_script} ->
		    %% We accept this
		    Dict;
		{ok, Terms} ->
		    ?report(debug, "Stored terms (~s) =~n    ~p~n", 
			   [BuildOptsF, Terms]),
		    store_options(Terms, Dict);
		{error, Reason} ->
		    exit({bad_options_file, {BuildOptsF, Reason}})
	    end;
	error ->
	    Path = 
		case dict:find(app_dir, Dict) of
		    {ok, AppDir} ->
			[AppDir, filename:join(AppDir, "src")];
		    error ->
			[".", "src"]
		end,
	    case path_script(Path, "BUILD_OPTIONS") of
		{ok, Terms, Fullname} ->
		    Dict1 = store_options(Terms, Dict),
		    ?report(debug, "Stored terms (~s) =~n    ~p~n",
			   [Fullname, Terms]),
		    Dict1;
		{error, enoent} ->
		    Dict;
		Error ->
		    exit({build_options, Error})
	    end
    end.


mk_dict(Options) ->
    %% pust a unique ref onto the dictionary. This will allow us to 
    %% redefine e.g. the report_level (for the same instance of 
    %% the dictionary, and stack the same (for new instances of the 
    %% dictionary -- recursive build.)
    store_options([{'#ref#', make_ref()}|Options], dict:new()).

store_options(Options, Dict) ->
    ?report(debug, "store_options(~p)~n", [Options]),
    lists:foldl(
      fun({report, Level}, D) ->
	      push_report_level(Level, D),
	      dict:store(report, Level, D);
	  ({Key,Value}, D) ->
	      dict:store(Key,Value,D)
      end, Dict, Options).
    

make_rel(Dict) ->
    case dict:find(make_rel, Dict) of
	{ok,true} ->
	    ?report(debug, "will make rel~n", []),
	    App = atom_to_list(fetch(app_name, Dict)),
	    Vsn = fetch_key(vsn, fetch(app, Dict)),
	    Apps = fetch(app_info, Dict),
	    AppInfo = [{A,V} || {A,V,F} <- Apps],
	    ?report(debug,"AppInfo = ~p~n", [AppInfo]),
	    {RelName, Rel} = 
		case dict:find(rel_src, Dict) of
		    %% mremond: Added this case clause:
		    %%          Without it you get an error if you define
		    %%          the erts version in your release file subset.
		    %%          The ERTS version is anyway replace by the
		    %%          system ERTS
		    {ok, {release,{Name,RelVsn}, {erts, ErtsVsn}, RelApps}} ->
			?report(debug,"found rel_src: RelApps=~p~n",
			    [RelApps]),
			{Name,
			 {release, {Name,RelVsn}, {erts, get_erts_vsn()},
			  AppInfo}};
		    {ok, {release,{Name,RelVsn},RelApps}} ->
			%% here we should check that Apps is a subset of 
			%% RelApps; if so, use RelApps; otherwise exit
			?report(debug,"found rel_src: RelApps=~p~n",
			    [RelApps]),
			{Name,
			 {release, {Name,RelVsn}, {erts, get_erts_vsn()},
			  AppInfo}};
		    error ->
			{App, 
			 {release, {App, Vsn}, {erts, get_erts_vsn()}, 
			  AppInfo}}
		end,
	    RelFileTarget =
		filename:join(fetch(out_dir,Dict), RelName ++ ".rel"),
	    out(RelFileTarget, Rel),
	    store_options([{rel_name, RelName},
			   {rel_file, RelFileTarget}], Dict);
	_ ->
	    ?report(debug, "will NOT make rel~n", []),
	    Dict
    end.

make_config(Dict) ->
    AppInfo = fetch(app_info, Dict),
    AppName = fetch(app_name, Dict),
    ForeignData = lists:foldl(
		   fun({A,V,Ebin}, Acc) when A =/= AppName ->
			   Acc ++ try_get_config(A,Ebin,Dict);
		      (_, Acc) ->
			   Acc
		   end, [], AppInfo),
    
    Data = case dict:find(config, Dict) of
	       {ok, Type} ->
		   get_config_data(Type);
	       error ->
		   []
	   end,
    merge_config(Data ++ ForeignData, Dict).


try_get_config(AppName, Ebin, Dict) ->
    case dict:find({config, AppName}, Dict) of
	{ok, Type} ->
	    get_config_data(Type);
	error ->
	    BuildOpts = filename:join(
			  filename:join(filename:dirname(Ebin), "src"),
			  "BUILD_OPTIONS"),
	    case script(BuildOpts) of
		{error, enoent} ->
		    [];
		{ok, Result} ->
		    ?report(debug, "script(~p) ->~n   ~p~n",
			[BuildOpts, Result]),
		    case lists:keysearch(config, 1, Result) of
			{value, {_, Type}} ->
			    get_config_data(Type);
			false ->
			    []
		    end
	    end
    end.


get_config_data({file,F}) ->
    ?report(debug, "get_config_data({file, ~p})~n", [F]),
    case file:consult(F) of
	{ok, [Terms]} when list(Terms) ->
	    Terms;
	{error, Reason} ->
	    exit({config, {F, Reason}})
    end;
get_config_data({M,F,A}) ->
    ?report(debug, "get_config_data(~p)~n", [{M,F,A}]),
    apply(M,F,A);
get_config_data(Data) when list(Data) ->
    Data.


merge_config([], Dict) ->
    ok;
merge_config(Data, Dict) ->
    ConfigF = sys_config(Dict),
    case file:consult(ConfigF) of
	{error, enoent} ->
	    out(ConfigF, Data);
	{ok, [Terms]} ->
	    ?report(debug, "merging terms (~p,~p), F = ~p~n",
		[Data,Terms, ConfigF]),
	    NewData = merge_terms(Data, Terms),
	    out(ConfigF, NewData)
    end.

merge_terms([{App,Vars}|Data], Old) ->
    case lists:keysearch(App, 1, Old) of
	false ->
	    merge_terms(Data, Old ++ [{App,Vars}]);
	{value, {_, OldVars}} ->
	    ?report(debug, "merging vars (~p,~p) for ~p~n",
		[Vars,OldVars, App]),
	    NewVars = merge_vars(Vars, OldVars, App),
	    merge_terms(Data, lists:keyreplace(App,1,Old,{App,NewVars}))
    end;
merge_terms([], Data) ->
    Data.

merge_vars([{Key,Vals}|Data], Old, App) ->
    case lists:keysearch(Key, 1, Old) of
	{value, {_, OldVals}} when OldVals =/= Vals ->
	    exit({config_conflict, {App, Key}});
	{value, {_, OldVals}} when OldVals == Vals ->
	    merge_vars(Data, Old, App);
	false ->
	    merge_vars(Data, Old ++ [{Key,Vals}], App)
    end;
merge_vars([], Data, _App) ->
    Data.



fetch_key(Key, L) ->
    {value, {_, Value}} = lists:keysearch(Key, 1, L),
    Value.

make_app(Dict) ->
    case dict:find(make_app, Dict) of
	{ok,true} ->
	    do_make_app(Dict);
	{ok,auto} ->
	    AppName = fetch(app_name, Dict),
	    AppF = filename:join(ebin_dir(Dict), 
				 atom_to_list(AppName) ++ ".app"),
	    case file:read_file_info(AppF) of
		{ok, _} ->
		    ok;
		{error, enoent} ->
		    do_make_app(AppF, AppName, Dict)
	    end;
	_ ->
	    ok
    end.

do_make_app(Dict) ->
    AppName = fetch(app_name, Dict),
    AppF = filename:join(ebin_dir(Dict), atom_to_list(AppName) ++ ".app"),
    do_make_app(AppF, AppName, Dict).

do_make_app(AppF, AppName, Dict) ->
    [Vsn, Descr, Id, Reg, Apps, Env, Mod, Phases] = fetch(app, Dict),
    Modules = modules(Dict),
    Remove = case {Mod, Phases} of
		 {{_,[]},{_,undefined}} -> [Mod, Phases];
		 {{_,{_,_}},{_,undefined}} -> [Phases];
		 {{_,[]}, {_,_}} -> [Mod];
		 _ -> []
	     end,
    Options = [Vsn, Descr, Id, Modules, 
	       Reg, Apps, Env, Mod, Phases] -- Remove,
    out(AppF, {application, AppName, Options}).


make_boot(Dict) ->
    case dict:find(make_boot, Dict) of
	{ok,true} ->
	    ensure_rel_file(Dict),
	    App = fetch(app_name, Dict),
	    CWD = cwd(),
	    ok = file:set_cwd(fetch(out_dir,Dict)),
	    SystoolsOpts0 = merge_opts([{path, systools_path(Dict)}],
				       fetch(systools, Dict)),
	    SystoolsOpts = maybe_local(SystoolsOpts0, Dict),
	    ?report(debug, "SystoolsOpts = ~p~n", [SystoolsOpts]),
	    RelName = fetch(rel_name, Dict),
	    Res = systools:make_script(RelName, SystoolsOpts),
	    ?report(progress, "systools:make_script() -> ~p~n", [Res]),
	    make_load_script(RelName),
	    ok = file:set_cwd(CWD),
	    case Res of
		error ->
		    exit(error_in_make_script);
		ok ->
		    ok
	    end;
	_ ->
	    ok
    end.


maybe_local(Opts, Dict) ->
    case lists:keymember(variables, 1, Opts) of
	true ->
	    Opts;
	false ->
	    [local|Opts -- [local]]
    end.


make_load_script(RelName) ->
    {ok, Bin} = file:read_file(RelName ++ ".boot"),
    {script,Id,Cmds} = binary_to_term(Bin),
    Keep =
	lists:filter(
	  fun({apply,{application,start_boot,[kernel,Type]}}) -> true;
	     ({apply,{application,start_boot,[stdlib,Type]}}) -> true;
	     ({apply,{application,start_boot,[sasl,Type]}}) -> true;
	     ({apply,{application,start_boot,[_,Type]}}) -> false;
	     (_) ->
		  true
	  end, Cmds),
    NewScript = {script,Id,Keep},
    file:write_file(RelName ++ "_load.boot", term_to_binary(NewScript)),
    out(RelName ++ "_load.script", NewScript).
	  

boot_vars(Dict) ->
    case dict:find(systools, Dict) of
	{ok, Opts} when Opts =/= [] ->
	    ?report(debug, "systools opts = ~p~n", [Opts]),
	    case lists:keysearch(variables, 1, Opts) of
		{value, {_, Vars}} ->
		    Vars;
		false ->
		    []
	    end;
	_ ->
	    ?report(debug,"will make boot_vars~n", []),
	    {ok,[[Root]]} = init:get_argument(root),
	    Apps = dict:fetch(app_info, Dict),
	    prefixes(Apps, [{"ROOT",Root}])
    end.

prefixes([{A,_,D}|Apps], Prefixes) ->
    case [P || {_,P} <- Prefixes,
	       lists:prefix(P, D)] of
	[] ->
	    prefixes(Apps, guess_root(D, Prefixes));
	[_|_] ->
	    prefixes(Apps, Prefixes)
    end;
prefixes([], Prefixes) ->
    Prefixes.

guess_root(D, Pfxs) ->
    case lists:reverse(filename:split(D)) of
	["ebin",App,"lib",Dir|T] ->
	    guess(to_upper(Dir), 0, 
		  filename:join(lists:reverse([Dir|T])), Pfxs);
	["ebin",App,Dir|T] ->
	    guess(to_upper(Dir), 0,
		  filename:join(lists:reverse([Dir|T])), Pfxs)
    end.

guess(Guess,N,RootDir, Pfxs) ->
    case lists:keymember(Guess,1,Pfxs) of
	false ->
	    ?report(debug, "manufactured boot_var {~p,~p}~n",
		[Guess,RootDir]),
	    [{Guess,RootDir}|Pfxs];
	true ->
	    ?report(debug, "duplicate Guess = ~p~n", [Guess]),
	    guess(increment(Guess,N), N+1, RootDir, Pfxs)
    end.

increment(Guess,0) ->
    Guess ++ "-1";
increment(Guess,N) when N < 10 ->
    [_,$-|T] = lists:reverse(Guess),
    lists:reverse(T) ++ [$-|integer_to_list(N+1)].

to_upper([H|T]) when H >= $a, H =< $z ->
    [$A+(H-$a)|to_upper(T)];
to_upper([H|T]) ->
    [H|T];
to_upper([]) ->
    [].

make_sh_script(Dict) ->
    case dict:find(sh_script, Dict) of
	{ok, auto} ->
	    OutDir = fetch(out_dir, Dict),
	    RelName = fetch(rel_name, Dict),
	    StartF = filename:join(OutDir, RelName ++ ".start"),
	    LoadF = filename:join(OutDir, RelName ++ ".load"),
	    do_make_sh_script(StartF,start,Dict),
	    do_make_sh_script(LoadF,load,Dict);
	_ ->
	    ok
    end.

do_make_sh_script(ScriptF, Type, Dict) ->
    ok = file:write_file(ScriptF, list_to_binary(
				    sh_script(ScriptF, Type,Dict))),
    {ok,FI} = file:read_file_info(ScriptF),
    Mode = FI#file_info.mode bor 8#00100,
    ok = file:write_file_info(ScriptF, FI#file_info{mode = Mode}).



sh_script(Fname, Mode,Dict) ->
    OutDir = fetch(out_dir, Dict),
    AppDir = fetch(app_dir, Dict),
    BaseName = filename:basename(Fname),
    XOpts = case dict:find(erl_opts, Dict) of
		{ok, Opts} ->
		    Opts;
		error ->
		    []
	    end,
    OptionalSname =
	case regexp:match(XOpts, "-sname") of
	    {match,_,_} -> false;
	    nomatch -> true
	end,
    ?report(debug, "OptionalSname (~p) = ~p~n", [XOpts, OptionalSname]),
    %% mremond: The path it need to be sure the script is predictible	    
%     Path = [" -pa ", filename:join(AppDir, "ebin")],
    RelName = fetch(rel_name, Dict),
    Boot = case Mode of
	       start -> [" -boot ", filename:join(OutDir, RelName)];
	       load  -> [" -boot ", filename:join(OutDir, RelName++"_load")]
	   end,
    Sys = case dict:find(make_config, Dict) of
	      {ok, true} ->
		  [" -config ", filename:join(OutDir, "sys")];
	      _ ->
		  []
	  end,
    BootVars = [[" -boot_var ", Var, " ", Dir] ||
		   {Var,Dir} <- dict:fetch(boot_vars, Dict)],
%     ["#/bin/sh\n"
%      "erl ", Boot, Sys, BootVars, XOpts, "\n"].
    ["#!/bin/bash\n",
     "ERL=\"`which erl`\"\n",
     case OptionalSname of
	 true -> "SNAME=\"\"\n";
	 false -> ""
     end,
     "\n"
     "while [ \$# -gt 0 ]\n"
     "  do\n"
     "    case \$1 in\n"
     "      -erl)\n"
     "        ERL=\"\$2\"\n"
     "        shift\n"
     "        ;;\n",
     case OptionalSname of
	 true ->
	     ("      -sname)\n"
	      "        SNAME=\"\-sname $2\"\n"
	      "        shift\n"
	      "        ;;\n");
	 false ->
	     ""
     end,
     "      *)\n"
     "        echo \"Faulty arguments: \$*\"\n"
     "        echo \"usage: ", BaseName, (
				 [" [-erl <ErlLocation>]",
				  case OptionalSname of
				      true -> " [-sname <Sname>]";
				      false -> ""
				  end, "\"\n"]),
     "        exit 1\n"
     "        ;;\n"
     "      esac\n"
     "      shift\n"
     "  done\n"
     "\n",
     case OptionalSname of
	 true ->
	     ["\$ERL \$SNAME", Boot, Sys, BootVars, opt_space(XOpts), "\n"];
	 false ->
     	     ["\$ERL", Boot, Sys, BootVars, opt_space(XOpts), "\n"]
     end
    ].

opt_space([]) ->
    [];
opt_space([_|_] = L) ->
    [$\s|L].

ensure_rel_file(Dict) ->
    OutDir = fetch(out_dir, Dict),
    case dict:find(rel_file, Dict) of
	{ok, F} ->
	    case filename:dirname(F) of
		D when D == OutDir ->
		    ok;
		_ ->
		    %% This doesn't necessarily mean that it's not there
		    RelName = fetch(rel_name, Dict),
		    {ok, [{release, {N,Vsn}, Erts, Apps}]} =
			file:consult(F),
		    RelF = filename:join(OutDir,
					 fetch(rel_name,Dict) ++ ".rel"),
		    out(RelF, {release, {RelName,Vsn}, Erts, Apps})
	    end;
	error ->
	    exit(no_rel_file)
    end.


systools_path(Dict) ->
    AppInfo = fetch(app_info, Dict),
    [D || {_A,_V,D} <- AppInfo].

merge_opts(Opts, [{path,P}|Opts2]) ->
    exit({not_allowed, {systools, {path,P}}});
merge_opts(Opts, [Opt|Opts2]) ->
    [Opt|merge_opts(Opts, Opts2)];
merge_opts(Opts, []) ->
    Opts.

	

cwd() ->
    {ok, CWD} = file:get_cwd(),
    CWD.

out_dir(Dict) ->
    case dict:find(out_dir, Dict) of
	{ok, OutDir} ->
	    OutDir;
	error ->
	    filename:join(fetch(app_dir,Dict), "priv")
    end.

ebin_dir(Dict) ->
    AppDir = fetch(app_dir, Dict),
    filename:join(AppDir, "ebin").

sys_config(Dict) ->
    case dict:find(sys_config, Dict) of
	{ok, Filename} ->
	    Filename;
	error ->
	    filename:join(fetch(out_dir,Dict), "sys.config")
    end.

get_app(Dict) ->
    AppDir = fetch(app_dir, Dict),
    %% mremond: The program was crashing when the directory where not
    %%          containing the application version
    %%  Now we assume that version number is "BLDR" by default.
    case string:tokens(hd(lists:reverse(string:tokens(AppDir, "/"))), "-") of
    	[App, Vsn] -> {AppDir, [App, Vsn]};
	[App]      ->
	    case dict:find(app_vsn, Dict) of
		{ok, Vsn} ->
		    {AppDir, [App, Vsn]};
		error ->
		    {AppDir, [App, "BLDR"]}
	    end
    end.

get_app_filename(App, Dict) ->
    AppDir = fetch(app_dir, Dict),
    filename:join([AppDir, "src", App++".app.src"]).

get_rel_filename(Dict) ->
    App = atom_to_list(dict:fetch(app_name, Dict)),
    AppDir = fetch(app_dir, Dict),
    filename:join([AppDir, "src", App++".rel.src"]).


modules(Dict) ->
    Ebin = ebin_dir(Dict),
    Ext = code:objfile_extension(),
    ?report(debug, "looking for modules (~p) in ~p~n", [Ext,Ebin]),
    L = recursive_list_dir(Ebin),
    ?report(debug, "L = ~p~n", [L]),
    Skip = fetch(skip, Dict),
    ?report(debug, "Skip = ~p~n", [Skip]),
    Modules = lists:foldr(
		fun(F, Acc) ->
			case make_mod(F) of
			    {ok, M} ->
				[M|Acc];
			    error ->
				Acc
			end
		end, [], [F || F <- L,
			       check_f(F, Ext)]),
    {modules, Modules -- Skip}.


recursive_list_dir(Dir) ->
    ?report(debug, "recursive_list_dir(~p)~n", [Dir]),
    {ok, Fs} = file:list_dir(Dir),
    lists:concat([f_expand(F,Dir) || F <- Fs]).

f_expand(Name,Dir) ->
    Fname = filename:join(Dir,Name),
    case file:read_file_info(Fname) of
	{ok, #file_info{type=regular}} ->
	    [Fname];
	{ok, #file_info{type=directory}} ->
	    ?report(debug, "~p is a directory under ~p~n", [Fname, Dir]),
	    {ok, Fs} = file:list_dir(Fname),
	    lists:concat([f_expand(F,Fname) || F <- Fs]);
	{ok, Finfo} ->
	    ?report(debug, "~p not a directory or a regular file. Ignoring~n",
		   [Fname]),
	    [];
	{error, Reason} ->
	    ?report(error, "*** read_file_info(~p) -> {error, ~p}~n", 
		   [Fname, Reason]),
	    []
    end.

make_mod(F) ->
    case beam_lib:version(F) of
	{ok, {Module, Vsn}} ->
	    {ok, Module};
	_ ->
	    error
    end.
%     {ok,Fd} = file:open(F, [read]),
%     parse_for_mod(Fd, F).
%     {ok,Bin} = file:read_file(F),
%     Text = binary_to_list(Bin),
%     {match,Start,Len} = 
% 	regexp:first_match(Text, 
% 			   "^[ ]*-[ ]*module[ ]*[(][ ]*.*[ ]*[)][ ]*\."),
%     Name = case string:substr(Text, Start+8,Len-10) of
% 	       "'" ++ Quoted ->
% 		   "'" ++ Rest = lists:reverse(Quoted),
% 		   list_to_atom(lists:reverse(Rest));
% 	       Str ->
% 		   list_to_atom(Str)
% 	   end.

parse_for_mod(Fd, F) ->
    parse_for_mod(Fd, io:parse_erl_exprs(Fd,''), F).

parse_for_mod(Fd, {ok,[{op,_,'-',{call,_,{atom,_,module},[Mod]}}],_}, F) ->
    {ok,parse_mod_expr(Mod)};
% parse_for_mod(Fd, {ok,[{op,_,'-',
% 			{call,_,{atom,_,module},[{atom,_,Mod}]}}],_}, F) ->
%     {ok,Mod};
parse_for_mod(Fd, {ok,_,_}, F) ->
    parse_for_mod(Fd, io:parse_erl_exprs(Fd,''), F);
parse_for_mod(Fd, {error,_,_}=Error, F) ->
    io:format("*** Error parsing ~s: ~p~n", [F, Error]),
    error;
parse_for_mod(Fd, {eof,_}, F) ->
    io:format("*** No module attribute found in ~s~n", [F]),
    error.

parse_mod_expr(Expr) ->
    list_to_atom(parse_mod_expr1(Expr)).

parse_mod_expr1({atom,_,A}) ->
    atom_to_list(A);
parse_mod_expr1({record_field,_,R,{atom,_,A}}) ->
    parse_mod_expr1(R) ++ "." ++ atom_to_list(A).
    


check_f(F, Ext) -> 
    case regexp:match(F, ".*"++Ext++"\$") of
	{match, _, _} -> true;
	_ -> false
    end.


find_apps(Dict) ->
    App = fetch(app, Dict),
    {value, {_, NeededApps}} = lists:keysearch(applications, 1, App),
    AppInfo = case dict:find(apps, Dict) of
		  {ok, As} ->
		      app_info(As, Dict);
		  error ->
		      []
	      end,
    RelApps = case dict:find(rel_src, Dict) of
		  {ok, {release, _, RA}} ->
		      RA;
		  error ->
		      []
	      end,
    AppName = dict:fetch(app_name, Dict),
    Apps = merge_apps(RelApps, NeededApps, AppInfo) -- [AppName],
    ?report(debug, "Apps = ~p~n", [Apps]),
    KnownApps = [{A,V,D} || {A,V,D} <- AppInfo,
			    V =/= unknown,
			    D =/= unknown],
    search_apps(Apps, Dict, AppInfo, KnownApps).

merge_apps(RelApps, AppApps, AppInfo) ->
    ?report(debug, "merge_apps(~p, ~p, ~p)~n", [RelApps, AppApps, AppInfo]),
    Acc0 = lists:map(fun({App,Vsn,Incl}) ->
			     App;
			({App,Vsn}) ->
			     App;
			(App) when atom(App) ->
			     App
		     end, RelApps),
    with(Acc0,
	 [fun(Acc) ->
		  Acc ++ [A || {A,_,_} <- AppInfo,
			       not(lists:member(A, Acc))]
	  end,
	  fun(Acc) ->
		  Acc ++ [A || A <- AppApps,
			       not(lists:member(A, Acc))]
	  end,
	  fun(Acc) ->
		  InclApps = lists:foldl(
			       fun({_,_,Incls}, AccX) ->
				       AccX ++ Incls;
				  (_, AccX) ->
				       AccX
			       end, [], RelApps),
		  Acc ++ [A || A <- InclApps,
			       not(lists:member(A, Acc))]
	  end]).
			     

app_info(Apps, Dict) ->
    MyName = dict:fetch(app_name, Dict),
    MyVsn = dict:fetch(app_vsn, Dict),
    MyEbin = ebin_dir(Dict),
    Info = lists:map(
	     fun(A) when atom(A) -> {A,unknown,unknown};
		({A,V})          -> {A,V,unknown};
		({A,V,D})        -> {A,V,D}
	     end, Apps),
    AppName = dict:fetch(app_name, Dict),
    case lists:keysearch(AppName, 1, Info) of
	{value, {Ai,Vi,Di}} ->
	    [Version,Dir] = lists:foldr(
			      fun({unknown,Vx}, Acc) -> [Vx|Acc];
				 ({Vx,Vx}, Acc) -> [Vx|Acc];
				 ({V1,V2},_) ->
				      exit({conflicting_info_in_apps,
					    {{Ai,Vi,Di},
					     {MyName,MyVsn,MyEbin}}})
			      end, [], [{Vi,MyVsn},{Di,MyEbin}]),
	    lists:keyreplace(AppName, 1, Info,
			     {AppName, Version, Dir});
	false ->
	    Info ++ [{AppName, dict:fetch(app_vsn, Dict), ebin_dir(Dict)}]
    end.

	      

search_apps(Apps, Dict, AppInfo, InitAcc) ->
    ?report(debug,"search_apps(~p,Dict,~p,~p)~n", [Apps,AppInfo,InitAcc]),
    Path = fetch(path, Dict),
    Ebin = ebin_dir(Dict),
    Path1 = Path ++ [Ebin],
    MyAppName = dict:fetch(app_name, Dict),
    MyApp = {MyAppName, dict:fetch(app_vsn,Dict), ebin_dir(Dict)},
    ?report(debug, "Search Path = ~p~n", [Path1]),
    Found = case expand_path(Path ++ [Ebin], Apps, AppInfo, InitAcc) of
		{FoundInfo, []} ->
		    ?report(debug, "LeftApps = [], FoundInfo = ~p~n", [FoundInfo]),
		    check_found(FoundInfo, Dict);
		{FoundInfo, LeftApps} ->
		    %% LeftApps may still include applications for which we've
		    %% found versions (but didn't know if they were the right ones)
		    ?report(debug, "LeftApps = ~p,~nFoundInfo = ~p~n", 
			[LeftApps, FoundInfo]),
		    case [A || A <- LeftApps,
			       not(lists:keymember(A, 1, FoundInfo))] of
			[] ->
			    check_found(FoundInfo, Dict);
			NotFound ->
			    exit({not_found, NotFound})
		    end
	    end,
    replaceadd(MyAppName,1,Found,MyApp).
	    
check_found(FoundInfo, Dict) ->
    case lists:foldr(
	   fun({A,unknown,D}, {Found,Left}) ->
		   AppF = filename:join(D,atom_to_list(A) ++ ".app"),
		   case vsn_from_app_file(AppF) of
		       unknown ->
			   {Found,[{A,unknown,D}|Left]};
		       FoundVsn ->
			   {[{A,FoundVsn,D}|Found], Left}
		   end;
	      (GoodApp, {Found,Left}) ->
		   {[GoodApp|Found], Left}
	   end, {[],[]}, FoundInfo) of
	{Found, []} ->
	    ?report(debug, "Found apps:~n   ~p~n", [FoundInfo]),
	    Found;
	{Found, UnknownVsns} ->
	    %% These app directories didn't have a version
	    %% suffix. This is allowed, but we must then 
	    %% extract the version some other way.... FFS
	    ?report(debug, "Should find the versions of"
		" these apps:~n   ~p~n", [UnknownVsns]),
	    Found
    end.

replaceadd(Key,Pos,[H|T],Obj) when element(Pos,H) == Key ->
    [Obj|T];
replaceadd(Key,Pos,[H|T],Obj) ->
    [H|replaceadd(Key,Pos,T,Obj)];
replaceadd(Key,Pos,[],Obj) ->
    [Obj].

vsn_from_app_file(AppF) ->
    case file:consult(AppF) of
	{ok, [{application,_,Opts}]} ->
	    case lists:keysearch(vsn,1,Opts) of
		{value,{_,Vsn}} ->
		    Vsn;
		false ->
		    unknown
	    end;
	Other ->
	    ?report(debug, "file:consult(~p) -> ~p~n", [AppF,Other]),
	    unknown
    end.


%%% expand_path/2 takes a list of path expressions, where each expression
%%% may contain wildcards (sh expressions)
expand_path([Dir|Ds], Apps, AppInfo, Acc) ->
    ?report(debug, "Dir = ~p~n", [Dir]),
    SplitDir = filename:split(Dir),
    case file:read_file_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    case lists:reverse(SplitDir) of
		["ebin",AppDir|_] ->
		    case string:tokens(AppDir, "-") of
			[App,Vsn] ->
			    ?report(debug, "App = ~p, Vsn = ~p~n", [App,Vsn]),
			    AppA = list_to_atom(App),
			    {Acc1,Apps1} =
				case lists:member(AppA, Apps) of
				    true ->
					maybe_save_app(AppA, Vsn, Dir, 
						       Apps, AppInfo, Acc);
				    false ->
					{Acc, Apps}
				end,
			    expand_path(Ds, Apps1, AppInfo, Acc1);
			[App] ->
			    %% App dir without version suffix -- legal,
			    %% but we must extract the version somehow
			    AppA = list_to_atom(App),
			    AppF = filename:join(Dir, App ++ ".app"),
			    Vsn = vsn_from_app_file(AppF),
			    case lists:member(AppA, Apps) of
				true ->
				    {Acc1,Apps1} = 
					maybe_save_app(AppA, Vsn, Dir,
						       Apps, AppInfo, Acc),
				    expand_path(Ds, Apps1, AppInfo, Acc1);
				false ->
				    expand_path(Ds, Apps, AppInfo, Acc)
			    end;
			Other ->
			    ?report(debug, "*** strange app dir ~p~n", 
				   [AppDir]),
			    expand_path(Ds, Apps, AppInfo, Acc)
		    end;
		_ ->
		    expand_path(Ds, Apps, AppInfo, Acc)
	    end;
	{error, enoent} ->
	    %% assume it is a regexp
	    {Acc1, RestApps} = expand_path(
				 d_expand(SplitDir), Apps, AppInfo, Acc),
	    expand_path(Ds, RestApps, AppInfo, Acc1);
	_ ->
	    %% something else -- ignore
	    expand_path(Ds, Apps, AppInfo, Acc)
    end;
expand_path([], Apps, AppInfo, Acc) ->
    {Acc, Apps}.


maybe_save_app(AppA, Vsn, Dir, Apps, AppInfo, Acc) ->
    ?report(debug, ("maybe_save_app(~p,~p,~p,_,~n"
	"               ~p,~n"
	"               ~p)~n"), 
	[AppA,Vsn,Dir,AppInfo, Acc]),
    case lists:keysearch(AppA, 1, Acc) of
	{value, {_,unknown,unknown}} ->
	    {lists:keyreplace(
	       AppA,1,Acc,{AppA,Vsn,Dir}),
	     Apps};
	{value, {_,Vsn,_}} ->
	    %% We already have this version
	    {Acc, Apps};
	Result ->
	    case lists:keysearch(AppA, 1, AppInfo) of
		{value, {_,unknown,_}} ->
		    case Result of
			{value, {_,OtherVsn,_}} ->
			    case later_vsn(Vsn, OtherVsn) of
				true ->
				    {lists:keyreplace(
				       AppA,1,Acc,{AppA,Vsn,Dir}),
				     Apps};
				false ->
				    {Acc, Apps}
			    end;
			false ->
			    {[{AppA,Vsn,Dir}|Acc], Apps}
		    end;
		{value, {_, Vsn, unknown}} ->
		    %% This is the version we want. Look no further for 
		    %% this app.
		    {[{AppA,Vsn,Dir}|Acc], Apps -- [AppA]};
		{value, {_, OtherVsn, _}} ->
		    %% This is not the version we want
		    {Acc, Apps};
		false ->
		    %% We have no prior knowledge of this app, other than
		    %% that it should be included
		    case Result of
			{value, {_,OtherVsn,_}} ->
			    case later_vsn(Vsn, OtherVsn) of
				true ->
				    {[{AppA,Vsn,Dir}|Acc], Apps};
				false ->
				    {Acc, Apps}
			    end;
			false ->
			    {[{AppA,Vsn,Dir}|Acc], Apps}
		    end
	    end
    end.


%%% later_vsn(VsnA, VsnB) -> true | false.
%%%  true if VsnA > VsnB, false otherwise
later_vsn(unknown,_) ->
    false;
later_vsn(_,unknown) ->
    true;
later_vsn(Vsn, Vsn) ->
    false;
later_vsn(VsnA, VsnB) ->
    case get_newest([{VsnB,[]},{VsnA,[]}]) of
	{VsnB,_} ->
	    false;
	{VsnA,[]} ->
	    true
    end.

%%% get_newest([{Vsn,Info}]) -> {NewestVsn,RelatedInfo}.
%%%
get_newest([{Vsn,_}] =X) -> X;
get_newest([{Vsn1,_}=X1, {Vsn2,_}=X2 | Rest]) ->
    V1 = split(Vsn1),
    V2 = split(Vsn2),
    Newest = get_newest1(V1, V2, X1, X2),
    get_newest([Newest | Rest]).

get_newest1([H1|T1], [H2|T2], X1, X2) ->
    if  H1 > H2 -> X1;
        H2 > H1 -> X2;
        H1 == H2 ->
            get_newest1(T1, T2, X1, X2)
    end;
get_newest1([], [], X1, X2) -> X1;
get_newest1([H1|_], [], X1, X2) -> X1;
get_newest1([], [H2|_], X1, X2) -> X2.

split(VStr) ->
    split(VStr, undefined, [], []).

split([], Mode, Acc1, Acc) ->
    lists:reverse([subv(Mode, lists:reverse(Acc1)) | Acc]);
split("." ++ T, Mode, Acc1, Acc) ->
    split(T, Mode, [], [subv(Mode, lists:reverse(Acc1)) | Acc]);
split([I|T], string, Acc1, Acc) when $0 =< I, I =< $9 ->
    split(T, integer, [I], [subv(string, lists:reverse(Acc1)) | Acc]);
split([I|T], integer, Acc1, Acc) when $0 =< I, I =< $9 ->
    split(T, integer, [I|Acc1], Acc);
split([C|T], integer, Acc1, Acc) ->
    split(T, string, [C], [subv(integer, lists:reverse(Acc1)) | Acc]);
split([I|T], undefined, [], Acc) when $0 =< I, I =< $9 ->
    split(T, integer, [I], Acc);
split([C|T], undefined, [], Acc) ->
    split(T, string, [C], Acc).

subv(integer, SubV) ->
    list_to_integer(SubV);
subv(string, SubV) ->
    SubV.

%%% end later_vsn()


member_of(AppName, [AppName|As]) ->
    {true, AppName};
member_of(AppName, [{AppName,Vsn}|As]) ->
    {true, {AppName, Vsn}};
member_of(AppName, [_|As]) ->
    member_of(AppName, As);
member_of(_, []) ->
    false.


d_expand(["/"|Ds]) ->
    d_expand(Ds, ["/"]);
d_expand(Ds) ->
    {ok,Cwd} = file:get_cwd(),
    d_expand(Ds, [Cwd]).


d_expand([D|Ds], Cur) ->
    Pat = regexp:sh_to_awk(D),
    Cur1 = lists:foldl(
	     fun(C, Acc) ->
		     case list_dir(C) of
			 [] ->
			     Acc;
			 Fs ->
			     Matches = [F || F <- Fs,
					     matches(F, Pat)],
			     Acc ++ [filename:join(C,F) || F <- Matches,
							   is_dir(C,F)]
		     end
	     end, [], Cur),
    d_expand(Ds, Cur1);
d_expand([], Cur) ->
    Cur.

matches(Str, Pat) ->
    case regexp:match(Str,Pat) of
	{match,_,_} ->
	    true;
	nomatch ->
	    false
    end.


is_dir(Parent, Dir) ->
    case file:read_file_info(filename:join(Parent, Dir)) of
	{ok, #file_info{type=directory}} ->
	    true;
	_ ->
	    false
    end.

list_dir(Dir) ->
    case file:read_file_info(Dir) of
	{ok, #file_info{type=directory}} ->
	    case file:list_dir(Dir) of
		{ok, Fs} ->
		    Fs;
		_ ->
		    []
	    end;
	_ ->
	    []
    end.
    
    

apps(Apps, Dict) ->
    Path = fetch(path, Dict),
    Pat = [filename:basename(filename:dirname(Dir))||Dir<-get_path(Dict)],
    Vss = [string:tokens(D,"-") || D <- Pat, app(D)],
    do_apps(Apps, Vss).

app(D) ->
    case regexp:match(D,".*-[0-9].*") of
	nomatch -> false;
	_ -> true
    end.
do_apps([], Vss) -> [];
do_apps([App|Apps], Vss) when list(App) -> 
    do_apps([list_to_atom(App)|Apps], Vss);
do_apps([App|Apps], Vss) when atom(App) ->
    [{App, get_vsn(atom_to_list(App), Vss)}|do_apps(Apps, Vss)].


get_vsn(App, [[App, Vsn]|_]) -> Vsn;
get_vsn(App, [_|T]) -> get_vsn(App, T).

get_erts_vsn() ->
    {ok, [[Root]]} = init:get_argument(root),
    {ok, Fs} = file:list_dir(Root),
    get_erts_vsn(Fs).
get_erts_vsn([[$e,$r,$t,$s,$-|Vsn]|_]) -> Vsn;
get_erts_vsn([_|T]) -> get_erts_vsn(T).

out(Dir, App, Ext, Term) ->
    FN = filename:join(Dir, App++Ext),
    out(FN, Term).

out(FN, Term) ->
    ?report(debug, "out(~p,~p)~n", [FN,Term]),
    case file:open(FN, [write]) of
	{ok, FD} ->
	    io:fwrite(FD, "~p.~n", [Term]),
	    file:close(FD);
	{error, R} -> exit({bad_filename, {FN, R}})
    end.

read_app_file(Dict) ->
    AppName = fetch(app_name, Dict),
    AppNameS = atom_to_list(AppName),
    AppF = get_app_filename(AppNameS, Dict),
    App = case file:consult(AppF) of
	      {ok, [{application, _Name, Options}]} ->
		  app_options(Options);
	      {ok, [Options]} when list(Options) ->
		  app_options(Options);
	      {error, enoent} ->
		  RealAppF = filename:join(ebin_dir(Dict), 
					   AppNameS ++ ".app"),
		  case file:consult(RealAppF) of
		      {ok, [{application, _Name, Options}]} ->
			  app_options(Options);
		      {error, enoent} ->
			  ?report(debug,
			      "no app file (~p) -- must generate one.~n",
			      [AppName]),
			  Options = 
			      app_options(
				[{description, 
				  "auto-generated app file for " ++
				  AppNameS},
				 {applications, [kernel,stdlib]}])
		  end;
	      Other ->
		  exit({error_reading_app_file, {AppF, Other}})
	  end,
    ?report(debug, "App = ~p~n", [App]),
    App.

app_options(Opts) ->
    [opt(description,Opts,""), opt(id,Opts,""),
     opt(registered,Opts,[]), opt(applications,Opts,[kernel,stdlib]),
     opt(env,Opts,[]), opt(mod,Opts,[]),
     opt(start_phases,Opts,undefined)].
    

opt(Key,List,Default) ->
    case lists:keysearch(Key,1,List) of
	{value, {_, Value}} ->
	    {Key, Value};
	false ->
	    {Key, Default}
    end.

get_path(Dict) ->
    AppPath = ebin_dir(Dict),
    Path = code:get_path(),
    case lists:member(AppPath, Path) of
	true ->
	    Path;
	false ->
	    Path ++ [AppPath]
    end.

% otp_path(Dict) ->
%     case dict:find(otp_path, Dict) of
% 	{ok, Path} ->
% 	    Path;
% 	error ->
% 	    {ok,[[Root]]} = init:get_argument(root),
% 	    filename:join([Root,"lib","*","ebin"])
%     end.
    
script(File) ->
    script(File, erl_eval:new_bindings()).

script(File, Bs) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    Bs1 = erl_eval:add_binding('ScriptName',File,Bs),
	    R = case eval_stream(Fd, Bs1) of
		    {ok, Result} ->
			{ok, Result};
		    Error ->
			Error
		end,
	    file:close(Fd),
	    R;
	Error ->
	    Error
    end.

path_script(Path, File) ->
    path_script(Path, File, erl_eval:new_bindings()).

path_script(Path, File, Bs) ->
    case file:path_open(Path, File, [read]) of
	{ok,Fd,Full} ->
	    Bs1 = erl_eval:add_binding('ScriptName',Full,Bs),
	    case eval_stream(Fd, Bs1) of
		{error,E} ->
		    file:close(Fd),
		    {error, {E, Full}};
		{ok, R} ->
		    file:close(Fd),
		    {ok,R,Full}
	    end;
	Error ->
	    Error
    end.
    
eval_stream(Fd, Bs) ->
    eval_stream(Fd, undefined, Bs).

eval_stream(Fd, Last, Bs) ->
    eval_stream(io:parse_erl_exprs(Fd, ''), Fd, Last, Bs).


eval_stream({ok,Form,EndLine}, Fd, Last, Bs0) ->
    case catch erl_eval:exprs(Form, Bs0) of
	{value,V,Bs} ->
	    eval_stream(Fd, {V}, Bs);
	{'EXIT',Reason} ->
	    {error, Reason}
    end;
eval_stream({error,What,EndLine}, Fd, L, Bs) ->
    {error, {parse_error, {What,EndLine}}};
eval_stream({eof,EndLine}, Fd, Last, Bs) ->
    case Last of
	{Val} ->
	    {ok, Val};
	undefined ->
	    %% empty script
	    {error, empty_script}
    end.


do_report(error) ->
    %% always report errors
    true;
do_report(Level) ->
    LevelN = rpt_level(Level),
    case get(builder_debug) of
	true ->
	    true;
	false ->
	    false;
	undefined ->
	    case get(builder_report_level) of
		undefined ->
		    false;
		[{AtLevel,_}|_] when AtLevel >= LevelN ->
		    true;
		_ ->
		    false
	    end
    end.

rpt_level(none) -> 0;
rpt_level(progress) -> 1;
rpt_level(verbose) -> 2;
rpt_level(debug) -> 3;
rpt_level(N) when integer(N), N >= 0 ->
    N.
    
push_report_level(Level, Dict) ->
    Ref = fetch('#ref#', Dict),
    N = rpt_level(Level),
    case get(builder_report_level) of
	undefined ->
	    put(builder_report_level, [{N,Ref}]),
	    io:format("pushed 1st report level ~p(~p)~n",[Level,N]);
	[{Prev,Ref}|Tail] ->
	    put(builder_report_level, [{N,Ref}|Tail]),
	    io:format("redefined report level ~p(~p)~n",[Level,N]);
	[_|_] = Levels ->
	    put(builder_report_level, [{N,Ref}|Levels]),
	    io:format("pushed next report level ~p(~p)~n",[Level,N])
    end.

pop_report_level() ->
    Level = get(builder_report_level),
    case Level of
	[_] ->
	    erase(builder_report_level);
	[_|Tail] ->
	    put(builder_report_level, Tail);
	undefined ->
	    ok
    end.



verify_dir(Dir) ->
    case file:read_file_info(Dir) of
        {ok, FI} when FI#file_info.type == directory,
                      FI#file_info.access == read_write ->
            ok;
        {ok, FI} when FI#file_info.type == directory ->
            exit({access, Dir});
        {error, enoent} ->
            try_create(Dir)
    end.

try_create("/") ->
    exit({create, "/"});
try_create(".") ->
    exit({create, "/"});
try_create(Dir) ->
    case file:make_dir(Dir) of
        {error, Reason} ->
            try_create(filename:dirname(Dir)),
            try_create(Dir);
        ok ->
            ok
    end.
