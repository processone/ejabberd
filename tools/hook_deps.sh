#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

-record(state, {run_hooks = dict:new(),
		run_fold_hooks = dict:new(),
		hooked_funs = dict:new(),
		mfas = dict:new(),
		specs = dict:new(),
		module :: module(),
		file :: filename:filename()}).

main([Dir]) ->
    State =
	fold_beams(
	  fun(File0, Tree, Acc0) ->
		  BareName = filename:rootname(filename:basename(File0)),
		  Mod = list_to_atom(BareName),
		  File = BareName ++ ".erl",
		  Acc1 = Acc0#state{file = File, module = Mod},
		  erl_syntax_lib:fold(
		    fun(Form, Acc) ->
			    case erl_syntax:type(Form) of
				application ->
				    case erl_syntax_lib:analyze_application(Form) of
					{ejabberd_hooks, {run, N}}
					  when N == 2; N == 3 ->
					    analyze_run_hook(Form, Acc);
					{ejabberd_hooks, {run_fold, N}}
					  when N == 3; N == 4 ->
					    analyze_run_fold_hook(Form, Acc);
					{ejabberd_hooks, {add, N}}
					  when N == 4; N == 5 ->
					    analyze_run_fun(Form, Acc);
					{gen_iq_handler, {add_iq_handler, N}}
					  when N == 5; N == 6 ->
					    analyze_iq_handler(Form, Acc);
					_ ->
					    Acc
				    end;
				attribute ->
				    case catch erl_syntax_lib:analyze_attribute(Form) of
					{spec, _} ->
					    analyze_type_spec(Form, Acc);
					_ ->
					    Acc
				    end;
				_ ->
				    Acc
			    end
		    end, Acc1, Tree)
	  end, #state{}, Dir),
    report_orphaned_funs(State),
    RunDeps = build_deps(State#state.run_hooks, State#state.hooked_funs),
    RunFoldDeps = build_deps(State#state.run_fold_hooks, State#state.hooked_funs),
    emit_module(RunDeps, RunFoldDeps, State#state.specs, Dir, hooks_type_test).

analyze_run_hook(Form, State) ->
    [Hook|Tail] = erl_syntax:application_arguments(Form),
    case atom_value(Hook, State) of
	undefined ->
	    State;
	HookName ->
	    Args = case Tail of
		       [_Host, Args0] -> Args0;
		       [Args0] ->
			   Args0
		   end,
	    Arity = erl_syntax:list_length(Args),
	    Hooks = dict:store({HookName, Arity},
			       {State#state.file, erl_syntax:get_pos(Hook)},
			       State#state.run_hooks),
	    State#state{run_hooks = Hooks}
    end.

analyze_run_fold_hook(Form, State) ->
    [Hook|Tail] = erl_syntax:application_arguments(Form),
    case atom_value(Hook, State) of
	undefined ->
	    State;
	HookName ->
	    Args = case Tail of
		       [_Host, _Val, Args0] -> Args0;
		       [_Val, Args0] -> Args0
		   end,
	    Arity = erl_syntax:list_length(Args) + 1,
	    Hooks = dict:store({HookName, Arity},
			       {State#state.file, erl_syntax:get_pos(Form)},
			       State#state.run_fold_hooks),
	    State#state{run_fold_hooks = Hooks}
    end.

analyze_run_fun(Form, State) ->
    [Hook|Tail] = erl_syntax:application_arguments(Form),
    case atom_value(Hook, State) of
	undefined ->
	    State;
	HookName ->
	    {Module, Fun, Seq} = case Tail of
				     [_Host, M, F, S] ->
					 {M, F, S};
				     [M, F, S] ->
					 {M, F, S}
				 end,
	    ModName = module_name(Module, State),
	    FunName = atom_value(Fun, State),
	    if ModName /= undefined, FunName /= undefined ->
		    Funs = dict:append(
			     HookName,
			     {ModName, FunName, integer_value(Seq, State),
			      {State#state.file, erl_syntax:get_pos(Form)}},
			     State#state.hooked_funs),
		    State#state{hooked_funs = Funs};
	       true ->
		    State
	    end
    end.

analyze_iq_handler(Form, State) ->
    [_Component, _Host, _NS, Module, Function|_] =
	erl_syntax:application_arguments(Form),
    Mod = module_name(Module, State),
    Fun = atom_value(Function, State),
    if Mod /= undefined, Fun /= undefined ->
	    code:ensure_loaded(Mod),
	    case erlang:function_exported(Mod, Fun, 1) of
		false ->
		    err("~s:~p: Error: function ~s:~s/1 is registered "
			"as iq handler, but is not exported~n",
			[State#state.file, erl_syntax:get_pos(Form),
			 Mod, Fun]);
		true ->
		    ok
	    end;
       true ->
	    ok
    end,
    State.

analyze_type_spec(Form, State) ->
    case catch erl_syntax:revert(Form) of
	{attribute, _, spec, {{F, A}, _}} ->
	    Specs = dict:store({State#state.module, F, A},
			       {Form, State#state.file},
			       State#state.specs),
	    State#state{specs = Specs};
	_ ->
	    State
    end.

build_deps(Hooks, Hooked) ->
    dict:fold(
      fun({Hook, Arity}, {_File, _LineNo} = Meta, Deps) ->
	      case dict:find(Hook, Hooked) of
		  {ok, Funs} ->
		      ExportedFuns =
			  lists:flatmap(
			    fun({M, F, Seq, {FunFile, FunLineNo} = FunMeta}) ->
				    code:ensure_loaded(M),
				    case erlang:function_exported(M, F, Arity) of
					false ->
					    err("~s:~p: Error: function ~s:~s/~p "
						"is hooked on ~s/~p, but is not "
						"exported~n",
						[FunFile, FunLineNo, M, F,
						 Arity, Hook, Arity]),
					    [];
					true ->
					    [{{M, F, Arity}, Seq, FunMeta}]
				    end
			    end, Funs),
		      dict:append_list({Hook, Arity, Meta}, ExportedFuns, Deps);
		  error ->
		      %% log("~s:~p: Warning: hook ~p/~p is unused~n",
		      %% 	  [_File, _LineNo, Hook, Arity]),
		      dict:append_list({Hook, Arity, Meta}, [], Deps)
	      end
      end, dict:new(), Hooks).

report_orphaned_funs(State) ->
    dict:map(
      fun(Hook, Funs) ->
	      lists:foreach(
		fun({M, F, _, {File, Line}}) ->
			case get_fun_arities(M, F, State) of
			    [] ->
				err("~s:~p: Error: function ~s:~s is "
				    "hooked on hook ~s, but is not exported~n",
				    [File, Line, M, F, Hook]);
			    Arities ->
				case lists:any(
				       fun(Arity) ->
					       dict:is_key({Hook, Arity},
							   State#state.run_hooks) orelse
						   dict:is_key({Hook, Arity},
							       State#state.run_fold_hooks);
					  (_) ->
					       false
				       end, Arities) of
				    false ->
					Arity = hd(Arities),
					err("~s:~p: Error: function ~s:~s/~p is hooked"
					    " on non-existent hook ~s/~p~n",
					    [File, Line, M, F, Arity, Hook, Arity]);
				    true ->
					ok
				end
			end
		end, Funs)
      end, State#state.hooked_funs).

get_fun_arities(Mod, Fun, _State) ->
    proplists:get_all_values(Fun, Mod:module_info(exports)).

module_name(Form, State) ->
    try
	Name = erl_syntax:macro_name(Form),
	'MODULE' = erl_syntax:variable_name(Name),
	State#state.module
    catch _:_ ->
	    atom_value(Form, State)
    end.

atom_value(Form, State) ->
    case erl_syntax:type(Form) of
	atom ->
	    erl_syntax:atom_value(Form);
	_ ->
	    log("~s:~p: Warning: not an atom: ~s~n",
		[State#state.file,
		 erl_syntax:get_pos(Form),
		 erl_prettypr:format(Form)]),
	    undefined
    end.

integer_value(Form, State) ->
    case erl_syntax:type(Form) of
	integer ->
	    erl_syntax:integer_value(Form);
	_ ->
	    log("~s:~p: Warning: not an integer: ~s~n",
		[State#state.file,
		 erl_syntax:get_pos(Form),
		 erl_prettypr:format(Form)]),
	    0
    end.

emit_module(RunDeps, RunFoldDeps, Specs, _Dir, Module) ->
    File = filename:join(["src", Module]) ++ ".erl",
    try
	{ok, Fd} = file:open(File, [write]),
	write(Fd, "-module(~s).~n~n", [Module]),
	emit_export(Fd, RunDeps, "run hooks"),
	emit_export(Fd, RunFoldDeps, "run_fold hooks"),
	emit_run_hooks(Fd, RunDeps, Specs),
	emit_run_fold_hooks(Fd, RunFoldDeps, Specs),
	file:close(Fd),
	log("Module written to file ~s~n", [File])
    catch _:{badmatch, {error, Reason}} ->
	    err("writing to ~s failed: ~s", [File, file:format_error(Reason)])
    end.

emit_run_hooks(Fd, Deps, Specs) ->
    DepsList = lists:sort(dict:to_list(Deps)),
    lists:foreach(
      fun({{Hook, Arity, {File, LineNo}}, []}) ->
	      Args = lists:duplicate(Arity, "_"),
	      write(Fd, "%% called at ~s:~p~n", [File, LineNo]),
	      write(Fd, "~s(~s) -> ok.~n~n", [Hook, string:join(Args, ", ")]);
	 ({{Hook, Arity, {File, LineNo}}, Funs}) ->
	      emit_specs(Fd, Funs, Specs),
	      write(Fd, "%% called at ~s:~p~n", [File, LineNo]),
	      Args = string:join(
		       [[N] || N <- lists:sublist(lists:seq($A, $Z), Arity)],
		       ", "),
	      write(Fd, "~s(~s) ->~n    ", [Hook, Args]),
	      Calls = [io_lib:format("~s:~s(~s)", [Mod, Fun, Args])
		       || {{Mod, Fun, _}, _Seq, _} <- lists:keysort(2, Funs)],
	      write(Fd, "~s.~n~n", [string:join(Calls, ",\n    ")])
      end, DepsList).

emit_run_fold_hooks(Fd, Deps, Specs) ->
    DepsList = lists:sort(dict:to_list(Deps)),
    lists:foreach(
      fun({{Hook, Arity, {File, LineNo}}, []}) ->
	      write(Fd, "%% called at ~s:~p~n", [File, LineNo]),
	      Args = ["Acc"|lists:duplicate(Arity - 1, "_")],
	      write(Fd, "~s(~s) -> Acc.~n~n", [Hook, string:join(Args, ", ")]);
	 ({{Hook, Arity, {File, LineNo}}, Funs}) ->
	      emit_specs(Fd, Funs, Specs),
	      write(Fd, "%% called at ~s:~p~n", [File, LineNo]),
	      Args = [[N] || N <- lists:sublist(lists:seq($A, $Z), Arity - 1)],
	      write(Fd, "~s(~s) ->~n    ", [Hook, string:join(["Acc0"|Args], ", ")]),
	      {Calls, _} = lists:mapfoldl(
			     fun({{Mod, Fun, _}, _Seq, _}, N) ->
				     Args1 = ["Acc" ++ integer_to_list(N)|Args],
				     {io_lib:format("Acc~p = ~s:~s(~s)",
						    [N+1, Mod, Fun,
						     string:join(Args1, ", ")]),
				      N + 1}
			     end, 0, lists:keysort(2, Funs)),
	      write(Fd, "~s,~n", [string:join(Calls, ",\n    ")]),
	      write(Fd, "    Acc~p.~n~n", [length(Funs)])
      end, DepsList).

emit_export(Fd, Deps, Comment) ->
    DepsList = lists:sort(dict:to_list(Deps)),
    Exports = lists:map(
		fun({{Hook, Arity, _}, _}) ->
			io_lib:format("~s/~p", [Hook, Arity])
		end, DepsList),
    write(Fd, "%% ~s~n-export([~s]).~n~n",
	      [Comment, string:join(Exports, ",\n         ")]).

emit_specs(Fd, Funs, Specs) ->
    lists:foreach(
      fun({{M, _, _} = MFA, _, _}) ->
	      case dict:find(MFA, Specs) of
		  {ok, {Form, _File}} ->
		      Lines = string:tokens(erl_syntax:get_ann(Form), "\n"),
		      lists:foreach(
			fun("%" ++ _) ->
				ok;
			   ("-spec" ++ Spec) ->
				write(Fd, "%% -spec ~p:~s~n",
				      [M, string:strip(Spec, left)]);
			   (Line) ->
				write(Fd, "%% ~s~n", [Line])
			end, Lines);
		  error ->
		      ok
	      end
      end, lists:keysort(2, Funs)).

fold_beams(Fun, State, Dir) ->
    filelib:fold_files(
      Dir, ".+\.beam\$", false,
      fun(File, Acc) ->
	      AbsCode = get_code_from_beam(File),
	      lists:foldl(
		fun(Form, Acc1) ->
			Fun(File, Form, Acc1)
		end, Acc, AbsCode)
      end, State).

get_code_from_beam(File) ->
    try
	{ok, {_, List}} = beam_lib:chunks(File, [abstract_code]),
        {_, {raw_abstract_v1, Forms}} = lists:keyfind(abstract_code, 1, List),
	Forms
    catch _:{badmatch, _} ->
	    err("no abstract code found in ~s~n", [File])
    end.

log(Format, Args) ->
    io:format(standard_io, Format, Args).

err(Format, Args) ->
    io:format(standard_error, "Error: " ++ Format, Args),
    halt(1).

write(Fd, Format, Args) ->
    file:write(Fd, io_lib:format(Format, Args)).
