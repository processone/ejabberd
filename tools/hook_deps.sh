#!/usr/bin/env escript
%% -*- erlang -*-

-record(state, {run_hooks = #{},
		run_fold_hooks = #{},
		hooked_funs = {#{}, #{}},
		iq_handlers = {#{}, #{}},
		exports = #{},
		module :: module(),
		file :: filename:filename()}).

main(Paths) ->
    State =
	fold_beams(
	  fun(File0, Tree, X, Acc0) ->
		  BareName = filename:rootname(filename:basename(File0)),
		  Mod = list_to_atom(BareName),
		  File = BareName ++ ".erl",
		  Exports = maps:put(Mod, X, Acc0#state.exports),
		  Acc1 = Acc0#state{file = File, module = Mod, exports = Exports},
		  erl_syntax_lib:fold(
		    fun(Form, Acc) ->
		  	    case erl_syntax:type(Form) of
		  		application ->
		  		    case erl_syntax_lib:analyze_application(Form) of
		  			{ejabberd_hooks, {run, N}} when N == 2; N == 3 ->
		  			    collect_run_hook(Form, Acc);
		  			{ejabberd_hooks, {run_fold, N}} when N == 3; N == 4 ->
		  			    collect_run_fold_hook(Form, Acc);
		  			{ejabberd_hooks, {add, N}} when N == 4; N == 5 ->
		  			    collect_run_fun(Form, add, Acc);
		  			{ejabberd_hooks, {delete, N}} when N == 4; N == 5 ->
		  			    collect_run_fun(Form, delete, Acc);
		  			{gen_iq_handler, {add_iq_handler, 5}} ->
		  			    collect_iq_handler(Form, add, Acc);
		  			{gen_iq_handler, {remove_iq_handler, 3}} ->
		  			    collect_iq_handler(Form, delete, Acc);
		  			_ ->
		  			    Acc
		  		    end;
		  		_ ->
		  		    Acc
		  	    end
		    end, Acc1, Tree)
	  end, #state{}, Paths),
    check_hooks_arity(State#state.run_hooks),
    check_hooks_arity(State#state.run_fold_hooks),
    check_iq_handlers_export(State#state.iq_handlers, State#state.exports),
    analyze_iq_handlers(State#state.iq_handlers),
    analyze_hooks(State#state.hooked_funs),
    RunDeps = build_deps(State#state.run_hooks, State#state.hooked_funs),
    RunFoldDeps = build_deps(State#state.run_fold_hooks, State#state.hooked_funs),
    emit_module(RunDeps, RunFoldDeps, hooks_type_test).

collect_run_hook(Form, State) ->
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
	    Hooks = maps:put({HookName, Arity},
			     {State#state.file, erl_syntax:get_pos(Hook)},
			     State#state.run_hooks),
	    State#state{run_hooks = Hooks}
    end.

collect_run_fold_hook(Form, State) ->
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
	    Hooks = maps:put({HookName, Arity},
			     {State#state.file, erl_syntax:get_pos(Form)},
			     State#state.run_fold_hooks),
	    State#state{run_fold_hooks = Hooks}
    end.

collect_run_fun(Form, Action, State) ->
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
	    SeqInt = integer_value(Seq, State),
	    if ModName /= undefined, FunName /= undefined, SeqInt /= undefined ->
		    Pos = case Action of
			      add -> 1;
			      delete -> 2
			  end,
		    Funs = maps_append(
			     HookName,
			     {ModName, FunName, SeqInt,
			      {State#state.file, erl_syntax:get_pos(Form)}},
			     element(Pos, State#state.hooked_funs)),
		    Hooked = setelement(Pos, State#state.hooked_funs, Funs),
		    State#state{hooked_funs = Hooked};
	       true ->
		    State
	    end
    end.

collect_iq_handler(Form, add, #state{iq_handlers = {Add, Del}} = State) ->
    [Component, _Host, Namespace, Module, Function] = erl_syntax:application_arguments(Form),
    Mod = module_name(Module, State),
    Fun = atom_value(Function, State),
    Comp = atom_value(Component, State),
    NS = binary_value(Namespace, State),
    if Mod /= undefined, Fun /= undefined, Comp /= undefined, NS /= undefined ->
	    Handlers = maps_append(
			 {Comp, NS},
			 {Mod, Fun,
			  {State#state.file, erl_syntax:get_pos(Form)}},
			 Add),
	    State#state{iq_handlers = {Handlers, Del}};
       true ->
	    State
    end;
collect_iq_handler(Form, delete, #state{iq_handlers = {Add, Del}} = State) ->
    [Component, _Host, Namespace] = erl_syntax:application_arguments(Form),
    Comp = atom_value(Component, State),
    NS = binary_value(Namespace, State),
    if Comp /= undefined, NS /= undefined ->
	    Handlers = maps_append(
			 {Comp, NS},
			 {State#state.file, erl_syntax:get_pos(Form)},
			 Del),
	    State#state{iq_handlers = {Add, Handlers}};
       true ->
	    State
    end.

check_hooks_arity(Hooks) ->
    maps:fold(
      fun({Hook, Arity}, _, M) ->
	      case maps:is_key(Hook, M) of
		  true ->
		      err("Error: hook ~s is called with different "
			  "number of arguments~n", [Hook]);
		  false ->
		      maps:put(Hook, Arity, M)
	      end
      end, #{}, Hooks).

check_iq_handlers_export({HookedFuns, _}, Exports) ->
    maps:map(
      fun(_, Funs) ->
	      lists:foreach(
		fun({Mod, Fun, {File, FileNo}}) ->
			case is_exported(Mod, Fun, 1, Exports) of
			    true -> ok;
			    false ->
				err("~s:~B: Error: "
				    "iq handler is registered on unexported function: "
				    "~s:~s/1~n", [File, FileNo, Mod, Fun])
			end
		end, Funs)
      end, HookedFuns).

analyze_iq_handlers({Add, Del}) ->
    maps:map(
      fun(Handler, Funs) ->
	      lists:foreach(
		fun({_, _, {File, FileNo}}) ->
			case maps:is_key(Handler, Del) of
			    true -> ok;
			    false ->
				err("~s:~B: Error: "
				    "iq handler is added but not removed~n",
				    [File, FileNo])
			end
		end, Funs)
      end, Add),
    maps:map(
      fun(Handler, Meta) ->
	      lists:foreach(
		fun({File, FileNo}) ->
			case maps:is_key(Handler, Add) of
			    true -> ok;
			    false ->
				err("~s:~B: Error: "
				    "iq handler is removed but not added~n",
				    [File, FileNo])
			end
		end, Meta)
      end, Del).

analyze_hooks({Add, Del}) ->
    Del1 = maps:fold(
	     fun(Hook, Funs, D) ->
		     lists:foldl(
		       fun({Mod, Fun, Seq, {File, FileNo}}, D1) ->
			       maps:put({Hook, Mod, Fun, Seq}, {File, FileNo}, D1)
		       end, D, Funs)
	     end, #{}, Del),
    Add1 = maps:fold(
	     fun(Hook, Funs, D) ->
		     lists:foldl(
		       fun({Mod, Fun, Seq, {File, FileNo}}, D1) ->
			       maps:put({Hook, Mod, Fun, Seq}, {File, FileNo}, D1)
		       end, D, Funs)
	     end, #{}, Add),
    lists:foreach(
      fun({{Hook, Mod, Fun, _} = Key, {File, FileNo}}) ->
	      case maps:is_key(Key, Del1) of
		  true -> ok;
		  false ->
		      err("~s:~B: Error: "
			  "hook ~s->~s->~s is added but was never removed~n",
			  [File, FileNo, Hook, Mod, Fun])
	      end
      end, maps:to_list(Add1)),
    lists:foreach(
      fun({{Hook, Mod, Fun, _} = Key, {File, FileNo}}) ->
	      case maps:is_key(Key, Add1) of
		  true -> ok;
		  false ->
		      err("~s:~B: Error: "
			  "hook ~s->~s->~s is removed but was never added~n",
			  [File, FileNo, Hook, Mod, Fun])
	      end
      end, maps:to_list(Del1)).

build_deps(Hooks, {HookedFuns, _}) ->
    maps:fold(
      fun({Hook, Arity}, Meta, Deps) ->
	      case maps:find(Hook, HookedFuns) of
		  {ok, Funs} ->
		      ExportedFuns =
			  lists:map(
			    fun({M, F, Seq, FunMeta}) ->
				    {{M, F, Arity}, Seq, FunMeta}
			    end, Funs),
		      maps_append_list({Hook, Arity, Meta}, ExportedFuns, Deps);
		  error ->
		      maps_append_list({Hook, Arity, Meta}, [], Deps)
	      end
      end, #{}, Hooks).

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
	    warn_type(Form, State, "not an atom"),
	    undefined
    end.

integer_value(Form, State) ->
    case erl_syntax:type(Form) of
	integer ->
	    erl_syntax:integer_value(Form);
	_ ->
	    warn_type(Form, State, "not an integer"),
	    undefined
    end.

binary_value(Form, State) ->
    try erl_syntax:concrete(Form) of
	Binary when is_binary(Binary) ->
	    Binary;
	_ ->
	    warn_type(Form, State, "not a binary"),
	    undefined
    catch _:_ ->
	    warn_type(Form, State, "not a binary"),
	    undefined
    end.

is_exported(Mod, Fun, Arity, Exports) ->
    try maps:get(Mod, Exports) of
	L -> lists:member({Fun, Arity}, L)
    catch _:{badkey, _} -> false
    end.

warn_type(Form, State, Warning) ->
    log("~s:~p: Warning: " ++ Warning ++ ": ~s~n",
	[State#state.file,
	 erl_syntax:get_pos(Form),
	 erl_prettypr:format(Form)]).

emit_module(RunDeps, RunFoldDeps, Module) ->
    File = filename:join(["src", Module]) ++ ".erl",
    try
	{ok, Fd} = file:open(File, [write]),
	write(Fd,
	      "%% Generated automatically~n"
	      "%% DO NOT EDIT: run `make hooks` instead~n~n", []),
	write(Fd, "-module(~s).~n", [Module]),
	write(Fd, "-compile(nowarn_unused_vars).~n", []),
	write(Fd, "-dialyzer(no_return).~n~n", []),
	emit_export(Fd, RunDeps, "run hooks"),
	emit_export(Fd, RunFoldDeps, "run_fold hooks"),
	emit_run_hooks(Fd, RunDeps),
	emit_run_fold_hooks(Fd, RunFoldDeps),
	file:close(Fd),
	log("Module written to ~s~n", [File])
    catch _:{badmatch, {error, Reason}} ->
	    err("Error: writing to ~s failed: ~s", [File, file:format_error(Reason)])
    end.

emit_run_hooks(Fd, Deps) ->
    DepsList = lists:sort(maps:to_list(Deps)),
    lists:foreach(
      fun({{Hook, Arity, {File, LineNo}}, Funs}) ->
	      write(Fd, "%% called at ~s:~p~n", [File, LineNo]),
	      Args = string:join(
		       [[N] || N <- lists:sublist(lists:seq($A, $Z), Arity)],
		       ", "),
	      write(Fd, "~s(~s) ->~n    ", [Hook, Args]),
	      Calls = [io_lib:format("_ = ~s:~s(~s)", [Mod, Fun, Args])
		       || {{Mod, Fun, _}, _Seq, _} <- lists:keysort(2, Funs)],
	      write(Fd, "~s.~n~n",
		    [string:join(Calls ++ ["ok"], ",\n    ")])
      end, DepsList).

emit_run_fold_hooks(Fd, Deps) ->
    DepsList = lists:sort(maps:to_list(Deps)),
    lists:foreach(
      fun({{Hook, Arity, {File, LineNo}}, []}) ->
	      write(Fd, "%% called at ~s:~p~n", [File, LineNo]),
	      Args = ["Acc"|lists:duplicate(Arity - 1, "_")],
	      write(Fd, "~s(~s) -> Acc.~n~n", [Hook, string:join(Args, ", ")]);
	 ({{Hook, Arity, {File, LineNo}}, Funs}) ->
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
    DepsList = lists:sort(maps:to_list(Deps)),
    Exports = lists:map(
		fun({{Hook, Arity, _}, _}) ->
			io_lib:format("~s/~p", [Hook, Arity])
		end, DepsList),
    write(Fd, "%% ~s~n-export([~s]).~n~n",
	      [Comment, string:join(Exports, ",\n         ")]).

fold_beams(Fun, State, Paths) ->
    Paths1 = fold_paths(Paths),
    Total = length(Paths1),
    {_, State1} =
	lists:foldl(
	  fun(File, {I, Acc}) ->
		  io:format("Progress: ~B% (~B/~B)\r",
			    [round(I*100/Total), I, Total]),
		  case is_elixir_beam(File) of
		      true -> {I+1, Acc};
		      false ->
			  {AbsCode, Exports} = get_code_from_beam(File),
			  Acc2 = lists:foldl(
				   fun(Form, Acc1) ->
					   Fun(File, Form, Exports, Acc1)
				   end, Acc, AbsCode),
			  {I+1, Acc2}
		  end
	  end, {0, State}, Paths1),
    State1.

fold_paths(Paths) ->
    lists:flatmap(
      fun(Path) ->
	      case filelib:is_dir(Path) of
		  true ->
		      lists:reverse(
			filelib:fold_files(
			  Path, ".+\.beam\$", false,
			  fun(File, Acc) ->
					  [File|Acc]
			  end, []));
		  false ->
		      [Path]
	      end
      end, Paths).

is_elixir_beam(File) ->
    case filename:basename(File) of
	"Elixir" ++ _ -> true;
	_ -> false
    end.

get_code_from_beam(File) ->
    case beam_lib:chunks(File, [abstract_code, exports]) of
	{ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}, {exports, X}]}} ->
	    {Forms, X};
	_ ->
	    err("No abstract code found in ~s~n", [File])
    end.

log(Format, Args) ->
    io:format(standard_io, Format, Args).

err(Format, Args) ->
    io:format(standard_error, Format, Args),
    halt(1).

write(Fd, Format, Args) ->
    file:write(Fd, io_lib:format(Format, Args)).

maps_append(K, V, M) ->
    maps_append_list(K, [V], M).

maps_append_list(K, L1, M) ->
    L2 = maps:get(K, M, []),
    maps:put(K, L2 ++ L1, M).
