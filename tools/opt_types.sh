#!/usr/bin/env escript
%% -*- erlang -*-

-compile([nowarn_unused_function]).
-record(state, {g_opts = #{} :: map(),
		m_opts = #{} :: map(),
		globals = [] :: [atom()],
		defaults = #{} :: map(),
		mod_defaults = #{} :: map(),
		specs = #{} :: map(),
		mod_specs = #{} :: map()}).

main([Mod|Paths]) ->
    State = fold_beams(
	      fun(File, Form, StateAcc) ->
		      append(Form, File, StateAcc)
	      end, #state{}, Paths),
    emit_modules(map_to_specs(State#state.m_opts,
			      State#state.mod_defaults,
			      State#state.mod_specs)),
    emit_config(Mod,
		map_to_specs(State#state.g_opts,
			     State#state.defaults,
			     State#state.specs),
		State#state.globals).

emit_config(Mod, Specs, Globals) ->
    File = filename:join("src", Mod ++ ".erl"),
    case file:open(File, [write]) of
	{ok, Fd} ->
	    emit_header(Fd, Mod, Specs, Globals),
	    emit_funs(Fd, Mod, Specs, Globals);
	{error, Reason} ->
	    err("Failed to open file ~s for writing: ~s",
		[File, file:format_error(Reason)])
    end.

emit_modules(Specs) ->
    M = lists:foldl(
	  fun({{Mod, Opt}, Spec}, Acc) ->
		  Opts = maps:get(Mod, Acc, []),
		  Opts1 = [{Opt, Spec}|Opts],
		  maps:put(Mod, Opts1, Acc)
	  end, #{}, Specs),
    maps:fold(
      fun(Mod, OptSpecs, _) ->
	      ModS = atom_to_list(Mod) ++ "_opt",
	      File = filename:join("src", ModS ++ ".erl"),
	      case file:open(File, [write]) of
		  {ok, Fd} ->
		      OptSpecs1 = lists:reverse(OptSpecs),
		      emit_header(Fd, ModS, OptSpecs1),
		      emit_funs(Fd, Mod, OptSpecs1);
		  {error, Reason} ->
		      err("Failed to open file ~s for writing: ~s",
			  [File, file:format_error(Reason)])
	      end
      end, ok, M).

emit_header(Fd, Mod, Specs, Globals) ->
    log(Fd, comment(), []),
    log(Fd, "-module(~s).~n", [Mod]),
    lists:foreach(
      fun({{_, Opt}, _}) ->
	      case lists:member(Opt, Globals) of
		  true ->
		      log(Fd, "-export([~s/0]).", [Opt]);
		  false ->
		      log(Fd, "-export([~s/0, ~s/1]).", [Opt, Opt])
	      end
      end, Specs),
    log(Fd, "", []).

emit_header(Fd, Mod, Specs) ->
    log(Fd, comment(), []),
    log(Fd, "-module(~s).~n", [Mod]),
    lists:foreach(
      fun({Opt, _}) ->
	      log(Fd, "-export([~s/1]).", [Opt])
      end, Specs),
    log(Fd, "", []).

emit_funs(Fd, _Mod, Specs, Globals) ->
    lists:foreach(
      fun({{_, Opt}, Type}) ->
	      SType = t_to_string(Type),
	      case lists:member(Opt, Globals) of
		  true ->
		      log(Fd,
			  "-spec ~s() -> ~s.~n"
			  "~s() ->~n"
			  "    ejabberd_config:get_option({~s, global}).~n",
			  [Opt, SType, Opt, Opt]);
		  false ->
		      log(Fd,
			  "-spec ~s() -> ~s.~n"
			  "~s() ->~n"
			  "    ~s(global).~n"
			  "-spec ~s(global | binary()) -> ~s.~n"
			  "~s(Host) ->~n"
			  "    ejabberd_config:get_option({~s, Host}).~n",
			  [Opt, SType, Opt, Opt, Opt, SType, Opt, Opt])
	      end
      end, Specs).

emit_funs(Fd, Mod, Specs) ->
    lists:foreach(
      fun({Opt, Type}) ->
              Mod2 = strip_db_type(Mod),
	      log(Fd,
		  "-spec ~s(gen_mod:opts() | global | binary()) -> ~s.~n"
		  "~s(Opts) when is_map(Opts) ->~n"
		  "    gen_mod:get_opt(~s, Opts);~n"
		  "~s(Host) ->~n"
		  "    gen_mod:get_module_opt(Host, ~s, ~s).~n",
		  [Opt, t_to_string(Type), Opt, Opt, Opt, Mod2, Opt])
      end, Specs).

strip_db_type(mod_vcard_ldap) ->
    mod_vcard;
strip_db_type(mod_vcard_mnesia) ->
    mod_vcard;
strip_db_type(Mod) ->
    Mod.

append({globals, Form}, _File, State) ->
    [Clause] = erl_syntax:function_clauses(Form),
    Body = lists:last(erl_syntax:clause_body(Clause)),
    Gs = lists:map(fun erl_syntax:atom_value/1,
		   erl_syntax:list_elements(Body)),
    Globals = State#state.globals ++ Gs,
    State#state{globals = Globals};
append({Index, Form}, File, State) when Index == #state.defaults;
					Index == #state.mod_defaults ->
    Mod = module(File),
    [Clause] = erl_syntax:function_clauses(Form),
    Body = lists:last(erl_syntax:clause_body(Clause)),
    case erl_syntax:is_proper_list(Body) of
	true ->
	    Opts = lists:foldl(
		     fun(E, M) ->
			     try
				 [E1, E2|_] = erl_syntax:tuple_elements(E),
				 Name = erl_syntax:atom_value(E1),
				 Val = erl_syntax:concrete(E2),
				 maps:put({Mod, Name}, Val, M)
			     catch _:_ ->
				     M
			     end
		     end, element(Index, State), erl_syntax:list_elements(Body)),
	    setelement(Index, State, Opts);
	false ->
	    warn("~s: improper list", [format_file(File, Body)]),
	    State
    end;
append({Index, Form}, File, State) when Index == #state.specs;
					Index == #state.mod_specs ->
    Specs = element(Index, State),
    Mod = module(File),
    try
	{type, _, 'fun', Form1} = Form,
	{type, _, list, Form2} = lists:last(Form1),
	Tuples = case Form2 of
		     [{type, _, union, Form3}] -> Form3;
		     _ -> Form2
		 end,
	Specs1 = lists:foldl(
		   fun({type, _, tuple, [{atom, _, Atom}, Form5]}, Acc) ->
			   maps:put({Mod, Atom}, Form5, Acc);
		      (_, Acc) ->
			   Acc
		   end, Specs, Tuples),
	setelement(Index, State, Specs1)
    catch _:_ ->
	    warn("~s: unsupported type spec", [format_file(File, Form)]),
	    State
    end;
append({Type, Form}, File, State) when Type == opt_type; Type == mod_opt_type ->
    Clauses = erl_syntax:function_clauses(Form),
    Mod = module(File),
    lists:foldl(
      fun(Clause, StateAcc) ->
              [Arg] = erl_syntax:clause_patterns(Clause),
	      Body = lists:last(erl_syntax:clause_body(Clause)),
              case erl_syntax:type(Arg) of
                  atom ->
                      Name = erl_syntax:atom_value(Arg),
		      case Type of
			  opt_type ->
			      GOpts = StateAcc#state.g_opts,
			      State#state{
				g_opts = append_body({Mod, Name}, Body, GOpts)};
			  mod_opt_type ->
			      MOpts = StateAcc#state.m_opts,
			      State#state{
				m_opts = append_body({Mod, Name}, Body, MOpts)}
		      end;
		  T ->
		      warn("~s: unexpected option name: ~s",
			   [format_file(File, Arg), T]),
		      StateAcc
              end
      end, State, Clauses).

append_body(Name, Body, Map) ->
    maps:put(Name, Body, Map).

map_to_specs(Map, Defaults, Specs) ->
    lists:keysort(
      1, maps:fold(
	   fun({Mod, Opt} = Key, Val, Acc) ->
		   S1 = type_with_default(Key, Val, Defaults),
		   S2 = case t_is_any(S1) of
			    true ->
				try maps:get(Key, Specs)
				catch _:{badkey, _} ->
					warn("Cannot derive type for ~s->~s", [Mod, Opt]),
					S1
				end;
			    false ->
				S1
			end,
		   [{Key, S2}|Acc]
	   end, [], Map)).

type_with_default({Mod, _} = Key, Val, Defaults) ->
    S = try spec(Mod, Val)
	catch throw:unknown -> erl_types:t_any()
	end,
    case t_is_any(S) of
	true ->
	    S;
	false ->
	    try maps:get(Key, Defaults) of
		T ->
		    erl_types:t_sup(
		      [S, erl_types:t_from_term(T)])
	    catch _:{badkey, _} ->
		    S
	    end
    end.

spec(Mod, Form) ->
    case erl_syntax:type(Form) of
	application ->
	    case erl_syntax_lib:analyze_application(Form) of
		{M, {Fun, Arity}} when M == econf;
				       M == yconf ->
		    Args = erl_syntax:application_arguments(Form),
		    spec(Fun, Arity, Args, Mod);
		_ ->
		    t_unknown(Mod)
	    end;
	_ ->
	    t_unknown(Mod)
    end.

spec(pos_int, 0, _, _) ->
    erl_types:t_pos_integer();
spec(pos_int, 1, [Inf], _) ->
    erl_types:t_sup(
      erl_types:t_pos_integer(),
      erl_types:t_atom(erl_syntax:atom_value(Inf)));
spec(non_neg_int, 0, _, _) ->
    erl_types:t_non_neg_integer();
spec(non_neg_int, 1, [Inf], _) ->
    erl_types:t_sup(
      erl_types:t_non_neg_integer(),
      erl_types:t_atom(erl_syntax:atom_value(Inf)));
spec(int, 0, _, _) ->
    erl_types:t_integer();
spec(int, 2, [Min, Max], _) ->
    erl_types:t_from_range(
      erl_syntax:integer_value(Min),
      erl_syntax:integer_value(Max));
spec(number, 1, _, _) ->
    erl_types:t_number();
spec(octal, 0, _, _) ->
    erl_types:t_non_neg_integer();
spec(binary, A, _, _) when A == 0; A == 1; A == 2 ->
    erl_types:t_binary();
spec(enum, 1, [L], _) ->
    try
	Els = erl_syntax:list_elements(L),
	Atoms = lists:map(
		  fun(A) ->
			  erl_types:t_atom(
			    erl_syntax:atom_value(A))
		  end, Els),
	erl_types:t_sup(Atoms)
    catch _:_ ->
	    erl_types:t_binary()
    end;
spec(bool, 0, _, _) ->
    erl_types:t_boolean();
spec(atom, 0, _, _) ->
    erl_types:t_atom();
spec(string, A, _, _) when A == 0; A == 1; A == 2 ->
    erl_types:t_string();
spec(any, 0, _, Mod) ->
    t_unknown(Mod);
spec(url, A, _, _) when A == 0; A == 1 ->
    erl_types:t_binary();
spec(file, A, _, _) when A == 0; A == 1 ->
    erl_types:t_binary();
spec(directory, A, _, _) when A == 0; A == 1 ->
    erl_types:t_binary();
spec(ip, 0, _, _) ->
    t_remote(inet, ip_address);
spec(ipv4, 0, _, _) ->
    t_remote(inet, ip4_address);
spec(ipv6, 0, _, _) ->
    t_remote(inet, ip6_address);
spec(ip_mask, 0, _, _) ->
    erl_types:t_sup(
      erl_types:t_tuple(
	[t_remote(inet, ip4_address), erl_types:t_from_range(0, 32)]),
      erl_types:t_tuple(
	[t_remote(inet, ip6_address), erl_types:t_from_range(0, 128)]));
spec(port, 0, _, _) ->
    erl_types:t_from_range(1, 65535);
spec(re, A, _, _) when A == 0; A == 1 ->
    t_remote(misc, re_mp);
spec(glob, A, _, _) when A == 0; A == 1 ->
    t_remote(misc, re_mp);
spec(path, 0, _, _) ->
    erl_types:t_binary();
spec(binary_sep, 1, _, _) ->
    erl_types:t_list(erl_types:t_binary());
spec(beam, A, _, _) when A == 0; A == 1 ->
    erl_types:t_module();
spec(timeout, 1, _, _) ->
    erl_types:t_pos_integer();
spec(timeout, 2, [_, Inf], _) ->
    erl_types:t_sup(
      erl_types:t_pos_integer(),
      erl_types:t_atom(erl_syntax:atom_value(Inf)));
spec(non_empty, 1, [Form], Mod) ->
    S = spec(Mod, Form),
    case erl_types:t_is_list(S) of
	true ->
	    erl_types:t_nonempty_list(
	      erl_types:t_list_elements(S));
	false ->
	    S
    end;
spec(unique, 1, [Form], Mod) ->
    spec(Mod, Form);
spec(acl, 0, _, _) ->
    t_remote(acl, acl);
spec(shaper, 0, _, _) ->
    erl_types:t_sup(
      [erl_types:t_atom(),
       erl_types:t_list(t_remote(ejabberd_shaper, shaper_rule))]);
spec(url_or_file, 0, _, _) ->
    erl_types:t_tuple(
      [erl_types:t_sup([erl_types:t_atom(file),
			erl_types:t_atom(url)]),
       erl_types:t_binary()]);
spec(lang, 0, _, _) ->
    erl_types:t_binary();
spec(pem, 0, _, _) ->
    erl_types:t_binary();
spec(jid, 0, _, _) ->
    t_remote(jid, jid);
spec(domain, 0, _, _) ->
    erl_types:t_binary();
spec(db_type, 1, _, _) ->
    erl_types:t_atom();
spec(queue_type, 0, _, _) ->
    erl_types:t_sup([erl_types:t_atom(ram),
		     erl_types:t_atom(file)]);
spec(ldap_filter, 0, _, _) ->
    erl_types:t_binary();
spec(sip_uri, 0, _, _) ->
    t_remote(esip, uri);
spec(Fun, A, [Form|_], Mod) when (A == 1 orelse A == 2) andalso
				 (Fun == list orelse Fun == list_or_single) ->
    erl_types:t_list(spec(Mod, Form));
spec(map, A, [F1, F2|OForm], Mod) when A == 2; A == 3 ->
    T1 = spec(Mod, F1),
    T2 = spec(Mod, F2),
    case options_return_type(OForm) of
	map ->
	    erl_types:t_map([], T1, T2);
	dict ->
	    t_remote(dict, dict);
	_ ->
	    erl_types:t_list(erl_types:t_tuple([T1, T2]))
    end;
spec(either, 2, [F1, F2], Mod) ->
    Spec1 = case erl_syntax:type(F1) of
		atom -> erl_types:t_atom(erl_syntax:atom_value(F1));
		_ -> spec(Mod, F1)
	    end,
    Spec2 = spec(Mod, F2),
    erl_types:t_sup([Spec1, Spec2]);
spec(and_then, 2, [_, F], Mod) ->
    spec(Mod, F);
spec(host, 0, _, _) ->
    erl_types:t_binary();
spec(hosts, 0, _, _) ->
    erl_types:t_list(erl_types:t_binary());
spec(vcard_temp, 0, _, _) ->
    erl_types:t_sup([erl_types:t_atom(undefined),
		     erl_types:t_tuple()]);
spec(options, A, [Form|OForm], Mod) when A == 1; A == 2 ->
    case erl_syntax:type(Form) of
	map_expr ->
	    Fs = erl_syntax:map_expr_fields(Form),
	    Required = options_required(OForm),
	    {Els, {DefK, DefV}} =
		lists:mapfoldl(
		  fun(F, Acc) ->
			  Name = erl_syntax:map_field_assoc_name(F),
			  Val = erl_syntax:map_field_assoc_value(F),
			  OptType = spec(Mod, Val),
			  case erl_syntax:atom_value(Name) of
			      '_' ->
				  {[], {erl_types:t_atom(), OptType}};
			      Atom ->
				  Mand = case lists:member(Atom, Required) of
					     true -> mandatory;
					     false -> optional
					 end,
				  {[{erl_types:t_atom(Atom), Mand, OptType}], Acc}
			  end
		  end, {erl_types:t_none(), erl_types:t_none()}, Fs),
	    case options_return_type(OForm) of
		map ->
		    erl_types:t_map(lists:keysort(1, lists:flatten(Els)), DefK, DefV);
		dict ->
		    t_remote(dict, dict);
		_ ->
		    erl_types:t_list(
		      erl_types:t_sup(
			[erl_types:t_tuple([DefK, DefV])|
			 lists:map(
			   fun({K, _, V}) ->
				   erl_types:t_tuple([K, V])
			   end, lists:flatten(Els))]))
	    end;
	_ ->
	    t_unknown(Mod)
    end;
spec(_, _, _, Mod) ->
    t_unknown(Mod).

t_from_form(Spec) ->
    {T, _} = erl_types:t_from_form(
               Spec, sets:new(), {type, {mod, foo, 1}}, dict:new(),
               erl_types:var_table__new(), erl_types:cache__new()),
    T.

t_remote(Mod, Type) ->
    D = maps:from_list([{{opaque, Type, []},
                         {{Mod, 1, 2, []}, type}}]),
    [T] = erl_types:t_opaque_from_records(D),
    T.

t_unknown(_Mod) ->
    throw(unknown).

t_is_any(T) ->
    T == erl_types:t_any().

t_to_string(T) ->
    case erl_types:is_erl_type(T) of
	true -> erl_types:t_to_string(T);
	false -> erl_types:t_form_to_string(T)
    end.

options_return_type([]) ->
    list;
options_return_type([Form]) ->
    Opts = erl_syntax:concrete(Form),
    proplists:get_value(return, Opts, list).

options_required([]) ->
    [];
options_required([Form]) ->
    Opts = erl_syntax:concrete(Form),
    proplists:get_value(required, Opts, []).

format_file(Path, Form) ->
    Line = case erl_syntax:get_pos(Form) of
      {L, _} -> L;
      L -> L
    end,
    filename:rootname(filename:basename(Path)) ++ ".erl:" ++
	integer_to_list(Line).

module(Path) ->
    list_to_atom(filename:rootname(filename:basename(Path))).

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
			  AbsCode = get_code_from_beam(File),
			  Acc2 = case is_behaviour(AbsCode, ejabberd_config) of
				     true ->
					 fold_opt(File, Fun, Acc, AbsCode);
				     false ->
					 fold_mod_opt(File, Fun, Acc, AbsCode)
				 end,
			  {I+1, Acc2}
		  end
	  end, {0, State}, Paths1),
    State1.

fold_opt(File, Fun, Acc, AbsCode) ->
    lists:foldl(
      fun(Form, Acc1) ->
	      case erl_syntax_lib:analyze_form(Form) of
		  {function, {opt_type, 1}} ->
		      Fun(File, {opt_type, Form}, Acc1);
		  {function, {globals, 0}} ->
		      Fun(File, {globals, Form}, Acc1);
		  {function, {options, 0}} ->
		      Fun(File, {#state.defaults, Form}, Acc1);
		  {attribute, {spec, {spec, {{options, 0}, Spec}}}} ->
		      Fun(File, {#state.specs, hd(Spec)}, Acc1);
		  {attribute, {spec, {{options, 0}, Spec}}} ->
		      Fun(File, {#state.specs, hd(Spec)}, Acc1);
		  _ ->
		      Acc1
	      end
      end, Acc, AbsCode).

fold_mod_opt(File, Fun, Acc, AbsCode) ->
    lists:foldl(
      fun(Form, Acc1) ->
	      case erl_syntax_lib:analyze_form(Form) of
		  {function, {mod_opt_type, 1}} ->
		      Fun(File, {mod_opt_type, Form}, Acc1);
		  {function, {mod_options, 1}} ->
		      Fun(File, {#state.mod_defaults, Form}, Acc1);
		  {attribute, {spec, {spec, {{mod_options, 1}, Spec}}}} ->
		      Fun(File, {#state.mod_specs, hd(Spec)}, Acc1);
		  {attribute, {spec, {{mod_options, 1}, Spec}}} ->
		      Fun(File, {#state.mod_specs, hd(Spec)}, Acc1);
		  _ ->
		      Acc1
	      end
      end, Acc, AbsCode).

fold_paths(Paths) ->
    lists:flatmap(
      fun(Path) ->
	      case filelib:is_dir(Path) of
		  true ->
		      Beams = lists:reverse(
				filelib:fold_files(
				  Path, ".+\.beam\$", false,
				  fun(File, Acc) ->
					  [File|Acc]
				  end, [])),
		      case Beams of
			  [] -> ok;
			  _ -> code:add_path(Path)
		      end,
		      Beams;
		  false ->
		      [Path]
	      end
      end, Paths).

is_behaviour(AbsCode, Mod) ->
    lists:any(
      fun(Form) ->
	      case erl_syntax_lib:analyze_form(Form) of
		  {attribute, {Attr, {_, Mod}}}
		    when Attr == behaviour orelse Attr == behavior ->
		      true;
		  {attribute, {behaviour, Mod}} ->
		      true;
		  _ ->
		      false
	      end
      end, AbsCode).

is_elixir_beam(File) ->
    case filename:basename(File) of
	"Elixir" ++ _ -> true;
	_ -> false
    end.

get_code_from_beam(File) ->
    try
        {ok, {_, List}} = beam_lib:chunks(File, [abstract_code]),
        {_, {raw_abstract_v1, Forms}} = lists:keyfind(abstract_code, 1, List),
        Forms
    catch _:{badmatch, _} ->
            err("no abstract code found in ~s", [File])
    end.

comment() ->
    "%% Generated automatically~n"
    "%% DO NOT EDIT: run `make options` instead~n".

log(Format, Args) ->
    log(standard_io, Format, Args).

log(Fd, Format, Args) ->
    case io:format(Fd, Format ++ "~n", Args) of
	ok -> ok;
	{error, Reason} ->
	    err("Failed to write to file: ~s", [file:format_error(Reason)])
    end.

warn(Format, Args) ->
    io:format(standard_error, "Warning: " ++ Format ++ "~n", Args).

err(Format, Args) ->
    io:format(standard_error, "Error: " ++ Format ++ "~n", Args),
    halt(1).
