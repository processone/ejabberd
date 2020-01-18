-module(override_deps_versions2).
-export([preprocess/2, 'pre_update-deps'/2, new_replace/1, new_replace/0]).

preprocess(Config, _Dirs) ->
    update_deps(Config).

update_deps(Config) ->
    LocalDeps = rebar_config:get_local(Config, deps, []),
    TopDeps = case rebar_config:get_xconf(Config, top_deps, []) of
		  [] -> LocalDeps;
		  Val -> Val
	      end,
    Config2 = rebar_config:set_xconf(Config, top_deps, TopDeps),
    NewDeps = lists:map(fun({Name, _, _} = Dep) ->
				case lists:keyfind(Name, 1, TopDeps) of
				    false -> Dep;
				    TopDep -> TopDep
				end
			end, LocalDeps),
    %io:format("LD ~p~n", [LocalDeps]),
    %io:format("TD ~p~n", [TopDeps]),

    Config3 = rebar_config:set(Config2, deps, NewDeps),
    {ok, Config3, []}.


'pre_update-deps'(Config, _Dirs) ->
    {ok, Config2, _} = update_deps(Config),

    case code:is_loaded(old_rebar_config) of
	false ->
	    {_, Beam, _} = code:get_object_code(rebar_config),
	    NBeam = rename(Beam, old_rebar_config),
	    code:load_binary(old_rebar_config, "blank", NBeam),
	    replace_mod(Beam);
	_ ->
	    ok
    end,
    {ok, Config2}.

new_replace() ->
    old_rebar_config:new().
new_replace(Config) ->
    NC = old_rebar_config:new(Config),
    {ok, Conf, _} = update_deps(NC),
    Conf.

replace_mod(Beam) ->
    {ok, {_, [{exports, Exports}]}} = beam_lib:chunks(Beam, [exports]),
    Funcs = lists:filtermap(
	      fun({module_info, _}) ->
		      false;
		 ({Name, Arity}) ->
		      Args = args(Arity),
		      Call = case Name of
				 new ->
				     [erl_syntax:application(
					erl_syntax:abstract(override_deps_versions2),
					erl_syntax:abstract(new_replace),
					Args)];
				 _ ->
				     [erl_syntax:application(
					erl_syntax:abstract(old_rebar_config),
					erl_syntax:abstract(Name),
					Args)]
			     end,
		      {true, erl_syntax:function(erl_syntax:abstract(Name),
						 [erl_syntax:clause(Args, none,
								    Call)])}
	      end, Exports),
    Forms0 = ([erl_syntax:attribute(erl_syntax:abstract(module),
				    [erl_syntax:abstract(rebar_config)])]
	      ++ Funcs),
    Forms = [erl_syntax:revert(Form) || Form <- Forms0],
    %io:format("--------------------------------------------------~n"
	%      "~s~n",
	 %     [[erl_pp:form(Form) || Form <- Forms]]),
    {ok, Mod, Bin} = compile:forms(Forms, [report, export_all]),
    code:purge(rebar_config),
    {module, Mod} = code:load_binary(rebar_config, "mock", Bin).


args(0) ->
    [];
args(N) ->
    [arg(N) | args(N-1)].

arg(N) ->
    erl_syntax:variable(list_to_atom("A"++integer_to_list(N))).

rename(BeamBin0, Name) ->
    BeamBin = replace_in_atab(BeamBin0, Name),
    update_form_size(BeamBin).

%% Replace the first atom of the atom table with the new name
replace_in_atab(<<"Atom", CnkSz0:32, Cnk:CnkSz0/binary, Rest/binary>>, Name) ->
    replace_first_atom(<<"Atom">>, Cnk, CnkSz0, Rest, latin1, Name);
replace_in_atab(<<"AtU8", CnkSz0:32, Cnk:CnkSz0/binary, Rest/binary>>, Name) ->
    replace_first_atom(<<"AtU8">>, Cnk, CnkSz0, Rest, unicode, Name);
replace_in_atab(<<C, Rest/binary>>, Name) ->
    <<C, (replace_in_atab(Rest, Name))/binary>>.

replace_first_atom(CnkName, Cnk, CnkSz0, Rest, Encoding, Name) ->
    <<NumAtoms:32, NameSz0:8, _Name0:NameSz0/binary, CnkRest/binary>> = Cnk,
    NumPad0 = num_pad_bytes(CnkSz0),
    <<_:NumPad0/unit:8, NextCnks/binary>> = Rest,
    NameBin = atom_to_binary(Name, Encoding),
    NameSz = byte_size(NameBin),
    CnkSz = CnkSz0 + NameSz - NameSz0,
    NumPad = num_pad_bytes(CnkSz),
    <<CnkName/binary, CnkSz:32, NumAtoms:32, NameSz:8, NameBin:NameSz/binary,
      CnkRest/binary, 0:NumPad/unit:8, NextCnks/binary>>.


%% Calculate the number of padding bytes that have to be added for the
%% BinSize to be an even multiple of ?beam_num_bytes_alignment.
num_pad_bytes(BinSize) ->
    case 4 - (BinSize rem 4) of
	4 -> 0;
	N -> N
    end.

%% Update the size within the top-level form
update_form_size(<<"FOR1", _OldSz:32, Rest/binary>> = Bin) ->
    Sz = size(Bin) - 8,
<<"FOR1", Sz:32, Rest/binary>>.
