%%%-------------------------------------------------------------------
%%% @author Mickael Remond <mremond@process-one.net>
%%% @copyright (C) 2002-2016, ProcessOne
%%% @doc
%%% This is a common test wrapper to run our ejabberd tests written in
%%% Elixir from standard common test code.
%%%
%%% Example: Is run with:
%%%     ./rebar skip_deps=true ct suites=elixir
%%% or from ejabber overall test suite:
%%%     make quicktest
%%% @end
%%% Created :  19 Feb 2015 by Mickael Remond <mremond@process-one.net>
%%%-------------------------------------------------------------------

-module(elixir_SUITE).

-compile(export_all).

init_per_suite(Config) ->
    check_meck(),
    Config.

init_per_testcase(_TestCase, Config) ->
    process_flag(error_handler, ?MODULE),
    Config.

all() ->
    case is_elixir_available() of
        true ->
            Dir = test_dir(),
            filelib:fold_files(Dir, ".*\.exs", false,
                               fun(Filename, Acc) -> [list_to_atom(filename:basename(Filename)) | Acc] end,
                               []);
        false ->
            []
    end.

check_meck() ->
    case catch meck:module_info(module) of
        meck ->
            ok;
        {'EXIT',{undef, _}} ->
            ct:print("meck is not available. Please make sure you configured ejabberd with --enable-elixir --enable-tools"),
            ok
    end.

is_elixir_available() ->
    case catch elixir:module_info() of
        {'EXIT',{undef,_}} ->
            ct:print("ejabberd has not been build with Elixir support, skipping Elixir tests."),
            false;
        ModInfo when is_list(ModInfo) ->
            true
    end.

undefined_function(?MODULE, Func, Args) ->
    case lists:suffix(".exs", atom_to_list(Func)) of
        true ->
            run_elixir_test(Func);
        false ->
            error_handler:undefined_function(?MODULE, Func, Args)
    end;
undefined_function(Module, Func, Args) ->
    error_handler:undefined_function(Module, Func,Args).

run_elixir_test(Func) ->
    'Elixir.ExUnit':start([]),
    'Elixir.Code':load_file(list_to_binary(filename:join(test_dir(), atom_to_list(Func)))),
    %% I did not use map syntax, so that this file can still be build under R16
    ResultMap = 'Elixir.ExUnit':run(),
    case maps:find(failures, ResultMap) of
        {ok, 0} ->
            %% Zero failures
            ok;
        {ok, Failures} ->
            ct:print("Elixir tests failed: ~.10B~nSee logs for details", [Failures]),
            ct:fail(elixir_test_failure)
    end.

test_dir() ->
    {ok, CWD} = file:get_cwd(),
    filename:join(CWD, "../../test").
