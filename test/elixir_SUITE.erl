%%%-------------------------------------------------------------------
%%% @author Mickael Remond <mremond@process-one.net>
%%% @copyright (C) 2002-2015, ProcessOne
%%% @doc
%%% This is a common test wrapper to run our ejabberd tests written in
%%% Elixir from standard common test code.
%%%
%%% Example: Is run with:
%%% ./rebar skip_deps=true ct suites=test/elixir_SUITE
%%% @end
%%% Created :  19 Feb 2015 by Mickael Remond <mremond@process-one.net>
%%%-------------------------------------------------------------------

-module(elixir_SUITE).

-compile(export_all).

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

is_elixir_available() ->
    case catch elixir:module_info() of
        {'EXIT',{undef,_}} ->
            false;
        ModInfo when is_list(ModInfo) ->
            true
    end.

undefined_function(?MODULE, Func, Args) ->
    case lists:suffix(".exs", atom_to_list(Func)) of
        true ->            
            'Elixir.ExUnit':start([]),
            'Elixir.Code':load_file(list_to_binary(filename:join(test_dir(), atom_to_list(Func)))),
            'Elixir.ExUnit':run();
        false ->
            error_handler:undefined_function(?MODULE, Func, Args)
    end;
undefined_function(Module, Func, Args) ->
    error_handler:undefined_function(Module, Func,Args).

test_dir() ->
    {ok, CWD} = file:get_cwd(),
    filename:join(CWD, "../../test").
