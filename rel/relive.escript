#!/usr/bin/env escript

main(Args) ->
    Target = case Args of
        ["ctl"] -> "relivectl";
        _ -> "relive"
    end,
    Base = "_build/" ++ Target,
    prepare(Base, "", none),
    prepare(Base, "conf", {os, cmd, "rel/setup-relive.sh " ++ Target}),
    prepare(Base, "database", none),
    prepare(Base, "logs", none),
    c:erlangrc([os:cmd("echo -n $HOME")]),
    ok.

prepare(BaseDir, SuffixDir, MFA) ->
    Dir = filename:join(BaseDir, SuffixDir),
    case file:make_dir(Dir) of
        ok ->
            io:format("Preparing relive dir ~s...~n", [Dir]),
            case MFA of
                none -> ok;
                {M, F, A} -> M:F(A)
            end;
        {error, eexist} ->
            ok;
        {error, LogsError} ->
            io:format("Error creating dir ~s: ~p~n", [Dir, LogsError]),
            halt(1)
    end.
