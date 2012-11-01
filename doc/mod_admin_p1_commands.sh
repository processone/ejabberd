#!/usr/bin/env escript
%% -*- erlang -*-

-record(cmd, {name, desc, longdesc, args, result}).

main(_) ->
    Dir = filename:absname(filename:join(["..", "src"])),
    FileIn = filename:join([Dir, "mod_admin_p1.erl"]),
    {ok, Forms1} = epp_dodger:parse_file(FileIn, [no_fail]),
    Comments = erl_comment_scan:file(FileIn),
    Forms = erl_recomment:recomment_forms(Forms1, Comments),
    Tree = erl_syntax:flatten_form_list(Forms),
    AuxFile = "mod_admin.tex",
    case file:open(AuxFile, [write]) of
        {ok, Fd} ->
            io:format(Fd, "\\newcommand{\\modadminsection}{\\begin{description}~n", []),
            process(Fd, Tree),
            io:format(Fd, "\\end{description}}~n", []),
            file:close(Fd),
            halt(0);
        {error, Why} ->
            io:format("failed to open file ~s: ~s",
                      [AuxFile, file:format_error(Why)]),
            halt(1)
    end.

process(Fd, Tree) ->
    case erl_syntax:type(Tree) of
        record_expr ->
            case erl_syntax_lib:analyze_record_expr(Tree) of
                {record_expr, {ejabberd_commands, _}} ->
                    Fs = erl_syntax:record_expr_fields(Tree),
                    Cmd = lists:foldl(
                            fun(F, C) ->
                                    Name = erl_syntax:record_field_name(F),
                                    Value = erl_syntax:record_field_value(F),
                                    case {erl_syntax:concrete(Name),
                                          catch erl_syntax:concrete(Value)} of
                                        {_, {'EXIT', _}} ->
                                            C;
                                        {name, V} ->
                                            C#cmd{name = V};
                                        {desc, V} ->
                                            C#cmd{desc = V};
                                        {longdesc, V} ->
                                            C#cmd{longdesc = V};
                                        {args, V} ->
                                            C#cmd{args = V};
                                        {result, V} ->
                                            C#cmd{result = V};
                                        _ ->
                                            C
                                    end
                            end, #cmd{}, Fs),
                    format_command(Fd, Cmd);
                _ ->
                    ok
            end;
        _ ->
            case erl_syntax:subtrees(Tree) of
                [] ->
                    ok;
                List ->
                    lists:foreach(
                      fun(Group) ->
                              lists:foreach(
                                fun(Subtree) ->
                                        process(Fd, Subtree)
                                end, Group)
                      end, List)
            end
    end.

-define(B(S), S).

format_command(Fd, #cmd{name = Cmd,
                        desc = Desc,
                        longdesc = _LongDesc,
                        args = ArgsDef,
                        result = _ResultDef}) ->
    io:format(Fd, "\\titem{~s ~s} ~s~n",
              [escape_underscores(atom_to_list(Cmd)),
               flatten_arguments(ArgsDef),
               escape_underscores(Desc)]).

flatten_arguments(Args) ->
    string:join(
      lists:map(
        fun({Name, _Type}) ->
                escape_underscores(io_lib:format("~s", [Name]))
        end, Args),
      " ").

escape_underscores(S) ->
    lists:flatten(
      [case C of
           $_ -> "\\_";
           _ -> C
       end || C <- S]).
