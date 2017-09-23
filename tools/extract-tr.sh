#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

main([Dir]) ->
    Txts =
	filelib:fold_files(
	  Dir, ".+\.beam\$", false,
	  fun(FileIn, Res) ->
		  case get_forms(FileIn) of
		      {ok, Forms} ->
			  Tree = erl_syntax:form_list(Forms),
			  Mod = mod(FileIn),
			  erl_syntax_lib:fold_subtrees(
			    fun(Form, Acc) ->
				    case erl_syntax:type(Form) of
					function ->
					    case map(Form, Mod) of
						[] ->
						    Acc;
						Vars ->
						    Vars ++ Acc
					    end;
					_ ->
					    Acc
				    end
			    end, [], Tree) ++ Res;
		      _Err ->
			  Res
		  end
	  end, []),
    Dict = lists:foldl(
	     fun({B, Meta}, Acc) ->
		     dict:update(
		       binary_to_list(B),
		       fun(OldMeta) ->
			       lists:usort([Meta|OldMeta])
		       end,
		       [Meta], Acc)
	     end, dict:new(), Txts),
    generate_pot(Dict).

map(Tree, Mod) ->
    Vars = erl_syntax_lib:fold(
	     fun(Form, Acc) ->
		     case erl_syntax:type(Form) of
			 application ->
			     analyze_app(Form, Mod) ++ Acc;
			 _ ->
			     Acc
		     end
	     end, [], Tree),
    Bins = lists:flatmap(
	     fun({Var, Pos}) when is_atom(Var) ->
		     Res = erl_syntax_lib:fold(
			     fun(Form, Acc) ->
				     case process_match_expr(
					    Form, Var, Mod) of
					 {ok, Binary, NewPos} ->
					     [{Binary, NewPos}|Acc];
					 error ->
					     Acc
				     end
			     end, [], Tree),
		     case Res of
			 [] ->
			     log("~s:~p: unresolved variable: ~s~n",
				 [Mod, Pos, Var]);
			 _ ->
			     ok
		     end,
		     Res;
		({Var, Pos}) when is_binary(Var) ->
		     [{Var, Pos}]
	     end, lists:usort(Vars)),
    [{B, {Mod, Pos}} || {B, Pos} <- Bins, B /= <<"">>].

process_match_expr(Form, Var, Mod) ->
    case erl_syntax:type(Form) of
	match_expr ->
	    Pattern = erl_syntax:match_expr_pattern(Form),
	    Body = erl_syntax:match_expr_body(Form),
	    {V, Expr} =
		case {erl_syntax:type(Pattern), erl_syntax:type(Body)} of
		    {variable, _} ->
			{erl_syntax:variable_name(Pattern), Body};
		    {_, variable} ->
			{erl_syntax:variable_name(Body), Pattern};
		    _ ->
			{'', none}
		end,
	    Text = maybe_extract_tuple(Expr),
	    if V == Var ->
		    Pos = erl_syntax:get_pos(Text),
		    try {ok, erl_syntax:concrete(Text), Pos}
		    catch _:_ ->
			    case catch erl_syntax_lib:analyze_application(Text) of
				{_M, {Fn, 1}} when Fn == format_error;
						   Fn == io_format_error ->
				    error;
				_ ->
				    log("~s:~p: not a binary: ~s~n",
					[Mod, Pos, erl_prettypr:format(Text)]),
				    {ok, <<>>, Pos}
			    end
		    end;
	       true ->
		    error
	    end;
	_ ->
	    error
    end.

maybe_extract_tuple(none) ->
    none;
maybe_extract_tuple(Form) ->
    try
	tuple = erl_syntax:type(Form),
	[Text, _] = erl_syntax:tuple_elements(Form),
	Text
    catch _:{badmatch, _} ->
	    Form
    end.

analyze_app(Form, Mod) ->
    try
	{M, {F, A}} = erl_syntax_lib:analyze_application(Form),
	Args = erl_syntax:application_arguments(Form),
	Txt = case {M, atom_to_list(F), A, Args} of
		  {xmpp, "err_" ++ _, 2, [T|_]} -> T;
		  {xmpp, "serr_" ++ _, 2, [T|_]} -> T;
		  {xmpp, "mk_text", 2, [T|_]} -> T;
		  {translate, "translate", 2, [_,T|_]} -> T;
		  {translate, "mark", 1, [T]} -> T
	      end,
	Pos = erl_syntax:get_pos(Txt),
	case erl_syntax:type(Txt) of
	    binary ->
		try [{erl_syntax:concrete(Txt), Pos}]
		catch _:_ ->
			Pos = erl_syntax:get_pos(Txt),
			log("~s:~p: not a binary: ~s~n",
			    [Mod, Pos, erl_prettypr:format(Txt)]),
			[]
		end;
	    variable ->
		[{erl_syntax:variable_name(Txt), Pos}];
	    application ->
		Vars = sets:to_list(erl_syntax_lib:variables(Txt)),
		case Vars of
		    [Var] ->
			[{Var, Pos}];
		    [_|_] ->
			log("Too many variables: ~p~n", [Vars]),
			[];
		    [] ->
			[]
		end;
	    _ ->
		[]
	end
    catch _:{badmatch, _} ->
	    [];
	  _:{case_clause, _} ->
	    []
    end.

generate_pot(Dict) ->
    io:format("~s~n~n", [pot_header()]),
    lists:foreach(
      fun({Msg, Location}) ->
	      S1 = format_location(Location),
	      S2 = format_msg(Msg),
	      io:format("~smsgstr \"\"~n~n", [S1 ++ S2])
      end, lists:keysort(1, dict:to_list(Dict))).

format_location([A, B, C|T]) ->
    format_location_list([A,B,C]) ++ format_location(T);
format_location([A, B|T]) ->
    format_location_list([A,B]) ++ format_location(T);
format_location([A|T]) ->
    format_location_list([A]) ++ format_location(T);
format_location([]) ->
    "".

format_location_list(L) ->
    "#: " ++ string:join(
	       lists:map(
		 fun({File, Pos}) ->
			 io_lib:format("~s:~B", [File, Pos])
		 end, L),
	       " ") ++ io_lib:nl().

format_msg(Bin) ->
    io_lib:format("msgid \"~s\"~n", [escape(Bin)]).

escape(Bin) ->
    lists:map(
      fun($") -> "\\\"";
	 (C) -> C
      end, binary_to_list(iolist_to_binary(Bin))).

pot_header() ->
    string:join(
      ["msgid \"\"",
       "msgstr \"\"",
       "\"Project-Id-Version: 15.11.127\\n\"",
       "\"X-Language: Language Name\\n\"",
       "\"Last-Translator: Translator name and contact method\\n\"",
       "\"MIME-Version: 1.0\\n\"",
       "\"Content-Type: text/plain; charset=UTF-8\\n\"",
       "\"Content-Transfer-Encoding: 8bit\\n\""],
      io_lib:nl()).

mod(Path) ->
    filename:rootname(filename:basename(Path)) ++ ".erl".

log(Format, Args) ->
    io:format(standard_error, Format, Args).

get_forms(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {_, List}} ->
            case lists:keyfind(abstract_code, 1, List) of
                {abstract_code, {raw_abstract_v1, Abstr}} ->
                    {ok, Abstr};
                _ ->
                    error
            end;
        _ ->
            error
    end.
