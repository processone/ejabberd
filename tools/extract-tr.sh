#!/usr/bin/env escript
%% -*- erlang -*-

main(Paths) ->
    Dict = fold_erls(
	     fun(File, Tokens, Acc) ->
		     File1 = filename:rootname(filename:basename(File)),
		     extract_tr(File1, Tokens, Acc)
	     end, dict:new(), Paths),
    generate_pot(Dict).

extract_tr(File, [{'?', _}, {var, _, 'T'}, {'(', Line}|Tokens], Acc) ->
    case extract_string(Tokens, "") of
	{"", Tokens1} ->
	    err("~s:~B: Warning: invalid string", [File, Line]),
	    extract_tr(File, Tokens1, Acc);
	{String, Tokens1} ->
	    extract_tr(File, Tokens1, dict:append(String, {File, Line}, Acc))
    end;
extract_tr(_File, [{atom,_,module}, {'(',_}, {atom,_,ejabberd_doc} | _Tokens], Acc) ->
    Acc;
extract_tr(File, [{atom, _, F}, {'(',_} | Tokens], Acc)
    when (F == mod_doc); (F == doc) ->
    Tokens2 = consume_tokens_until_dot(Tokens),
    extract_tr(File, Tokens2, Acc);
extract_tr(File, [_|Tokens], Acc) ->
    %%err("~p~n", [A]),
    extract_tr(File, Tokens, Acc);
extract_tr(_, [], Acc) ->
    Acc.

consume_tokens_until_dot([{dot, _} | Tokens]) ->
    Tokens;
consume_tokens_until_dot([_ | Tokens]) ->
    consume_tokens_until_dot(Tokens).

extract_string([{string, _, S}|Tokens], Acc) ->
    extract_string(Tokens, [S|Acc]);
extract_string([{')', _}|Tokens], Acc) ->
    {lists:flatten(lists:reverse(Acc)), Tokens};
extract_string(Tokens, _) ->
    {"", Tokens}.

fold_erls(Fun, State, Paths) ->
    Paths1 = fold_paths(Paths),
    Total = length(Paths1),
    {_, State1} =
        lists:foldl(
          fun(File, {I, Acc}) ->
                  io:format(standard_error,
			    "Progress: ~B% (~B/~B)\r",
                            [round(I*100/Total), I, Total]),
		  case tokens(File) of
		      {ok, Tokens} ->
			  {I+1, Fun(File, Tokens, Acc)};
		      error ->
			  {I+1, Acc}
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
			  Path, ".+\.erl\$", false,
			  fun(File, Acc) ->
				  [File|Acc]
			  end, []));
                  false ->
                      [Path]
              end
      end, Paths).

tokens(File) ->
    case file:read_file(File) of
	{ok, Data} ->
	    case erl_scan:string(binary_to_list(Data)) of
		{ok, Tokens, _} ->
		    {ok, Tokens};
		{error, {_, Module, Desc}, Line} ->
		    err("~s:~n: Warning: scan error: ~s",
			[filename:basename(File), Line, Module:format_error(Desc)]),
		    error
	    end;
	{error, Why} ->
	    err("Warning: failed to read file ~s: ~s",
		[File, file:format_error(Why)]),
	    error
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
			 io_lib:format("~s.erl:~B", [File, Pos])
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
       "\"Content-Transfer-Encoding: 8bit\\n\"",
       "\"X-Poedit-Basepath: ../../src\\n\"",
       "\"X-Poedit-SearchPath-0: .\\n\""],
      io_lib:nl()).

err(Format, Args) ->
    io:format(standard_error, Format ++ io_lib:nl(), Args).
