%%%----------------------------------------------------------------------
%%% File    : extract_translations.erl
%%% Author  : Sergei Golovan <sgolovan@nes.ru>
%%% Purpose : Auxiliary tool for interface/messages translators
%%% Created : 23 Apr 2005 by Sergei Golovan <sgolovan@nes.ru>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(extract_translations).
-author('sgolovan@nes.ru').

-export([start/0]).

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).

-include_lib("kernel/include/file.hrl").


start() ->
    ets:new(translations, [named_table, public]),
    ets:new(files, [named_table, public]),
    ets:new(vars, [named_table, public]),
    case init:get_plain_arguments() of
	["-unused", Dir, File] ->
	    Status = process(Dir, File, unused),
	    halt(Status);
	[Dir, File] ->
	    Status = process(Dir, File, used),
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.


process(Dir, File, Used) ->
    case load_file(File) of
	{error, Reason} ->
	    io:format("~s: ~s~n", [File, file:format_error(Reason)]),
	    ?STATUS_ERROR;
	_ ->
	    FileList = find_src_files(Dir),
	    lists:foreach(
	      fun(F) ->
		    parse_file(Dir, F, Used)
	      end, FileList),
	    case Used of
		unused ->
		    ets:foldl(fun({Key, _}, _) ->
				      io:format("~p~n", [Key])
			      end, ok, translations); 
		_ ->
		    ok
	    end,
	    ?STATUS_SUCCESS
    end.

parse_file(Dir, File, Used) ->
    ets:delete_all_objects(vars),
    case epp:parse_file(File, [Dir, filename:dirname(File)], []) of
	{ok, Forms} ->
	    lists:foreach(
	      fun(F) ->
		    parse_form(Dir, File, F, Used)
	      end, Forms);
	_ ->
	    ok
    end.

parse_form(Dir, File, Form, Used) ->
    case Form of
	{call,
	 _,
	 {remote, _, {atom, _, translate}, {atom, _, translate}},
	 [_, {string, _, Str}]
	} ->
	    process_string(Dir, File, Str, Used);
	{call,
	 _,
	 {remote, _, {atom, _, translate}, {atom, _, translate}},
	 [_, {var, _, Name}]
	} ->
	    case ets:lookup(vars, Name) of
		[{_Name, Value}] ->
		    process_string(Dir, File, Value, Used);
		_ ->
		    ok
	    end;
	{match,
	 _,
	 {var, _, Name},
	 {string, _, Value}
	} ->
	    ets:insert(vars, {Name, Value});
	L when is_list(L) ->
	    lists:foreach(
	      fun(F) ->
		    parse_form(Dir, File, F, Used)
	      end, L);
	T when is_tuple(T) ->
	    lists:foreach(
	      fun(F) ->
		    parse_form(Dir, File, F, Used)
	      end, tuple_to_list(T));
	_ ->
	    ok
    end.
	    
process_string(_Dir, File, Str, Used) ->
    case {ets:lookup(translations, Str), Used} of
	{[{_Key, _Trans}], unused} ->
	    ets:delete(translations, Str);
	{[{_Key, _Trans}], used} ->
	    ok;
	{_, used} ->
	    case ets:lookup(files, File) of
		[{_}] ->
		    ok;
		_ ->
		    io:format("~n% ~s~n", [File]),
		    ets:insert(files, {File})
	    end,
	    case Str of
		[] -> ok;
		_ -> io:format("{~p, \"\"}.~n", [Str])
	    end,
	    ets:insert(translations, {Str, ""});
	_ ->
	    ok
    end.

load_file(File) ->
    case file:consult(File) of
	{ok, Terms} ->
	    lists:foreach(
	      fun({Orig, Trans}) ->
		    case Trans of
			"" ->
			    ok;
			_ ->
			    ets:insert(translations, {Orig, Trans})
		    end
	      end, Terms);
	Err ->
	    Err
    end.

find_src_files(Dir) ->
    case file:list_dir(Dir) of
	{ok, FileList} ->
	    recurse_filelist(
	      lists:map(
	        fun(F) ->
			filename:join(Dir, F)
		end, FileList));
	_ ->
	    []
    end.

recurse_filelist(FileList) ->
    recurse_filelist(FileList, []).

recurse_filelist([], Acc) ->
    lists:reverse(Acc);

recurse_filelist([H | T], Acc) ->
    case file:read_file_info(H) of
	{ok, #file_info{type = directory}} ->
	    recurse_filelist(T, lists:reverse(find_src_files(H)) ++ Acc);
	{ok, #file_info{type = regular}} ->
	    case string:substr(H, string:len(H) - 3) of
		".erl" ->
		    recurse_filelist(T, [H | Acc]);
		".hrl" ->
		    recurse_filelist(T, [H | Acc]);
		_ ->
		    recurse_filelist(T, Acc)
	    end;
	_ ->
	    recurse_filelist(T, Acc)
    end.


print_usage() ->
    io:format(
      "Usage: extract_translations [-unused] dir file~n"
      "~n"
      "Example:~n"
      "  extract_translations . ./msgs/ru.msg~n"
     ).

