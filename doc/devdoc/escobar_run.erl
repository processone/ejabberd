%%%-------------------------------------------------------------------
%%% File    : escobar_run.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Frontend to run Escobar
%%% Created : 16 Apr 2008 by Badlop <badlop@process-one.net>
%%%-------------------------------------------------------------------
-module(escobar_run).

%%% Download ejabberd_hilite.erl from http://code.google.com/p/erl-escobar/

%%% Example calls:
%%% escobar_run:file("escobar_run.erl", "../ejascobar/").
%%% escobar_run:dir(".", "../ejascobar/").
%%% escobar_run:dir(".", "../doc/api/").
%%% find ./ -type f -name '*.html' -exec sed -i 's/class="function" >\([a-z0-9]*\)</class="function" id="\1">\1</;' {} \;

-export([file/2, file/1, dir/1]).

file([F, OutDir]) ->
    file(F, OutDir).

file(F, OutDir) ->
    String = escobar_hilite:file(F),
    FB = filename:basename(F),
    FilenameHTML = filename:join(OutDir, FB ++ ".html"),
    escobar_hilite:out(FilenameHTML, String).

dir([SrcDir, OutDir]) ->
    SrcDirAbs = filename:absname(SrcDir),
    OutDirAbs = filename:absname(OutDir),
    Files = get_files([SrcDirAbs]),
    lists:foreach(
      fun(F) ->
	      case filename:extension(F) of
		  ".erl" ->
		      file(F, OutDirAbs);
		  ".hrl" ->
		      file(F, OutDirAbs);
		  _ ->
		      ok
	      end
      end,
      Files).

get_files([]) ->
    [];
get_files([FHead | FTail]) ->
    case catch file:list_dir(FHead) of
	{ok, Files} ->
	    FilesHead = [filename:join(FHead, FilesN) || FilesN <- Files],
	    get_files(FilesHead ++ FTail);
	{error, enotdir} ->
	    [FHead] ++ get_files(FTail)
    end.
