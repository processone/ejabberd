%%%----------------------------------------------------------------------
%%% File    : translate.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  6 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(translate).
-author('alexey@sevcom.net').

-export([start/0,
	 load_dir/1,
	 load_file/2,
	 translate/2]).

-include("ejabberd.hrl").

start() ->
    ets:new(translations, [named_table, public]),
    load_dir(?MSGS_DIR),
    ok.

load_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    MsgFiles = lists:filter(
		 fun(FN) ->
			 case string:len(FN) > 4 of
			     true ->
				 string:substr(FN,
					       string:len(FN) - 3) == ".msg";
			     _ ->
				 false
			 end
		 end, Files),
    lists:foreach(
      fun(FN) ->
	      load_file(string:substr(FN, 1, string:len(FN) - 4),
			Dir ++ "/" ++ FN)
      end, MsgFiles),
    ok.

load_file(Lang, File) ->
    {ok, Bin} = file:read_file(File),
    Content = binary_to_list(Bin),
    parse(Lang, Content).


parse(Lang, String) ->
    case erl_scan:tokens([], String, 0) of
	{done, Result, Left} ->
	    {ok, Tokens, _} = Result,
	    {ok, Term} = erl_parse:parse_term(Tokens),
	    {Orig, Trans} = Term,
	    ets:insert(translations, {{Lang, Orig}, Trans}),
	    parse(Lang, Left);
	_ ->
	    ok
    end.

translate(Lang, Msg) ->
    case ets:lookup(translations, {Lang, Msg}) of
	[{_, Trans}] ->
	    Trans;
	_ ->
	    Msg
    end.

