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
    Dir = 
	case os:getenv("EJABBERD_MSGS_PATH") of
	    false ->
		case code:priv_dir(ejabberd) of
		    {error, _} ->
			?MSGS_DIR;
		    Path ->
			filename:join([Path, "msgs"])
		end;
	    Path ->
		Path
	end,
    load_dir(Dir),
    ok.

load_dir(Dir) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    MsgFiles = lists:filter(
			 fun(FN) ->
				 case string:len(FN) > 4 of
				     true ->
					 string:substr(
					   FN,
					   string:len(FN) - 3) == ".msg";
				     _ ->
					 false
				 end
			 end, Files),
	    lists:foreach(
	      fun(FN) ->
		      L = ascii_tolower(
			    string:substr(FN, 1, string:len(FN) - 4)),
		      load_file(L, Dir ++ "/" ++ FN)
	      end, MsgFiles),
	    ok;
	{error, Reason} ->
	    ?ERROR_MSG("~p", [Reason])
    end.

load_file(Lang, File) ->
    case file:consult(File) of
	{ok, Terms} ->
	    lists:foreach(fun({Orig, Trans}) ->
			      Trans1 = case Trans of
					   "" ->
					       Orig;
					   _ ->
					       Trans
				       end,
			      ets:insert(translations,
					     {{Lang, Orig}, Trans1})
			  end, Terms);
	{error, Reason} ->
	    exit(file:format_error(Reason))
    end.

translate(Lang, Msg) ->
    LLang = ascii_tolower(Lang),
    case ets:lookup(translations, {LLang, Msg}) of
	[{_, Trans}] ->
	    Trans;
	_ ->
	    ShortLang = case string:tokens(LLang, "-") of
			    [] ->
				LLang;
			    [SL | _] ->
				SL
			end,
	    case ShortLang of
		"en" ->
		    Msg;
		LLang ->
		    translate(Msg);
		_ ->
		    case ets:lookup(translations, {ShortLang, Msg}) of
			[{_, Trans}] ->
			    Trans;
			_ ->
			    translate(Msg)
		    end
	    end
    end.

translate(Msg) ->
    case ?MYLANG of
	undefined ->
	    Msg;
	"en" ->
	    Msg;
	Lang ->
	    LLang = ascii_tolower(Lang),
	    case ets:lookup(translations, {LLang, Msg}) of
		[{_, Trans}] ->
		    Trans;
		_ ->
		    ShortLang = case string:tokens(LLang, "-") of
				    [] ->
					LLang;
				    [SL | _] ->
					SL
				end,
		    case ShortLang of
			"en" ->
			    Msg;
			Lang ->
			    Msg;
			_ ->
			    case ets:lookup(translations, {ShortLang, Msg}) of
				[{_, Trans}] ->
				    Trans;
				_ ->
				    Msg
			    end
		    end
	    end
    end.

ascii_tolower([C | Cs]) when C >= $A, C =< $Z ->
    [C + ($a - $A) | ascii_tolower(Cs)];
ascii_tolower([C | Cs]) ->
    [C | ascii_tolower(Cs)];
ascii_tolower([]) ->
    [].

