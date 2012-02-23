%%%----------------------------------------------------------------------
%%% File    : translate.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Localization helper
%%% Created :  6 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(translate).
-author('alexey@process-one.net').

-export([start/0,
	 load_dir/1,
	 load_file/2,
	 translate/2]).

-export([tokens/2, ascii_tolower/1]).

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
		      LP = string:substr(FN, 1, string:len(FN) - 4),
		      L1 = case string:tokens(LP, ".") of
			       [Language] -> Language;
			       [Language, _Project] -> Language
			   end,
		      L2 = ascii_tolower(list_to_binary(L1)),
		      load_file(L2, Dir ++ "/" ++ FN)
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
        %% Code copied from ejabberd_config.erl
	{error, {_LineNumber, erl_parse, _ParseMessage} = Reason} ->
	    ExitText = lists:flatten(File ++ " approximately in the line "
				     ++ file:format_error(Reason)),
	    ?ERROR_MSG("Problem loading translation file ~n~s", [ExitText]),
	    exit(ExitText);
	{error, Reason} ->
	    ExitText = lists:flatten(File ++ ": " ++ file:format_error(Reason)),
	    ?ERROR_MSG("Problem loading translation file ~n~s", [ExitText]),
	    exit(ExitText)
    end.

translate(Lang, Msg) ->
    LLang = ascii_tolower(Lang),
    case ets:lookup(translations, {LLang, Msg}) of
	[{_, Trans}] ->
	    Trans;
	_ ->
	    ShortLang = case tokens(LLang, $-) of
			    [] ->
				LLang;
			    [SL | _] ->
				SL
			end,
	    case ShortLang of
		<<"en">> ->
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
    %%TODO: ?MYLANG macro returns lang as a list(). Lang should be a binary.
    case ?MYLANG of
	undefined ->
	    Msg;
	"en" ->
	    Msg;
	Lang ->
        BLang = list_to_binary(Lang),
	    LLang = ascii_tolower(BLang),
	    case ets:lookup(translations, {LLang, Msg}) of
		[{_, Trans}] ->
		    Trans;
		_ ->
		    ShortLang = case tokens(LLang, $-) of
				    [] ->
					LLang;
				    [SL | _] ->
					SL
				end,
		    case ShortLang of
			<<"en">> ->
			    Msg;
			BLang ->
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


ascii_tolower(undefined) ->
    <<>>;
ascii_tolower(Bin) ->
    << <<(char_tolower(X))>> || <<X>> <= Bin >>.

char_tolower(C) when C >= $A, C =< $Z ->
    C + ($a -$A);
char_tolower(C) ->
    C.

tokens(<<>>,_Sep) ->
    [];
tokens(Bin, Sep) ->
    tokens(Bin, Sep, <<>>, []).

tokens(<<>>, _Sep, T, Tokens) ->
    lists:reverse([T|Tokens]);
tokens(<<Sep, R/binary>>, Sep, T, Tokens) ->
    tokens(R, Sep, <<>>, [T | Tokens]);
tokens(<<C, R/binary>>, Sep, T, Tokens) ->
    tokens(R, Sep, <<T/binary, C>>, Tokens).
