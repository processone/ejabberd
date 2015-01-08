%%%----------------------------------------------------------------------
%%% File    : translate.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Localization helper
%%% Created :  6 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(translate).

-author('alexey@process-one.net').

-export([start/0, load_dir/1, load_file/2,
	 translate/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

start() ->
    ets:new(translations, [named_table, public]),
    Dir = case os:getenv("EJABBERD_MSGS_PATH") of
	    false ->
		case code:priv_dir(ejabberd) of
		  {error, _} -> ?MSGS_DIR;
		  Path -> filename:join([Path, "msgs"])
		end;
	    Path -> Path
	  end,
    load_dir(iolist_to_binary(Dir)),
    ok.

-spec load_dir(binary()) -> ok.

load_dir(Dir) ->
    case file:list_dir(Dir) of
      {ok, Files} ->
	  MsgFiles = lists:filter(fun (FN) ->
					  case length(FN) > 4 of
					    true ->
						string:substr(FN, length(FN) - 3)
						  == ".msg";
					    _ -> false
					  end
				  end,
				  Files),
	  lists:foreach(fun (FNS) ->
                                FN = list_to_binary(FNS),
				LP = ascii_tolower(str:substr(FN, 1,
                                                              byte_size(FN) - 4)),
				L = case str:tokens(LP, <<".">>) of
				      [Language] -> Language;
				      [Language, _Project] -> Language
				    end,
				load_file(L, <<Dir/binary, "/", FN/binary>>)
			end,
			MsgFiles),
	  ok;
      {error, Reason} -> ?ERROR_MSG("~p", [Reason])
    end.

load_file(Lang, File) ->
    case file:open(File, [read]) of
        {ok, Fd} ->
            io:setopts(Fd, [{encoding,latin1}]),
            load_file_loop(Fd, 1, File, Lang),
            file:close(Fd);
        Error ->
            ExitText = iolist_to_binary([File, ": ",
                                         file:format_error(Error)]),
            ?ERROR_MSG("Problem loading translation file ~n~s",
                       [ExitText]),
            exit(ExitText)
    end.

load_file_loop(Fd, Line, File, Lang) ->
    case io:read(Fd, '', Line) of
        {ok,{Orig, Trans}, NextLine} ->
            Trans1 = case Trans of
                         <<"">> -> Orig;
                         _ -> Trans
                     end,
            ets:insert(translations,
                       {{Lang, iolist_to_binary(Orig)},
                        iolist_to_binary(Trans1)}),

            load_file_loop(Fd, NextLine, File, Lang);
        {ok,_, _NextLine} ->
            ExitText = iolist_to_binary([File,
                                         " approximately in the line ",
                                         Line]),
            ?ERROR_MSG("Problem loading translation file ~n~s",
                       [ExitText]),
            exit(ExitText);
        {error,
         {_LineNumber, erl_parse, _ParseMessage} = Reason} ->
            ExitText = iolist_to_binary([File,
                                         " approximately in the line ",
                                         file:format_error(Reason)]),
            ?ERROR_MSG("Problem loading translation file ~n~s",
                       [ExitText]),
            exit(ExitText);
        {error, Reason} ->
            ExitText = iolist_to_binary([File, ": ",
                                         file:format_error(Reason)]),
            ?ERROR_MSG("Problem loading translation file ~n~s",
                       [ExitText]),
            exit(ExitText);
        {eof,_Line} ->
            ok
    end.

-spec translate(binary(), binary()) -> binary().

translate(Lang, Msg) ->
    LLang = ascii_tolower(Lang),
    case ets:lookup(translations, {LLang, Msg}) of
      [{_, Trans}] -> Trans;
      _ ->
	  ShortLang = case str:tokens(LLang, <<"-">>) of
			[] -> LLang;
			[SL | _] -> SL
		      end,
	  case ShortLang of
	    <<"en">> -> Msg;
	    LLang -> translate(Msg);
	    _ ->
		case ets:lookup(translations, {ShortLang, Msg}) of
		  [{_, Trans}] -> Trans;
		  _ -> translate(Msg)
		end
	  end
    end.

translate(Msg) ->
    case ?MYLANG of
      <<"en">> -> Msg;
      Lang ->
	  LLang = ascii_tolower(Lang),
	  case ets:lookup(translations, {LLang, Msg}) of
	    [{_, Trans}] -> Trans;
	    _ ->
		ShortLang = case str:tokens(LLang, <<"-">>) of
			      [] -> LLang;
			      [SL | _] -> SL
			    end,
		case ShortLang of
		  <<"en">> -> Msg;
		  Lang -> Msg;
		  _ ->
		      case ets:lookup(translations, {ShortLang, Msg}) of
			[{_, Trans}] -> Trans;
			_ -> Msg
		      end
		end
	  end
    end.

ascii_tolower(B) ->
    iolist_to_binary(ascii_tolower_s(binary_to_list(B))).

ascii_tolower_s([C | Cs]) when C >= $A, C =< $Z ->
    [C + ($a - $A) | ascii_tolower_s(Cs)];
ascii_tolower_s([C | Cs]) -> [C | ascii_tolower_s(Cs)];
ascii_tolower_s([]) -> [].
