%%%----------------------------------------------------------------------
%%% File    : translate.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Localization helper
%%% Created :  6 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-behaviour(gen_server).

-export([start_link/0, reload/0, translate/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("kernel/include/file.hrl").

-define(ZERO_DATETIME, {{0,0,0}, {0,0,0}}).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    load(),
    xmpp:set_tr_callback({?MODULE, translate}),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    xmpp:set_tr_callback(undefined).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec reload() -> ok.
reload() ->
    load(true).

-spec load() -> ok.
load() ->
    load(false).

-spec load(boolean()) -> ok.
load(ForceCacheRebuild) ->
    {MsgsDirMTime, MsgsDir} = get_msg_dir(),
    {CacheMTime, CacheFile} = get_cache_file(),
    {FilesMTime, MsgFiles} = get_msg_files(MsgsDir),
    LastModified = lists:max([MsgsDirMTime, FilesMTime]),
    if ForceCacheRebuild orelse CacheMTime < LastModified ->
	    load(MsgFiles, MsgsDir),
	    dump_to_file(CacheFile);
       true ->
	    case ets:file2tab(CacheFile) of
		{ok, _} ->
		    ok;
		{error, {read_error, {file_error, _, enoent}}} ->
		    load(MsgFiles, MsgsDir);
		{error, {read_error, {file_error, _, Reason}}} ->
		    ?WARNING_MSG("Failed to read translation cache from ~s: ~s",
				 [CacheFile, file:format_error(Reason)]),
		    load(MsgFiles, MsgsDir);
		{error, Reason} ->
		    ?WARNING_MSG("Failed to read translation cache from ~s: ~p",
				 [CacheFile, Reason]),
		    load(MsgFiles, MsgsDir)
	    end
    end.

-spec load([file:filename()], file:filename()) -> ok.

load(Files, Dir) ->
    try ets:new(translations, [named_table, public])
    catch _:badarg -> ok
    end,
    case Files of
	[] ->
	    ?WARNING_MSG("No translation files found in ~s, "
			 "check directory access", [Dir]);
	_ ->
	    ets:delete_all_objects(translations),
	    ?INFO_MSG("Building translation cache, this may take a while", []),
	    lists:foreach(
	      fun(File) ->
		      BaseName = filename:basename(File),
		      Lang = str:to_lower(filename:rootname(BaseName)),
		      load_file(iolist_to_binary(Lang), File)
	      end, Files)
    end.

load_file(Lang, File) ->
    case file:open(File, [read]) of
        {ok, Fd} ->
            io:setopts(Fd, [{encoding,latin1}]),
            load_file_loop(Fd, 1, File, Lang),
            file:close(Fd);
        {error, Error} ->
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

-spec get_msg_dir() -> {calendar:datetime(), file:filename()}.
get_msg_dir() ->
    Dir = case os:getenv("EJABBERD_MSGS_PATH") of
	      false ->
		  case code:priv_dir(ejabberd) of
		      {error, _} -> ?MSGS_DIR;
		      Path -> filename:join([Path, "msgs"])
		  end;
	      Path -> Path
	  end,
    case file:read_file_info(Dir) of
	{ok, #file_info{mtime = MTime}} ->
	    {MTime, Dir};
	{error, Reason} ->
	    ?ERROR_MSG("Failed to read directory ~s: ~s",
		       [Dir, file:format_error(Reason)]),
	    {?ZERO_DATETIME, Dir}
    end.

-spec get_msg_files(file:filename()) -> {calendar:datetime(), [file:filename()]}.
get_msg_files(MsgsDir) ->
    Res = filelib:fold_files(
	    MsgsDir, ".+\\.msg", false,
	    fun(File, {MTime, Files} = Acc) ->
		    case file:read_file_info(File) of
			{ok, #file_info{mtime = Time}} ->
			    {lists:max([MTime, Time]), [File|Files]};
			{error, Reason} ->
			    ?ERROR_MSG("Failed to read translation file ~s: ~s",
				       [File, file:format_error(Reason)]),
			    Acc
		    end
	    end, {?ZERO_DATETIME, []}),
    case Res of
	{_, []} ->
	    case file:list_dir(MsgsDir) of
		{ok, _} -> ok;
		{error, Reason} ->
		    ?ERROR_MSG("Failed to read directory ~s: ~s",
			       [MsgsDir, file:format_error(Reason)])
	    end;
	_ ->
	    ok
    end,
    Res.

-spec get_cache_file() -> {calendar:datetime(), file:filename()}.
get_cache_file() ->
    MnesiaDir = mnesia:system_info(directory),
    CacheFile = filename:join(MnesiaDir, "translations.cache"),
    CacheMTime = case file:read_file_info(CacheFile) of
		     {ok, #file_info{mtime = Time}} -> Time;
		     {error, _} -> ?ZERO_DATETIME
		 end,
    {CacheMTime, CacheFile}.

-spec dump_to_file(file:filename()) -> ok.
dump_to_file(CacheFile) ->
    case ets:tab2file(translations, CacheFile) of
	ok -> ok;
	{error, Reason} ->
	    ?WARNING_MSG("Failed to create translation cache in ~s: ~p",
			 [CacheFile, Reason])
    end.
