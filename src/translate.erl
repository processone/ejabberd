%%%----------------------------------------------------------------------
%%% File    : translate.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Localization helper
%%% Created :  6 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-include("logger.hrl").
-include_lib("kernel/include/file.hrl").

-define(ZERO_DATETIME, {{0,0,0}, {0,0,0}}).

-type error_reason() :: file:posix() | {integer(), module(), term()} |
			badarg | terminated | system_limit | bad_file |
			bad_encoding.

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    case load() of
	ok ->
	    xmpp:set_tr_callback({?MODULE, translate}),
	    {ok, #state{}};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    xmpp:set_tr_callback(undefined).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec reload() -> ok | {error, error_reason()}.
reload() ->
    load(true).

-spec load() -> ok | {error, error_reason()}.
load() ->
    load(false).

-spec load(boolean()) -> ok | {error, error_reason()}.
load(ForceCacheRebuild) ->
    {MsgsDirMTime, MsgsDir} = get_msg_dir(),
    {CacheMTime, CacheFile} = get_cache_file(),
    {FilesMTime, MsgFiles} = get_msg_files(MsgsDir),
    LastModified = lists:max([MsgsDirMTime, FilesMTime]),
    if ForceCacheRebuild orelse CacheMTime < LastModified ->
	    case load(MsgFiles, MsgsDir) of
		ok -> dump_to_file(CacheFile);
		Err -> Err
	    end;
       true ->
	    case ets:file2tab(CacheFile) of
		{ok, _} ->
		    ok;
		{error, {read_error, {file_error, _, enoent}}} ->
		    load(MsgFiles, MsgsDir);
		{error, {read_error, {file_error, _, Reason}}} ->
		    ?WARNING_MSG("Failed to read translation cache from ~ts: ~ts",
				 [CacheFile, format_error(Reason)]),
		    load(MsgFiles, MsgsDir);
		{error, Reason} ->
		    ?WARNING_MSG("Failed to read translation cache from ~ts: ~p",
				 [CacheFile, Reason]),
		    load(MsgFiles, MsgsDir)
	    end
    end.

-spec load([file:filename()], file:filename()) -> ok | {error, error_reason()}.
load(Files, Dir) ->
    try ets:new(translations, [named_table, public]) of
	_ -> ok
    catch _:badarg -> ok
    end,
    case Files of
	[] ->
	    ?WARNING_MSG("No translation files found in ~ts, "
			 "check directory access",
			 [Dir]);
	_ ->
	    ?INFO_MSG("Building language translation cache", []),
	    Objs = lists:flatten(misc:pmap(fun load_file/1, Files)),
	    case lists:keyfind(error, 1, Objs) of
		false ->
		    ets:delete_all_objects(translations),
		    ets:insert(translations, Objs),
		    ?DEBUG("Language translation cache built successfully", []);
		{error, File, Reason} ->
		    ?ERROR_MSG("Failed to read translation file ~ts: ~ts",
			       [File, format_error(Reason)]),
		    {error, Reason}
	    end
    end.

-spec load_file(file:filename()) -> [{{binary(), binary()}, binary()} |
				     {error, file:filename(), error_reason()}].
load_file(File) ->
    Lang = lang_of_file(File),
    try file:consult(File) of
	{ok, Lines} ->
	    lists:map(
	      fun({In, Out}) ->
		      try {unicode:characters_to_binary(In),
			   unicode:characters_to_binary(Out)} of
			  {InB, OutB} when is_binary(InB), is_binary(OutB) ->
			      {{Lang, InB}, OutB};
			  _ ->
			      {error, File, bad_encoding}
		      catch _:badarg ->
			      {error, File, bad_encoding}
		      end;
		 (_) ->
		      {error, File, bad_file}
	      end, Lines);
	{error, Reason} ->
	    [{error, File, Reason}]
    catch _:{case_clause, {error, _}} ->
	    %% At the moment of the writing there was a bug in
	    %% file:consult_stream/3 - it doesn't process {error, term()}
	    %% result from io:read/3
	    [{error, File, bad_file}]
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

-spec translate(binary()) -> binary().
translate(Msg) ->
    case ejabberd_option:language() of
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

-spec ascii_tolower(list() | binary()) -> binary().
ascii_tolower(B) when is_binary(B) ->
    << <<(if X >= $A, X =< $Z ->
                  X + 32;
             true ->
                  X
          end)>> || <<X>> <= B >>;
ascii_tolower(S) ->
    ascii_tolower(unicode:characters_to_binary(S)).

-spec get_msg_dir() -> {calendar:datetime(), file:filename()}.
get_msg_dir() ->
    Dir = misc:msgs_dir(),
    case file:read_file_info(Dir) of
	{ok, #file_info{mtime = MTime}} ->
	    {MTime, Dir};
	{error, Reason} ->
	    ?ERROR_MSG("Failed to read directory ~ts: ~ts",
		       [Dir, format_error(Reason)]),
	    {?ZERO_DATETIME, Dir}
    end.

-spec get_msg_files(file:filename()) -> {calendar:datetime(), [file:filename()]}.
get_msg_files(MsgsDir) ->
    Res = filelib:fold_files(
	    MsgsDir, ".+\\.msg", false,
	    fun(File, {MTime, Files} = Acc) ->
		    case xmpp_lang:is_valid(lang_of_file(File)) of
			true ->
			    case file:read_file_info(File) of
				{ok, #file_info{mtime = Time}} ->
				    {lists:max([MTime, Time]), [File|Files]};
				{error, Reason} ->
				    ?ERROR_MSG("Failed to read translation file ~ts: ~ts",
					       [File, format_error(Reason)]),
				    Acc
			    end;
			false ->
			    ?WARNING_MSG("Ignoring translation file ~ts: file name "
					 "must be a valid language tag",
					 [File]),
			    Acc
		    end
	    end, {?ZERO_DATETIME, []}),
    case Res of
	{_, []} ->
	    case file:list_dir(MsgsDir) of
		{ok, _} -> ok;
		{error, Reason} ->
		    ?ERROR_MSG("Failed to read directory ~ts: ~ts",
			       [MsgsDir, format_error(Reason)])
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
	    ?WARNING_MSG("Failed to create translation cache in ~ts: ~p",
			 [CacheFile, Reason])
    end.

-spec lang_of_file(file:filename()) -> binary().
lang_of_file(FileName) ->
    BaseName = filename:basename(FileName),
    ascii_tolower(filename:rootname(BaseName)).

-spec format_error(error_reason()) -> string().
format_error(bad_file) ->
    "corrupted or invalid translation file";
format_error(bad_encoding) ->
    "cannot translate from UTF-8";
format_error({_, _, _} = Reason) ->
    "at line " ++ file:format_error(Reason);
format_error(Reason) ->
    file:format_error(Reason).
