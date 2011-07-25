%%%  Copyright (C) 2005 Nicolas Niclausse
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%%-------------------------------------------------------------------
%%% File    : ts_file_server.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : Read a line-based file
%%%
%%% Created :  6 Jul 2005 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------

-module(ts_file_server).
-author('nicolas.niclausse@niclux.org').

-behaviour(gen_server).

%% External exports
-export([start/0,
         get_random_line/0,
         get_random_line/1,
         get_next_line/0,
         get_next_line/1,
         get_all_lines/0,
         get_all_lines/1,
         stop/0,
         read/1, read/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(file, {items,       %% tuple of lines read from a file
               size,        %% total number of lines
               current=-1   %% current line in file
              }).

-record(state, {files}).

-define(DICT, dict).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("xmerl.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------

read(Filenames) ->
    gen_server:call({global, ?MODULE}, {read, Filenames}, ?config(file_server_timeout)).

read(Filenames, Timeout) ->
    gen_server:call({global, ?MODULE}, {read, Filenames}, Timeout).

start() ->
    ?LOG("Starting~n",?DEB),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).



get_random_line(FileID)->
    gen_server:call({global, ?MODULE}, {get_random_line, FileID}).
get_random_line() ->
    get_random_line(default).

get_next_line(FileID)->
    gen_server:call({global, ?MODULE}, {get_next_line, FileID}).
get_next_line() ->
    get_next_line(default).

get_all_lines(FileID)->
    gen_server:call({global, ?MODULE}, {get_all_lines, FileID}).
get_all_lines() ->
    get_all_lines(default).

stop()->
    gen_server:call({global, ?MODULE}, stop).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, #state{files=?DICT:new()}}.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({get_all_lines, FileID}, _From, State) ->
    FileDesc = ?DICT:fetch(FileID, State#state.files),
    Reply = {ok, tuple_to_list(FileDesc#file.items)},
    {reply, Reply, State};

handle_call({get_random_line, FileID}, _From, State) ->
    FileDesc = ?DICT:fetch(FileID, State#state.files),
    I = random:uniform(FileDesc#file.size),
    Reply = {ok, element(I, FileDesc#file.items)},
    {reply, Reply, State};

handle_call({get_next_line, FileID}, _From, State) ->
    FileDesc = ?DICT:fetch(FileID, State#state.files),
    I = (FileDesc#file.current + 1) rem FileDesc#file.size,
    Reply = {ok, element(I+1, FileDesc#file.items)},
    NewFileDesc = FileDesc#file{current=I},
    {reply, Reply,
     State#state{files=?DICT:store(FileID, NewFileDesc, State#state.files)}};

handle_call({read, Filenames}, _From, State) ->
    lists:foldl(fun open_file/2, {reply, ok, State}, Filenames);

handle_call(stop, _From, State)->
    {stop, normal, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Open a file and return a new state
%%----------------------------------------------------------------------
open_file({ID, Path}, {reply, Result, State}) ->
    case ?DICT:find(ID, State#state.files) of
        {ok, _} ->
            ?LOGF("File with id ~p already opened (path is ~p)~n",[ID, Path], ?WARN),
            {reply, {error, already_open}, State};
        error ->
            ?LOGF("Opening file ~p~n",[Path], ?INFO),
            {Status, File} = file:open(Path, [read] ),
            case Status of
                error ->
                    ?LOGF("Error while opening ~p file ~p~n",[File, Path], ?ERR),
                    {reply, {error, File}, State};
                _ ->
                    List_items = read_item(File, []),
                    file:close(File), % Close the config file
                    FileDesc = #file{items = list_to_tuple(List_items), size=length(List_items)},
                    {reply, Result,
                     State#state{files = ?DICT:store(ID, FileDesc, State#state.files)}}
            end
    end.


%%----------------------------------------------------------------------
%% Treate one line of the file
%% Lines starting by '#' are skipped
%%----------------------------------------------------------------------
read_item(File, L)->
    %% Read one line
    Line = io:get_line(File, ""),
    case Line of
        eof ->
            lists:reverse(L);
        _->
            Tokens = string:tokens(Line, "\n"),
            case Tokens of
                [] ->
                    read_item(File, L);
                ["#" | _] ->
                    read_item(File, L);
                [Value] ->
                    %% FIXME: maybe we should use an ets table instead ?
                    read_item(File, [Value|L])
            end
    end.
