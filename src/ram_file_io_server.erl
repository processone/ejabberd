%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%% 
%% This file is mostly copied from Erlang file_io_server.erl
%% See: http://www.erlang.org/ml-archive/erlang-questions/200607/msg00080.html
%% for details on ram_file_io_server.erl (Erlang OTP R11B-2)
-module(ram_file_io_server).

%% A simple file server for io to one file instance per server instance.

-export([format_error/1]).
-export([start/3, start_link/3]).

-record(state, {handle,owner,mref,buf,read_mode}).

-define(PRIM_FILE, ram_file).
-define(READ_SIZE_LIST, 128).
-define(READ_SIZE_BINARY, (8*1024)).

-define(eat_message(M, T), receive M -> M after T -> timeout end).

%%%-----------------------------------------------------------------
%%% Exported functions

format_error({_Line, ?MODULE, Reason}) ->
    io_lib:format("~w", [Reason]);
format_error({_Line, Mod, Reason}) ->
    Mod:format_error(Reason);
format_error(ErrorId) ->
    erl_posix_msg:message(ErrorId).

start(Owner, FileName, ModeList) 
  when pid(Owner), list(FileName), list(ModeList) ->
    do_start(spawn, Owner, FileName, ModeList).

start_link(Owner, FileName, ModeList) 
  when pid(Owner), list(FileName), list(ModeList) ->
    do_start(spawn_link, Owner, FileName, ModeList).

%%%-----------------------------------------------------------------
%%% Server starter, dispatcher and helpers

do_start(Spawn, Owner, FileName, ModeList) ->
    Self = self(),
    Ref = make_ref(),
    Pid = 
	erlang:Spawn(
	  fun() ->
		  %% process_flag(trap_exit, true),
		  {ReadMode,Opts} = 
		      case lists:member(binary, ModeList) of
			  true ->
			      {binary,ModeList};
			  false ->
			      {list,[binary|ModeList]}
		      end,
		  case ?PRIM_FILE:open(FileName, Opts) of
		      {error, Reason} = Error ->
			  Self ! {Ref, Error},
			  exit(Reason);
		      {ok, Handle} ->
			  %% XXX must I handle R6 nodes here?
			  M = erlang:monitor(process, Owner),
			  Self ! {Ref, ok},
			  server_loop(
			    #state{handle    = Handle,
				   owner     = Owner, 
				   mref      = M, 
				   buf       = <<>>,
				   read_mode = ReadMode})
		  end
	  end),
    Mref = erlang:monitor(process, Pid),
    receive
	{Ref, {error, _Reason} = Error} ->
	    erlang:demonitor(Mref),
	    receive {'DOWN', Mref, _, _, _} -> ok after 0 -> ok end,
	    Error;
	{Ref, ok} ->
	    erlang:demonitor(Mref),
	    receive
		{'DOWN', Mref, _, _, Reason} ->
		    {error, Reason}
	    after 0 ->
		    {ok, Pid}
	    end;
	{'DOWN', Mref, _, _, Reason} ->
	    {error, Reason}
    end.

server_loop(#state{mref = Mref} = State) ->
    receive
	{file_request, From, ReplyAs, Request} when pid(From) ->
	    case file_request(Request, State) of
		{reply, Reply, NewState} ->
		    file_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{error, Reply, NewState} ->
		    %% error is the same as reply, except that
		    %% it breaks the io_request_loop further down
		    file_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{stop, Reason, Reply, _NewState} ->
		    file_reply(From, ReplyAs, Reply),
		    exit(Reason)
	    end;
	{io_request, From, ReplyAs, Request} when pid(From) ->
	    case io_request(Request, State) of
		{reply, Reply, NewState} ->
		    io_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{error, Reply, NewState} ->
		    %% error is the same as reply, except that
		    %% it breaks the io_request_loop further down
		    io_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{stop, Reason, Reply, _NewState} ->
		    io_reply(From, ReplyAs, Reply),
		    exit(Reason)
	    end;
	{'DOWN', Mref, _, _, Reason} ->
	    exit(Reason);
	_ ->
	    server_loop(State)
    end.

file_reply(From, ReplyAs, Reply) ->
    From ! {file_reply, ReplyAs, Reply}.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

%%%-----------------------------------------------------------------
%%% file requests

file_request({pread,At,Sz}, 
	     #state{handle=Handle,buf=Buf,read_mode=ReadMode}=State) ->
    case position(Handle, At, Buf) of
	{ok,_Offs} ->
	    case ?PRIM_FILE:read(Handle, Sz) of
		{ok,Bin} when ReadMode==list ->
		    std_reply({ok,binary_to_list(Bin)}, State);
		Reply ->
		    std_reply(Reply, State)
	    end;
	Reply ->
	    std_reply(Reply, State)
    end;
file_request({pwrite,At,Data}, 
	     #state{handle=Handle,buf=Buf}=State) ->
    case position(Handle, At, Buf) of
	{ok,_Offs} ->
	    std_reply(?PRIM_FILE:write(Handle, Data), State);
	Reply ->
	    std_reply(Reply, State)
    end;
file_request(sync, 
	     #state{handle=Handle}=State) ->
    case ?PRIM_FILE:sync(Handle) of
	{error,_}=Reply ->
	    {stop,normal,Reply,State};
	Reply ->
	    {reply,Reply,State}
    end;
file_request(close, 
	     #state{handle=Handle}=State) ->
    {stop,normal,?PRIM_FILE:close(Handle),State#state{buf= <<>>}};
file_request({position,At}, 
	     #state{handle=Handle,buf=Buf}=State) ->
    std_reply(position(Handle, At, Buf), State);
file_request(truncate, 
	     #state{handle=Handle}=State) ->
    case ?PRIM_FILE:truncate(Handle) of
	{error,_Reason}=Reply ->
	    {stop,normal,Reply,State#state{buf= <<>>}};
	Reply ->
	    {reply,Reply,State}
    end;
file_request(Unknown, 
	     #state{}=State) ->
    Reason = {request, Unknown},
    {error,{error,Reason},State}.

std_reply({error,_}=Reply, State) ->
    {error,Reply,State#state{buf= <<>>}};
std_reply(Reply, State) ->
    {reply,Reply,State#state{buf= <<>>}}.

%%%-----------------------------------------------------------------
%%% I/O request 

io_request({put_chars,Chars}, % binary(Chars) new in R9C
	   #state{buf= <<>>}=State) ->
    put_chars(Chars, State);
io_request({put_chars,Chars}, % binary(Chars) new in R9C
	   #state{handle=Handle,buf=Buf}=State) ->
    case position(Handle, cur, Buf) of
	{error,_}=Reply ->
	    {stop,normal,Reply,State#state{buf= <<>>}};
	_ ->
	    put_chars(Chars, State#state{buf= <<>>})
    end;
io_request({put_chars,Mod,Func,Args}, 
	   #state{}=State) ->
    case catch apply(Mod, Func, Args) of
	Chars when list(Chars); binary(Chars) ->
	    io_request({put_chars,Chars}, State);
	_ ->
	    {error,{error,Func},State}
    end;
io_request({get_until,_Prompt,Mod,Func,XtraArgs}, 
	   #state{}=State) ->
    get_chars(io_lib, get_until, {Mod, Func, XtraArgs}, State);
io_request({get_chars,_Prompt,N}, % New in R9C
	   #state{}=State) ->
    get_chars(N, State);
io_request({get_chars,_Prompt,Mod,Func,XtraArg}, % New in R9C
	   #state{}=State) ->
    get_chars(Mod, Func, XtraArg, State);
io_request({get_line,_Prompt}, % New in R9C
	   #state{}=State) ->
    get_chars(io_lib, collect_line, [], State);
io_request({setopts, Opts}, % New in R9C
	   #state{}=State) when list(Opts) ->
    setopts(Opts, State);
io_request({requests,Requests}, 
	   #state{}=State) when list(Requests) ->
    io_request_loop(Requests, {reply,ok,State});
io_request(Unknown, 
	   #state{}=State) ->
    Reason = {request,Unknown},
    {error,{error,Reason},State}.



%% Process a list of requests as long as the results are ok.

io_request_loop([], Result) ->
    Result;
io_request_loop([_Request|_Tail], 
		{stop,_Reason,_Reply,_State}=Result) ->
    Result;
io_request_loop([_Request|_Tail],
		{error,_Reply,_State}=Result) ->
    Result;
io_request_loop([Request|Tail], 
		{reply,_Reply,State}) ->
    io_request_loop(Tail, io_request(Request, State)).



%% I/O request put_chars
%%
put_chars(Chars, #state{handle=Handle}=State) ->
    case ?PRIM_FILE:write(Handle, Chars) of
	{error,_}=Reply ->
	    {stop,normal,Reply,State};
	Reply ->
	    {reply,Reply,State}
    end.


%% Process the I/O request get_chars
%%
get_chars(0, #state{read_mode=ReadMode}=State) ->
    {reply,cast(<<>>, ReadMode),State};
get_chars(N, #state{buf=Buf,read_mode=ReadMode}=State) 
  when integer(N), N > 0, N =< size(Buf) ->
    {B1,B2} = split_binary(Buf, N),
    {reply,cast(B1, ReadMode),State#state{buf=B2}};
get_chars(N, #state{handle=Handle,buf=Buf,read_mode=ReadMode}=State) 
  when integer(N), N > 0 ->
    BufSize = size(Buf),
    NeedSize = N-BufSize,
    Size = max(NeedSize, ?READ_SIZE_BINARY),
    case ?PRIM_FILE:read(Handle, Size) of
	{ok, B} ->
	    if BufSize+size(B) < N ->
		    std_reply(cat(Buf, B, ReadMode), State);
	       true ->
		    {B1,B2} = split_binary(B, NeedSize),
		    {reply,cat(Buf, B1, ReadMode),State#state{buf=B2}}
	    end;
	eof when BufSize==0 ->
	    {reply,eof,State};
	eof ->
	    std_reply(cast(Buf, ReadMode), State);
	{error,Reason}=Error ->
	    {stop,Reason,Error,State#state{buf= <<>>}}
    end;
get_chars(_N, #state{}=State) ->
    {error,{error,get_chars},State}.

get_chars(Mod, Func, XtraArg, #state{buf= <<>>}=State) ->
    get_chars_empty(Mod, Func, XtraArg, start, State);
get_chars(Mod, Func, XtraArg, #state{buf=Buf}=State) ->
    get_chars_apply(Mod, Func, XtraArg, start, State#state{buf= <<>>}, Buf).

get_chars_empty(Mod, Func, XtraArg, S, 
		#state{handle=Handle,read_mode=ReadMode}=State) ->
    case ?PRIM_FILE:read(Handle, read_size(ReadMode)) of
	{ok,Bin} ->
	    get_chars_apply(Mod, Func, XtraArg, S, State, Bin);
	eof ->
	    get_chars_apply(Mod, Func, XtraArg, S, State, eof);
	{error,Reason}=Error ->
	    {stop,Reason,Error,State}
    end.

get_chars_apply(Mod, Func, XtraArg, S0, 
		#state{read_mode=ReadMode}=State, Data0) ->
    Data1 = case ReadMode of
	       list when binary(Data0) -> binary_to_list(Data0);
	       _ -> Data0
	    end,
    case catch Mod:Func(S0, Data1, XtraArg) of
	{stop,Result,Buf} ->
	    {reply,Result,State#state{buf=cast_binary(Buf)}};
	{'EXIT',Reason} ->
	    {stop,Reason,{error,err_func(Mod, Func, XtraArg)},State};
	S1 ->
	    get_chars_empty(Mod, Func, XtraArg, S1, State)
    end.

%% Convert error code to make it look as before
err_func(io_lib, get_until, {_,F,_}) ->
    F;
err_func(_, F, _) ->
    F.



%% Process the I/O request setopts
%%
%% setopts
setopts(Opts0, State) ->
    Opts = proplists:substitute_negations([{list,binary}], Opts0),
    case proplists:get_value(binary, Opts) of
	true ->
	    {ok,ok,State#state{read_mode=binary}};
	false ->
	    {ok,ok,State#state{read_mode=list}};
	_ ->
	    {error,{error,badarg},State}
    end.



%% Concatenate two binaries and convert the result to list or binary
cat(B1, B2, binary) ->
    list_to_binary([B1,B2]);
cat(B1, B2, list) ->
    binary_to_list(B1)++binary_to_list(B2).

%% Cast binary to list or binary
cast(B, binary) ->
    B;
cast(B, list) ->
    binary_to_list(B).

%% Convert buffer to binary
cast_binary(Binary) when binary(Binary) ->
    Binary;
cast_binary(List) when list(List) ->
    list_to_binary(List);
cast_binary(_EOF) ->
    <<>>.

%% Read size for different read modes
read_size(binary) ->
    ?READ_SIZE_BINARY;
read_size(list) ->
    ?READ_SIZE_LIST.

max(A, B) when A >= B ->
    A;
max(_, B) ->
    B.

%%%-----------------------------------------------------------------
%%% ?PRIM_FILE helpers

%% Compensates ?PRIM_FILE:position/2 for the number of bytes 
%% we have buffered

position(Handle, cur, Buf) ->
    position(Handle, {cur, 0}, Buf);
position(Handle, {cur, Offs}, Buf) when list(Buf) ->
    ?PRIM_FILE:position(Handle, {cur, Offs-length(Buf)});
position(Handle, {cur, Offs}, Buf) when binary(Buf) ->
    ?PRIM_FILE:position(Handle, {cur, Offs-size(Buf)});
position(Handle, At, _Buf) ->
    ?PRIM_FILE:position(Handle, At).

