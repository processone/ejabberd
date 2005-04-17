%%%----------------------------------------------------------------------
%%% File    : ejabberd_http.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 27 Feb 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_http).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_fsm).

%% External exports
-export([start/2,
	 start_link/2]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_headers/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DICT, dict).

-record(state, {socket,
		request_method,
		request_path,
		request_auth
	       }).


-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(XHTML_DOCTYPE,
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
	"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    supervisor:start_child(ejabberd_http_sup, [SockData, Opts]).

start_link(SockData, Opts) ->
    gen_fsm:start_link(ejabberd_http, [SockData, Opts], ?FSMOPTS).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}                   
%%----------------------------------------------------------------------
init([{SockMod, Socket}, Opts]) ->
    ?INFO_MSG("started: ~p", [{SockMod, Socket}]),
    case SockMod of
	gen_tcp ->
	    inet:setopts(Socket, [{packet, http}, {active, true}, {recbuf, 0}]);
	ssl ->
	    ssl:setopts(Socket, [{packet, http}, {active, true}])
    end,
    {ok, wait_for_headers, #state{socket = Socket}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------

wait_for_headers(_, StateData) ->
    {next_state, wait_for_headers, StateData}.


%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info({http_request, _Socket, Method, Path, _Version},
	    StateName, StateData) ->
    {next_state, StateName, StateData#state{request_method = Method,
					    request_path = Path}};
handle_info({http_header, _Socket, _, 'Authorization', _, Auth},
	    StateName, StateData) ->
    {next_state, StateName,
     StateData#state{request_auth = parse_auth(Auth)}};
handle_info({http_eoh, _Socket}, StateName, StateData) ->
    process_request(StateData),
    {stop, normal, StateData};

handle_info({tcp_closed, _Socket}, StateName, StateData) ->
    {stop, normal, StateData};

handle_info({tcp_error, _Socket, _Reason}, StateName, StateData) ->
    {stop, normal, StateData};

handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, _StateName, StateData) ->
    ?INFO_MSG("terminated: ~p", [Reason]),
    gen_tcp:close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(State, Text) ->
    gen_tcp:send(State#state.socket, Text).


process_request(#state{request_method = 'GET',
		       request_path = {abs_path, Path},
		       request_auth = undefined} = State) ->
    Out = make_xhtml_output(
	    401,
	    [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
	    make_xhtml([{xmlelement, "h1", [],
			 [{xmlcdata, "403 Unauthorized"}]}])),
    send_text(State, Out),
    ok;

process_request(#state{request_method = 'GET',
		       request_path = {abs_path, Path},
		       request_auth = {User, Pass}} = State) ->
    Out = make_xhtml_output(
	    200,
	    [],
	    make_xhtml([{xmlelement, "h1", [],
			 [{xmlcdata, "Welcome " ++ User}]}])),
    send_text(State, Out),
    ok;

process_request(State) ->
    todo.



make_xhtml(Els) ->
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"},
			  {"xml:lang", "en"},
			  {"lang", "en"}],
     [{xmlelement, "head", [],
       [{xmlelement, "meta", [{"http-equiv", "Content-Type"},
			      {"content", "text/html; charset=utf-8"}], []}]},
      {xmlelement, "body", [], Els}
     ]}.


make_xhtml_output(Status, Headers, XHTML) ->
    Data = list_to_binary([?XHTML_DOCTYPE, xml:element_to_string(XHTML)]),
    Headers1 = [{"Content-Type", "text/html; charset=utf-8"},
		{"Content-Length", integer_to_list(size(Data))} | Headers],
    H = lists:map(fun({Attr, Val}) ->
			  [Attr, ": ", Val, "\r\n"]
		  end, Headers1),
    SL = ["HTTP/1.1 ", integer_to_list(Status), " ",
	  code_to_phrase(Status), "\r\n"],
    [SL, H, "\r\n", Data].
    


% Code below is taken (with some modifications) from the yaws webserver, which
% is distributed under the folowing license:
%
%This software (the yaws webserver) is free software.
%Parts of this software is Copyright (c) Claes Wikstrom <klacke@hyber.org>
%Any use or misuse of the source code is hereby freely allowed.
%
%1. Redistributions of source code must retain the above copyright
%   notice as well as this list of conditions.
%
%2. Redistributions in binary form must reproduce the above copyright
%   notice as well as this list of conditions.


%% url decode the path and return {Path, QueryPart}

url_decode_q_split(Path) ->
    url_decode_q_split(Path, []).

url_decode_q_split([$%, $C, $2, $%, Hi, Lo | Tail], Ack) ->
    Hex = hex_to_integer([Hi, Lo]),
    url_decode_q_split(Tail, [Hex|Ack]);
url_decode_q_split([$%, $C, $3, $%, Hi, Lo | Tail], Ack) when Hi > $9 ->
    Hex = hex_to_integer([Hi+4, Lo]),
    url_decode_q_split(Tail, [Hex|Ack]);
url_decode_q_split([$%, $C, $3, $%, Hi, Lo | Tail], Ack) when Hi < $A ->
    Hex = hex_to_integer([Hi+4+7, Lo]),
    url_decode_q_split(Tail, [Hex|Ack]);
url_decode_q_split([$%, Hi, Lo | Tail], Ack) ->
    Hex = hex_to_integer([Hi, Lo]),
    url_decode_q_split(Tail, [Hex|Ack]);
url_decode_q_split([$?|T], Ack) ->
    %% Don't decode the query string here, that is parsed separately.
    {path_norm_reverse(Ack), T};
url_decode_q_split([H|T], Ack) ->
    url_decode_q_split(T, [H|Ack]);
url_decode_q_split([], Ack) ->
    {path_norm_reverse(Ack), []}.

path_norm_reverse("/" ++ T) -> start_dir(0, "/", T);
path_norm_reverse(       T) -> start_dir(0,  "", T).

start_dir(N, Path, ".."       ) -> rest_dir(N, Path, "");
start_dir(N, Path, "/"   ++ T ) -> start_dir(N    , Path, T);
start_dir(N, Path, "./"  ++ T ) -> start_dir(N    , Path, T);
start_dir(N, Path, "../" ++ T ) -> start_dir(N + 1, Path, T);
start_dir(N, Path,          T ) -> rest_dir (N    , Path, T).

rest_dir (_N, Path, []         ) -> case Path of 
				       [] -> "/";
				       _  -> Path
				   end;
rest_dir (0, Path, [ $/ | T ] ) -> start_dir(0    , [ $/ | Path ], T);
rest_dir (N, Path, [ $/ | T ] ) -> start_dir(N - 1,        Path  , T);
rest_dir (0, Path, [  H | T ] ) -> rest_dir (0    , [  H | Path ], T);
rest_dir (N, Path, [ _H | T ] ) -> rest_dir (N    ,        Path  , T).


%% hex_to_integer


hex_to_integer(Hex) ->
    case catch erlang:list_to_integer(Hex, 16) of
	{'EXIT', _} ->
	    old_hex_to_integer(Hex);
	X ->
	    X
    end.


old_hex_to_integer(Hex) ->
    DEHEX = fun (H) when H >= $a, H =< $f -> H - $a + 10;
		(H) when H >= $A, H =< $F -> H - $A + 10;
		(H) when H >= $0, H =< $9 -> H - $0
	    end,
    lists:foldl(fun(E, Acc) -> Acc*16+DEHEX(E) end, 0, Hex).

code_to_phrase(100) -> "Continue";
code_to_phrase(101) -> "Switching Protocols ";
code_to_phrase(200) -> "OK";
code_to_phrase(201) -> "Created";
code_to_phrase(202) -> "Accepted";
code_to_phrase(203) -> "Non-Authoritative Information";
code_to_phrase(204) -> "No Content";
code_to_phrase(205) -> "Reset Content";
code_to_phrase(206) -> "Partial Content";
code_to_phrase(300) -> "Multiple Choices";
code_to_phrase(301) -> "Moved Permanently";
code_to_phrase(302) -> "Found";
code_to_phrase(303) -> "See Other";
code_to_phrase(304) -> "Not Modified";
code_to_phrase(305) -> "Use Proxy";
code_to_phrase(306) -> "(Unused)";
code_to_phrase(307) -> "Temporary Redirect";
code_to_phrase(400) -> "Bad Request";
code_to_phrase(401) -> "Unauthorized";
code_to_phrase(402) -> "Payment Required";
code_to_phrase(403) -> "Forbidden";
code_to_phrase(404) -> "Not Found";
code_to_phrase(405) -> "Method Not Allowed";
code_to_phrase(406) -> "Not Acceptable";
code_to_phrase(407) -> "Proxy Authentication Required";
code_to_phrase(408) -> "Request Timeout";
code_to_phrase(409) -> "Conflict";
code_to_phrase(410) -> "Gone";
code_to_phrase(411) -> "Length Required";
code_to_phrase(412) -> "Precondition Failed";
code_to_phrase(413) -> "Request Entity Too Large";
code_to_phrase(414) -> "Request-URI Too Long";
code_to_phrase(415) -> "Unsupported Media Type";
code_to_phrase(416) -> "Requested Range Not Satisfiable";
code_to_phrase(417) -> "Expectation Failed";
code_to_phrase(500) -> "Internal Server Error";
code_to_phrase(501) -> "Not Implemented";
code_to_phrase(502) -> "Bad Gateway";
code_to_phrase(503) -> "Service Unavailable";
code_to_phrase(504) -> "Gateway Timeout";
code_to_phrase(505) -> "HTTP Version Not Supported".


parse_auth(Orig = "Basic " ++ Auth64) ->
    case decode_base64(Auth64) of
	{error, _Err} ->
	    undefined;
	Auth ->
	    case string:tokens(Auth, ":") of
		[User, Pass] ->
		    {User, Pass};
		_ ->
		    undefined
	    end
    end;
parse_auth(_) ->
    undefined.



decode_base64([]) ->
  [];
decode_base64([Sextet1,Sextet2,$=,$=|Rest]) ->
  Bits2x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12),
  Octet1=Bits2x6 bsr 16,
  [Octet1|decode_base64(Rest)];
decode_base64([Sextet1,Sextet2,Sextet3,$=|Rest]) ->
  Bits3x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12) bor
    (d(Sextet3) bsl 6),
  Octet1=Bits3x6 bsr 16,
  Octet2=(Bits3x6 bsr 8) band 16#ff,
  [Octet1,Octet2|decode_base64(Rest)];
decode_base64([Sextet1,Sextet2,Sextet3,Sextet4|Rest]) ->
  Bits4x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12) bor
    (d(Sextet3) bsl 6) bor
    d(Sextet4),
  Octet1=Bits4x6 bsr 16,
  Octet2=(Bits4x6 bsr 8) band 16#ff,
  Octet3=Bits4x6 band 16#ff,
  [Octet1,Octet2,Octet3|decode_base64(Rest)];
decode_base64(_CatchAll) ->
  {error, bad_base64}.

d(X) when X >= $A, X =<$Z ->
    X-65;
d(X) when X >= $a, X =<$z ->
    X-71;
d(X) when X >= $0, X =<$9 ->
    X+4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.

