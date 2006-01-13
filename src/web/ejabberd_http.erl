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

%% External exports
-export([start/2,
	 start_link/2,
	 become_controller/1,
	 receive_headers/1,
	 url_encode/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

-record(state, {sockmod,
		socket,
		request_method,
		request_version,
		request_path,
		request_auth,
		request_keepalive,
		request_content_length,
		request_lang = "en",
		use_http_poll = false,
		use_web_admin = false,
		end_of_request = false,
		trail = ""
	       }).


-define(XHTML_DOCTYPE,
	"<?xml version='1.0'?>\n"
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
	"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n").

-define(HTML_DOCTYPE,
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
	"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n").


start(SockData, Opts) ->
    supervisor:start_child(ejabberd_http_sup, [SockData, Opts]).

start_link({SockMod, Socket}, Opts) ->
    TLSEnabled = lists:member(tls, Opts),
    TLSOpts = lists:filter(fun({certfile, _}) -> true;
			      (_) -> false
			   end, Opts),
    {SockMod1, Socket1} =
	if
	    TLSEnabled ->
		inet:setopts(Socket, [{recbuf, 8192}]),
		{ok, TLSSocket} = tls:tcp_to_tls(Socket, TLSOpts),
		{tls, TLSSocket};
	    true ->
		{SockMod, Socket}
	end,
    case SockMod1 of
	gen_tcp ->
	    inet:setopts(Socket1, [{packet, http}, {recbuf, 8192}]);
	_ ->
	    ok
    end,
    UseHTTPPoll = lists:member(http_poll, Opts),
    UseWebAdmin = lists:member(web_admin, Opts),
    ?DEBUG("S: ~p~n", [{UseHTTPPoll, UseWebAdmin}]),
    ?INFO_MSG("started: ~p", [{SockMod1, Socket1}]),
    {ok, proc_lib:spawn_link(ejabberd_http,
			     receive_headers,
			     [#state{sockmod = SockMod1,
				     socket = Socket1,
				     use_http_poll = UseHTTPPoll,
				     use_web_admin = UseWebAdmin}])}.


become_controller(_Pid) ->
    ok.

send_text(State, Text) ->
    (State#state.sockmod):send(State#state.socket, Text).


receive_headers(State) ->
    SockMod = State#state.sockmod,
    Socket = State#state.socket,
    Data = SockMod:recv(Socket, 0, 300000),
    case State#state.sockmod of
	gen_tcp ->
	    NewState = process_header(State, Data),
	    case NewState#state.end_of_request of
		true ->
		    ok;
		_ ->
		    receive_headers(NewState)
	    end;
	_ ->
	    case Data of
		{ok, Binary} ->
		    {Request, Trail} = parse_request(
					 State,
					 State#state.trail ++ binary_to_list(Binary)),
		    State1 = State#state{trail = Trail},
		    NewState = lists:foldl(
				 fun(D, S) ->
					case S#state.end_of_request of
					    true ->
						S;
					    _ ->
						process_header(S, D)
					end
				 end, State1, Request),
		    case NewState#state.end_of_request of
			true ->
			    ok;
			_ ->
			    receive_headers(NewState)
		    end;
		_ ->
		    ok
	    end
    end.

process_header(State, Data) ->
    SockMod = State#state.sockmod,
    Socket = State#state.socket,
    case Data of
	{ok, {http_request, Method, Path, Version}} ->
	    KeepAlive = case Version of
		{1, 1} ->
		    true;
		_ ->
		    false
	    end,
	    State#state{request_method = Method,
			request_version = Version,
			request_path = Path,
			request_keepalive = KeepAlive};
	{ok, {http_header, _, 'Connection', _, Conn}} ->
	    KeepAlive1 = case jlib:tolower(Conn) of
			     "keep-alive" ->
				 true;
			     "close" ->
				 false;
			     _ ->
				 State#state.request_keepalive
			 end,
	    State#state{request_keepalive = KeepAlive1};
	{ok, {http_header, _, 'Authorization', _, Auth}} ->
	    State#state{request_auth = parse_auth(Auth)};
	{ok, {http_header, _, 'Content-Length', _, SLen}} ->
	    case catch list_to_integer(SLen) of
		Len when is_integer(Len) ->
		    State#state{request_content_length = Len};
		_ ->
		    State
	    end;
	{ok, {http_header, _, 'Accept-Language', _, Langs}} ->
	    State#state{request_lang = parse_lang(Langs)};
	{ok, {http_header, _, _, _, _}} ->
	    State;
	{ok, http_eoh} ->
	    ?INFO_MSG("(~w) http query: ~w ~s~n",
		      [State#state.socket,
		       State#state.request_method,
		       element(2, State#state.request_path)]),
	    Out = process_request(State),
	    send_text(State, Out),
	    case State#state.request_keepalive of
		true ->
		    case SockMod of
			gen_tcp ->
			    inet:setopts(Socket, [{packet, http}]);
			_ ->
			    ok
		    end,
		    #state{sockmod = SockMod,
			   socket = Socket,
			   use_http_poll = State#state.use_http_poll,
			   use_web_admin = State#state.use_web_admin};
		_ ->
		    #state{end_of_request = true}
	    end;
	{error, _Reason} ->
	    #state{end_of_request = true};
	_ ->
	    #state{end_of_request = true}
    end.

process_request(#state{request_method = 'GET',
		       request_path = {abs_path, Path},
		       request_auth = Auth,
		       request_lang = Lang,
		       use_http_poll = UseHTTPPoll,
		       use_web_admin = UseWebAdmin} = State) ->
    US = case Auth of
	     {SJID, P} ->
		 case jlib:string_to_jid(SJID) of
		     error ->
			 unauthorized;
		     #jid{user = U, server = S} ->
			 case ejabberd_auth:check_password(U, S, P) of
			     true ->
				 {U, S};
			     false ->
				 unauthorized
			 end
		 end;
	     _ ->
		 undefined
	 end,
    case US of
	unauthorized ->
	    make_xhtml_output(
	      State, 
	      401,
	      [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
	      ejabberd_web:make_xhtml([{xmlelement, "h1", [],
					[{xmlcdata, "401 Unauthorized"}]}]));
	_ ->
	    case (catch url_decode_q_split(Path)) of
		{'EXIT', _} ->
		    process_request(false);
		{NPath, Query} ->
		    LQuery = case (catch parse_urlencoded(Query)) of
				 {'EXIT', _Reason} ->
				     [];
				 LQ ->
				     LQ
			     end,
		    LPath = string:tokens(NPath, "/"),
		    Request = #request{method = 'GET',
				       path = LPath,
				       q = LQuery,
				       us = US,
				       lang = Lang},
		    case ejabberd_web:process_get({UseHTTPPoll, UseWebAdmin},
						  Request) of
			El when element(1, El) == xmlelement ->
			    make_xhtml_output(State, 200, [], El);
			{Status, Headers, El} when
			      element(1, El) == xmlelement ->
			    make_xhtml_output(State, Status, Headers, El);
			Text when is_list(Text) ->
			    make_text_output(State, 200, [], Text);
			{Status, Headers, Text} when
			      is_list(Text) ->
			    make_text_output(State, Status, Headers, Text)
		    end
	    end
    end;

process_request(#state{request_method = 'POST',
		       request_path = {abs_path, Path},
		       request_auth = Auth,
		       request_content_length = Len,
		       request_lang = Lang,
		       sockmod = SockMod,
		       socket = Socket,
		       use_http_poll = UseHTTPPoll,
		       use_web_admin = UseWebAdmin} = State)
  when is_integer(Len) ->
    US = case Auth of
	     {SJID, P} ->
		 case jlib:string_to_jid(SJID) of
		     error ->
			 unauthorized;
		     #jid{user = U, server = S} ->
			 case ejabberd_auth:check_password(U, S, P) of
			     true ->
				 {U, S};
			     false ->
				 unauthorized
			 end
		 end;
	     _ ->
		 undefined
	 end,
    case US of
	unauthorized ->
	    make_xhtml_output(
	      State, 
	      401,
	      [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
	      ejabberd_web:make_xhtml([{xmlelement, "h1", [],
					[{xmlcdata, "401 Unauthorized"}]}]));
	_ ->
    	    case SockMod of
		gen_tcp ->
		    inet:setopts(Socket, [{packet, 0}]);
		_ ->
		    ok
	    end,
	    Data = recv_data(State, Len),
	    ?DEBUG("client data: ~p~n", [Data]),
	    case (catch url_decode_q_split(Path)) of
		{'EXIT', _} ->
		    process_request(false);
		{NPath, Query} ->
		    LPath = string:tokens(NPath, "/"),
		    LQuery = case (catch parse_urlencoded(Data)) of
				 {'EXIT', _Reason} ->
				     [];
				 LQ ->
				     LQ
			     end,
		    Request = #request{method = 'POST',
				       path = LPath,
				       q = LQuery,
				       us = US,
				       data = Data,
				       lang = Lang},
		    case ejabberd_web:process_get({UseHTTPPoll, UseWebAdmin},
						  Request) of
			El when element(1, El) == xmlelement ->
			    make_xhtml_output(State, 200, [], El);
			{Status, Headers, El} when
			      element(1, El) == xmlelement ->
			    make_xhtml_output(State, Status, Headers, El);
			Text when is_list(Text) ->
			    make_text_output(State, 200, [], Text);
			{Status, Headers, Text} when is_list(Text) ->
			    make_text_output(State, Status, Headers, Text)
		    end
	    end
    end;

process_request(State) ->
    make_xhtml_output(State, 
      400,
      [],
      ejabberd_web:make_xhtml([{xmlelement, "h1", [],
				[{xmlcdata, "400 Bad Request"}]}])).


recv_data(State, Len) ->
    recv_data(State, Len, []).

recv_data(State, 0, Acc) ->
    binary_to_list(list_to_binary(Acc));
recv_data(State, Len, Acc) ->
    case State#state.trail of
	[] ->
	    case (State#state.sockmod):recv(State#state.socket, Len, 300000) of
		{ok, Data} ->
		    recv_data(State, Len - size(Data), [Acc | Data]);
		_ ->
		    ""
	    end;
	_ ->
	    Trail = State#state.trail,
	    recv_data(State#state{trail = ""}, Len - length(Trail), [Acc | Trail])
    end.


make_xhtml_output(State, Status, Headers, XHTML) ->
    Data = case lists:member(html, Headers) of
	       true ->
		   list_to_binary([?HTML_DOCTYPE,
				   xml:element_to_string(XHTML)]);
	       _ ->
		   list_to_binary([?XHTML_DOCTYPE,
				   xml:element_to_string(XHTML)])
	   end,
    Headers1 = case lists:keysearch("Content-Type", 1, Headers) of
		   {value, _} ->
		       [{"Content-Length", integer_to_list(size(Data))} |
			Headers];
		   _ ->
		       [{"Content-Type", "text/html; charset=utf-8"},
			{"Content-Length", integer_to_list(size(Data))} |
			Headers]
	       end,
    HeadersOut = case {State#state.request_version,
		       State#state.request_keepalive} of
		     {{1, 1}, true} -> Headers1;
		     {_, true} -> 
			 [{"Connection", "keep-alive"} | Headers1];
		     {_, false} -> 
			 % not required for http versions < 1.1
			 % but would make no harm
			 [{"Connection", "close"} | Headers1]
		 end,

    Version = case State#state.request_version of
		  {1, 1} -> "HTTP/1.1 ";
		  _ -> "HTTP/1.0 "
	      end,
	       
    H = lists:map(fun({Attr, Val}) ->
			  [Attr, ": ", Val, "\r\n"];
		     (_) ->
			  []
		  end, HeadersOut),
    SL = [Version, integer_to_list(Status), " ",
	  code_to_phrase(Status), "\r\n"],
    [SL, H, "\r\n", Data].

make_text_output(State, Status, Headers, Text) ->
    Data = list_to_binary(Text),
    Headers1 = case lists:keysearch("Content-Type", 1, Headers) of
		   {value, _} ->
		       [{"Content-Length", integer_to_list(size(Data))} |
			Headers];
		   _ ->
		       [{"Content-Type", "text/html; charset=utf-8"},
			{"Content-Length", integer_to_list(size(Data))} |
			Headers]
	       end,

    HeadersOut = case {State#state.request_version,
		       State#state.request_keepalive} of
		     {{1, 1}, true} -> Headers1;
		     {_, true} -> 
			 [{"Connection", "keep-alive"} | Headers1];
		     {_, false} -> 
			 % not required for http versions < 1.1
			 % but would make no harm
			 [{"Connection", "close"} | Headers1]
		 end,

    Version = case State#state.request_version of
		  {1, 1} -> "HTTP/1.1 ";
		  _ -> "HTTP/1.0 "
	      end,

    H = lists:map(fun({Attr, Val}) ->
			  [Attr, ": ", Val, "\r\n"]
		  end, HeadersOut),
    SL = [Version, integer_to_list(Status), " ",
	  code_to_phrase(Status), "\r\n"],
    [SL, H, "\r\n", Data].


parse_lang(Langs) ->
    case string:tokens(Langs, ",; ") of
	[First | _] ->
	    First;
	[] ->
	    "en"
    end.



% Code below is taken (with some modifications) from the yaws webserver, which
% is distributed under the folowing license:
%
% This software (the yaws webserver) is free software.
% Parts of this software is Copyright (c) Claes Wikstrom <klacke@hyber.org>
% Any use or misuse of the source code is hereby freely allowed.
%
% 1. Redistributions of source code must retain the above copyright
%    notice as well as this list of conditions.
%
% 2. Redistributions in binary form must reproduce the above copyright
%    notice as well as this list of conditions.


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


parse_urlencoded(S) ->
    parse_urlencoded(S, nokey, [], key).

parse_urlencoded([$%, Hi, Lo | Tail], Last, Cur, State) ->
    Hex = hex_to_integer([Hi, Lo]),
    parse_urlencoded(Tail, Last, [Hex | Cur],  State);

parse_urlencoded([$& | Tail], _Last, Cur, key) ->
    [{lists:reverse(Cur), ""} |
     parse_urlencoded(Tail, nokey, [], key)];  %% cont keymode

parse_urlencoded([$& | Tail], Last, Cur, value) ->
    V = {Last, lists:reverse(Cur)},
    [V | parse_urlencoded(Tail, nokey, [], key)];

parse_urlencoded([$+ | Tail], Last, Cur,  State) ->
    parse_urlencoded(Tail, Last, [$\s | Cur], State);

parse_urlencoded([$= | Tail], _Last, Cur, key) ->
    parse_urlencoded(Tail, lists:reverse(Cur), [], value); %% change mode

parse_urlencoded([H | Tail], Last, Cur, State) ->
    parse_urlencoded(Tail, Last, [H|Cur], State);

parse_urlencoded([], Last, Cur, _State) ->
    [{Last, lists:reverse(Cur)}];

parse_urlencoded(undefined, _, _, _) ->
    [].


url_encode([H|T]) ->
    if
	H >= $a, $z >= H ->
	    [H|url_encode(T)];
	H >= $A, $Z >= H ->
	    [H|url_encode(T)];
	H >= $0, $9 >= H ->
	    [H|url_encode(T)];
	H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
	    [H|url_encode(T)];
	true ->
	    case integer_to_hex(H) of
		[X, Y] ->
		    [$%, X, Y | url_encode(T)];
		[X] ->
		    [$%, $0, X | url_encode(T)]
	    end
    end;

url_encode([]) ->
    [].

integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
	{'EXIT', _} ->
	    old_integer_to_hex(I);
	Int ->
	    Int
    end.


old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I<16 ->
    [I-10+$A];
old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).


% The following code is mostly taken from yaws_ssl.erl

parse_request(State, Data) ->
    case Data of
	[] ->
	    {[], []};
	_ ->
	    ?DEBUG("GOT ssl data ~p~n", [Data]),
	    {R, Trail} = case State#state.request_method of
			     undefined ->
				 {R1, Trail1} = get_req(Data),
				 ?DEBUG("Parsed request ~p~n", [R1]),
				 {[R1], Trail1};
			     _ ->
				 {[], Data}
			 end,
	    {H, Trail2} = get_headers(Trail),
	    {R ++ H, Trail2}
    end.

get_req("\r\n\r\n" ++ _) ->
    bad_request;
get_req("\r\n" ++ Data) ->
    get_req(Data);
get_req(Data) ->
    {FirstLine, Trail} = lists:splitwith(fun not_eol/1, Data),
    R = parse_req(FirstLine),
    {R, Trail}.
	    

not_eol($\r)->
    false;
not_eol($\n) ->
    false;
not_eol(_) ->
    true.


get_word(Line)->
    {Word, T} = lists:splitwith(fun(X)-> X /= $\  end, Line),
    {Word, lists:dropwhile(fun(X) -> X == $\  end, T)}.


parse_req(Line) ->
    {MethodStr, L1} = get_word(Line),
    ?DEBUG("Method: ~p~n", [MethodStr]),
    case L1 of
	[] ->
	    bad_request;
	_ ->
	    {URI, L2} = get_word(L1),
	    {VersionStr, L3} = get_word(L2),
	    ?DEBUG("URI: ~p~nVersion: ~p~nL3: ~p~n",
		[URI, VersionStr, L3]),
	    case L3 of
		[] ->
		    Method = case MethodStr of
				 "GET" -> 'GET';
				 "POST" -> 'POST';
				 "HEAD" -> 'HEAD';
				 "OPTIONS" -> 'OPTIONS';
				 "TRACE" -> 'TRACE';
				 "PUT" -> 'PUT';
				 "DELETE" -> 'DELETE';
				 S -> S
			     end,
		    Path = case URI of
			       "*" ->
			       % Is this correct?
				   "*";
			       P ->
			       % FIXME: Handle
			       % absolute URIs
				   {abs_path, P}
			   end,
		    case VersionStr of
			[] ->
			    {ok, {http_request, Method, Path, {0,9}}};
			"HTTP/1.0" ->
			    {ok, {http_request, Method, Path, {1,0}}};
			"HTTP/1.1" ->
			    {ok, {http_request, Method, Path, {1,1}}};
			_ ->
			    bad_request
		    end;
		_ ->
		    bad_request
	    end
    end.


get_headers(Tail) ->
    get_headers([], Tail).

get_headers(H, Tail) ->
    case get_line(Tail) of
	{incomplete, Tail2} ->
	    {H, Tail2};
	{line, Line, Tail2} ->
	    get_headers(H ++ parse_line(Line), Tail2);
	{lastline, Line, Tail2} ->
	    {H ++ parse_line(Line) ++ [{ok, http_eoh}], Tail2}
    end.


parse_line("Connection:" ++ Con) ->
    [{ok, {http_header,  undefined, 'Connection', undefined, strip_spaces(Con)}}];
parse_line("Host:" ++ Con) ->
    [{ok, {http_header,  undefined, 'Host', undefined, strip_spaces(Con)}}];
parse_line("Accept:" ++ Con) ->
    [{ok, {http_header,  undefined, 'Accept', undefined, strip_spaces(Con)}}];
parse_line("If-Modified-Since:" ++ Con) ->
    [{ok, {http_header,  undefined, 'If-Modified-Since', undefined, strip_spaces(Con)}}];
parse_line("If-Match:" ++ Con) ->
    [{ok, {http_header,  undefined, 'If-Match', undefined, strip_spaces(Con)}}];
parse_line("If-None-Match:" ++ Con) ->
    [{ok, {http_header,  undefined, 'If-None-Match', undefined, strip_spaces(Con)}}];
parse_line("If-Range:" ++ Con) ->
    [{ok, {http_header,  undefined, 'If-Range', undefined, strip_spaces(Con)}}];
parse_line("If-Unmodified-Since:" ++ Con) ->
    [{ok, {http_header,  undefined, 'If-Unmodified-Since', undefined, strip_spaces(Con)}}];
parse_line("Range:" ++ Con) ->
    [{ok, {http_header,  undefined, 'Range', undefined, strip_spaces(Con)}}];
parse_line("User-Agent:" ++ Con) ->
    [{ok, {http_header,  undefined, 'User-Agent', undefined, strip_spaces(Con)}}];
parse_line("Accept-Ranges:" ++ Con) ->
    [{ok, {http_header,  undefined, 'Accept-Ranges', undefined, strip_spaces(Con)}}];
parse_line("Authorization:" ++ Con) ->
    [{ok, {http_header,  undefined, 'Authorization', undefined, strip_spaces(Con)}}];
parse_line("Keep-Alive:" ++ Con) ->
    [{ok, {http_header,  undefined, 'Keep-Alive', undefined, strip_spaces(Con)}}];
parse_line("Referer:" ++ Con) ->
    [{ok, {http_header,  undefined, 'Referer', undefined, strip_spaces(Con)}}];
parse_line("Content-type:"++Con) ->
    [{ok, {http_header,  undefined, 'Content-Type', undefined, strip_spaces(Con)}}];
parse_line("Content-Type:"++Con) ->
    [{ok, {http_header,  undefined, 'Content-Type', undefined, strip_spaces(Con)}}];
parse_line("Content-Length:"++Con) ->
    [{ok, {http_header,  undefined, 'Content-Length', undefined, strip_spaces(Con)}}];
parse_line("Content-length:"++Con) ->
    [{ok, {http_header,  undefined, 'Content-Length', undefined, strip_spaces(Con)}}];
parse_line("Cookie:"++Con) ->
    [{ok, {http_header,  undefined, 'Cookie', undefined, strip_spaces(Con)}}];
parse_line("Accept-Language:"++Con) ->
    [{ok, {http_header,  undefined, 'Accept-Language', undefined, strip_spaces(Con)}}];
parse_line("Accept-Encoding:"++Con) ->
    [{ok, {http_header,  undefined, 'Accept-Encoding', undefined, strip_spaces(Con)}}];
parse_line(S) ->
    case lists:splitwith(fun(C)->C /= $: end, S) of
	{Name, [$:|Val]} ->
	    [{ok, {http_header,  undefined, Name, undefined, strip_spaces(Val)}}];
	_ ->
	    []
    end.


is_space($\s) ->
    true;
is_space($\r) ->
    true;
is_space($\n) ->
    true;
is_space($\t) ->
    true;
is_space(_) ->
    false.


strip_spaces(String) ->
    strip_spaces(String, both).

strip_spaces(String, left) ->
    drop_spaces(String);
strip_spaces(String, right) ->
    lists:reverse(drop_spaces(lists:reverse(String)));
strip_spaces(String, both) ->
    strip_spaces(drop_spaces(String), right).

drop_spaces([]) ->
    [];
drop_spaces(YS=[X|XS]) ->
    case is_space(X) of
	true ->
	    drop_spaces(XS);
	false ->
	    YS
    end.

is_nb_space(X) ->
    lists:member(X, [$\s, $\t]).
    

% ret: {line, Line, Trail} | {lastline, Line, Trail}

get_line(L) ->    
    get_line(L, []).
get_line("\r\n\r\n" ++ Tail, Cur) ->
    {lastline, lists:reverse(Cur), Tail};
get_line("\r\n" ++ Tail, Cur) ->
    case Tail of
	[] ->
	    {incomplete, lists:reverse(Cur) ++ "\r\n"};
	_ ->
	    case is_nb_space(hd(Tail)) of
		true ->  %% multiline ... continue 
		    get_line(Tail, [$\n, $\r | Cur]);
		false ->
		    {line, lists:reverse(Cur), Tail}
	    end
    end;
get_line([H|T], Cur) ->
    get_line(T, [H|Cur]).

