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
	 receive_headers/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

-record(state, {sockmod,
		socket,
		request_method,
		request_path,
		request_auth,
		request_content_length
	       }).


-define(XHTML_DOCTYPE,
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
	"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n").


start(SockData, Opts) ->
    supervisor:start_child(ejabberd_http_sup, [SockData, Opts]).

start_link({SockMod, Socket}, Opts) ->
    ?INFO_MSG("started: ~p", [{SockMod, Socket}]),
    case SockMod of
	gen_tcp ->
	    inet:setopts(Socket, [{packet, http}]);
	ssl ->
	    ssl:setopts(Socket, [{packet, http}])
    end,
    {ok, proc_lib:spawn_link(ejabberd_http,
			     receive_headers,
			     [#state{sockmod = SockMod, socket = Socket}])}.


send_text(State, Text) ->
    (State#state.sockmod):send(State#state.socket, Text).


receive_headers(State) ->
    Data = (State#state.sockmod):recv(State#state.socket, 0, 300000),
    ?DEBUG("recv: ~p~n", [Data]),
    case Data of
	{ok, {http_request, Method, Path, _Version}} ->
	    receive_headers(State#state{request_method = Method,
					request_path = Path});
	{ok, {http_header, _, 'Authorization', _, Auth}} ->
	    receive_headers(State#state{request_auth = parse_auth(Auth)});
	{ok, {http_header, _, 'Content-Length', _, SLen}} ->
	    case catch list_to_integer(SLen) of
		Len when is_integer(Len) ->
		    receive_headers(State#state{request_content_length = Len});
		_ ->
		    receive_headers(State)
	    end;
	{ok, {http_header, _, _, _, _}} ->
	    receive_headers(State);
	{ok, http_eoh} ->
	    ?INFO_MSG("(~w) http query: ~w ~s~n",
		      [State#state.socket,
		       State#state.request_method,
		       element(2, State#state.request_path)]),
	    Out = process_request(State),
	    send_text(State, Out),
	    ok;
	{error, _Reason} ->
	    ok;
	_ ->
	    ok
    end.



process_request(#state{request_method = 'GET',
		       request_path = {abs_path, Path},
		       request_auth = Auth}) ->
    User = case Auth of
	       {U, P} ->
		   case ejabberd_auth:check_password(U, P) of
		       true ->
			   U;
		       false ->
			   unauthorized
		   end;
	       _ ->
		   undefined
	   end,
    case User of
	unauthorized ->
	    make_xhtml_output(
	      401,
	      [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
	      ejabberd_web:make_xhtml([{xmlelement, "h1", [],
					[{xmlcdata, "401 Unauthorized"}]}]));
	_ ->
	    case (catch url_decode_q_split(Path)) of
		{'EXIT', _} ->
		    process_request(false);
		{NPath, Query} ->
		    LQuery = parse_urlencoded(Query),
		    LPath = string:tokens(NPath, "/"),
		    Request = #request{method = 'GET',
				       path = LPath,
				       q = LQuery,
				       user = User},
		    case ejabberd_web:process_get(Request) of
			El when element(1, El) == xmlelement ->
			    make_xhtml_output(200, [], El);
			{Status, Headers, El} when
			      element(1, El) == xmlelement ->
			    make_xhtml_output(Status, Headers, El);
			Text when is_list(Text) ->
			    make_text_output(200, [], Text);
			{Status, Headers, Text} when
			      is_list(Text) ->
			    make_text_output(Status, Headers, Text)
		    end
	    end
    end;

process_request(#state{request_method = 'POST',
		       request_path = {abs_path, Path},
		       request_auth = Auth,
		       request_content_length = Len,
		       sockmod = SockMod,
		       socket = Socket} = State) when is_integer(Len) ->
    User = case Auth of
	       {U, P} ->
		   case ejabberd_auth:check_password(U, P) of
		       true ->
			   U;
		       false ->
			   unauthorized
		   end;
	       _ ->
		   undefined
	   end,
    case User of
	unauthorized ->
	    make_xhtml_output(
	      401,
	      [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],
	      ejabberd_web:make_xhtml([{xmlelement, "h1", [],
					[{xmlcdata, "401 Unauthorized"}]}]));
	_ ->
    	    case SockMod of
		gen_tcp ->
		    inet:setopts(Socket, [{packet, 0}]);
		ssl ->
		    ssl:setopts(Socket, [{packet, 0}])
	    end,
	    Data = recv_data(State, Len),
	    ?DEBUG("client data: ~p~n", [Data]),
	    case (catch url_decode_q_split(Path)) of
		{'EXIT', _} ->
		    process_request(false);
		{NPath, Query} ->
		    LPath = string:tokens(NPath, "/"),
		    LQuery = parse_urlencoded(Data),
		    Request = #request{method = 'POST',
				       path = LPath,
				       q = LQuery,
				       user = User,
				       data = Data},
		    case ejabberd_web:process_get(Request) of
			El when element(1, El) == xmlelement ->
			    make_xhtml_output(200, [], El);
			{Status, Headers, El} when
			      element(1, El) == xmlelement ->
			    make_xhtml_output(Status, Headers, El);
			Text when is_list(Text) ->
			    make_text_output(200, [], Text);
			{Status, Headers, Text} when
			      is_list(Text) ->
			    make_text_output(Status, Headers, Text)
		    end
	    end
    end;

process_request(State) ->
    make_xhtml_output(
      400,
      [],
      ejabberd_web:make_xhtml([{xmlelement, "h1", [],
				[{xmlcdata, "400 Bad Request"}]}])).


recv_data(State, Len) ->
    recv_data(State, Len, []).

recv_data(State, 0, Acc) ->
    binary_to_list(list_to_binary(Acc));
recv_data(State, Len, Acc) ->
    case (State#state.sockmod):recv(State#state.socket, Len, 300000) of
	{ok, Data} ->
	    recv_data(State, Len - size(Data), [Acc | Data]);
	_ ->
	    ""
    end.


make_xhtml_output(Status, Headers, XHTML) ->
    Data = list_to_binary([?XHTML_DOCTYPE, xml:element_to_string(XHTML)]),
    Headers1 = case lists:keysearch("Content-Type", 1, Headers) of
		   {value, _} ->
		       [{"Content-Length", integer_to_list(size(Data))} |
			Headers];
		   _ ->
		       [{"Content-Type", "text/html; charset=utf-8"},
			{"Content-Length", integer_to_list(size(Data))} |
			Headers]
	       end,
    H = lists:map(fun({Attr, Val}) ->
			  [Attr, ": ", Val, "\r\n"]
		  end, Headers1),
    SL = ["HTTP/1.1 ", integer_to_list(Status), " ",
	  code_to_phrase(Status), "\r\n"],
    [SL, H, "\r\n", Data].

make_text_output(Status, Headers, Text) ->
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
    H = lists:map(fun({Attr, Val}) ->
			  [Attr, ": ", Val, "\r\n"]
		  end, Headers1),
    SL = ["HTTP/1.1 ", integer_to_list(Status), " ",
	  code_to_phrase(Status), "\r\n"],
    [SL, H, "\r\n", Data].
    


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


