%% Copyright (C) 2003 Joakim Grebenö <jocke@gleipnir.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(xmlrpc_http).
-author('jocke@gleipnir.com').
-export([handler/4]).

-include("log.hrl").

-include("xmlrpc.hrl").

%% Exported: handler/4

handler(Socket, Timeout, Handler, State) ->
    case parse_request(Socket, Timeout) of
	{ok, Header} ->
	    ?DEBUG_LOG({header, Header}),
	    handle_payload(Socket, Timeout, Handler, State, Header);
	{status, StatusCode} ->
	    send(Socket, StatusCode),
	    handler(Socket, Timeout, Handler, State);
	{error, Reason} -> {error, Reason}
    end.

parse_request(Socket, Timeout) ->
    inet:setopts(Socket, [{packet, line}]),
    case gen_tcp:recv(Socket, 0, Timeout) of
	{ok, RequestLine} ->
	    case string:tokens(RequestLine, " \r\n") of
		["POST", _, "HTTP/1.0"] ->
		    ?DEBUG_LOG({http_version, "1.0"}),
		    parse_header(Socket, Timeout, #header{connection = close});
		["POST", _, "HTTP/1.1"] ->
		    ?DEBUG_LOG({http_version, "1.1"}),
		    parse_header(Socket, Timeout);
		[_Method, _, "HTTP/1.1"] -> {status, 501};
		["POST", _, _HTTPVersion] -> {status, 505};
		_ -> {status, 400}
	    end;
	{error, Reason} -> {error, Reason}
    end.

parse_header(Socket, Timeout) -> parse_header(Socket, Timeout, #header{}).

parse_header(Socket, Timeout, Header) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
	{ok, "\r\n"} when Header#header.content_length == undefined ->
	    {status, 411};
	{ok, "\r\n"} when Header#header.content_type == undefined ->
	    {status, 400};
	{ok, "\r\n"} when Header#header.user_agent == undefined ->
	    {status, 400};
	{ok, "\r\n"} -> {ok, Header};
	{ok, HeaderField} ->
	    case split_header_field(HeaderField) of
		{[$C,$o,$n,$t,$e,$n,$t,$-,_,$e,$n,$g,$t,$h,$:],
		 ContentLength} ->
		    case catch list_to_integer(ContentLength) of
			N ->
			    parse_header(Socket, Timeout,
					 Header#header{content_length = N});
			_ -> {status, 400}
			  end;
		{"Content-Type:", "text/xml"} ->
		    parse_header(Socket, Timeout,
				 Header#header{content_type = "text/xml"});
		{"Content-Type:", "text/xml; charset=utf-8"} ->
		    parse_header(Socket, Timeout,
				 Header#header{content_type = "text/xml; charset=utf-8"});
		{"Content-Type:", _ContentType} -> {status, 415};
		{"User-Agent:", UserAgent} ->
		    parse_header(Socket, Timeout,
				 Header#header{user_agent = UserAgent});
		{"Connection:", "close"} ->
		    parse_header(Socket, Timeout,
				 Header#header{connection = close});
		{"Connection:", [_,$e,$e,$p,$-,_,$l,$i,$v,$e]} ->
		    parse_header(Socket, Timeout,
				 Header#header{connection = undefined});
		{"Authorization:", Authorization} ->
		    parse_header(Socket, Timeout,
				 Header#header{authorization = Authorization});
		{"Cookie:", Cookie} ->
			Cookies = [ Cookie | Header#header.cookies ],
		    parse_header(Socket, Timeout,
				 Header#header{cookies = Cookies});
		_ ->
		    ?DEBUG_LOG({skipped_header, HeaderField}),
		    parse_header(Socket, Timeout, Header)
	    end;
	{error, Reason} -> {error, Reason}
    end.

split_header_field(HeaderField) -> split_header_field(HeaderField, []).

split_header_field([], Name) -> {Name, ""};
split_header_field([$ |Rest], Name) -> {lists:reverse(Name), Rest -- "\r\n"};
split_header_field([C|Rest], Name) -> split_header_field(Rest, [C|Name]).

handle_payload(Socket, Timeout, Handler, State,
	       #header{connection = Connection} = Header) ->
    case get_payload(Socket, Timeout, Header#header.content_length) of
	{ok, Payload} ->
	    ?DEBUG_LOG({encoded_call, Payload}),
	    case xmlrpc_decode:payload(Payload) of
		{ok, DecodedPayload} ->
		    ?DEBUG_LOG({decoded_call, DecodedPayload}),
		    eval_payload(Socket, Timeout, Handler, State, Connection,
				 DecodedPayload, Header);
		{error, Reason} when Connection == close ->
		    ?ERROR_LOG({xmlrpc_decode, payload, Payload, Reason}),
		    send(Socket, 400);
		{error, Reason} ->
		    ?ERROR_LOG({xmlrpc_decode, payload, Payload, Reason}),
		    send(Socket, 400),
		    handler(Socket, Timeout, Handler, State)
	    end;
	{error, Reason} -> {error, Reason}
    end.

get_payload(Socket, Timeout, ContentLength) ->
    inet:setopts(Socket, [{packet, raw}]),
    gen_tcp:recv(Socket, ContentLength, Timeout).

%% Check whether module has defined new function
%% M:F(State, Payload, Header)
has_newcall(M, F) ->
    erlang:function_exported(M, F, 3).

%% Handle module call
do_call({M, F} = _Handler, State, Payload, Header) ->
	case has_newcall(M, F) of
		true ->
			M:F(State, Payload, Header);
		false ->
			M:F(State, Payload)
	end.

eval_payload(Socket, Timeout, {M, F} = Handler, State, Connection, Payload, Header) ->
    case catch do_call(Handler, State, Payload, Header) of
	{'EXIT', Reason} when Connection == close ->
	    ?ERROR_LOG({M, F, {'EXIT', Reason}}),
	    send(Socket, 500, "Connection: close\r\n");
	{'EXIT', Reason} ->
	    ?ERROR_LOG({M, F, {'EXIT', Reason}}),
	    send(Socket, 500),
	    handler(Socket, Timeout, Handler, State);
	{error, Reason} when Connection == close ->
	    ?ERROR_LOG({M, F, Reason}),
	    send(Socket, 500, "Connection: close\r\n");
	{error, Reason} ->
	    ?ERROR_LOG({M, F, Reason}),
	    send(Socket, 500),
	    handler(Socket, Timeout, Handler, State);
	{false, ResponsePayload} ->
	    encode_send(Socket, 200, "Connection: close\r\n", ResponsePayload);
	{false, ResponsePayload, ExtraHeaders} ->
        encode_send(Socket, 200, [ExtraHeaders, "Connection: close\r\n"], ResponsePayload);
	{true, _NewTimeout, _NewState, ResponsePayload} when
	      Connection == close ->
	    encode_send(Socket, 200, "Connection: close\r\n", ResponsePayload);
	{true, NewTimeout, NewState, ResponsePayload} ->
	    encode_send(Socket, 200, "", ResponsePayload),
	    handler(Socket, NewTimeout, Handler, NewState);
	{true, _NewTimeout, _NewState, ResponsePayload, ExtraHeaders} when
	      Connection == close ->
	    encode_send(Socket, 200, [ExtraHeaders, "Connection: close\r\n"], ResponsePayload);
	{true, NewTimeout, NewState, ResponsePayload, ExtraHeaders} ->
	    encode_send(Socket, 200, ExtraHeaders, ResponsePayload),
	    handler(Socket, NewTimeout, Handler, NewState)
    end.

encode_send(Socket, StatusCode, ExtraHeader, Payload) ->
    ?DEBUG_LOG({decoded_response, Payload}),
    case xmlrpc_encode:payload(Payload) of
	{ok, EncodedPayload} ->
	    ?DEBUG_LOG({encoded_response, lists:flatten(EncodedPayload)}),
	    send(Socket, StatusCode, ExtraHeader, EncodedPayload);
	{error, Reason} ->
	    ?ERROR_LOG({xmlrpc_encode, payload, Payload, Reason}),
	    send(Socket, 500)
    end.

send(Socket, StatusCode) -> send(Socket, StatusCode, "", "").

send(Socket, StatusCode, ExtraHeader) ->
    send(Socket, StatusCode, ExtraHeader, "").

send(Socket, StatusCode, ExtraHeader, Payload) ->
    Response =
	["HTTP/1.1 ", integer_to_list(StatusCode), " ",
	 reason_phrase(StatusCode), "\r\n",
	 "Content-Length: ", integer_to_list(lists:flatlength(Payload)),
	 "\r\n",
	 "Server: Erlang/1.13\r\n",
	 "Content-Type: text/xml\r\n",
	 ExtraHeader, "\r\n",
	 Payload],
    gen_tcp:send(Socket, Response).

reason_phrase(200) -> "OK";
reason_phrase(400) -> "Bad Request";
reason_phrase(411) -> "Length required";
reason_phrase(415) -> "Unsupported Media Type";
reason_phrase(500) -> "Internal Server Error";
reason_phrase(501) -> "Not Implemented";
reason_phrase(505) -> "HTTP Version not supported".
