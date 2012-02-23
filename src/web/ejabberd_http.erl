%%%----------------------------------------------------------------------
%%% File    : ejabberd_http.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 27 Feb 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_http).
-author('alexey@process-one.net').

%% External exports
-export([start/2,
	 start_link/2,
	 become_controller/1,
	 socket_type/0,
	 receive_headers/1,
	 url_encode/1]).

%% Callbacks
-export([init/2]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("ejabberd_http.hrl").

%% @type request() = term()
%% @type query() = list()

-record(state, {sockmod,
		socket,
		request_method,
		request_version,
		request_path,
		request_auth,
		request_keepalive,
		request_content_length,
		request_lang = <<"en">>,
		%% XXX bard: request handlers are configured in
		%% ejabberd.cfg under the HTTP service.	 For example,
		%% to have the module test_web handle requests with
		%% paths starting with "/test/module":
		%%
		%%   {5280, ejabberd_http,    [http_poll, web_admin,
		%%			       {request_handlers, [{["test", "module"], mod_test_web}]}]}
		%%
		request_handlers = [],
		request_host,
		request_port,
		request_tp,
		request_headers = [],
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

start_link(SockData, Opts) ->
    {ok, proc_lib:spawn_link(ejabberd_http, init, [SockData, Opts])}.

init({SockMod, Socket}, Opts) ->
    TLSEnabled = lists:member(tls, Opts),
    TLSOpts1 = lists:filter(fun({certfile, _}) -> true;
			      (_) -> false
			   end, Opts),
    TLSOpts = [verify_none | TLSOpts1],
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
	    inet:setopts(Socket1, [list, {packet, http}, {recbuf, 8192}]);
	_ ->
	    ok
    end,

    %% XXX bard: for backward compatibility, expand in Opts:
    %%  web_admin -> {["admin"], ejabberd_web_admin}
    %%  http_bind -> {["http-bind"], mod_http_bind}
    %%  http_poll -> {["http-poll"], ejabberd_http_poll}
    %%  register -> {["register"], mod_register_web}

    RequestHandlers =
	case lists:keysearch(request_handlers, 1, Opts) of
	    {value, {request_handlers, H}} -> H;
	    false -> []
        end ++
	case lists:member(captcha, Opts) of
            true -> [{["captcha"], ejabberd_captcha}];
            false -> []
        end ++
        case lists:member(register, Opts) of
            true -> [{["register"], mod_register_web}];
            false -> []
        end ++
        case lists:member(web_admin, Opts) of
            true -> [{["admin"], ejabberd_web_admin}];
            false -> []
        end ++
        case lists:member(http_bind, Opts) of
            true -> [{["http-bind"], mod_http_bind}];
            false -> []
        end ++
        case lists:member(http_poll, Opts) of
            true -> [{["http-poll"], ejabberd_http_poll}];
            false -> []
        end,
    ?DEBUG("S: ~p~n", [RequestHandlers]),

    ?INFO_MSG("started: ~p", [{SockMod1, Socket1}]),
    State = #state{sockmod = SockMod1,
                   socket = Socket1,
                   request_handlers = RequestHandlers},
    receive_headers(State).


become_controller(_Pid) ->
    ok.

socket_type() ->
    raw.

send_text(State, Text) ->
    case catch (State#state.sockmod):send(State#state.socket, Text) of
        ok -> ok;
	{error, timeout} ->
	    ?INFO_MSG("Timeout on ~p:send",[State#state.sockmod]),
	    exit(normal);
        Error ->
	    ?DEBUG("Error in ~p:send: ~p",[State#state.sockmod, Error]),
	    exit(normal)
    end.

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
	{ok, {http_request, Method, Uri, Version}} ->
	    KeepAlive = case Version of
		{1, 1} ->
		    true;
		_ ->
		    false
	    end,
	    Path = case Uri of
	        {absoluteURI, _Scheme, _Host, _Port, P} -> {abs_path, P};
	        _ -> Uri
	    end,
	    State#state{request_method = Method,
			request_version = Version,
			request_path = Path,
			request_keepalive = KeepAlive};
	{ok, {http_header, _, 'Connection'=Name, _, Conn}} ->
	    KeepAlive1 = case exmpp_stringprep:to_lower(Conn) of
			     "keep-alive" ->
				 true;
			     "close" ->
				 false;
			     _ ->
				 State#state.request_keepalive
			 end,
	    State#state{request_keepalive = KeepAlive1,
			request_headers=add_header(Name, Conn, State)};
	{ok, {http_header, _, 'Authorization'=Name, _, Auth}} ->
	    State#state{request_auth = parse_auth(Auth),
			request_headers=add_header(Name, Auth, State)};
	{ok, {http_header, _, 'Content-Length'=Name, _, SLen}} ->
	    case catch list_to_integer(SLen) of
		Len when is_integer(Len) ->
		    State#state{request_content_length = Len,
				request_headers=add_header(Name, SLen, State)};
		_ ->
		    State
	    end;
	{ok, {http_header, _, 'Accept-Language'=Name, _, Langs}} ->
	    State#state{request_lang = parse_lang(Langs),
			request_headers=add_header(Name, Langs, State)};
	{ok, {http_header, _, 'Host'=Name, _, Host}} ->
	    State#state{request_host = Host,
			request_headers=add_header(Name, Host, State)};
	{ok, {http_header, _, Name, _, Value}} ->
	    State#state{request_headers=add_header(Name, Value, State)};
	{ok, http_eoh} when State#state.request_host == undefined ->
	    ?WARNING_MSG("An HTTP request without 'Host' HTTP header was received.", []),
	    throw(http_request_no_host_header);
	{ok, http_eoh} ->
	    ?DEBUG("(~w) http query: ~w ~s~n",
		   [State#state.socket,
		    State#state.request_method,
		    element(2, State#state.request_path)]),
	    {Host, Port, TP} = get_transfer_protocol(SockMod,
						     State#state.request_host),
	    State2 = State#state{request_host = Host,
				 request_port = Port,
				 request_tp = TP},
	    Out = process_request(State2),
	    send_text(State2, Out),
	    case State2#state.request_keepalive of
		true ->
		    case SockMod of
			gen_tcp ->
			    inet:setopts(Socket, [{packet, http}]);
			_ ->
			    ok
		    end,
		    #state{sockmod = SockMod,
			   socket = Socket,
			   request_handlers = State#state.request_handlers};
		_ ->
		    #state{end_of_request = true,
			   request_handlers = State#state.request_handlers}
	    end;
	{error, _Reason} ->
	    #state{end_of_request = true,
		   request_handlers = State#state.request_handlers};
	_ ->
	    #state{end_of_request = true,
		   request_handlers = State#state.request_handlers}
    end.

add_header(Name, Value, State) ->
    [{Name, Value} | State#state.request_headers].

%% @spec (SockMod, HostPort) -> {Host::string(), Port::integer(), TP}
%% where
%%       SockMod = gen_tcp | tls
%%       HostPort = string()
%%       TP = http | https
%% @doc Given a socket and hostport header, return data of transfer protocol.
%% Note that HostPort can be a string of a host like "example.org",
%% or a string of a host and port like "example.org:5280".
get_transfer_protocol(SockMod, HostPort) ->
    [Host | PortList] = string:tokens(HostPort, ":"),
    case {SockMod, PortList} of
	{gen_tcp, []} ->
	    {Host, 80, http};
	{gen_tcp, [Port]} ->
	    {Host, list_to_integer(Port), http};
	{tls, []} ->
	    {Host, 443, https};
	{tls, [Port]} ->
	    {Host, list_to_integer(Port), https}
    end.

%% XXX bard: search through request handlers looking for one that
%% matches the requested URL path, and pass control to it.  If none is
%% found, answer with HTTP 404.
process([], _) ->
    ejabberd_web:error(not_found);
process(Handlers, Request) ->
    [{HandlerPathPrefix, HandlerModule} | HandlersLeft] = Handlers,

    case (lists:prefix(HandlerPathPrefix, Request#request.path) or
         (HandlerPathPrefix==Request#request.path)) of
	true ->
            ?DEBUG("~p matches ~p", [Request#request.path, HandlerPathPrefix]),
            %% LocalPath is the path "local to the handler", i.e. if
            %% the handler was registered to handle "/test/" and the
            %% requested path is "/test/foo/bar", the local path is
            %% ["foo", "bar"]
            LocalPath = lists:nthtail(length(HandlerPathPrefix), Request#request.path),
	        R = HandlerModule:process(LocalPath, Request),
            ejabberd_hooks:run(http_request_debug, [{LocalPath, Request}]),
            R;
	false ->
	    process(HandlersLeft, Request)
    end.

process_request(#state{request_method = Method,
		       request_path = {abs_path, Path},
		       request_auth = Auth,
		       request_lang = Lang,
		       request_handlers = RequestHandlers,
		       request_host = Host,
		       request_port = Port,
		       request_tp = TP,
		       request_headers = RequestHeaders,
		       sockmod = SockMod,
		       socket = Socket} = State)
  when Method=:='GET' orelse Method=:='HEAD' orelse Method=:='DELETE' orelse Method=:='OPTIONS' ->
    case (catch url_decode_q_split(Path)) of
	{'EXIT', Error} ->
	    throw({error_decoding_url, Path, Error});
	{NPath, Query} ->
	    LPath = [path_decode(NPE) || NPE <- string:tokens(NPath, "/")],
	    LQuery = case (catch parse_urlencoded(Query)) of
			 {'EXIT', _Reason} ->
			     [];
			 LQ ->
			     LQ
		     end,
	    {ok, IPHere} =
		case SockMod of
		    gen_tcp ->
			inet:peername(Socket);
		    _ ->
			SockMod:peername(Socket)
		end,
	    XFF = proplists:get_value('X-Forwarded-For', RequestHeaders, []),
	    IP = analyze_ip_xff(IPHere, XFF, Host),
	    Request = #request{method = Method,
			       path = LPath,
			       q = LQuery,
			       auth = Auth,
			       lang = Lang,
			       host = Host,
			       port = Port,
			       tp = TP,
			       headers = RequestHeaders,
			       ip = IP},
	    %% XXX bard: This previously passed control to
	    %% ejabberd_web:process_get, now passes it to a local
	    %% procedure (process) that handles dispatching based on
	    %% URL path prefix.
	    case process(RequestHandlers, Request) of
		El when is_record(El, xmlel) ->
		    make_xhtml_output(State, 200, [], El);
		{Status, Headers, El} when is_record(El, xmlel) ->
		    make_xhtml_output(State, Status, Headers, El);
		Output when is_list(Output) or is_binary(Output) ->
		    make_text_output(State, 200, [], Output);
		{Status, Headers, Output} when is_list(Output) or is_binary(Output) ->
		    make_text_output(State, Status, Headers, Output)
	    end
    end;

process_request(#state{request_method = Method,
		       request_path = {abs_path, Path},
		       request_auth = Auth,
		       request_content_length = Len,
		       request_lang = Lang,
		       sockmod = SockMod,
		       socket = Socket,
		       request_host = Host,
		       request_port = Port,
		       request_tp = TP,
		       request_headers = RequestHeaders,
		       request_handlers = RequestHandlers} = State)
  when (Method=:='POST' orelse Method=:='PUT') andalso is_integer(Len) ->
    {ok, IPHere} =
	case SockMod of
	    gen_tcp ->
		inet:peername(Socket);
	    _ ->
		SockMod:peername(Socket)
	end,
    XFF = proplists:get_value('X-Forwarded-For', RequestHeaders, []),
    IP = analyze_ip_xff(IPHere, XFF, Host),
    case SockMod of
	gen_tcp ->
	    inet:setopts(Socket, [{packet, 0}]);
	_ ->
	    ok
    end,
    Data = recv_data(State, Len),
    ?DEBUG("client data: ~p~n", [Data]),
    case (catch url_decode_q_split(Path)) of
	{'EXIT', Error} ->
	    throw({error_decoding_url, Path, Error});
	{NPath, _Query} ->
	    LPath = [path_decode(NPE) || NPE <- string:tokens(NPath, "/")],
	    LQuery = case (catch parse_urlencoded(Data)) of
			 {'EXIT', _Reason} ->
			     [];
			 LQ ->
			     LQ
		     end,
	    Request = #request{method = Method,
			       path = LPath,
			       q = LQuery,
			       auth = Auth,
			       data = Data,
			       lang = Lang,
			       host = Host,
			       port = Port,
			       tp = TP,
			       headers = RequestHeaders,
			       ip = IP},
	    case process(RequestHandlers, Request) of
		El when is_record(El, xmlel) ->
		    make_xhtml_output(State, 200, [], El);
		{Status, Headers, El} when is_record(El, xmlel) ->
		    make_xhtml_output(State, Status, Headers, El);
		Output when is_list(Output) or is_binary(Output) ->
		    make_text_output(State, 200, [], Output);
		{Status, Headers, Output} when is_list(Output) or is_binary(Output) ->
		    make_text_output(State, Status, Headers, Output)
	    end
    end;

process_request(State) ->
    make_xhtml_output(State,
      400,
      [],
      ejabberd_web:make_xhtml([#xmlel{ns = ?NS_XHTML, name = 'h1', children =
				[#xmlcdata{cdata = <<"400 Bad Request">>}]}])).

%% Support for X-Forwarded-From
analyze_ip_xff(IP, [], _Host) ->
    IP;
analyze_ip_xff({IPLast, Port}, XFF, Host) ->
    [ClientIP | ProxiesIPs] = string:tokens(XFF, ", ")
	++ [inet_parse:ntoa(IPLast)],
    TrustedProxies = case ejabberd_config:get_local_option(
			    {trusted_proxies, Host}) of
			 undefined -> [];
			 TPs -> TPs
		     end,
    IPClient = case is_ipchain_trusted(ProxiesIPs, TrustedProxies) of
		   true ->
		       {ok, IPFirst} = inet_parse:address(ClientIP),
		       ?DEBUG("The IP ~w was replaced with ~w due to header "
			      "X-Forwarded-For: ~s", [IPLast, IPFirst, XFF]),
		       IPFirst;
		   false ->
		       IPLast
	       end,
    {IPClient, Port}.
is_ipchain_trusted(_UserIPs, all) ->
    true;
is_ipchain_trusted(UserIPs, TrustedIPs) ->
    [] == UserIPs -- ["127.0.0.1" | TrustedIPs].

recv_data(State, Len) ->
    recv_data(State, Len, []).

recv_data(_State, 0, Acc) ->
    binary_to_list(list_to_binary(Acc));
recv_data(State, Len, Acc) ->
    case State#state.trail of
	[] ->
	    case (State#state.sockmod):recv(State#state.socket,   Len, 300000) of
		{ok, Binary} when is_binary(Binary) ->
		    Data = binary_to_list(Binary),
		    recv_data(State, Len - length(Data), Acc ++ Data);
		{ok, Data} ->
		    recv_data(State, Len - length(Data), Acc ++ Data);
		_ ->
		    ""
	    end;
	_ ->
	    Trail = State#state.trail,
	    recv_data(State#state{trail = ""}, Len - length(Trail), Acc ++ Trail)
    end.


make_xhtml_output(State, Status, Headers, XHTML) ->
    Data = case lists:member(html, Headers) of
	       true ->
		   list_to_binary([?HTML_DOCTYPE,
				   exmpp_xml:document_to_list(XHTML)]);
	       _ ->
		   list_to_binary([?XHTML_DOCTYPE,
				   exmpp_xml:document_to_list(XHTML)])
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

    Data2 = case State#state.request_method of
		  'HEAD' -> "";
		  _ -> Data
	      end,

    [SL, H, "\r\n", Data2].

make_text_output(State, Status, Headers, Text) when is_list(Text) ->
    make_text_output(State, Status, Headers, list_to_binary(Text));

make_text_output(State, Status, Headers, Data) when is_binary(Data) ->
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

    Data2 = case State#state.request_method of
		  'HEAD' -> "";
		  _ -> Data
	      end,

    [SL, H, "\r\n", Data2].


parse_lang(Langs) ->
    case string:tokens(Langs, ",; ") of
	[First | _] ->
	    list_to_binary(First);
	[] ->
	    <<"en">>
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


%% @doc Split the URL and return {Path, QueryPart}
url_decode_q_split(Path) ->
    url_decode_q_split(Path, []).
url_decode_q_split([$?|T], Ack) ->
    %% Don't decode the query string here, that is parsed separately.
    {path_norm_reverse(Ack), T};
url_decode_q_split([H|T], Ack) when H /= 0 ->
    url_decode_q_split(T, [H|Ack]);
url_decode_q_split([], Ack) ->
    {path_norm_reverse(Ack), []}.

%% @doc Decode a part of the URL and return string()
path_decode(Path) ->
    path_decode(Path, []).
path_decode([$%, Hi, Lo | Tail], Ack) ->
    Hex = hex_to_integer([Hi, Lo]),
    if Hex  == 0 -> exit(badurl);
       true -> ok
    end,
    path_decode(Tail, [Hex|Ack]);
path_decode([H|T], Ack) when H /= 0 ->
    path_decode(T, [H|Ack]);
path_decode([], Ack) ->
    lists:reverse(Ack).

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
rest_dir (N, Path, [  _H | T ] ) -> rest_dir (N    ,        Path  , T).

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


parse_auth(_Orig = "Basic " ++ Auth64) ->
    case decode_base64(Auth64) of
	{error, _Err} ->
	    undefined;
	Auth ->
	    %% Auth should be a string with the format: user@server:password
	    %% Note that password can contain additional characters '@' and ':'
	    case string:chr(Auth, $:) of
		0 ->
		    undefined;
		SplitIndex ->
		    {User, [$: | Pass]} = lists:split(SplitIndex-1, Auth),
		    {User, Pass}
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
			       _ ->
				   case string:str(URI, "://") of
				       0 ->
				           % Relative URI
				           % ex: /index.html
				           {abs_path, URI};
				       N ->
				           % Absolute URI
				           % ex: http://localhost/index.html

				           % Remove scheme
				           % ex: URI2 = localhost/index.html
				           URI2 = string:substr(URI, N + 3),
				           % Look for the start of the path
				           % (or the lack of a path thereof)
				           case string:chr(URI2, $/) of
				               0 -> {abs_path, "/"};
				               M -> {abs_path,
				                   string:substr(URI2, M + 1)}
				           end
				   end
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

%%strip_spaces(String, left) ->
%%    drop_spaces(String);
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
