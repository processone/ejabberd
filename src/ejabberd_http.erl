%%%----------------------------------------------------------------------
%%% File    : ejabberd_http.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 27 Feb 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
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

-module(ejabberd_http).

-author('alexey@process-one.net').

%% External exports
-export([start/2, start_link/2, become_controller/1,
	 socket_type/0, receive_headers/1, url_encode/1,
         transform_listen_option/2]).

%% Callbacks
-export([init/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

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
		default_host,
		trail = <<>>
	       }).

-define(XHTML_DOCTYPE,
	<<"<?xml version='1.0'?>\n<!DOCTYPE html "
	  "PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//"
	  "EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1"
	  "-transitional.dtd\">\n">>).

-define(HTML_DOCTYPE,
	<<"<!DOCTYPE html PUBLIC \"-//W3C//DTD "
	  "XHTML 1.0 Transitional//EN\" \"http://www.w3."
	  "org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
	  "">>).

start(SockData, Opts) ->
    supervisor:start_child(ejabberd_http_sup,
			   [SockData, Opts]).

start_link(SockData, Opts) ->
    {ok,
     proc_lib:spawn_link(ejabberd_http, init,
			 [SockData, Opts])}.

init({SockMod, Socket}, Opts) ->
    TLSEnabled = proplists:get_bool(tls, Opts),
    TLSOpts1 = lists:filter(fun ({certfile, _}) -> true;
				(_) -> false
			    end,
			    Opts),
    TLSOpts2 = case proplists:get_bool(tls_compression, Opts) of
                   false -> [compression_none | TLSOpts1];
                   true -> TLSOpts1
               end,
    TLSOpts = [verify_none | TLSOpts2],
    {SockMod1, Socket1} = if TLSEnabled ->
				 inet:setopts(Socket, [{recbuf, 8192}]),
				 {ok, TLSSocket} = p1_tls:tcp_to_tls(Socket,
								  TLSOpts),
				 {p1_tls, TLSSocket};
			     true -> {SockMod, Socket}
			  end,
    case SockMod1 of
      gen_tcp ->
	  inet:setopts(Socket1, [{packet, http_bin}, {recbuf, 8192}]);
      _ -> ok
    end,
    Captcha = case proplists:get_bool(captcha, Opts) of
                  true -> [{[<<"captcha">>], ejabberd_captcha}];
                  false -> []
              end,
    Register = case proplists:get_bool(register, Opts) of
                 true -> [{[<<"register">>], mod_register_web}];
                 false -> []
               end,
    Admin = case proplists:get_bool(web_admin, Opts) of
              true -> [{[<<"admin">>], ejabberd_web_admin}];
              false -> []
            end,
    Bind = case proplists:get_bool(http_bind, Opts) of
             true -> [{[<<"http-bind">>], mod_http_bind}];
             false -> []
           end,
    Poll = case proplists:get_bool(http_poll, Opts) of
             true -> [{[<<"http-poll">>], ejabberd_http_poll}];
             false -> []
           end,
    DefinedHandlers = gen_mod:get_opt(
                        request_handlers, Opts,
                        fun(Hs) ->
                                [{str:tokens(
                                    iolist_to_binary(Path), <<"/">>),
                                  Mod} || {Path, Mod} <- Hs]
                        end, []),
    RequestHandlers = DefinedHandlers ++ Captcha ++ Register ++
        Admin ++ Bind ++ Poll,
    ?DEBUG("S: ~p~n", [RequestHandlers]),

    DefaultHost = gen_mod:get_opt(default_host, Opts, fun(A) -> A end, undefined),

    ?INFO_MSG("started: ~p", [{SockMod1, Socket1}]),
    State = #state{sockmod = SockMod1,
                   socket = Socket1,
                   default_host = DefaultHost,
                   request_handlers = RequestHandlers},
    receive_headers(State).

become_controller(_Pid) -> ok.

socket_type() ->
    raw.

send_text(State, Text) ->
    case catch
	   (State#state.sockmod):send(State#state.socket, Text)
	of
      ok -> ok;
      {error, timeout} ->
	  ?INFO_MSG("Timeout on ~p:send", [State#state.sockmod]),
	  exit(normal);
      Error ->
	  ?DEBUG("Error in ~p:send: ~p",
		 [State#state.sockmod, Error]),
	  exit(normal)
    end.

receive_headers(#state{trail = Trail} = State) ->
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
                {ok, D} ->
                    parse_headers(State#state{trail = <<Trail/binary, D/binary>>});
                {error, _} ->
                    ok
            end
    end.

parse_headers(#state{trail = <<>>} = State) ->
    receive_headers(State);
parse_headers(#state{request_method = Method,
		     trail = Data} =
		  State) ->
    PktType = case Method of
                  undefined -> http_bin;
                  _ -> httph_bin
              end,
    case erlang:decode_packet(PktType, Data, []) of
        {ok, Pkt, Rest} ->
            NewState = process_header(State#state{trail = Rest}, {ok, Pkt}),
	    case NewState#state.end_of_request of
		true ->
		    ok;
		_ ->
                    parse_headers(NewState)
	    end;
        {more, _} ->
            receive_headers(State#state{trail = Data});
        _ ->
            ok
    end.

process_header(State, Data) ->
    SockMod = State#state.sockmod,
    Socket = State#state.socket,
    case Data of
      {ok, {http_request, Method, Uri, Version}} ->
	  KeepAlive = case Version of
			{1, 1} -> true;
			_ -> false
		      end,
	  Path = case Uri of
		   {absoluteURI, _Scheme, _Host, _Port, P} ->
		       {abs_path, P};
		   {abs_path, P} ->
		       {abs_path, P};
		   _ -> Uri
		 end,
	  State#state{request_method = Method,
		      request_version = Version, request_path = Path,
		      request_keepalive = KeepAlive};
      {ok, {http_header, _, 'Connection' = Name, _, Conn}} ->
	  KeepAlive1 = case jlib:tolower(Conn) of
			 <<"keep-alive">> -> true;
			 <<"close">> -> false;
			 _ -> State#state.request_keepalive
		       end,
	  State#state{request_keepalive = KeepAlive1,
		      request_headers = add_header(Name, Conn, State)};
      {ok,
       {http_header, _, 'Authorization' = Name, _, Auth}} ->
	  State#state{request_auth = parse_auth(Auth),
		      request_headers = add_header(Name, Auth, State)};
      {ok,
       {http_header, _, 'Content-Length' = Name, _, SLen}} ->
	  case catch jlib:binary_to_integer(SLen) of
	    Len when is_integer(Len) ->
		State#state{request_content_length = Len,
			    request_headers = add_header(Name, SLen, State)};
	    _ -> State
	  end;
      {ok,
       {http_header, _, 'Accept-Language' = Name, _, Langs}} ->
	  State#state{request_lang = parse_lang(Langs),
		      request_headers = add_header(Name, Langs, State)};
      {ok, {http_header, _, 'Host' = Name, _, Host}} ->
	  State#state{request_host = Host,
		      request_headers = add_header(Name, Host, State)};
      {ok, {http_header, _, Name, _, Value}} ->
	  State#state{request_headers =
			  add_header(Name, Value, State)};
      {ok, http_eoh}
	  when State#state.request_host == undefined ->
	  ?WARNING_MSG("An HTTP request without 'Host' HTTP "
		       "header was received.",
		       []),
	  throw(http_request_no_host_header);
      {ok, http_eoh} ->
	  ?DEBUG("(~w) http query: ~w ~s~n",
		 [State#state.socket, State#state.request_method,
		  element(2, State#state.request_path)]),
	  {HostProvided, Port, TP} =
	      get_transfer_protocol(SockMod,
				    State#state.request_host),
	  Host = get_host_really_served(State#state.default_host,
					HostProvided),
	  State2 = State#state{request_host = Host,
			       request_port = Port, request_tp = TP},
	  Out = process_request(State2),
	  send_text(State2, Out),
	  case State2#state.request_keepalive of
	    true ->
		case SockMod of
		  gen_tcp -> inet:setopts(Socket, [{packet, http_bin}]);
		  _ -> ok
		end,
		#state{sockmod = SockMod, socket = Socket,
		       request_handlers = State#state.request_handlers};
	    _ ->
		#state{end_of_request = true,
		       request_handlers = State#state.request_handlers}
	  end;
      _ ->
	  #state{end_of_request = true,
		 request_handlers = State#state.request_handlers}
    end.

add_header(Name, Value, State)->
    [{Name, Value} | State#state.request_headers].

get_host_really_served(undefined, Provided) ->
    Provided;
get_host_really_served(Default, Provided) ->
    case lists:member(Provided, ?MYHOSTS) of
      true -> Provided;
      false -> Default
    end.

%% @spec (SockMod, HostPort) -> {Host::string(), Port::integer(), TP}
%% where
%%       SockMod = gen_tcp | tls
%%       HostPort = string()
%%       TP = http | https
%% @doc Given a socket and hostport header, return data of transfer protocol.
%% Note that HostPort can be a string of a host like "example.org",
%% or a string of a host and port like "example.org:5280".
get_transfer_protocol(SockMod, HostPort) ->
    [Host | PortList] = str:tokens(HostPort, <<":">>),
    case {SockMod, PortList} of
      {gen_tcp, []} -> {Host, 80, http};
      {gen_tcp, [Port]} ->
	  {Host, jlib:binary_to_integer(Port), http};
      {p1_tls, []} -> {Host, 443, https};
      {p1_tls, [Port]} ->
	  {Host, jlib:binary_to_integer(Port), https}
    end.

%% XXX bard: search through request handlers looking for one that
%% matches the requested URL path, and pass control to it.  If none is
%% found, answer with HTTP 404.
process([], _) ->
    ejabberd_web:error(not_found);
process(Handlers, Request) ->
    %% Only the first element in the path prefix is checked
    [{HandlerPathPrefix, HandlerModule} | HandlersLeft] =
	Handlers,
    case lists:prefix(HandlerPathPrefix,
                    Request#request.path)
	   or (HandlerPathPrefix == Request#request.path)
	of
      true ->
	  ?DEBUG("~p matches ~p",
		 [Request#request.path, HandlerPathPrefix]),
	  LocalPath = lists:nthtail(length(HandlerPathPrefix),
				    Request#request.path),
            ?DEBUG("~p", [Request#request.headers]),
	  R = HandlerModule:process(LocalPath, Request),
	  ejabberd_hooks:run(http_request_debug,
			     [{LocalPath, Request}]),
	  R;
      false -> process(HandlersLeft, Request)
    end.

process_request(#state{request_method = Method,
		       request_path = {abs_path, Path}, request_auth = Auth,
		       request_lang = Lang, request_handlers = RequestHandlers,
		       request_host = Host, request_port = Port,
		       request_tp = TP, request_headers = RequestHeaders,
		       sockmod = SockMod,
		       socket = Socket} = State)
  when Method=:='GET' orelse Method=:='HEAD' orelse Method=:='DELETE' orelse Method=:='OPTIONS' ->
    case (catch url_decode_q_split(Path)) of
	{'EXIT', _} ->
	    make_bad_request(State);
	{NPath, Query} ->
	    LPath = normalize_path([NPE || NPE <- str:tokens(path_decode(NPath), <<"/">>)]),
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
		El when element(1, El) == xmlel ->
		    make_xhtml_output(State, 200, [], El);
		{Status, Headers, El} when
		element(1, El) == xmlel ->
		    make_xhtml_output(State, Status, Headers, El);
		Output when is_list(Output) or is_binary(Output) ->
		    make_text_output(State, 200, [], Output);
		{Status, Headers, Output} when is_list(Output) or is_binary(Output) ->
		    make_text_output(State, Status, Headers, Output)
	    end
    end;
process_request(#state{request_method = Method,
		       request_path = {abs_path, Path}, request_auth = Auth,
		       request_content_length = Len, request_lang = Lang,
		       sockmod = SockMod, socket = Socket, request_host = Host,
		       request_port = Port, request_tp = TP,
		       request_headers = RequestHeaders,
		       request_handlers = RequestHandlers} =
		    State)
    when (Method =:= 'POST' orelse Method =:= 'PUT') andalso
	   is_integer(Len) ->
    {ok, IPHere} = case SockMod of
		     gen_tcp -> inet:peername(Socket);
		     _ -> SockMod:peername(Socket)
		   end,
    XFF = proplists:get_value('X-Forwarded-For',
			      RequestHeaders, []),
    IP = analyze_ip_xff(IPHere, XFF, Host),
    case SockMod of
      gen_tcp -> inet:setopts(Socket, [{packet, 0}]);
      _ -> ok
    end,
    Data = recv_data(State, Len),
    ?DEBUG("client data: ~p~n", [Data]),
    case (catch url_decode_q_split(Path)) of
	{'EXIT', _} ->
            make_bad_request(State);
	{NPath, _Query} ->
	    LPath = normalize_path([NPE || NPE <- str:tokens(path_decode(NPath), <<"/">>)]),
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
		El when element(1, El) == xmlel ->
		    make_xhtml_output(State, 200, [], El);
		{Status, Headers, El} when
		element(1, El) == xmlel ->
		    make_xhtml_output(State, Status, Headers, El);
		Output when is_list(Output) or is_binary(Output) ->
		    make_text_output(State, 200, [], Output);
		{Status, Headers, Output} when is_list(Output) or is_binary(Output) ->
		    make_text_output(State, Status, Headers, Output)
	    end
    end;
process_request(State) -> make_bad_request(State).

make_bad_request(State) ->
%% Support for X-Forwarded-From
    make_xhtml_output(State, 400, [],
		      ejabberd_web:make_xhtml([#xmlel{name = <<"h1">>,
						      attrs = [],
						      children =
							  [{xmlcdata,
							    <<"400 Bad Request">>}]}])).

analyze_ip_xff(IP, [], _Host) -> IP;
analyze_ip_xff({IPLast, Port}, XFF, Host) ->
    [ClientIP | ProxiesIPs] = str:tokens(XFF, <<", ">>) ++
				[jlib:ip_to_list(IPLast)],
    TrustedProxies = ejabberd_config:get_option(
                       {trusted_proxies, Host},
                       fun(TPs) ->
                               [iolist_to_binary(TP) || TP <- TPs]
                       end, []),
    IPClient = case is_ipchain_trusted(ProxiesIPs,
				       TrustedProxies)
		   of
		 true ->
		     {ok, IPFirst} = inet_parse:address(
                                       binary_to_list(ClientIP)),
		     ?DEBUG("The IP ~w was replaced with ~w due to "
			    "header X-Forwarded-For: ~s",
			    [IPLast, IPFirst, XFF]),
		     IPFirst;
		 false -> IPLast
	       end,
    {IPClient, Port}.

is_ipchain_trusted(_UserIPs, all) -> true;
is_ipchain_trusted(UserIPs, TrustedIPs) ->
    [] == UserIPs -- [<<"127.0.0.1">> | TrustedIPs].

recv_data(State, Len) -> recv_data(State, Len, <<>>).

recv_data(_State, 0, Acc) -> (iolist_to_binary(Acc));
recv_data(State, Len, Acc) ->
    case State#state.trail of
      <<>> ->
	  case (State#state.sockmod):recv(State#state.socket, Len,
					  300000)
	      of
	    {ok, Data} ->
		recv_data(State, Len - byte_size(Data), <<Acc/binary, Data/binary>>);
	    _ -> <<"">>
	  end;
      _ ->
	  Trail = (State#state.trail),
	  recv_data(State#state{trail = <<>>},
		    Len - byte_size(Trail), <<Acc/binary, Trail/binary>>)
    end.

make_xhtml_output(State, Status, Headers, XHTML) ->
    Data = case lists:member(html, Headers) of
	     true ->
		 iolist_to_binary([?HTML_DOCTYPE,
				   xml:element_to_binary(XHTML)]);
	     _ ->
		 iolist_to_binary([?XHTML_DOCTYPE,
				   xml:element_to_binary(XHTML)])
	   end,
    Headers1 = case lists:keysearch(<<"Content-Type">>, 1,
				    Headers)
		   of
		 {value, _} ->
		     [{<<"Content-Length">>,
		       iolist_to_binary(integer_to_list(byte_size(Data)))}
		      | Headers];
		 _ ->
		     [{<<"Content-Type">>, <<"text/html; charset=utf-8">>},
		      {<<"Content-Length">>,
		       iolist_to_binary(integer_to_list(byte_size(Data)))}
		      | Headers]
	       end,
    HeadersOut = case {State#state.request_version,
		       State#state.request_keepalive}
		     of
		   {{1, 1}, true} -> Headers1;
		   {_, true} ->
		       [{<<"Connection">>, <<"keep-alive">>} | Headers1];
		   {_, false} ->
		       [{<<"Connection">>, <<"close">>} | Headers1]
		 end,
    Version = case State#state.request_version of
		{1, 1} -> <<"HTTP/1.1 ">>;
		_ -> <<"HTTP/1.0 ">>
	      end,
    H = lists:map(fun ({Attr, Val}) ->
			  [Attr, <<": ">>, Val, <<"\r\n">>];
		      (_) -> []
		  end,
		  HeadersOut),
    SL = [Version,
	  iolist_to_binary(integer_to_list(Status)), <<" ">>,
	  code_to_phrase(Status), <<"\r\n">>],
    Data2 = case State#state.request_method of
	      'HEAD' -> <<"">>;
	      _ -> Data
	    end,
    [SL, H, <<"\r\n">>, Data2].

make_text_output(State, Status, Headers, Text) ->
    make_text_output(State, Status, <<"">>, Headers, Text).

make_text_output(State, Status, Reason, Headers, Text) ->
    Data = iolist_to_binary(Text),
    Headers1 = case lists:keysearch(<<"Content-Type">>, 1,
				    Headers)
		   of
		 {value, _} ->
		     [{<<"Content-Length">>,
		       jlib:integer_to_binary(byte_size(Data))}
		      | Headers];
		 _ ->
		     [{<<"Content-Type">>, <<"text/html; charset=utf-8">>},
		      {<<"Content-Length">>,
		       jlib:integer_to_binary(byte_size(Data))}
		      | Headers]
	       end,
    HeadersOut = case {State#state.request_version,
		       State#state.request_keepalive}
		     of
		   {{1, 1}, true} -> Headers1;
		   {_, true} ->
		       [{<<"Connection">>, <<"keep-alive">>} | Headers1];
		   {_, false} ->
		       [{<<"Connection">>, <<"close">>} | Headers1]
		 end,
    Version = case State#state.request_version of
		{1, 1} -> <<"HTTP/1.1 ">>;
		_ -> <<"HTTP/1.0 ">>
	      end,
    H = lists:map(fun ({Attr, Val}) ->
			  [Attr, <<": ">>, Val, <<"\r\n">>]
		  end,
		  HeadersOut),
    NewReason = case Reason of
		  <<"">> -> code_to_phrase(Status);
		  _ -> Reason
		end,
    SL = [Version,
	  jlib:integer_to_binary(Status), <<" ">>,
	  NewReason, <<"\r\n">>],
    Data2 = case State#state.request_method of
	      'HEAD' -> <<"">>;
	      _ -> Data
	    end,
    [SL, H, <<"\r\n">>, Data2].

parse_lang(Langs) ->
    case str:tokens(Langs, <<",; ">>) of
      [First | _] -> First;
      [] -> <<"en">>
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
    url_decode_q_split(Path, <<>>).

url_decode_q_split(<<$?, T/binary>>, Acc) ->
    %% Don't decode the query string here, that is parsed separately.
    {path_norm_reverse(Acc), T};
url_decode_q_split(<<H, T/binary>>, Acc) when H /= 0 ->
    url_decode_q_split(T, <<H, Acc/binary>>);
url_decode_q_split(<<>>, Ack) ->
    {path_norm_reverse(Ack), <<>>}.

%% @doc Decode a part of the URL and return string()
path_decode(Path) -> path_decode(Path, <<>>).

path_decode(<<$%, Hi, Lo, Tail/binary>>, Acc) ->
    Hex = hex_to_integer([Hi, Lo]),
    if Hex == 0 -> exit(badurl);
       true -> ok
    end,
    path_decode(Tail, <<Acc/binary, Hex>>);
path_decode(<<H, T/binary>>, Acc) when H /= 0 ->
    path_decode(T, <<Acc/binary, H>>);
path_decode(<<>>, Acc) -> Acc.

path_norm_reverse(<<"/", T/binary>>) -> start_dir(0, <<"/">>, T);
path_norm_reverse(T) -> start_dir(0, <<"">>, T).

start_dir(N, Path, <<"..">>) -> rest_dir(N, Path, <<"">>);
start_dir(N, Path, <<"/", T/binary>>) -> start_dir(N, Path, T);
start_dir(N, Path, <<"./", T/binary>>) -> start_dir(N, Path, T);
start_dir(N, Path, <<"../", T/binary>>) ->
    start_dir(N + 1, Path, T);
start_dir(N, Path, T) -> rest_dir(N, Path, T).

rest_dir(_N, Path, <<>>) ->
    case Path of
      <<>> -> <<"/">>;
      _ -> Path
    end;
rest_dir(0, Path, <<$/, T/binary>>) ->
    start_dir(0, <<$/, Path/binary>>, T);
rest_dir(N, Path, <<$/, T/binary>>) ->
    start_dir(N - 1, Path, T);
rest_dir(0, Path, <<H, T/binary>>) ->
    rest_dir(0, <<H, Path/binary>>, T);
rest_dir(N, Path, <<_H, T/binary>>) -> rest_dir(N, Path, T).

%% hex_to_integer

hex_to_integer(Hex) ->
    case catch list_to_integer(Hex, 16) of
      {'EXIT', _} -> old_hex_to_integer(Hex);
      X -> X
    end.

old_hex_to_integer(Hex) ->
    DEHEX = fun (H) when H >= $a, H =< $f -> H - $a + 10;
		(H) when H >= $A, H =< $F -> H - $A + 10;
		(H) when H >= $0, H =< $9 -> H - $0
	    end,
    lists:foldl(fun (E, Acc) -> Acc * 16 + DEHEX(E) end, 0,
		Hex).

code_to_phrase(100) -> <<"Continue">>;
code_to_phrase(101) -> <<"Switching Protocols ">>;
code_to_phrase(200) -> <<"OK">>;
code_to_phrase(201) -> <<"Created">>;
code_to_phrase(202) -> <<"Accepted">>;
code_to_phrase(203) ->
    <<"Non-Authoritative Information">>;
code_to_phrase(204) -> <<"No Content">>;
code_to_phrase(205) -> <<"Reset Content">>;
code_to_phrase(206) -> <<"Partial Content">>;
code_to_phrase(300) -> <<"Multiple Choices">>;
code_to_phrase(301) -> <<"Moved Permanently">>;
code_to_phrase(302) -> <<"Found">>;
code_to_phrase(303) -> <<"See Other">>;
code_to_phrase(304) -> <<"Not Modified">>;
code_to_phrase(305) -> <<"Use Proxy">>;
code_to_phrase(306) -> <<"(Unused)">>;
code_to_phrase(307) -> <<"Temporary Redirect">>;
code_to_phrase(400) -> <<"Bad Request">>;
code_to_phrase(401) -> <<"Unauthorized">>;
code_to_phrase(402) -> <<"Payment Required">>;
code_to_phrase(403) -> <<"Forbidden">>;
code_to_phrase(404) -> <<"Not Found">>;
code_to_phrase(405) -> <<"Method Not Allowed">>;
code_to_phrase(406) -> <<"Not Acceptable">>;
code_to_phrase(407) ->
    <<"Proxy Authentication Required">>;
code_to_phrase(408) -> <<"Request Timeout">>;
code_to_phrase(409) -> <<"Conflict">>;
code_to_phrase(410) -> <<"Gone">>;
code_to_phrase(411) -> <<"Length Required">>;
code_to_phrase(412) -> <<"Precondition Failed">>;
code_to_phrase(413) -> <<"Request Entity Too Large">>;
code_to_phrase(414) -> <<"Request-URI Too Long">>;
code_to_phrase(415) -> <<"Unsupported Media Type">>;
code_to_phrase(416) ->
    <<"Requested Range Not Satisfiable">>;
code_to_phrase(417) -> <<"Expectation Failed">>;
code_to_phrase(500) -> <<"Internal Server Error">>;
code_to_phrase(501) -> <<"Not Implemented">>;
code_to_phrase(502) -> <<"Bad Gateway">>;
code_to_phrase(503) -> <<"Service Unavailable">>;
code_to_phrase(504) -> <<"Gateway Timeout">>;
code_to_phrase(505) -> <<"HTTP Version Not Supported">>.

parse_auth(<<"Basic ", Auth64/binary>>) ->
    Auth = jlib:decode_base64(Auth64),
    %% Auth should be a string with the format: user@server:password
    %% Note that password can contain additional characters '@' and ':'
    case str:chr(Auth, $:) of
        0 ->
            undefined;
        Pos ->
            {User, <<$:, Pass/binary>>} = erlang:split_binary(Auth, Pos-1),
            {User, Pass}
    end;
parse_auth(<<_/binary>>) -> undefined.

parse_urlencoded(S) ->
    parse_urlencoded(S, nokey, <<>>, key).

parse_urlencoded(<<$%, Hi, Lo, Tail/binary>>, Last, Cur,
		 State) ->
    Hex = hex_to_integer([Hi, Lo]),
    parse_urlencoded(Tail, Last, <<Cur/binary, Hex>>, State);
parse_urlencoded(<<$&, Tail/binary>>, _Last, Cur, key) ->
    [{Cur, <<"">>} | parse_urlencoded(Tail,
                                      nokey, <<>>,
                                      key)];  %% cont keymode
parse_urlencoded(<<$&, Tail/binary>>, Last, Cur, value) ->
    V = {Last, Cur},
    [V | parse_urlencoded(Tail, nokey, <<>>, key)];
parse_urlencoded(<<$+, Tail/binary>>, Last, Cur, State) ->
    parse_urlencoded(Tail, Last, <<Cur/binary, $\s>>, State);
parse_urlencoded(<<$=, Tail/binary>>, _Last, Cur, key) ->
    parse_urlencoded(Tail, Cur, <<>>,
		     value); %% change mode
parse_urlencoded(<<H, Tail/binary>>, Last, Cur, State) ->
    parse_urlencoded(Tail, Last, <<Cur/binary, H>>, State);
parse_urlencoded(<<>>, Last, Cur, _State) ->
    [{Last, Cur}];
parse_urlencoded(undefined, _, _, _) -> [].


url_encode(A) ->
    url_encode(A, <<>>).

url_encode(<<H:8, T/binary>>, Acc) when
      (H >= $a andalso H =< $z) orelse
      (H >= $A andalso H =< $Z) orelse
      (H >= $0 andalso H =< $9) orelse
      H == $_ orelse
      H == $. orelse
      H == $- orelse
      H == $/ orelse
      H == $: ->
    url_encode(T, <<Acc/binary, H>>);
url_encode(<<H:8, T/binary>>, Acc) ->
    case integer_to_hex(H) of
        [X, Y] -> url_encode(T, <<Acc/binary, $%, X, Y>>);
        [X] -> url_encode(T, <<Acc/binary, $%, $0, X>>)
    end;
url_encode(<<>>, Acc) ->
    Acc.


integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
      {'EXIT', _} -> old_integer_to_hex(I);
      Int -> Int
    end.

old_integer_to_hex(I) when I < 10 -> integer_to_list(I);
old_integer_to_hex(I) when I < 16 -> [I - 10 + $A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I / 16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

normalize_path(Path) ->
    normalize_path(Path, []).

normalize_path([], Norm) -> lists:reverse(Norm);
normalize_path([<<"..">>|Path], Norm) ->
    normalize_path(Path, Norm);
normalize_path([_Parent, <<"..">>|Path], Norm) ->
    normalize_path(Path, Norm);
normalize_path([Part | Path], Norm) ->
    normalize_path(Path, [Part|Norm]).

transform_listen_option(captcha, Opts) ->
    [{captcha, true}|Opts];
transform_listen_option(register, Opts) ->
    [{register, true}|Opts];
transform_listen_option(web_admin, Opts) ->
    [{web_admin, true}|Opts];
transform_listen_option(http_bind, Opts) ->
    [{http_bind, true}|Opts];
transform_listen_option(http_poll, Opts) ->
    [{http_poll, true}|Opts];
transform_listen_option({request_handlers, Hs}, Opts) ->
    Hs1 = lists:map(
            fun({PList, Mod}) when is_list(PList) ->
                    Path = iolist_to_binary([[$/, P] || P <- PList]),
                    {Path, Mod};
               (Opt) ->
                    Opt
            end, Hs),
    [{request_handlers, Hs1} | Opts];
transform_listen_option(Opt, Opts) ->
    [Opt|Opts].
