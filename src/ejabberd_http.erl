%%%----------------------------------------------------------------------
%%% File    : ejabberd_http.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 27 Feb 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-behaviour(ejabberd_listener).

-author('alexey@process-one.net').

%% External exports
-export([start/3, start_link/3,
	 accept/1, receive_headers/1, recv_file/2,
	 listen_opt_type/1, listen_options/0,
	 apply_custom_headers/2]).
-export([get_url/4, get_auto_url/2, find_handler_port_path/2]).
-export([init/3]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_http.hrl").

-include_lib("kernel/include/file.hrl").

-record(state, {sockmod,
		socket,
		request_method,
		request_version,
		request_path,
		request_auth,
		request_keepalive,
		request_content_length = 0,
		request_lang = <<"en">>,
		%% XXX bard: request handlers are configured in
		%% ejabberd.cfg under the HTTP service.	 For example,
		%% to have the module test_web handle requests with
		%% paths starting with "/test/module":
		%%
		%%   {5280, ejabberd_http,    [http_bind, web_admin,
		%%			       {request_handlers, [{["test", "module"], mod_test_web}]}]}
		%%
		request_handlers = [],
		request_host,
		request_port,
		request_tp,
		request_headers = [],
		end_of_request = false,
		options = [],
		custom_headers,
		trail = <<>>,
		allow_unencrypted_sasl2,
		addr_re,
		sock_peer_name = none
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

-define(RECV_BUF, 65536).
-define(SEND_BUF, 65536).
-define(MAX_POST_SIZE, 20971520). %% 20Mb

start(SockMod, Socket, Opts) ->
    {ok,
     proc_lib:spawn(ejabberd_http, init,
		    [SockMod, Socket, Opts])}.

start_link(SockMod, Socket, Opts) ->
    {ok,
     proc_lib:spawn_link(ejabberd_http, init,
			 [SockMod, Socket, Opts])}.

init(SockMod, Socket, Opts) ->
    TLSEnabled = proplists:get_bool(tls, Opts),
    TLSOpts1 = lists:filter(fun ({ciphers, _}) -> true;
				({dhfile, _}) -> true;
				({cafile, _}) -> true;
				({protocol_options, _}) -> true;
				(_) -> false
			    end,
			    Opts),
    TLSOpts2 = case proplists:get_bool(tls_compression, Opts) of
                   false -> [compression_none | TLSOpts1];
                   true -> TLSOpts1
               end,
    TLSOpts3 = case ejabberd_pkix:get_certfile(
		      ejabberd_config:get_myname()) of
		   error -> TLSOpts2;
		   {ok, CertFile} -> [{certfile, CertFile}|TLSOpts2]
	       end,
    TLSOpts = [verify_none | TLSOpts3],
    {SockMod1, Socket1} = if TLSEnabled ->
				 inet:setopts(Socket, [{recbuf, ?RECV_BUF}]),
				 {ok, TLSSocket} = fast_tls:tcp_to_tls(Socket,
								  TLSOpts),
				 {fast_tls, TLSSocket};
			     true -> {SockMod, Socket}
			  end,
    SockPeer =  proplists:get_value(sock_peer_name, Opts, none),
    RequestHandlers0 = proplists:get_value(request_handlers, Opts, []),
    RequestHandlers = ejabberd_hooks:run_fold(http_request_handlers_init,
                                              RequestHandlers0,
                                              [Opts]),
    ?DEBUG("S: ~p~n", [RequestHandlers]),

    {ok, RE} = re:compile(<<"^(?:\\[(.*?)\\]|(.*?))(?::(\\d+))?$">>),

    CustomHeaders = proplists:get_value(custom_headers, Opts, []),

    AllowUnencryptedSasl2 = proplists:get_bool(allow_unencrypted_sasl2, Opts),
    State = #state{sockmod = SockMod1,
                   socket = Socket1,
		   custom_headers = CustomHeaders,
		   options = Opts,
		   allow_unencrypted_sasl2 = AllowUnencryptedSasl2,
		   request_handlers = RequestHandlers,
		   sock_peer_name = SockPeer,
		   addr_re = RE},
    try receive_headers(State) of
        V -> V
    catch
        {error, _} -> State
    end.

accept(_Pid) ->
    ok.

send_text(_State, none) ->
    ok;
send_text(State, Text) ->
    case (State#state.sockmod):send(State#state.socket, Text) of
	ok -> ok;
	{error, timeout} ->
	    ?INFO_MSG("Timeout on ~p:send", [State#state.sockmod]),
	    exit(normal);
	Error ->
	    ?DEBUG("Error in ~p:send: ~p",
		   [State#state.sockmod, Error]),
	    exit(normal)
    end.

send_file(State, Fd, Size, FileName) ->
    try
	case State#state.sockmod of
	    gen_tcp ->
		{ok, _} = file:sendfile(Fd, State#state.socket, 0, Size, []),
		ok;
	    _ ->
		case file:read(Fd, ?SEND_BUF) of
		    {ok, Data} ->
			send_text(State, Data),
			send_file(State, Fd, Size, FileName);
		    eof ->
			ok
		end
	end
    catch _:{case_clause, {error, Why}} ->
	    if Why /= closed ->
		    ?WARNING_MSG("Failed to read ~ts: ~ts",
				 [FileName, file_format_error(Why)]),
		    exit(normal);
	       true ->
		    ok
	    end
    end.

receive_headers(#state{trail = Trail} = State) ->
    SockMod = State#state.sockmod,
    Socket = State#state.socket,
    Data = SockMod:recv(Socket, 0, 300000),
    case Data of
	{error, closed} when State#state.request_method == undefined ->
	    % socket closed without receiving anything in it
	    ok;
        {error, Error} ->
	    ?DEBUG("Error when retrieving http headers ~p: ~p",
		   [State#state.sockmod, Error]),
	    ok;
        {ok, D} ->
            parse_headers(State#state{trail = <<Trail/binary, D/binary>>})
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
		true -> ok;
		_ -> parse_headers(NewState)
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
	  KeepAlive1 = case misc:tolower(Conn) of
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
	  case catch binary_to_integer(SLen) of
	    Len when is_integer(Len) ->
		State#state{request_content_length = Len,
			    request_headers = add_header(Name, SLen, State)};
	    _ -> State
	  end;
      {ok,
       {http_header, _, 'Accept-Language' = Name, _, Langs}} ->
	  State#state{request_lang = parse_lang(Langs),
		      request_headers = add_header(Name, Langs, State)};
      {ok, {http_header, _, 'Host' = Name, _, Value}} ->
	  {Host, Port, TP} = get_transfer_protocol(State#state.addr_re, SockMod, Value),
	  State#state{request_host = ejabberd_config:resolve_host_alias(Host),
		      request_port = Port,
		      request_tp = TP,
		      request_headers = add_header(Name, Value, State)};
      {ok, {http_header, _, Name, _, Value}} when is_binary(Name) ->
	  State#state{request_headers =
			  add_header(normalize_header_name(Name), Value, State)};
      {ok, {http_header, _, Name, _, Value}} ->
	  State#state{request_headers =
			  add_header(Name, Value, State)};
      {ok, http_eoh} when State#state.request_host == undefined;
			  State#state.request_host == error ->
	    {State1, Out} = process_request(State),
	    send_text(State1, Out),
	    process_header(State, {ok, {http_error, <<>>}});
      {ok, http_eoh} ->
	    ?DEBUG("(~w) http query: ~w ~p~n",
		   [State#state.socket, State#state.request_method,
		    element(2, State#state.request_path)]),
	    {State3, Out} = process_request(State),
	    send_text(State3, Out),
	    case State3#state.request_keepalive of
		true ->
		    #state{sockmod = SockMod, socket = Socket,
			   trail = State3#state.trail,
			   options = State#state.options,
			   custom_headers = State#state.custom_headers,
			   request_handlers = State#state.request_handlers,
			   addr_re = State#state.addr_re};
		_ ->
		    #state{end_of_request = true,
			   trail = State3#state.trail,
			   options = State#state.options,
			   custom_headers = State#state.custom_headers,
			   request_handlers = State#state.request_handlers,
			   addr_re = State#state.addr_re}
	    end;
      _ ->
	  #state{end_of_request = true,
		 options = State#state.options,
		 custom_headers = State#state.custom_headers,
		 request_handlers = State#state.request_handlers,
		 addr_re = State#state.addr_re}
    end.

add_header(Name, Value, State)->
    [{Name, Value} | State#state.request_headers].

get_transfer_protocol(RE, SockMod, HostPort) ->
    {Proto, DefPort} = case SockMod of
			   gen_tcp -> {http, 80};
			   fast_tls -> {https, 443}
		       end,
    {Host, Port} = case re:run(HostPort, RE, [{capture,[1,2,3],binary}]) of
		       nomatch ->
			   {error, DefPort};
		       {match, [<<>>, H, <<>>]} ->
			   {jid:nameprep(H), DefPort};
		       {match, [H, <<>>, <<>>]} ->
			   {jid:nameprep(H), DefPort};
		       {match, [<<>>, H, PortStr]} ->
			   {jid:nameprep(H), binary_to_integer(PortStr)};
		       {match, [H, <<>>, PortStr]} ->
			   {jid:nameprep(H), binary_to_integer(PortStr)}
		   end,

    {Host, Port, Proto}.

%% XXX bard: search through request handlers looking for one that
%% matches the requested URL path, and pass control to it.  If none is
%% found, answer with HTTP 404.

process([], _) -> ejabberd_web:error(not_found);
process(Handlers, Request) ->
    {HandlerPathPrefix, HandlerModule, HandlerOpts, HandlersLeft} =
        case Handlers of
            [{Pfx, Mod} | Tail] ->
                {Pfx, Mod, [], Tail};
            [{Pfx, Mod, Opts} | Tail] ->
                {Pfx, Mod, Opts, Tail}
        end,

    case (lists:prefix(HandlerPathPrefix, Request#request.path) or
         (HandlerPathPrefix==Request#request.path)) of
        true ->
            ?DEBUG("~p matches ~p", [Request#request.path, HandlerPathPrefix]),
            %% LocalPath is the path "local to the handler", i.e. if
            %% the handler was registered to handle "/test/" and the
            %% requested path is "/test/foo/bar", the local path is
            %% ["foo", "bar"]
            LocalPath = lists:nthtail(length(HandlerPathPrefix), Request#request.path),
	    R = case erlang:function_exported(HandlerModule, socket_handoff, 3) of
		    true ->
			HandlerModule:socket_handoff(
			  LocalPath, Request, HandlerOpts);
		    false ->
                        try
                            HandlerModule:process(LocalPath, Request)
                        catch
                            Class:Reason:Stack ->
                                ?ERROR_MSG(
                                  "HTTP handler crashed: ~s",
                                  [misc:format_exception(2, Class, Reason, Stack)]),
                                erlang:raise(Class, Reason, Stack)
                        end
		end,
            ejabberd_hooks:run(http_request_debug, [{LocalPath, Request}]),
            R;
        false ->
	    process(HandlersLeft, Request)
    end.

extract_path_query(#state{request_method = Method,
			  request_path = {abs_path, Path}} = State)
    when Method =:= 'GET' orelse
	   Method =:= 'HEAD' orelse
	     Method =:= 'DELETE' orelse Method =:= 'OPTIONS' ->
    case catch url_decode_q_split_normalize(Path) of
	{'EXIT', Error} ->
	    ?DEBUG("Error decoding URL '~p': ~p", [Path, Error]),
	    {State, false};
	{LPath, Query} ->
	    LQuery = case catch parse_urlencoded(Query) of
			 {'EXIT', _Reason} -> [];
			 LQ -> LQ
		     end,
	    {State, {LPath, LQuery, <<"">>, Path}}
    end;
extract_path_query(#state{request_method = Method,
			  request_path = {abs_path, Path},
			  request_content_length = Len,
			  trail = Trail,
			  sockmod = _SockMod,
			  socket = _Socket} = State)
  when (Method =:= 'POST' orelse Method =:= 'PUT') andalso Len>0 ->
    case catch url_decode_q_split_normalize(Path) of
	{'EXIT', Error} ->
	    ?DEBUG("Error decoding URL '~p': ~p", [Path, Error]),
	    {State, false};
        {LPath, _Query} ->
	    case Method of
		'PUT' ->
		    {State, {LPath, [], Trail, Path}};
		'POST' ->
		    case recv_data(State) of
			{ok, Data} ->
			    LQuery = case catch parse_urlencoded(Data) of
					 {'EXIT', _Reason} -> [];
					 LQ -> LQ
				     end,
			    {State, {LPath, LQuery, Data, Path}};
			error ->
			    {State, false}
		    end
	    end
    end;
extract_path_query(State) ->
    {State, false}.

process_request(#state{request_host = undefined,
		       custom_headers = CustomHeaders} = State) ->
    {State, make_text_output(State, 400, CustomHeaders,
			     <<"Missing Host header">>)};
process_request(#state{request_host = error,
		       custom_headers = CustomHeaders} = State) ->
    {State, make_text_output(State, 400, CustomHeaders,
			     <<"Malformed Host header">>)};
process_request(#state{request_method = Method,
		       request_auth = Auth,
		       request_lang = Lang,
		       request_version = Version,
		       sockmod = SockMod,
		       socket = Socket,
		       sock_peer_name = SockPeer,
		       options = Options,
		       request_host = Host,
		       request_port = Port,
		       request_tp = TP,
		       request_content_length = Length,
		       request_headers = RequestHeaders,
		       request_handlers = RequestHandlers,
		       custom_headers = CustomHeaders} = State) ->
    case proplists:get_value(<<"Expect">>, RequestHeaders, <<>>) of
	<<"100-", _/binary>> when Version == {1, 1} ->
	    send_text(State, <<"HTTP/1.1 100 Continue\r\n\r\n">>);
	_ ->
	    ok
    end,
    case extract_path_query(State) of
	{State2, false} ->
	    {State2, make_bad_request(State)};
	{State2, {LPath, LQuery, Data, RawPath}} ->
	    PeerName = case SockPeer of
			   none ->
			       case SockMod of
				   gen_tcp ->
				       inet:peername(Socket);
				   _ ->
				       SockMod:peername(Socket)
			       end;
			   {_, Peer} ->
			       {ok, Peer}
		       end,
            IPHere = case PeerName of
                         {ok, V} -> V;
                         {error, _} = E -> throw(E)
                     end,
	    XFF = proplists:get_value('X-Forwarded-For', RequestHeaders, []),
	    IP = analyze_ip_xff(IPHere, XFF),
            Request = #request{method = Method,
                               path = LPath,
                               raw_path = RawPath,
                               q = LQuery,
                               auth = Auth,
			       length = Length,
			       sockmod = SockMod,
			       socket = Socket,
			       data = Data,
                               lang = Lang,
                               host = Host,
                               port = Port,
                               tp = TP,
			       opts = Options,
                               headers = RequestHeaders,
                               ip = IP},
	    RequestHandlers1 = ejabberd_hooks:run_fold(
				http_request_handlers, RequestHandlers, [Host, Request]),
	    Res = case process(RequestHandlers1, Request) of
		      El when is_record(El, xmlel) ->
			  make_xhtml_output(State, 200, CustomHeaders, El);
		      {Status, Headers, El}
			when is_record(El, xmlel) ->
			  make_xhtml_output(State, Status,
					    apply_custom_headers(Headers, CustomHeaders), El);
		      Output when is_binary(Output) or is_list(Output) ->
			  make_text_output(State, 200, CustomHeaders, Output);
		      {Status, Headers, Output}
			when is_binary(Output) or is_list(Output) ->
			  make_text_output(State, Status,
					   apply_custom_headers(Headers, CustomHeaders), Output);
		      {Status, Headers, {file, FileName}} ->
			  make_file_output(State, Status, Headers, FileName);
		      {Status, Reason, Headers, Output}
			when is_binary(Output) or is_list(Output) ->
			  make_text_output(State, Status, Reason,
					   apply_custom_headers(Headers, CustomHeaders), Output);
		      _ ->
			  none
		  end,
	    {State2#state{trail = <<>>}, Res}
    end.

make_bad_request(State) ->
    make_xhtml_output(State, 400, State#state.custom_headers,
		      ejabberd_web:make_xhtml([#xmlel{name = <<"h1">>,
						      attrs = [],
						      children =
							  [{xmlcdata,
							    <<"400 Bad Request">>}]}])).

analyze_ip_xff(IP, []) -> IP;
analyze_ip_xff({IPLast, Port}, XFF) ->
    [ClientIP | ProxiesIPs] = str:tokens(XFF, <<", ">>) ++
				[misc:ip_to_list(IPLast)],
    TrustedProxies = ejabberd_option:trusted_proxies(),
    IPClient = case is_ipchain_trusted(ProxiesIPs,
				       TrustedProxies)
		   of
		 true ->
		     case inet_parse:address(binary_to_list(ClientIP)) of
			 {ok, IPFirst} ->
			     ?DEBUG("The IP ~w was replaced with ~w due to "
				    "header X-Forwarded-For: ~ts",
				    [IPLast, IPFirst, XFF]),
			     IPFirst;
			 E -> throw(E)
		     end;
		 false -> IPLast
	       end,
    {IPClient, Port}.

is_ipchain_trusted([], _) -> false;
is_ipchain_trusted(_UserIPs, all) -> true;
is_ipchain_trusted(UserIPs, Masks) ->
    lists:all(
	fun(IP) ->
	    case inet:parse_address(binary_to_list(IP)) of
		{ok, IP2} ->
		    lists:any(
			fun({Mask, MaskLen}) ->
				misc:match_ip_mask(IP2, Mask, MaskLen)
			end, Masks);
		_ ->
		    false
	    end
	end, UserIPs).

recv_data(#state{request_content_length = Len}) when Len >= ?MAX_POST_SIZE ->
    error;
recv_data(#state{request_content_length = Len, trail = Trail,
		 sockmod = SockMod, socket = Socket}) ->
    NewLen = Len - byte_size(Trail),
    if NewLen > 0 ->
	    case SockMod:recv(Socket, NewLen, 60000) of
		{ok, Data} -> {ok, <<Trail/binary, Data/binary>>};
		{error, _} -> error
	    end;
       true ->
	    {ok, Trail}
    end.

recv_file(#request{length = Len, data = Trail,
		   sockmod = SockMod, socket = Socket}, Path) ->
    case file:open(Path, [write, exclusive, raw]) of
	{ok, Fd} ->
	    Res = case file:write(Fd, Trail) of
		      ok ->
			  NewLen = max(0, Len - byte_size(Trail)),
			  do_recv_file(NewLen, SockMod, Socket, Fd);
		      {error, _} = Err ->
			  Err
		  end,
	    file:close(Fd),
	    case Res of
		ok -> ok;
		{error, _} -> file:delete(Path)
	    end,
	    Res;
	{error, _} = Err ->
	    Err
    end.

do_recv_file(0, _SockMod, _Socket, _Fd) ->
    ok;
do_recv_file(Len, SockMod, Socket, Fd) ->
    ChunkLen = min(Len, ?RECV_BUF),
    case SockMod:recv(Socket, ChunkLen, timer:seconds(30)) of
	{ok, Data} ->
	    case file:write(Fd, Data) of
		ok ->
		    do_recv_file(Len-size(Data), SockMod, Socket, Fd);
		{error, _} = Err ->
		    Err
	    end;
	{error, _} ->
	    {error, closed}
    end.

make_headers(State, Status, Reason, Headers, Data) ->
    Len = if is_integer(Data) -> Data;
	     true -> iolist_size(Data)
	  end,
    Headers1 = [{<<"Content-Length">>, integer_to_binary(Len)} | Headers],
    Headers2 = case lists:keyfind(<<"Content-Type">>, 1, Headers) of
		   {_, _} ->
		       Headers1;
		   false ->
		       [{<<"Content-Type">>, <<"text/html; charset=utf-8">>}
			| Headers1]
	       end,
    HeadersOut = case {State#state.request_version,
		       State#state.request_keepalive} of
		     {{1, 1}, true} -> Headers2;
		     {_, true} ->
			 [{<<"Connection">>, <<"keep-alive">>} | Headers2];
		     {_, false} ->
			 [{<<"Connection">>, <<"close">>} | Headers2]
		 end,
    Version = case State#state.request_version of
		  {1, 1} -> <<"HTTP/1.1 ">>;
		  _ -> <<"HTTP/1.0 ">>
	      end,
    H = [[Attr, <<": ">>, Val, <<"\r\n">>] || {Attr, Val} <- HeadersOut],
    NewReason = case Reason of
		  <<"">> -> code_to_phrase(Status);
		  _ -> Reason
		end,
    SL = [Version,
	  integer_to_binary(Status), <<" ">>,
	  NewReason, <<"\r\n">>],
    [SL, H, <<"\r\n">>].

make_xhtml_output(State, Status, Headers, XHTML) ->
    Data = case State#state.request_method of
	       'HEAD' -> <<"">>;
	       _ ->
		   DocType = case lists:member(html, Headers) of
				 true -> ?HTML_DOCTYPE;
				 false -> ?XHTML_DOCTYPE
			     end,
		   iolist_to_binary([DocType, fxml:element_to_binary(XHTML)])
	   end,
    EncodedHdrs = make_headers(State, Status, <<"">>, Headers, Data),
    [EncodedHdrs, Data].

make_text_output(State, Status, Headers, Text) ->
    make_text_output(State, Status, <<"">>, Headers, Text).

make_text_output(State, Status, Reason, Headers, Text) ->
    Data = iolist_to_binary(Text),
    Data2 = case State#state.request_method of
		'HEAD' -> <<"">>;
		_ -> Data
	    end,
    EncodedHdrs = make_headers(State, Status, Reason, Headers, Data2),
    [EncodedHdrs, Data2].

make_file_output(State, Status, Headers, FileName) ->
    case file:read_file_info(FileName) of
	{ok, #file_info{size = Size}} when State#state.request_method == 'HEAD' ->
	    make_headers(State, Status, <<"">>, Headers, Size);
	{ok, #file_info{size = Size}} ->
	    case file:open(FileName, [raw, read]) of
		{ok, Fd} ->
		    EncodedHdrs = make_headers(State, Status, <<"">>, Headers, Size),
		    send_text(State, EncodedHdrs),
		    send_file(State, Fd, Size, FileName),
		    file:close(Fd),
		    none;
		{error, Why} ->
		    Reason = file_format_error(Why),
		    ?ERROR_MSG("Failed to open ~ts: ~ts", [FileName, Reason]),
		    make_text_output(State, 404, Reason, [], <<>>)
	    end;
	{error, Why} ->
	    Reason = file_format_error(Why),
	    ?ERROR_MSG("Failed to read info of ~ts: ~ts", [FileName, Reason]),
	    make_text_output(State, 404, Reason, [], <<>>)
    end.

parse_lang(Langs) ->
    case str:tokens(Langs, <<",; ">>) of
      [First | _] -> First;
      [] -> <<"en">>
    end.

file_format_error(Reason) ->
    case file:format_error(Reason) of
	"unknown POSIX error" -> atom_to_list(Reason);
	Text -> Text
    end.

url_decode_q_split_normalize(Path) ->
    {NPath, Query} = url_decode_q_split(Path),
    LPath = normalize_path([NPE
		    || NPE <- str:tokens(uri_string:percent_decode(NPath), <<"/">>)]),
    {LPath, Query}.

% Code below is taken (with some modifications) from the yaws webserver, which
% is distributed under the following license:
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

-spec parse_auth(binary()) -> {binary(), binary()} | {oauth, binary(), []} | invalid.
parse_auth(<<"Basic ", Auth64/binary>>) ->
    try base64:decode(Auth64) of
	Auth ->
	    case binary:split(Auth, <<":">>) of
		[User, Pass] ->
		    PassUtf8 = unicode:characters_to_binary(Pass, utf8),
		    {User, PassUtf8};
		_ ->
		    invalid
	    end
    catch _:_ ->
	invalid
    end;
parse_auth(<<"Bearer ", SToken/binary>>) ->
    Token = str:strip(SToken),
    {oauth, Token, []};
parse_auth(<<_/binary>>) ->
    invalid.

parse_urlencoded(S) ->
    parse_urlencoded(S, nokey, <<>>, key).

parse_urlencoded(<<$%, Hi, Lo, Tail/binary>>, Last, Cur,
		 State) ->
    Hex = list_to_integer([Hi, Lo], 16),
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

apply_custom_headers(Headers, CustomHeaders) ->
    {Doctype, Headers2} = case Headers -- [html] of
	Headers -> {[], Headers};
	Other -> {[html], Other}
    end,
    M = maps:merge(maps:from_list(Headers2),
		   maps:from_list(CustomHeaders)),
    Doctype ++ maps:to_list(M).

%%%--------------------------------
%%%-export([get_url/4, get_auto_url/2]).

get_url(M, bosh, Tls, Host) ->
    get_url(M, Tls, Host, bosh_service_url, mod_bosh);
get_url(M, websocket, Tls, Host) ->
    get_url(M, Tls, Host, websocket_url, ejabberd_http_ws);
get_url(M, Option, Tls, Host) ->
    get_url(M, Tls, Host, Option, M).

get_url(M, Tls, Host, Option, Handler) ->
    case get_url_preliminar(M, Tls, Host, Option, Handler) of
        undefined -> undefined;
        Url -> misc:expand_keyword(<<"@HOST@">>, Url, Host)
    end.

get_url_preliminar(M, Tls, Host, Option, Handler) ->
    case gen_mod:get_module_opt(Host, M, Option) of
        undefined -> undefined;
        auto -> get_auto_url(Tls, Handler);
        <<"auto">> -> get_auto_url(Tls, Handler);
        U when is_binary(U) -> U
    end.

get_auto_url(Tls, Handler) ->
    case find_handler_port_path(Tls, Handler) of
        [] -> undefined;
        [{ThisTls, Port, Path} | _] ->
            Protocol = case {ThisTls, Handler} of
                           {false, ejabberd_http_ws} -> <<"ws">>;
                           {true, ejabberd_http_ws} -> <<"wss">>;
                           {false, _} -> <<"http">>;
                           {true, _} -> <<"https">>
                       end,
            <<Protocol/binary,
              "://@HOST@:",
              (integer_to_binary(Port))/binary,
              "/",
              (str:join(Path, <<"/">>))/binary>>
    end.

find_handler_port_path(Tls, Handler) ->
    lists:filtermap(
      fun({{Port, _, _},
           ejabberd_http,
           #{tls := ThisTls, request_handlers := Handlers}})
            when is_integer(Port) and ((Tls == any) or (Tls == ThisTls)) ->
              case lists:keyfind(Handler, 2, Handlers) of
                  false -> false;
                  {Path, Handler} -> {true, {ThisTls, Port, Path}}
              end;
         (_) -> false
      end, ets:tab2list(ejabberd_listener)).


%%%--------------------------------

% The following code is mostly taken from yaws_ssl.erl

toupper(C) when C >= $a andalso C =< $z -> C - 32;
toupper(C) -> C.

tolower(C) when C >= $A andalso C =< $Z -> C + 32;
tolower(C) -> C.

normalize_header_name(Name) ->
    normalize_header_name(Name, [], true).

normalize_header_name(<<"">>, Acc, _) ->
    iolist_to_binary(Acc);
normalize_header_name(<<"-", Rest/binary>>, Acc, _) ->
    normalize_header_name(Rest, [Acc, "-"], true);
normalize_header_name(<<C:8, Rest/binary>>, Acc, true) ->
    normalize_header_name(Rest, [Acc, toupper(C)], false);
normalize_header_name(<<C:8, Rest/binary>>, Acc, false) ->
    normalize_header_name(Rest, [Acc, tolower(C)], false).

normalize_path(Path) ->
    normalize_path(Path, []).

normalize_path([], Norm) -> lists:reverse(Norm);
normalize_path([<<"..">>|Path], Norm) ->
    normalize_path(Path, Norm);
normalize_path([_Parent, <<"..">>|Path], Norm) ->
    normalize_path(Path, Norm);
normalize_path([Part | Path], Norm) ->
    normalize_path(Path, [Part|Norm]).

listen_opt_type(tag) ->
    econf:binary();
listen_opt_type(allow_unencrypted_sasl2) ->
    econf:bool();
listen_opt_type(request_handlers) ->
    econf:map(
      econf:and_then(
	econf:binary(),
	fun(Path) -> str:tokens(Path, <<"/">>) end),
      econf:beam([[{socket_handoff, 3}, {process, 2}]]));
listen_opt_type(custom_headers) ->
    econf:map(
      econf:binary(),
      econf:binary()).

listen_options() ->
    [{ciphers, undefined},
     {dhfile, undefined},
     {cafile, undefined},
     {protocol_options, undefined},
     {tls, false},
     {tls_compression, false},
     {allow_unencrypted_sasl2, false},
     {request_handlers, []},
     {tag, <<>>},
     {custom_headers, []}].
