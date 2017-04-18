%%%----------------------------------------------------------------------
%%% File    : ejabberd_http.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 27 Feb 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

%% External exports
-export([start/2, start_link/2, become_controller/1,
	 socket_type/0, receive_headers/1, url_encode/1,
         transform_listen_option/2]).

-export([init/2, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

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
		default_host,
		custom_headers,
		trail = <<>>,
		addr_re
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
    {ok,
     proc_lib:spawn(ejabberd_http, init,
		    [SockData, Opts])}.

start_link(SockData, Opts) ->
    {ok,
     proc_lib:spawn_link(ejabberd_http, init,
			 [SockData, Opts])}.

init({SockMod, Socket}, Opts) ->
    TLSEnabled = proplists:get_bool(tls, Opts),
    TLSOpts1 = lists:filter(fun ({certfile, _}) -> true;
				({ciphers, _}) -> true;
				({dhfile, _}) -> true;
				(_) -> false
			    end,
			    Opts),
    TLSOpts2 = case lists:keysearch(protocol_options, 1, Opts) of
                   {value, {_, O}} ->
                       [_|ProtocolOptions] = lists:foldl(
                                    fun(X, Acc) -> X ++ Acc end, [],
                                    [["|" | binary_to_list(Opt)] || Opt <- O, is_binary(Opt)]
                                   ),
                        [{protocol_options, iolist_to_binary(ProtocolOptions)} | TLSOpts1];
                   _ -> TLSOpts1
               end,
    TLSOpts3 = case proplists:get_bool(tls_compression, Opts) of
                   false -> [compression_none | TLSOpts2];
                   true -> TLSOpts2
               end,
    TLSOpts = [verify_none | TLSOpts3],
    {SockMod1, Socket1} = if TLSEnabled ->
				 inet:setopts(Socket, [{recbuf, 8192}]),
				 {ok, TLSSocket} = fast_tls:tcp_to_tls(Socket,
								  TLSOpts),
				 {fast_tls, TLSSocket};
			     true -> {SockMod, Socket}
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
	     true -> [{[<<"http-bind">>], mod_bosh}];
             false -> []
           end,
    XMLRPC = case proplists:get_bool(xmlrpc, Opts) of
		 true -> [{[], ejabberd_xmlrpc}];
		 false -> []
	     end,
    DefinedHandlers = gen_mod:get_opt(
                        request_handlers, Opts,
                        fun(Hs) ->
                                Hs1 = lists:map(fun
                                  ({Mod, Path}) when is_atom(Mod) -> {Path, Mod};
                                  ({Path, Mod}) -> {Path, Mod}
                                end, Hs),

				Hs2 = [{str:tokens(
					  iolist_to_binary(Path), <<"/">>),
					Mod} || {Path, Mod} <- Hs1],
				[{Path,
				  case Mod of
				      mod_http_bind -> mod_bosh;
				      _ -> Mod
				  end} || {Path, Mod} <- Hs2]
                        end, []),
    RequestHandlers = DefinedHandlers ++ Captcha ++ Register ++
        Admin ++ Bind ++ XMLRPC,
    ?DEBUG("S: ~p~n", [RequestHandlers]),

    DefaultHost = gen_mod:get_opt(default_host, Opts, fun(A) -> A end, undefined),
    {ok, RE} = re:compile(<<"^(?:\\[(.*?)\\]|(.*?))(?::(\\d+))?$">>),

    CustomHeaders = gen_mod:get_opt(custom_headers, Opts,
				    fun expand_custom_headers/1,
				    []),

    ?INFO_MSG("started: ~p", [{SockMod1, Socket1}]),
    State = #state{sockmod = SockMod1,
                   socket = Socket1,
                   default_host = DefaultHost,
		   custom_headers = CustomHeaders,
		   options = Opts,
		   request_handlers = RequestHandlers,
		   addr_re = RE},
    try receive_headers(State) of
        V -> V
    catch
        {error, _} -> State
    end.

become_controller(_Pid) ->
    ok.

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
    case Data of
        {error, _} -> ok;
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
      {ok, {http_header, _, 'Host' = Name, _, Host}} ->
	  State#state{request_host = Host,
		      request_headers = add_header(Name, Host, State)};
      {ok, {http_header, _, Name, _, Value}} when is_binary(Name) ->
	  State#state{request_headers =
			  add_header(normalize_header_name(Name), Value, State)};
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
	  ?DEBUG("(~w) http query: ~w ~p~n",
		 [State#state.socket, State#state.request_method,
		  element(2, State#state.request_path)]),
	  {HostProvided, Port, TP} =
	      get_transfer_protocol(State#state.addr_re, SockMod,
				    State#state.request_host),
	  Host = get_host_really_served(State#state.default_host,
					HostProvided),
	  State2 = State#state{request_host = Host,
			       request_port = Port, request_tp = TP},
	  {State3, Out} = process_request(State2),
	  send_text(State3, Out),
	  case State3#state.request_keepalive of
	    true ->
		#state{sockmod = SockMod, socket = Socket,
		       trail = State3#state.trail,
		       options = State#state.options,
		       default_host = State#state.default_host,
		       custom_headers = State#state.custom_headers,
		       request_handlers = State#state.request_handlers,
		       addr_re = State#state.addr_re};
	    _ ->
		#state{end_of_request = true,
		       trail = State3#state.trail,
		       options = State#state.options,
		       default_host = State#state.default_host,
		       custom_headers = State#state.custom_headers,
		       request_handlers = State#state.request_handlers,
		       addr_re = State#state.addr_re}
	  end;
      _ ->
	  #state{end_of_request = true,
		 options = State#state.options,
		 default_host = State#state.default_host,
		 custom_headers = State#state.custom_headers,
		 request_handlers = State#state.request_handlers,
		 addr_re = State#state.addr_re}
    end.

add_header(Name, Value, State)->
    [{Name, Value} | State#state.request_headers].

get_host_really_served(undefined, Provided) ->
    Provided;
get_host_really_served(Default, Provided) ->
    case ejabberd_router:is_my_host(Provided) of
      true -> Provided;
      false -> Default
    end.

get_transfer_protocol(RE, SockMod, HostPort) ->
    {Proto, DefPort} = case SockMod of
			   gen_tcp -> {http, 80};
			   fast_tls -> {https, 443}
		       end,
    {Host, Port} = case re:run(HostPort, RE, [{capture,[1,2,3],binary}]) of
		       nomatch ->
			   {<<"0.0.0.0">>, DefPort};
		       {match, [<<>>, H, <<>>]} ->
			   {H, DefPort};
		       {match, [H, <<>>, <<>>]} ->
			   {H, DefPort};
		       {match, [<<>>, H, PortStr]} ->
			   {H, binary_to_integer(PortStr)};
		       {match, [H, <<>>, PortStr]} ->
			   {H, binary_to_integer(PortStr)}
		   end,

    {Host, Port, Proto}.

%% XXX bard: search through request handlers looking for one that
%% matches the requested URL path, and pass control to it.  If none is
%% found, answer with HTTP 404.

process([], _, _, _, _) -> ejabberd_web:error(not_found);
process(Handlers, Request, Socket, SockMod, Trail) ->
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
	    R = try
		    HandlerModule:socket_handoff(
		      LocalPath, Request, Socket, SockMod, Trail, HandlerOpts)
		catch error:undef ->
			HandlerModule:process(LocalPath, Request)
		end,
            ejabberd_hooks:run(http_request_debug, [{LocalPath, Request}]),
            R;
        false ->
	    process(HandlersLeft, Request, Socket, SockMod, Trail)
    end.

extract_path_query(#state{request_method = Method,
			  request_path = {abs_path, Path}} = State)
    when Method =:= 'GET' orelse
	   Method =:= 'HEAD' orelse
	     Method =:= 'DELETE' orelse Method =:= 'OPTIONS' ->
    case catch url_decode_q_split(Path) of
	{'EXIT', _} -> {State, false};
	{NPath, Query} ->
	    LPath = normalize_path([NPE
				    || NPE <- str:tokens(path_decode(NPath), <<"/">>)]),
	    LQuery = case catch parse_urlencoded(Query) of
			 {'EXIT', _Reason} -> [];
			 LQ -> LQ
		     end,
	    {State, {LPath, LQuery, <<"">>}}
    end;
extract_path_query(#state{request_method = Method,
			  request_path = {abs_path, Path},
			  request_content_length = Len,
			  sockmod = _SockMod,
			  socket = _Socket} = State)
    when (Method =:= 'POST' orelse Method =:= 'PUT') andalso
	   is_integer(Len) ->
    case recv_data(State, Len) of
	error -> {State, false};
	{NewState, Data} ->
    ?DEBUG("client data: ~p~n", [Data]),
    case catch url_decode_q_split(Path) of
        {'EXIT', _} -> {NewState, false};
        {NPath, _Query} ->
            LPath = normalize_path([NPE
                                    || NPE <- str:tokens(path_decode(NPath), <<"/">>)]),
            LQuery = case catch parse_urlencoded(Data) of
                         {'EXIT', _Reason} -> [];
                         LQ -> LQ
                     end,
            {NewState, {LPath, LQuery, Data}}
	    end
    end;
extract_path_query(State) ->
    {State, false}.

process_request(#state{request_method = Method,
		       request_auth = Auth,
		       request_lang = Lang,
		       sockmod = SockMod,
		       socket = Socket,
		       options = Options,
		       request_host = Host,
		       request_port = Port,
		       request_tp = TP,
		       request_headers = RequestHeaders,
		       request_handlers = RequestHandlers,
		       custom_headers = CustomHeaders,
		       trail = Trail} = State) ->
    case extract_path_query(State) of
	{State2, false} ->
	    {State2, make_bad_request(State)};
	{State2, {LPath, LQuery, Data}} ->
	    PeerName =
		case SockMod of
		    gen_tcp ->
			inet:peername(Socket);
		    _ ->
			SockMod:peername(Socket)
		end,
            IPHere = case PeerName of
                         {ok, V} -> V;
                         {error, _} = E -> throw(E)
                     end,
	    XFF = proplists:get_value('X-Forwarded-For', RequestHeaders, []),
	    IP = analyze_ip_xff(IPHere, XFF, Host),
            Request = #request{method = Method,
                               path = LPath,
                               q = LQuery,
                               auth = Auth,
                               data = Data,
                               lang = Lang,
                               host = Host,
                               port = Port,
                               tp = TP,
			       opts = Options,
                               headers = RequestHeaders,
                               ip = IP},
	    Res = case process(RequestHandlers, Request, Socket, SockMod, Trail) of
		      El when is_record(El, xmlel) ->
			  make_xhtml_output(State, 200, CustomHeaders, El);
		      {Status, Headers, El}
			when is_record(El, xmlel) ->
			  make_xhtml_output(State, Status,
					    Headers ++ CustomHeaders, El);
		      Output when is_binary(Output) or is_list(Output) ->
			  make_text_output(State, 200, CustomHeaders, Output);
		      {Status, Headers, Output}
			when is_binary(Output) or is_list(Output) ->
			  make_text_output(State, Status,
					   Headers ++ CustomHeaders, Output);
		      {Status, Reason, Headers, Output}
			when is_binary(Output) or is_list(Output) ->
			  make_text_output(State, Status, Reason,
					   Headers ++ CustomHeaders, Output);
		      _ ->
			  none
		  end,
	    {State2, Res}
    end.

make_bad_request(State) ->
    make_xhtml_output(State, 400, State#state.custom_headers,
		      ejabberd_web:make_xhtml([#xmlel{name = <<"h1">>,
						      attrs = [],
						      children =
							  [{xmlcdata,
							    <<"400 Bad Request">>}]}])).

analyze_ip_xff(IP, [], _Host) -> IP;
analyze_ip_xff({IPLast, Port}, XFF, Host) ->
    [ClientIP | ProxiesIPs] = str:tokens(XFF, <<", ">>) ++
				[misc:ip_to_list(IPLast)],
    TrustedProxies = ejabberd_config:get_option(
                       {trusted_proxies, Host},
                       fun(all) -> all;
                          (TPs) ->
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

recv_data(State, 0, Acc) -> {State, Acc};
recv_data(#state{trail = Trail} = State, Len, <<>>) when byte_size(Trail) > Len ->
    <<Data:Len/binary, Rest/binary>> = Trail,
    {State#state{trail = Rest}, Data};
recv_data(State, Len, Acc) ->
    case State#state.trail of
	<<>> ->
	    case (State#state.sockmod):recv(State#state.socket,
					    min(Len, 16#4000000), 300000)
	    of
		{ok, Data} ->
		    recv_data(State, Len - byte_size(Data), <<Acc/binary, Data/binary>>);
		Err ->
		    ?DEBUG("Cannot receive HTTP data: ~p", [Err]),
		    error
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
		    fxml:element_to_binary(XHTML)]);
	_ ->
	    iolist_to_binary([?XHTML_DOCTYPE,
		    fxml:element_to_binary(XHTML)])
    end,
    Headers1 = case lists:keysearch(<<"Content-Type">>, 1,
				    Headers)
		   of
		 {value, _} ->
		     [{<<"Content-Length">>,
		       integer_to_binary(byte_size(Data))}
		      | Headers];
		 _ ->
		     [{<<"Content-Type">>, <<"text/html; charset=utf-8">>},
		      {<<"Content-Length">>,
		       integer_to_binary(byte_size(Data))}
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
	  integer_to_binary(Status), <<" ">>,
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
		       integer_to_binary(byte_size(Data))}
		      | Headers];
		 _ ->
		     [{<<"Content-Type">>, <<"text/html; charset=utf-8">>},
		      {<<"Content-Length">>,
		       integer_to_binary(byte_size(Data))}
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
	  integer_to_binary(Status), <<" ">>,
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

expand_custom_headers(Headers) ->
    lists:map(fun({K, V}) ->
		      {K, misc:expand_keyword(<<"@VERSION@">>, V, ?VERSION)}
	      end, Headers).

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

-spec parse_auth(binary()) -> {binary(), binary()} | {oauth, binary(), []} | undefined.
parse_auth(<<"Basic ", Auth64/binary>>) ->
    Auth = misc:decode_base64(Auth64),
    %% Auth should be a string with the format: user@server:password
    %% Note that password can contain additional characters '@' and ':'
    case str:chr(Auth, $:) of
        0 ->
            undefined;
        Pos ->
            {User, <<$:, Pass/binary>>} = erlang:split_binary(Auth, Pos-1),
            PassUtf8 = unicode:characters_to_binary(binary_to_list(Pass), utf8),
            {User, PassUtf8}
    end;
parse_auth(<<"Bearer ", SToken/binary>>) ->
    Token = str:strip(SToken),
    {oauth, Token, []};
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

transform_listen_option(captcha, Opts) ->
    [{captcha, true}|Opts];
transform_listen_option(register, Opts) ->
    [{register, true}|Opts];
transform_listen_option(web_admin, Opts) ->
    [{web_admin, true}|Opts];
transform_listen_option(http_bind, Opts) ->
    [{http_bind, true}|Opts];
transform_listen_option(http_poll, Opts) ->
    Opts;
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

opt_type(trusted_proxies) ->
    fun (all) -> all;
        (TPs) -> [iolist_to_binary(TP) || TP <- TPs] end;
opt_type(_) -> [trusted_proxies].
