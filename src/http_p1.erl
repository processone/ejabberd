%%%----------------------------------------------------------------------
%%% File    : http_p1.erl
%%% Author  : Emilio Bustos <ebustos@process-one.net>
%%% Purpose : Provide a common API for inets / lhttpc / ibrowse
%%% Created : 29 Jul 2010 by Emilio Bustos <ebustos@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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

-module(http_p1).

-author('ebustos@process-one.net').

-export([start/0, stop/0, get/1, get/2, post/2, post/3,
	 request/3, request/4, request/5,
	 get_pool_size/0, set_pool_size/1]).

-include("logger.hrl").

-define(USE_INETS, 1).
% -define(USE_LHTTPC, 1).
% -define(USE_IBROWSE, 1).
% inets used as default if none specified

-ifdef(USE_IBROWSE).

start() ->
    ejabberd:start_app(ibrowse).

stop() ->
    application:stop(ibrowse).

request(Method, URL, Hdrs, Body, Opts) ->
    TimeOut = proplists:get_value(timeout, Opts, infinity),
    Options = [{inactivity_timeout, TimeOut}
	       | proplists:delete(timeout, Opts)],
    case ibrowse:send_req(URL, Hdrs, Method, Body, Options)
	of
      {ok, Status, Headers, Response} ->
	  {ok, jlib:binary_to_integer(Status), Headers,
	   Response};
      {error, Reason} -> {error, Reason}
    end.

get_pool_size() ->
    application:get_env(ibrowse, default_max_sessions, 10).

set_pool_size(Size) ->
    application:set_env(ibrowse, default_max_sessions, Size).

-else.

-ifdef(USE_LHTTPC).

start() ->
    ejabberd:start_app(lhttpc).

stop() ->
    application:stop(lhttpc).

request(Method, URL, Hdrs, Body, Opts) ->
    {[TO, SO], Rest} = proplists:split(Opts, [timeout, socket_options]),
    TimeOut = proplists:get_value(timeout, TO, infinity),
    SockOpt = proplists:get_value(socket_options, SO, []),
    Options = [{connect_options, SockOpt} | Rest],
    Result = lhttpc:request(URL, Method, Hdrs, Body, TimeOut, Options),
    ?DEBUG("HTTP request -> response:~n"
	   "** Method = ~p~n"
	   "** URI = ~s~n"
	   "** Body = ~s~n"
	   "** Hdrs = ~p~n"
	   "** Timeout = ~p~n"
	   "** Options = ~p~n"
	   "** Response = ~p",
	   [Method, URL, Body, Hdrs, TimeOut, Options, Result]),
    case Result of
      {ok, {{Status, _Reason}, Headers, Response}} ->
	  {ok, Status, Headers, (Response)};
      {error, Reason} -> {error, Reason}
    end.

get_pool_size() ->
    Opts = proplists:get_value(lhttpc_manager, lhttpc_manager:list_pools()),
    proplists:get_value(max_pool_size,Opts).

set_pool_size(Size) ->
    lhttpc_manager:set_max_pool_size(lhttpc_manager, Size).

-else.

start() ->
    ejabberd:start_app(inets).

stop() ->
    application:stop(inets).

to_list(Str) when is_binary(Str) ->
    binary_to_list(Str);
to_list(Str) ->
    Str.

request(Method, URLRaw, HdrsRaw, Body, Opts) ->
    Hdrs = lists:map(fun({N, V}) ->
                             {to_list(N), to_list(V)}
                     end, HdrsRaw),
    URL = to_list(URLRaw),

    Request = case Method of
		get -> {URL, Hdrs};
		head -> {URL, Hdrs};
		delete -> {URL, Hdrs};
		_ -> % post, etc.
		    {URL, Hdrs,
		     to_list(proplists:get_value(<<"content-type">>, HdrsRaw, [])),
                     Body}
	      end,
    Options = case proplists:get_value(timeout, Opts,
				       infinity)
		  of
		infinity -> proplists:delete(timeout, Opts);
		_ -> Opts
	      end,
    case httpc:request(Method, Request, Options, []) of
      {ok, {{_, Status, _}, Headers, Response}} ->
	  {ok, Status, Headers, Response};
      {error, Reason} -> {error, Reason}
    end.

get_pool_size() ->
    {ok, Size} = httpc:get_option(max_sessions),
    Size.

set_pool_size(Size) ->
    httpc:set_option(max_sessions, Size).

-endif.

-endif.

-type({header,
       {type, 63, tuple,
	[{type, 63, union,
	  [{type, 63, string, []}, {type, 63, atom, []}]},
	 {type, 63, string, []}]},
       []}).

-type({headers,
       {type, 64, list, [{type, 64, header, []}]}, []}).

-type({option,
       {type, 67, union,
	[{type, 67, tuple,
	  [{atom, 67, connect_timeout}, {type, 67, timeout, []}]},
	 {type, 68, tuple,
	  [{atom, 68, timeout}, {type, 68, timeout, []}]},
	 {type, 70, tuple,
	  [{atom, 70, send_retry},
	   {type, 70, non_neg_integer, []}]},
	 {type, 71, tuple,
	  [{atom, 71, partial_upload},
	   {type, 71, union,
	    [{type, 71, non_neg_integer, []},
	     {atom, 71, infinity}]}]},
	 {type, 72, tuple,
	  [{atom, 72, partial_download}, {type, 72, pid, []},
	   {type, 72, union,
	    [{type, 72, non_neg_integer, []},
	     {atom, 72, infinity}]}]}]},
       []}).

-type({options,
       {type, 74, list, [{type, 74, option, []}]}, []}).

-type({result,
       {type, 76, union,
	[{type, 76, tuple,
	  [{atom, 76, ok},
	   {type, 76, tuple,
	    [{type, 76, tuple,
	      [{type, 76, pos_integer, []}, {type, 76, string, []}]},
	     {type, 76, headers, []}, {type, 76, string, []}]}]},
	 {type, 77, tuple,
	  [{atom, 77, error}, {type, 77, atom, []}]}]},
       []}).

%% @spec (URL) -> Result
%%   URL = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a GET request.
%% Would be the same as calling `request(get, URL, [])',
%% that is {@link request/3} with an empty header list.
%% @end
%% @see request/3
-spec get(string()) -> result().
get(URL) -> request(get, URL, []).

%% @spec (URL, Hdrs) -> Result
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a GET request.
%% Would be the same as calling `request(get, URL, Hdrs)'.
%% @end
%% @see request/3
-spec get(string(), headers()) -> result().
get(URL, Hdrs) -> request(get, URL, Hdrs).

%% @spec (URL, RequestBody) -> Result
%%   URL = string()
%%   RequestBody = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a POST request with form data.
%% Would be the same as calling
%% `request(post, URL, [{"content-type", "x-www-form-urlencoded"}], Body)'.
%% @end
%% @see request/4
-spec post(string(), string()) -> result().
post(URL, Body) ->
    request(post, URL,
	    [{<<"content-type">>, <<"x-www-form-urlencoded">>}],
	    Body).

%% @spec (URL, Hdrs, RequestBody) -> Result
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   RequestBody = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a POST request.
%% Would be the same as calling
%% `request(post, URL, Hdrs, Body)'.
%% @end
%% @see request/4
-spec post(string(), headers(), string()) -> result().
post(URL, Hdrs, Body) ->
    NewHdrs = case [X
		    || {X, _} <- Hdrs,
		       str:to_lower(X) == <<"content-type">>]
		  of
		[] ->
		    [{<<"content-type">>, <<"x-www-form-urlencoded">>}
		     | Hdrs];
		_ -> Hdrs
	      end,
    request(post, URL, NewHdrs, Body).

%% @spec (Method, URL, Hdrs) -> Result
%%   Method = atom()
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request without a body.
%% Would be the same as calling `request(Method, URL, Hdrs, [], [])',
%% that is {@link request/5} with an empty body.
%% @end
%% @see request/5
-spec request(atom(), string(), headers()) -> result().
request(Method, URL, Hdrs) ->
    request(Method, URL, Hdrs, [], []).

%% @spec (Method, URL, Hdrs, RequestBody) -> Result
%%   Method = atom()
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   RequestBody = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%% Would be the same as calling
%% `request(Method, URL, Hdrs, Body, [])', that is {@link request/5}
%% with no options.
%% @end
%% @see request/5
-spec request(atom(), string(), headers(), string()) -> result().
request(Method, URL, Hdrs, Body) ->
    request(Method, URL, Hdrs, Body, []).

%% @spec (Method, URL, Hdrs, RequestBody, Options) -> Result
%%   Method = atom()
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   RequestBody = string()
%%   Options = [Option]
%%   Option = {timeout, Milliseconds | infinity} |
%%            {connect_timeout, Milliseconds | infinity} |
%%            {socket_options, [term()]} |

%%   Milliseconds = integer()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%% Would be the same as calling
%% `request(Method, URL, Hdrs, Body, [])', that is {@link request/5}
%% with no options.
%% @end
%% @see request/5
-spec request(atom(), string(), headers(), string(), options()) -> result().

% ibrowse {response_format, response_format()} |
% Options - [option()]
%                     Option - {sync, boolean()} | {stream, StreamTo} | {body_format, body_format()} | {full_result,
%                     boolean()} | {headers_as_is, boolean()}
%body_format() = string() | binary()
%                       The body_format option is only valid for the synchronous request and the default is  string.
%                     When making an asynchronous request the body will always be received as a binary.
% lhttpc: always binary

