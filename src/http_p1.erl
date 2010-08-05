%%%----------------------------------------------------------------------
%%% File    : http_p1.erl
%%% Author  : Emilio Bustos <ebustos@process-one.net>
%%% Purpose : Provide a common API for inets / lhttpc / ibrowse
%%% Created : 29 Jul 2010 by Emilio Bustos <ebustos@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
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

-module(http_p1).
-author('ebustos@process-one.net').

-export([
        start/0,
        stop/0,
        get/1,
        get/2,
        post/2,
        post/3,
        request/3,
        request/4,
        request/5
]).

% -define(USE_INETS, 1).
% -define(USE_LHTTPC, 1).
% -define(USE_IBROWSE, 1).
% inets used as default if none specified

-ifdef(USE_IBROWSE).
    -define(start(), start_ibrowse()).
    -define(request(M, U, H, B, O), request_ibrowse(M, U, H, B, O)).
    -define(stop(), stop_ibrowse()).
-else.
    -ifdef(USE_LHTTPC).
        -define(start(), start_lhttpc()).
        -define(request(M, U, H, B, O), request_lhttpc(M, U, H, B, O)).
        -define(stop(), stop_lhttpc()).
    -else.
        -define(start(), start_inets()).
        -define(request(M, U, H, B, O), request_inets(M, U, H, B, O)).
        -define(stop(), stop_inets()).
    -endif.
-endif.

-type header() :: {string() | atom(), string()}.
-type headers() :: [header()].

-type option() ::
        {connect_timeout, timeout()} |
        {timeout, timeout()} |

        {send_retry, non_neg_integer()} |
        {partial_upload, non_neg_integer() | infinity} |
        {partial_download, pid(), non_neg_integer() | infinity}.

-type options() :: [option()].

-type result() :: {ok, {{pos_integer(), string()}, headers(), string()}} |
    {error, atom()}.

%% @spec () -> ok | {error, Reason}
%%   Reason = term()
%% @doc
%% Start the application.
%% This is a helper function that will start the corresponding backend.
%% It allows the library to be started using the `-s' flag.
%% For instance:
%% `$ erl -s http_p1'
%%
%% @end
-spec start() -> ok | {error, any()}.
start() ->
    ?start().

start_inets()->
    inets:start(),
    ssl:start().

start_lhttpc()->
    application:start(crypto),
    application:start(ssl),
    lhttpc:start().

start_ibrowse()->
    ibrowse:start(),
    ssl:start().

%% @spec () -> ok | {error, Reason}
%%   Reason = term()
%% @doc
%% Stops the application.
%% This is a helper function that will stop the corresponding backend.
%%
%% @end
-spec stop() -> ok | {error, any()}.
stop() ->
    ?stop().

stop_inets()->
    inets:stop(),
    ssl:stop().

stop_lhttpc()->
    lhttpc:stop(),
    application:stop(ssl).

stop_ibrowse()->
    ibrowse:stop().

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
get(URL) ->
    request(get, URL, []).

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
get(URL, Hdrs) ->
    request(get, URL, Hdrs).

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
    request(post, URL, [{"content-type", "x-www-form-urlencoded"}], Body).

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
    NewHdrs = case [X || {X,_}<-Hdrs, string:to_lower(X) == "content-type"] of
            [] ->
            [{"content-type", "x-www-form-urlencoded"} | Hdrs];
        _ ->
            Hdrs
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
request(Method, URL, Hdrs, Body, Opts) ->
%    ?DEBUG("Making request with headers: ~p~n~n", [Hdrs]),
%    Headers = lists:map(fun({H, V}) ->
%                H2 = if
%                    is_atom(H) ->
%                        string:to_lower(atom_to_list(H));
%                    is_list(H) ->
%                        string:to_lower(H);
%                    true ->
%                        H
%                end,
%                {H2, V}
%        end, Hdrs),
    ?request(Method, URL, Hdrs, Body, Opts).

request_inets(Method, URL, Hdrs, Body, Opts) ->
    Request = case Method of
        get ->
            {URL, Hdrs};
        head ->
            {URL, Hdrs};
        _ -> % post, etc.
            {URL, Hdrs, proplists:get_value("content-type", Hdrs, []), Body}
    end,
    Options = case proplists:get_value(timeout, Opts, infinity) of
        infinity ->
            proplists:delete(timeout, Opts);
        _ ->
            Opts
    end,
    case http:request(Method, Request, Options, []) of
        {ok, {{_, Status, _}, Headers, Response}} ->
            {ok, Status, Headers, Response};
        {error, Reason} ->
            {error, Reason}
    end.

request_lhttpc(Method, URL, Hdrs, Body, Opts) ->
    TimeOut = proplists:get_value(timeout, Opts, infinity),
    SockOpt = proplists:get_value(socket_options, Opts, []),
    Options = [{connect_options, SockOpt} | proplists:delete(timeout, Opts)],
    case lhttpc:request(URL, Method, Hdrs, Body, TimeOut, Options) of
        {ok, {{Status, _Reason}, Headers, Response}} ->
            {ok, Status, Headers, binary_to_list(Response)};
        {error, Reason} ->
            {error, Reason}
    end.

request_ibrowse(Method, URL, Hdrs, Body, Opts) ->
    TimeOut = proplists:get_value(timeout, Opts, infinity),
    Options = [{inactivity_timeout, TimeOut} | proplists:delete(timeout, Opts)],
    case ibrowse:send_req(URL, Hdrs, Method, Body, Options) of
        {ok, Status, Headers, Response} ->
            {ok, list_to_integer(Status), Headers, Response};
        {error, Reason} ->
            {error, Reason}
    end.

% ibrowse {response_format, response_format()} |
% Options - [option()]
%                     Option - {sync, boolean()} | {stream, StreamTo} | {body_format, body_format()} | {full_result,
%                     boolean()} | {headers_as_is, boolean()}
%body_format() = string() | binary()
%                       The body_format option is only valid for the synchronous request and the default is  string.
%                     When making an asynchronous request the body will always be received as a binary.
% lhttpc: always binary
