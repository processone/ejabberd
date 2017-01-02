%%%----------------------------------------------------------------------
%%% File    : rest.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Generic REST client
%%% Created : 16 Oct 2014 by Christophe Romain <christophe.romain@process-one.net>
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

-module(rest).

-behaviour(ejabberd_config).

-export([start/1, stop/1, get/2, get/3, post/4, delete/2,
	 put/4, patch/4, request/6, with_retry/4, opt_type/1]).

-include("logger.hrl").

-define(HTTP_TIMEOUT, 10000).
-define(CONNECT_TIMEOUT, 8000).

start(Host) ->
    p1_http:start(),
    Pool_size =
	ejabberd_config:get_option({ext_api_http_pool_size, Host},
				   fun(X) when is_integer(X), X > 0->
					   X
				   end,
				   100),
    p1_http:set_pool_size(Pool_size).

stop(_Host) ->
    ok.

with_retry(Method, Args, MaxRetries, Backoff) ->
    with_retry(Method, Args, 0, MaxRetries, Backoff).
with_retry(Method, Args, Retries, MaxRetries, Backoff) ->
    case apply(?MODULE, Method, Args) of
        %% Only retry on timeout errors
        {error, {http_error,{error,Error}}}
           when Retries < MaxRetries
           andalso (Error == 'timeout' orelse Error == 'connect_timeout') ->
            timer:sleep(round(math:pow(2, Retries)) * Backoff),
            with_retry(Method, Args, Retries+1, MaxRetries, Backoff);
        Result ->
            Result
    end.

get(Server, Path) ->
    request(Server, get, Path, [], "application/json", <<>>).
get(Server, Path, Params) ->
    request(Server, get, Path, Params, "application/json", <<>>).

delete(Server, Path) ->
    request(Server, delete, Path, [], "application/json", <<>>).

post(Server, Path, Params, Content) ->
    Data = encode_json(Content),
    request(Server, post, Path, Params, "application/json", Data).

put(Server, Path, Params, Content) ->
    Data = encode_json(Content),
    request(Server, put, Path, Params, "application/json", Data).

patch(Server, Path, Params, Content) ->
    Data = encode_json(Content),
    request(Server, patch, Path, Params, "application/json", Data).

request(Server, Method, Path, Params, Mime, Data) ->
    URI = url(Server, Path, Params),
    Opts = [{connect_timeout, ?CONNECT_TIMEOUT},
            {timeout, ?HTTP_TIMEOUT}],
    Hdrs = [{"connection", "keep-alive"},
            {"content-type", Mime},
            {"User-Agent", "ejabberd"}],
    Begin = os:timestamp(),
    Result = case catch p1_http:request(Method, URI, Hdrs, Data, Opts) of
        {ok, Code, _, <<>>} ->
            {ok, Code, []};
        {ok, Code, _, <<" ">>} ->
            {ok, Code, []};
        {ok, Code, _, <<"\r\n">>} ->
            {ok, Code, []};
        {ok, Code, _, Body} ->
            try jiffy:decode(Body) of
                JSon ->
                    {ok, Code, JSon}
            catch
                _:Error ->
                    ?ERROR_MSG("HTTP response decode failed:~n"
                               "** URI = ~s~n"
                               "** Body = ~p~n"
                               "** Err = ~p",
                               [URI, Body, Error]),
                    {error, {invalid_json, Body}}
            end;
        {error, Reason} ->
            ?ERROR_MSG("HTTP request failed:~n"
                       "** URI = ~s~n"
                       "** Err = ~p",
                       [URI, Reason]),
            {error, {http_error, {error, Reason}}};
        {'EXIT', Reason} ->
            ?ERROR_MSG("HTTP request failed:~n"
                       "** URI = ~s~n"
                       "** Err = ~p",
                       [URI, Reason]),
            {error, {http_error, {error, Reason}}}
    end,
    ejabberd_hooks:run(backend_api_call, Server, [Server, Method, Path]),
    case Result of
        {ok, _, _} ->
            End = os:timestamp(),
            Elapsed = timer:now_diff(End, Begin) div 1000, %% time in ms
            ejabberd_hooks:run(backend_api_response_time, Server,
			       [Server, Method, Path, Elapsed]);
        {error, {http_error,{error,timeout}}} ->
            ejabberd_hooks:run(backend_api_timeout, Server,
			       [Server, Method, Path]);
        {error, {http_error,{error,connect_timeout}}} ->
            ejabberd_hooks:run(backend_api_timeout, Server,
			       [Server, Method, Path]);
        {error, _} ->
            ejabberd_hooks:run(backend_api_error, Server,
			       [Server, Method, Path])
    end,
    Result.

%%%----------------------------------------------------------------------
%%% HTTP helpers
%%%----------------------------------------------------------------------

encode_json(Content) ->
    case catch jiffy:encode(Content) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("HTTP content encodage failed:~n"
                       "** Content = ~p~n"
                       "** Err = ~p",
                       [Content, Reason]),
            <<>>;
        Encoded ->
            Encoded
    end.

base_url(Server, Path) ->
    Tail = case iolist_to_binary(Path) of
        <<$/, Ok/binary>> -> Ok;
        Ok -> Ok
    end,
    case Tail of
        <<"http", _Url/binary>> -> Tail;
        _ ->
            Base = ejabberd_config:get_option({ext_api_url, Server},
                                              fun(X) ->
						      iolist_to_binary(X)
					      end,
                                              <<"http://localhost/api">>),
            <<Base/binary, "/", Tail/binary>>
    end.

url(Server, Path, []) ->
    binary_to_list(base_url(Server, Path));
url(Server, Path, Params) ->
    Base = base_url(Server, Path),
    [<<$&, ParHead/binary>> | ParTail] =
        [<<"&", (iolist_to_binary(Key))/binary, "=",
	  (ejabberd_http:url_encode(Value))/binary>>
            || {Key, Value} <- Params],
    Tail = iolist_to_binary([ParHead | ParTail]),
    binary_to_list(<<Base/binary, $?, Tail/binary>>).

opt_type(ext_api_http_pool_size) ->
    fun (X) when is_integer(X), X > 0 -> X end;
opt_type(ext_api_url) ->
    fun (X) -> iolist_to_binary(X) end;
opt_type(_) -> [ext_api_http_pool_size, ext_api_url].
