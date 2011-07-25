%%%
%%%  Copyright (C) Nicolas Niclausse 2008
%%%
%%%  Author : Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%  Created: 31 Mar 2008 by Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

-module(ts_proxy_webdav).
-vc('$Id: ts_proxy_webdav.erl 822 2008-03-31 13:18:34Z nniclausse $ ').
-author('Nicolas.Niclausse@niclux.org').

-include("ts_profile.hrl").
-include("ts_http.hrl").
-include("ts_recorder.hrl").


-export([parse/4, record_request/2, socket_opts/0]).
-export([gettype/0]).

-export([rewrite_serverdata/1]).
-export([rewrite_ssl/1]).

%%--------------------------------------------------------------------
%% Func: socket_opts/0
%%--------------------------------------------------------------------
socket_opts() -> [{packet, 0}].

%%--------------------------------------------------------------------
%% Func: gettype/0
%%--------------------------------------------------------------------
gettype() -> "ts_webdav".

%%--------------------------------------------------------------------
%% Func: rewrite_serverdata/1
%%--------------------------------------------------------------------
rewrite_serverdata(Data)->
    ts_utils:from_https(Data).

%%--------------------------------------------------------------------
%% Func: rewrite_ssl/1
%%--------------------------------------------------------------------
rewrite_ssl(Data)->
    ts_utils:to_https(Data).

%%--------------------------------------------------------------------
%% Func: parse/4
%% Purpose: parse HTTP/WEBDAV request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
parse(State,ClientSocket,ServerSocket,String) ->
    ts_proxy_http:parse(State, ClientSocket,ServerSocket,String).

%%--------------------------------------------------------------------
%% Func: record_http_request/2
%% Purpose: record request given State=#state_rec and Request=#http_request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
record_request(State=#state_rec{prev_host=Host, prev_port=Port, prev_scheme=Scheme},
                    #http_request{method  = Method, url = RequestURI,
                                  version = HTTPVersion,
                                  headers = ParsedHeader,body=Body}) ->

    FullURL = ts_utils:to_https({url, RequestURI}),

    {URL,NewPort,NewHost, NewScheme} =
        case ts_config_http:parse_URL(FullURL) of
            #url{path=RelURL,host=Host,port=Port,querypart=[],scheme=Scheme}->
                {RelURL, Port, Host, Scheme};
            #url{path=RelURL,host=Host,port=Port,querypart=Args,scheme=Scheme}->
                {RelURL++"?"++Args, Port, Host, Scheme};
            #url{host=Host2,port=Port2,scheme=Sc2}->
                {FullURL,Port2,Host2,Sc2 }
        end,
    Fd = State#state_rec.logfd,
    URL2 = ts_utils:export_text(URL),
    io:format(Fd,"<request><http url='~s' version='~s' ", [URL2, HTTPVersion]),
    case Body of
        [] -> ok;
        _  ->
            Body2 = ts_utils:export_text(Body),
            io:format(Fd," contents='~s' ", [Body2]) % must be a POST method
    end,

    %% Content-type recording (This is useful for SOAP post for example):
    ts_proxy_http:record_header(Fd,ParsedHeader,"content-type", "content_type='~s' "),
    ts_proxy_http:record_header(Fd,ParsedHeader,"if-modified-since", "if_modified_since='~s' "),

    io:format(Fd,"method='~s'>", [Method]),

    %% authentication
    ts_proxy_http:record_header(Fd,ParsedHeader,"authorization",
                  "~n  <www_authenticate userid=~p passwd=~p />"),
     %% webdav
    ts_proxy_http:record_header(Fd,ParsedHeader,"depth", "~n  <http_header name='depth' value='~s'/>~n"),
    ts_proxy_http:record_header(Fd,ParsedHeader,"if", "~n  <http_header name='if' value='~s'/>~n"),
    ts_proxy_http:record_header(Fd,ParsedHeader,"timeout", "~n  <http_header name='timeout' value='~s'/>~n"),
    ts_proxy_http:record_header(Fd,ParsedHeader,"overwrite", "~n  <http_header name='overwrite' value='~s'/>~n"),
    ts_proxy_http:record_header(Fd,ParsedHeader,"destination", "~n  <http_header name='destination' value='~s'/>~n", fun(A) -> ts_utils:to_https({url, A}) end),
    ts_proxy_http:record_header(Fd,ParsedHeader,"url", "~n  <http_header name='url' value='~s'/>~n"),
    ts_proxy_http:record_header(Fd,ParsedHeader,"lock-token", "~n  <http_header name='lock-token' value='~s'/>~n"),
    ts_proxy_http:record_header(Fd,ParsedHeader,"x-svn-options", "~n  <http_header name='x-svn-options' value='~s'/>~n"),

    %% subversion use x-svn-result-fulltext-md5 ; add this ?
    %% http://svn.collab.net/repos/svn/branches/artem-soc-work/notes/webdav-protocol

    io:format(Fd,"</http></request>~n",[]),
    {ok,State#state_rec{prev_port=NewPort,prev_host=NewHost,prev_scheme=NewScheme}}.

