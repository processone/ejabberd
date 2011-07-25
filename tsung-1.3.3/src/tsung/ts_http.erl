%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
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

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

-module(ts_http).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").
-include("ts_http.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/1,
         session_defaults/0,
         parse/2,
         parse_config/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (ack_type and persistent)
%% Returns: {ok, "parse"|"no_ack"|"local", "true"|"false"}
%%----------------------------------------------------------------------
session_defaults() ->
    %% we parse the server response, and continue if the tcp
    %% connection is closed
    {ok, true}.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #http{}.

%%----------------------------------------------------------------------
%% Function: get_message/21
%% Purpose: Build a message/request ,
%% Args:    #http_request
%% Returns: binary
%%----------------------------------------------------------------------
get_message(Req=#http_request{method=get}) ->
    ts_http_common:http_no_body(?GET, Req);

get_message(Req=#http_request{method=head}) ->
    ts_http_common:http_no_body(?HEAD, Req);

get_message(Req=#http_request{method=delete}) ->
    ts_http_common:http_no_body(?DELETE, Req);

get_message(Req=#http_request{method=post}) ->
    ts_http_common:http_body(?POST, Req);

get_message(Req=#http_request{method=options}) ->
    ts_http_common:http_no_body(?OPTIONS, Req);

get_message(Req=#http_request{method=put}) ->
    ts_http_common:http_body(?PUT, Req).

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: Parse the given data and return a new state
%% Args:    Data (binary)
%%            State (record)
%% Returns: {NewState, Options for socket (list), Close}
%%----------------------------------------------------------------------
parse(Data, State) ->
    ts_http_common:parse(Data, State).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
    ts_config_http:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%          this is used for ex. for Cookies in HTTP
%% Args: Subst (true|false), DynData = #dyndata, Param = #http_request,
%%                                               HostData  = {Hostname, Port}
%% Returns: #http_request or { #http_request, {Host, Port, Scheme}}
%%----------------------------------------------------------------------
add_dynparams(false, DynData, Param, HostData) ->
    add_dynparams(DynData#dyndata.proto, Param, HostData);
add_dynparams(true, DynData, OldReq=#http_request{url=OldUrl}, HostData) ->
    Req = subst(OldReq, DynData#dyndata.dynvars),
    case Req#http_request.url of
        OldUrl ->
            add_dynparams(DynData#dyndata.proto,Req, HostData);
        "http" ++ Rest -> % URL has changed and is absolute
            URL=ts_config_http:parse_URL(Req#http_request.url),
            ?DebugF("URL dynamic subst: ~p~n",[URL]),
            NewPort = ts_config_http:set_port(URL),
            NewReq  = add_dynparams(DynData#dyndata.proto,
                                    Req#http_request{host_header=undefined},
                                    {URL#url.host, NewPort, URL#url.scheme}), % add scheme
            case OldUrl of
                "http"++_ -> % old url absolute: useproxy must be true
                    NewReq#http_request{url="http"++Rest};
                _ ->
                    NewUrl=ts_config_http:set_query(URL),
                    {NewReq#http_request{url=NewUrl}, {URL#url.host, NewPort,ts_config_http:set_scheme(URL#url.scheme)}}
                end;
        _ -> % Same host:port
            add_dynparams(DynData#dyndata.proto, Req, HostData)
    end.

%% Function: add_dynparams/3
add_dynparams(DynData,Param=#http_request{host_header=undefined}, HostData )->
    Header = case HostData of
                 {Host,80, http}->
                     Host;
                 {Host,443, https}->
                     Host;
                 {Host,Port,_} ->
                     Host++":"++ integer_to_list(Port);
                 {Host,Port} ->
                     Host++":"++ integer_to_list(Port)
             end,
    ?DebugF("set host header dynamically: ~s~n",[Header]),
    add_dynparams(DynData, Param#http_request{host_header=Header},HostData);
%% no cookies
add_dynparams(#http_dyndata{cookies=[],user_agent=UA},Param, _) ->
    Param#http_request{user_agent=UA};
%% cookies
add_dynparams(#http_dyndata{cookies=DynCookie,user_agent=UA}, Req, _) ->
    %% FIXME: should we use the Port value in the Cookie ?
    Cookie=DynCookie++Req#http_request.cookie,
    Req#http_request{cookie=Cookie,user_agent=UA}.

init_dynparams() ->
    %% FIXME: optimization: suppress this call if we don't need
    %% customised users agents
    UserAgent = ts_session_cache:get_user_agent(),
    #dyndata{proto=#http_dyndata{user_agent=UserAgent}}.


%%----------------------------------------------------------------------
%% @spec subst(Req::#http_request{}, DynData::#dyndata{} ) -> #http_request{}
%% @doc Replace on the fly dynamic element of the HTTP request For
%%          the moment, we only do dynamic substitution in URL, body,
%%          userid, passwd, because we see no need for the other HTTP
%%          request parameters.
%% @end
%%----------------------------------------------------------------------
subst(Req=#http_request{url=URL, body=Body, headers = Headers, userid=UserId, passwd=Passwd}, DynData) ->
    Req#http_request{url = ts_search:subst(URL, DynData),
             body   = ts_search:subst(Body, DynData),
             headers = lists:foldl(fun ({Name, Value}, Result) ->
                                           [{Name, ts_search:subst(Value, DynData)} | Result]
                                   end, [], Headers),
             userid = ts_search:subst(UserId, DynData),
             passwd = ts_search:subst(Passwd, DynData)}.
