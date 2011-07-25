%%%
%%%  Copyright © IDEALX S.A.S. 2004
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 20 Apr 2004 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%% common functions used by http clients to parse config

-module(ts_config_http).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([parse_config/2, parse_URL/1, set_port/1, set_scheme/1,
         check_user_agent_sum/1, set_query/1]).

-include("ts_profile.hrl").
-include("ts_http.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=http},
             Config=#config{curid = Id, session_tab = Tab, servers = Servers,
                            sessions = [CurS | _], dynvar=DynVar,
                            subst    = SubstFlag, match=MatchRegExp}) ->
    Version  = ts_config:getAttr(string,Element#xmlElement.attributes, version, "1.1"),
    URL      = ts_config:getAttr(Element#xmlElement.attributes, url),
    Contents = case  ts_config:getAttr(string, Element#xmlElement.attributes, contents_from_file) of
                   [] ->
                       C=ts_config:getAttr(Element#xmlElement.attributes, contents),
                       list_to_binary(C);
                   FileName ->
                       {ok, FileContent} = file:read_file(FileName),
                       FileContent
               end,
    UseProxy = case ets:lookup(Tab,{http_use_server_as_proxy}) of
                   [] -> false;
                   _  -> true
               end,
    %% Apache Tomcat applications need content-type informations to read post forms
    ContentType = ts_config:getAttr(string,Element#xmlElement.attributes,
                             content_type, "application/x-www-form-urlencoded"),
    Date   = ts_config:getAttr(string, Element#xmlElement.attributes,
                               'if_modified_since', undefined),
    Method = list_to_atom(ts_utils:to_lower(ts_config:getAttr(string, Element#xmlElement.attributes, method, "get"))),
    Request = #http_request{url         = URL,
                            method      = Method,
                            version     = Version,
                            get_ims_date= Date,
                            content_type= ContentType,
                            body        = Contents},
    %% SOAP Support: Add SOAPAction header to the message
    Request2 = case lists:keysearch(soap,#xmlElement.name,
                                    Element#xmlElement.content) of
                   {value, SoapEl=#xmlElement{} } ->
                       SOAPAction  = ts_config:getAttr(SoapEl#xmlElement.attributes,
                                                       action),
                       Request#http_request{soap_action=SOAPAction};
                   _ ->
                       Request
               end,
    PreviousHTTPServer = get_previous_http_server(Tab, CurS#session.id),
    %% client side cookies
    Cookies = parse_cookie(Element#xmlElement.content, []),
    %% Custom HTTP headers
    Headers=  parse_headers(Element#xmlElement.content,
                            Request2#http_request.headers),
    Request3 = Request2#http_request{headers=Headers,cookie=Cookies},
    %% HTTP Authentication
    Msg = case lists:keysearch(www_authenticate, #xmlElement.name,
                               Element#xmlElement.content) of
              {value, AuthEl=#xmlElement{} } ->
                  UserId= ts_config:getAttr(string,AuthEl#xmlElement.attributes,
                                              userid, undefined),
                  Passwd= ts_config:getAttr(string,AuthEl#xmlElement.attributes,
                                              passwd, undefined),
                  NewReq=Request3#http_request{userid=UserId, passwd=Passwd},
                  set_msg(NewReq, {SubstFlag, MatchRegExp, UseProxy, Servers, PreviousHTTPServer, Tab, CurS#session.id} );
              _ ->
                  set_msg(Request3, {SubstFlag, MatchRegExp, UseProxy, Servers, PreviousHTTPServer, Tab, CurS#session.id} )
          end,
    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id},Msg#ts_request{endpage=true,
                                                         dynvar_specs=DynVar}}),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content);
%% Parsing default values
parse_config(Element = #xmlElement{name=option}, Conf = #config{session_tab = Tab}) ->
    case ts_config:getAttr(Element#xmlElement.attributes, name) of
        "user_agent" ->
            lists:foldl( fun(A,B)->parse_config(A,B) end, Conf, Element#xmlElement.content);
        "http_use_server_as_proxy" ->
            Val = ts_config:getAttr(Element#xmlElement.attributes, value),
            ets:insert(Tab,{{http_use_server_as_proxy}, Val})
    end,
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end, Conf, Element#xmlElement.content);
%% Parsing user_agent
parse_config(Element = #xmlElement{name=user_agent}, Conf = #config{session_tab = Tab}) ->
    Proba = ts_config:getAttr(integer,Element#xmlElement.attributes, probability),
    ValRaw = ts_config:getText(Element#xmlElement.content),
    Val = ts_utils:clean_str(ValRaw),
    ?LOGF("Get user agent: ~p ~p ~n",[Proba, Val],?WARN),
    Previous = case ets:lookup(Tab, {http_user_agent, value}) of
                   [] ->
                       [];
                   [{_Key,Old}] ->
                       Old
               end,
    ets:insert(Tab,{{http_user_agent, value}, [{Proba, Val}|Previous]}),
    lists:foldl( fun(A,B)->parse_config(A,B) end, Conf, Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

get_previous_http_server(Ets, Id) ->
    case ets:lookup(Ets, {http_server, Id}) of
        [] -> [];
        [{_Key,PrevServ}] -> PrevServ
    end.

parse_headers([], Headers) ->
    Headers;
parse_headers([Element = #xmlElement{name=http_header} | Tail], Headers) ->
    Name   = ts_config:getAttr(string, Element#xmlElement.attributes, name),
    Value  = ts_config:getAttr(string, Element#xmlElement.attributes, value),
    EncodedValue = case ts_config:getAttr(atom, Element#xmlElement.attributes, encoding, none) of
                       base64 ->
                           ts_utils:encode_base64(Value);
                       none ->
                           Value
                   end,
    parse_headers(Tail, [{Name, EncodedValue} | Headers]);
parse_headers([_| Tail], Headers) ->
    parse_headers(Tail, Headers).

parse_cookie([], Cookies) ->
    Cookies;
parse_cookie([Element = #xmlElement{name=add_cookie} | Tail], Cookies) ->
    Key   = ts_config:getAttr(string, Element#xmlElement.attributes, key),
    Value = ts_config:getAttr(string, Element#xmlElement.attributes, value),
    Path  = ts_config:getAttr(string, Element#xmlElement.attributes, path,"/"),
    Domain= ts_config:getAttr(string,Element#xmlElement.attributes,domain,"."),
    parse_cookie(Tail,[#cookie{key=Key,value=Value,path=Path,domain=Domain}|Cookies]);
parse_cookie([_| Tail], Cookies) ->
    parse_cookie(Tail, Cookies).

%%----------------------------------------------------------------------
%% Func: set_msg/2
%% Returns: #ts_request record
%% Purpose: build the #ts_request record from an #http_request,
%%          and Substition def.
%%----------------------------------------------------------------------
%% if the URL is full (http://...), we parse it and get server host,
%% port and scheme from the URL and override the global setup of the
%% server. These informations are stored in the #ts_request record.
set_msg(HTTP=#http_request{url="http" ++ URL},
        {SubstFlag, MatchRegExp, UseProxy, [Server|_], _PrevHTTPServer, Tab, Id}) ->
    URLrec     = parse_URL("http" ++ URL),
    Path       = set_query(URLrec),
    HostHeader = set_host_header(URLrec),
    Port       = set_port(URLrec),
    Scheme     = set_scheme(URLrec#url.scheme),
    ets:insert(Tab,{{http_server, Id}, {HostHeader, URLrec#url.scheme}}),

    {RealServer, RealPath} = case UseProxy of
                     true  -> {Server, "http"++ URL};
                     false -> {#server{host=URLrec#url.host, port=Port, type=Scheme},
                               Path}
                 end,

    set_msg2(HTTP#http_request{url=RealPath, host_header = HostHeader},
             #ts_request{ack    = parse,
                         subst  = SubstFlag,
                         match  = MatchRegExp,
                         host   = RealServer#server.host,
                         scheme = RealServer#server.type,
                         port   = RealServer#server.port});

%% relative URL, no previous HTTP server, use proxy, error !
set_msg(_, {_, _, true, _Server, [],_Tab,_Id}) ->
    ?LOG("Need absolute URL when using a proxy ! Abort",?ERR),
    throw({error, badurl_proxy});
%% url head is a dynvar, don't preset host header
set_msg(HTTP=#http_request{url="%%" ++ TailURL},  {true, MatchRegExp, _Proxy, _Servers, _Headers,_Tab,_Id}) ->
    set_msg2(HTTP,
             #ts_request{ack = parse, subst = true, match = MatchRegExp });
%% relative URL, no proxy, a single server => we can preset host header at configuration time
set_msg(HTTPRequest, {SubstFlag, MatchRegExp, false, [Server], [],_Tab,_Id}) ->
    ?LOG("Relative URL, single server ",?NOTICE),
    URL = server_to_url(Server) ++ HTTPRequest#http_request.url,
    set_msg(HTTPRequest#http_request{url=URL}, {SubstFlag, MatchRegExp, false, [Server], [],_Tab,_Id});
%% relative URL, no proxy, several servers: don't set host header
%% since the real server will be choose at run time
set_msg(HTTPRequest, {SubstFlag, MatchRegExp, false, _Servers, [],_Tab,_Id}) ->
    set_msg2(HTTPRequest,
             #ts_request{ack = parse, subst = SubstFlag, match = MatchRegExp });
%% relative URL, no proxy
set_msg(HTTPRequest, {SubstFlag, MatchRegExp, false, _Server, {HostHeader,_},_Tab,_Id}) ->
    set_msg2(HTTPRequest#http_request{host_header= HostHeader},
             #ts_request{ack = parse, subst = SubstFlag, match = MatchRegExp });
%% relative URL, use proxy
set_msg(HTTPRequest, {SubstFlag, MatchRegExp, true, Server, {HostHeader, PrevScheme},Tab,Id}) ->
    %% set absolute URL using previous Server used.
    URL = atom_to_list(PrevScheme) ++ "://" ++ HostHeader ++ HTTPRequest#http_request.url,
    set_msg(HTTPRequest#http_request{url=URL}, {SubstFlag, MatchRegExp, true, Server, {HostHeader, PrevScheme},Tab,Id}).

%% Func: set_mgs2/3
%% Purpose: set param  in ts_request
set_msg2(HTTPRequest, Msg) -> Msg#ts_request{ param = HTTPRequest }.

server_to_url(#server{port=80, host= Host, type= gen_tcp})->
    "http://" ++ Host;
server_to_url(#server{port=Port, host= Host, type= gen_tcp})->
    "http://" ++ Host ++ ":" ++ integer_to_list(Port);
server_to_url(#server{port=443, host= Host, type= ssl})->
    "https://" ++ Host;
server_to_url(#server{port=Port, host= Host, type= ssl})->
    "https://" ++ Host ++ ":" ++ integer_to_list(Port).

%%--------------------------------------------------------------------
%% Func: set_host_header/1
%%--------------------------------------------------------------------
%% if port is undefined, don't need to set port, because it use the default (80 or 443)
set_host_header(#url{host=Host,port=undefined})   -> Host;
set_host_header(#url{host=Host,port=Port}) when is_integer(Port) ->
    Host ++ ":" ++ integer_to_list(Port).

%%--------------------------------------------------------------------
%% Func: set_port/1
%% Purpose: Returns port according to scheme if not already defined
%% Returns: PortNumber (integer)
%%--------------------------------------------------------------------
set_port(#url{scheme=https,port=undefined})  -> 443;
set_port(#url{scheme=http,port=undefined})   -> 80;
set_port(#url{port=Port}) when is_integer(Port) -> Port;
set_port(#url{port=Port}) -> integer_to_list(Port).

set_scheme(http)  -> gen_tcp;
set_scheme(https) -> ssl.


set_query(URLrec = #url{querypart=""}) ->
    URLrec#url.path;
set_query(URLrec = #url{}) ->
    URLrec#url.path ++ "?" ++ URLrec#url.querypart.


%%----------------------------------------------------------------------
%% Func: parse_URL/1
%% Returns: #url
%%----------------------------------------------------------------------
parse_URL("https://" ++ String) ->
    parse_URL(host, String, [], #url{scheme=https});
parse_URL("http://" ++ String) ->
    parse_URL(host, String, [], #url{scheme=http});
parse_URL(String) when is_list(String) ->
    [Host, Port] = string:tokens(String,":"),
    #url{scheme=connect, host=Host, port=list_to_integer(Port)}.

%%----------------------------------------------------------------------
%% Func: parse_URL/4 (inspired by yaws_api.erl)
%% Returns: #url record
%%----------------------------------------------------------------------
% parse host
parse_URL(host, [], Acc, URL) -> % no path or port
    URL#url{host=lists:reverse(Acc), path= "/"};
parse_URL(host, [$/|Tail], Acc, URL) -> % path starts here
    parse_URL(path, Tail, "/", URL#url{host=lists:reverse(Acc)});
parse_URL(host, [$:|Tail], Acc, URL) -> % port starts here
    parse_URL(port, Tail, [], URL#url{host=lists:reverse(Acc)});
parse_URL(host, [H|Tail], Acc, URL) ->
    parse_URL(host, Tail, [H|Acc], URL);

% parse port
parse_URL(port,[], Acc, URL) ->
    URL#url{port=list_to_integer(lists:reverse(Acc)), path= "/"};
parse_URL(port,[$/|T], Acc, URL) ->
    parse_URL(path, T, "/", URL#url{port=list_to_integer(lists:reverse(Acc))});
parse_URL(port,[H|T], Acc, URL) ->
    parse_URL(port, T, [H|Acc], URL);

% parse path
parse_URL(path,[], Acc, URL) ->
    URL#url{path=lists:reverse(Acc)};
parse_URL(path,[$?|T], Acc, URL) ->
    URL#url{path=lists:reverse(Acc), querypart=T};
parse_URL(path,[H|T], Acc, URL) ->
    parse_URL(path, T, [H|Acc], URL).

% check if the sum of all user agent probabilities is equal to 100%
check_user_agent_sum(Tab) ->
    case ets:lookup(Tab, {http_user_agent, value}) of
        [] ->
            ok; % no user agent, will use the default one.
        [{_Key, UserAgents}] ->
            ts_utils:check_sum(UserAgents, 1, ?USER_AGENT_ERROR_MSG)
    end.
