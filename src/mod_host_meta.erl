%%%-------------------------------------------------------------------
%%% File    : mod_host_meta.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Serve host-meta files as described in XEP-0156
%%% Created :  25 March 2022 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2022   ProcessOne
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

-module(mod_host_meta).

-author('badlop@process-one.net').

-protocol({xep, 156, '1.4.0', '22.05', "", ""}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process/2,
         mod_opt_type/1, mod_options/1, depends/2]).
-export([mod_doc/0]).
-export([get_url/4]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-include("translate.hrl").

%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    report_hostmeta_listener(),
    ok.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    report_hostmeta_listener(),
    ok.

depends(_Host, _Opts) ->
    [{mod_bosh, soft}].

%%%----------------------------------------------------------------------
%%% HTTP handlers
%%%----------------------------------------------------------------------

process([], #request{method = 'GET', host = Host, path = Path}) ->
    case lists:last(Path) of
        <<"host-meta">> ->
            file_xml(Host);
        <<"host-meta.json">> ->
            file_json(Host)
    end;
process(_Path, _Request) ->
    {404, [], "Not Found"}.

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

%% When set to 'auto', it only takes the first valid listener options it finds

file_xml(Host) ->
    BoshList = case get_url(?MODULE, bosh, true, Host) of
                   undefined -> [];
                   BoshUrl ->
                       [?XA(<<"Link">>,
                            [{<<"rel">>, <<"urn:xmpp:alt-connections:xbosh">>},
                             {<<"href">>, BoshUrl}]
                           )]
               end,
    WsList = case get_url(?MODULE, websocket, true, Host) of
                 undefined -> [];
                 WsUrl ->
                     [?XA(<<"Link">>,
                          [{<<"rel">>, <<"urn:xmpp:alt-connections:websocket">>},
                           {<<"href">>, WsUrl}]
                         )]
             end,
    {200, [html,
           {<<"Content-Type">>, <<"application/xrd+xml">>},
           {<<"Access-Control-Allow-Origin">>, <<"*">>}],
     [<<"<?xml version='1.0' encoding='utf-8'?>\n">>,
      fxml:element_to_binary(
        ?XAE(<<"XRD">>,
             [{<<"xmlns">>,<<"http://docs.oasis-open.org/ns/xri/xrd-1.0">>}],
             BoshList ++ WsList)
       )]}.

file_json(Host) ->
    BoshList = case get_url(?MODULE, bosh, true, Host) of
                   undefined -> [];
                   BoshUrl -> [#{rel => <<"urn:xmpp:alt-connections:xbosh">>,
                                 href => BoshUrl}]
               end,
    WsList = case get_url(?MODULE, websocket, true, Host) of
                 undefined -> [];
                 WsUrl -> [#{rel => <<"urn:xmpp:alt-connections:websocket">>,
                             href => WsUrl}]
             end,
    {200, [html,
           {<<"Content-Type">>, <<"application/json">>},
           {<<"Access-Control-Allow-Origin">>, <<"*">>}],
     [jiffy:encode(#{links => BoshList ++ WsList})]}.

get_url(M, bosh, Tls, Host) ->
    get_url(M, Tls, Host, bosh_service_url, mod_bosh);
get_url(M, websocket, Tls, Host) ->
    get_url(M, Tls, Host, websocket_url, ejabberd_http_ws).

get_url(M, Tls, Host, Option, Module) ->
    case get_url_preliminar(M, Tls, Host, Option, Module) of
        undefined -> undefined;
        Url -> misc:expand_keyword(<<"@HOST@">>, Url, Host)
    end.

get_url_preliminar(M, Tls, Host, Option, Module) ->
    case gen_mod:get_module_opt(Host, M, Option) of
        undefined -> undefined;
        auto -> get_auto_url(Tls, Module);
        <<"auto">> -> get_auto_url(Tls, Module);
        U when is_binary(U) -> U
    end.

get_auto_url(Tls, Module) ->
    case find_handler_port_path(Tls, Module) of
        [] -> undefined;
        [{ThisTls, Port, Path} | _] ->
            Protocol = case {ThisTls, Module} of
                           {false, mod_bosh} -> <<"http">>;
                           {true, mod_bosh} -> <<"https">>;
                           {false, ejabberd_http_ws} -> <<"ws">>;
                           {true, ejabberd_http_ws} -> <<"wss">>
                       end,
            <<Protocol/binary,
              "://@HOST@:",
              (integer_to_binary(Port))/binary,
              "/",
              (str:join(Path, <<"/">>))/binary>>
    end.

find_handler_port_path(Tls, Module) ->
    lists:filtermap(
      fun({{Port, _, _},
           ejabberd_http,
           #{tls := ThisTls, request_handlers := Handlers}})
            when (Tls == any) or (Tls == ThisTls) ->
              case lists:keyfind(Module, 2, Handlers) of
                  false -> false;
                  {Path, Module} -> {true, {ThisTls, Port, Path}}
              end;
         (_) -> false
      end, ets:tab2list(ejabberd_listener)).

report_hostmeta_listener() ->
    case {find_handler_port_path(false, ?MODULE),
          find_handler_port_path(true, ?MODULE)} of
        {[], []} ->
            ?CRITICAL_MSG("It seems you enabled ~p in 'modules' but forgot to "
                          "add it as a request_handler in an ejabberd_http "
                          "listener.", [?MODULE]);
        {[_|_], _} ->
            ?WARNING_MSG("Apparently ~p is enabled in a request_handler in a "
                         "non-encrypted ejabberd_http listener. This is "
                         "disallowed by XEP-0156. Please enable 'tls' in that "
                         "listener, or setup a proxy encryption mechanism.",
                         [?MODULE]);
        {[], [_|_]} ->
            ok
    end.

%%%----------------------------------------------------------------------
%%% Options and Doc
%%%----------------------------------------------------------------------

mod_opt_type(bosh_service_url) ->
    econf:either(undefined, econf:binary());
mod_opt_type(websocket_url) ->
    econf:either(undefined, econf:binary()).

mod_options(_) ->
    [{bosh_service_url, <<"auto">>},
     {websocket_url, <<"auto">>}].

mod_doc() ->
    #{desc =>
          [?T("This module serves small 'host-meta' files as described in "
              "https://xmpp.org/extensions/xep-0156.html[XEP-0156: Discovering "
              "Alternative XMPP Connection Methods]."), "",
           ?T("This module is available since ejabberd 22.05."), "",
           ?T("To use this module, in addition to adding it to the 'modules' "
              "section, you must also enable it in 'listen' -> 'ejabberd_http' -> "
              "http://../listen-options/#request-handlers[request_handlers]."), "",
           ?T("Notice it only works if ejabberd_http has tls enabled.")],
      example =>
          ["listen:",
           "  -",
           "    port: 443",
           "    module: ejabberd_http",
           "    tls: true",
           "    request_handlers:",
           "      /bosh: mod_bosh",
           "      /ws: ejabberd_http_ws",
           "      /.well-known/host-meta: mod_host_meta",
           "      /.well-known/host-meta.json: mod_host_meta",
           "",
           "modules:",
           "  mod_bosh: {}",
           "  mod_host_meta:",
           "    bosh_service_url: \"https://@HOST@:5443/bosh\"",
           "    websocket_url: \"wss://@HOST@:5443/ws\""],

      opts =>
          [{websocket_url,
            #{value => "undefined | auto | WebSocketURL",
              desc =>
                  ?T("WebSocket URL to announce. "
                     "The keyword '@HOST@' is replaced with the real virtual "
                     "host name. "
                     "If set to 'auto', it will build the URL of the first "
                     "configured WebSocket request handler. "
                     "The default value is 'auto'.")}},
           {bosh_service_url,
            #{value => "undefined | auto | BoshURL",
              desc =>
                  ?T("BOSH service URL to announce. "
                     "The keyword '@HOST@' is replaced with the real "
                     "virtual host name. "
                     "If set to 'auto', it will build the URL of the first "
                     "configured BOSH request handler. "
                     "The default value is 'auto'.")}}]
     }.
