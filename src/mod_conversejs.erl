%%%----------------------------------------------------------------------
%%% File    : mod_conversejs.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve simple page for Converse.js XMPP web browser client
%%% Created :  8 Nov 2021 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-module(mod_conversejs).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process/2, depends/2,
         mod_opt_type/1, mod_options/1, mod_doc/0]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("translate.hrl").
-include("ejabberd_web_admin.hrl").

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

process([], #request{method = 'GET'}) ->
    Host = ejabberd_config:get_myname(),
    Domain = gen_mod:get_module_opt(Host, ?MODULE, default_domain),
    Script = gen_mod:get_module_opt(Host, ?MODULE, conversejs_script),
    CSS = gen_mod:get_module_opt(Host, ?MODULE, conversejs_css),
    Init = [{<<"discover_connection_methods">>, false},
            {<<"jid">>, Domain},
            {<<"default_domain">>, Domain},
            {<<"domain_placeholder">>, Domain},
            {<<"view_mode">>, <<"fullscreen">>}],
    Init2 =
        case gen_mod:get_module_opt(Host, ?MODULE, websocket_url) of
            undefined -> Init;
            WSURL -> [{<<"websocket_url">>, WSURL} | Init]
        end,
    Init3 =
        case gen_mod:get_module_opt(Host, ?MODULE, bosh_service_url) of
            undefined -> Init2;
            BoshURL -> [{<<"bosh_service_url">>, BoshURL} | Init2]
        end,
    {200, [html],
     [<<"<!DOCTYPE html>">>,
      <<"<html>">>,
      <<"<head>">>,
      <<"<meta charset='utf-8'>">>,
      <<"<link rel='stylesheet' type='text/css' media='screen' href='">>,
      fxml:crypt(CSS), <<"'>">>,
      <<"<script src='">>, fxml:crypt(Script), <<"' charset='utf-8'></script>">>,
      <<"</head>">>,
      <<"<body>">>,
      <<"<script>">>,
      <<"converse.initialize(">>, jiffy:encode({Init3}), <<");">>,
      <<"</script>">>,
      <<"</body>">>,
      <<"</html>">>]};
process(_, _) ->
    ejabberd_web:error(not_found).

mod_opt_type(bosh_service_url) ->
    econf:either(undefined, econf:binary());
mod_opt_type(websocket_url) ->
    econf:either(undefined, econf:binary());
mod_opt_type(conversejs_script) ->
    econf:binary();
mod_opt_type(conversejs_css) ->
    econf:binary();
mod_opt_type(default_domain) ->
    econf:binary().

mod_options(_) ->
    [{bosh_service_url, undefined},
     {websocket_url, undefined},
     {default_domain, ejabberd_config:get_myname()},
     {conversejs_script, <<"https://cdn.conversejs.org/dist/converse.min.js">>},
     {conversejs_css, <<"https://cdn.conversejs.org/dist/converse.min.css">>}].

mod_doc() ->
    #{desc =>
          [?T("This module serves a simple page for the "
              "https://conversejs.org/[Converse] XMPP web browser client."), "",
           ?T("This module is available since ejabberd 21.12."), "",
           ?T("To use this module, in addition to adding it to the 'modules' "
              "section, you must also enable it in 'listen' -> 'ejabberd_http' -> "
              "http://../listen-options/#request-handlers[request_handlers]."), "",
           ?T("You must also setup either the option 'websocket_url' or 'bosh_service_url'."), "",
           ?T("By default, the options 'conversejs_css' and 'conversejs_script'"
              " point to the public Converse.js client. Alternatively, you can"
              " host the client locally using _`mod_http_fileserver`_.")
          ],
     example =>
         ["listen:",
          "  -",
          "    port: 5280",
          "    module: ejabberd_http",
          "    request_handlers:",
          "      /websocket: ejabberd_http_ws",
          "      /conversejs: mod_conversejs",
          "",
          "modules:",
          "  mod_conversejs:",
          "    websocket_url: \"ws://example.org:5280/websocket\""],
      opts =>
          [{websocket_url,
            #{value => ?T("WebsocketURL"),
              desc =>
                  ?T("A websocket URL to which Converse.js can connect to.")}},
           {bosh_service_url,
            #{value => ?T("BoshURL"),
              desc =>
                  ?T("BOSH service URL to which Converse.js can connect to.")}},
           {default_domain,
            #{value => ?T("Domain"),
              desc =>
                  ?T("Specify a domain to act as the default for user JIDs. "
                     "The default value is the first domain defined in the "
                     "ejabberd configuration file.")}},
           {conversejs_script,
            #{value => ?T("URL"),
              desc =>
                  ?T("Converse.js main script URL.")}},
           {conversejs_css,
            #{value => ?T("URL"),
              desc =>
                  ?T("Converse.js CSS URL.")}}]
     }.
