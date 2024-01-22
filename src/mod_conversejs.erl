%%%----------------------------------------------------------------------
%%% File    : mod_conversejs.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve simple page for Converse.js XMPP web browser client
%%% Created :  8 Nov 2021 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

process([], #request{method = 'GET', host = Host, raw_path = RawPath}) ->
    ExtraOptions = get_auth_options(Host)
        ++ get_register_options(Host)
        ++ get_extra_options(Host),
    DomainRaw = gen_mod:get_module_opt(Host, ?MODULE, default_domain),
    Domain = misc:expand_keyword(<<"@HOST@">>, DomainRaw, Host),
    Script = get_file_url(Host, conversejs_script,
                          <<RawPath/binary, "/converse.min.js">>,
                          <<"https://cdn.conversejs.org/dist/converse.min.js">>),
    CSS = get_file_url(Host, conversejs_css,
                       <<RawPath/binary, "/converse.min.css">>,
                       <<"https://cdn.conversejs.org/dist/converse.min.css">>),
    Init = [{<<"discover_connection_methods">>, false},
            {<<"default_domain">>, Domain},
            {<<"domain_placeholder">>, Domain},
            {<<"registration_domain">>, Domain},
            {<<"assets_path">>, RawPath},
            {<<"view_mode">>, <<"fullscreen">>}
           | ExtraOptions],
    Init2 =
        case mod_host_meta:get_url(?MODULE, websocket, any, Host) of
            undefined -> Init;
            WSURL -> [{<<"websocket_url">>, WSURL} | Init]
        end,
    Init3 =
        case mod_host_meta:get_url(?MODULE, bosh, any, Host) of
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
process(LocalPath, #request{host = Host}) ->
    case is_served_file(LocalPath) of
        true -> serve(Host, LocalPath);
        false -> ejabberd_web:error(not_found)
    end.

%%----------------------------------------------------------------------
%% File server
%%----------------------------------------------------------------------

is_served_file([<<"converse.min.js">>]) -> true;
is_served_file([<<"converse.min.css">>]) -> true;
is_served_file([<<"converse.min.js.map">>]) -> true;
is_served_file([<<"converse.min.css.map">>]) -> true;
is_served_file([<<"emojis.js">>]) -> true;
is_served_file([<<"locales">>, _]) -> true;
is_served_file([<<"locales">>, <<"dayjs">>, _]) -> true;
is_served_file([<<"webfonts">>, _]) -> true;
is_served_file(_) -> false.

serve(Host, LocalPath) ->
    case get_conversejs_resources(Host) of
        undefined ->
            Path = str:join(LocalPath, <<"/">>),
            {303, [{<<"Location">>, <<"https://cdn.conversejs.org/dist/", Path/binary>>}], <<>>};
        MainPath -> serve2(LocalPath, MainPath)
    end.

get_conversejs_resources(Host) ->
    Opts = gen_mod:get_module_opts(Host, ?MODULE),
    mod_conversejs_opt:conversejs_resources(Opts).

%% Copied from mod_muc_log_http.erl

serve2(LocalPathBin, MainPathBin) ->
    LocalPath = [binary_to_list(LPB) || LPB <- LocalPathBin],
    MainPath = binary_to_list(MainPathBin),
    FileName = filename:join(filename:split(MainPath) ++ LocalPath),
    case file:read_file(FileName) of
        {ok, FileContents} ->
            ?DEBUG("Delivering content.", []),
            {200,
             [{<<"Content-Type">>, content_type(FileName)}],
             FileContents};
        {error, eisdir} ->
            {403, [], "Forbidden"};
        {error, Error} ->
            ?DEBUG("Delivering error: ~p", [Error]),
            case Error of
                eacces -> {403, [], "Forbidden"};
                enoent -> {404, [], "Not found"};
                _Else -> {404, [], atom_to_list(Error)}
            end
    end.

content_type(Filename) ->
    case string:to_lower(filename:extension(Filename)) of
        ".css"  -> "text/css";
        ".js"   -> "text/javascript";
        ".map"  -> "application/json";
        ".ttf"  -> "font/ttf";
        ".woff"  -> "font/woff";
        ".woff2"  -> "font/woff2"
    end.

%%----------------------------------------------------------------------
%% Options parsing
%%----------------------------------------------------------------------

get_auth_options(Domain) ->
    case {ejabberd_auth_anonymous:is_login_anonymous_enabled(Domain),
          ejabberd_auth_anonymous:is_sasl_anonymous_enabled(Domain)} of
        {false, false} ->
            [{<<"authentication">>, <<"login">>}];
        {true, false} ->
            [{<<"authentication">>, <<"external">>}];
        {_, true} ->
            [{<<"authentication">>, <<"anonymous">>},
             {<<"jid">>, Domain}]
    end.

get_register_options(Server) ->
    AuthSupportsRegister =
        lists:any(
          fun(ejabberd_auth_mnesia) -> true;
             (ejabberd_auth_external) -> true;
             (ejabberd_auth_sql) -> true;
             (_) -> false
          end,
          ejabberd_auth:auth_modules(Server)),
    Modules = get_register_modules(Server),
    ModRegisterAllowsMe = (Modules == all) orelse lists:member(?MODULE, Modules),
    [{<<"allow_registration">>, AuthSupportsRegister and ModRegisterAllowsMe}].

get_register_modules(Server) ->
    try mod_register_opt:allow_modules(Server)
    catch
        error:{module_not_loaded, mod_register, _} ->
            ?DEBUG("mod_conversejs couldn't get mod_register configuration for "
                   "vhost ~p: module not loaded in that vhost.", [Server]),
            []
    end.

get_extra_options(Host) ->
    RawOpts = gen_mod:get_module_opt(Host, ?MODULE, conversejs_options),
    lists:map(fun({Name, <<"true">>}) -> {Name, true};
                 ({Name, <<"false">>}) -> {Name, false};
                 ({<<"locked_domain">> = Name, Value}) ->
                      {Name, misc:expand_keyword(<<"@HOST@">>, Value, Host)};
                 ({Name, Value}) ->
                      {Name, Value}
              end,
              RawOpts).

get_file_url(Host, Option, Filename, Default) ->
    FileRaw = case gen_mod:get_module_opt(Host, ?MODULE, Option) of
                  auto -> get_auto_file_url(Host, Filename, Default);
                  F -> F
              end,
    misc:expand_keyword(<<"@HOST@">>, FileRaw, Host).

get_auto_file_url(Host, Filename, Default) ->
    case get_conversejs_resources(Host) of
        undefined -> Default;
        _ -> Filename
    end.

%%----------------------------------------------------------------------
%%
%%----------------------------------------------------------------------

mod_opt_type(bosh_service_url) ->
    econf:either(auto, econf:binary());
mod_opt_type(websocket_url) ->
    econf:either(auto, econf:binary());
mod_opt_type(conversejs_resources) ->
    econf:either(undefined, econf:directory());
mod_opt_type(conversejs_options) ->
    econf:map(econf:binary(), econf:either(econf:binary(), econf:int()));
mod_opt_type(conversejs_script) ->
    econf:binary();
mod_opt_type(conversejs_css) ->
    econf:binary();
mod_opt_type(default_domain) ->
    econf:binary().

mod_options(_) ->
    [{bosh_service_url, auto},
     {websocket_url, auto},
     {default_domain, <<"@HOST@">>},
     {conversejs_resources, undefined},
     {conversejs_options, []},
     {conversejs_script, auto},
     {conversejs_css, auto}].

mod_doc() ->
    #{desc =>
          [?T("This module serves a simple page for the "
              "https://conversejs.org/[Converse] XMPP web browser client."), "",
           ?T("This module is available since ejabberd 21.12."),
           ?T("Several options were improved in ejabberd 22.05."), "",
           ?T("To use this module, in addition to adding it to the 'modules' "
              "section, you must also enable it in 'listen' -> 'ejabberd_http' -> "
              "http://../listen-options/#request-handlers[request_handlers]."), "",
           ?T("Make sure either 'mod_bosh' or 'ejabberd_http_ws' "
              "http://../listen-options/#request-handlers[request_handlers] "
              "are enabled."), "",
           ?T("When 'conversejs_css' and 'conversejs_script' are 'auto', "
              "by default they point to the public Converse client.")
          ],
      example =>
          [{?T("Manually setup WebSocket url, and use the public Converse client:"),
            ["listen:",
             "  -",
             "    port: 5280",
             "    module: ejabberd_http",
             "    request_handlers:",
             "      /bosh: mod_bosh",
             "      /websocket: ejabberd_http_ws",
             "      /conversejs: mod_conversejs",
             "",
             "modules:",
             "  mod_bosh: {}",
             "  mod_conversejs:",
             "    websocket_url: \"ws://@HOST@:5280/websocket\""]},
           {?T("Host Converse locally and let auto detection of WebSocket and Converse URLs:"),
            ["listen:",
             "  -",
             "    port: 443",
             "    module: ejabberd_http",
             "    tls: true",
             "    request_handlers:",
             "      /websocket: ejabberd_http_ws",
             "      /conversejs: mod_conversejs",
             "",
             "modules:",
             "  mod_conversejs:",
             "    conversejs_resources: \"/home/ejabberd/conversejs-9.0.0/package/dist\""]},
           {?T("Configure some additional options for Converse"),
            ["modules:",
             "  mod_conversejs:",
             "    websocket_url: auto",
             "    conversejs_options:",
             "      auto_away: 30",
             "      clear_cache_on_logout: true",
             "      i18n: \"pt\"",
             "      locked_domain: \"@HOST@\"",
             "      message_archiving: always",
             "      theme: dracula"]}
          ],
      opts =>
          [{websocket_url,
            #{value => ?T("auto | WebSocketURL"),
              desc =>
                  ?T("A WebSocket URL to which Converse can connect to. "
                     "The keyword '@HOST@' is replaced with the real virtual "
                     "host name. "
                     "If set to 'auto', it will build the URL of the first "
                     "configured WebSocket request handler. "
                     "The default value is 'auto'.")}},
           {bosh_service_url,
            #{value => ?T("auto | BoshURL"),
              desc =>
                  ?T("BOSH service URL to which Converse can connect to. "
                     "The keyword '@HOST@' is replaced with the real "
                     "virtual host name. "
                     "If set to 'auto', it will build the URL of the first "
                     "configured BOSH request handler. "
                     "The default value is 'auto'.")}},
           {default_domain,
            #{value => ?T("Domain"),
              desc =>
                  ?T("Specify a domain to act as the default for user JIDs. "
                     "The keyword '@HOST@' is replaced with the hostname. "
                     "The default value is '@HOST@'.")}},
           {conversejs_resources,
            #{value => ?T("Path"),
              note => "added in 22.05",
              desc =>
                  ?T("Local path to the Converse files. "
                     "If not set, the public Converse client will be used instead.")}},
           {conversejs_options,
            #{value => "{Name: Value}",
              note => "added in 22.05",
              desc =>
                  ?T("Specify additional options to be passed to Converse. "
                     "See https://conversejs.org/docs/html/configuration.html[Converse configuration]. "
                     "Only boolean, integer and string values are supported; "
                     "lists are not supported.")}},
           {conversejs_script,
            #{value => ?T("auto | URL"),
              desc =>
                  ?T("Converse main script URL. "
                     "The keyword '@HOST@' is replaced with the hostname. "
                     "The default value is 'auto'.")}},
           {conversejs_css,
            #{value => ?T("auto | URL"),
              desc =>
                  ?T("Converse CSS URL. "
                     "The keyword '@HOST@' is replaced with the hostname. "
                     "The default value is 'auto'.")}}]
     }.
