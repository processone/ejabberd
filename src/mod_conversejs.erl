%%%----------------------------------------------------------------------
%%% File    : mod_conversejs.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve simple page for Converse.js XMPP web browser client
%%% Created :  8 Nov 2021 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-export([web_menu_system/2]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("translate.hrl").
-include("ejabberd_web_admin.hrl").

start(_Host, _Opts) ->
    {ok, [{hook, webadmin_menu_system_post, web_menu_system, 50, global}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

process([], #request{method = 'GET', host = Host, q = Query, raw_path = RawPath1}) ->
    [RawPath | _] = string:split(RawPath1, "?"),
    ExtraOptions = get_auth_options(Host)
        ++ get_autologin_options(Query)
        ++ get_register_options(Host)
        ++ get_extra_options(Host),
    Domain = mod_conversejs_opt:default_domain(Host),
    Script = get_file_url(Host, conversejs_script,
                          <<RawPath/binary, "/converse.min.js">>,
                          <<"https://cdn.conversejs.org/dist/converse.min.js">>),
    CSS = get_file_url(Host, conversejs_css,
                       <<RawPath/binary, "/converse.min.css">>,
                       <<"https://cdn.conversejs.org/dist/converse.min.css">>),
    PluginsHtml = get_plugins_html(Host, RawPath),
    Init = [{<<"discover_connection_methods">>, false},
            {<<"default_domain">>, Domain},
            {<<"domain_placeholder">>, Domain},
            {<<"registration_domain">>, Domain},
            {<<"assets_path">>, <<RawPath/binary, "/">>},
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
    Init4 = maps:from_list(Init3),
    {200, [html],
     [<<"<!DOCTYPE html>">>,
      <<"<html>">>,
      <<"<head>">>,
      <<"<meta charset='utf-8'>">>,
      <<"<link rel='stylesheet' type='text/css' media='screen' href='">>,
      fxml:crypt(CSS), <<"'>">>,
      <<"<script src='">>, fxml:crypt(Script), <<"' charset='utf-8'></script>">>
     ] ++ PluginsHtml ++ [
      <<"</head>">>,
      <<"<body>">>,
      <<"<script>">>,
      <<"converse.initialize(">>, misc:json_encode(Init4), <<");">>,
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
is_served_file([<<"plugins">>, _]) -> true;
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

get_plugins_html(Host, RawPath) ->
    Resources = get_conversejs_resources(Host),
    lists:map(fun(F) ->
                 Plugin =
                     case {F, Resources} of
                         {<<"libsignal">>, undefined} ->
                             <<"https://cdn.conversejs.org/3rdparty/libsignal-protocol.min.js">>;
                         {<<"libsignal">>, Path} ->
                             ?WARNING_MSG("~p is configured to use local Converse files "
                                          "from path ~ts but the public plugin ~ts!",
                                          [?MODULE, Path, F]),
                             <<"https://cdn.conversejs.org/3rdparty/libsignal-protocol.min.js">>;
                         _ ->
                             fxml:crypt(<<RawPath/binary, "plugins/", F/binary>>)
                     end,
                 <<"<script src='", Plugin/binary, "' charset='utf-8'></script>">>
              end,
              gen_mod:get_module_opt(Host, ?MODULE, conversejs_plugins)).

%%----------------------------------------------------------------------
%% WebAdmin link and autologin
%%----------------------------------------------------------------------

%% @format-begin

web_menu_system(Result,
                #request{host = Host,
                         auth = Auth,
                         tp = Protocol}) ->
    AutoUrl = mod_host_meta:get_auto_url(any, ?MODULE),
    ConverseUrl = misc:expand_keyword(<<"@HOST@">>, AutoUrl, Host),
    AutologinQuery =
        case {Protocol, Auth} of
            {http, {Jid, _Password}} ->
                <<"/?autologinjid=", Jid/binary>>;
            {https, {Jid, Password}} ->
                AuthToken = build_token(Jid, Password),
                <<"/?autologinjid=", Jid/binary, "&autologintoken=", AuthToken/binary>>;
            _ ->
                <<"">>
        end,
    ConverseEl =
        ?LI([?C(unicode:characters_to_binary("☯️")),
             ?XAE(<<"a">>,
                  [{<<"href">>, <<ConverseUrl/binary, AutologinQuery/binary>>},
                   {<<"target">>, <<"_blank">>}],
                  [?C(unicode:characters_to_binary("Converse"))])]),
    [ConverseEl | Result].

get_autologin_options(Query) ->
    case {proplists:get_value(<<"autologinjid">>, Query),
          proplists:get_value(<<"autologintoken">>, Query)}
    of
        {undefined, _} ->
            [];
        {Jid, Token} ->
            [{<<"auto_login">>, <<"true">>},
             {<<"jid">>, <<"admin@localhost">>},
             {<<"password">>, check_token_get_password(Jid, Token)}]
    end.

build_token(Jid, Password) ->
    Minutes =
        integer_to_binary(calendar:datetime_to_gregorian_seconds(
                              calendar:universal_time())
                          div 60),
    Cookie =
        misc:atom_to_binary(
            erlang:get_cookie()),
    str:sha(<<Jid/binary, Password/binary, Minutes/binary, Cookie/binary>>).

check_token_get_password(_, undefined) ->
    <<"">>;
check_token_get_password(JidString, TokenProvided) ->
    Jid = jid:decode(JidString),
    Password = ejabberd_auth:get_password_s(Jid#jid.luser, Jid#jid.lserver),
    case build_token(JidString, Password) of
        TokenProvided ->
            Password;
        _ ->
            <<"">>
    end.
%% @format-end

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
mod_opt_type(conversejs_plugins) ->
    econf:list(econf:binary());
mod_opt_type(default_domain) ->
    econf:host().

mod_options(Host) ->
    [{bosh_service_url, auto},
     {websocket_url, auto},
     {default_domain, Host},
     {conversejs_resources, undefined},
     {conversejs_options, []},
     {conversejs_script, auto},
     {conversejs_plugins, []},
     {conversejs_css, auto}].

mod_doc() ->
    #{desc =>
          [?T("This module serves a simple page for the "
              "https://conversejs.org/[Converse] XMPP web browser client."), "",
           ?T("To use this module, in addition to adding it to the 'modules' "
              "section, you must also enable it in 'listen' -> 'ejabberd_http' -> "
              "_`listen-options.md#request_handlers|request_handlers`_."), "",
           ?T("Make sure either _`mod_bosh`_ or _`listen.md#ejabberd_http_ws|ejabberd_http_ws`_ "
              "are enabled in at least one 'request_handlers'."), "",
           ?T("When 'conversejs_css' and 'conversejs_script' are 'auto', "
              "by default they point to the public Converse client."), "",
           ?T("This module is available since ejabberd 21.12.")
          ],
      note => "improved in 25.07",
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
             "    conversejs_plugins: [\"libsignal\"]",
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
             "    conversejs_resources: \"/home/ejabberd/conversejs-x.y.z/package/dist\"",
             "    conversejs_plugins: [\"libsignal-protocol.min.js\"]",
             "    # File path is: /home/ejabberd/conversejs-x.y.z/package/dist/plugins/libsignal-protocol.min.js"]},
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
                     "The '@HOST@' keyword is replaced with the real virtual "
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
           {conversejs_plugins,
            #{value => ?T("[Filename]"),
              desc =>
                  ?T("List of additional local files to include as scripts in the homepage. "
                     "Please make sure those files are available in the path specified in "
                     "'conversejs_resources' option, in subdirectory 'plugins/'. "
                     "If using the public Converse client, then '\"libsignal\"' "
                     "gets replaced with the URL of the public library. "
                     "The default value is '[]'.")}},
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
