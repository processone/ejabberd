%%%-------------------------------------------------------------------
%%% File    : mod_providers.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Serve xmpp-provider-v2.json files as described by XMPP Providers
%%% Created :  7 August 2025 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2026   ProcessOne
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

%%--------------------------------------------------------------------
%%| Definitions

%% This module is based in mod_host_meta.erl

%% @format-begin

-module(mod_providers).

-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process/2, mod_opt_type/1, mod_options/1, depends/2]).
-export([mod_doc/0]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_http.hrl").
-include("ejabberd_web_admin.hrl").
-include("translate.hrl").

%%--------------------------------------------------------------------
%%| gen_mod callbacks

start(_Host, _Opts) ->
    report_hostmeta_listener(),
    ok.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    report_hostmeta_listener(),
    ok.

depends(_Host, _Opts) ->
    [].

%%--------------------------------------------------------------------
%%| HTTP handlers

process([],
        #request{method = 'GET',
                 host = Host,
                 path = Path}) ->
    case lists:last(Path) of
        <<"xmpp-provider-v2.json">> ->
            file_json(Host)
    end;
process(_Path, _Request) ->
    {404, [], "Not Found"}.

%%--------------------------------------------------------------------
%%| JSON

file_json(Host) ->
    Content =
        #{website => build_urls(Host, website),
          alternativeJids => gen_mod:get_module_opt(Host, ?MODULE, alternativeJids),
          busFactor => gen_mod:get_module_opt(Host, ?MODULE, busFactor),
          organization => gen_mod:get_module_opt(Host, ?MODULE, organization),
          passwordReset => get_password_url(Host),
          serverTesting => gen_mod:get_module_opt(Host, ?MODULE, serverTesting),
          maximumHttpFileUploadTotalSize => get_upload_size(Host),
          maximumHttpFileUploadStorageTime => get_upload_time(Host),
          maximumMessageArchiveManagementStorageTime =>
              gen_mod:get_module_opt(Host, ?MODULE, maximumMessageArchiveManagementStorageTime),
          professionalHosting => gen_mod:get_module_opt(Host, ?MODULE, professionalHosting),
          freeOfCharge => gen_mod:get_module_opt(Host, ?MODULE, freeOfCharge),
          legalNotice => build_urls(Host, legalNotice),
          serverLocations => gen_mod:get_module_opt(Host, ?MODULE, serverLocations),
          since => gen_mod:get_module_opt(Host, ?MODULE, since)},
    {200,
     [html,
      {<<"Content-Type">>, <<"application/json">>},
      {<<"Access-Control-Allow-Origin">>, <<"*">>}],
     [misc:json_encode(Content)]}.

%%--------------------------------------------------------------------
%%| Upload Size

get_upload_size(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, maximumHttpFileUploadTotalSize) of
        default_value ->
            get_upload_size_mhuq(Host);
        I when is_integer(I) ->
            I
    end.

get_upload_size_mhuq(Host) ->
    case gen_mod:is_loaded(Host, mod_http_upload_quota) of
        true ->
            Access = gen_mod:get_module_opt(Host, mod_http_upload_quota, access_hard_quota),
            Rules = ejabberd_shaper:read_shaper_rules(Access, Host),
            get_upload_size_rules(Rules);
        false ->
            0
    end.

get_upload_size_rules(Rules) ->
    case lists:keysearch([{acl, all}], 2, Rules) of
        {value, {Size, _}} ->
            Size;
        false ->
            0
    end.

%%--------------------------------------------------------------------
%%| Upload Time

get_upload_time(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, maximumHttpFileUploadStorageTime) of
        default_value ->
            get_upload_time_mhuq(Host);
        I when is_integer(I) ->
            I
    end.

get_upload_time_mhuq(Host) ->
    case gen_mod:is_loaded(Host, mod_http_upload_quota) of
        true ->
            case gen_mod:get_module_opt(Host, mod_http_upload_quota, max_days) of
                infinity ->
                    0;
                I when is_integer(I) ->
                    I
            end;
        false ->
            0
    end.

%%--------------------------------------------------------------------
%%| Password URL

get_password_url(Host) ->
    build_urls(Host, get_password_url2(Host)).

get_password_url2(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, passwordReset) of
        default_value ->
            get_password_url3(Host);
        U when is_binary(U) ->
            U
    end.

get_password_url3(Host) ->
    case find_handler_port_path2(any, mod_register_web) of
        [] ->
            <<"">>;
        [{ThisTls, Port, Path} | _] ->
            Protocol =
                case ThisTls of
                    false ->
                        <<"http">>;
                    true ->
                        <<"https">>
                end,
            <<Protocol/binary,
              "://",
              Host/binary,
              ":",
              (integer_to_binary(Port))/binary,
              "/",
              (str:join(Path, <<"/">>))/binary,
              "/">>
    end.

%% TODO Ya hay otra funciona como esta
find_handler_port_path2(Tls, Module) ->
    lists:filtermap(fun ({{Port, _, _},
                          ejabberd_http,
                          #{tls := ThisTls, request_handlers := Handlers}})
                            when (Tls == any) or (Tls == ThisTls) ->
                            case lists:keyfind(Module, 2, Handlers) of
                                false ->
                                    false;
                                {Path, Module} ->
                                    {true, {ThisTls, Port, Path}}
                            end;
                        (_) ->
                            false
                    end,
                    ets:tab2list(ejabberd_listener)).

%%--------------------------------------------------------------------
%%| Build URLs

build_urls(Host, Option) when is_atom(Option) ->
    build_urls(Host, gen_mod:get_module_opt(Host, ?MODULE, Option));
build_urls(_Host, <<"">>) ->
    #{};
build_urls(Host, Url) when not is_atom(Url) ->
    Languages = gen_mod:get_module_opt(Host, ?MODULE, languages),
    maps:from_list([{L, misc:expand_keyword(<<"@LANGUAGE_URL@">>, Url, L)}
                    || L <- Languages]).

find_handler_port_path(Tls, Module) ->
    lists:filtermap(fun ({{Port, _, _},
                          ejabberd_http,
                          #{tls := ThisTls, request_handlers := Handlers}})
                            when is_integer(Port) and ((Tls == any) or (Tls == ThisTls)) ->
                            case lists:keyfind(Module, 2, Handlers) of
                                false ->
                                    false;
                                {Path, Module} ->
                                    {true, {ThisTls, Port, Path}}
                            end;
                        (_) ->
                            false
                    end,
                    ets:tab2list(ejabberd_listener)).

report_hostmeta_listener() ->
    case {find_handler_port_path(false, ?MODULE), find_handler_port_path(true, ?MODULE)} of
        {[], []} ->
            ?CRITICAL_MSG("It seems you enabled ~p in 'modules' but forgot to "
                          "add it as a request_handler in an ejabberd_http "
                          "listener.",
                          [?MODULE]);
        {[_ | _], _} ->
            ?WARNING_MSG("Apparently ~p is enabled in a request_handler in a "
                         "non-encrypted ejabberd_http listener. If this is "
                         "not desired, enable 'tls' in that "
                         "listener, or setup a proxy encryption mechanism.",
                         [?MODULE]);
        {[], [_ | _]} ->
            ok
    end.

%%--------------------------------------------------------------------
%%| Options

mod_opt_type(languages) ->
    econf:list(
        econf:binary());
mod_opt_type(website) ->
    econf:binary();
mod_opt_type(alternativeJids) ->
    econf:list(
        econf:domain(), [unique]);
mod_opt_type(busFactor) ->
    econf:int();
mod_opt_type(organization) ->
    econf:enum([company,
                'commercial person',
                'private person',
                governmental,
                'non-governmental']);
mod_opt_type(passwordReset) ->
    econf:binary();
mod_opt_type(serverTesting) ->
    econf:bool();
mod_opt_type(maximumHttpFileUploadTotalSize) ->
    econf:int();
mod_opt_type(maximumHttpFileUploadStorageTime) ->
    econf:int();
mod_opt_type(maximumMessageArchiveManagementStorageTime) ->
    econf:int();
mod_opt_type(professionalHosting) ->
    econf:bool();
mod_opt_type(freeOfCharge) ->
    econf:bool();
mod_opt_type(legalNotice) ->
    econf:binary();
mod_opt_type(serverLocations) ->
    econf:list(
        econf:binary());
mod_opt_type(since) ->
    econf:binary().

mod_options(Host) ->
    [{languages, [ejabberd_option:language(Host)]},
     {website, <<"">>},
     {alternativeJids, []},
     {busFactor, -1},
     {organization, ''},
     {passwordReset, default_value},
     {serverTesting, false},
     {maximumHttpFileUploadTotalSize, default_value},
     {maximumHttpFileUploadStorageTime, default_value},
     {maximumMessageArchiveManagementStorageTime, 0},
     {professionalHosting, false},
     {freeOfCharge, false},
     {legalNotice, <<"">>},
     {serverLocations, []},
     {since, <<"">>}].

%%--------------------------------------------------------------------
%%| Doc

mod_doc() ->
    #{desc =>
          [?T("This module serves JSON provider files API v2 as described by "
              "https://providers.xmpp.net/provider-file-generator/[XMPP Providers]."),
           "",
           ?T("It attempts to fill some properties gathering values automatically from your existing ejabberd configuration. Try enabling the module, check what values are displayed, and then customize using the options."),
           "",
           ?T("To use this module, in addition to adding it to the 'modules' "
              "section, you must also enable it in 'listen' -> 'ejabberd_http' -> "
              "_`listen-options.md#request_handlers|request_handlers`_. "
              "Notice you should set in _`listen.md#ejabberd_http|ejabberd_http`_ "
              "the option _`listen-options.md#tls|tls`_ enabled.")],
      note => "added in 25.08",
      opts =>
          [{languages,
            #{value => "[string()]",
              desc =>
                  ?T("List of language codes that your pages are available. "
                     "Some options define URL where the keyword '@LANGUAGE_URL@' "
                     "will be replaced with each of those language codes. "
                     "The default value is a list with the language set in the "
                     "option _`language`_, for example: '[en]'.")}},
           {website,
            #{value => "string()",
              desc =>
                  ?T("Provider website. "
                     "The keyword '@LANGUAGE_URL@' is replaced with each language. "
                     "The default value is '\"\"'.")}},
           {alternativeJids,
            #{value => "[string()]",
              desc =>
                  ?T("List of JIDs (XMPP server domains) a provider offers for "
                     "registration other than its main JID. "
                     "The default value is '[]'.")}},
           {busFactor,
            #{value => "integer()",
              desc =>
                  ?T("Bus factor of the XMPP service (i.e., the minimum number of "
                     "team members that the service could not survive losing) or '-1' for n/a. "
                     "The default value is '-1'.")}},
           {organization,
            #{value => "string()",
              desc =>
                  ?T("Type of organization providing the XMPP service. "
                     "Allowed values are: 'company', '\"commercial person\"', '\"private person\"', "
                     "'governmental', '\"non-governmental\"' or '\"\"'. "
                     "The default value is '\"\"'.")}},
           {passwordReset,
            #{value => "string()",
              desc =>
                  ?T("Password reset web page (per language) used for an automatic password reset "
                     "(e.g., via email) or describing how to manually reset a password "
                     "(e.g., by contacting the provider). "
                     "The keyword '@LANGUAGE_URL@' is replaced with each language. "
                     "The default value is an URL built automatically "
                     "if _`mod_register_web`_ is configured as a 'request_handler', "
                     "or '\"\"' otherwise.")}},
           {serverTesting,
            #{value => "true | false",
              desc =>
                  ?T("Whether tests against the provider's server are allowed "
                     "(e.g., certificate checks and uptime monitoring). "
                     "The default value is 'false'.")}},
           {maximumHttpFileUploadTotalSize,
            #{value => "integer()",
              desc =>
                  ?T("Maximum size of all shared files in total per user (number in megabytes (MB), "
                     "'0' for no limit or '-1' for less than 1 MB). "
                     "Attention: MB is used instead of MiB (e.g., 104,857,600 bytes = 100 MiB ≈ 104 MB). "
                     "This property is not about the maximum size of each shared file, "
                     "which is already retrieved via XMPP. "
                     "The default value is the value of the shaper value "
                     "of option 'access_hard_quota' "
                     "from module _`mod_http_upload_quota`_, or '0' otherwise.")}},
           {maximumHttpFileUploadStorageTime,
            #{value => "integer()",
              desc =>
                  ?T("Maximum storage duration of each shared file "
                     "(number in days, '0' for no limit or '-1' for less than 1 day). "
                     "The default value is the same as option 'max_days' "
                     "from module _`mod_http_upload_quota`_, or '0' otherwise.")}},
           {maximumMessageArchiveManagementStorageTime,
            #{value => "integer()",
              desc =>
                  ?T("Maximum storage duration of each exchanged message "
                     "(number in days, '0' for no limit or '-1' for less than 1 day). "
                     "The default value is '0'.")}},
           {professionalHosting,
            #{value => "true | false",
              desc =>
                  ?T("Whether the XMPP server is hosted with good internet connection speed, "
                     "uninterruptible power supply, access protection and regular backups. "
                     "The default value is 'false'.")}},
           {freeOfCharge,
            #{value => "true | false",
              desc =>
                  ?T("Whether the XMPP service can be used for free. "
                     "The default value is 'false'.")}},
           {legalNotice,
            #{value => "string()",
              desc =>
                  ?T("Legal notice web page (per language). "
                     "The keyword '@LANGUAGE_URL@' is replaced with each language. "
                     "The default value is '\"\"'.")}},
           {serverLocations,
            #{value => "[string()]",
              desc =>
                  ?T("List of language codes of Server/Backup locations. "
                     "The default value is an empty list: '[]'.")}},
           {since,
            #{value => "string()",
              desc =>
                  ?T("Date since the XMPP service is available. "
                     "The default value is an empty string: '\"\"'.")}}],
      example =>
          ["listen:",
           "  -",
           "    port: 443",
           "    module: ejabberd_http",
           "    tls: true",
           "    request_handlers:",
           "      /.well-known/xmpp-provider-v2.json: mod_providers",
           "",
           "modules:",
           "  mod_providers:",
           "    alternativeJids: [\"example1.com\", \"example2.com\"]",
           "    busFactor: 1",
           "    freeOfCharge: true",
           "    languages: [ag, ao, bg, en]",
           "    legalNotice: \"http://@HOST@/legal/@LANGUAGE_URL@/\"",
           "    maximumHttpFileUploadStorageTime: 0",
           "    maximumHttpFileUploadTotalSize: 0",
           "    maximumMessageArchiveManagementStorageTime: 0",
           "    organization: \"non-governmental\"",
           "    passwordReset: \"http://@HOST@/reset/@LANGUAGE_URL@/\"",
           "    professionalHosting: true",
           "    serverLocations: [ao, bg]",
           "    serverTesting: true",
           "    since: \"2025-12-31\"",
           "    website: \"http://@HOST@/website/@LANGUAGE_URL@/\""]}.

%%--------------------------------------------------------------------

%%| vim: set foldmethod=marker foldmarker=%%|,%%-:
