%%%----------------------------------------------------------------------
%%% File    : mod_invites_http.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Provide web page(s) to sign up using an invite token.
%%% Created : Fri Oct 31 2025 by Stefan Strigler <stefan@strigler.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2026 ProcessOne
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
-module(mod_invites_http).

-author('stefan@strigler.de').

-include("logger.hrl").

-export([process/2, landing_page/2]).

-ifdef(TEST).
-export([apps_json/3]).
-endif.

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_http.hrl").
-include("mod_invites.hrl").
-include("translate.hrl").

-define(HTTP(Code, CT, Text), {Code, [{<<"Content-Type">>, CT}], Text}).
-define(HTTP(Code, Text), ?HTTP(Code, <<"text/plain">>, Text)).
-define(HTTP_OK(Text), ?HTTP(200, <<"text/html">>, Text)).
-define(NOT_FOUND, ?HTTP(404, ?T("NOT FOUND"))).
-define(NOT_FOUND(Text), ?HTTP(404, <<"text/html">>, Text)).
-define(BAD_REQUEST, ?HTTP(400, ?T("BAD REQUEST"))).
-define(BAD_REQUEST(Text), ?HTTP(400, <<"text/html">>, Text)).

-define(DEFAULT_CONTENT_TYPE, <<"application/octet-stream">>).
-define(CONTENT_TYPES,
	[{<<".js">>, <<"application/javascript">>},
	 {<<".png">>, <<"image/png">>},
	 {<<".svg">>, <<"image/svg+xml">>}]).

-define(STATIC, <<"static">>).
-define(REGISTRATION, <<"registration">>).
-define(STATIC_CTX, {static, <<"/", Base/binary, "/", ?STATIC/binary>>}).
-define(SITE_NAME_CTX(Name), {site_name, Name}).
-define(LANG(Lang), {lang, Lang}).

%% @format-begin

landing_page(Host, Invite) ->
    case mod_invites_opt:landing_page(Host) of
        none ->
            <<>>;
        auto ->
            try ejabberd_http:get_auto_url(any, mod_invites) of
                AutoURL0 ->
                    AutoURL = misc:expand_keyword(<<"@HOST@">>, AutoURL0, Host),
                    render_landing_page_url(<<AutoURL/binary, "{{ invite.token }}">>, Host, Invite)
            catch
                _:_ ->
                    ?WARNING_MSG("'auto' URL configured for mod_invites but no request_handler found in your ~s listeners configuration.",
                                 [Host]),
                    <<>>
            end;
        Tmpl ->
            render_landing_page_url(Tmpl, Host, Invite)
    end.

render_landing_page_url(Tmpl, Host, Invite) ->
    Ctx = [{invite, invite_to_proplist(Invite)}, {host, Host}],
    render_url(Tmpl, Ctx).

-spec process(LocalPath :: [binary()], #request{}) ->
                 {HTTPCode :: integer(), [{binary(), binary()}], Page :: string()}.
process([?STATIC | StaticFile], #request{host = Host} = Request) ->
    ?DEBUG("Static file requested ~p:~n~p", [StaticFile, Request]),
    TemplatesDir = mod_invites_opt:templates_dir(Host),
    Filename = filename:join([TemplatesDir, "static" | StaticFile]),
    case file:read_file(Filename) of
        {ok, Content} ->
            CT = guess_content_type(Filename),
            ?HTTP(200, CT, Content);
        {error, _} ->
            ?NOT_FOUND
    end;
process([Token | _] = LocalPath, #request{host = Host, lang = Lang} = Request) ->
    ?DEBUG("Requested:~n~p", [Request]),
    try mod_invites:is_token_valid(Host, Token) of
        true ->
            case mod_invites:get_invite(Host, Token) of
                #invite_token{type = roster_only} = Invite ->
                    process_roster_token(LocalPath, Request, Invite);
                Invite ->
                    process_valid_token(LocalPath, Request, Invite)
            end;
        false ->
            ?NOT_FOUND(render(Host, Lang, <<"invite_invalid.html">>, ctx(Request, LocalPath)))
    catch
        _:not_found ->
            ?NOT_FOUND
    end;
process([], _Request) ->
    ?NOT_FOUND.

process_valid_token([_Token, AppID, ?REGISTRATION] = LocalPath,
                    #request{method = 'POST'} = Request,
                    Invite) ->
    process_register_post(Invite, AppID, Request, LocalPath);
process_valid_token([_Token, AppID, ?REGISTRATION] = LocalPath, Request, Invite) ->
    process_register_form(Invite, AppID, Request, LocalPath);
process_valid_token([_Token, ?REGISTRATION] = LocalPath,
                    #request{method = 'POST'} = Request,
                    Invite) ->
    process_register_post(Invite, <<>>, Request, LocalPath);
process_valid_token([_Token, ?REGISTRATION] = LocalPath, Request, Invite) ->
    process_register_form(Invite, <<>>, Request, LocalPath);
process_valid_token([_Token, AppID] = LocalPath,
                    #request{host = Host, lang = Lang} = Request,
                    Invite) ->
    try app_ctx(Host, AppID, Lang, ctx(Invite, Request, LocalPath)) of
        AppCtx ->
            render_ok(Host, Lang, <<"client.html">>, AppCtx)
    catch
        _:not_found ->
            ?NOT_FOUND
    end;
process_valid_token([_Token] = LocalPath,
                    #request{host = Host, lang = Lang} = Request,
                    Invite) ->
    Ctx0 = ctx(Invite, Request, LocalPath),
    Apps =
        lists:map(fun(App0) ->
                     App = app_id(App0),
                     render_app_urls(App, [{app, App} | Ctx0])
                  end,
                  apps_json(Host, Lang, Ctx0)),
    Ctx = [{apps, Apps} | Ctx0],
    render_ok(Host, Lang, <<"invite.html">>, Ctx);
process_valid_token(_, _, _) ->
    ?NOT_FOUND.

process_register_form(Invite,
                      AppID,
                      #request{host = Host, lang = Lang} = Request,
                      LocalPath) ->
    try app_ctx(Host, AppID, Lang, ctx(Invite, Request, LocalPath)) of
        AppCtx ->
            Body = render_register_form(Request, AppCtx, maybe_add_username(Invite)),
            ?HTTP_OK(Body)
    catch
        _:not_found ->
            ?NOT_FOUND
    end.

render_register_form(#request{host = Host, lang = Lang}, Ctx, AdditionalCtx) ->
    MinLength =
        case mod_register_opt:password_strength(Host) of
            0 ->
                0;
            _ ->
                6
        end,
    render(Host,
           Lang,
           <<"register.html">>,
           [{password_min_length, MinLength} | Ctx] ++ AdditionalCtx).

process_register_post(Invite,
                      AppID,
                      #request{host = Host,
                               q = Q,
                               lang = Lang,
                               ip = {Source, _}} =
                          Request,
                      LocalPath) ->
    ?DEBUG("got query: ~p", [Q]),
    Username = proplists:get_value(<<"user">>, Q),
    Password = proplists:get_value(<<"password">>, Q),
    Token = Invite#invite_token.token,
    try {app_ctx(Host, AppID, Lang, ctx(Invite, Request, LocalPath)),
         ensure_same(Token, proplists:get_value(<<"token">>, Q))}
    of
        {AppCtx, ok} ->
            case mod_invites_register:try_register(Invite, Username, Host, Password, Source, Lang)
            of
                {ok, _UpdatedInvite} ->
                    Ctx = [{username, Username}, {password, Password} | AppCtx],
                    render_ok(Host, Lang, <<"register_success.html">>, Ctx);
                {error,
                 #stanza_error{text = Text,
                               type = Type,
                               reason = Reason} =
                     Error} ->
                    ?DEBUG("registration failed with error: ~p", [Error]),
                    Msg = xmpp:get_text(Text, xmpp:prep_lang(Lang)),
                    case Type of
                        T when T == cancel; T == modify ->
                            Body =
                                render_register_form(Request,
                                                     AppCtx,
                                                     [{username, Username},
                                                      {error,
                                                       [{text, Msg},
                                                        {class, error_class(Reason)}]}]),
                            ?BAD_REQUEST(Body);
                        _ ->
                            render_bad_request(Host,
                                               <<"register_error.html">>,
                                               [{message, Msg} | ctx(Request, LocalPath)])
                    end
            end
    catch
        _:not_found ->
            ?NOT_FOUND;
        _:no_match ->
            ?BAD_REQUEST
    end.

error_class('jid-malformed') ->
    username;
error_class('not-allowed') ->
    username;
error_class(conflict) ->
    username;
error_class('not-acceptable') ->
    password;
error_class(_) ->
    undefined.

process_roster_token([_Token] = LocalPath,
                     #request{host = Host, lang = Lang} = Request,
                     Invite) ->
    Ctx0 = ctx(Invite, Request, LocalPath),
    Apps =
        lists:map(fun(App = #{<<"download">> := #{<<"buttons">> := [Button | _]}}) ->
                     ProceedUrl =
                         case render_app_button_url(Button, Ctx0) of
                             #{magic_link := MagicLink} ->
                                 MagicLink;
                             #{<<"url">> := Url} ->
                                 Url
                         end,
                     App#{proceed_url => ProceedUrl,
                          select_text => translate:translate(Lang, ?T("Install"))}
                  end,
                  apps_json(Host, Lang, Ctx0)),
    Ctx = [{apps, Apps} | Ctx0],
    render_ok(Host, Lang, <<"roster.html">>, Ctx);
process_roster_token(_, _, _) ->
    ?NOT_FOUND.

ensure_same(V, V) ->
    ok;
ensure_same(_, _) ->
    throw(no_match).

app_ctx(_Host, <<>>, _Lang, Ctx) ->
    Ctx;
app_ctx(Host, AppID, Lang, Ctx) ->
    FilteredApps =
        [App || A <- apps_json(Host, Lang, Ctx), maps:get(<<"id">>, App = app_id(A)) == AppID],
    case FilteredApps of
        [App] ->
            [{app, render_app_button_urls(App, Ctx)} | Ctx];
        [] ->
            throw(not_found)
    end.

ctx(#request{host = Host,
             path = Path,
             lang = Lang},
    LocalPath) ->
    Base =
        iolist_to_binary(uri_string:normalize(
                             lists:join(<<"/">>, Path -- LocalPath))),
    SiteName = mod_invites_opt:site_name(Host),
    [{base, Base}, ?STATIC_CTX, ?SITE_NAME_CTX(SiteName), ?LANG(Lang)].

ctx(Invite, #request{host = Host} = Request, LocalPath) ->
    [{invite, invite_to_proplist(Invite)},
     {uri, mod_invites:token_uri(Invite)},
     {domain, Host},
     {token, Invite#invite_token.token},
     {registration_url, <<(Invite#invite_token.token)/binary, "/", ?REGISTRATION/binary>>}
     | ctx(Request, LocalPath)].

apps_json(Host, Lang, Ctx) ->
    AppsBins = render(Host, Lang, <<"apps.json">>, Ctx),
    AppsBin = binary_join(AppsBins, <<>>),
    misc:json_decode(AppsBin).

app_id(App = #{<<"id">> := _ID}) ->
    App;
app_id(App = #{<<"name">> := Name}) ->
    App#{<<"id">> => re:replace(Name, "[^a-zA-Z0-9]+", "-", [global, {return, binary}])}.

invite_to_proplist(I) ->
    [{uri, mod_invites:token_uri(I)} | lists:zip(record_info(fields, invite_token),
                                                 tl(tuple_to_list(I)))].

render_url(Tmpl, Vars) ->
    Renderer = tmpl_to_renderer(Tmpl),
    {ok, URL} = Renderer:render(Vars),
    binary_join(URL, <<>>).

render_app_urls(App = #{<<"supports_preauth_uri">> := true}, Vars) ->
    App#{proceed_url => render_url(<<"{{ invite.token }}/{{ app.id }}">>, Vars)};
render_app_urls(App, Vars) ->
    App#{proceed_url =>
             render_url(<<"{{ invite.token }}/{{ app.id }}/", ?REGISTRATION/binary>>, Vars)}.

render_app_button_urls(App = #{<<"download">> := #{<<"buttons">> := Buttons}}, Vars) ->
    App#{<<"download">> =>
             #{<<"buttons">> =>
                   lists:map(fun(Button) -> render_app_button_url(Button, [{button, Button} | Vars])
                             end,
                             Buttons)}};
render_app_button_urls(App, _Vars) ->
    App.

render_app_button_url(Button = #{<<"magic_link_format">> := MLF}, Vars) ->
    Button#{magic_link => render_url(MLF, Vars)};
render_app_button_url(Button, _Vars) ->
    Button.

file_to_renderer(Host, Filename) ->
    ModName =
        binary_to_atom(<<"mod_invites_template__", Host/binary, "__", Filename/binary>>),
    TemplatesDir = mod_invites_opt:templates_dir(Host),
    TemplatePath = binary_to_list(filename:join([TemplatesDir, Filename])),
    {ok, _Mod, Warnings} =
        erlydtl:compile_file(TemplatePath,
                             ModName,
                             [{out_dir, false},
                              return,
                              {libraries, [{mod_invites_http_erlylib, mod_invites_http_erlylib}]},
                              {default_libraries, [mod_invites_http_erlylib]}]),
    ?DEBUG("got warnings: ~p", [Warnings]),
    ModName.

tmpl_to_renderer(Tmpl) ->
    ModName = binary_to_atom(<<"mod_invites_template__", Tmpl/binary>>),
    case erlang:function_exported(ModName, render, 1) of
        true ->
            ModName;
        false ->
            {ok, _Mod} =
                erlydtl:compile_template(Tmpl,
                                         ModName,
                                         [{out_dir, false},
                                          {libraries,
                                           [{mod_invites_http_erlylib, mod_invites_http_erlylib}]},
                                          {default_libraries, [mod_invites_http_erlylib]}]),
            ModName
    end.

render(Host, Lang, File, Ctx) ->
    Renderer = file_to_renderer(Host, File),
    {ok, Rendered} =
        Renderer:render(Ctx,
                        [{locale, Lang},
                         {translation_fun,
                          fun(Msg, TFLang) -> translate:translate(lang(TFLang), list_to_binary(Msg))
                          end}]),
    Rendered.

lang(default) ->
    <<"en">>;
lang(Lang) ->
    Lang.

render_ok(Host, Lang, File, Ctx) ->
    ?HTTP_OK(render(Host, Lang, File, Ctx)).

render_bad_request(Host, File, Ctx) ->
    Renderer = file_to_renderer(Host, File),
    {ok, Rendered} = Renderer:render(Ctx),
    ?BAD_REQUEST(Rendered).

-spec guess_content_type(binary()) -> binary().
guess_content_type(FileName) ->
    mod_http_fileserver:content_type(FileName, ?DEFAULT_CONTENT_TYPE, ?CONTENT_TYPES).

maybe_add_username(#invite_token{account_name = <<>>}) ->
    [];
maybe_add_username(#invite_token{account_name = AccountName}) ->
    [{username, AccountName}].

-spec binary_join(binary() | [binary()], binary()) -> binary().
binary_join(Bin, _Sep) when is_binary(Bin) ->
    Bin;
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(fun(A, B) ->
                   if bit_size(B) > 0 ->
                          <<A/binary, Sep/binary, B/binary>>;
                      true ->
                          A
                   end
                end,
                <<>>,
                List).
