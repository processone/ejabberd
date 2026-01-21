%%%----------------------------------------------------------------------
%%% File    : mod_invites.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Account and Roster Invitation (aka Great Invitations)
%%% Created : Mon Sep 15 2025 by Stefan Strigler <stefan@strigler.de>
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
-module(mod_invites).

-author('stefan@strigler.de').

-protocol({xep, 379, '0.3.3', '26.01', "complete", ""}).
-protocol({xep, 401, '0.5.0', '26.01', "complete", ""}).
-protocol({xep, 445, '0.2.0', '26.01', "complete", ""}).

-behaviour(gen_mod).

%% gen_mod
-export([depends/2, mod_doc/0, mod_options/1, mod_opt_type/1, reload/3, start/2, stop/1]).

%% hooks and callbacks
-export([adhoc_commands/4, adhoc_items/4, c2s_unauthenticated_packet/2, remove_user/2,
         s2s_receive_packet/1, sm_receive_packet/1, stream_feature_register/2]).

%% Service Discovery
-export([get_local_identity/5, get_local_features/5]).

%% commands
-export([cleanup_expired/0, expire_tokens/2, gen_invite/1, gen_invite/2, list_invites/1]).

%% helpers
-export([create_account_allowed/2, get_invite/2, get_invites/2, get_max_invites/2, is_create_allowed/2,
         is_expired/1, is_reserved/3, is_token_valid/2, roster_add/2, send_presence/3,
         set_invitee/3, set_invitee/5, token_uri/1, xdata_field/3]).

%% ejabberd_http
-export([process/2]).

-ifdef(TEST).
-export([create_roster_invite/2, create_account_invite/4, is_token_valid/3]).
-endif.

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_commands.hrl").
-include("mod_invites.hrl").
-include("translate.hrl").

-type invite_token() :: #invite_token{}.

-callback cleanup_expired(Host :: binary()) -> non_neg_integer().
-callback create_invite(Invite :: invite_token()) -> invite_token().
-callback expire_tokens(User :: binary(), Server :: binary()) -> non_neg_integer().
-callback get_invite(Host :: binary(), Token :: binary()) ->
    invite_token() | {error, not_found}.
-callback get_invites(Host :: binary(), Inviter :: {User :: binary(), Host :: binary()}) ->
    [invite_token()].
-callback init(Host :: binary(), gen_mod:opts()) -> any().
-callback is_reserved(Host :: binary(), Token :: binary(), User :: binary()) -> boolean().
-callback is_token_valid(Host :: binary(), binary(), {binary(), binary()}) -> boolean().
-callback list_invites(Host :: binary()) -> [tuple()].
-callback remove_user(User :: binary(), Server :: binary()) -> any().
-callback set_invitee(Fun :: fun(() -> OkOrError),
                                Host :: binary(),
                                Token :: binary(),
                                Invitee :: binary(),
                                AccountName :: binary()) -> OkOrError | {error, conflict}
 when OkOrError :: ok | {error, term()}.

%% @format-begin

%%--------------------------------------------------------------------
%%| gen_mod callbacks

depends(_Host, _Opts) ->
    [{mod_adhoc, soft}, {mod_register, soft}, {mod_roster, soft}].

mod_doc() ->
    #{desc =>
          [?T("Allow User Invitation and Account Creation to create out-of-band "
              "links to onboard others onto the XMPP network and establish "
              "a mutual subscription."),
           ?T("This implements "
              "https://xmpp.org/extensions/xep-0379.html"
              "[XEP-0379: Pre-Authenticated Roster Subscription], "
              "https://xmpp.org/extensions/xep-0401.html"
              "[XEP-0401: Ad-hoc Account Invitation Generation], and "
              "https://xmpp.org/extensions/xep-0445.html"
              "[XEP-0445: Pre-Authenticated In-Band Registration]."),
           "",
           ?T("These invitations are created as XMPP URIs either via ad-hoc "
              "commands or via API commands (like _`generate_invite`_ API and "
              " _`generate_invite_with_username`_ API), are then meant to be "
              "sent out-of-band."),
           "",
           ?T("The receiving user should have installed a client that supports "
              "those invitations. Since this has proven to be a common obstacle "
              "for easy adoption, this module comes with an optional landing "
              "page parameter, that can either be some external service like an "
              "installation of "
              "https://github.com/modernxmpp/easy-xmpp-invitation[easy-xmpp-invitation], "
              "a third-party service like "
              "https://invite.joinjabber.org[JoinJabber] "
              "or for convenience a built-in service. This landing page will "
              "then guide the recipient with setting up a client "
              "and creating an account if required."),
           "",
           ?T("In order to use the included landing page feature, you have to"),
           "",
           ?T(" * have a copy of https://jquery.com[jQuery 3] and "
              "   https://getbootstrap.com/docs/4.6/getting-started/introduction/[Bootstrap 4] "
              "   in a shared directory on your system. If you're using Debian or "
              "   derivatives this is easiest accomplished by installing both "
              "   `libjs-jquery` and `libjs-bootstrap4` which will put them under "
              "   `/usr/share/javascript/{jquery,bootstrap4}`"),
           ?T(" * in `ejabberd.yml` configure a listener for module `ejabberd_http` "
              "   with a request handler for `/share: mod_http_fileserver`"),
           ?T(" * in the `modules` section configure `mod_http_fileserver` so that "
              "   `docroot` points to the shared directory from above "
              "   (e.g. `docroot: /usr/share/javascript`)"),
           ?T(" * configure `mod_invites` and set `landing_page` to either `auto` "
              "   or an URL template like `https://{{ host }}/invites/{{ invite.token }}` "
              "   if your server setup includes a so called reverse proxy"),
           "",
           "If you'd rather want to use an external service, set `landing_page` "
           "to something like "
           "`http://{{ host }}:8080/easy-xmpp-invites/#{{ invite.uri|strip_protocol }}` "
           "or `https://invites.joinjabber.org/#{{ invite.uri|strip_protocol }}`."],
      note => "added in 26.01",
      opts =>
          [{access_create_account,
            #{value => ?T("Access Rule Name"),
              desc =>
                  ?T("This is the name of an access rule that specifies who is allowed to create "
                     "invites of `create account`. The default value is `none`, i.e. nobody is able to"
                     " create such invites. Furthermore it applies to 'roster invites' and allows "
                     "to do in-band registration (IBR) if the sending user is allowed by this rule. "
                     "Users from the `admin` ACL are always allowed to create those invites."),
              example => ["mod_invites:", "  access_create_account: local"]}},
           {db_type,
            #{value => "mnesia | sql",
              desc =>
                  ?T("Same as top-level _`default_db`_ option, but applied to this "
                     "module only.")}},
           {landing_page,
            #{value => "none | auto | LandingPageURLTemplate",
              desc =>
                  ?T("Whether or not to use a landing page for the invites that "
                     "are being created. If using a template URL this can be "
                     "either be external or internal. Template variables include "
                     "`host`, `invite.token` and `invite.uri`, there are also "
                     "filters defined, most notably `strip_protocol`. Here's an example: "
                     "`http://{{ host }}:8080/easy-xmpp-invites/#{{ invite.uri|strip_protocol }}`. "
                     "For convenience you can choose `auto` here and the "
                     "`ejabberd_http` handler for `mod_invites` will  be used to "
                     "construct the landing page URL. Default is `none`.")}},
           {max_invites,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Maximum number of 'create account' invites that can be created "
                     "by an individual user. Users that match the `admin` ACL are "
                     "exempt from this limitation. Furthermore it restricts the use of "
                     "`roster invites` for account creation. Default is `infinity`.")}},
           {site_name,
            #{value => ?T("Site Name"),
              desc =>
                  ?T("A human readable name for your site. E.g. `\"My Beautiful Laundrette\"`. "
                     "Used in landing page templates.")}},
           {templates_dir,
            #{value => ?T("binary()"),
              desc =>
                  ?T("The directory containing templates and static files used "
                     "for landing page and web registration form. Only needs to "
                     "be set if you want to ship your own set of templates or "
                     "list of recommended apps.")}},
           {token_expire_seconds,
            #{value => "pos_integer()",
              desc =>
                  ?T("Number of seconds until token expires. Default value "
                     "is `432000` (that is five days: `5 * 24 * 60 * 60`)")}}],
      example =>
          [{?T("Basic configuration with landing page but without creating "
               "accounts, just roster invites:"),
            ["listen: ",
             "  -",
             "    port: 5281",
             "    module: ejabberd_http",
             "    request_handlers:",
             "      /invites: mod_invites",
             "      /share: mod_http_fileserver",
             "# [...]",
             "modules:",
             "  mod_http_fileserver:",
             "    docroot: /usr/share/javascript",
             "  mod_invites:",
             "    landing_page: auto"]},
           {?T("To allow only admin users to create invites of 'create account' and "
               "disable regular in-band registration, you would have a config like this:"),
            ["acl:",
             "  admin:",
             "    - user: \"my_admin_user@example.com\"",
             "",
             "access_rules:",
             "  register:",
             "    allow: admin",
             "",
             "modules:",
             "  mod_invites:",
             "    landing_page: auto",
             "  mod_register:",
             "    allow_modules:",
             "      - mod_invites"]},
           {?T("If you want all your users to be able to send 'create account' "
               "invites, you would configure your server like this instead. "
               "Note that the names of the access rules are just examples and "
               "you're free to change them."),
            ["acl:",
             "  local:",
             "    user_regexp: \"\"",
             "access_rules:",
             "  create_account_invite:",
             "    allow: local",
             "",
             "modules:",
             "  mod_invites:",
             "    access_create_account: create_account_invite",
             "    landing_page: auto",
             "  mod_register:",
             "    allow_modules:",
             "      - mod_invites"]}]}.

mod_options(Host) ->
    [{access_create_account, none},
     {db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {landing_page, none},
     {max_invites, infinity},
     {site_name, Host},
     {templates_dir, filename:join([code:priv_dir(ejabberd), ?MODULE, <<>>])},
     {token_expire_seconds, ?INVITE_TOKEN_EXPIRE_SECONDS_DEFAULT}].

reload(ServerHost, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(OldOpts, ?MODULE),
    if NewMod /= OldMod ->
           NewMod:init(ServerHost, NewOpts);
       true ->
           ok
    end.

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    Mod:init(Host, Opts),
    {ok,
     [{hook, remove_user, remove_user, 50},
      {hook, adhoc_local_items, adhoc_items, 50},
      {hook, adhoc_local_commands, adhoc_commands, 50},
      {hook, disco_local_features, get_local_features, 50},
      {hook, disco_local_identity, get_local_identity, 50},
      {hook, s2s_receive_packet, s2s_receive_packet, 50},
      {hook, sm_receive_packet, sm_receive_packet, 50},
      {hook, c2s_pre_auth_features, stream_feature_register, 50},
      %% note the sequence below is important
      {hook, c2s_unauthenticated_packet, c2s_unauthenticated_packet, 10},
      {commands, get_commands_spec()}]}.

stop(_Host) ->
    ok.

mod_opt_type(access_create_account) ->
    econf:acl();
mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(landing_page) ->
    econf:either(none, econf:binary());
mod_opt_type(max_invites) ->
    econf:pos_int(infinity);
mod_opt_type(site_name) ->
    econf:binary();
mod_opt_type(templates_dir) ->
    econf:directory();
mod_opt_type(token_expire_seconds) ->
    econf:pos_int().

%%--------------------------------------------------------------------
%%| ejabberd command callbacks

-spec get_commands_spec() -> [ejabberd_commands()].
get_commands_spec() ->
    [#ejabberd_commands{name = cleanup_expired_invite_tokens,
                        tags = [purge],
                        desc = "Delete invite tokens that have expired",
                        module = ?MODULE,
                        function = cleanup_expired,
                        note = "added in 26.01",
                        args = [],
                        result_example = 42,
                        result = {num_deleted, integer}},
     #ejabberd_commands{name = expire_invite_tokens,
                        tags = [purge],
                        desc =
                            "Sets expiration to a date in the past for all tokens belonging "
                            "to user",
                        module = ?MODULE,
                        function = expire_tokens,
                        note = "added in 26.01",
                        args = [{username, binary}, {host, binary}],
                        result_example = 42,
                        result = {num_deleted, integer}},
     #ejabberd_commands{name = generate_invite,
                        tags = [accounts],
                        desc = "Create a new 'create account' invite",
                        module = ?MODULE,
                        function = gen_invite,
                        note = "added in 26.01",
                        args = [{host, binary}],
                        args_desc = ["Hostname to generate 'create account' invite for."],
                        args_example = [<<"example.com">>],
                        result_example =
                            {<<"xmpp:example.com?register;preauth=4bsdpwVrRDQYnF9aQQKXGbF7">>,
                             <<"https://example.com/invites/4bsdpwVrRDQYnF9aQQKXGbF7">>},
                        result = {invite, {tuple, [{invite_uri, string}, {landing_page, string}]}}},
     #ejabberd_commands{name = generate_invite_with_username,
                        tags = [accounts],
                        desc =
                            "Create a new 'create account' invite token with a preselected "
                            "username",
                        module = ?MODULE,
                        function = gen_invite,
                        note = "added in 26.01",
                        args = [{username, binary}, {host, binary}],
                        args_desc =
                            ["Preselected Username",
                             "hostname  to generate 'create account' invite for."],
                        args_example = [<<"juliet">>, <<"example.com">>],
                        result_example =
                            {<<"xmpp:juliet@example.com?register;preauth=4bsdpwVrRDQYnF9aQQKXGbF7">>,
                             <<"https://example.com/invites/4bsdpwVrRDQYnF9aQQKXGbF7">>},
                        result = {invite, {tuple, [{invite_uri, string}, {landing_page, string}]}}},
     #ejabberd_commands{name = list_invites,
                        tags = [accounts],
                        desc = "List invite tokens",
                        module = ?MODULE,
                        function = list_invites,
                        note = "added in 26.01",
                        args = [{host, binary}],
                        args_desc = ["Hostname tokens are valid for"],
                        args_example = [<<"example.com">>],
                        %result_example = [{invite_token, invite}],
                        result =
                            {invites,
                             {list,
                              {invite,
                               {tuple,
                                [{token, string},
                                 {valid, atom},
                                 {created_at, string},
                                 {expires, string},
                                 {type, atom},
                                 {inviter, string},
                                 {invitee, string},
                                 {account_name, string},
                                 {token_uri, string},
                                 {landing_page, string}]}}}}}].

cleanup_expired() ->
    lists:foldl(fun(Host, Count) ->
                   case gen_mod:is_loaded(Host, ?MODULE) of
                       true ->
                           Count + db_call(Host, cleanup_expired, [Host]);
                       false ->
                           Count
                   end
                end,
                0,
                ejabberd_option:hosts()).

-spec expire_tokens(binary(), binary()) -> non_neg_integer().
expire_tokens(User0, Server0) ->
    User = jid:nodeprep(User0),
    Server = jid:nameprep(Server0),
    db_call(Server, expire_tokens, [User, Server]).

-spec gen_invite(binary()) -> binary() | {error, any()}.
gen_invite(Host) ->
    gen_invite(<<>>, Host).

-spec gen_invite(binary(), binary()) -> binary() | {error, any()}.
gen_invite(AccountName, Host0) ->
    Host = jid:nameprep(Host0),
    case create_account_invite(Host, {<<>>, Host}, AccountName, false) of
        {error, {module_not_loaded, ?MODULE, Host}} ->
            {error, host_unknown};
        {error, _Reason} = Error ->
            Error;
        Invite ->
            {token_uri(Invite), landing_page(Host, Invite)}
    end.

list_invites(Host) ->
    Invites = db_call(Host, list_invites, [Host]),
    Format =
        fun(#invite_token{token = TO,
                          inviter = {IU, IS},
                          invitee = IE,
                          created_at = CA,
                          expires = Exp,
                          type = TY,
                          account_name = AN} =
                Invite) ->
           {TO,
            is_token_valid(Host, TO),
            encode_datetime(CA),
            encode_datetime(Exp),
            TY,
            jid:encode(
                jid:make(IU, IS)),
            IE,
            AN,
            token_uri(Invite),
            landing_page(Host, Invite)}
        end,
    [Format(Invite) || Invite <- Invites].

%%--------------------------------------------------------------------
%%| hooks and callbacks

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    db_call(Server, remove_user, [LUser, LServer]).

%% ---

-spec adhoc_items(empty | {error, stanza_error()} | {result, [disco_item()]},
                  jid(),
                  jid(),
                  binary()) ->
                     {error, stanza_error()} | {result, [disco_item()]} | empty.
adhoc_items(Acc,
            #jid{lserver = LServer} = From,
            #jid{lserver = LServer, server = Server} = _To,
            Lang) ->
    InviteUser =
        #disco_item{jid = jid:make(Server),
                    node = ?NS_INVITE_INVITE,
                    name = trans(Lang, ?T("Invite User"))},
    CreateAccount =
        #disco_item{jid = jid:make(Server),
                    node = ?NS_INVITE_CREATE_ACCOUNT,
                    name = trans(Lang, ?T("Create Account"))},
    MyItems =
        case create_account_allowed(LServer, From) of
            ok ->
                [InviteUser, CreateAccount];
            {error, not_allowed} ->
                [InviteUser]
        end,
    case Acc of
        {result, AccItems} ->
            {result, AccItems ++ MyItems};
        _ ->
            {result, MyItems}
    end;
adhoc_items(Acc, _From, _To, _Lang) ->
    Acc.

%% ---

-spec adhoc_commands(empty | adhoc_command(), jid(), jid(), adhoc_command()) ->
                        adhoc_command() | {error, stanza_error()}.
adhoc_commands(_Acc,
               #jid{luser = LUser, lserver = LServer},
               #jid{lserver = LServer},
               #adhoc_command{node = ?NS_INVITE_INVITE = Node,
                              action = execute,
                              sid = SID,
                              lang = Lang}) ->
    Invite = create_roster_invite(LServer, {LUser, LServer}),
    XData =
        #xdata{type = result,
               title = trans(Lang, <<"New Invite Token Created">>),
               fields =
                   maybe_add_landing_url(LServer,
                                         Invite,
                                         Lang,
                                         [#xdata_field{var = <<"uri">>,
                                                       label = trans(Lang, <<"Invite URI">>),
                                                       type = 'text-single',
                                                       values = [token_uri(Invite)]},
                                          #xdata_field{var = <<"expire">>,
                                                       label =
                                                           trans(Lang,
                                                                 <<"Invite token valid until">>),
                                                       type = 'text-single',
                                                       values =
                                                           [encode_datetime(Invite#invite_token.expires)]}])},
    Result =
        #adhoc_command{status = completed,
                       node = Node,
                       xdata = XData,
                       sid = SID},
    {stop, Result};
adhoc_commands(_Acc,
               #jid{luser = LUser, lserver = LServer} = From,
               #jid{lserver = LServer},
               #adhoc_command{node = ?NS_INVITE_CREATE_ACCOUNT = Node,
                              sid = SID,
                              lang = Lang,
                              xdata = #xdata{type = submit, fields = Fields}}) ->
    check(fun create_account_allowed/2,
          [LServer, From],
          fun() ->
             AccountName = xdata_field(<<"username">>, Fields, <<>>),
             Invite =
                 create_account_invite(LServer,
                                       {LUser, LServer},
                                       AccountName,
                                       to_boolean(xdata_field(<<"roster-subscription">>,
                                                              Fields,
                                                              false))),
             case Invite of
                 {error, Reason} ->
                     {stop, {error, to_stanza_error(Lang, Reason)}};
                 _Invite ->
                     ResultFields =
                         maybe_add_landing_url(LServer,
                                               Invite,
                                               Lang,
                                               [#xdata_field{var = <<"uri">>,
                                                             label = trans(Lang, <<"Invite URI">>),
                                                             type = 'text-single',
                                                             values = [token_uri(Invite)]},
                                                #xdata_field{var = <<"expire">>,
                                                             label =
                                                                 trans(Lang,
                                                                       <<"Invite token valid until">>),
                                                             type = 'text-single',
                                                             values =
                                                                 [encode_datetime(Invite#invite_token.expires)]}]),
                     ResultXData = #xdata{type = result, fields = ResultFields},
                     Result =
                         #adhoc_command{status = completed,
                                        sid = SID,
                                        node = Node,
                                        xdata = ResultXData},
                     {stop, Result}
             end
          end,
          fun(Reason) -> {stop, {error, to_stanza_error(Lang, Reason)}} end);
adhoc_commands(_Acc,
               #jid{lserver = LServer} = From,
               #jid{lserver = LServer},
               #adhoc_command{node = ?NS_INVITE_CREATE_ACCOUNT = Node,
                              action = execute,
                              sid = SID,
                              lang = Lang}) ->
    check(fun create_account_allowed/2,
          [LServer, From],
          fun() ->
             XData =
                 #xdata{type = form,
                        title = trans(Lang, <<"Account Creation Invite">>),
                        fields =
                            [#xdata_field{var = <<"username">>,
                                          label = trans(Lang, <<"Username">>),
                                          type = 'text-single'},
                             #xdata_field{var = <<"roster-subscription">>,
                                          label = trans(Lang, <<"Roster Subscription">>),
                                          type = boolean}]},
             Actions = #adhoc_actions{execute = complete, complete = true},
             Result =
                 #adhoc_command{status = executing,
                                node = Node,
                                sid = maybe_gen_sid(SID),
                                actions = Actions,
                                xdata = XData},
             {stop, Result}
          end,
          fun(Reason) -> {stop, {error, to_stanza_error(Lang, Reason)}} end);
adhoc_commands(Acc, _From, _To, _Command) ->
    Acc.

-spec s2s_receive_packet({stanza() | drop, State}) ->
                            {stanza() | drop, State} | {stop, {drop, State}}
    when State :: ejabberd_s2s_in:state().
s2s_receive_packet({Stanza, State}) ->
    case sm_receive_packet(Stanza) of
        {stop, drop} ->
            {stop, {drop, State}};
        Res ->
            {Res, State}
    end.

-spec sm_receive_packet(stanza() | drop) -> stanza() | drop | {stop, drop}.
sm_receive_packet(#presence{from = From,
                            to = To,
                            type = subscribe,
                            sub_els = Els} =
                      Presence) ->
    case handle_pre_auth_token(Els, To, From) of
        true ->
            {stop, drop};
        false ->
            Presence
    end;
sm_receive_packet(Other) ->
    Other.

handle_pre_auth_token([], _To, _From) ->
    false;
handle_pre_auth_token([El | Els],
                      #jid{luser = LUser, lserver = LServer} = To,
                      FromFullJid) ->
    From = jid:remove_resource(FromFullJid),
    try xmpp:decode(El) of
        #preauth{token = Token} = PreAuth ->
            ?DEBUG("got preauth token: ~p", [PreAuth]),
            case is_token_valid(LServer, Token, {LUser, LServer}) of
                true ->
                    roster_add(To, From),
                    send_presence(To, From, subscribed),
                    send_presence(To, From, subscribe),
                    set_invitee(LServer, Token, From),
                    true;
                false ->
                    ?INFO_MSG("Got invalid preauth token from ~s: ~p", [jid:encode(From), PreAuth]),
                    false
            end;
        _Other ->
            handle_pre_auth_token(Els, To, From)
    catch
        _:{xmpp_codec, _} ->
            handle_pre_auth_token(Els, To, From)
    end.

%%--------------------------------------------------------------------
%%| Service Disco

-define(INFO_IDENTITY(Category, Type, Name, Lang),
        [#identity{category = Category,
                   type = Type,
                   name = trans(Lang, Name)}]).
-define(INFO_COMMAND(Name, Lang),
        ?INFO_IDENTITY(<<"automation">>, <<"command-node">>, Name, Lang)).

-spec get_local_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
get_local_identity(_Acc, _From, _To, ?NS_INVITE_CREATE_ACCOUNT, Lang) ->
    ?INFO_COMMAND(?T("Create Account"), Lang);
get_local_identity(_Acc, _From, _To, ?NS_INVITE_INVITE, Lang) ->
    ?INFO_COMMAND(?T("Invite User"), Lang);
get_local_identity(Acc, _From, _To, _NS, _Lang) ->
    Acc.

-spec get_local_features(mod_disco:features_acc(), jid(), jid(), binary(), binary()) ->
                            mod_disco:features_acc().
get_local_features(Acc, From, #jid{lserver = LServer} = _To, Ns, Lang) ->
    maybe
        allow ?=
            case Ns of
                ?NS_INVITE_CREATE_ACCOUNT ->
                    Access = mod_invites_opt:access_create_account(LServer),
                    acl:match_rule(LServer, Access, From);
                ?NS_INVITE_INVITE ->
                    allow;
                _ ->
                    false
            end,
        {result, [?NS_COMMANDS]}
    else
        false ->
            Acc;
        deny ->
            {error, xmpp:err_forbidden(?T("Access denied by service policy"), Lang)}
    end.

%%--------------------------------------------------------------------
%%| ibr hooks
stream_feature_register(Acc, Host) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
        true ->
            mod_invites_register:stream_feature_register(Acc, Host);
        false ->
            Acc
    end.

c2s_unauthenticated_packet(State, IQ) ->
    mod_invites_register:c2s_unauthenticated_packet(State, IQ).

%%--------------------------------------------------------------------
%%| ejabberd_http
process(LocalPath, Request) ->
    mod_invites_http:process(LocalPath, Request).

%%--------------------------------------------------------------------
%%| helpers
get_invite(Host, Token) ->
    db_call(Host, get_invite, [Host, Token]).

get_invites(Host, Inviter) ->
    db_call(Host, get_invites, [Host, Inviter]).

is_expired(#invite_token{expires = Expires}) ->
    Now = erlang:timestamp(),
    calendar:datetime_to_gregorian_seconds(Expires)
    < calendar:datetime_to_gregorian_seconds(
          calendar:now_to_universal_time(Now)).

is_reserved(Host, Token, User) ->
    db_call(Host, is_reserved, [Host, Token, User]).

-spec is_token_valid(binary(), binary()) -> boolean().
is_token_valid(Host, Token) ->
    is_token_valid(Host, Token, {<<>>, Host}).

-spec is_token_valid(binary(), binary(), {binary(), binary()}) -> boolean().
is_token_valid(Host, Token, Inviter) ->
    db_call(Host, is_token_valid, [Host, Token, Inviter]).

-spec set_invitee(binary(), binary(), jid() | binary()) -> ok.
set_invitee(Host, Token, #jid{} = InviteeJid) ->
    set_invitee(Host,
                Token,
                jid:encode(
                    jid:remove_resource(InviteeJid)),
                <<>>);
set_invitee(Host, Token, Invitee) ->
    set_invitee(Host, Token, Invitee, <<>>).

set_invitee(Host, Token, Invitee, AccountName) ->
    set_invitee(fun() -> ok end, Host, Token, Invitee, AccountName).

-spec set_invitee(binary(), binary(), binary(), binary()) -> ok.
set_invitee(F, Host, Token, Invitee, AccountName) ->
    %% This invalidates the invite token if Invitee isn't empty
    db_call(Host, set_invitee, [F, Host, Token, Invitee, AccountName]).

create_roster_invite(Host, Inviter) ->
    create_invite(roster_only, Host, Inviter, <<>>).

create_account_invite(Host, Inviter, AccountName, _Subscribe = true) ->
    create_invite(account_subscription, Host, Inviter, AccountName);
create_account_invite(Host, Inviter, AccountName, _Subcribe = false) ->
    create_invite(account_only, Host, Inviter, AccountName).

create_invite(Type, Host, Inviter, AccountName) ->
    try invite_token(Type, Host, Inviter, AccountName) of
        Invite ->
            ?DEBUG("Created invite: ~p", [Invite]),
            db_call(Host, create_invite, [Invite])
    catch
        _:({error, _Reason} = Error) ->
            Error;
        _:Error ->
            {error, Error}
    end.

check_account_name(<<>>, _) ->
    <<>>;
check_account_name(error, _) ->
    {error, account_name_invalid};
check_account_name(_, error) ->
    {error, hostname_invalid};
check_account_name(AccountName, Host) ->
    MyHosts = ejabberd_option:hosts(),
    case lists:member(Host, MyHosts) of
        false ->
            {error, host_unknown};
        true ->
            case ejabberd_auth:user_exists(AccountName, Host) of
                true ->
                    {error, user_exists};
                false ->
                    case is_reserved(Host, <<>>, AccountName) of
                        true ->
                            {error, reserved};
                        false ->
                            AccountName
                    end
            end
    end.

check_max_invites(roster_only, _) ->
    ok;
check_max_invites(_Type, {User, Host}) ->
    case is_create_allowed(User, Host) of
        true ->
            ok;
        false ->
            {error, num_invites_exceeded}
    end.

is_create_allowed(User, Host) ->
    case get_max_invites(User, Host) of
        infinity ->
            true;
        MaxInvites ->
            Invites = get_invites(Host, {User, Host}),
            NumCreated =
                lists:foldl(fun (#invite_token{type = roster_only, account_name = <<>>}, Num) ->
                                    Num;
                                (#invite_token{type = roster_only}, Num) ->
                                    %% We make sure to set account_name to the registered name when
                                    %% creating the account. This field is not used in roster_only
                                    %% scenario otherwise.
                                    Num + 1;
                                (#invite_token{invitee = <<>>} = Invite, Num) ->
                                    %% account create tokens count unless they haven't been used and
                                    %% are expired
                                    case mod_invites:is_expired(Invite) of
                                        true ->
                                            Num;
                                        false ->
                                            Num + 1
                                    end;
                                (_, Num) ->
                                    %% account create token where invitee is not empty
                                    Num + 1
                            end,
                            0,
                            Invites),
            NumCreated < MaxInvites
    end.

get_max_invites(<<>>, _Server) ->
    infinity;
get_max_invites(User, Server) ->
    case {mod_invites_opt:max_invites(Server),
          acl:match_acl(Server, {acl, admin}, #{usr => {User, Server, <<>>}})}
    of
        {infinity, _} ->
            infinity;
        {_, true} ->
            infinity;
        {MaxInvites, false} ->
            MaxInvites
    end.

maybe_throw({error, _} = Error) ->
    throw(Error);
maybe_throw(Good) ->
    Good.

invite_token(Type, Host, Inviter, AccountName0) ->
    maybe_throw(check_max_invites(Type, Inviter)),
    Token = p1_rand:get_alphanum_string(?INVITE_TOKEN_LENGTH_DEFAULT),
    AccountName = maybe_throw(check_account_name(jid:nodeprep(AccountName0), Host)),
    set_token_expires(#invite_token{token = Token,
                                    inviter = Inviter,
                                    type = Type,
                                    account_name = AccountName},
                      mod_invites_opt:token_expire_seconds(Host)).

token_uri(#invite_token{type = Type,
                        token = Token,
                        account_name = AccountName,
                        inviter = {_User, Host}})
    when Type =:= account_only; Type =:= account_subscription ->
    Invitee =
        case AccountName of
            <<>> ->
                Host;
            _ ->
                <<AccountName/binary, "@", Host/binary>>
        end,
    <<"xmpp:", Invitee/binary, "?register;preauth=", Token/binary>>;
token_uri(#invite_token{type = roster_only,
                        token = Token,
                        inviter = {User, Host}}) ->
    IBR = maybe_add_ibr_allowed(User, Host),
    Inviter =
        jid:encode(
            jid:make(User, Host)),
    <<"xmpp:", Inviter/binary, "?roster;preauth=", Token/binary, IBR/binary>>.

maybe_add_ibr_allowed(User, Host) ->
    case create_account_allowed(Host, jid:make(User, Host)) of
        ok ->
            <<";ibr=y">>;
        {error, not_allowed} ->
            <<>>
    end.

landing_page(Host, Invite) ->
    mod_invites_http:landing_page(Host, Invite).

-spec db_call(binary(), atom(), list()) -> any().
db_call(Host, Fun, Args) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    apply(Mod, Fun, Args).

-spec trans(binary(), binary()) -> binary().
trans(Lang, Msg) ->
    translate:translate(Lang, Msg).

-spec encode_datetime(calendar:datetime()) -> binary().
encode_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                 [Year, Month, Day, Hour, Minute, Second])).

set_token_expires(#invite_token{created_at = CreatedAt} = Invite, ExpireSecs) ->
    Invite#invite_token{expires =
                            calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(CreatedAt)
                                                                   + ExpireSecs)}.

xdata_field(_Field, [], Default) ->
    Default;
xdata_field(Field, [#xdata_field{var = Field, values = [<<>> | _]} | _], Default) ->
    Default;
xdata_field(Field, [#xdata_field{var = Field, values = [Result | _]} | _], _Default) ->
    Result;
xdata_field(Field, [_NoMatch | Fields], Default) ->
    xdata_field(Field, Fields, Default).

maybe_add_landing_url(Host, Invite, Lang, XData) ->
    case landing_page(Host, Invite) of
        <<>> ->
            XData;
        LandingPage ->
            [#xdata_field{var = <<"landing-url">>,
                          values = [LandingPage],
                          label = trans(Lang, <<"Invite Landing Page URL">>),
                          type = 'text-single'}
             | XData]
    end.

check(Check, Args, Fun, Else) ->
    case erlang:apply(Check, Args) of
        ok ->
            Fun();
        {error, Reason} ->
            Else(Reason)
    end.

create_account_allowed(Host, User) ->
    case mod_invites_opt:access_create_account(Host) of
        none ->
            {error, not_allowed};
        Access ->
            case acl:match_rule(Host, Access, User) of
                deny ->
                    {error, not_allowed};
                allow ->
                    ok
            end
    end.

to_boolean(Boolean) when is_boolean(Boolean) ->
    Boolean;
to_boolean(True) when True == <<"1">>; True == <<"true">> ->
    true;
to_boolean(False) when False == <<"0">>; False == <<"false">> ->
    false.

to_stanza_error(Lang, not_allowed) ->
    Text = trans(Lang, <<"Access forbidden">>),
    xmpp:err_forbidden(Text, Lang);
to_stanza_error(Lang, Reason) ->
    Text = trans(Lang, reason_to_text(Reason)),
    xmpp:err_bad_request(Text, Lang).

reason_to_text(account_name_invalid) ->
    ?T("Username invalid");
reason_to_text(host_unknown) ->
    ?T("Host unknown");
reason_to_text(hostname_invalid) ->
    ?T("Hostname invalid");
reason_to_text(num_invites_exceeded) ->
    ?T("Maximum number of invites reached");
reason_to_text(reserved) ->
    ?T("Username is reserved");
reason_to_text(user_exists) ->
    ?T("User already exists").

maybe_gen_sid(<<>>) ->
    p1_rand:get_alphanum_string(?INVITE_TOKEN_LENGTH_DEFAULT);
maybe_gen_sid(SID) ->
    SID.

roster_add(UserJID, RosterItemJID) ->
    RosterItem =
        #roster_item{jid = RosterItemJID,
                     subscription = from,
                     ask = subscribe},
    mod_roster:set_item_and_notify_clients(UserJID, RosterItem, true).

send_presence(From, To, Type) ->
    Presence =
        #presence{from = From,
                  to = To,
                  type = Type},
    ejabberd_router:route(Presence).
