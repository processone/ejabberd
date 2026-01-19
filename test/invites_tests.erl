%%%-------------------------------------------------------------------
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Created : 16 September 2025 by Stefan Strigler
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
-module(invites_tests).

-compile(export_all).

-import(suite, [auth/1, bind/1, disconnect/1, get_features/2, init_stream/1, open_session/1,
                recv_presence/1, recv_message/1, recv_iq/1, self_presence/2, send_recv/2, send/2,
                set_opt/3, set_opts/2]).

-include("suite.hrl").
-include("mod_invites.hrl").
-include("mod_roster.hrl").

%% killme
-record(ejabberd_module,
        {module_host = {undefined, <<"">>} :: {atom(), binary()},
         opts = [] :: any(),
         registrations = [] :: [any()],
         order = 0 :: integer()}).

%% @format-begin

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single tests
%%%===================================================================

single_cases() ->
    {invites_single,
     [sequence],
     [single_test(gen_invite),
      single_test(cleanup_expired),
      single_test(adhoc_items),
      single_test(adhoc_command_invite),
      single_test(adhoc_command_create_account),
      single_test(token_valid),
      single_test(remove_user),
      single_test(expire_tokens),
      single_test(max_invites),
      single_test(presence_with_preauth_token),
      single_test(is_reserved),
      single_test(stream_feature),
      single_test(ibr),
      single_test(ibr_reserved),
      single_test(ibr_subscription),
      single_test(ibr_conflict),
      single_test(http)]}.

%%%===================================================================

gen_invite(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    {TokenURI, _LandingPage} = mod_invites:gen_invite(<<"foo">>, Server),
    ?match(<<"xmpp:foo@", Server:(size(Server))/binary, "?register;preauth=", _/binary>>,
           TokenURI),
    Token = token_from_uri(TokenURI),
    #invite_token{inviter = {<<>>, Server},
                  type = account_only,
                  account_name = <<"foo">>} =
        mod_invites:get_invite(Server, Token),
    {TokenURI2, _LP2} = mod_invites:gen_invite(Server),
    ?match(<<"xmpp:", _/binary>>, TokenURI2),
    Token2 = token_from_uri(TokenURI2),
    #invite_token{inviter = {<<>>, Server},
                  type = account_only,
                  account_name = <<>>} =
        mod_invites:get_invite(Server, Token2),
    ?match({error, user_exists}, mod_invites:gen_invite(User, Server)),
    ?match({error, account_name_invalid},
           mod_invites:gen_invite(<<"@bad_acccount_name">>, Server)),
    ?match({error, host_unknown}, mod_invites:gen_invite(<<"bar">>, <<"non.existant.host">>)),

    ?match(2, length(mod_invites:list_invites(Server))),
    %% TooLongHostname = list_to_binary([$a || _ <- lists:seq(1, 1024)]),
    %% ?match({error, hostname_invalid}, mod_invites:gen_invite(<<"foo">>, TooLongHostname)),
    mod_invites:expire_tokens(<<>>, Server),
    ?match(2, mod_invites:cleanup_expired()),
    disconnect(Config).

cleanup_expired(Config) ->
    Server = ?config(server, Config),
    create_account_invite(Server, {<<"foo">>, Server}),
    mod_invites:expire_tokens(<<"foo">>, Server),
    Token = token_from_uri(element(1, mod_invites:gen_invite(<<"foobar">>, Server))),
    ?match(1, mod_invites:cleanup_expired()),
    ?match(#invite_token{}, mod_invites:get_invite(Server, Token)),
    ?match(0, mod_invites:cleanup_expired()),
    mod_invites:expire_tokens(<<>>, Server),
    ?match(1, mod_invites:cleanup_expired()),
    disconnect(Config).

adhoc_items(Config) ->
    Server = ?config(server, Config),
    ServerJID = jid:from_string(Server),
    User = ?config(user, Config),
    UserJID = jid:from_string(User),

    ?match(deny,
           acl:match_rule(Server,
                          gen_mod:get_module_opt(Server, mod_invites, access_create_account),
                          UserJID)),

    DiscoCommands = #disco_items{node = ?NS_COMMANDS},
    #iq{type = result, sub_els = [#disco_items{node = ?NS_COMMANDS, items = Items}]} =
        send_recv(Config,
                  #iq{type = get,
                      to = ServerJID,
                      sub_els = [DiscoCommands]}),
    ?match(true, [I || I = #disco_item{node = ?NS_INVITE_INVITE} <- Items] /= []),
    #iq{type = result,
        sub_els =
            [#disco_info{node = ?NS_INVITE_INVITE,
                         identities =
                             [#identity{category = <<"automation">>, type = <<"command-node">>}],
                         features = [?NS_COMMANDS]}]} =
        send_recv(Config,
                  #iq{type = get,
                      to = ServerJID,
                      sub_els = [#disco_info{node = ?NS_INVITE_INVITE}]}),
    #iq{type = error} =
        send_recv(Config,
                  #iq{type = get,
                      to = ServerJID,
                      sub_els = [#disco_info{node = ?NS_INVITE_CREATE_ACCOUNT}]}),

    OldOpts = gen_mod:get_module_opts(Server, mod_invites),
    NewOpts = gen_mod:set_opt(access_create_account, account_invite, OldOpts),
    update_module_opts(Server, mod_invites, NewOpts),
    ?match(allow,
           acl:match_rule(Server,
                          gen_mod:get_module_opt(Server, mod_invites, access_create_account),
                          UserJID)),
    #iq{type = result, sub_els = [#disco_items{node = ?NS_COMMANDS, items = NewItems}]} =
        send_recv(Config,
                  #iq{type = get,
                      to = ServerJID,
                      sub_els = [DiscoCommands]}),
    ?match(true, [I || I = #disco_item{node = ?NS_INVITE_INVITE} <- NewItems] /= []),
    ?match(true, [I || I = #disco_item{node = ?NS_INVITE_CREATE_ACCOUNT} <- NewItems] /= []),

    #iq{type = result,
        sub_els =
            [#disco_info{node = ?NS_INVITE_INVITE,
                         identities =
                             [#identity{category = <<"automation">>, type = <<"command-node">>}],
                         features = [?NS_COMMANDS]}]} =
        send_recv(Config,
                  #iq{type = get,
                      to = ServerJID,
                      sub_els = [#disco_info{node = ?NS_INVITE_INVITE}]}),
    #iq{type = result,
        sub_els =
            [#disco_info{node = ?NS_INVITE_CREATE_ACCOUNT,
                         identities =
                             [#identity{category = <<"automation">>, type = <<"command-node">>}],
                         features = [?NS_COMMANDS]}]} =
        send_recv(Config,
                  #iq{type = get,
                      to = ServerJID,
                      sub_els = [#disco_info{node = ?NS_INVITE_CREATE_ACCOUNT}]}),

    update_module_opts(Server, mod_invites, OldOpts),
    ?match(deny,
           acl:match_rule(Server,
                          gen_mod:get_module_opt(Server, mod_invites, access_create_account),
                          UserJID)),
    disconnect(Config).

adhoc_command_invite(Config) ->
    Server = ?config(server, Config),
    ServerJID = jid:from_string(Server),
    User = ?config(user, Config),
    Command = #adhoc_command{node = ?NS_INVITE_INVITE},
    #iq{type = result,
        sub_els =
            [#adhoc_command{status = completed,
                            xdata = #xdata{type = result, fields = XdataFields}}]} =
        send_recv(Config,
                  #iq{type = set,
                      to = ServerJID,
                      sub_els = [Command]}),
    Uri = xdata_field(<<"uri">>, XdataFields),
    ?match({match, [_, _]},
           re:run(Uri,
                  <<"xmpp:",
                    (re_escape(User))/binary,
                    "@",
                    Server/binary,
                    "\\?roster;preauth=(.+)">>)),
    ?match(true, xdata_field(<<"expire">>, XdataFields) /= undefined),
    Token = token_from_uri(Uri),
    ?match(true, mod_invites:is_token_valid(Server, Token, {User, Server})),
    mod_invites:remove_user(User, Server),
    disconnect(Config).

adhoc_command_create_account(Config) ->
    Server = ?config(server, Config),
    ServerJID = jid:from_string(Server),
    Command = #adhoc_command{node = ?NS_INVITE_CREATE_ACCOUNT},
    ResForbidden =
        send_recv(Config,
                  #iq{type = set,
                      to = ServerJID,
                      sub_els = [Command]}),
    ?match(#iq{type = error}, ResForbidden),
    #iq{sub_els = ForbiddenSubEls} = ResForbidden,
    ?match(true,
           [ok || #stanza_error{type = auth, reason = forbidden} <- ForbiddenSubEls] /= []),
    OldOpts = gen_mod:get_module_opts(Server, mod_invites),
    NewOpts = gen_mod:set_opt(access_create_account, account_invite, OldOpts),
    update_module_opts(Server, mod_invites, NewOpts),
    ResultXDataFields1 = test_create_account(Config, <<>>, <<"0">>),
    ?match({match, [_, _]},
           re:run(xdata_field(<<"uri">>, ResultXDataFields1),
                  <<"xmpp:", Server/binary, "\\?register;preauth=(.+)">>)),
    ResultXDataFields2 = test_create_account(Config, <<"foobar">>, <<"0">>),
    ?match({match, [_, _]},
           re:run(xdata_field(<<"uri">>, ResultXDataFields2),
                  <<"xmpp:foobar@", Server/binary, "\\?register;preauth=(.+)">>)),
    ResultXDataFields3 = test_create_account(Config, <<>>, <<"1">>),
    ?match({match, _},
           re:run(xdata_field(<<"uri">>, ResultXDataFields3),
                  <<"xmpp:", Server/binary, "\\?register;preauth=([a-zA-Z0-9]+)">>,
                  [{capture, all_but_first, binary}])),
    Token3 = token_from_uri(xdata_field(<<"uri">>, ResultXDataFields3, <<>>)),
    #invite_token{account_name = <<>>, type = account_subscription} =
        mod_invites:get_invite(Server, Token3),
    ResultXDataFields4 = test_create_account(Config, <<"foobar_with_sub">>, <<"1">>),
    ?match({match, _},
           re:run(xdata_field(<<"uri">>, ResultXDataFields4),
                  <<"xmpp:foobar_with_sub@", Server/binary, "\\?register;preauth=([a-zA-Z0-9]+)">>,
                  [{capture, all_but_first, binary}])),
    Token4 = token_from_uri(xdata_field(<<"uri">>, ResultXDataFields4, <<>>)),
    #invite_token{account_name = <<"foobar_with_sub">>, type = account_subscription} =
        mod_invites:get_invite(Server, Token4),
    update_module_opts(Server, mod_invites, OldOpts),
    User = jid:nodeprep(?config(user, Config)),
    mod_invites:remove_user(User, Server),
    disconnect(Config).

token_valid(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    {TokenURI, _LandingPage} = mod_invites:gen_invite(<<"foobar">>, Server),
    Token = token_from_uri(TokenURI),
    ?match(true, mod_invites:is_token_valid(Server, Token)),
    Inviter = {<<"foo">>, Server},
    #invite_token{token = AccountToken} = create_account_invite(Server, Inviter),
    ?match(true, mod_invites:is_token_valid(Server, AccountToken, Inviter)),
    try mod_invites:is_token_valid(Server, <<"madeUptoken">>) of
        break ->
            broken
    catch
        _:E ->
            ?match(not_found, E)
    end,
    ?match(false,
           mod_invites:is_token_valid(Server, AccountToken, {<<"someoneElse">>, Server})),
    mod_invites:expire_tokens(<<"foo">>, Server),
    ?match(false, mod_invites:is_token_valid(Server, AccountToken, Inviter)),
    mod_invites:cleanup_expired(),
    mod_invites:remove_user(User, Server),
    mod_invites:expire_tokens(<<>>, Server),
    ?match(1, mod_invites:cleanup_expired()),
    disconnect(Config).

remove_user(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    Inviter = {User, Server},
    #invite_token{} = create_account_invite(Server, Inviter),
    ?match(1, length(mod_invites:get_invites(Server, Inviter))),
    mod_invites:remove_user(User, Server),
    ?match(0, length(mod_invites:get_invites(Server, Inviter))),
    disconnect(Config).

expire_tokens(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    Inviter = {User, Server},
    #invite_token{token = RosterToken} = mod_invites:create_roster_invite(Server, Inviter),
    #invite_token{token = AccountToken} = create_account_invite(Server, Inviter),
    ?match(true, mod_invites:is_token_valid(Server, RosterToken, Inviter)),
    ?match(1, mod_invites:expire_tokens(User, Server)),
    ?match(true, mod_invites:is_token_valid(Server, RosterToken, Inviter)),
    ?match(false, mod_invites:is_token_valid(Server, AccountToken, Inviter)),
    ?match(0, mod_invites:expire_tokens(User, Server)),
    mod_invites:cleanup_expired(),
    mod_invites:remove_user(User, Server),
    disconnect(Config).

max_invites(Config0) ->
    Server = ?config(server, Config0),
    User = ?config(user, Config0),
    Inviter = {User, Server},
    OldOpts = gen_mod:get_module_opts(Server, mod_invites),
    NewOpts =
        gen_mod_set_opts(OldOpts,
                         [{max_invites, 3},
                          {access_create_account, account_invite},
                          {allow_modules, [mod_invites]}]),
    update_module_opts(Server, mod_invites, NewOpts),

    Config = reconnect(Config0),

    #invite_token{} = create_account_invite(Server, Inviter),
    #invite_token{} = create_account_invite(Server, Inviter),
    #invite_token{} = create_account_invite(Server, Inviter),
    ?match({error, num_invites_exceeded}, create_account_invite(Server, Inviter)),
    #invite_token{token = RosterInviteToken} =
        mod_invites:create_roster_invite(Server, Inviter),
    ?match(#iq{type = error}, send_pars(Config, RosterInviteToken)),

    %% we can create more than 3 as an "admin" user
    ?match(0,
           length([error
                   || _ <- lists:seq(1, 4), element(1, mod_invites:gen_invite(Server)) == error])),

    update_module_opts(Server, mod_invites, OldOpts),
    #invite_token{} = create_account_invite(Server, Inviter),
    mod_invites:remove_user(User, Server),
    mod_invites:expire_tokens(<<>>, Server),
    ?match(4, mod_invites:cleanup_expired()),
    disconnect(Config).

presence_with_preauth_token(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    Inviter = {<<"inviter">>, Server},
    InviterJID = jid:make(<<"inviter">>, Server),
    #invite_token{token = RosterToken} = mod_invites:create_roster_invite(Server, Inviter),
    send(Config,
         #presence{type = subscribe,
                   to = InviterJID,
                   sub_els = [#preauth{token = RosterToken}]}),
    _ = ?recv2(#iq{type = set,
                   sub_els = [#roster_query{items = [#roster_item{ask = subscribe}]}]},
               #iq{type = set,
                   sub_els = [#roster_query{items = [#roster_item{subscription = to}]}]}),
    ?match(false, mod_invites:is_token_valid(Server, RosterToken, Inviter)),
    %% cleanup the mess
    mod_roster:del_roster(User, Server, jid:tolower(InviterJID)),
    #iq{type = set} = suite:recv_iq(Config),
    mod_invites:remove_user(<<"inviter">>, Server),
    disconnect(Config).

is_reserved(Config) ->
    Server = ?config(server, Config),
    Inviter = {<<"inviter">>, Server},
    mod_invites:expire_tokens(<<"inviter">>, Server),
    mod_invites:cleanup_expired(),
    #invite_token{token = Token} =
        mod_invites:create_account_invite(Server, Inviter, <<"reserved_user">>, false),
    ?match({error, reserved},
           mod_invites:create_account_invite(Server, Inviter, <<"reserved_user">>, false)),
    ?match(false, mod_invites:is_reserved(Server, Token, <<"some_other_username">>)),
    ?match(false, mod_invites:is_reserved(Server, Token, <<"reserved_user">>)),
    ?match(true,
           mod_invites:is_reserved(Server, <<"some_other_token">>, <<"reserved_user">>)),
    %% "use" token to create account under different name, then it should not be reserved anymore
    mod_invites:set_invitee(Server, Token, jid:make(<<"some_other_username">>, Server)),
    ?match(false,
           mod_invites:is_reserved(Server, <<"some_other_token">>, <<"reserved_user">>)),
    #invite_token{token = OtherToken} =
        mod_invites:create_account_invite(Server, Inviter, <<"reserved_user">>, false),
    ?match(true, OtherToken /= Token),
    mod_invites:remove_user(<<"inviter">>, Server),
    disconnect(Config).

stream_feature(Config0) ->
    Server = ?config(server, Config0),
    OldOpts = gen_mod:get_module_opts(Server, mod_invites),
    Config1 = reconnect(Config0),
    ?match(true, ?config(register, Config1)),
    ?match(false, ?config(register_ibr_token, Config1)),
    NewOpts = gen_mod:set_opt(access_create_account, account_invite, OldOpts),
    update_module_opts(Server, mod_invites, NewOpts),
    Config2 = reconnect(Config1),
    ?match(true, ?config(register, Config2)),
    ?match(true, ?config(register_ibr_token, Config2)),
    update_module_opts(Server, mod_invites, OldOpts),
    disconnect(Config2).

ibr(Config0) ->
    Server = ?config(server, Config0),
    User = ?config(user, Config0),
    AccountName = <<"new_user">>,

    OldRegisterOpts = gen_mod:get_module_opts(Server, mod_register),
    NewRegisterOpts = gen_mod:set_opt(allow_modules, [mod_invites], OldRegisterOpts),
    update_module_opts(Server, mod_register, NewRegisterOpts),

    Config1 = reconnect(Config0),

    ?match(#iq{type = error}, send_iq_register(Config1, AccountName)),

    ?match(#iq{type = error}, send_pars(Config1, <<"bad_token">>)),

    #invite_token{token = RosterToken} =
        mod_invites:create_roster_invite(Server, {<<"inviter">>, Server}),
    ?match(#iq{type = result}, send_pars(Config1, RosterToken)),
    ?match(#iq{type = result}, send_iq_register(Config1, <<"roster_invite_user">>)),
    %% roster tokens should still be valid because they will be used for roster pars later by the
    %% client
    ?match(true, mod_invites:is_token_valid(Server, RosterToken)),

    Config =
        open_session(bind(auth(set_opt(password,
                                       <<"mySecret">>,
                                       set_opt(user, <<"roster_invite_user">>, Config1))))),

    send(Config,
         #presence{type = subscribe,
                   to = jid:make(<<"inviter">>, Server),
                   sub_els = [#preauth{token = RosterToken}]}),
    _ = ?recv2(#iq{type = set,
                   sub_els = [#roster_query{items = [#roster_item{ask = subscribe}]}]},
               #iq{type = set,
                   sub_els = [#roster_query{items = [#roster_item{subscription = to}]}]}),
    ?match(false, mod_invites:is_token_valid(Server, RosterToken)),

    #iq{type = set} = suite:recv_iq(Config),
    Config11 = reconnect(Config),

    #invite_token{token = Token} =
        mod_invites:create_account_invite(Server, {<<>>, Server}, AccountName, false),
    ?match(#iq{type = result}, send_pars(Config11, Token)),
    ?match(#iq{type = result, sub_els = [#register{username = AccountName}]},
           send_get_iq_register(Config11)),
    ?match(#iq{type = result}, send_iq_register(Config11, AccountName)),

    Config2 = reconnect(Config11),
    ?match(#iq{type = error}, send_pars(Config2, Token)),

    #invite_token{token = Token2} =
        mod_invites:create_account_invite(Server,
                                          {<<>>, Server},
                                          <<"some_unfavorable_name">>,
                                          false),
    ?match(#iq{type = result}, send_pars(Config2, Token2)),
    ?match(#iq{type = result, sub_els = [#register{username = <<"some_unfavorable_name">>}]},
           send_get_iq_register(Config2)),
    ?match(#iq{type = error}, send_iq_register(Config2, User)),
    ?match(#iq{type = error}, send_iq_register(Config2, <<"@invalid_user">>)),
    ?match(#iq{type = result}, send_iq_register(Config2, <<"some_much_better_name">>)),
    ?match(#iq{type = error}, send_iq_register(Config2, <<"one_more_try">>)),

    Config3 = reconnect(Config2),
    #invite_token{token = Token3} = create_account_invite(Server, {<<>>, Server}),
    ?match(#iq{type = result}, send_pars(Config3, Token3)),
    ?match(#iq{type = result, sub_els = [#register{username = <<>>}]},
           send_get_iq_register(Config3)),
    ?match(#iq{type = result}, send_iq_register(Config3, <<"some_self_chosen_name">>)),

    ejabberd_auth:remove_user(AccountName, Server),
    ejabberd_auth:remove_user(<<"some_self_chosen_name">>, Server),
    ejabberd_auth:remove_user(<<"some_much_better_name">>, Server),
    update_module_opts(Server, mod_register, OldRegisterOpts),
    mod_invites:remove_user(<<"inviter">>, Server),
    mod_invites:expire_tokens(<<>>, Server),
    ?match(3, mod_invites:cleanup_expired()),
    disconnect(Config3).

ibr_reserved(Config0) ->
    Server = ?config(server, Config0),
    Config1 = reconnect(Config0),
    #invite_token{token = _ReservedToken} =
        mod_invites:create_account_invite(Server, {<<>>, Server}, <<"reserved">>, false),
    #invite_token{token = OtherToken} =
        mod_invites:create_account_invite(Server, {<<>>, Server}, <<"some_other">>, false),
    ?match(#iq{type = result}, send_iq_register(Config1, <<"check_registration_works">>)),
    Config2 = reconnect(Config1),
    ?match(#iq{type = error}, send_iq_register(Config2, <<"reserved">>)),
    ?match(#iq{type = result}, send_pars(Config2, OtherToken)),
    ejabberd_auth:remove_user(<<"check_registration_works">>, Server),
    mod_invites:expire_tokens(<<>>, Server),
    ?match(2, mod_invites:cleanup_expired()),
    disconnect(Config2).

ibr_subscription(Config0) ->
    Server = ?config(server, Config0),
    ServerJID = jid:from_string(Server),
    User = ?config(user, Config0),
    UserJID = jid:make(User, Server),
    NewAccount = <<"new_friend">>,
    NewAccountJID = jid:make(NewAccount, Server),
    gen_mod:stop_module_keep_config(Server, mod_vcard_xupdate),
    OldOpts = gen_mod:get_module_opts(Server, mod_invites),
    NewOpts = gen_mod:set_opt(access_create_account, account_invite, OldOpts),
    update_module_opts(Server, mod_invites, NewOpts),

    self_presence(Config0, available),

    #invite_token{token = Token} =
        mod_invites:create_account_invite(Server, {User, Server}, NewAccount, true),

    Config1 =
        set_opts([{user, NewAccount},
                  {password, <<"mySecret">>},
                  {resource, <<"invite_tests">>},
                  {receiver, undefined}],
                 Config0),
    Config = connect(Config1),

    ?match(#iq{type = result}, send_pars(Config, Token)),
    ?match(#iq{type = result}, send_iq_register(Config, NewAccount)),

    open_session(bind(auth(Config))),

    _ = ?recv3(#iq{type = set,
                   sub_els =
                       [#roster_query{items =
                                          [#roster_item{jid = NewAccountJID,
                                                        subscription = from}]}]},
               #iq{type = set,
                   sub_els =
                       [#roster_query{items =
                                          [#roster_item{jid = NewAccountJID,
                                                        subscription = both}]}]},
               #presence{from = NewAccountJID, type = subscribed}),

    ?match(true,
           [Friend
            || Friend = #roster{jid = {RUser, RServer, <<>>}, subscription = both}
                   <- mod_roster:get_roster(User, Server),
               {RUser, RServer} == {NewAccount, Server}]
           /= []),
    ?match(true,
           [Friend
            || Friend = #roster{jid = {RUser, RServer, <<>>}, subscription = both}
                   <- mod_roster:get_roster(NewAccount, Server),
               {RUser, RServer} == {User, Server}]
           /= []),
    UserFullJID = jid:make(User, Server, ?config(resource, Config0)),
    NewAccountFullJID = jid:make(NewAccount, Server, ?config(resource, Config)),
    send(Config, #presence{}),

    receive_subscription_stanzas(ServerJID, UserFullJID, NewAccountFullJID),

    mod_roster:del_roster(User, Server, jid:tolower(NewAccountJID)),
    mod_roster:del_roster(NewAccount, Server, jid:tolower(UserJID)),

    update_module_opts(Server, mod_invites, OldOpts),
    disconnect(Config0),
    disconnect(Config),
    ejabberd_auth:remove_user(NewAccount, Server),
    mod_invites:remove_user(User, Server),
    ok.

receive_subscription_stanzas(ServerJID, UserFullJID, NewAccountFullJID) ->
    Stanzas = [pres1, pres2, pres3, msg],
    receive_subscription_stanzas(length(Stanzas),
                                 Stanzas,
                                 ServerJID,
                                 UserFullJID,
                                 NewAccountFullJID).

receive_subscription_stanzas(_, {timeout, ElementsLeft}, _, _, _) ->
    {error, {timeout, ElementsLeft}};
receive_subscription_stanzas(0, [], _, _, _) ->
    done;
receive_subscription_stanzas(0, NotEmpty, _, _, _) ->
    {error, {elements_left, NotEmpty}};
receive_subscription_stanzas(Count,
                             Elements,
                             ServerJID,
                             UserFullJID,
                             NewAccountFullJID) ->
    Res = receive
              #presence{from = UserFullJID, to = NewAccountFullJID} ->
                  lists:delete(pres1, Elements);
              #presence{from = NewAccountFullJID, to = UserFullJID} ->
                  lists:delete(pres2, Elements);
              #presence{from = NewAccountFullJID, to = NewAccountFullJID} ->
                  lists:delete(pres3, Elements);
              #message{from = ServerJID} ->
                  lists:delete(msg, Elements)
          after 100 ->
              {timeout, Elements}
          end,
    receive_subscription_stanzas(Count - 1, Res, ServerJID, UserFullJID, NewAccountFullJID).

ibr_conflict(Config0) ->
    Server = ?config(server, Config0),

    OldRegisterOpts = gen_mod:get_module_opts(Server, mod_register),
    NewRegisterOpts = gen_mod:set_opt(allow_modules, [mod_invites], OldRegisterOpts),
    update_module_opts(Server, mod_register, NewRegisterOpts),

    Config1 = reconnect(Config0),

    #invite_token{token = Token} =
        mod_invites:create_account_invite(Server, {<<>>, Server}, <<>>, false),
    ?match(#iq{type = result}, send_pars(Config1, Token)),
    Parent = self(),
    Pid = spawn(fun() ->
                   Config11 = connect(lists:keydelete(receiver, 1, Config1)),
                   #iq{type = result} = send_pars(Config11, Token),
                   #iq{type = Type} = send_iq_register(Config11, <<"ibr_conflict_1">>),
                   Parent ! {self(), Type},
                   disconnect(Config11)
                end),
    receive
        {Pid, Result} ->
            ?match(result, Result)
    after 1000 ->
        ct:fail(timeout)
    end,

    ?match(#iq{type = result}, send_get_iq_register(Config1)),
    ?match(#iq{type = error}, send_iq_register(Config1, <<"ibr_conflict_2">>)),
    ?match(false, ejabberd_auth:user_exists(<<"ibr_conflict_2">>, Server)),
    ?match(false, mod_invites:is_token_valid(Server, Token)),

    Config2 = reconnect(Config1),

    #invite_token{token = RosterToken} =
        mod_invites:create_roster_invite(Server, {<<"inviter">>, Server}),
    ?match(#iq{type = result}, send_pars(Config2, RosterToken)),

    Pid2 =
        spawn(fun() ->
                 Config21 = connect(lists:keydelete(receiver, 1, Config2)),
                 #iq{type = result} = send_pars(Config21, RosterToken),
                 #iq{type = Type} = send_iq_register(Config21, <<"ibr_conflict_3">>),
                 Parent ! {self(), Type},
                 disconnect(Config21)
              end),
    receive
        {Pid2, Result2} ->
            ?match(result, Result2)
    after 1000 ->
        ct:fail(timeout)
    end,

    ?match(#iq{type = result}, send_get_iq_register(Config2)),
    ?match(#iq{type = error}, send_iq_register(Config2, <<"ibr_conflict_4">>)),
    ?match(false, ejabberd_auth:user_exists(<<"ibr_conflict_4">>, Server)),
    ?match(true, mod_invites:is_token_valid(Server, RosterToken)),

    mod_invites:remove_user(<<"inviter">>, Server),
    mod_invites:expire_tokens(<<>>, Server),
    ?match(1, mod_invites:cleanup_expired()),
    disconnect(Config2),
    ok.

http(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    {TokenURI, LandingPage} = mod_invites:gen_invite(Server),
    Token = token_from_uri(TokenURI),
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request(LandingPage),
    {match, RegistrationURLs} =
        re:run(Body,
               <<"href=\"", Token/binary, "([a-zA-Z0-9\/\-]+)\"">>,
               [global, {capture, [1], binary}]),
    Apps =
        mod_invites_http:apps_json(Server, <<"en">>, [{static, <<"/static">>}, {uri, <<>>}]),
    ?match(true, length(RegistrationURLs) == length(Apps) + 1),
    BaseURL = mod_invites_http:landing_page(Server, mod_invites:get_invite(Server, Token)),
    lists:foreach(fun([URL]) ->
                     FullURL = <<BaseURL/binary, "/", URL/binary>>,
                     ct:pal("Checking url ~p", [FullURL]),
                     ?match({ok, {{_, 200, _}, _, _}}, httpc:request(FullURL))
                  end,
                  RegistrationURLs),

    {ok, {{_, 404, _}, _, _}} = httpc:request(<<BaseURL/binary, "/UnkonwnApp">>),
    {ok, {{_, 404, _}, _, _}} = httpc:request(<<BaseURL/binary, "/UnkonwnApp/registration">>),
    {ok, {{_, 404, _}, _, _}} = httpc:request(<<BaseURL/binary, "/Dino/unknownpath">>),

    [Last] = hd(lists:reverse(RegistrationURLs)),
    RegURL = <<BaseURL/binary, Last/binary>>,
    {ok, {{_, 400, _}, _, _}} = post(RegURL, <<"badtoken">>, <<"foo">>, <<"bar">>),
    {ok, {{_, 400, _}, _, _}} = post(RegURL, Token, User, <<"bar">>),
    {ok, {{_, 400, _}, _, _}} = post(RegURL, Token, <<"@invalidUser">>, <<"bar">>),
    {ok, {{_, 200, _}, _, _}} = post(RegURL, Token, <<"foo">>, <<"bar">>),
    {ok, {{_, 404, _}, _, _}} = post(RegURL, Token, <<"foo">>, <<"bar">>),
    {ok, {{_, 404, _}, _, _}} = httpc:request(LandingPage),
    lists:foreach(fun([URL]) ->
                     FullURL = <<BaseURL/binary, "/", URL/binary>>,
                     ct:pal("Checking url ~p", [FullURL]),
                     ?match({ok, {{_, 404, _}, _, _}}, httpc:request(FullURL))
                  end,
                  RegistrationURLs),
    RosterInvite =
        #invite_token{token = RosterToken} =
            mod_invites:create_roster_invite(Server, {<<"inviter">>, Server}),
    RosterURL = mod_invites_http:landing_page(Server, RosterInvite),
    {ok, {{_, 200, _}, _, _}} = httpc:request(RosterURL),
    FakeRegURL = <<RosterURL/binary, "/registration">>,
    {ok, {{_, 404, _}, _, _}} = post(FakeRegURL, RosterToken, <<"baz">>, <<"bar">>),
    ejabberd_auth:remove_user(<<"foo">>, Server),
    mod_invites:remove_user(<<"inviter">>, Server),
    mod_invites:expire_tokens(<<>>, Server),
    ?match(1, mod_invites:cleanup_expired()),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("invites_" ++ atom_to_list(T)).

token_from_uri(Uri) ->
    {match, [Token]} =
        re:run(Uri, ".+preauth=([a-zA-z0-9]+)", [{capture, all_but_first, binary}]),
    Token.

create_account_invite(Server, Inviter) ->
    mod_invites:create_account_invite(Server, Inviter, <<>>, false).

update_module_opts(Host, Module, Opts) ->
    [EjabMod] = ets:lookup(ejabberd_modules, {Module, Host}),
    ets:insert(ejabberd_modules, EjabMod#ejabberd_module{opts = Opts}).

xdata_field(Var, Fields) ->
    xdata_field(Var, Fields, undefined).

xdata_field(_Var, [], Default) ->
    Default;
xdata_field(Var, [#xdata_field{var = Var, values = [<<>> | _]} | _], Default) ->
    Default;
xdata_field(Var, [#xdata_field{var = Var, values = [Result | _]} | _], _Default) ->
    Result;
xdata_field(Var, [_NoMatch | Fields], Default) ->
    xdata_field(Var, Fields, Default).

xdata_field_set(Var, Val, Fields) ->
    xdata_field_set(Var, Val, Fields, []).

xdata_field_set(Var, _Val, [], _Result) ->
    throw({error, {not_found, Var}});
xdata_field_set(Var, Val, [#xdata_field{var = Var} = Field | Fields], Result) ->
    Result ++ [Field#xdata_field{values = [Val]} | Fields];
xdata_field_set(Var, Val, [Field | Tail], Result) ->
    xdata_field_set(Var, Val, Tail, Result ++ [Field]).

test_create_account(Config, Username, Subscription) ->
    Server = ?config(server, Config),
    ServerJID = jid:from_string(Server),
    Command = #adhoc_command{node = ?NS_INVITE_CREATE_ACCOUNT},
    #iq{type = result,
        sub_els =
            [#adhoc_command{status = executing,
                            sid = SID,
                            node = ?NS_INVITE_CREATE_ACCOUNT,
                            actions = #adhoc_actions{execute = complete, complete = true},
                            xdata = #xdata{type = form, fields = XdataFields0}}]} =
        send_recv(Config,
                  #iq{type = set,
                      to = ServerJID,
                      sub_els = [Command]}),
    XdataFields =
        xdata_field_set(<<"username">>,
                        Username,
                        xdata_field_set(<<"roster-subscription">>, Subscription, XdataFields0)),
    #iq{type = result,
        sub_els =
            [#adhoc_command{status = completed,
                            sid = SID,
                            node = ?NS_INVITE_CREATE_ACCOUNT,
                            xdata = #xdata{type = result, fields = ResultXDataFields}}]} =
        send_recv(Config,
                  #iq{type = set,
                      to = ServerJID,
                      sub_els =
                          [Command#adhoc_command{sid = SID,
                                                 xdata =
                                                     #xdata{type = submit,
                                                            fields = XdataFields}}]}),
    ResultXDataFields.

connect(Config) ->
    process_stream_features(init_stream(Config)).

reconnect(Config) ->
    connect(disconnect(Config)).

process_stream_features(Config) ->
    receive
        #stream_features{sub_els = Fs} ->
            ct:pal("stream features: ~p", [Fs]),
            lists:foldl(fun (#feature_register{}, Acc) ->
                                set_opt(register, true, Acc);
                            (#feature_register_ibr_token{}, Acc) ->
                                set_opt(register_ibr_token, true, Acc);
                            (_, Acc) ->
                                Acc
                        end,
                        set_opt(register, false, set_opt(register_ibr_token, false, Config)),
                        Fs)
    end.

send_get_iq_register(Config) ->
    ServerJID = jid:from_string(?config(server, Config)),
    send_recv(Config,
              #iq{type = get,
                  to = ServerJID,
                  sub_els = [#register{}]}).

send_iq_register(Config, AccountName) ->
    ServerJID = jid:from_string(?config(server, Config)),
    send_recv(Config,
              #iq{type = set,
                  to = ServerJID,
                  sub_els = [#register{username = AccountName, password = <<"mySecret">>}]}).

send_pars(Config, Token) ->
    ServerJID = jid:from_string(?config(server, Config)),
    send_recv(Config,
              #iq{type = set,
                  to = ServerJID,
                  sub_els = [#preauth{token = Token}]}).

post(URL, Token, User, Password) ->
    Data = <<"token=", Token/binary, "&user=", User/binary, "&password=", Password/binary>>,
    httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Data}, [], []).

gen_mod_set_opts(OldOpts, NewOpts) ->
    lists:foldl(fun({Opt, Val}, Opts) -> gen_mod:set_opt(Opt, Val, Opts) end,
                OldOpts,
                NewOpts).

re_escape(Str) ->
    re_escape(Str, <<>>).

re_escape(<<>>, Escaped) ->
    ct:pal("escaped: ~p", [Escaped]),
    Escaped;
re_escape(<<C:1/binary, Tail/binary>>, Acc) ->
    case lists:member(C,
                      [<<".">>,
                       <<"*">>,
                       <<"+">>,
                       <<"?">>,
                       <<"^">>,
                       <<"$">>,
                       <<"(">>,
                       <<")">>,
                       <<"[">>,
                       <<"]">>,
                       <<"{">>,
                       <<"}">>,
                       <<"|">>,
                       <<";">>,
                       <<"!">>,
                       <<"`">>,
                       <<"#">>,
                       <<"~">>,
                       <<"!">>,
                       <<"_">>,
                       <<"-">>,
                       <<"=">>,
                       <<"\\">>])
    of
        true ->
            re_escape(Tail, <<Acc/binary, <<"\\", C/binary>>/binary>>);
        false ->
            re_escape(Tail, <<Acc/binary, C/binary>>)
    end.
