%%%-------------------------------------------------------------------
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Created : 16 September 2025 by Stefan Strigler
%%%
%%%
%%% ejabberd, Copyright (C) 2025   ProcessOne
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

-import(suite, [recv_presence/1, send_recv/2, my_jid/1, muc_room_jid/1,
		send/2, recv_message/1, recv_iq/1, muc_jid/1,
		alt_room_jid/1, wait_for_slave/1, wait_for_master/1,
		disconnect/1, put_event/2, get_event/1, peer_muc_jid/1,
		my_muc_jid/1, get_features/2, set_opt/3]).

-include("suite.hrl").
-include("mod_invites.hrl").

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
      single_test(presence_with_preauth_token)]}.

%%%===================================================================

gen_invite(Config) ->
    Server = ?config(server, Config),
    User = ?config(user, Config),
    Res = mod_invites:gen_invite(<<"foo">>, Server),
    ?match(<<"xmpp:", _/binary>>, Res),
    Token = token_from_uri(Res),
    #invite_token{inviter = {<<>>, Server},
                  type = account_only,
                  account_name = <<"foo">>} =
        mod_invites:get_invite(Server, Token),
    Res2 = mod_invites:gen_invite(undefined, Server),
    ?match(<<"xmpp:", _/binary>>, Res),
    Token2 = token_from_uri(Res2),
    #invite_token{inviter = {<<>>, Server},
                  type = account_only,
                  account_name = undefined} =
        mod_invites:get_invite(Server, Token2),
    ?match({error, user_exists}, mod_invites:gen_invite(User, Server)),
    ?match({error, account_name_invalid},
           mod_invites:gen_invite(<<"@bad_acccount_name">>, Server)),
    ?match({error, host_unknown}, mod_invites:gen_invite(<<"foo">>, <<"non.existant.host">>)),
    %% TooLongHostname = list_to_binary([$a || _ <- lists:seq(1, 1024)]),
    %% ?match({error, hostname_invalid}, mod_invites:gen_invite(<<"foo">>, TooLongHostname)),
    ok.

cleanup_expired(Config) ->
    Server = ?config(server, Config),
    mod_invites:create_account_invite(Server, {<<"foo">>, Server}, undefined, false),
    mod_invites:expire_tokens(<<"foo">>, Server),
    Token = token_from_uri(mod_invites:gen_invite(<<"foobar">>, Server)),
    ?match(1, mod_invites:cleanup_expired()),
    ?match(#invite_token{}, mod_invites:get_invite(Server, Token)),
    ?match(0, mod_invites:cleanup_expired()),
    ok.

adhoc_items(Config) ->
    Server = ?config(server, Config),
    ServerJID = jid:from_string(Server),
    User = ?config(user, Config),
    UserJID = jid:from_string(User),
    Disco = #disco_items{node = ?NS_COMMANDS},
    #iq{type = result, sub_els = [#disco_items{node = ?NS_COMMANDS, items = Items}]} =
        send_recv(Config,
                  #iq{type = get,
                      to = ServerJID,
                      sub_els = [Disco]}),
    ?match(true, [I || I = #disco_item{node = ?NS_INVITE_INVITE} <- Items] /= []),
    ?match(deny,
           acl:match_rule(Server,
                          gen_mod:get_module_opt(Server, mod_invites, access_create_account),
                          UserJID)),
    ?match(false, [I || I = #disco_item{node = ?NS_INVITE_CREATE_ACCOUNT} <- Items] /= []),
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
                      sub_els = [Disco]}),
    ?match(true, [I || I = #disco_item{node = ?NS_INVITE_INVITE} <- NewItems] /= []),
    ?match(true, [I || I = #disco_item{node = ?NS_INVITE_CREATE_ACCOUNT} <- NewItems] /= []),
    update_module_opts(Server, mod_invites, OldOpts),
    ?match(deny,
           acl:match_rule(Server,
                          gen_mod:get_module_opt(Server, mod_invites, access_create_account),
                          UserJID)),
    ok.

adhoc_command_invite(Config) ->
    Server = ?config(server, Config),
    ServerJID = jid:from_string(Server),
    Command = #adhoc_command{node = ?NS_INVITE_INVITE},
    #iq{type = result,
        sub_els =
            [#adhoc_command{status = completed,
                            xdata = #xdata{type = result, fields = XdataFields}}]} =
        send_recv(Config,
                  #iq{type = set,
                      to = ServerJID,
                      sub_els = [Command]}),
    [Uri] = [V || #xdata_field{var = <<"uri">>, values = [V]} <- XdataFields],
    ?match(<<"xmpp:", _/binary>>, Uri),
    ?match(true, [V || #xdata_field{var = <<"expire">>, values = [V]} <- XdataFields] /= []),
    Token = token_from_uri(Uri),
    User = jid:nodeprep(?config(user, Config)),
    ?match(true, mod_invites:is_token_valid(Server, Token, {User, Server})),
    mod_invites:remove_user(User, Server),
    ok.

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
    Inviter = jid:nodeprep(?config(user, Config)),
    ?match({match, [Inviter, _]},
           re:run(xdata_field(<<"uri">>, ResultXDataFields3),
                  <<"xmpp:(.+)", "@", Server/binary, "\\?roster;preauth=([a-zA-Z0-9]+);ibr=y">>,
                  [{capture, all_but_first, binary}])),
    Token = token_from_uri(xdata_field(<<"uri">>, ResultXDataFields3, <<>>)),
    #invite_token{account_name = undefined, type = account_subscription} =
        mod_invites:get_invite(Server, Token),
    ResultXDataFields4 = test_create_account(Config, <<"foobar">>, <<"1">>),
    ?match({match, [Inviter, _]},
           re:run(xdata_field(<<"uri">>, ResultXDataFields4),
                  <<"xmpp:(.+)", "@", Server/binary, "\\?roster;preauth=([a-zA-Z0-9]+);ibr=y">>,
                  [{capture, all_but_first, binary}])),
    update_module_opts(Server, mod_invites, OldOpts),
    User = jid:nodeprep(?config(user, Config)),
    mod_invites:remove_user(User, Server),
    ok.

token_valid(Config) ->
    Server = ?config(server, Config),
    User = jid:nodeprep(?config(user, Config)),
    Res = mod_invites:gen_invite(<<"foobar">>, Server),
    Token = token_from_uri(Res),
    ?match(true, mod_invites:is_token_valid(Server, Token)),
    Inviter = {<<"foo">>, Server},
    #invite_token{token = AccountToken} =
        mod_invites:create_account_invite(Server, Inviter, undefined, false),
    ?match(true, mod_invites:is_token_valid(Server, AccountToken, Inviter)),
    ?match(false, mod_invites:is_token_valid(Server, <<"madeUptoken">>)),
    ?match(false,
           mod_invites:is_token_valid(Server, AccountToken, {<<"someoneElse">>, Server})),
    mod_invites:expire_tokens(<<"foo">>, Server),
    ?match(false, mod_invites:is_token_valid(Server, AccountToken, Inviter)),
    mod_invites:cleanup_expired(),
    mod_invites:remove_user(User, Server),
    ok.

remove_user(Config) ->
    Server = ?config(server, Config),
    User = jid:nodeprep(?config(user, Config)),
    Inviter = {User, Server},
    #invite_token{} = mod_invites:create_account_invite(Server, Inviter, undefined, false),
    ?match(1, mod_invites:num_account_invites(User, Server)),
    mod_invites:remove_user(User, Server),
    ?match(0, mod_invites:num_account_invites(User, Server)),
    ok.

expire_tokens(Config) ->
    Server = ?config(server, Config),
    User = jid:nodeprep(?config(user, Config)),
    Inviter = {User, Server},
    #invite_token{token = RosterToken} = mod_invites:create_roster_invite(Server, Inviter),
    #invite_token{token = AccountToken} =
        mod_invites:create_account_invite(Server, Inviter, undefined, false),
    ?match(true, mod_invites:is_token_valid(Server, RosterToken, Inviter)),
    ?match(1, mod_invites:expire_tokens(User, Server)),
    ?match(true, mod_invites:is_token_valid(Server, RosterToken, Inviter)),
    ?match(false, mod_invites:is_token_valid(Server, AccountToken, Inviter)),
    ?match(0, mod_invites:expire_tokens(User, Server)),
    mod_invites:cleanup_expired().

max_invites(Config) ->
    Server = ?config(server, Config),
    User = jid:nodeprep(?config(user, Config)),
    Inviter = {User, Server},
    OldOpts = gen_mod:get_module_opts(Server, mod_invites),
    NewOpts = gen_mod:set_opt(max_invites, 3, OldOpts),
    update_module_opts(Server, mod_invites, NewOpts),
    #invite_token{} = mod_invites:create_account_invite(Server, Inviter, undefined, false),
    #invite_token{} = mod_invites:create_account_invite(Server, Inviter, undefined, false),
    #invite_token{} = mod_invites:create_account_invite(Server, Inviter, undefined, false),
    ?match({error, num_invites_exceeded},
           mod_invites:create_account_invite(Server, Inviter, undefined, false)),
    update_module_opts(Server, mod_invites, OldOpts),
    #invite_token{} = mod_invites:create_account_invite(Server, Inviter, undefined, false),
    ok.

presence_with_preauth_token(Config) ->
    Server = ?config(server, Config),
    Inviter = {<<"inviter">>, Server},
    #invite_token{token = RosterToken} = mod_invites:create_roster_invite(Server, Inviter),
    send(Config,
         #presence{type = 'subscribe',
                   to = jid:make(<<"inviter">>, Server),
                   sub_els = [#preauth{token = RosterToken}]}),
    ?recv2(
        #iq{type = 'set',
            sub_els = [#roster_query{items = [#roster_item{ask = 'subscribe'}]}]},
        #iq{type = 'set',
            sub_els = [#roster_query{items = [#roster_item{subscription = 'to'}]}]}),
    ?match(
       false,
       mod_invites:is_token_valid(Server, RosterToken, Inviter)
      ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("invites_" ++ atom_to_list(T)).

token_from_uri(Uri) ->
    {match, [Token]} =
        re:run(Uri, ".+preauth=([a-zA-z0-9]+)", [{capture, all_but_first, binary}]),
    Token.

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
