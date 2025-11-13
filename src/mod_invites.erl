%%%----------------------------------------------------------------------
%%% File    : mod_invites.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Account and Roster Invitation (aka Great Invitations)
%%% Created : Mon Sep 15 2025 by Stefan Strigler <stefan@strigler.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2025 ProcessOne
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

-xep({xep, 379, ''}).
-xep({xep, 401, '0.5.0'}). % [TODO]
-xep({xep, 445, ''}).

-behaviour(gen_mod).

-export([depends/2, mod_doc/0, mod_options/1, mod_opt_type/1, reload/3, start/2, stop/1]).
-export([adhoc_commands/4, adhoc_items/4, c2s_unauthenticated_packet/2, cleanup_expired/0,
         create_account_allowed/2, expire_tokens/2, gen_invite/1, gen_invite/2, get_invite/2,
         is_reserved/3, is_token_valid/2, list_invites/1, remove_user/2, roster_add/2,
         s2s_receive_packet/1, send_presence/3, set_invitee/3, sm_receive_packet/1,
         stream_feature_register/2, token_uri/1, xdata_field/3]).

-export([process/2]).

-ifdef(TEST).
-export([create_roster_invite/2, create_account_invite/4, is_token_valid/3, num_account_invites/2]).
-endif.

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_commands.hrl").
-include("mod_invites.hrl").
-include("translate.hrl").

-type invite_token() :: #invite_token{}.

-callback cleanup_expired(Host :: binary()) -> non_neg_integer().
-callback create_invite(Invitee :: binary()) -> invite_token().
-callback expire_tokens(User :: binary(), Server :: binary()) -> non_neg_integer().
-callback get_invite(Host :: binary(), Token :: binary()) ->
                        invite_token() | {error, not_found}.
-callback init(Host :: binary(), gen_mod:opts()) -> any().
-callback is_reserved(Host :: binary(), Token :: binary(), User :: binary()) -> boolean().
-callback is_token_valid(Host :: binary(), binary(), {binary(), binary()}) -> boolean().
-callback list_invites(Host :: binary()) -> [tuple()].
-callback num_account_invites(User :: binary(), Server :: binary()) -> non_neg_integer().
-callback remove_user(User :: binary(), Server :: binary()) -> any().
-callback set_invitee(Host :: binary(), Token :: binary(), Invitee :: binary()) -> ok.

%% @format-begin

%%--------------------------------------------------------------------
%%| gen_mod callbacks

depends(_Host, _Opts) ->
    [{mod_adhoc, soft}, {mod_register, soft}, {mod_roster, soft}].

mod_doc() ->
    #{desc =>
          ?T("Allow User Invitation and Account Creation to create out-of-band "
             "links to onboard others onto the XMPP network and establish "
             "a mutual subscription."),
      opts =>
          [{access_create_account,
            #{value => ?T("AccessRuleName"),
              desc =>
                  ?T("This option specifies who is allowed to send 'create account' "
                     "invites. The default value is 'none', i.e. nobody is able to "
                     "create such invites.")}},
           {db_type,
            #{value => "mnesia | sql",
              desc =>
                  ?T("Same as top-level _`default_db`_ option, but applied to this "
                     "module only.")}},
           {landing_page,
            #{value => "none | binary()",
             desc => ?T("Web address of service handling invite links. This is either a local address handled by `mod_invites` configured as a handler at `ejabberd_http` or an external service like 'easy-xmpp-invitation'. The address must be given as a template pattern with fields from the `invite` that will then get replaced accordingly. Eg.: 'https://{{ host }}:5281/invites/{{ invite.token }}' or as an external service like 'http://{{ host }}:8080/easy-xmpp-invites/#{{ invite.uri|strip_protocol }}'. This is the landing page that is being communicated when creating an invite using one of the ad-hoc commands.")}
           },
           {max_invites,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Maximum number of 'create account' invites that can be created "
                     "by an individual user. Users that match the 'admin' acl are "
                     "exempt from this limitation.")}},
           {site_name,
            #{value => ?T("Site Name"),
             desc => ?T("A human readable name for your site. E.g. 'My Beautiful Laundrette'")}
           },
           {templates_dir,
            #{value => ?T("binary()"),
              desc => ?T("The directory containing templates and static files used for landing page and web registration form. Only needs to be set if you want to ship your own set of templates or list of recommended apps.")}},
           {token_expire_seconds,
            #{value => "pos_integer()",
              desc => ?T("Number of seconds until token expires (e.g.: '7 * 86400')")}}],
      example =>
          [{?T("To allow only admin users to send such invites, you would have "
               "a config like this:"),
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
             "    access_create_account: register"]},
           {?T("If you want all your users to be able to send 'create account' "
               "invites, you would configure your server like this instead:"),
            ["acl:",
             "  local:",
             "    user_regexp: \"\"",
             "access_rules:",
             "  create_account_invite:",
             "    allow: local",
             "",
             "modules:",
             "  mod_invites:",
             "    access_create_account: create_account_invite"]},
           ?T("Note that the names of the access rules are just examples and "
              "you're free to change them.")
           %% TODO add example for invite page
          ]}.

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
    econf:either(none, econf:binary("^http[s]?://"));
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
                        args = [{username, binary}, {host, binary}],
                        result_example = 42,
                        result = {num_deleted, integer}},
     #ejabberd_commands{name = generate_invite,
                        tags = [accounts],
                        desc = "Create a new 'create account' invite",
                        module = ?MODULE,
                        function = gen_invite,
                        args = [{host, binary}],
                        args_desc = ["Hostname to generate 'create account' invite for."],
                        args_example = [<<"example.com">>],
                        result_example = <<"xmpp:example.com?register;preauth=CJAi3TvpzuBJpmuf">>,
                        result = {invite, {tuple, [{invite_uri, string}, {landing_page, string}]}}},
     #ejabberd_commands{name = generate_invite_with_username,
                        tags = [accounts],
                        desc =
                            "Create a new 'create account' invite token with a preselected "
                            "username",
                        module = ?MODULE,
                        function = gen_invite,
                        args = [{username, binary}, {host, binary}],
                        args_desc =
                            ["Preselected Username",
                             "hostname  to generate 'create account' invite for."],
                        args_example = [<<"juliet">>, <<"example.com">>],
                        result_example =
                            <<"xmpp:juliet@example.com?register;preauth=CJAi3TvpzuBJpmuf">>,
                        result = {invite, {tuple, [{invite_uri, string}, {landing_page, string}]}}},
     #ejabberd_commands{name = list_invites,
                        tags = [accounts],
                        desc = "List invite tokens",
                        module = ?MODULE,
                        function = list_invites,
                        args = [{host, binary}],
                        args_desc = ["Hostname tokens are valid for"],
                        args_example = [<<"example.com">>],
                        %result_example = [{invite_token, invite}],
                        result =
                            {invites,
                            {list, {invite, {tuple, [{token, string},
                                                     {valid, atom},
                                                     {created_at, string},
                                                     {expires, string},
                                                     {type, atom},
                                                     {inviter, string},
                                                     {invitee, string},
                                                     {account_name, string},
                                                     {token_uri, string},
                                                     {landing_page, string}
                                                    ]}}}}
                       }].

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
gen_invite(Username, Host0) ->
    Host = jid:nameprep(Host0),
    case create_account_invite(Host, {<<>>, Host}, Username, false) of
        {error, {module_not_loaded, ?MODULE, Host}} ->
            {error, host_unknown};
        {error, _Reason} = Error ->
            Error;
        Invite ->
            {token_uri(Invite), landing_page(Host, Invite)}
    end.

list_invites(Host) ->
    Invites = db_call(Host, list_invites, [Host]),
    Format = fun(#invite_token{token = TO, inviter = {IU, IS}, invitee = IE, created_at = CA, expires = Exp, type = TY, account_name = AN} = Invite) ->
                     {TO,
                      is_token_valid(Host, TO),
                      encode_datetime(CA),
                      encode_datetime(Exp),
                      TY,
                      jid:encode(jid:make(IU, IS)),
                      IE,
                      AN,
                      token_uri(Invite),
                      landing_page(Host, Invite)
                     }
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
                    name = translate:translate(Lang, "Invite User")},
    CreateAccount =
        #disco_item{jid = jid:make(Server),
                    node = ?NS_INVITE_CREATE_ACCOUNT,
                    name = translate:translate(Lang, "Create Account")},
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
                   maybe_add_landing_url(
                     LServer,
                     Invite,
                     Lang,
                     [#xdata_field{var = <<"uri">>,
                                   label = trans(Lang, <<"Invite URI">>),
                                   type = 'text-single',
                                   values = [token_uri(Invite)]},
                      #xdata_field{var = <<"expire">>,
                                   label = trans(Lang, <<"Invite token valid until">>),
                                   type = 'text-single',
                                   values = [encode_datetime(Invite#invite_token.expires)]}])},
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
                         maybe_add_landing_url(
                           LServer,
                           Invite,
                           Lang,
                           [#xdata_field{var = <<"uri">>,
                                         label = trans(Lang, <<"Invite URI">>),
                                         type = 'text-single',
                                         values = [token_uri(Invite)]},
                            #xdata_field{var = <<"expire">>,
                                         label = trans(Lang, <<"Invite token valid until">>),
                                         type = 'text-single',
                                         values = [encode_datetime(Invite#invite_token.expires)]}]),
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

is_reserved(Host, Token, User) ->
    db_call(Host, is_reserved, [Host, Token, User]).

-spec is_token_valid(binary(), binary()) -> boolean().
is_token_valid(Host, Token) ->
    is_token_valid(Host, Token, {<<>>, Host}).

-spec is_token_valid(binary(), binary(), {binary(), binary()}) -> boolean().
is_token_valid(Host, Token, Inviter) ->
    db_call(Host, is_token_valid, [Host, Token, Inviter]).

set_invitee(Host, Token, InviteeJid) ->
    %% This invalidates the invite token
    db_call(Host,
            set_invitee,
            [Host,
             Token,
             jid:encode(
                 jid:remove_resource(InviteeJid))]).

create_roster_invite(Host, Inviter) ->
    create_roster_invite(Host, Inviter, <<>>).

create_roster_invite(Host, Inviter, AccountName) ->
    create_invite(roster_only, Host, Inviter, AccountName).

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
                    AccountName
            end
    end.

num_account_invites(User, Server) ->
    db_call(Server, num_account_invites, [User, Server]).

check_max_invites(roster_only, _, _) ->
    ok;
check_max_invites(_Type, Host, {User, Server}) ->
    case {mod_invites_opt:max_invites(Host),
          acl:match_acl(Host, {acl, admin}, #{usr => {User, Server, <<>>}})}
    of
        {infinity, _} ->
            ok;
        {_, true} ->
            ok;
        {MaxInvites, false} ->
            case num_account_invites(User, Server) of
                NumInvites when MaxInvites =< NumInvites ->
                    {error, num_invites_exceeded};
                _AllGood ->
                    ok
            end
    end.

maybe_throw({error, _} = Error) ->
    throw(Error);
maybe_throw(Good) ->
    Good.

invite_token(Type, Host, Inviter, AccountName0) ->
    maybe_throw(check_max_invites(Type, Host, Inviter)),
    Token = p1_rand:get_alphanum_string(?INVITE_TOKEN_LENGTH_DEFAULT),
    AccountName =
        maybe_throw(check_account_name(jid:nodeprep(AccountName0), Host)),
    set_token_expires(#invite_token{token = Token,
                                    inviter = Inviter,
                                    type = Type,
                                    account_name = AccountName},
                      mod_invites_opt:token_expire_seconds(Host)).

token_uri(#invite_token{type = Type,
                        token = Token,
                        account_name = AccountName,
                        inviter = {_User, Host}})
  when Type =:= account_only;
       Type =:= account_subscription ->
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
    Inviter = jid:to_string(jid:make(User, Host)),
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

db_call(Host, Fun, Args) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    apply(Mod, Fun, Args).

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
                          type = 'text-single'
                         } | XData]
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

reason_to_text(host_unknown) ->
    "Host unknown";
reason_to_text(hostname_invalid) ->
    "Hostname invalid";
reason_to_text(account_name_invalid) ->
    "Username invalid";
reason_to_text(user_exists) ->
    "User already exists";
reason_to_text(num_invites_exceeded) ->
    "Maximum number of invites reached".

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
    Presence = #presence{from = From,
                         to = To,
                         type = Type},
    ejabberd_router:route(From, To, Presence).
