%%%----------------------------------------------------------------------
%%% File    : mod_invites_register.erl
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
-module(mod_invites_register).

-author('stefan@strigler.de').

-export([c2s_unauthenticated_packet/2, stream_feature_register/2]).
-export([try_register/6]).

-import(mod_invites, [roster_add/2, send_presence/3, xdata_field/3]).
-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_commands.hrl").
-include("mod_invites.hrl").
-include("translate.hrl").

%% @format-begin

-define(TRY_SUBTAG(IQ, SUBTAG, F, Else),
        try xmpp:try_subtag(IQ, SUBTAG) of
            false ->
                Else();
            SubTag ->
                F(SubTag)
        catch
            _:{xmpp_codec, Why} ->
                Txt = xmpp:io_format_error(Why),
                Lang = maps:get(lang, State),
                Err = make_stripped_error(IQ, SUBTAG, xmpp:err_bad_request(Txt, Lang)),
                {stop, ejabberd_c2s:send(State, Err)}
        end).
-define(TRY_SUBTAG(IQ, SUBTAG, F), ?TRY_SUBTAG(IQ, SUBTAG, F, fun() -> State end)).

-spec stream_feature_register([xmpp_element()], binary()) -> [xmpp_element()].
stream_feature_register(Acc, Host) ->
    case mod_invites_opt:access_create_account(Host) of
        none ->
            Acc;
        _ ->
            [#feature_register_ibr_token{} | Acc]
    end.

c2s_unauthenticated_packet(#{invite := Invite} = State,
                           #iq{type = get, sub_els = [_]} = IQ) ->
    %% User requests registration form after processing token
    ?TRY_SUBTAG(IQ,
                #register{},
                fun(Register) ->
                   #{server := Server} = State,
                   IQ1 = xmpp:set_els(IQ, [Register]),
                   User = Invite#invite_token.account_name,
                   IQ2 = xmpp:set_from_to(IQ1, jid:make(User, Server), jid:make(Server)),
                   ResIQ = mod_register:process_iq(IQ2),
                   ResIQ1 = xmpp:set_from_to(ResIQ, jid:make(Server), undefined),
                   {stop, ejabberd_c2s:send(State, ResIQ1)}
                end);
c2s_unauthenticated_packet(#{invite := Invite, server := Server} = State,
                           #iq{type = set,
                               sub_els = [_],
                               lang = Lang} =
                               IQ) ->
    %% Process registration request after processing token
    ?TRY_SUBTAG(IQ,
                #register{},
                fun(Register) ->
                   case check_captcha(mod_register_opt:captcha_protected(Server), Register, IQ) of
                       {ok, {Username, Password}} ->
                           #{ip := IP} = State,
                           {Address, _} = IP,
                           case try_register(Invite, Username, Server, Password, Address, Lang) of
                               {ok, UpdatedInvite} ->
                                   ResState = State#{invite => UpdatedInvite},
                                   {stop, ejabberd_c2s:send(ResState, xmpp:make_iq_result(IQ))};
                               {error, #stanza_error{} = Err} ->
                                   ResIQ = make_stripped_error(IQ, #register{}, Err),
                                   {stop, ejabberd_c2s:send(State, ResIQ)}
                           end;
                       {error, ResIQ} ->
                           {stop, ejabberd_c2s:send(State, ResIQ)}
                   end
                end);
c2s_unauthenticated_packet(State, #iq{type = set, sub_els = [_]} = IQ) ->
    %% Check for preauth token and process it
    ?TRY_SUBTAG(IQ,
                #preauth{},
                fun(#preauth{token = Token}) ->
                   #{server := Server} = State,
                   IQ1 = xmpp:set_from_to(IQ, jid:make(<<>>), jid:make(Server)),
                   {ResState, ResIQ} = process_token(State, Token, IQ1),
                   ResIQ1 = xmpp:set_from_to(ResIQ, jid:make(Server), undefined),
                   {stop, ejabberd_c2s:send(ResState, ResIQ1)}
                end,
                fun() ->
                   ?TRY_SUBTAG(IQ,
                               #register{},
                               fun (#register{username = User, password = Password})
                                       when is_binary(User), is_binary(Password) ->
                                       #{server := Server} = State,
                                       case mod_invites:is_reserved(Server, <<>>, User) of
                                           true ->
                                               ResIQ =
                                                   make_stripped_error(IQ,
                                                                       #register{},
                                                                       xmpp:err_not_allowed()),
                                               {stop, ejabberd_c2s:send(State, ResIQ)};
                                           false ->
                                               State
                                       end;
                                   (_) ->
                                       State
                               end)
                end);
c2s_unauthenticated_packet(State, _) ->
    State.

make_stripped_error(IQ, SubTag, Err) ->
    xmpp:make_error(
        xmpp:remove_subtag(IQ, SubTag), Err).

maybe_create_mutual_subscription(#invite_token{inviter = {User, _Server}, type = Type})
    when User == <<>>; % server token
         Type /= account_subscription ->
    noop;
maybe_create_mutual_subscription(#invite_token{inviter = {User, Server},
                                               invitee = Invitee}) ->
    InviterJID = jid:make(User, Server),
    InviteeJID = jid:decode(Invitee),
    roster_add(InviterJID, InviteeJID),
    roster_add(InviteeJID, InviterJID),
    send_presence(InviteeJID, InviterJID, subscribe),
    send_presence(InviterJID, InviteeJID, subscribed),
    send_presence(InviterJID, InviteeJID, subscribe),
    send_presence(InviteeJID, InviterJID, subscribed),
    ok.

process_token(#{server := Host} = State, Token, #iq{lang = Lang} = IQ) ->
    ?DEBUG("processing token (~s): ~s", [Host, Token]),
    case can_create(Host, Token) of
        {true, Invite} ->
            NewState = State#{invite => Invite},
            {NewState, xmpp:make_iq_result(IQ)};
        false ->
            {State, preauth_invalid(IQ, Lang)}
    end.

can_create(Host, Token) ->
    try mod_invites:is_token_valid(Host, Token) of
        true ->
            case mod_invites:get_invite(Host, Token) of
                #invite_token{type = roster_only, account_name = AccountName}
                    when AccountName /= <<>> ->
                    false;
                Invite ->
                    case create_account_allowed(Invite) of
                        ok ->
                            {true, Invite};
                        {error, not_allowed} ->
                            false
                    end
            end;
        false ->
            false
    catch
        _:not_found ->
            false
    end.

create_account_allowed(#invite_token{type = roster_only} = Invite) ->
    #invite_token{inviter = {User, Host}} = Invite,
    case mod_invites:is_create_allowed(User, Host) of
        true ->
            ok;
        false ->
            {error, not_allowed}
    end;
create_account_allowed(#invite_token{inviter = {<<>>, _Host}}) ->
    ok;
create_account_allowed(#invite_token{inviter = {User, Host}}) ->
    mod_invites:create_account_allowed(Host, jid:make(User, Host)).

preauth_invalid(IQ, Lang) ->
    Text = ?T("The token provided is either invalid or expired."),
    make_stripped_error(IQ, #preauth{}, xmpp:err_item_not_found(Text, Lang)).

try_register(Invite, User, Server, Password, Source, Lang) ->
    #invite_token{token = Token} = Invite,
    case {jid:nodeprep(User), not mod_invites:is_reserved(Server, Token, User)} of
        {error, _} ->
            {error,
             xmpp:err_jid_malformed(
                 mod_register:format_error(invalid_jid), Lang)};
        {_, false} ->
            {error,
             xmpp:err_not_allowed(
                 mod_register:format_error(not_allowed), Lang)};
        {_, true} ->
            RegF =
                fun() ->
                   mod_register:try_register(User, Server, Password, Source, mod_invites, Lang)
                end,
            NewInvite =
                #invite_token{invitee = Invitee, account_name = AccountName} =
                    maybe_set_account_name(maybe_set_invitee(Invite, jid:make(User, Server)), User),
            case mod_invites:set_invitee(RegF, Server, Token, Invitee, AccountName) of
                ok ->
                    maybe_create_mutual_subscription(NewInvite),
                    {ok, NewInvite};
                {error, conflict} ->
                    ?LOG_WARNING("Conflict when redeeming invite token: ~p", [NewInvite]),
                    {error,
                     xmpp:err_conflict(
                         mod_register:format_error(not_allowed), Lang)};
                {error, _Reason} = Error ->
                    Error
            end
    end.

check_captcha(true, #register{xdata = X}, #iq{lang = Lang} = IQ) ->
    XdataC =
        xmpp_util:set_xdata_field(#xdata_field{var = <<"FORM_TYPE">>,
                                               type = hidden,
                                               values = [?NS_CAPTCHA]},
                                  X),
    case ejabberd_captcha:process_reply(XdataC) of
        ok ->
            case process_xdata_submit(X) of
                {ok, _} = Result ->
                    Result;
                _ ->
                    Txt = ?T("Incorrect data form"),
                    make_stripped_error(IQ, #register{}, xmpp:err_bad_request(Txt, Lang))
            end;
        {error, malformed} ->
            Txt = ?T("Incorrect CAPTCHA submit"),
            make_stripped_error(IQ, #register{}, xmpp:err_bad_request(Txt, Lang));
        _ ->
            ErrText = ?T("The CAPTCHA verification has failed"),
            make_stripped_error(IQ, #register{}, xmpp:err_not_allowed(ErrText, Lang))
    end;
check_captcha(false, #register{username = Username, password = Password}, _IQ)
    when is_binary(Username), is_binary(Password) ->
    {ok, {Username, Password}};
check_captcha(_IsCaptchaEnabled, _Register, IQ) ->
    ResIQ = make_stripped_error(IQ, #register{}, xmpp:err_bad_request()),
    {error, ResIQ}.

process_xdata_submit(#xdata{fields = Fields}) ->
    case {mod_invites:xdata_field(<<"username">>, Fields, undefined),
          mod_invites:xdata_field(<<"password">>, Fields, undefined)}
    of
        {UndefU, UndefP} when UndefU == undefined; UndefP == undefined ->
            error;
        {Username, Password} ->
            {ok, {Username, Password}}
    end.

maybe_set_invitee(#invite_token{type = roster_only} = Invite, _Invitee) ->
    Invite;
maybe_set_invitee(Invite, Invitee) ->
    Invite#invite_token{invitee = jid:encode(Invitee)}.

maybe_set_account_name(#invite_token{type = roster_only} = Invite, AccountName) ->
    Invite#invite_token{account_name = AccountName};
maybe_set_account_name(Invite, _AccountName) ->
    Invite.
