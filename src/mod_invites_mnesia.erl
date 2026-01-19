%%%----------------------------------------------------------------------
%%% File    : mod_invites_mnesia.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
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
-module(mod_invites_mnesia).

-author('stefan@strigler.de').

-behaviour(mod_invites).

-export([cleanup_expired/1, create_invite/1, expire_tokens/2, get_invite/2, get_invites/2, init/2,
         is_reserved/3, is_token_valid/3, list_invites/1, remove_user/2,
         set_invitee/5]).

-include("mod_invites.hrl").

%% @format-begin

%%--------------------------------------------------------------------
%%| mod_invite callbacks

cleanup_expired(_Host) ->
    lists:foldl(fun(Token, Count) ->
                   [Invite] = mnesia:dirty_read(invite_token, Token),
                   case mod_invites:is_expired(Invite) of
                       true ->
                           ok = mnesia:dirty_delete(invite_token, Token),
                           Count + 1;
                       false ->
                           Count
                   end
                end,
                0,
                mnesia:dirty_all_keys(invite_token)).

create_invite(Invite) ->
    ok = mnesia:dirty_write(Invite),
    Invite.

expire_tokens(User, Server) ->
    length([mnesia:dirty_write(I#invite_token{expires = {{1970, 1, 1}, {0, 0, 1}}})
            || I <- mnesia:dirty_index_read(invite_token, {User, Server}, #invite_token.inviter),
               not mod_invites:is_expired(I),
               I#invite_token.type /= roster_only]).

get_invite(_Host, Token) ->
    case mnesia:dirty_read(invite_token, Token) of
        [Invite] ->
            Invite;
        [] ->
            {error, not_found}
    end.

get_invites(_Host, Inviter) ->
    mnesia:dirty_index_read(invite_token, Inviter, #invite_token.inviter).

init(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE,
                           invite_token,
                           [{disc_copies, [node()]},
                            {attributes, record_info(fields, invite_token)},
                            {index, [inviter]}]).

is_reserved(_Host, Token, User) ->
    [T
     || T <- mnesia:dirty_all_keys(invite_token),
        not mod_invites:is_expired(I = hd(mnesia:dirty_read(invite_token, T))),
        I#invite_token.token /= Token,
        I#invite_token.invitee == <<>>,
        I#invite_token.account_name == User]
    =/= [].

is_token_valid(Host, Token, Scope) ->
    case mnesia:dirty_read(invite_token, Token) of
        [Invite = #invite_token{invitee = <<>>, inviter = {_, Host} = Inviter}]
            when Scope == Inviter; Scope == {<<>>, Host} ->
            not mod_invites:is_expired(Invite);
        [#invite_token{}] ->
            false;
        [] ->
            throw(not_found)
    end.

list_invites(Host) ->
    [Invite
     || Token <- mnesia:dirty_all_keys(invite_token),
        element(2, (Invite = hd(mnesia:dirty_read(invite_token, Token)))#invite_token.inviter)
        == Host].

remove_user(User, Server) ->
    Inviter = {User, Server},
    [ok = mnesia:dirty_delete(invite_token, Token)
     || #invite_token{token = Token}
            <- mnesia:dirty_index_read(invite_token, Inviter, #invite_token.inviter)],
    ok.

-spec set_invitee(fun(() -> OkOrError), binary(), binary(), binary(), binary()) ->
                     OkOrError | {error, conflict}
    when OkOrError :: ok | {error, term()}.
set_invitee(F, _Host, Token, Invitee, AccountName) ->
    Transaction =
        fun() ->
           case hd(mnesia:read(invite_token, Token)) of
               #invite_token{type = Type,
                             invitee = OInvitee,
                             account_name = OAccountName}
                   when OInvitee =/= <<>>
                        orelse Type == roster_only
                               andalso OAccountName =/= <<>>
                               andalso AccountName =/= <<>> ->
                   {error, conflict};
               Invite ->
                   case F() of
                       ok ->
                           ok =
                               mnesia:write(Invite#invite_token{invitee = Invitee,
                                                                account_name = AccountName});
                       {error, _Res} = Error ->
                           Error
                   end
           end
        end,
    {atomic, Res} = mnesia:transaction(Transaction),
    Res.
