%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_accounts.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(mod_admin_extra_accounts).
-author('badlop@process-one.net').

-export([
    commands/0,

    %% Accounts
    set_password/3,
    check_password_hash/4,
    delete_old_users/1,
    delete_old_users_vhost/2,
    ban_account/3,
    num_active_users/2
    ]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").

%%%
%%% Register commands
%%%

commands() ->
    [
        #ejabberd_commands{name = change_password, tags = [accounts],
                           desc = "Change the password of an account",
                           module = ?MODULE, function = set_password,
                           args = [{user, binary}, {host, binary}, {newpass, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = check_password_hash, tags = [accounts],
                           desc = "Check if the password hash is correct",
                           longdesc = "Allowed hash methods: md5, sha.",
                           module = ?MODULE, function = check_password_hash,
                           args = [{user, binary}, {host, binary}, {passwordhash, string}, {hashmethod, string}],
                           result = {res, rescode}},
        #ejabberd_commands{name = delete_old_users, tags = [accounts, purge],
                           desc = "Delete users that didn't log in last days, or that never logged",
                           module = ?MODULE, function = delete_old_users,
                           args = [{days, integer}],
                           result = {res, restuple}},
        #ejabberd_commands{name = delete_old_users_vhost, tags = [accounts, purge],
                           desc = "Delete users that didn't log in last days in vhost, or that never logged",
                           module = ?MODULE, function = delete_old_users_vhost,
                           args = [{host, binary}, {days, integer}],
                           result = {res, restuple}},
        #ejabberd_commands{name = ban_account, tags = [accounts],
                           desc = "Ban an account: kick sessions and set random password",
                           module = ?MODULE, function = ban_account,
                           args = [{user, binary}, {host, binary}, {reason, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = num_active_users, tags = [accounts, stats],
                           desc = "Get number of users active in the last days",
                           module = ?MODULE, function = num_active_users,
                           args = [{host, binary}, {days, integer}],
                           result = {users, integer}},
        #ejabberd_commands{name = check_account, tags = [accounts],
                           desc = "Check if an account exists or not",
                           module = ejabberd_auth, function = is_user_exists,
                           args = [{user, binary}, {host, binary}],
                           result = {res, rescode}},
        #ejabberd_commands{name = check_password, tags = [accounts],
                           desc = "Check if a password is correct",
                           module = ejabberd_auth, function = check_password,
                           args = [{user, binary}, {host, binary}, {password, binary}],
                           result = {res, rescode}}
        ].

%%%
%%% Accounts
%%%

set_password(User, Host, Password) ->
    case ejabberd_auth:set_password(User, Host, Password) of
        ok -> ok;
        _ ->  error
    end.

check_password_hash(User, Host, PasswordHash, HashMethod) ->
    AccountPass = ejabberd_auth:get_password_s(User, Host),
    AccountPassHash = case HashMethod of
        "md5" -> get_md5(AccountPass);
        "sha" -> get_sha(AccountPass);
        _ -> undefined
    end,
    case AccountPassHash of
        undefined -> error;
        PasswordHash -> ok;
        _ -> error
    end.
get_md5(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:md5(AccountPass))]).
get_sha(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:sha(AccountPass))]).

num_active_users(Host, Days) ->
    Mod = mod_admin_extra_last:get_lastactivity_module(Host),
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    TS = TimeStamp - Days * 86400,
    case catch Mod:select(Host, TS, '>') of
        {'EXIT', _Reason} ->
            0;
        Vals0 ->
            length(Vals0)           
    end.

delete_old_users(Days) ->
    %% Get the list of registered users
    Users = ejabberd_auth:dirty_get_registered_users(),

    {removed, N, UR} = delete_old_users(Days, Users),
    {ok, io_lib:format("Deleted ~p users: ~p", [N, UR])}.

delete_old_users_vhost(Host, Days) ->
    %% Get the list of registered users
    Users = ejabberd_auth:get_vh_registered_users(Host),

    {removed, N, UR} = delete_old_users(Days, Users),
    {ok, io_lib:format("Deleted ~p users: ~p", [N, UR])}.

delete_old_users(Days, Users) ->
    %% Convert older time
    SecOlder = Days*24*60*60,

    %% Get current time
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp_now = MegaSecs * 1000000 + Secs,

    %% For a user, remove if required and answer true
    F = fun({LUser, LServer}) ->
            %% Check if the user is logged
            case ejabberd_sm:get_user_resources(LUser, LServer) of
                %% If it isnt
                [] ->
                    %% Look for his last_activity
                    case (get_lastactivity_module(LServer)):get_last_info(LUser, LServer) of
                        %% If it is
                        %% existent:
                        {ok, TimeStamp, _Status} ->
                            %% get his age
                            Sec = TimeStamp_now - TimeStamp,
                            %% If he is
                            if
                                %% younger than SecOlder:
                                Sec < SecOlder ->
                                    %% do nothing
                                    false;
                                %% older:
                                true ->
                                    %% remove the user
                                    ejabberd_auth:remove_user(LUser, LServer),
                                    true
                            end;
                        %% nonexistent:
                        not_found ->
                            %% remove the user
                            ejabberd_auth:remove_user(LUser, LServer),
                            true
                    end;
                %% Else
                _ ->
                    %% do nothing
                    false
            end
    end,
    %% Apply the function to every user in the list
    Users_removed = lists:filter(F, Users),
    {removed, length(Users_removed), Users_removed}.

get_lastactivity_module(Server) ->
    case lists:member(mod_last, gen_mod:loaded_modules(Server)) of
        true -> mod_last;
        _ -> mod_last_odbc
    end.

ban_account(User, Host, ReasonText) ->
    Reason = mod_admin_extra_sessions:prepare_reason(ReasonText),
    kick_sessions(User, Host, Reason),
    set_random_password(User, Host, Reason),
    ok.

kick_sessions(User, Server, Reason) ->
    lists:map(
        fun(Resource) ->
                mod_admin_extra_sessions:kick_this_session(User, Server, Resource, Reason)
        end,
        ejabberd_sm:get_user_resources(User, Server)).

set_random_password(User, Server, Reason) ->
    NewPass = build_random_password(Reason),
    set_password_auth(User, Server, NewPass).

build_random_password(Reason) ->
    Date = list_to_binary(jlib:timestamp_to_iso(calendar:universal_time())),
    RandomString = list_to_binary(randoms:get_string()),
    <<"BANNED_ACCOUNT--", Date/binary, "--", RandomString/binary, "--", Reason/binary>>.

set_password_auth(User, Server, Password) ->
    ok = ejabberd_auth:set_password(User, Server, Password).

