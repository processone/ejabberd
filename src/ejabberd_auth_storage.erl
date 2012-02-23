%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_storage.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>, Stephan Maka
%%% Purpose : Authentification via gen_storage
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%%----------------------------------------------------------------------

%%% Database schema (version / storage / table)
%%%
%%% 2.1.x / mnesia / passwd
%%%  us = {Username::string(), Host::string()}
%%%  password = string()
%%%
%%% 2.1.x / odbc / users
%%%  username = varchar250
%%%  password = text
%%%
%%% 3.0.0-prealpha / mnesia / passwd
%%%  Same as 2.1.x
%%%
%%% 3.0.0-prealpha / odbc / users
%%%  Same as 2.1.x
%%%
%%% 3.0.0-alpha / mnesia / passwd
%%%  user_host = {Username::string(), Host::string()}
%%%  password = string()
%%%
%%% 3.0.0-beta / mnesia / passwd
%%%  user_host = {Username::string(), Host::string()}
%%%  password = string()
%%%  storedkey = base64 binary()
%%%  serverkey = base64 binary()
%%%  iterationcount = integer()
%%%  salt = base64 binary()
%%%
%%% 3.0.0-alpha / odbc / passwd
%%%  user = varchar150
%%%  host = varchar150
%%%  password = text
%%%
%%% 3.0.0-beta / odbc / passwd
%%%  user = varchar150
%%%  host = varchar150
%%%  password = base64 text
%%%  storedkey = base64 text
%%%  serverkey = base64 text
%%%  iterationcount = integer
%%%  salt = base64 text

-module(ejabberd_auth_storage).
-author('alexey@process-one.net').

%% External exports
-export([start/1,
	 stop/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 store_type/0,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

-record(passwd, {user_host, password, storedkey, serverkey, salt, iterationcount}).
-record(reg_users_counter, {vhost, count}).

-define(SALT_LENGTH, 16).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec (Host) -> ok
%%     Host = string()

start(Host) ->
    Backend =
	case ejabberd_config:get_local_option({auth_storage, Host}) of
	    undefined -> mnesia;
	    B -> B
	end,
    HostB = list_to_binary(Host),
    gen_storage:create_table(Backend, HostB, passwd,
			     [{odbc_host, Host},
			      {disc_copies, [node()]},
			      {attributes, record_info(fields, passwd)},
			      {types, [{user_host, {text, text}},
			               {storedkey, binary},
			               {serverkey, binary},
			               {salt, binary},
			               {iterationcount, int}]}
			     ]),
    update_table(Host, Backend),
    maybe_scram_passwords(HostB),
    mnesia:create_table(reg_users_counter,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, reg_users_counter)}]),
    update_reg_users_counter_table(Host),
    ok.

stop(_Host) ->
    ok.

update_reg_users_counter_table(Server) ->
    Set = get_vh_registered_users(Server),
    Size = length(Set),
    LServer = exmpp_jid:prep_domain(exmpp_jid:parse(Server)),
    F = fun() ->
        mnesia:write(#reg_users_counter{vhost = LServer,
					count = Size})
    end,
    mnesia:sync_dirty(F).

%% @spec () -> bool()

plain_password_required() ->
    case is_option_scram(?MYNAME) of
	false -> false;
	true -> true
    end.

store_type() ->
    case is_option_scram(?MYNAME) of
	false -> plain; %% allows: PLAIN DIGEST-MD5 SCRAM
	true -> scram %% allows: PLAIN SCRAM
    end.

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()

check_password(User, Server, Password) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    case catch gen_storage:dirty_read(LServer, {passwd, US}) of
	[#passwd{password = ""} = Passwd] ->
	    is_password_scram_valid(Password, Passwd);
	[#passwd{password = Password}] ->
	    Password /= "";
	_ ->
	    false
    end.

%% @spec (User, Server, Password, Digest, DigestGen) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     Digest = string()
%%     DigestGen = function()

check_password(User, Server, Password, Digest, DigestGen) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    case catch gen_storage:dirty_read(LServer, {passwd, US}) of
	[#passwd{password = ""} = Passwd] ->
	    Storedkey = base64:decode(Passwd#passwd.storedkey),
	    DigRes = if
			 Digest /= "" ->
			     Digest == DigestGen(Storedkey);
			 true ->
			     false
		     end,
	    if DigRes ->
		    true;
	       true ->
		    (Storedkey == Password) and (Password /= "")
	    end;
	[#passwd{password = Passwd}] ->
	    DigRes = if
			 Digest /= "" ->
			     Digest == DigestGen(Passwd);
			 true ->
			     false
		     end,
	    if DigRes ->
		    true;
	       true ->
		    (Passwd == Password) and (Password /= "")
	    end;
	_ ->
	    false
    end.

%% @spec (User, Server, Password) -> ok | {error, invalid_jid}
%%     User = string()
%%     Server = string()
%%     Password = string()

set_password(User, Server, Password) ->
    LUser = (catch exmpp_stringprep:nodeprep(User)),
    LServer = (catch exmpp_stringprep:nameprep(Server)),
    case {LUser, LServer} of
	{{stringprep, _, invalid_string, _}, _} ->
	    {error, invalid_jid};
	{_, {stringprep, _, invalid_string, _}} ->
	    {error, invalid_jid};
	US ->
	    %% TODO: why is this a transaction?
	    F = fun() ->
			Passwd = case is_option_scram(LServer) and (Password /= "") of
					true -> password_to_scram(Password, #passwd{user_host=US});
					false -> #passwd{user_host = US, password = Password}
				    end,
			gen_storage:write(LServer, Passwd)
		end,
	    {atomic, ok} = gen_storage:transaction(LServer, passwd, F),
	    ok
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {aborted, Reason}
%%     User = string()
%%     Server = string()
%%     Password = string()

try_register(User, Server, Password) ->
    LUser = (catch exmpp_stringprep:nodeprep(User)),
    LServer = (catch exmpp_stringprep:nameprep(Server)),
    case {LUser, LServer} of
	{{stringprep, _, invalid_string, _}, _} ->
	    {error, invalid_jid};
	{_, {stringprep, _, invalid_string, _}} ->
	    {error, invalid_jid};
	US ->
	    F = fun() ->
			case gen_storage:read(LServer, {passwd, US}) of
			    [] ->
				Passwd = case is_option_scram(LServer) and (Password /= "") of
						true -> password_to_scram(Password, #passwd{user_host=US});
						false -> #passwd{user_host = US, password = Password}
					    end,
				gen_storage:write(LServer, Passwd),
				mnesia:dirty_update_counter(
						    reg_users_counter,
						    exmpp_jid:prep_domain(exmpp_jid:parse(Server)), 1),
				ok;
			    [_E] ->
				exists
			end
		end,
	    %% TODO: transaction return value?
	    gen_storage:transaction(LServer, passwd, F)
    end.

%% @spec () -> [{LUser, LServer}]
%%     LUser = string()
%%     LServer = string()
%% @doc Get all registered users in Mnesia.

dirty_get_registered_users() ->
    lists:foldl(
	fun(HostB, Res) ->
	    get_vh_registered_users(binary_to_list(HostB)) ++ Res
	end,
	[],
	gen_storage:all_table_hosts(passwd)).

%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users(Server) ->
    LServer = exmpp_stringprep:nameprep(Server),
    lists:map(fun(#passwd{user_host = US}) ->
		      US
	      end,
	      gen_storage:dirty_select(LServer, passwd,
				       [{'=', user_host, {'_', LServer}}])).

%% @spec (Server, Opts) -> [{LUser, LServer}]
%%     Server = string()
%%     Opts = [{Opt, Val}]
%%         Opt = atom()
%%         Val = term()
%%     LUser = string()
%%     LServer = string()
%% @doc Return the registered users for the specified host.
%%
%% `Opts' can be one of the following:
%% <ul>
%% <li>`[{from, integer()}, {to, integer()}]'</li>
%% <li>`[{limit, integer()}, {offset, integer()}]'</li>
%% <li>`[{prefix, string()}]'</li>
%% <li>`[{prefix, string()}, {from, integer()}, {to, integer()}]'</li>
%% <li>`[{prefix, string()}, {limit, integer()}, {offset, integer()}]'</li>
%% </ul>

get_vh_registered_users(Server, [{from, Start}, {to, End}]) 
	when is_integer(Start) and is_integer(End) ->
    get_vh_registered_users(Server, [{limit, End-Start+1}, {offset, Start}]);

get_vh_registered_users(Server, [{limit, Limit}, {offset, Offset}]) 
	when is_integer(Limit) and is_integer(Offset) ->
    case get_vh_registered_users(Server) of
    [] ->
	[];
    Users ->
	Set = lists:keysort(1, Users),
	L = length(Set),
	Start = if Offset < 1 -> 1;
	           Offset > L -> L;
	           true -> Offset
	        end,
	lists:sublist(Set, Start, Limit)
    end;

get_vh_registered_users(Server, [{prefix, Prefix}]) 
	when is_list(Prefix) ->
    Set = [{U,S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)],
    lists:keysort(1, Set);

get_vh_registered_users(Server, [{prefix, Prefix}, {from, Start}, {to, End}]) 
	when is_list(Prefix) and is_integer(Start) and is_integer(End) ->
    get_vh_registered_users(Server, [{prefix, Prefix}, {limit, End-Start+1}, {offset, Start}]);

get_vh_registered_users(Server, [{prefix, Prefix}, {limit, Limit}, {offset, Offset}]) 
	when is_list(Prefix) and is_integer(Limit) and is_integer(Offset) ->
    case [{U,S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)] of
    [] ->
	[];
    Users ->
	Set = lists:keysort(1, Users),
	L = length(Set),
	Start = if Offset < 1 -> 1;
	           Offset > L -> L;
	           true -> Offset
	        end,
	lists:sublist(Set, Start, Limit)
    end;

get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).

%% @spec (Server) -> Users_Number
%%     Server = string()
%%     Users_Number = integer()

get_vh_registered_users_number(Server) ->
    LServer = exmpp_jid:prep_domain(exmpp_jid:parse(Server)),
    Query = mnesia:dirty_select(
		reg_users_counter,
		[{#reg_users_counter{vhost = LServer, count = '$1'},
		  [],
		  ['$1']}]),
    case Query of
	[Count] ->
	    Count;
	_ -> 0
    end.

%% @spec (Server, [{prefix, Prefix}]) -> Users_Number
%%     Server = string()
%%     Prefix = string()
%%     Users_Number = integer()

get_vh_registered_users_number(Server, [{prefix, Prefix}]) when is_list(Prefix) ->
    Set = [{U, S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)],
    length(Set);
    
get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

%% @spec (User, Server) -> Password | false
%%     User = string()
%%     Server = string()
%%     Password = string()

get_password(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
        case catch gen_storage:dirty_read(LServer, passwd, US) of
	[#passwd{password = ""} = Passwd] ->
	    {base64:decode(Passwd#passwd.storedkey),
	     base64:decode(Passwd#passwd.serverkey),
	     base64:decode(Passwd#passwd.salt),
	     Passwd#passwd.iterationcount};
	[#passwd{password = Password}] ->
		Password;
	_ ->
		false
	end
    catch
	_ ->
	    false
    end.

%% @spec (User, Server) -> Password | nil()
%%     User = string()
%%     Server = string()
%%     Password = string()

get_password_s(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
        case catch gen_storage:dirty_read(LServer, passwd, US) of
	[#passwd{password = Password}] ->
	    Password;
	    _ ->
		[]
	end
    catch
	_ ->
	    []
    end.

%% @spec (User, Server) -> true | false | {error, Error}
%%     User = string()
%%     Server = string()

is_user_exists(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
        case catch gen_storage:dirty_read(LServer, {passwd, US}) of
	    [] ->
		false;
	    [_] ->
		true;
	    Other ->
		{error, Other}
	end
    catch
	_ ->
	    false
    end.

%% @spec (User, Server) -> ok
%%     User = string()
%%     Server = string()
%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.

remove_user(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
	F = fun() ->
		    gen_storage:delete(LServer, {passwd, US}),
		    mnesia:dirty_update_counter(reg_users_counter,
						exmpp_jid:prep_domain(exmpp_jid:parse(Server)), -1)
	    end,
        gen_storage:transaction(LServer, passwd, F),
	ok
    catch
	_ ->
	    ok
    end.

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request
%%     User = string()
%%     Server = string()
%%     Password = string()
%% @doc Remove user if the provided password is correct.

remove_user(User, Server, Password) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
	F = fun() ->
		    case gen_storage:read(LServer, {passwd, US}) of
		    [#passwd{password = ""} = Passwd] ->
			case is_password_scram_valid(Password, Passwd) of
			    true ->
				gen_storage:delete(LServer, {passwd, US}),
				mnesia:dirty_update_counter(reg_users_counter,
							    LServer, -1),
				ok;
			    false ->
				not_allowed
			end;
			[#passwd{password = Password}] ->
			    gen_storage:delete(LServer, {passwd, US}),
			    mnesia:dirty_update_counter(reg_users_counter,
							exmpp_jid:prep_domain(exmpp_jid:parse(Server)), -1),
			    ok;
			_ ->
			    not_exists
		    end
	    end,
        case gen_storage:transaction(LServer, passwd, F) of
	    {atomic, ok} ->
		ok;
	    {atomic, Res} ->
		Res
	end
    catch
	_ ->
	    bad_request
    end.

%%%
%%% SCRAM
%%%

%% The passwords are stored scrammed in the table either if the option says so,
%% or if at least the first password is empty.

action_password_format(Host) ->
    OptionScram = is_option_scram(Host),
    case {OptionScram, get_format_first_element(Host)} of
	{true, scram} -> scram;
	{true, any} -> scram;
	{true, plain} -> must_scram;
	{false, plain} -> plain;
	{false, any} -> plain;
	{false, scram} ->
	    set_option_password_format(scram),
	    forced_scram
    end.

get_format_first_element(HostB) ->
    case gen_storage:dirty_select(HostB, passwd, []) of
	[] -> any;
	[#passwd{password = ""} | _] -> scram;
	[#passwd{} | _] -> plain
    end.

is_option_scram(Host) when is_list(Host) ->
    scram == ejabberd_config:get_local_option({auth_password_format, Host}).

set_option_password_format(Value) ->
    ?ERROR_MSG("Some passwords are stored in the database as SCRAM bits, "
	       "but the option 'auth_password_format' is not configured 'scram'. "
	       "The option will now be considered to be 'scram'.", []),
    ejabberd_config:add_local_option({auth_password_format, ?MYNAME}, Value).

maybe_scram_passwords(HostB) ->
    case action_password_format(binary_to_list(HostB)) of
	must_scram -> scram_passwords(HostB);
	_ -> ok
    end.

scram_passwords(HostB) ->
    Backend =
        case ejabberd_config:get_local_option({auth_storage, binary_to_list(HostB)}) of
            undefined -> mnesia;
            B -> B
        end,
    scram_passwords(HostB, Backend).
scram_passwords(HostB, mnesia) ->
    ?INFO_MSG("Converting the passwords stored in mnesia for host ~p into SCRAM bits", [HostB]),
    gen_storage_migration:migrate_mnesia(
      HostB, passwd,
      [{passwd, [user_host, password, storedkey, serverkey, salt, iterationcount],
	fun(#passwd{password = Password} = Passwd) ->
		password_to_scram(Password, Passwd)
	end}]);
scram_passwords(HostB, odbc) ->
    ?INFO_MSG("Converting the passwords stored in odbc for host ~p into SCRAM bits", [HostB]),
    gen_storage_migration:migrate_odbc(
      HostB, [passwd],
      [{"passwd", ["user", "host", "password", "storedkey", "serverkey", "salt", "iterationcount"],
	fun(_, User, Host2, Password, _Storedkey, _Serverkey, _Iterationcount, _Salt) ->
		password_to_scram(Password, #passwd{user_host = {User, Host2}})
	end}]).

password_to_scram(Password, Passwd) ->
    password_to_scram(Password, Passwd, ?SCRAM_DEFAULT_ITERATION_COUNT).

password_to_scram(Password, Passwd, IterationCount) ->
    Salt = crypto:rand_bytes(?SALT_LENGTH),
    SaltedPassword = scram:salted_password(Password, Salt, IterationCount),
    StoredKey = scram:stored_key(scram:client_key(SaltedPassword)),
    ServerKey = scram:server_key(SaltedPassword),
    Passwd#passwd{password = "",
	   storedkey = base64:encode(StoredKey),
	   salt = base64:encode(Salt),
	   iterationcount = IterationCount,
	   serverkey = base64:encode(ServerKey)}.

is_password_scram_valid(Password, Passwd) ->
    IterationCount = Passwd#passwd.iterationcount,
    Salt = base64:decode(Passwd#passwd.salt),
    SaltedPassword = scram:salted_password(Password, Salt, IterationCount),
    StoredKey = scram:stored_key(scram:client_key(SaltedPassword)),
    (base64:decode(Passwd#passwd.storedkey) == StoredKey).


update_table(Host, mnesia) ->
    gen_storage_migration:migrate_mnesia(
      Host, passwd,
      [{passwd, [us, password],
	fun({passwd, {User, _Host}, Password}) ->
		case is_list(Password) of
		    true ->
			#passwd{user_host = {User, Host},
				password = Password};
		    false ->
			#passwd{user_host = {User, Host},
				password = "",
				storedkey = Password#scram.storedkey,
				serverkey = Password#scram.serverkey,
				salt = Password#scram.salt,
				iterationcount = Password#scram.iterationcount}
		end
	end}]);
update_table(Host, odbc) ->
    gen_storage_migration:migrate_odbc(
      Host, [passwd],
      [{"users", ["username", "password"],
	fun(_, User, Password) ->
		#passwd{user_host = {User, Host},
			password = Password}
	end}]).
