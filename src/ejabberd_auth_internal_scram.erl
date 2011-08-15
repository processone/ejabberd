-module(ejabberd_auth_internal_scram).
-author('stephen.roettger@googlemail.com').

%% External exports
-export([start/1,
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
	 storage_type/0,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

-record(passwd, {us, stored_key, salt, iteration_count, server_key}).
-record(reg_users_counter, {vhost, count}).

-define(DEFAULT_ITERATION_COUNT, 4096).
-define(SALT_LENGTH, 16).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    mnesia:create_table(passwd, [{disc_copies, [node()]},
				 {attributes, record_info(fields, passwd)}]),
    mnesia:create_table(reg_users_counter,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, reg_users_counter)}]),
    update_table(),
    update_reg_users_counter_table(Host),
    ok.

update_reg_users_counter_table(Server) ->
    Set = get_vh_registered_users(Server),
    Size = length(Set),
    LServer = jlib:nameprep(Server),
    F = fun() ->
	    mnesia:write(#reg_users_counter{vhost = LServer,
					    count = Size})
	end,
    mnesia:sync_dirty(F).

plain_password_required() ->
    true.

storage_type() ->
	scram.

check_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    [UserEntry] = (catch mnesia:dirty_read({passwd, US})),
	IterationCount = UserEntry#passwd.iteration_count,
	Salt = UserEntry#passwd.salt,
	SaltedPassword = scram:salted_password(Password, Salt, IterationCount),
	StoredKey = scram:stored_key(scram:client_key(SaltedPassword)),
	if
	(UserEntry#passwd.stored_key == StoredKey) ->
		true;
	true ->
		false
	end.

check_password(User, Server, Password, Digest, DigestGen) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
	[#passwd{stored_key = Passwd}] ->
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

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
	Salt = crypto:rand_bytes(?SALT_LENGTH),
	IterationCount = ?DEFAULT_ITERATION_COUNT,
	SaltedPassword = scram:salted_password(Password, Salt, IterationCount),
	StoredKey = scram:stored_key(scram:client_key(SaltedPassword)),
	ServerKey = scram:server_key(SaltedPassword),
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    F = fun() ->
			mnesia:write(#passwd{us = US,
						     stored_key = StoredKey,
							 salt = Salt,
							 iteration_count = IterationCount,
							 server_key = ServerKey})
		end,
	    {atomic, ok} = mnesia:transaction(F),
	    ok
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {aborted, Reason}
try_register(User, Server, Password) ->
	try_register(User, Server, Password, ?DEFAULT_ITERATION_COUNT).

try_register(User, Server, Password, IterationCount) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
	Salt = crypto:rand_bytes(?SALT_LENGTH),
	SaltedPassword = scram:salted_password(Password, Salt, IterationCount),
	StoredKey = scram:stored_key(scram:client_key(SaltedPassword)),
	ServerKey = scram:server_key(SaltedPassword),
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    F = fun() ->
			case mnesia:read({passwd, US}) of
			    [] ->
				mnesia:write(#passwd{us = US,
						     stored_key = StoredKey,
							 salt = Salt,
							 iteration_count = IterationCount,
							 server_key = ServerKey}),
				mnesia:dirty_update_counter(
						    reg_users_counter,
						    LServer, 1),
				ok;
			    [_E] ->
				exists
			end
		end,
	    mnesia:transaction(F)
    end.

%% Get all registered users in Mnesia
dirty_get_registered_users() ->
    mnesia:dirty_all_keys(passwd).

get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    mnesia:dirty_select(
      passwd,
      [{#passwd{us = '$1', _ = '_'}, 
	[{'==', {element, 2, '$1'}, LServer}], 
	['$1']}]).

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

get_vh_registered_users_number(Server) ->
    LServer = jlib:nameprep(Server),
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

get_vh_registered_users_number(Server, [{prefix, Prefix}]) when is_list(Prefix) ->
    Set = [{U, S} || {U, S} <- get_vh_registered_users(Server), lists:prefix(Prefix, U)],
    length(Set);
    
get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

get_password(User, Server) ->
	%false.
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read(passwd, US) of
	[#passwd{stored_key = Password, server_key = ServerKey, salt = Salt, iteration_count = IterationCount}] ->
		{Password, ServerKey, Salt, IterationCount};
	_ ->
	    false
    end.

get_password_s(User, Server) ->
	%"".
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read(passwd, US) of
	[#passwd{stored_key = Password, server_key = ServerKey, salt = Salt, iteration_count = IterationCount}] ->
		{Password, ServerKey, Salt, IterationCount};
	_ ->
	    []
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
	[] ->
	    false;
	[_] ->
	    true;
	Other ->
	    {error, Other}
    end.

%% @spec (User, Server) -> ok
%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		mnesia:delete({passwd, US}),
		mnesia:dirty_update_counter(reg_users_counter,
					    LServer, -1)
        end,
    mnesia:transaction(F),
	ok.

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		case mnesia:read({passwd, US}) of
		[UserEntry] ->
			IterationCount = UserEntry#passwd.iteration_count,
			Salt = UserEntry#passwd.salt,
			SaltedPassword = scram:salted_password(Password, Salt, IterationCount),
			StoredKey = scram:stored_key(scram:client_key(SaltedPassword)),
			if
			(UserEntry#passwd.stored_key == StoredKey) ->
				mnesia:delete({passwd, US}),
				mnesia:dirty_update_counter(reg_users_counter,
								LServer, -1),
				ok;
			true ->
				not_allowed
			end;
		_Else ->
			not_exists
        end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{atomic, Res} ->
	    Res;
	_ ->
	    bad_request
    end.


update_table() ->
    Fields = record_info(fields, passwd),
    case mnesia:table_info(passwd, attributes) of
	Fields ->
	    ok;
	[user, stored_key] ->
	    ?INFO_MSG("Converting passwd table from "
		      "{user, stored_key} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     ejabberd_auth_internal_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, passwd},
			      {attributes, record_info(fields, passwd)}]),
	    mnesia:transform_table(passwd, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(ejabberd_auth_internal_tmp_table),
			 mnesia:foldl(
			   fun(#passwd{us = U} = R, _) ->
				   mnesia:dirty_write(
				     ejabberd_auth_internal_tmp_table,
				     R#passwd{us = {U, Host}})
			   end, ok, passwd)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(passwd),
	    F2 = fun() ->
			 mnesia:write_lock_table(passwd),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, ejabberd_auth_internal_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(ejabberd_auth_internal_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating passwd table", []),
	    mnesia:transform_table(passwd, ignore, Fields)
    end.



