%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_internal.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via mnesia
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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

-module(ejabberd_auth_internal).
-author('alexey@process-one.net').

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
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

-record(passwd, {us, password}).
-record(reg_users_counter, {vhost, count}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec (Host) -> ok
%%     Host = string()

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
    LServer = exmpp_jid:prep_domain(exmpp_jid:parse(Server)),
    set_vh_registered_users_counter(LServer, Size).

%% @spec () -> bool()

plain_password_required() ->
    false.

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()

check_password(User, Server, Password) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({passwd, US}) of
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
    case catch mnesia:dirty_read({passwd, US}) of
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
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    F = fun() ->
			mnesia:write(#passwd{us = US,
					     password = Password})
		end,
	    {atomic, ok} = mnesia:transaction(F),
	    ok
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {aborted, Reason}
%%     User = string()
%%     Server = string()
%%     Password = string()

try_register(User, Server, Password) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    US = {LUser, LServer},
    if
	(LUser == error) or (LServer == error) ->
	    {error, invalid_jid};
	true ->
	    F = fun() ->
			case mnesia:read({passwd, US}) of
			    [] ->
				mnesia:write(#passwd{us = US,
						     password = Password}),
				inc_vh_registered_users_counter(LServer),
				ok;
			    [_E] ->
				exists
			end
		end,
	    mnesia:transaction(F)
    end.

%% @spec () -> [{LUser, LServer}]
%%     LUser = string()
%%     LServer = string()
%% @doc Get all registered users in Mnesia.

dirty_get_registered_users() ->
    mnesia:dirty_all_keys(passwd).

%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users(Server) ->
    LServer = exmpp_stringprep:nameprep(Server),
    mnesia:dirty_select(
      passwd,
      [{#passwd{us = '$1', _ = '_'}, 
	[{'==', {element, 2, '$1'}, LServer}], 
	['$1']}]).

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

inc_vh_registered_users_counter(LServer) ->
    F = fun() ->
		case mnesia:wread({reg_users_counter, LServer}) of
		    [C] ->
			Count = C#reg_users_counter.count + 1,
			C2 = C#reg_users_counter{count = Count},
			mnesia:write(C2);
		    _ ->
			mnesia:write(#reg_users_counter{vhost = LServer,
						      count = 1})
		end
	end,
    mnesia:sync_dirty(F).

dec_vh_registered_users_counter(LServer) ->
    F = fun() ->
		case mnesia:wread({reg_users_counter, LServer}) of
		    [C] ->
			Count = C#reg_users_counter.count - 1,
			C2 = C#reg_users_counter{count = Count},
			mnesia:write(C2);
		    _ ->
			error
		end
	end,
    mnesia:sync_dirty(F).

set_vh_registered_users_counter(LServer, Count) ->
    F = fun() ->
		mnesia:write(#reg_users_counter{vhost = LServer,
						count = Count})
	end,
    mnesia:sync_dirty(F).

%% @spec (User, Server) -> Password | false
%%     User = string()
%%     Server = string()
%%     Password = string()

get_password(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
	case catch mnesia:dirty_read(passwd, US) of
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
	case catch mnesia:dirty_read(passwd, US) of
	    [#passwd{password = Password}] ->
		Password;
	    _ ->
		[]
	end
    catch
	_ ->
	    []
    end.

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
	case catch mnesia:dirty_read({passwd, US}) of
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
		    mnesia:delete({passwd, US}),
		    dec_vh_registered_users_counter(LServer)
	    end,
	mnesia:transaction(F),
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
		    case mnesia:read({passwd, US}) of
			[#passwd{password = Password}] ->
			    mnesia:delete({passwd, US}),
			    dec_vh_registered_users_counter(LServer),
			    ok;
			[_] ->
			    not_allowed;
			_ ->
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
	end
    catch
	_ ->
	    bad_request
    end.

%% @spec () -> term()

update_table() ->
    Fields = record_info(fields, passwd),
    case mnesia:table_info(passwd, attributes) of
	Fields ->
	    % No conversion is needed when the table comes from an exmpp-less
	    % Ejabberd because ejabberd_auth* modules use string() and not
	    % binary().
	    ok;
	[user, password] ->
	    ?INFO_MSG("Converting passwd table from "
		      "{user, password} format", []),
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



