%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_odbc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Authentification via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_auth_odbc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% External exports
-export([start/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

-record(passwd, {user, password}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    ejabberd_odbc_sup:start_link(Host),
    ok.

plain_password_required() ->
    false.

check_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    case catch ejabberd_odbc:sql_query(
			 jlib:nameprep(Server),
			 ["select password from users "
			  "where username='", Username, "'"]) of
		{selected, ["password"], [{Password}]} ->
		    true;
		_ ->
		    false
	    end
    end.

check_password(User, Server, Password, StreamID, Digest) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    case catch ejabberd_odbc:sql_query(
			 jlib:nameprep(Server),
			 ["select password from users "
			  "where username='", Username, "'"]) of
		{selected, ["password"], [{Passwd}]} ->
		    DigRes = if
				 Digest /= "" ->
				     Digest == sha:sha(StreamID ++ Passwd);
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
	    end
    end.

set_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
	error ->
	    {error, invalid_jid};
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    Pass = ejabberd_odbc:escape(Password),
	    catch ejabberd_odbc:sql_query(
		    jlib:nameprep(Server),
		    ["begin;"
		     "delete from users where username='", Username ,"';"
		     "insert into users(username, password) "
		     "values ('", Username, "', '", Pass, "'); commit"])
    end.


try_register(User, Server, Password) ->
    case jlib:nodeprep(User) of
	error ->
	    {error, invalid_jid};
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    Pass = ejabberd_odbc:escape(Password),
	    case catch ejabberd_odbc:sql_query(
			 jlib:nameprep(Server),
			 ["insert into users(username, password) "
			  "values ('", Username, "', '", Pass, "')"]) of
		{updated, _} ->
		    {atomic, ok};
		_ ->
		    {atomic, exists}
	    end
    end.

dirty_get_registered_users() ->
    get_vh_registered_users(?MYNAME).

get_vh_registered_users(Server) ->
    case catch ejabberd_odbc:sql_query(
		 jlib:nameprep(Server),
		 "select username from users") of
	{selected, ["username"], Res} ->
	    [U || {U} <- Res];
	_ ->
	    []
    end.

get_password(User, Server) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    case catch ejabberd_odbc:sql_query(
			 jlib:nameprep(Server),
			 ["select password from users "
			  "where username='", Username, "'"]) of
		{selected, ["password"], [{Password}]} ->
		    Password;
		_ ->
		    false
	    end
    end.

get_password_s(User, Server) ->
    case jlib:nodeprep(User) of
	error ->
	    "";
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    case catch ejabberd_odbc:sql_query(
			 jlib:nameprep(Server),
			 ["select password from users "
			  "where username='", Username, "'"]) of
		{selected, ["password"], [{Password}]} ->
		    Password;
		_ ->
		    ""
	    end
    end.

is_user_exists(User, Server) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    case catch ejabberd_odbc:sql_query(
			 jlib:nameprep(Server),
			 ["select password from users "
			  "where username='", Username, "'"]) of
		{selected, ["password"], [{_Password}]} ->
		    true;
		_ ->
		    false
	    end
    end.

remove_user(User, Server) ->
    case jlib:nodeprep(User) of
	error ->
	    error;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    catch ejabberd_odbc:sql_query(
		    jlib:nameprep(Server),
		    ["delete from users where username='", Username ,"'"]),
	    ejabberd_hooks:run(remove_user, jlib:nameprep(Server), [User])
    end.

remove_user(User, Server, Password) ->
    case jlib:nodeprep(User) of
	error ->
	    error;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    Pass = ejabberd_odbc:escape(Password),
	    case catch
		ejabberd_odbc:sql_query(
		  jlib:nameprep(Server),
		  ["begin;"
		   "select password from users where username='", Username, "';"
		   "delete from users "
		   "where username='", Username, "' and password='", Pass, "';"
		   "commit"]) of
		{selected, ["password"], [{Password}]} ->
		    ejabberd_hooks:run(remove_user, jlib:nameprep(Server),
				       [User]),
		    ok;
		{selected, ["password"], []} ->
		    not_exists;
		_ ->
		    not_allowed
	    end
    end.
