%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_internal.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Authentification via mnesia
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_auth_internal).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% External exports
-export([start/0,
	 set_password/2,
	 check_password/2,
	 check_password/4,
	 try_register/2,
	 dirty_get_registered_users/0,
	 get_password/1,
	 get_password_s/1,
	 is_user_exists/1,
	 remove_user/1,
	 remove_user/2,
	 plain_password_required/0
	]).

-record(passwd, {user, password}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    mnesia:create_table(passwd,[{disc_copies, [node()]},
				{attributes, record_info(fields, passwd)}]),
    ok.

plain_password_required() ->
    false.

check_password(User, Password) ->
    LUser = jlib:nodeprep(User),
    case catch mnesia:dirty_read({passwd, LUser}) of
	[#passwd{password = Password}] ->
	    true;
	_ ->
	    false
    end.

check_password(User, Password, StreamID, Digest) ->
    LUser = jlib:nodeprep(User),
    case catch mnesia:dirty_read({passwd, LUser}) of
	[#passwd{password = Passwd}] ->
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
    end.

set_password(User, Password) ->
    case jlib:nodeprep(User) of
	error -> {error, invalid_jid};
	LUser ->
	    F = fun() ->
			mnesia:write(#passwd{user = LUser,
					     password = Password})
		end,
	    mnesia:transaction(F)
    end.


try_register(User, Password) ->
    case jlib:nodeprep(User) of
	error -> {error, invalid_jid};
	LUser ->
	    F = fun() ->
			case mnesia:read({passwd, LUser}) of
			    [] ->
				mnesia:write(#passwd{user = LUser,
						     password = Password}),
				ok;
			    [_E] ->
				exists
			end
		end,
	    mnesia:transaction(F)
    end.

dirty_get_registered_users() ->
    mnesia:dirty_all_keys(passwd).

get_password(User) ->
    LUser = jlib:nodeprep(User),
    case catch mnesia:dirty_read(passwd, LUser) of
	[#passwd{password = Password}] ->
	    Password;
	_ ->
	    false
    end.

get_password_s(User) ->
    LUser = jlib:nodeprep(User),
    case catch mnesia:dirty_read(passwd, LUser) of
	[#passwd{password = Password}] ->
	    Password;
	_ ->
	    []
    end.

is_user_exists(User) ->
    LUser = jlib:nodeprep(User),
    case catch mnesia:dirty_read({passwd, LUser}) of
	[] ->
	    false;
	[_] ->
	    true;
	_ ->
	    false
    end.

remove_user(User) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
		mnesia:delete({passwd, LUser})
        end,
    mnesia:transaction(F),
    ejabberd_hooks:run(remove_user, [User]).

remove_user(User, Password) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
		case mnesia:read({passwd, LUser}) of
		    [#passwd{password = Password}] ->
			mnesia:delete({passwd, LUser}),
			ok;
		    [_] ->
			not_allowed;
		    _ ->
			not_exists
		end
        end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ejabberd_hooks:run(remove_user, [User]),
	    ok;
	{atomic, Res} ->
	    Res;
	_ ->
	    bad_request
    end.
