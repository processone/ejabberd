%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_auth).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_server).

%% External exports
-export([start/0, start_link/0,
	 set_password/2,
	 check_password/2,
	 check_password/4,
	 try_register/2,
	 dirty_get_registered_users/0,
	 get_password/1,
	 get_password_s/1,
	 is_user_exists/1,
	 remove_user/1,
	 remove_user/2]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).

-record(state, {}).

-record(passwd, {user, password}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ejabberd_auth}, ejabberd_auth, [], []).
start_link() ->
    gen_server:start_link({local, ejabberd_auth}, ejabberd_auth, [], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    mnesia:create_table(passwd,[{disc_copies, [node()]},
				{attributes, record_info(fields, passwd)}]),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

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
		    Passwd == Password
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
    catch mod_roster:remove_user(User),
    catch mod_offline:remove_user(User),
    catch mod_last:remove_user(User),
    catch mod_vcard:remove_user(User),
    catch mod_private:remove_user(User).

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
	    catch mod_roster:remove_user(User),
	    catch mod_offline:remove_user(User),
	    catch mod_last:remove_user(User),
	    catch mod_vcard:remove_user(User),
	    catch mod_private:remove_user(User),
	    ok;
	{atomic, Res} ->
	    Res;
	_ ->
	    bad_request
    end.

