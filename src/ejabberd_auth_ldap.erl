%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_ldap.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Authentification via LDAP
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_auth_ldap).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% External exports
-export([start/0,
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
-include("eldap/eldap.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    LDAPServers = ejabberd_config:get_local_option(ldap_servers),
    RootDN = ejabberd_config:get_local_option(ldap_rootdn),
    Password = ejabberd_config:get_local_option(ldap_password),
    eldap:start_link("ejabberd", LDAPServers, 389, RootDN, Password),
    eldap:start_link("ejabberd_bind", LDAPServers, 389, RootDN, Password),
    ok.

plain_password_required() ->
    true.

check_password(User, _Server, Password) ->
    case find_user_dn(User) of
	false ->
	    false;
	DN ->
	    case eldap:bind("ejabberd_bind", DN, Password) of
		ok ->
		    true;
		_ ->
		    false
	    end
    end.

check_password(User, Server, Password, _StreamID, _Digest) ->
    check_password(User, Server, Password).

set_password(_User, _Server, _Password) ->
    {error, not_allowed}.

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

dirty_get_registered_users() ->
    get_vh_registered_users(?MYNAME).

get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    Attr = ejabberd_config:get_local_option(ldap_uidattr),
    Filter = eldap:present(Attr),
    Base = ejabberd_config:get_local_option(ldap_base),
    case eldap:search("ejabberd", [{base, Base},
				   {filter, Filter},
				   {attributes, [Attr]}]) of
	#eldap_search_result{entries = Es} ->
	    lists:flatmap(
	      fun(E) ->
		      case lists:keysearch(Attr, 1, E#eldap_entry.attributes) of
			  {value, {_, [U]}} ->
			      case jlib:nodeprep(U) of
				  error ->
				      [];
				  LU ->
				      [{LU, LServer}]
			      end;
			  _ ->
			      []
		      end
	      end, Es);
	_ ->
	    []
    end.

get_password(_User, _Server) ->
    false.

get_password_s(_User, _Server) ->
    "".

is_user_exists(User, _Server) ->
    case find_user_dn(User) of
	false ->
	    false;
	_DN ->
	    true
    end.

remove_user(_User, _Server) ->
    {error, not_allowed}.

remove_user(_User, _Server, _Password) ->
    not_allowed.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

find_user_dn(User) ->
    Attr = ejabberd_config:get_local_option(ldap_uidattr),
    Filter = eldap:equalityMatch(Attr, User),
    Base = ejabberd_config:get_local_option(ldap_base),
    case eldap:search("ejabberd", [{base, Base},
				   {filter, Filter},
				   {attributes, []}]) of
	#eldap_search_result{entries = [E | _]} ->
	    E#eldap_entry.object_name;
	_ ->
	    false
    end.

