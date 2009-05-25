%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_ldap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via LDAP
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

-module(ejabberd_auth_ldap).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
	 handle_info/2,
	 handle_call/3,
	 handle_cast/2,
	 terminate/2,
	 code_change/3
	]).

%% External exports
-export([start/1,
	 stop/1,
	 start_link/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users_number/1,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").
-include("eldap/eldap.hrl").

-record(state, {host,
		eldap_id,
		bind_eldap_id,
		servers,
		backups,
		port,
		encrypt,
		dn,
		password,
		base,
		uids,
		ufilter,
		sfilter,
		lfilter, %% Local filter (performed by ejabberd, not LDAP)
		dn_filter,
		dn_filter_attrs
	       }).

%% Unused callbacks.
handle_cast(_Request, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_info(_Info, State) ->
    {noreply, State}.
%% -----


-define(LDAP_SEARCH_TIMEOUT, 5). % Timeout for LDAP search queries in seconds


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @spec (Host) -> term()
%%     Host = string()

start(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {
      Proc, {?MODULE, start_link, [Host]},
      transient, 1000, worker, [?MODULE]
     },
    supervisor:start_child(ejabberd_sup, ChildSpec).

%% @spec (Host) -> term()
%%     Host = string()

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%% @spec (Host) -> term()
%%     Host = string()

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, Host, []).

%% @hidden

terminate(_Reason, _State) ->
    ok.

%% @spec (Host) -> {ok, State}
%%     Host = string()
%%     State = term()

init(Host) ->
    State = parse_options(Host),
    eldap_pool:start_link(State#state.eldap_id,
		     State#state.servers,
		     State#state.backups,
		     State#state.port,
		     State#state.dn,
		     State#state.password,
		     State#state.encrypt),
    eldap_pool:start_link(State#state.bind_eldap_id,
		     State#state.servers,
		     State#state.backups,
		     State#state.port,
		     State#state.dn,
		     State#state.password,
		     State#state.encrypt),
    {ok, State}.

%% @spec () -> true

plain_password_required() ->
    true.

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()

check_password(User, Server, Password) ->
    %% In LDAP spec: empty password means anonymous authentication.
    %% As ejabberd is providing other anonymous authentication mechanisms
    %% we simply prevent the use of LDAP anonymous authentication.
    if Password == "" ->
        false;
    true ->
        case catch check_password_ldap(User, Server, Password) of
	        {'EXIT', _} -> false;
	        Result -> Result
        end
    end.

%% @spec (User, Server, Password, Digest, DigestGen) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()
%%     Digest = string()
%%     DigestGen = function()

check_password(User, Server, Password, _Digest, _DigestGen) ->
    check_password(User, Server, Password).

%% @spec (User, Server, Password) -> {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string()

set_password(_User, _Server, _Password) ->
    {error, not_allowed}.

%% @spec (User, Server, Password) -> {error, not_allowed}
%%     User = string()
%%     Server = string()
%%     Password = string()

try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

%% @spec () -> [{LUser, LServer}]
%%     LUser = string()
%%     LServer = string()

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(ldap),
    lists:flatmap(
      fun(Server) ->
	      get_vh_registered_users(Server)
      end, Servers).

%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users(Server) ->
    case catch get_vh_registered_users_ldap(Server) of
	{'EXIT', _} -> [];
	Result -> Result
	end.

%% @spec (Server) -> Users_Number
%%     Server = string()
%%     Users_Number = integer()

get_vh_registered_users_number(Server) ->
    length(get_vh_registered_users(Server)).

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

get_password(_User, _Server) ->
    false.

%% @spec (User, Server) -> nil()
%%     User = string()
%%     Server = string()

get_password_s(_User, _Server) ->
    "".

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    case catch is_user_exists_ldap(User, Server) of
	{'EXIT', Error} ->
	    {error, Error};
	Result ->
	    Result
    end.

%% @spec (User, Server) -> {error, not_allowed}
%%     User = string()
%%     Server = string()

remove_user(_User, _Server) ->
    {error, not_allowed}.

%% @spec (User, Server, Password) -> not_allowed
%%     User = string()
%%     Server = string()
%%     Password = string()

remove_user(_User, _Server, _Password) ->
    not_allowed.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% @spec (User, Server, Password) -> bool()
%%     User = string()
%%     Server = string()
%%     Password = string()

check_password_ldap(User, Server, Password) ->
	{ok, State} = eldap_utils:get_state(Server, ?MODULE),
	case find_user_dn(User, State) of
	false ->
	    false;
	DN ->
	    case eldap_pool:bind(State#state.bind_eldap_id, DN, Password) of
		ok -> true;
		_ -> false
	    end
	end.

%% @spec (Server) -> [{LUser, LServer}]
%%     Server = string()
%%     LUser = string()
%%     LServer = string()

get_vh_registered_users_ldap(Server) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    UIDs = State#state.uids,
    Eldap_ID = State#state.eldap_id,
    Server = State#state.host,
    SortedDNAttrs = eldap_utils:usort_attrs(State#state.dn_filter_attrs),
    case eldap_filter:parse(State#state.sfilter) of
		{ok, EldapFilter} ->
		    case eldap_pool:search(Eldap_ID, [{base, State#state.base},
						 {filter, EldapFilter},
						 {timeout, ?LDAP_SEARCH_TIMEOUT},
						 {attributes, SortedDNAttrs}]) of
			#eldap_search_result{entries = Entries} ->
			    lists:flatmap(
			      fun(#eldap_entry{attributes = Attrs,
					       object_name = DN}) ->
				      case is_valid_dn(DN, Attrs, State) of
					  false -> [];
					  _ ->
					      case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
						  "" -> [];
						  {User, UIDFormat} ->
						      case eldap_utils:get_user_part(User, UIDFormat) of
							  {ok, U} ->
							      try
								  [{exmpp_stringprep:nodeprep(U), exmpp_stringprep:nameprep(Server)}]
							      catch
								  _ ->
								      []
							      end;
							  _ -> []
						      end
					      end
				      end
			      end, Entries);
			_ ->
			    []
		    end;
		_ ->
		    []
	end.

%% @spec (User, Server) -> bool()
%%     User = string()
%%     Server = string()

is_user_exists_ldap(User, Server) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    case find_user_dn(User, State) of
		false -> false;
		_DN -> true
	end.

handle_call(get_state, _From, State) ->
	{reply, {ok, State}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

find_user_dn(User, State) ->
    DNAttrs = eldap_utils:usort_attrs(State#state.dn_filter_attrs),
    case eldap_filter:parse(State#state.ufilter, [{"%u", User}]) of
	{ok, Filter} ->
	    case eldap_pool:search(State#state.eldap_id, [{base, State#state.base},
						     {filter, Filter},
						     {attributes, DNAttrs}]) of
		#eldap_search_result{entries = [#eldap_entry{attributes = Attrs,
							     object_name = DN} | _]} ->
			dn_filter(DN, Attrs, State);
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

%% apply the dn filter and the local filter:
dn_filter(DN, Attrs, State) ->
    %% Check if user is denied access by attribute value (local check)
    case check_local_filter(Attrs, State) of
        false -> false;
        true -> is_valid_dn(DN, Attrs, State)
    end.

%% Check that the DN is valid, based on the dn filter
is_valid_dn(DN, _, #state{dn_filter = undefined}) ->
    DN;

is_valid_dn(DN, Attrs, State) ->
    DNAttrs = State#state.dn_filter_attrs,
    UIDs = State#state.uids,
    Values = [{"%s", eldap_utils:get_ldap_attr(Attr, Attrs), 1} || Attr <- DNAttrs],
    SubstValues = case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
		      "" -> Values;
		      {S, UAF} ->
			  case eldap_utils:get_user_part(S, UAF) of
			      {ok, U} -> [{"%u", U} | Values];
			      _ -> Values
			  end
		  end ++ [{"%d", State#state.host}, {"%D", DN}],
    case eldap_filter:parse(State#state.dn_filter, SubstValues) of
	{ok, EldapFilter} ->
	    case eldap_pool:search(State#state.eldap_id, [
						     {base, State#state.base},
						     {filter, EldapFilter},
						     {attributes, ["dn"]}]) of
		#eldap_search_result{entries = [_|_]} ->
		    DN;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

%% The local filter is used to check an attribute in ejabberd
%% and not in LDAP to limit the load on the LDAP directory.
%% A local rule can be either:
%%    {equal, {"accountStatus",["active"]}}
%%    {notequal, {"accountStatus",["disabled"]}}
%% {ldap_local_filter, {notequal, {"accountStatus",["disabled"]}}}
check_local_filter(_Attrs, #state{lfilter = undefined}) ->
    true;
check_local_filter(Attrs, #state{lfilter = LocalFilter}) ->
    {Operation, FilterMatch} = LocalFilter,
    local_filter(Operation, Attrs, FilterMatch).
    
local_filter(equal, Attrs, FilterMatch) ->
    {Attr, Value} = FilterMatch,
    case lists:keysearch(Attr, 1, Attrs) of
        false -> false;
        {value,{Attr,Value}} -> true;
        _ -> false
    end;
local_filter(notequal, Attrs, FilterMatch) ->
    not local_filter(equal, Attrs, FilterMatch).

%%%----------------------------------------------------------------------
%%% Auxiliary functions
%%%----------------------------------------------------------------------
parse_options(Host) ->
    Eldap_ID = atom_to_list(gen_mod:get_module_proc(Host, ?MODULE)),
    Bind_Eldap_ID = atom_to_list(gen_mod:get_module_proc(Host, bind_ejabberd_auth_ldap)),
    LDAPServers = ejabberd_config:get_local_option({ldap_servers, Host}),
    LDAPBackups = case ejabberd_config:get_local_option({ldap_backups, Host}) of
		   undefined -> [];
		   Backups -> Backups
		   end,
    LDAPEncrypt = ejabberd_config:get_local_option({ldap_encrypt, Host}),
    LDAPPort = case ejabberd_config:get_local_option({ldap_port, Host}) of
		   undefined -> case LDAPEncrypt of
				    tls -> ?LDAPS_PORT;
				    starttls -> ?LDAP_PORT;
				    _ -> ?LDAP_PORT
				end;
		   P -> P
	       end,
    RootDN = case ejabberd_config:get_local_option({ldap_rootdn, Host}) of
		 undefined -> "";
		 RDN -> RDN
	     end,
    Password = case ejabberd_config:get_local_option({ldap_password, Host}) of
		   undefined -> "";
		   Pass -> Pass
	       end,
    UIDs = case ejabberd_config:get_local_option({ldap_uids, Host}) of
	       undefined -> [{"uid", "%u"}];
	       UI -> eldap_utils:uids_domain_subst(Host, UI)
	   end,
    SubFilter = lists:flatten(eldap_utils:generate_subfilter(UIDs)),
    UserFilter = case ejabberd_config:get_local_option({ldap_filter, Host}) of
		     undefined -> SubFilter;
		     "" -> SubFilter;
		     F -> "(&" ++ SubFilter ++ F ++ ")"
		 end,
    SearchFilter = eldap_filter:do_sub(UserFilter, [{"%u", "*"}]),
    LDAPBase = ejabberd_config:get_local_option({ldap_base, Host}),
    {DNFilter, DNFilterAttrs} =
	case ejabberd_config:get_local_option({ldap_dn_filter, Host}) of
	    undefined -> {undefined, undefined};
	    {DNF, DNFA} -> {DNF, DNFA}
	end,
	LocalFilter = ejabberd_config:get_local_option({ldap_local_filter, Host}),
    #state{host = Host,
	   eldap_id = Eldap_ID,
	   bind_eldap_id = Bind_Eldap_ID,
	   servers = LDAPServers,
	   backups = LDAPBackups,
	   port = LDAPPort,
	   encrypt = LDAPEncrypt,
	   dn = RootDN,
	   password = Password,
	   base = LDAPBase,
	   uids = UIDs,
	   ufilter = UserFilter,
	   sfilter = SearchFilter,
	   lfilter = LocalFilter,
	   dn_filter = DNFilter,
	   dn_filter_attrs = DNFilterAttrs
	  }.
