%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_ldap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via LDAP
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(ejabberd_auth_ldap).

-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(ejabberd_auth).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

%% External exports
-export([start/1, stop/1, start_link/1, set_password/3,
	 check_password/3, check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0,
	 plain_password_required/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("eldap.hrl").

-record(state,
	{host = <<"">>          :: binary(),
         eldap_id = <<"">>      :: binary(),
         bind_eldap_id = <<"">> :: binary(),
         servers = []           :: [binary()],
         backups = []           :: [binary()],
         port = ?LDAP_PORT      :: inet:port_number(),
	 tls_options = []       :: list(),
         dn = <<"">>            :: binary(),
         password = <<"">>      :: binary(),
         base = <<"">>          :: binary(),
         uids = []              :: [{binary()} | {binary(), binary()}],
         ufilter = <<"">>       :: binary(),
         sfilter = <<"">>       :: binary(),
	 lfilter                :: {any(), any()},
         deref_aliases = never  :: never | searching | finding | always,
         dn_filter              :: binary(),
         dn_filter_attrs = []   :: [binary()]}).

handle_cast(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> {noreply, State}.

-define(LDAP_SEARCH_TIMEOUT, 5).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {Proc, {?MODULE, start_link, [Host]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, Host, []).

terminate(_Reason, _State) -> ok.

init(Host) ->
    State = parse_options(Host),
    eldap_pool:start_link(State#state.eldap_id,
			  State#state.servers, State#state.backups,
			  State#state.port, State#state.dn,
			  State#state.password, State#state.tls_options),
    eldap_pool:start_link(State#state.bind_eldap_id,
			  State#state.servers, State#state.backups,
			  State#state.port, State#state.dn,
			  State#state.password, State#state.tls_options),
    {ok, State}.

plain_password_required() -> true.

store_type() -> external.

check_password(User, Server, Password) ->
    if Password == <<"">> -> false;
       true ->
	   case catch check_password_ldap(User, Server, Password)
	       of
	     {'EXIT', _} -> false;
	     Result -> Result
	   end
    end.

check_password(User, Server, Password, _Digest,
	       _DigestGen) ->
    check_password(User, Server, Password).

set_password(User, Server, Password) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    case find_user_dn(User, State) of
      false -> {error, user_not_found};
      DN ->
	  eldap_pool:modify_passwd(State#state.eldap_id, DN,
				   Password)
    end.

%% @spec (User, Server, Password) -> {error, not_allowed}
try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(ldap),
    lists:flatmap(fun (Server) ->
			  get_vh_registered_users(Server)
		  end,
		  Servers).

get_vh_registered_users(Server) ->
    case catch get_vh_registered_users_ldap(Server) of
      {'EXIT', _} -> [];
      Result -> Result
    end.

get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).

get_vh_registered_users_number(Server) ->
    length(get_vh_registered_users(Server)).

get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

get_password(_User, _Server) -> false.

get_password_s(_User, _Server) -> <<"">>.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    case catch is_user_exists_ldap(User, Server) of
      {'EXIT', Error} -> {error, Error};
      Result -> Result
    end.

remove_user(_User, _Server) -> {error, not_allowed}.

remove_user(_User, _Server, _Password) -> not_allowed.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
check_password_ldap(User, Server, Password) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    case find_user_dn(User, State) of
      false -> false;
      DN ->
	  case eldap_pool:bind(State#state.bind_eldap_id, DN,
			       Password)
	      of
	    ok -> true;
	    _ -> false
	  end
    end.

get_vh_registered_users_ldap(Server) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    UIDs = State#state.uids,
    Eldap_ID = State#state.eldap_id,
    Server = State#state.host,
    ResAttrs = result_attrs(State),
    case eldap_filter:parse(State#state.sfilter) of
      {ok, EldapFilter} ->
	  case eldap_pool:search(Eldap_ID,
				 [{base, State#state.base},
				  {filter, EldapFilter},
				  {timeout, ?LDAP_SEARCH_TIMEOUT},
				  {deref_aliases, State#state.deref_aliases},
				  {attributes, ResAttrs}])
	      of
	    #eldap_search_result{entries = Entries} ->
		lists:flatmap(fun (#eldap_entry{attributes = Attrs,
						object_name = DN}) ->
				      case is_valid_dn(DN, Attrs, State) of
					false -> [];
					_ ->
					    case
					      eldap_utils:find_ldap_attrs(UIDs,
									  Attrs)
						of
					      <<"">> -> [];
					      {User, UIDFormat} ->
						  case
						    eldap_utils:get_user_part(User,
									      UIDFormat)
						      of
						    {ok, U} ->
							case jlib:nodeprep(U) of
							  error -> [];
							  LU ->
							      [{LU,
								jlib:nameprep(Server)}]
							end;
						    _ -> []
						  end
					    end
				      end
			      end,
			      Entries);
	    _ -> []
	  end;
      _ -> []
    end.

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
    ResAttrs = result_attrs(State),
    case eldap_filter:parse(State#state.ufilter,
			    [{<<"%u">>, User}])
	of
      {ok, Filter} ->
	  case eldap_pool:search(State#state.eldap_id,
				 [{base, State#state.base}, {filter, Filter},
				  {deref_aliases, State#state.deref_aliases},
				  {attributes, ResAttrs}])
	      of
	    #eldap_search_result{entries =
				     [#eldap_entry{attributes = Attrs,
						   object_name = DN}
				      | _]} ->
		dn_filter(DN, Attrs, State);
	    _ -> false
	  end;
      _ -> false
    end.

%% apply the dn filter and the local filter:
dn_filter(DN, Attrs, State) ->
    case check_local_filter(Attrs, State) of
      false -> false;
      true -> is_valid_dn(DN, Attrs, State)
    end.

%% Check that the DN is valid, based on the dn filter
is_valid_dn(DN, _, #state{dn_filter = undefined}) -> DN;
is_valid_dn(DN, Attrs, State) ->
    DNAttrs = State#state.dn_filter_attrs,
    UIDs = State#state.uids,
    Values = [{<<"%s">>,
	       eldap_utils:get_ldap_attr(Attr, Attrs), 1}
	      || Attr <- DNAttrs],
    SubstValues = case eldap_utils:find_ldap_attrs(UIDs,
						   Attrs)
		      of
		    <<"">> -> Values;
		    {S, UAF} ->
			case eldap_utils:get_user_part(S, UAF) of
			  {ok, U} -> [{<<"%u">>, U} | Values];
			  _ -> Values
			end
		  end
		    ++ [{<<"%d">>, State#state.host}, {<<"%D">>, DN}],
    case eldap_filter:parse(State#state.dn_filter,
			    SubstValues)
	of
      {ok, EldapFilter} ->
	  case eldap_pool:search(State#state.eldap_id,
				 [{base, State#state.base},
				  {filter, EldapFilter},
				  {deref_aliases, State#state.deref_aliases},
				  {attributes, [<<"dn">>]}])
	      of
	    #eldap_search_result{entries = [_ | _]} -> DN;
	    _ -> false
	  end;
      _ -> false
    end.

%% The local filter is used to check an attribute in ejabberd
%% and not in LDAP to limit the load on the LDAP directory.
%% A local rule can be either:
%%    {equal, {"accountStatus",["active"]}}
%%    {notequal, {"accountStatus",["disabled"]}}
%% {ldap_local_filter, {notequal, {"accountStatus",["disabled"]}}}
check_local_filter(_Attrs,
		   #state{lfilter = undefined}) ->
    true;
check_local_filter(Attrs,
		   #state{lfilter = LocalFilter}) ->
    {Operation, FilterMatch} = LocalFilter,
    local_filter(Operation, Attrs, FilterMatch).

local_filter(equal, Attrs, FilterMatch) ->
    {Attr, Value} = FilterMatch,
    case lists:keysearch(Attr, 1, Attrs) of
      false -> false;
      {value, {Attr, Value}} -> true;
      _ -> false
    end;
local_filter(notequal, Attrs, FilterMatch) ->
    not local_filter(equal, Attrs, FilterMatch).

result_attrs(#state{uids = UIDs,
		    dn_filter_attrs = DNFilterAttrs}) ->
    lists:foldl(fun ({UID}, Acc) -> [UID | Acc];
		    ({UID, _}, Acc) -> [UID | Acc]
		end,
		DNFilterAttrs, UIDs).

%%%----------------------------------------------------------------------
%%% Auxiliary functions
%%%----------------------------------------------------------------------
parse_options(Host) ->
    Cfg = eldap_utils:get_config(Host, []),
    Eldap_ID = jlib:atom_to_binary(gen_mod:get_module_proc(Host, ?MODULE)),
    Bind_Eldap_ID = jlib:atom_to_binary(
                      gen_mod:get_module_proc(Host, bind_ejabberd_auth_ldap)),
    UIDsTemp = eldap_utils:get_opt(
                 {ldap_uids, Host}, [],
                 fun(Us) ->
                         lists:map(
                           fun({U, P}) ->
                                   {iolist_to_binary(U),
                                    iolist_to_binary(P)};
                              ({U}) ->
                                   {iolist_to_binary(U)};
                              (U) ->
                                   {iolist_to_binary(U)}
                           end, lists:flatten(Us))
                 end, [{<<"uid">>, <<"%u">>}]),
    UIDs = eldap_utils:uids_domain_subst(Host, UIDsTemp),
    SubFilter =	eldap_utils:generate_subfilter(UIDs),
    UserFilter = case eldap_utils:get_opt(
                        {ldap_filter, Host}, [],
                        fun check_filter/1, <<"">>) of
                     <<"">> ->
			 SubFilter;
                     F ->
                         <<"(&", SubFilter/binary, F/binary, ")">>
                 end,
    SearchFilter = eldap_filter:do_sub(UserFilter,
				       [{<<"%u">>, <<"*">>}]),
    {DNFilter, DNFilterAttrs} =
        eldap_utils:get_opt({ldap_dn_filter, Host}, [],
                            fun([{DNF, DNFA}]) ->
                                    NewDNFA = case DNFA of
                                                  undefined ->
                                                      [];
                                                  _ ->
                                                      [iolist_to_binary(A)
                                                       || A <- DNFA]
                                              end,
                                    NewDNF = check_filter(DNF),
                                    {NewDNF, NewDNFA}
                            end, {undefined, []}),
    LocalFilter = eldap_utils:get_opt(
                    {ldap_local_filter, Host}, [], fun(V) -> V end),
    #state{host = Host, eldap_id = Eldap_ID,
           bind_eldap_id = Bind_Eldap_ID,
           servers = Cfg#eldap_config.servers,
	   backups = Cfg#eldap_config.backups,
           port = Cfg#eldap_config.port,
	   tls_options = Cfg#eldap_config.tls_options,
	   dn = Cfg#eldap_config.dn,
           password = Cfg#eldap_config.password,
           base = Cfg#eldap_config.base,
           deref_aliases = Cfg#eldap_config.deref_aliases,
	   uids = UIDs, ufilter = UserFilter,
	   sfilter = SearchFilter, lfilter = LocalFilter,
	   dn_filter = DNFilter, dn_filter_attrs = DNFilterAttrs}.

check_filter(F) ->
    NewF = iolist_to_binary(F),
    {ok, _} = eldap_filter:parse(NewF),
    NewF.
