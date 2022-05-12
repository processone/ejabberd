%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_ldap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentication via LDAP
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-export([start/1, stop/1, start_link/1, set_password/3,
	 check_password/4, user_exists/2,
	 get_users/2, count_users/2,
	 store_type/1, plain_password_required/1,
	 reload/1]).

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
         deref_aliases = never  :: never | searching | finding | always,
         dn_filter              :: binary() | undefined,
         dn_filter_attrs = []   :: [binary()]}).

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-define(LDAP_SEARCH_TIMEOUT, 5).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {Proc, {?MODULE, start_link, [Host]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_backend_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    case supervisor:terminate_child(ejabberd_backend_sup, Proc) of
	ok -> supervisor:delete_child(ejabberd_backend_sup, Proc);
	Err -> Err
    end.

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, Host, []).

terminate(_Reason, _State) -> ok.

init(Host) ->
    process_flag(trap_exit, true),
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

reload(Host) ->
    stop(Host),
    start(Host).

plain_password_required(_) -> true.

store_type(_) -> external.

check_password(User, AuthzId, Server, Password) ->
    if AuthzId /= <<>> andalso AuthzId /= User ->
	    {nocache, false};
       Password == <<"">> ->
	    {nocache, false};
       true ->
	    case catch check_password_ldap(User, Server, Password) of
		{'EXIT', _} -> {nocache, false};
		Result -> {cache, Result}
	    end
    end.

set_password(User, Server, Password) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    case find_user_dn(User, State) of
      false -> {cache, {error, db_failure}};
      DN ->
	    case eldap_pool:modify_passwd(State#state.eldap_id, DN,
					  Password) of
		ok -> {cache, {ok, Password}};
		_Err -> {nocache, {error, db_failure}}
	    end
    end.

get_users(Server, []) ->
    case catch get_users_ldap(Server) of
      {'EXIT', _} -> [];
      Result -> Result
    end.

count_users(Server, Opts) ->
    length(get_users(Server, Opts)).

user_exists(User, Server) ->
    case catch user_exists_ldap(User, Server) of
	{'EXIT', _Error} -> {nocache, {error, db_failure}};
	Result -> {cache, Result}
    end.

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

get_users_ldap(Server) ->
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
							case jid:nodeprep(U) of
							  error -> [];
							  LU ->
							      [{LU,
								jid:nameprep(Server)}]
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

user_exists_ldap(User, Server) ->
    {ok, State} = eldap_utils:get_state(Server, ?MODULE),
    case find_user_dn(User, State) of
      false -> false;
      _DN -> true
    end.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

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
		is_valid_dn(DN, Attrs, State);
	    _ -> false
	  end;
      _ -> false
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
    Cfg = ?eldap_config(ejabberd_option, Host),
    Eldap_ID = misc:atom_to_binary(gen_mod:get_module_proc(Host, ?MODULE)),
    Bind_Eldap_ID = misc:atom_to_binary(
                      gen_mod:get_module_proc(Host, bind_ejabberd_auth_ldap)),
    UIDsTemp = ejabberd_option:ldap_uids(Host),
    UIDs = eldap_utils:uids_domain_subst(Host, UIDsTemp),
    SubFilter =	eldap_utils:generate_subfilter(UIDs),
    UserFilter = case ejabberd_option:ldap_filter(Host) of
                     <<"">> ->
			 SubFilter;
                     F ->
                         <<"(&", SubFilter/binary, F/binary, ")">>
                 end,
    SearchFilter = eldap_filter:do_sub(UserFilter, [{<<"%u">>, <<"*">>}]),
    {DNFilter, DNFilterAttrs} = ejabberd_option:ldap_dn_filter(Host),
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
	   sfilter = SearchFilter,
	   dn_filter = DNFilter, dn_filter_attrs = DNFilterAttrs}.
