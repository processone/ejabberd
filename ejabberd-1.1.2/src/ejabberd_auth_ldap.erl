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
		port,
		dn,
		password,
		base,
		uidattr,
		uidattr_format,
		ufilter,
		sfilter,
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

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {
      Proc, {?MODULE, start_link, [Host]},
      permanent, 1000, worker, [?MODULE]
     },
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, Host, []).

terminate(_Reason, State) ->
    ejabberd_ctl:unregister_commands(
      State#state.host,
      [{"registered-users", "list all registered users"}],
      ejabberd_auth, ctl_process_get_registered).

init(Host) ->
    State = parse_options(Host),
    eldap:start_link(State#state.eldap_id,
		     State#state.servers,
		     State#state.port,
		     State#state.dn,
		     State#state.password),
    eldap:start_link(State#state.bind_eldap_id,
		     State#state.servers,
		     State#state.port,
		     State#state.dn,
		     State#state.password),
    ejabberd_ctl:register_commands(
      Host,
      [{"registered-users", "list all registered users"}],
      ejabberd_auth, ctl_process_get_registered),
    {ok, State}.

-define(REPLY_TIMEOUT, 10000).

plain_password_required() ->
    true.

check_password(User, Server, Password) ->
    Proc = gen_mod:get_module_proc(Server, ?MODULE),
    case catch gen_server:call(Proc,
			       {check_pass, User, Password}, ?REPLY_TIMEOUT) of
	{'EXIT', _} ->
	    false;
	Result ->
	    Result
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
    Proc = gen_mod:get_module_proc(Server, ?MODULE),
    case catch gen_server:call(Proc,
			       get_vh_registered_users, ?REPLY_TIMEOUT) of
	{'EXIT', _} ->
	    [];
	Result ->
	    Result
    end.

get_password(_User, _Server) ->
    false.

get_password_s(_User, _Server) ->
    "".

is_user_exists(User, Server) ->
    Proc = gen_mod:get_module_proc(Server, ?MODULE),
    case catch gen_server:call(Proc,
			       {is_user_exists, User}, ?REPLY_TIMEOUT) of
	{'EXIT', _} ->
	    false;
	Result ->
	    Result
    end.

remove_user(_User, _Server) ->
    {error, not_allowed}.

remove_user(_User, _Server, _Password) ->
    not_allowed.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
handle_call({check_pass, User, Password}, _From, State) ->
    Reply = case find_user_dn(User, State) of
		false ->
		    false;
		DN ->
		    case eldap:bind(State#state.bind_eldap_id, DN, Password) of
			ok -> true;
			_ -> false
		    end
	    end,
    {reply, Reply, State};

handle_call(get_vh_registered_users, _From, State) ->
    UA = State#state.uidattr,
    UAF = State#state.uidattr_format,
    Eldap_ID = State#state.eldap_id,
    Server = State#state.host,
    SortedDNAttrs = usort_attrs(State#state.dn_filter_attrs),
    Reply = case eldap_filter:parse(State#state.sfilter) of
		{ok, EldapFilter} ->
		    case eldap:search(Eldap_ID, [{base, State#state.base},
						 {filter, EldapFilter},
						 {attributes, SortedDNAttrs}]) of
			#eldap_search_result{entries = Entries} ->
			    lists:flatmap(
			      fun(#eldap_entry{attributes = Attrs,
					       object_name = DN}) ->
				      case is_valid_dn(DN, Attrs, State) of
					  false -> [];
					  _ ->
					      case get_ldap_attr(UA, Attrs) of
						  "" -> [];
						  User ->
						      case get_user_part(User, UAF) of
							  {ok, U} ->
							      case jlib:nodeprep(U) of
								  error -> [];
								  LU -> [{LU, jlib:nameprep(Server)}]
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
	    end,
    {reply, Reply, State};

handle_call({is_user_exists, User}, _From, State) ->
    Reply = case find_user_dn(User, State) of
		false -> false;
		_DN -> true
	    end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

find_user_dn(User, State) ->
    DNAttrs = usort_attrs(State#state.dn_filter_attrs),
    case eldap_filter:parse(State#state.ufilter, [{"%u", User}]) of
	{ok, Filter} ->
	    case eldap:search(State#state.eldap_id, [{base, State#state.base},
						     {filter, Filter},
						     {attributes, DNAttrs}]) of
		#eldap_search_result{entries = [#eldap_entry{attributes = Attrs,
							     object_name = DN} | _]} ->
		    is_valid_dn(DN, Attrs, State);
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

is_valid_dn(DN, _, #state{dn_filter = undefined}) ->
    DN;

is_valid_dn(DN, Attrs, State) ->
    DNAttrs = State#state.dn_filter_attrs,
    UA = State#state.uidattr,
    UAF = State#state.uidattr_format,
    Values = [{"%s", get_ldap_attr(Attr, Attrs), 1} || Attr <- DNAttrs],
    SubstValues = case get_ldap_attr(UA, Attrs) of
		      "" -> Values;
		      S ->
			  case get_user_part(S, UAF) of
			      {ok, U} -> [{"%u", U} | Values];
			      _ -> Values
			  end
		  end ++ [{"%d", State#state.host}, {"%D", DN}],
    case eldap_filter:parse(State#state.dn_filter, SubstValues) of
	{ok, EldapFilter} ->
	    case eldap:search(State#state.eldap_id, [
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

%%%----------------------------------------------------------------------
%%% Auxiliary functions
%%%----------------------------------------------------------------------
get_user_part(String, Pattern) ->
    F = fun(S, P) ->
		First = string:str(P, "%u"),
		TailLength = length(P) - (First+1),
		string:sub_string(S, First, length(S) - TailLength)
	end,
    case catch F(String, Pattern) of
	{'EXIT', _} ->
	    {error, badmatch};
	Result ->
	    case regexp:sub(Pattern, "%u", Result) of
		{ok, String, _} -> {ok, Result};
		_ -> {error, badmatch}
	    end
    end.

case_insensitive_match(X, Y) ->
    X1 = stringprep:tolower(X),
    Y1 = stringprep:tolower(Y),
    if
	X1 == Y1 -> true;
	true -> false
    end.

get_ldap_attr(LDAPAttr, Attributes) ->
    Res = lists:filter(
	    fun({Name, _}) ->
		    case_insensitive_match(Name, LDAPAttr)
	    end, Attributes),
    case Res of
	[{_, [Value|_]}] -> Value;
	_ -> ""
    end.

usort_attrs(Attrs) when is_list(Attrs) ->
    lists:usort(Attrs);

usort_attrs(_) ->
    [].

parse_options(Host) ->
    Eldap_ID = atom_to_list(gen_mod:get_module_proc(Host, ?MODULE)),
    Bind_Eldap_ID = atom_to_list(gen_mod:get_module_proc(Host, bind_ejabberd_auth_ldap)),
    LDAPServers = ejabberd_config:get_local_option({ldap_servers, Host}),
    LDAPPort = case ejabberd_config:get_local_option({ldap_port, Host}) of
		   undefined -> 389;
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
    UIDAttr = case ejabberd_config:get_local_option({ldap_uidattr, Host}) of
		  undefined -> "uid";
		  UA -> UA
	      end,
    UIDAttrFormat = case ejabberd_config:get_local_option({ldap_uidattr_format, Host}) of
			undefined -> "%u";
			UAF -> UAF
		    end,
    SubFilter = "(" ++ UIDAttr ++ "=" ++ UIDAttrFormat ++ ")",
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
    #state{host = Host,
	   eldap_id = Eldap_ID,
	   bind_eldap_id = Bind_Eldap_ID,
	   servers = LDAPServers,
	   port = LDAPPort,
	   dn = RootDN,
	   password = Password,
	   base = LDAPBase,
	   uidattr = UIDAttr,
	   uidattr_format = UIDAttrFormat,
	   ufilter = UserFilter,
	   sfilter = SearchFilter,
	   dn_filter = DNFilter,
	   dn_filter_attrs = DNFilterAttrs
	  }.
