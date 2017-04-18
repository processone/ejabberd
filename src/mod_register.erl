%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Inband registration support
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(mod_register).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-protocol({xep, 77, '2.4'}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, stream_feature_register/2,
	 c2s_unauthenticated_packet/2, try_register/5,
	 process_iq/1, send_registration_notifications/3,
	 transform_options/1, transform_module_options/1,
	 mod_opt_type/1, opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_REGISTER, ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_REGISTER, ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_pre_auth_features, Host, ?MODULE,
		       stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_packet, Host,
		       ?MODULE, c2s_unauthenticated_packet, 50),
    ejabberd_mnesia:create(?MODULE, mod_register_ip,
			[{ram_copies, [node()]}, {local_content, true},
			 {attributes, [key, value]}]),
    mnesia:add_table_copy(mod_register_ip, node(),
			  ram_copies),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_pre_auth_features, Host,
			  ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_packet, Host,
			  ?MODULE, c2s_unauthenticated_packet, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_REGISTER).

reload(Host, NewOpts, OldOpts) ->
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
	    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_REGISTER,
					  ?MODULE, process_iq, IQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_REGISTER,
					  ?MODULE, process_iq, IQDisc);
	true ->
	    ok
    end.

depends(_Host, _Opts) ->
    [].

-spec stream_feature_register([xmpp_element()], binary()) -> [xmpp_element()].
stream_feature_register(Acc, Host) ->
    AF = gen_mod:get_module_opt(Host, ?MODULE, access_from,
                                          fun(A) -> A end,
					  all),
    case (AF /= none) of
	true ->
	    [#feature_register{}|Acc];
	false ->
	    Acc
    end.

c2s_unauthenticated_packet(#{ip := IP, server := Server} = State,
			   #iq{type = T, sub_els = [_]} = IQ)
  when T == set; T == get ->
    case xmpp:get_subtag(IQ, #register{}) of
	#register{} = Register ->
	    {Address, _} = IP,
	    IQ1 = xmpp:set_els(IQ, [Register]),
	    IQ2 = xmpp:set_from_to(IQ1, jid:make(<<>>), jid:make(Server)),
	    ResIQ = process_iq(IQ2, Address),
	    ResIQ1 = xmpp:set_from_to(ResIQ, jid:make(Server), undefined),
	    {stop, ejabberd_c2s:send(State, ResIQ1)};
	false ->
	    State
    end;
c2s_unauthenticated_packet(State, _) ->
    State.

process_iq(#iq{from = From} = IQ) ->
    process_iq(IQ, jid:tolower(From)).

process_iq(#iq{from = From, to = To} = IQ, Source) ->
    IsCaptchaEnabled =
	case gen_mod:get_module_opt(To#jid.lserver, ?MODULE,
				    captcha_protected,
				    fun(B) when is_boolean(B) -> B end,
				    false) of
	    true -> true;
	    false -> false
	end,
    Server = To#jid.lserver,
    Access = gen_mod:get_module_opt(Server, ?MODULE, access,
				    fun(A) -> A end, all),
    AllowRemove = allow == acl:match_rule(Server, Access, From),
    process_iq(IQ, Source, IsCaptchaEnabled, AllowRemove).

process_iq(#iq{type = set, lang = Lang,
	       sub_els = [#register{remove = true}]} = IQ,
	   _Source, _IsCaptchaEnabled, _AllowRemove = false) ->
    Txt = <<"Denied by ACL">>,
    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang));
process_iq(#iq{type = set, lang = Lang, to = To, from = From,
	       sub_els = [#register{remove = true,
				    username = User,
				    password = Password}]} = IQ,
	   _Source, _IsCaptchaEnabled, _AllowRemove = true) ->
    Server = To#jid.lserver,
    if is_binary(User) ->
	    case From of
		#jid{user = User, lserver = Server} ->
		    ejabberd_auth:remove_user(User, Server),
		    xmpp:make_iq_result(IQ);
		_ ->
		    if is_binary(Password) ->
			    ejabberd_auth:remove_user(User, Server, Password),
			    xmpp:make_iq_result(IQ);
		       true ->
			    Txt = <<"No 'password' found in this query">>,
			    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
		    end
	    end;
       true ->
	    case From of
		#jid{luser = LUser, lserver = Server} ->
		    ResIQ = xmpp:make_iq_result(IQ),
		    ejabberd_router:route(xmpp:set_from_to(ResIQ, From, From)),
		    ejabberd_auth:remove_user(LUser, Server),
		    ignore;
		_ ->
		    Txt = <<"The query is only allowed from local users">>,
		    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang))
	    end
    end;
process_iq(#iq{type = set, to = To,
	       sub_els = [#register{username = User,
				    password = Password}]} = IQ,
	   Source, IsCaptchaEnabled, _AllowRemove) when is_binary(User),
							is_binary(Password) ->
    Server = To#jid.lserver,
    try_register_or_set_password(
      User, Server, Password, IQ, Source, not IsCaptchaEnabled);
process_iq(#iq{type = set, to = To,
	       lang = Lang, sub_els = [#register{xdata = #xdata{} = X}]} = IQ,
	   Source, true, _AllowRemove) ->
    Server = To#jid.lserver,
    case ejabberd_captcha:process_reply(X) of
	ok ->
	    case process_xdata_submit(X) of
		{ok, User, Password} ->
		    try_register_or_set_password(
		      User, Server, Password, IQ, Source, true);
		_ ->
		    Txt = <<"Incorrect data form">>,
		    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
	    end;
	{error, malformed} ->
	    Txt = <<"Incorrect CAPTCHA submit">>,
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	_ ->
	    ErrText = <<"The CAPTCHA verification has failed">>,
	    xmpp:make_error(IQ, xmpp:err_not_allowed(ErrText, Lang))
    end;
process_iq(#iq{type = set} = IQ, _Source, _IsCaptchaEnabled, _AllowRemove) ->
    xmpp:make_error(IQ, xmpp:err_bad_request());
process_iq(#iq{type = get, from = From, to = To, id = ID, lang = Lang} = IQ,
	   Source, IsCaptchaEnabled, _AllowRemove) ->
    Server = To#jid.lserver,
    {IsRegistered, Username} =
	case From of
	    #jid{user = User, lserver = Server} ->
		case ejabberd_auth:is_user_exists(User, Server) of
		    true ->
			{true, User};
		    false ->
			{false, User}
		end;
	    _ ->
		{false, <<"">>}
	end,
    Instr = translate:translate(
	      Lang, <<"Choose a username and password to register "
		      "with this server">>),
    if IsCaptchaEnabled and not IsRegistered ->
	    TopInstr = translate:translate(
			 Lang, <<"You need a client that supports x:data "
				 "and CAPTCHA to register">>),
	    UField = #xdata_field{type = 'text-single',
				  label = translate:translate(Lang, <<"User">>),
				  var = <<"username">>,
				  required = true},
	    PField = #xdata_field{type = 'text-private',
				  label = translate:translate(Lang, <<"Password">>),
				  var = <<"password">>,
				  required = true},
	    X = #xdata{type = form, instructions = [Instr],
		       fields = [UField, PField]},
	    case ejabberd_captcha:create_captcha_x(ID, To, Lang, Source, X) of
		{ok, CaptchaEls} ->
		    xmpp:make_iq_result(
		      IQ, #register{instructions = TopInstr,
				    sub_els = CaptchaEls});
		{error, limit} ->
		    ErrText = <<"Too many CAPTCHA requests">>,
		    xmpp:make_error(
		      IQ, xmpp:err_resource_constraint(ErrText, Lang));
		_Err ->
		    ErrText = <<"Unable to generate a CAPTCHA">>,
		    xmpp:make_error(
		      IQ, xmpp:err_internal_server_error(ErrText, Lang))
	    end;
       true ->
	    xmpp:make_iq_result(
	      IQ,
	      #register{instructions = Instr,
			username = Username,
			password = <<"">>,
			registered = IsRegistered})
    end.

try_register_or_set_password(User, Server, Password,
			     #iq{from = From, lang = Lang} = IQ,
			     Source, CaptchaSucceed) ->
    case From of
	#jid{user = User, lserver = Server} ->
	    try_set_password(User, Server, Password, IQ);
	_ when CaptchaSucceed ->
	    case check_from(From, Server) of
		allow ->
		    case try_register(User, Server, Password, Source, Lang) of
			ok ->
			    xmpp:make_iq_result(IQ);
			{error, Error} ->
			    xmpp:make_error(IQ, Error)
		    end;
		deny ->
		    Txt = <<"Denied by ACL">>,
		    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang))
	    end;
	_ ->
	    xmpp:make_error(IQ, xmpp:err_not_allowed())
    end.

%% @doc Try to change password and return IQ response
try_set_password(User, Server, Password, #iq{lang = Lang, meta = M} = IQ) ->
    case is_strong_password(Server, Password) of
      true ->
	  case ejabberd_auth:set_password(User, Server, Password) of
	    ok ->
		?INFO_MSG("~s has changed password from ~s",
			  [jid:encode({User, Server, <<"">>}),
			   ejabberd_config:may_hide_data(
			     misc:ip_to_list(maps:get(ip, M, {0,0,0,0})))]),
		xmpp:make_iq_result(IQ);
	    {error, empty_password} ->
		Txt = <<"Empty password">>,
		xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	    {error, not_allowed} ->
		Txt = <<"Changing password is not allowed">>,
		xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
	    {error, invalid_jid} ->
		xmpp:make_error(IQ, xmpp:err_jid_malformed());
	    Err ->
		?ERROR_MSG("failed to register user ~s@~s: ~p",
			   [User, Server, Err]),
		xmpp:make_error(IQ, xmpp:err_internal_server_error())
	  end;
      error_preparing_password ->
	  ErrText = <<"The password contains unacceptable characters">>,
	  xmpp:make_error(IQ, xmpp:err_not_acceptable(ErrText, Lang));
      false ->
	  ErrText = <<"The password is too weak">>,
	  xmpp:make_error(IQ, xmpp:err_not_acceptable(ErrText, Lang))
    end.

try_register(User, Server, Password, SourceRaw, Lang) ->
    case jid:is_nodename(User) of
      false -> {error, xmpp:err_bad_request(<<"Malformed username">>, Lang)};
      _ ->
	  JID = jid:make(User, Server),
	  Access = gen_mod:get_module_opt(Server, ?MODULE, access,
                                          fun(A) -> A end,
					  all),
	  IPAccess = get_ip_access(Server),
	  case {acl:match_rule(Server, Access, JID),
		check_ip_access(SourceRaw, IPAccess)}
	      of
	    {deny, _} -> {error, xmpp:err_forbidden(<<"Denied by ACL">>, Lang)};
	    {_, deny} -> {error, xmpp:err_forbidden(<<"Denied by ACL">>, Lang)};
	    {allow, allow} ->
		Source = may_remove_resource(SourceRaw),
		case check_timeout(Source) of
		  true ->
		      case is_strong_password(Server, Password) of
			true ->
			    case ejabberd_auth:try_register(User, Server,
							    Password)
				of
			      {atomic, ok} ->
				  send_welcome_message(JID),
				  send_registration_notifications(
                                    ?MODULE, JID, Source),
				  ok;
			      Error ->
				  remove_timeout(Source),
				  case Error of
				    {atomic, exists} ->
					Txt = <<"User already exists">>,
					{error, xmpp:err_conflict(Txt, Lang)};
				    {error, invalid_jid} ->
					{error, xmpp:err_jid_malformed()};
				    {error, not_allowed} ->
					{error, xmpp:err_not_allowed()};
				    {error, too_many_users} ->
					Txt = <<"Too many users registered">>,
					{error, xmpp:err_resource_constraint(Txt, Lang)};
				    {error, _} ->
					?ERROR_MSG("failed to register user "
						   "~s@~s: ~p",
						   [User, Server, Error]),
					{error, xmpp:err_internal_server_error()}
				  end
			    end;
			error_preparing_password ->
			    remove_timeout(Source),
			    ErrText = <<"The password contains unacceptable characters">>,
			    {error, xmpp:err_not_acceptable(ErrText, Lang)};
			false ->
			    remove_timeout(Source),
			    ErrText = <<"The password is too weak">>,
			    {error, xmpp:err_not_acceptable(ErrText, Lang)}
		      end;
		  false ->
		      ErrText =
			  <<"Users are not allowed to register accounts "
			    "so quickly">>,
		      {error, xmpp:err_resource_constraint(ErrText, Lang)}
		end
	  end
    end.

send_welcome_message(JID) ->
    Host = JID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, welcome_message,
                                fun(Opts) ->
                                        S = proplists:get_value(
                                              subject, Opts, <<>>),
                                        B = proplists:get_value(
                                              body, Opts, <<>>),
                                        {iolist_to_binary(S),
                                         iolist_to_binary(B)}
                                end, {<<"">>, <<"">>})
	of
      {<<"">>, <<"">>} -> ok;
      {Subj, Body} ->
	  ejabberd_router:route(
	    #message{from = jid:make(Host),
		     to = JID,
		     subject = xmpp:mk_text(Subj),
		     body = xmpp:mk_text(Body)});
      _ -> ok
    end.

send_registration_notifications(Mod, UJID, Source) ->
    Host = UJID#jid.lserver,
    case gen_mod:get_module_opt(
           Host, Mod, registration_watchers,
           fun(Ss) ->
                   [jid:decode(iolist_to_binary(S)) || S <- Ss]
           end, []) of
        [] -> ok;
        JIDs when is_list(JIDs) ->
            Body =
                (str:format("[~s] The account ~s was registered from "
                                               "IP address ~s on node ~w using ~p.",
                                               [get_time_string(),
                                                jid:encode(UJID),
                                                ip_to_string(Source), node(),
                                                Mod])),
            lists:foreach(
              fun(JID) ->
                      ejabberd_router:route(
			#message{from = jid:make(Host),
				 to = JID,
				 type = chat,
				 body = xmpp:mk_text(Body)})
              end, JIDs)
    end.

check_from(#jid{user = <<"">>, server = <<"">>},
	   _Server) ->
    allow;
check_from(JID, Server) ->
    Access = gen_mod:get_module_opt(Server, ?MODULE, access_from,
                                    fun(A) -> A end,
                                    none),
    acl:match_rule(Server, Access, JID).

check_timeout(undefined) -> true;
check_timeout(Source) ->
    Timeout = ejabberd_config:get_option(
                registration_timeout,
                fun(TO) when is_integer(TO), TO > 0 ->
                        TO;
                   (infinity) ->
                        infinity;
                   (unlimited) ->
                        infinity
                end, 600),
    if is_integer(Timeout) ->
	   Priority = -p1_time_compat:system_time(seconds),
	   CleanPriority = Priority + Timeout,
	   F = fun () ->
		       Treap = case mnesia:read(mod_register_ip, treap, write)
				   of
				 [] -> treap:empty();
				 [{mod_register_ip, treap, T}] -> T
			       end,
		       Treap1 = clean_treap(Treap, CleanPriority),
		       case treap:lookup(Source, Treap1) of
			 error ->
			     Treap2 = treap:insert(Source, Priority, [],
						   Treap1),
			     mnesia:write({mod_register_ip, treap, Treap2}),
			     true;
			 {ok, _, _} ->
			     mnesia:write({mod_register_ip, treap, Treap1}),
			     false
		       end
	       end,
	   case mnesia:transaction(F) of
	     {atomic, Res} -> Res;
	     {aborted, Reason} ->
		 ?ERROR_MSG("mod_register: timeout check error: ~p~n",
			    [Reason]),
		 true
	   end;
       true -> true
    end.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
      true -> Treap;
      false ->
	  {_Key, Priority, _Value} = treap:get_root(Treap),
	  if Priority > CleanPriority ->
		 clean_treap(treap:delete_root(Treap), CleanPriority);
	     true -> Treap
	  end
    end.

remove_timeout(undefined) -> true;
remove_timeout(Source) ->
    Timeout = ejabberd_config:get_option(
                registration_timeout,
                fun(TO) when is_integer(TO), TO > 0 ->
                        TO;
                   (infinity) ->
                        infinity;
                   (unlimited) ->
                        infinity
                end, 600),
    if is_integer(Timeout) ->
	   F = fun () ->
		       Treap = case mnesia:read(mod_register_ip, treap, write)
				   of
				 [] -> treap:empty();
				 [{mod_register_ip, treap, T}] -> T
			       end,
		       Treap1 = treap:delete(Source, Treap),
		       mnesia:write({mod_register_ip, treap, Treap1}),
		       ok
	       end,
	   case mnesia:transaction(F) of
	     {atomic, ok} -> ok;
	     {aborted, Reason} ->
		 ?ERROR_MSG("mod_register: timeout remove error: "
			    "~p~n",
			    [Reason]),
		 ok
	   end;
       true -> ok
    end.

ip_to_string(Source) when is_tuple(Source) ->
    misc:ip_to_list(Source);
ip_to_string(undefined) -> <<"undefined">>;
ip_to_string(_) -> <<"unknown">>.

get_time_string() -> write_time(erlang:localtime()).
%% Function copied from ejabberd_logger_h.erl and customized

write_time({{Y, Mo, D}, {H, Mi, S}}) ->
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
		  [Y, Mo, D, H, Mi, S]).

process_xdata_submit(X) ->
    case {xmpp_util:get_xdata_values(<<"username">>, X),
	  xmpp_util:get_xdata_values(<<"password">>, X)} of
	{[User], [Pass]} -> {ok, User, Pass};
	_ -> error
    end.

is_strong_password(Server, Password) ->
    case jid:resourceprep(Password) of
	PP when is_binary(PP) ->
	    is_strong_password2(Server, Password);
	error ->
	    error_preparing_password
    end.

is_strong_password2(Server, Password) ->
    LServer = jid:nameprep(Server),
    case gen_mod:get_module_opt(LServer, ?MODULE, password_strength,
                                fun(N) when is_number(N), N>=0 -> N end,
                                0) of
        0 ->
            true;
        Entropy ->
            ejabberd_auth:entropy(Password) >= Entropy
    end.

transform_options(Opts) ->
    Opts1 = transform_ip_access(Opts),
    transform_module_options(Opts1).

transform_ip_access(Opts) ->
    try
        {value, {modules, ModOpts}, Opts1} = lists:keytake(modules, 1, Opts),
        {value, {?MODULE, RegOpts}, ModOpts1} = lists:keytake(?MODULE, 1, ModOpts),
        {value, {ip_access, L}, RegOpts1} = lists:keytake(ip_access, 1, RegOpts),
        true = is_list(L),
        ?WARNING_MSG("Old 'ip_access' format detected. "
                     "The old format is still supported "
                     "but it is better to fix your config: "
                     "use access rules instead.", []),
        ACLs = lists:flatmap(
                 fun({Action, S}) ->
                         ACLName = misc:binary_to_atom(
                                     iolist_to_binary(
                                       ["ip_", S])),
                         [{Action, ACLName},
                          {acl, ACLName, {ip, S}}]
                 end, L),
        Access = {access, mod_register_networks,
                  [{Action, ACLName} || {Action, ACLName} <- ACLs]},
        [ACL || {acl, _, _} = ACL <- ACLs] ++
            [Access,
             {modules,
              [{mod_register,
                [{ip_access, mod_register_networks}|RegOpts1]}
               | ModOpts1]}|Opts1]
    catch error:{badmatch, false} ->
            Opts
    end.

transform_module_options(Opts) ->
    lists:flatmap(
      fun({welcome_message, {Subj, Body}}) ->
              ?WARNING_MSG("Old 'welcome_message' format detected. "
                           "The old format is still supported "
                           "but it is better to fix your config: "
                           "change it to {welcome_message, "
                           "[{subject, Subject}, {body, Body}]}",
                           []),
              [{welcome_message, [{subject, Subj}, {body, Body}]}];
         (Opt) ->
              [Opt]
      end, Opts).

%%%
%%% ip_access management
%%%

may_remove_resource({_, _, _} = From) ->
    jid:remove_resource(From);
may_remove_resource(From) -> From.

get_ip_access(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, ip_access,
                           fun(A) when is_atom(A) -> A end,
                           all).

check_ip_access({User, Server, Resource}, IPAccess) ->
    case ejabberd_sm:get_user_ip(User, Server, Resource) of
        {IPAddress, _PortNumber} ->
            check_ip_access(IPAddress, IPAccess);
        _ ->
            deny
    end;
check_ip_access(undefined, _IPAccess) ->
    deny;
check_ip_access(IPAddress, IPAccess) ->
    acl:match_rule(global, IPAccess, IPAddress).

mod_opt_type(access) -> fun acl:access_rules_validator/1;
mod_opt_type(access_from) -> fun acl:access_rules_validator/1;
mod_opt_type(captcha_protected) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(ip_access) -> fun acl:access_rules_validator/1;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(password_strength) ->
    fun (N) when is_number(N), N >= 0 -> N end;
mod_opt_type(registration_watchers) ->
    fun (Ss) ->
	    [jid:decode(iolist_to_binary(S)) || S <- Ss]
    end;
mod_opt_type(welcome_message) ->
    fun (Opts) ->
	    S = proplists:get_value(subject, Opts, <<>>),
	    B = proplists:get_value(body, Opts, <<>>),
	    {iolist_to_binary(S), iolist_to_binary(B)}
    end;
mod_opt_type(_) ->
    [access, access_from, captcha_protected, ip_access,
     iqdisc, password_strength, registration_watchers,
     welcome_message].

opt_type(registration_timeout) ->
    fun (TO) when is_integer(TO), TO > 0 -> TO;
	(infinity) -> infinity;
	(unlimited) -> infinity
    end;
opt_type(_) -> [registration_timeout].
