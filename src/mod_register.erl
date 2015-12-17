%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Inband registration support
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_register).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-protocol({xep, 77, '2.4'}).

-behaviour(gen_mod).

-export([start/2, stop/1, stream_feature_register/2,
	 unauthenticated_iq_register/4, try_register/5,
	 process_iq/3, send_registration_notifications/3,
	 transform_options/1, transform_module_options/1,
	 mod_opt_type/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_REGISTER, ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_REGISTER, ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_stream_features, Host, ?MODULE,
		       stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_iq, Host,
		       ?MODULE, unauthenticated_iq_register, 50),
    mnesia:create_table(mod_register_ip,
			[{ram_copies, [node()]}, {local_content, true},
			 {attributes, [key, value]}]),
    mnesia:add_table_copy(mod_register_ip, node(),
			  ram_copies),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_stream_features, Host,
			  ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,
			  ?MODULE, unauthenticated_iq_register, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_REGISTER).

stream_feature_register(Acc, Host) ->
    AF = gen_mod:get_module_opt(Host, ?MODULE, access_from,
                                          fun(A) when is_atom(A) -> A end,
					  all),
    case (AF /= none) and lists:keymember(<<"mechanisms">>, 2, Acc) of
	true ->
	    [#xmlel{name = <<"register">>,
		    attrs = [{<<"xmlns">>, ?NS_FEATURE_IQREGISTER}],
		    children = []}
	     | Acc];
	false ->
	    Acc
    end.

unauthenticated_iq_register(_Acc, Server,
			    #iq{xmlns = ?NS_REGISTER} = IQ, IP) ->
    Address = case IP of
		{A, _Port} -> A;
		_ -> undefined
	      end,
    ResIQ = process_iq(jid:make(<<"">>, <<"">>,
				     <<"">>),
		       jid:make(<<"">>, Server, <<"">>), IQ, Address),
    Res1 = jlib:replace_from_to(jid:make(<<"">>,
					      Server, <<"">>),
				jid:make(<<"">>, <<"">>, <<"">>),
				jlib:iq_to_xml(ResIQ)),
    jlib:remove_attr(<<"to">>, Res1);
unauthenticated_iq_register(Acc, _Server, _IQ, _IP) ->
    Acc.

process_iq(From, To, IQ) ->
    process_iq(From, To, IQ, jid:tolower(From)).

process_iq(From, To,
	   #iq{type = Type, lang = Lang, sub_el = SubEl, id = ID} =
	       IQ,
	   Source) ->
    IsCaptchaEnabled = case
			 gen_mod:get_module_opt(To#jid.lserver, ?MODULE,
						captcha_protected,
                                                fun(B) when is_boolean(B) -> B end,
                                                false)
			   of
			 true -> true;
			 _ -> false
		       end,
    case Type of
      set ->
	  UTag = xml:get_subtag(SubEl, <<"username">>),
	  PTag = xml:get_subtag(SubEl, <<"password">>),
	  RTag = xml:get_subtag(SubEl, <<"remove">>),
	  Server = To#jid.lserver,
	  Access = gen_mod:get_module_opt(Server, ?MODULE, access,
                                          fun(A) when is_atom(A) -> A end,
					  all),
	  AllowRemove = allow ==
			  acl:match_rule(Server, Access, From),
	  if (UTag /= false) and (RTag /= false) and
	       AllowRemove ->
		 User = xml:get_tag_cdata(UTag),
		 case From of
		   #jid{user = User, lserver = Server} ->
		       ejabberd_auth:remove_user(User, Server),
		       IQ#iq{type = result, sub_el = []};
		   _ ->
		       if PTag /= false ->
			      Password = xml:get_tag_cdata(PTag),
			      case ejabberd_auth:remove_user(User, Server,
							     Password)
				  of
				ok -> IQ#iq{type = result, sub_el = []};
				%% TODO FIXME: This piece of
				%% code does not work since
				%% the code have been changed
				%% to allow several auth
				%% modules.  lists:foreach can
				%% only return ok:
				not_allowed ->
				    IQ#iq{type = error,
					  sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
				not_exists ->
				    IQ#iq{type = error,
					  sub_el =
					      [SubEl, ?ERR_ITEM_NOT_FOUND]};
				_ ->
				    IQ#iq{type = error,
					  sub_el =
					      [SubEl,
					       ?ERR_INTERNAL_SERVER_ERROR]}
			      end;
			  true ->
			      IQ#iq{type = error,
				    sub_el = [SubEl, ?ERR_BAD_REQUEST]}
		       end
		 end;
	     (UTag == false) and (RTag /= false) and AllowRemove ->
		 case From of
		   #jid{user = User, lserver = Server,
			resource = Resource} ->
		       ResIQ = #iq{type = result, xmlns = ?NS_REGISTER,
				   id = ID, sub_el = []},
		       ejabberd_router:route(jid:make(User, Server,
							   Resource),
					     jid:make(User, Server,
							   Resource),
					     jlib:iq_to_xml(ResIQ)),
		       ejabberd_auth:remove_user(User, Server),
		       ignore;
		   _ ->
		       IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		 end;
	     (UTag /= false) and (PTag /= false) ->
		 User = xml:get_tag_cdata(UTag),
		 Password = xml:get_tag_cdata(PTag),
		 try_register_or_set_password(User, Server, Password,
					      From, IQ, SubEl, Source, Lang,
					      not IsCaptchaEnabled);
	     IsCaptchaEnabled ->
		 case ejabberd_captcha:process_reply(SubEl) of
		   ok ->
		       case process_xdata_submit(SubEl) of
			 {ok, User, Password} ->
			     try_register_or_set_password(User, Server,
							  Password, From, IQ,
							  SubEl, Source, Lang,
							  true);
			 _ ->
			     IQ#iq{type = error,
				   sub_el = [SubEl, ?ERR_BAD_REQUEST]}
		       end;
		   {error, malformed} ->
		       IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]};
		   _ ->
		       ErrText = <<"The CAPTCHA verification has failed">>,
		       IQ#iq{type = error,
			     sub_el = [SubEl, ?ERRT_NOT_ALLOWED(Lang, ErrText)]}
		 end;
	     true ->
		 IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
	  end;
      get ->
	  {IsRegistered, UsernameSubels, QuerySubels} = case From
							    of
							  #jid{user = User,
							       lserver =
								   Server} ->
							      case
								ejabberd_auth:is_user_exists(User,
											     Server)
								  of
								true ->
								    {true,
								     [{xmlcdata,
								       User}],
								     [#xmlel{name
										 =
										 <<"registered">>,
									     attrs
										 =
										 [],
									     children
										 =
										 []}]};
								false ->
								    {false,
								     [{xmlcdata,
								       User}],
								     []}
							      end;
							  _ -> {false, [], []}
							end,
	  if IsCaptchaEnabled and not IsRegistered ->
		 TopInstrEl = #xmlel{name = <<"instructions">>,
				     attrs = [],
				     children =
					 [{xmlcdata,
					   translate:translate(Lang,
							       <<"You need a client that supports x:data "
								 "and CAPTCHA to register">>)}]},
		 InstrEl = #xmlel{name = <<"instructions">>, attrs = [],
				  children =
				      [{xmlcdata,
					translate:translate(Lang,
							    <<"Choose a username and password to register "
							      "with this server">>)}]},
		 UField = #xmlel{name = <<"field">>,
				 attrs =
				     [{<<"type">>, <<"text-single">>},
				      {<<"label">>,
				       translate:translate(Lang, <<"User">>)},
				      {<<"var">>, <<"username">>}],
				 children =
				     [#xmlel{name = <<"required">>, attrs = [],
					     children = []}]},
		 PField = #xmlel{name = <<"field">>,
				 attrs =
				     [{<<"type">>, <<"text-private">>},
				      {<<"label">>,
				       translate:translate(Lang,
							   <<"Password">>)},
				      {<<"var">>, <<"password">>}],
				 children =
				     [#xmlel{name = <<"required">>, attrs = [],
					     children = []}]},
		 case ejabberd_captcha:create_captcha_x(ID, To, Lang,
							Source,
							[InstrEl, UField,
							 PField])
		     of
		   {ok, CaptchaEls} ->
		       IQ#iq{type = result,
			     sub_el =
				 [#xmlel{name = <<"query">>,
					 attrs =
					     [{<<"xmlns">>,
					       ?NS_REGISTER}],
					 children =
					     [TopInstrEl | CaptchaEls]}]};
		   {error, limit} ->
		       ErrText = <<"Too many CAPTCHA requests">>,
		       IQ#iq{type = error,
			     sub_el =
				 [SubEl,
				  ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)]};
		   _Err ->
		       ErrText = <<"Unable to generate a CAPTCHA">>,
		       IQ#iq{type = error,
			     sub_el =
				 [SubEl,
				  ?ERRT_INTERNAL_SERVER_ERROR(Lang, ErrText)]}
		 end;
	     true ->
		 IQ#iq{type = result,
		       sub_el =
			   [#xmlel{name = <<"query">>,
				   attrs =
				       [{<<"xmlns">>,
					 ?NS_REGISTER}],
				   children =
				       [#xmlel{name = <<"instructions">>,
					       attrs = [],
					       children =
						   [{xmlcdata,
						     translate:translate(Lang,
									 <<"Choose a username and password to register "
									   "with this server">>)}]},
					#xmlel{name = <<"username">>,
					       attrs = [],
					       children = UsernameSubels},
					#xmlel{name = <<"password">>,
					       attrs = [], children = []}
					| QuerySubels]}]}
	  end
    end.

try_register_or_set_password(User, Server, Password,
			     From, IQ, SubEl, Source, Lang, CaptchaSucceed) ->
    case From of
      #jid{user = User, lserver = Server} ->
	  try_set_password(User, Server, Password, IQ, SubEl,
			   Lang);
      _ when CaptchaSucceed ->
	  case check_from(From, Server) of
	    allow ->
		case try_register(User, Server, Password, Source, Lang)
		    of
		  ok -> IQ#iq{type = result, sub_el = []};
		  {error, Error} ->
		      IQ#iq{type = error, sub_el = [SubEl, Error]}
		end;
	    deny ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
	  end;
      _ ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.

%% @doc Try to change password and return IQ response
try_set_password(User, Server, Password, IQ, SubEl,
		 Lang) ->
    case is_strong_password(Server, Password) of
      true ->
	  case ejabberd_auth:set_password(User, Server, Password)
	      of
	    ok -> IQ#iq{type = result, sub_el = []};
	    {error, empty_password} ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]};
	    {error, not_allowed} ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	    {error, invalid_jid} ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
	    _ ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
	  end;
      false ->
	  ErrText = <<"The password is too weak">>,
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)]}
    end.

try_register(User, Server, Password, SourceRaw, Lang) ->
    case jid:is_nodename(User) of
      false -> {error, ?ERR_BAD_REQUEST};
      _ ->
	  JID = jid:make(User, Server, <<"">>),
	  Access = gen_mod:get_module_opt(Server, ?MODULE, access,
                                          fun(A) when is_atom(A) -> A end,
					  all),
	  IPAccess = get_ip_access(Server),
	  case {acl:match_rule(Server, Access, JID),
		check_ip_access(SourceRaw, IPAccess)}
	      of
	    {deny, _} -> {error, ?ERR_FORBIDDEN};
	    {_, deny} -> {error, ?ERR_FORBIDDEN};
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
				    {atomic, exists} -> {error, ?ERR_CONFLICT};
				    {error, invalid_jid} ->
					{error, ?ERR_JID_MALFORMED};
				    {error, not_allowed} ->
					{error, ?ERR_NOT_ALLOWED};
				    {error, too_many_users} ->
					{error, ?ERR_NOT_ALLOWED};
				    {error, _Reason} ->
					{error, ?ERR_INTERNAL_SERVER_ERROR}
				  end
			    end;
			false ->
			    ErrText = <<"The password is too weak">>,
			    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)}
		      end;
		  false ->
		      ErrText =
			  <<"Users are not allowed to register accounts "
			    "so quickly">>,
		      {error, ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)}
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
	  ejabberd_router:route(jid:make(<<"">>, Host,
					      <<"">>),
				JID,
				#xmlel{name = <<"message">>,
				       attrs = [{<<"type">>, <<"normal">>}],
				       children =
					   [#xmlel{name = <<"subject">>,
						   attrs = [],
						   children =
						       [{xmlcdata, Subj}]},
					    #xmlel{name = <<"body">>,
						   attrs = [],
						   children =
						       [{xmlcdata, Body}]}]});
      _ -> ok
    end.

send_registration_notifications(Mod, UJID, Source) ->
    Host = UJID#jid.lserver,
    case gen_mod:get_module_opt(
           Host, Mod, registration_watchers,
           fun(Ss) ->
                   [#jid{} = jid:from_string(iolist_to_binary(S))
                    || S <- Ss]
           end, []) of
        [] -> ok;
        JIDs when is_list(JIDs) ->
            Body =
                iolist_to_binary(io_lib:format("[~s] The account ~s was registered from "
                                               "IP address ~s on node ~w using ~p.",
                                               [get_time_string(),
                                                jid:to_string(UJID),
                                                ip_to_string(Source), node(),
                                                Mod])),
            lists:foreach(
              fun(JID) ->
                      ejabberd_router:route(
                        jid:make(<<"">>, Host, <<"">>),
                        JID,
                        #xmlel{name = <<"message">>,
                               attrs = [{<<"type">>, <<"chat">>}],
                               children = [#xmlel{name = <<"body">>,
                                                  attrs = [],
                                                  children = [{xmlcdata,Body}]}]})
              end, JIDs)
    end.

check_from(#jid{user = <<"">>, server = <<"">>},
	   _Server) ->
    allow;
check_from(JID, Server) ->
    Access = gen_mod:get_module_opt(Server, ?MODULE, access_from,
                                    fun(A) when is_atom(A) -> A end,
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
    jlib:ip_to_list(Source);
ip_to_string(undefined) -> <<"undefined">>;
ip_to_string(_) -> <<"unknown">>.

get_time_string() -> write_time(erlang:localtime()).
%% Function copied from ejabberd_logger_h.erl and customized

write_time({{Y, Mo, D}, {H, Mi, S}}) ->
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
		  [Y, Mo, D, H, Mi, S]).

process_xdata_submit(El) ->
    case xml:get_subtag(El, <<"x">>) of
      false -> error;
      Xdata ->
	  Fields = jlib:parse_xdata_submit(Xdata),
	  case catch {proplists:get_value(<<"username">>, Fields),
		      proplists:get_value(<<"password">>, Fields)}
	      of
	    {[User | _], [Pass | _]} -> {ok, User, Pass};
	    _ -> error
	  end
    end.

is_strong_password(Server, Password) ->
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
                         ACLName = jlib:binary_to_atom(
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

mod_opt_type(access) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(access_from) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(captcha_protected) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(ip_access) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(password_strength) ->
    fun (N) when is_number(N), N >= 0 -> N end;
mod_opt_type(registration_watchers) ->
    fun (Ss) ->
	    [#jid{} = jid:from_string(iolist_to_binary(S))
	     || S <- Ss]
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
