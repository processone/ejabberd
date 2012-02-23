%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Inband registration support
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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

-module(mod_register).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 stream_feature_register/2,
	 unauthenticated_iq_register/4,
	 try_register/5,
	 process_iq/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_INBAND_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_INBAND_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_stream_features, HostB,
 		       ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_iq, HostB,
 		       ?MODULE, unauthenticated_iq_register, 50),
    mnesia:create_table(mod_register_ip,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, [key, value]}]),
    mnesia:add_table_copy(mod_register_ip, node(), ram_copies),
    ok.

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(c2s_stream_features, HostB,
 			  ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_iq, HostB,
			  ?MODULE, unauthenticated_iq_register, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_INBAND_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_INBAND_REGISTER).


stream_feature_register(Acc, _Host) ->
    [#xmlel{ns = ?NS_INBAND_REGISTER_FEAT, name = 'register'} | Acc].

unauthenticated_iq_register(_Acc,
			    Server, #iq{ns = ?NS_INBAND_REGISTER} = IQ_Rec, IP) ->
    Address = case IP of
		 {A, _Port} -> A;
		  _ -> undefined
	      end,
    BareJID = exmpp_jid:make(Server),
    ResIQ = process_iq(exmpp_jid:make(),
 		       BareJID,
 		       IQ_Rec,
		       Address),
    exmpp_iq:iq_to_xmlel(ResIQ, BareJID, exmpp_jid:make());

unauthenticated_iq_register(Acc, _Server, _IQ, _IP) ->
    Acc.

process_iq(From, To, IQ) ->
    process_iq(From, To, IQ, {exmpp_jid:prep_node_as_list(From),
                              exmpp_jid:prep_domain_as_list(From),
                              exmpp_jid:prep_resource_as_list(From)}).

process_iq(From, To,
	   #iq{type = Type, lang = Lang, payload = SubEl, id = ID} = IQ_Rec,
	   Source) ->
    IsCaptchaEnabled = case gen_mod:get_module_opt(
			      exmpp_jid:domain_as_list(To), ?MODULE, captcha_protected, false) of
			   true ->
			       true;
			   _ ->
			       false
		       end,
    case Type of
	set ->
	    UTag = exmpp_xml:get_element(SubEl, 'username'),
	    PTag = exmpp_xml:get_element(SubEl, 'password'),
	    RTag = exmpp_xml:get_element(SubEl, 'remove'),
	    Server = exmpp_jid:prep_domain_as_list(To),
	    Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
	    AllowRemove = (allow == acl:match_rule(Server, Access, From)),
	    if
		(UTag /= undefined) and (RTag /= undefined) and AllowRemove ->
		    User = exmpp_xml:get_cdata_as_list(UTag),
		    case {exmpp_jid:node_as_list(From), exmpp_jid:prep_domain_as_list(From)} of
		    {User, Server} ->
			    ejabberd_auth:remove_user(User, Server),
                            exmpp_iq:result(IQ_Rec, SubEl);
			_ ->
			    if
				PTag /= undefined ->
				    Password = exmpp_xml:get_cdata_as_list(PTag),
				    case ejabberd_auth:remove_user(User,
								   Server,
								   Password) of
					ok ->
                                            exmpp_iq:result(IQ_Rec, SubEl);
					%% TODO FIXME: This piece of
					%% code does not work since
					%% the code have been changed
					%% to allow several auth
					%% modules.  lists:foreach can
					%% only return ok:
					not_allowed ->
                                            exmpp_iq:error(IQ_Rec,
                                              'not-allowed');
					not_exists ->
                                            exmpp_iq:error(IQ_Rec,
                                              'item-not-found');
					_ ->
                                            exmpp_iq:error(IQ_Rec,
                                              'internal-server-error')
				    end;
				true ->
                                    exmpp_iq:error(IQ_Rec, 'bad-request')
			    end
		    end;
		(UTag == undefined) and (RTag /= undefined) and AllowRemove ->
		    case {exmpp_jid:node_as_list(From),
                  exmpp_jid:prep_domain_as_list(From),
                  exmpp_jid:resource_as_list(From)}of
			{User, Server, Resource} ->
			    ResIQ = exmpp_iq:result(IQ_Rec, SubEl),
			    ejabberd_router:route(
			      exmpp_jid:make(User, 
                                     Server, 
                                     Resource),
			      exmpp_jid:make(User, 
                                     Server, 
                                     Resource),
			      exmpp_iq:iq_to_xmlel(ResIQ)),
			    ejabberd_auth:remove_user(User, Server),
			    ignore;
			_ ->
                            exmpp_iq:error(IQ_Rec, 'not-allowed')
		    end;
		(UTag /= undefined) and (PTag /= undefined) ->
		    User = exmpp_xml:get_cdata_as_list(UTag),
		    Password = exmpp_xml:get_cdata_as_list(PTag),
		    try_register_or_set_password(
		      User, Server, Password, From,
		      IQ_Rec, SubEl, Source, Lang, not IsCaptchaEnabled);
		IsCaptchaEnabled ->
		    case ejabberd_captcha:process_reply(SubEl) of
			ok ->
			    case process_xdata_submit(SubEl) of
				{ok, User, Password} ->
				    try_register_or_set_password(
				      User, Server, Password, From,
				      IQ_Rec, SubEl, Source, Lang, true);
				_ ->
				    exmpp_iq:error(IQ_Rec, 'bad-request')
			    end;
			{error, malformed} ->
			    exmpp_iq:error(IQ_Rec, 'bad-request');
			_ ->
			    ErrText = translate:translate(Lang, "Captcha test failed"),
			    exmpp_iq:error(IQ_Rec, 'not-allowed', ErrText)
			end;
		true ->
                    exmpp_iq:error(IQ_Rec, 'bad-request')
	    end;
	get ->
	    {IsRegistered, UsernameSubels, QuerySubels} =
		case {exmpp_jid:node_as_list(From), exmpp_jid:prep_domain_as_list(From)} of
		    {User, Server} when is_list(User) and is_list(Server) ->
			case ejabberd_auth:is_user_exists(User,Server) of
			    true ->
				{true, [#xmlcdata{cdata = list_to_binary(User)}],
				 [#xmlel{ns = ?NS_INBAND_REGISTER, name = 'registered'}]};
			    false ->
				{false, [#xmlcdata{cdata = list_to_binary(User)}], []}
			end;
		    _ ->
			{false, [], []}
		end,
	    if IsCaptchaEnabled and not IsRegistered ->
		    TopInstrEl =
			#xmlel{ns = ?NS_INBAND_REGISTER, name = 'instructions',
			       children =
				   [#xmlcdata{cdata =
						  list_to_binary(
						    translate:translate(Lang,
				      "You need an x:data capable client "
				      "with CAPTCHA support to register"))}]},
		    InstrEl =
			#xmlel{ns = ?NS_INBAND_REGISTER, name = 'instructions',
			       children =
				   [#xmlcdata{cdata =
						  list_to_binary(
						    translate:translate(Lang,
									"Choose a username and password "
									"to register with this server"))}]},
		    UField = #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
					[?XMLATTR(<<"var">>, <<"username">>),
					 ?XMLATTR(<<"type">>, <<"text-single">>),
					 ?XMLATTR(<<"label">>, translate:translate(Lang, "User"))],
				    children =
					[#xmlel{ns = ?NS_DATA_FORMS, name = 'required'}]},
		    PField =
			#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
				   [?XMLATTR(<<"var">>, <<"password">>),
				    ?XMLATTR(<<"type">>, <<"text-private">>),
				    ?XMLATTR(<<"label">>, translate:translate(Lang, "Password"))],
			       children = [
					   #xmlel{ns = ?NS_DATA_FORMS, name = 'required'}
					  ]
			      },
		    case ejabberd_captcha:create_captcha_x(
			   ID, To, Lang, [InstrEl, UField, PField]) of
			{ok, CaptchaEls} ->
				Result = #xmlel{ns = ?NS_INBAND_REGISTER, name = 'query', children = [TopInstrEl | CaptchaEls]},
				exmpp_iq:result(IQ_Rec, Result);
			error ->
			    ErrText = translate:translate(Lang, "Unable to generate a captcha"),
			    exmpp_iq:error(IQ_Rec, 'internal-server-error', ErrText)
		    end;
	       true ->
            Result = #xmlel{ns = ?NS_INBAND_REGISTER, name = 'query', children =
				[#xmlel{ns = ?NS_INBAND_REGISTER, name = 'instructions', children =
					    [#xmlcdata{cdata = list_to_binary(
								 translate:translate(Lang,
										     "Choose a username and password "
										     "to register with this server"))}]},
				 #xmlel{ns = ?NS_INBAND_REGISTER, name = 'username', children = UsernameSubels},
				 #xmlel{ns = ?NS_INBAND_REGISTER, name = 'password'}
				 | QuerySubels]},
            exmpp_iq:result(IQ_Rec, Result)
	    end
    end.

try_register_or_set_password(User, Server, Password, From, IQ_Rec,
			     SubEl, Source, Lang, CaptchaSucceed) ->
    case {exmpp_jid:node_as_list(From), exmpp_jid:prep_domain_as_list(From)} of
	{User, Server} ->
	    try_set_password(User, Server, Password, IQ_Rec, SubEl, Lang);
	_ when CaptchaSucceed ->
	    case check_from(From, Server) of
		allow ->
		    case try_register(User, Server, Password,
				      Source, Lang) of
			ok ->
			    exmpp_iq:result(IQ_Rec, SubEl);
			{error, Error} ->
			    exmpp_iq:error(IQ_Rec, Error)
		    end;
		deny ->
		    exmpp_iq:error(IQ_Rec, 'forbidden')
	    end;
	_ ->
	    exmpp_iq:error(IQ_Rec, 'not-allowed')
    end.

%% @doc Try to change password and return IQ response
try_set_password(User, Server, Password, IQ_Rec, SubEl, Lang) ->
    case is_strong_password(Server, Password) of
	true ->
		try_set_password_strong(User, Server, Password, IQ_Rec, SubEl, Lang);
	false ->
		ErrText = translate:translate(Lang, "The password is too weak"),
		exmpp_iq:error(IQ_Rec, 'not-acceptable', ErrText)
    end.

try_set_password_strong(User, Server, Password, IQ_Rec, SubEl, _Lang) ->
    case ejabberd_auth:set_password(User, Server, Password) of
	ok ->
            exmpp_iq:result(IQ_Rec, SubEl);
	{error, empty_password} ->
            exmpp_iq:error(IQ_Rec, 'bad-request');
	{error, not_allowed} ->
            exmpp_iq:error(IQ_Rec, 'not-allowed');
	{error, invalid_jid} ->
            exmpp_iq:error(IQ_Rec, 'item-not-found');
	_ ->
            exmpp_iq:error(IQ_Rec, 'internal-server-error')
    end.

try_register_strong(User, Server, Password, Source, _Lang, JID) ->
			    case ejabberd_auth:try_register(User, Server, Password) of
				{atomic, ok} ->
				    send_welcome_message(JID),
				    send_registration_notifications(JID, Source),
				    ok;
				Error ->
				    remove_timeout(Source),
				    case Error of
					{atomic, exists} ->
					    {error, 'conflict'};
					{error, invalid_jid} ->
					    {error, 'jid-malformed'};
					{error, not_allowed} ->
					    {error, 'not-allowed'};
					{error, _Reason} ->
					    {error, 'internal-server-error'}
				    end
				    end.

try_register(User, Server, Password, SourceRaw, Lang) ->
    case exmpp_stringprep:is_node(User) of
	false ->
	    {error, 'bad-request'};
	_ ->
	    JID = exmpp_jid:make(User, Server),
	    Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
	    IPAccess = get_ip_access(Server),
		case {acl:match_rule(Server, Access, JID), 
		  check_ip_access(SourceRaw, IPAccess)} of
		{deny, _} ->
		    {error, 'forbidden'};
		{_, deny} ->
		    {error, 'forbidden'};
		{allow, allow} ->
		    Source = may_remove_resource(SourceRaw),
		    case check_timeout(Source) of
			true ->
			    case is_strong_password(Server, Password) of
				true ->
					try_register_strong(User, Server, Password, Source, Lang, JID);
				false ->
				    ErrText = "The password is too weak",
				    {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'not-acceptable', {Lang, ErrText})}
			    end;
			false ->
			    ErrText = "Users are not allowed to register "
				"accounts so quickly",
                            {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'resource-constraint', {Lang, ErrText})}
		    end
	    end
    end.


send_welcome_message(JID) ->
    Host = exmpp_jid:prep_domain_as_list(JID),
    case gen_mod:get_module_opt(Host, ?MODULE, welcome_message, {"", ""}) of
	{"", ""} ->
	    ok;
	{Subj, Body} ->
	    ejabberd_router:route(
	      exmpp_jid:make(Host),
	      JID,
              exmpp_message:normal(Subj, Body));
	_ ->
	    ok
    end.

send_registration_notifications(UJID, Source) ->
    Host = exmpp_jid:prep_domain_as_list(UJID),
    case gen_mod:get_module_opt(Host, ?MODULE, registration_watchers, []) of
	[] -> ok;
	JIDs when is_list(JIDs) ->
	    Body = lists:flatten(
		     io_lib:format(
		       "[~s] The account ~s was registered from IP address ~s "
		       "on node ~w using ~p.",
		       [get_time_string(), exmpp_jid:to_list(UJID),
			ip_to_string(Source), node(), ?MODULE])),
	    lists:foreach(
	      fun(S) ->
                      try
                          JID = exmpp_jid:parse(S),
                          ejabberd_router:route(
                            exmpp_jid:make(Host),
                            JID,
                            exmpp_message:chat(Body))
                      catch
                          _ ->
                              ok
		      end
	      end, JIDs);
	_ ->
	    ok
    end.

check_from(JID, Server) ->
    case {exmpp_jid:node(JID), exmpp_jid:prep_domain(JID)} of
	{undefined, undefined} ->
	    allow;
	_ ->
	    Access = gen_mod:get_module_opt(Server, ?MODULE, access_from, none),
	    acl:match_rule(Server, Access, JID)
    end.

check_timeout(undefined) ->
    true;
check_timeout(Source) ->
    Timeout = case ejabberd_config:get_local_option(registration_timeout) of
		  undefined -> 600;
		  TO -> TO
	      end,
    if
	is_integer(Timeout) ->
	    {MSec, Sec, _USec} = now(),
	    Priority = -(MSec * 1000000 + Sec),
	    CleanPriority = Priority + Timeout,
	    F = fun() ->
			Treap = case mnesia:read(mod_register_ip, treap,
						 write) of
				    [] ->
					treap:empty();
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
		{atomic, Res} ->
		    Res;
		{aborted, Reason} ->
		    ?ERROR_MSG("mod_register: timeout check error: ~p~n",
			       [Reason]),
		    true
	    end;
	true ->
	    true
    end.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
	true ->
	    Treap;
	false ->
	    {_Key, Priority, _Value} = treap:get_root(Treap),
	    if
		Priority > CleanPriority ->
		    clean_treap(treap:delete_root(Treap), CleanPriority);
		true ->
		    Treap
	    end
    end.

remove_timeout(undefined) ->
    true;
remove_timeout(Source) ->
    Timeout = case ejabberd_config:get_local_option(registration_timeout) of
		  undefined -> 600;
		  TO -> TO
	      end,
    if
	is_integer(Timeout) ->
	    F = fun() ->
			Treap = case mnesia:read(mod_register_ip, treap,
						 write) of
				    [] ->
					treap:empty();
				    [{mod_register_ip, treap, T}] -> T
				end,
			Treap1 = treap:delete(Source, Treap),
			mnesia:write({mod_register_ip, treap, Treap1}),
			ok
		end,
	    case mnesia:transaction(F) of
		{atomic, ok} ->
		    ok;
		{aborted, Reason} ->
		    ?ERROR_MSG("mod_register: timeout remove error: ~p~n",
			       [Reason]),
		    ok
	    end;
	true ->
	    ok
    end.

ip_to_string(Source) when is_tuple(Source) -> inet_parse:ntoa(Source);
ip_to_string(undefined) -> "undefined";
ip_to_string(_) -> "unknown".

get_time_string() -> write_time(erlang:localtime()).
%% Function copied from ejabberd_logger_h.erl and customized
write_time({{Y,Mo,D},{H,Mi,S}}) ->
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
		  [Y, Mo, D, H, Mi, S]).

process_xdata_submit(El) ->
    case exmpp_xml:get_element(El, x) of
        #xmlel{} = Xdata ->
            Fields = jlib:parse_xdata_submit(Xdata),
            case catch {proplists:get_value("username", Fields),
			proplists:get_value("password", Fields)} of
                {[User|_], [Pass|_]} ->
		    {ok, User, Pass};
		_ ->
		    error
	    end;
        _ ->
	    error
    end.

is_strong_password(Server, Password) ->
    LServer = exmpp_stringprep:nameprep(Server),
    case gen_mod:get_module_opt(LServer, ?MODULE, password_strength, 0) of
	Entropy when is_number(Entropy), Entropy >= 0 ->
	    if Entropy == 0 ->
		    true;
	       true ->
		    ejabberd_auth:entropy(Password) >= Entropy
	    end;
	Wrong ->
	    ?WARNING_MSG("Wrong value for password_strength option: ~p",
			 [Wrong]),
	    true
    end.

%%%
%%% ip_access management
%%%

may_remove_resource({U, S, _}) ->
	{U, S, ""};
may_remove_resource(From) ->
    From.

get_ip_access(Host) ->
    IPAccess = gen_mod:get_module_opt(Host, ?MODULE, ip_access, []),
    lists:flatmap(
      fun({Access, S}) ->
	      case parse_ip_netmask(S) of
		  {ok, IP, Mask} ->
		      [{Access, IP, Mask}];
		  error ->
		      ?ERROR_MSG("mod_register: invalid "
				 "network specification: ~p",
				 [S]),
		      []
	      end
      end, IPAccess).

parse_ip_netmask(S) ->
    case string:tokens(S, "/") of
	[IPStr] ->
	    case inet_parse:address(IPStr) of
		{ok, {_, _, _, _} = IP} ->
		    {ok, IP, 32};
		{ok, {_, _, _, _, _, _, _, _} = IP} ->
		    {ok, IP, 128};
		_ ->
		    error
	    end;
	[IPStr, MaskStr] ->
	    case catch list_to_integer(MaskStr) of
		Mask when is_integer(Mask),
			  Mask >= 0 ->
		    case inet_parse:address(IPStr) of
			{ok, {_, _, _, _} = IP} when Mask =< 32 ->
			    {ok, IP, Mask};
			{ok, {_, _, _, _, _, _, _, _} = IP} when Mask =< 128 ->
			    {ok, IP, Mask};
			_ ->
			    error
		    end;
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

check_ip_access(_Source, []) ->
    allow;
check_ip_access({User, Server, Resource}, IPAccess) ->
    JID = exmpp_jid:make(User, Server, Resource),
    case ejabberd_sm:get_user_ip(JID) of
	{IPAddress, _PortNumber} -> check_ip_access(IPAddress, IPAccess);
	_ -> true
    end;
check_ip_access({_, _, _, _} = IP,
		[{Access, {_, _, _, _} = Net, Mask} | IPAccess]) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot ((1 bsl (32 - Mask)) - 1),
    if
	IPInt band M =:= NetInt band M ->
	    Access;
	true ->
	    check_ip_access(IP, IPAccess)
    end;
check_ip_access({_, _, _, _, _, _, _, _} = IP,
		[{Access, {_, _, _, _, _, _, _, _} = Net, Mask} | IPAccess]) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot ((1 bsl (128 - Mask)) - 1),
    if
	IPInt band M =:= NetInt band M ->
	    Access;
	true ->
	    check_ip_access(IP, IPAccess)
    end;
check_ip_access(IP, [_ | IPAccess]) ->
    check_ip_access(IP, IPAccess).

ip_to_integer({IP1, IP2, IP3, IP4}) ->
    (((((IP1 bsl 8) bor IP2) bsl 8) bor IP3) bsl 8) bor IP4;
ip_to_integer({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}) ->
    (((((((((((((IP1 bsl 16) bor IP2) bsl 16) bor IP3) bsl 16) bor IP4)
	   bsl 16) bor IP5) bsl 16) bor IP6) bsl 16) bor IP7) bsl 16) bor IP8.
