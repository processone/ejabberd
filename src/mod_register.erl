%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Inband registration support
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

-author('alexey@process-one.net').

-protocol({xep, 77, '2.4'}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, stream_feature_register/2,
	 c2s_unauthenticated_packet/2, try_register/4, try_register/5,
	 process_iq/1, send_registration_notifications/3,
	 mod_opt_type/1, mod_options/1, depends/2,
	 format_error/1, mod_doc/0]).

-deprecated({try_register, 4}).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

start(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, mod_register_ip,
			[{ram_copies, [node()]}, {local_content, true},
			 {attributes, [key, value]}]),
    {ok, [{iq_handler, ejabberd_local, ?NS_REGISTER, process_iq},
          {iq_handler, ejabberd_sm, ?NS_REGISTER, process_iq},
          {hook, c2s_pre_auth_features, stream_feature_register, 50},
          {hook, c2s_unauthenticated_packet, c2s_unauthenticated_packet, 50}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

-spec stream_feature_register([xmpp_element()], binary()) -> [xmpp_element()].
stream_feature_register(Acc, Host) ->
    case {mod_register_opt:access(Host),
	  mod_register_opt:ip_access(Host),
	  mod_register_opt:redirect_url(Host)} of
	{none, _, undefined} -> Acc;
	{_, none, undefined} -> Acc;
	{_, _, _} -> [#feature_register{}|Acc]
    end.

c2s_unauthenticated_packet(#{ip := IP, server := Server} = State,
			   #iq{type = T, sub_els = [_]} = IQ)
  when T == set; T == get ->
    try xmpp:try_subtag(IQ, #register{}) of
	#register{} = Register ->
	    {Address, _} = IP,
	    IQ1 = xmpp:set_els(IQ, [Register]),
	    IQ2 = xmpp:set_from_to(IQ1, jid:make(<<>>), jid:make(Server)),
	    ResIQ = process_iq(IQ2, Address),
	    ResIQ1 = xmpp:set_from_to(ResIQ, jid:make(Server), undefined),
	    {stop, ejabberd_c2s:send(State, ResIQ1)};
	false ->
	    State
    catch _:{xmpp_codec, Why} ->
	    Txt = xmpp:io_format_error(Why),
	    Lang = maps:get(lang, State),
	    Err = xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang)),
	    {stop, ejabberd_c2s:send(State, Err)}
    end;
c2s_unauthenticated_packet(State, _) ->
    State.

process_iq(#iq{from = From} = IQ) ->
    process_iq(IQ, jid:tolower(From)).

process_iq(#iq{from = From, to = To} = IQ, Source) ->
    IsCaptchaEnabled =
	case mod_register_opt:captcha_protected(To#jid.lserver) of
	    true -> true;
	    false -> false
	end,
    Server = To#jid.lserver,
    Access = mod_register_opt:access_remove(Server),
    Remove = case {acl:match_rule(Server, Access, From), From#jid.lserver} of
		 {allow, Server} ->
		     allow;
		 {_, _} ->
		     deny
	     end,
    process_iq(IQ, Source, IsCaptchaEnabled, Remove == allow).

process_iq(#iq{type = set, lang = Lang,
	       sub_els = [#register{remove = true}]} = IQ,
	   _Source, _IsCaptchaEnabled, _AllowRemove = false) ->
    Txt = ?T("Access denied by service policy"),
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
                    ResIQ = xmpp:make_iq_result(IQ),
                    ejabberd_router:route(ResIQ),
		    ejabberd_auth:remove_user(User, Server),
                    ignore;
		_ ->
		    if is_binary(Password) ->
                            case ejabberd_auth:check_password(
                                   User, <<"">>, Server, Password) of
                                true ->
                                    ResIQ = xmpp:make_iq_result(IQ),
                                    ejabberd_router:route(ResIQ),
                                    ejabberd_auth:remove_user(User, Server),
                                    ignore;
                                false ->
                                    Txt = ?T("Incorrect password"),
                                    xmpp:make_error(
                                      IQ, xmpp:err_forbidden(Txt, Lang))
                            end;
		       true ->
			    Txt = ?T("No 'password' found in this query"),
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
		    Txt = ?T("The query is only allowed from local users"),
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
    XdataC = xmpp_util:set_xdata_field(
           #xdata_field{
              var = <<"FORM_TYPE">>,
              type = hidden, values = [?NS_CAPTCHA]},
           X),
    case ejabberd_captcha:process_reply(XdataC) of
	ok ->
	    case process_xdata_submit(X) of
		{ok, User, Password} ->
		    try_register_or_set_password(
		      User, Server, Password, IQ, Source, true);
		_ ->
		    Txt = ?T("Incorrect data form"),
		    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
	    end;
	{error, malformed} ->
	    Txt = ?T("Incorrect CAPTCHA submit"),
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	_ ->
	    ErrText = ?T("The CAPTCHA verification has failed"),
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
		case ejabberd_auth:user_exists(User, Server) of
		    true ->
			{true, User};
		    false ->
			{false, User}
		end;
	    _ ->
		{false, <<"">>}
	end,
    Instr = translate:translate(
	      Lang, ?T("Choose a username and password to register "
		       "with this server")),
    URL = mod_register_opt:redirect_url(Server),
    if (URL /= undefined) and not IsRegistered ->
	    Desc = str:translate_and_format(Lang, ?T("To register, visit ~s"), [URL]),
	    xmpp:make_iq_result(
	      IQ, #register{instructions = Desc,
			    sub_els = [#oob_x{url = URL}]});
       IsCaptchaEnabled and not IsRegistered ->
	    TopInstr = translate:translate(
			 Lang, ?T("You need a client that supports x:data "
				  "and CAPTCHA to register")),
	    UField = #xdata_field{type = 'text-single',
				  label = translate:translate(Lang, ?T("User")),
				  var = <<"username">>,
				  required = true},
	    PField = #xdata_field{type = 'text-private',
				  label = translate:translate(Lang, ?T("Password")),
				  var = <<"password">>,
				  required = true},
	    X = #xdata{type = form, instructions = [Instr],
		       fields = [UField, PField]},
	    case ejabberd_captcha:create_captcha_x(ID, To, Lang, Source, X) of
		{ok, CaptchaEls} ->
                    {value, XdataC, CaptchaEls2} = lists:keytake(xdata, 1, CaptchaEls),
                    Xdata = xmpp_util:set_xdata_field(
                               #xdata_field{
                                  var = <<"FORM_TYPE">>,
                                  type = hidden, values = [?NS_REGISTER]},
                               XdataC),
		    xmpp:make_iq_result(
		      IQ, #register{instructions = TopInstr,
				    sub_els = [Xdata | CaptchaEls2]});
		{error, limit} ->
		    ErrText = ?T("Too many CAPTCHA requests"),
		    xmpp:make_error(
		      IQ, xmpp:err_resource_constraint(ErrText, Lang));
		_Err ->
		    ErrText = ?T("Unable to generate a CAPTCHA"),
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
		    case try_register(User, Server, Password, Source, ?MODULE, Lang) of
			ok ->
			    xmpp:make_iq_result(IQ);
			{error, Error} ->
			    xmpp:make_error(IQ, Error)
		    end;
		deny ->
		    Txt = ?T("Access denied by service policy"),
		    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang))
	    end;
	_ ->
	    xmpp:make_error(IQ, xmpp:err_not_allowed())
    end.

try_set_password(User, Server, Password) ->
    case is_strong_password(Server, Password) of
	true ->
	    ejabberd_auth:set_password(User, Server, Password);
	error_preparing_password ->
	    {error, invalid_password};
	false ->
	    {error, weak_password}
    end.

try_set_password(User, Server, Password, #iq{lang = Lang, meta = M} = IQ) ->
    case try_set_password(User, Server, Password) of
	ok ->
	    ?INFO_MSG("~ts has changed password from ~ts",
		      [jid:encode({User, Server, <<"">>}),
		       ejabberd_config:may_hide_data(
			 misc:ip_to_list(maps:get(ip, M, {0,0,0,0})))]),
	    xmpp:make_iq_result(IQ);
	{error, not_allowed} ->
	    Txt = ?T("Changing password is not allowed"),
	    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
	{error, invalid_jid = Why} ->
	    xmpp:make_error(IQ, xmpp:err_jid_malformed(format_error(Why), Lang));
	{error, invalid_password = Why} ->
	    xmpp:make_error(IQ, xmpp:err_not_allowed(format_error(Why), Lang));
	{error, weak_password = Why} ->
	    xmpp:make_error(IQ, xmpp:err_not_acceptable(format_error(Why), Lang));
	{error, db_failure = Why} ->
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(format_error(Why), Lang))
    end.

try_register(User, Server, Password, SourceRaw, Module) ->
    Modules = mod_register_opt:allow_modules(Server),
    case (Modules == all) orelse lists:member(Module, Modules) of
        true -> try_register(User, Server, Password, SourceRaw);
        false -> {error, eaccess}
    end.

try_register(User, Server, Password, SourceRaw) ->
    case jid:is_nodename(User) of
	false ->
	    {error, invalid_jid};
	true ->
	    case check_access(User, Server, SourceRaw) of
		deny ->
		    {error, eaccess};
		allow ->
		    Source = may_remove_resource(SourceRaw),
		    case check_timeout(Source) of
			true ->
			    case is_strong_password(Server, Password) of
				true ->
				    case ejabberd_auth:try_register(
					   User, Server, Password) of
					ok ->
					    ok;
					{error, _} = Err ->
					    remove_timeout(Source),
					    Err
				    end;
				false ->
				    remove_timeout(Source),
				    {error, weak_password};
				error_preparing_password ->
				    remove_timeout(Source),
				    {error, invalid_password}
			    end;
			false ->
			    {error, wait}
		    end
	    end
    end.

try_register(User, Server, Password, SourceRaw, Module, Lang) ->
    case try_register(User, Server, Password, SourceRaw, Module) of
	ok ->
	    JID = jid:make(User, Server),
	    Source = may_remove_resource(SourceRaw),
	    ?INFO_MSG("The account ~ts was registered from IP address ~ts",
		      [jid:encode({User, Server, <<"">>}),
		       ejabberd_config:may_hide_data(ip_to_string(Source))]),
	    send_welcome_message(JID),
	    send_registration_notifications(?MODULE, JID, Source);
	{error, invalid_jid = Why} ->
	    {error, xmpp:err_jid_malformed(format_error(Why), Lang)};
	{error, eaccess = Why} ->
	    {error, xmpp:err_forbidden(format_error(Why), Lang)};
	{error, wait = Why} ->
	    {error, xmpp:err_resource_constraint(format_error(Why), Lang)};
	{error, weak_password = Why} ->
	    {error, xmpp:err_not_acceptable(format_error(Why), Lang)};
	{error, invalid_password = Why} ->
	    {error, xmpp:err_not_acceptable(format_error(Why), Lang)};
	{error, not_allowed = Why} ->
	    {error, xmpp:err_not_allowed(format_error(Why), Lang)};
	{error, exists = Why} ->
	    {error, xmpp:err_conflict(format_error(Why), Lang)};
	{error, db_failure = Why} ->
	    {error, xmpp:err_internal_server_error(format_error(Why), Lang)}
    end.

format_error(invalid_jid) ->
    ?T("Malformed username");
format_error(eaccess) ->
    ?T("Access denied by service policy");
format_error(wait) ->
    ?T("Users are not allowed to register accounts so quickly");
format_error(weak_password) ->
    ?T("The password is too weak");
format_error(invalid_password) ->
    ?T("The password contains unacceptable characters");
format_error(not_allowed) ->
    ?T("Not allowed");
format_error(exists) ->
    ?T("User already exists");
format_error(db_failure) ->
    ?T("Database failure");
format_error(Unexpected) ->
    list_to_binary(io_lib:format(?T("Unexpected error condition: ~p"), [Unexpected])).

send_welcome_message(JID) ->
    Host = JID#jid.lserver,
    case mod_register_opt:welcome_message(Host) of
      {<<"">>, <<"">>} -> ok;
      {Subj, Body} ->
	  ejabberd_router:route(
	    #message{from = jid:make(Host),
		     to = JID,
		     type = chat,
		     subject = xmpp:mk_text(Subj),
		     body = xmpp:mk_text(<<Subj/binary, "\n\n", Body/binary>>)}),
	  ejabberd_router:route(
	    #message{from = jid:make(Host),
		     to = JID,
		     subject = xmpp:mk_text(Subj),
		     body = xmpp:mk_text(Body)})
    end.

send_registration_notifications(Mod, UJID, Source) ->
    Host = UJID#jid.lserver,
    case mod_register_opt:registration_watchers(Host) of
        [] -> ok;
        JIDs when is_list(JIDs) ->
            Body =
                (str:format("[~s] The account ~s was registered from "
                                               "IP address ~s on node ~w using ~p.",
                                               [get_time_string(),
                                                jid:encode(UJID),
						ejabberd_config:may_hide_data(
						  ip_to_string(Source)),
						node(), Mod])),
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
    Access = mod_register_opt:access_from(Server),
    acl:match_rule(Server, Access, JID).

check_timeout(undefined) -> true;
check_timeout(Source) ->
    Timeout = ejabberd_option:registration_timeout(),
    if is_integer(Timeout) ->
	   Priority = -erlang:system_time(millisecond),
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
		 ?ERROR_MSG("timeout check error: ~p~n", [Reason]),
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
    Timeout = ejabberd_option:registration_timeout(),
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
		 ?ERROR_MSG("Mod_register: timeout remove error: "
			    "~p~n",
			    [Reason]),
		 ok
	   end;
       true -> ok
    end.

ip_to_string({_, _, _} = USR) ->
    jid:encode(USR);
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
    case mod_register_opt:password_strength(LServer) of
        0 ->
            true;
        Entropy ->
            ejabberd_auth:entropy(Password) >= Entropy
    end.

%%%
%%% ip_access management
%%%

may_remove_resource({_, _, _} = From) ->
    jid:remove_resource(From);
may_remove_resource(From) -> From.

get_ip_access(Host) ->
    mod_register_opt:ip_access(Host).

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

check_access(User, Server, Source) ->
    JID = jid:make(User, Server),
    Access = mod_register_opt:access(Server),
    IPAccess = get_ip_access(Server),
    case acl:match_rule(Server, Access, JID) of
	allow -> check_ip_access(Source, IPAccess);
	deny -> deny
    end.

mod_opt_type(access) ->
    econf:acl();
mod_opt_type(access_from) ->
    econf:acl();
mod_opt_type(access_remove) ->
    econf:acl();
mod_opt_type(allow_modules) ->
    econf:either(all, econf:list(econf:atom()));
mod_opt_type(captcha_protected) ->
    econf:bool();
mod_opt_type(ip_access) ->
    econf:acl();
mod_opt_type(password_strength) ->
    econf:number(0);
mod_opt_type(registration_watchers) ->
    econf:list(econf:jid());
mod_opt_type(welcome_message) ->
    econf:and_then(
      econf:options(
	#{subject => econf:binary(),
	  body => econf:binary()}),
      fun(Opts) ->
	      {proplists:get_value(subject, Opts, <<>>),
	       proplists:get_value(body, Opts, <<>>)}
      end);
mod_opt_type(redirect_url) ->
    econf:url().

-spec mod_options(binary()) -> [{welcome_message, {binary(), binary()}} |
				{atom(), term()}].
mod_options(_Host) ->
    [{access, all},
     {access_from, none},
     {access_remove, all},
     {allow_modules, all},
     {captcha_protected, false},
     {ip_access, all},
     {password_strength, 0},
     {registration_watchers, []},
     {redirect_url, undefined},
     {welcome_message, {<<>>, <<>>}}].

mod_doc() ->
    #{desc =>
          [?T("This module adds support for https://xmpp.org/extensions/xep-0077.html"
              "[XEP-0077: In-Band Registration]. "
              "This protocol enables end users to use an XMPP client to:"), "",
           ?T("* Register a new account on the server."), "",
           ?T("* Change the password from an existing account on the server."), "",
           ?T("* Delete an existing account on the server."), "",
           ?T("This module reads also the top-level _`registration_timeout`_ "
              "option defined globally for the server, "
              "so please check that option documentation too.")],
      opts =>
          [{access,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("Specify rules to restrict what usernames can be registered. "
                     "If a rule returns 'deny' on the requested username, "
                     "registration of that user name is denied. There are no "
                     "restrictions by default.")}},
           {access_from,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("By default, 'ejabberd' doesn't allow the client to register new accounts "
                     "from s2s or existing c2s sessions. You can change it by defining "
                     "access rule in this option. Use with care: allowing registration "
                     "from s2s leads to uncontrolled massive accounts creation by rogue users.")}},
           {access_remove,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("Specify rules to restrict access for user unregistration. "
                     "By default any user is able to unregister their account.")}},
           {allow_modules,
            #{value => "all | [Module, ...]",
              note => "added in 21.12",
              desc =>
                  ?T("List of modules that can register accounts, or 'all'. "
                     "The default value is 'all', which is equivalent to "
                     "something like '[mod_register, mod_register_web]'.")}},
           {captcha_protected,
            #{value => "true | false",
              desc =>
                  ?T("Protect registrations with _`basic.md#captcha|CAPTCHA`_. "
                     "The default is 'false'.")}},
           {ip_access,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("Define rules to allow or deny account registration depending "
                     "on the IP address of the XMPP client. The 'AccessName' should "
                     "be of type 'ip'. The default value is 'all'.")}},
           {password_strength,
            #{value => "Entropy",
              desc =>
                  ?T("This option sets the minimum "
                     "https://en.wikipedia.org/wiki/Entropy_(information_theory)"
                     "[Shannon entropy] for passwords. The value 'Entropy' is a "
                     "number of bits of entropy. The recommended minimum is 32 bits. "
                     "The default is '0', i.e. no checks are performed.")}},
           {registration_watchers,
            #{value => "[JID, ...]",
              desc =>
                  ?T("This option defines a list of JIDs which will be notified each "
                     "time a new account is registered.")}},
           {redirect_url,
            #{value => ?T("URL"),
              desc =>
                  ?T("This option enables registration redirection as described in "
                     "https://xmpp.org/extensions/xep-0077.html#redirect"
                     "[XEP-0077: In-Band Registration: Redirection].")}},
           {welcome_message,
            #{value => "{subject: Subject, body: Body}",
              desc =>
                  ?T("Set a welcome message that is sent to each newly registered account. "
                     "The message will have subject 'Subject' and text 'Body'."),
              example =>
                    ["modules:",
                     "  mod_register:",
                     "    welcome_message:",
                     "      subject: \"Welcome!\"",
                     "      body: |-",
                     "        Hi!",
                     "        Welcome to this XMPP server"]}}
          ]}.
