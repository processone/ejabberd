%%%----------------------------------------------------------------------
%%% File    : mod_irc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : IRC transport
%%% Created : 15 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_irc).

-author('alexey@process-one.net').

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3, export/1, import/1,
	 import/3, closed_connection/3, get_connection_params/3,
	 data_to_binary/2, process_disco_info/1, process_disco_items/1,
	 process_register/1, process_vcard/1, process_command/1]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include("mod_irc.hrl").

-define(DEFAULT_IRC_ENCODING, <<"iso8859-15">>).

-define(DEFAULT_IRC_PORT, 6667).

-define(DEFAULT_REALNAME, <<"WebIRC-User">>).

-define(DEFAULT_WEBIRC_PASSWORD, <<"">>).

-define(POSSIBLE_ENCODINGS,
	[<<"koi8-r">>, <<"iso8859-15">>, <<"iso8859-1">>, <<"iso8859-2">>,
	 <<"utf-8">>, <<"utf-8+latin-1">>]).

-record(state, {host = <<"">>        :: binary(),
                server_host = <<"">> :: binary(),
                access = all         :: atom()}).

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #irc_custom{}) -> ok | pass.
-callback get_data(binary(), binary(), jid()) -> error | empty | irc_data().
-callback set_data(binary(), binary(), jid(), irc_data()) -> {atomic, any()}.

%%====================================================================
%% gen_mod API
%%====================================================================
start(Host, Opts) ->
    start_supervisor(Host),
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    stop_supervisor(Host),
    gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, OldOpts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {reload, Host, NewOpts, OldOpts}).

depends(_Host, _Opts) ->
    [].

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    ejabberd:start_app(iconv),
    MyHost = gen_mod:get_opt_host(Host, Opts,
				  <<"irc.@HOST@">>),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    Access = gen_mod:get_opt(access, Opts,
                             fun acl:access_rules_validator/1,
                             all),
    catch ets:new(irc_connection,
		  [named_table, public,
		   {keypos, #irc_connection.jid_server_host}]),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    register_hooks(MyHost, IQDisc),
    ejabberd_router:register_route(MyHost, Host),
    {ok,
     #state{host = MyHost, server_host = Host,
	    access = Access}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({reload, ServerHost, NewOpts, OldOpts}, State) ->
    NewHost = gen_mod:get_opt_host(ServerHost, NewOpts, <<"irc.@HOST@">>),
    OldHost = gen_mod:get_opt_host(ServerHost, OldOpts, <<"irc.@HOST@">>),
    NewIQDisc = gen_mod:get_opt(iqdisc, NewOpts,
				fun gen_iq_handler:check_type/1,
				one_queue),
    OldIQDisc = gen_mod:get_opt(iqdisc, OldOpts,
				fun gen_iq_handler:check_type/1,
				one_queue),
    NewMod = gen_mod:db_mod(ServerHost, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(ServerHost, OldOpts, ?MODULE),
    Access = gen_mod:get_opt(access, NewOpts,
                             fun acl:access_rules_validator/1,
                             all),
    if NewMod /= OldMod ->
	    NewMod:init(ServerHost, NewOpts);
       true ->
	    ok
    end,
    if (NewIQDisc /= OldIQDisc) or (NewHost /= OldHost) ->
	    register_hooks(NewHost, NewIQDisc);
       true ->
	    ok
    end,
    if NewHost /= OldHost ->
	    ejabberd_router:register_route(NewHost, ServerHost),
	    ejabberd_router:unregister_route(OldHost),
	    unregister_hooks(OldHost);
       true ->
	    ok
    end,
    Access = gen_mod:get_opt(access, NewOpts,
                             fun acl:access_rules_validator/1,
                             all),
    {noreply, State#state{host = NewHost, access = Access}};
handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, Packet},
	    #state{host = Host, server_host = ServerHost,
		   access = Access} =
		State) ->
    case catch do_route(Host, ServerHost, Access, Packet) of
      {'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]);
      _ -> ok
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{host = MyHost}) ->
    ejabberd_router:unregister_route(MyHost),
    unregister_hooks(MyHost).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
register_hooks(Host, IQDisc) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
				  ?MODULE, process_disco_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
				  ?MODULE, process_disco_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_REGISTER,
				  ?MODULE, process_register, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
				  ?MODULE, process_vcard, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_COMMANDS,
				  ?MODULE, process_command, IQDisc).

unregister_hooks(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_COMMANDS).

start_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host,
				   ejabberd_mod_irc_sup),
    ChildSpec = {Proc,
		 {ejabberd_tmp_sup, start_link,
		  [Proc, mod_irc_connection]},
		 permanent, infinity, supervisor, [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_gen_mod_sup, ChildSpec).

stop_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host,
				   ejabberd_mod_irc_sup),
    supervisor:terminate_child(ejabberd_gen_mod_sup, Proc),
    supervisor:delete_child(ejabberd_gen_mod_sup, Proc).

do_route(Host, ServerHost, Access, Packet) ->
    #jid{luser = LUser, lresource = LResource} = xmpp:get_to(Packet),
    From = xmpp:get_from(Packet),
    case acl:match_rule(ServerHost, Access, From) of
	allow ->
	    case Packet of
		#iq{} when LUser == <<"">>, LResource == <<"">> ->
		    ejabberd_router:process_iq(Packet);
		#iq{} when LUser == <<"">>, LResource /= <<"">> ->
		    Err = xmpp:err_service_unavailable(),
		    ejabberd_router:route_error(Packet, Err);
		_ ->
		    sm_route(Host, ServerHost, Packet)
	    end;
	deny ->
	    Lang = xmpp:get_lang(Packet),
	    Err = xmpp:err_forbidden(<<"Denied by ACL">>, Lang),
	    ejabberd_router:route_error(Packet, Err)
    end.

process_disco_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_info(#iq{type = get, lang = Lang, to = To,
		       sub_els = [#disco_info{node = Node}]} = IQ) ->
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    Info = ejabberd_hooks:run_fold(disco_info, ServerHost,
				   [], [ServerHost, ?MODULE, <<"">>, <<"">>]),
    case iq_disco(ServerHost, Node, Lang) of
	undefined ->
	    xmpp:make_iq_result(IQ, #disco_info{});
	DiscoInfo ->
	    xmpp:make_iq_result(IQ, DiscoInfo#disco_info{xdata = Info})
    end.

process_disco_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get, lang = Lang, to = To,
			sub_els = [#disco_items{node = Node}]} = IQ) ->
    case Node of
	<<"">> ->
	    xmpp:make_iq_result(IQ, #disco_items{});
	<<"join">> ->
	    xmpp:make_iq_result(IQ, #disco_items{});
	<<"register">> ->
	    xmpp:make_iq_result(IQ, #disco_items{});
	?NS_COMMANDS ->
	    Host = To#jid.lserver,
	    ServerHost = ejabberd_router:host_of_route(Host),
	    xmpp:make_iq_result(
	      IQ, #disco_items{node = Node,
			       items = command_items(ServerHost, Host, Lang)});
	_ ->
	    Txt = <<"Node not found">>,
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang))
    end.

process_register(#iq{type = get, to = To, from = From, lang = Lang} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    case get_form(ServerHost, Host, From, Lang) of
	{result, Res} ->
	    xmpp:make_iq_result(IQ, Res);
	{error, Error} ->
	    xmpp:make_error(IQ, Error)
    end;
process_register(#iq{type = set, lang = Lang, to = To, from = From,
		     sub_els = [#register{xdata = #xdata{} = X}]} = IQ) ->
    case X#xdata.type of
	cancel ->
	    xmpp:make_iq_result(IQ, #register{});
	submit ->
	    Host = To#jid.lserver,
	    ServerHost = ejabberd_router:host_of_route(Host),
	    case set_form(ServerHost, Host, From, Lang, X) of
		{result, Res} ->
		    xmpp:make_iq_result(IQ, Res);
		{error, Error} ->
		    xmpp:make_error(IQ, Error)
	    end;
	_ ->
	    Txt = <<"Incorrect value of 'type' attribute">>,
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end;
process_register(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"No data form found">>,
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang)).

process_vcard(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_vcard(#iq{type = get, lang = Lang} = IQ) ->
    xmpp:make_iq_result(IQ, iq_get_vcard(Lang)).

process_command(#iq{type = get, lang = Lang} = IQ) ->
    Txt = <<"Value 'get' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_command(#iq{type = set, lang = Lang, to = To, from = From,
		    sub_els = [#adhoc_command{node = Node} = Request]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    case lists:keyfind(Node, 1, commands(ServerHost)) of
	{_, _, Function} ->
	    try Function(From, To, Request) of
		ignore ->
		    ignore;
		{error, Error} ->
		    xmpp:make_error(IQ, Error);
		Command ->
		    xmpp:make_iq_result(IQ, Command)
	    catch E:R ->
		    ?ERROR_MSG("ad-hoc handler failed: ~p",
			       [{E, {R, erlang:get_stacktrace()}}]),
		    xmpp:make_error(IQ, xmpp:err_internal_server_error())
	    end;
	_ ->
	    Txt = <<"Node not found">>,
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang))
    end.

sm_route(Host, ServerHost, Packet) ->
    From = xmpp:get_from(Packet),
    #jid{user = ChanServ, resource = Resource} = xmpp:get_to(Packet),
    case str:tokens(ChanServ, <<"%">>) of
	[<<_, _/binary>> = Channel, <<_, _/binary>> = Server] ->
	    case ets:lookup(irc_connection, {From, Server, Host}) of
		[] ->
		    ?DEBUG("open new connection~n", []),
		    {Username, Encoding, Port, Password} =
			get_connection_params(Host, ServerHost, From, Server),
		    ConnectionUsername = case Packet of
					     %% If the user tries to join a
					     %% chatroom, the packet for sure
					     %% contains the desired username.
					     #presence{} -> Resource;
					     %% Otherwise, there is no firm
					     %% conclusion from the packet.
					     %% Better to use the configured
					     %% username (which defaults to the
					     %% username part of the JID).
					     _ -> Username
					 end,
		    Ident = extract_ident(Packet),
		    RemoteAddr = extract_ip_address(Packet),
		    RealName = get_realname(ServerHost),
		    WebircPassword = get_webirc_password(ServerHost),
		    {ok, Pid} = mod_irc_connection:start(
				  From, Host, ServerHost, Server,
				  ConnectionUsername, Encoding, Port,
				  Password, Ident, RemoteAddr, RealName,
				  WebircPassword, ?MODULE),
		    ets:insert(irc_connection,
			       #irc_connection{
				  jid_server_host = {From, Server, Host},
				  pid = Pid}),
		    mod_irc_connection:route_chan(Pid, Channel, Resource, Packet);
		[R] ->
		    Pid = R#irc_connection.pid,
		    ?DEBUG("send to process ~p~n", [Pid]),
		    mod_irc_connection:route_chan(Pid, Channel, Resource, Packet)
	    end;
	_ ->
	    Lang = xmpp:get_lang(Packet),
	    case str:tokens(ChanServ, <<"!">>) of
		[<<_, _/binary>> = Nick, <<_, _/binary>> = Server] ->
		    case ets:lookup(irc_connection, {From, Server, Host}) of
			[] ->
			    Txt = <<"IRC connection not found">>,
			    Err = xmpp:err_service_unavailable(Txt, Lang),
			    ejabberd_router:route_error(Packet, Err);
			[R] ->
			    Pid = R#irc_connection.pid,
			    ?DEBUG("send to process ~p~n", [Pid]),
			    mod_irc_connection:route_nick(Pid, Nick, Packet)
		    end;
		_ ->
		    Txt = <<"Failed to parse chanserv">>,
		    Err = xmpp:err_bad_request(Txt, Lang),
		    ejabberd_router:route_error(Packet, Err)
	    end
    end.

closed_connection(Host, From, Server) ->
    ets:delete(irc_connection, {From, Server, Host}).

iq_disco(_ServerHost, <<"">>, Lang) ->
    #disco_info{
       identities = [#identity{category = <<"conference">>,
			       type = <<"irc">>,
			       name = translate:translate(Lang, <<"IRC Transport">>)}],
       features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS, ?NS_MUC,
		   ?NS_REGISTER, ?NS_VCARD, ?NS_COMMANDS]};
iq_disco(ServerHost, Node, Lang) ->
    case lists:keyfind(Node, 1, commands(ServerHost)) of
	{_, Name, _} ->
	    #disco_info{
	       identities = [#identity{category = <<"automation">>,
				       type = <<"command-node">>,
				       name = translate:translate(Lang, Name)}],
	       features = [?NS_COMMANDS, ?NS_XDATA]};
	_ ->
	    undefined
    end.

iq_get_vcard(Lang) ->
    Desc = translate:translate(Lang, <<"ejabberd IRC module">>),
    #vcard_temp{fn = <<"ejabberd/mod_irc">>,
		url = ?EJABBERD_URI,
		desc = <<Desc/binary, $\n, ?COPYRIGHT>>}.

command_items(ServerHost, Host, Lang) ->
    lists:map(fun({Node, Name, _Function}) ->
		      #disco_item{jid = jid:make(Host),
				  node = Node,
				  name = translate:translate(Lang, Name)}
	      end, commands(ServerHost)).

commands(ServerHost) ->
    [{<<"join">>, <<"Join channel">>, fun adhoc_join/3},
     {<<"register">>,
      <<"Configure username, encoding, port and "
	"password">>,
      fun (From, To, Request) ->
	      adhoc_register(ServerHost, From, To, Request)
      end}].

get_data(ServerHost, Host, From) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_data(LServer, Host, From).

get_form(ServerHost, Host, From, Lang) ->
    #jid{user = User, server = Server} = From,
    DefaultEncoding = get_default_encoding(Host),
    Customs = case get_data(ServerHost, Host, From) of
		  error ->
		      Txt1 = <<"Database failure">>,
		      {error, xmpp:err_internal_server_error(Txt1, Lang)};
		  empty -> {User, []};
		  Data -> get_username_and_connection_params(Data)
	      end,
    case Customs of
	{error, _Error} ->
	    Customs;
	{Username, ConnectionsParams} ->
	    Fs = [#xdata_field{type = 'text-single',
			       label =  translate:translate(Lang, <<"IRC Username">>),
			       var = <<"username">>,
			       values = [Username]},
		  #xdata_field{type = fixed,
			       values = [str:format(
					   translate:translate(
					     Lang,
					     <<"If you want to specify"
					       " different ports, "
					       "passwords, encodings "
					       "for IRC servers, "
					       "fill this list with "
					       "values in format "
					       "'{\"irc server\", "
					       "\"encoding\", port, "
					       "\"password\"}'.  "
					       "By default this "
					       "service use \"~s\" "
					       "encoding, port ~p, "
					       "empty password.">>),
					   [DefaultEncoding, ?DEFAULT_IRC_PORT])]},
		  #xdata_field{type = fixed,
			       values = [translate:translate(
					   Lang,
					   <<"Example: [{\"irc.lucky.net\", \"koi8-r\", "
					     "6667, \"secret\"}, {\"vendetta.fef.net\", "
					     "\"iso8859-1\", 7000}, {\"irc.sometestserver.n"
					     "et\", \"utf-8\"}].">>)]},
		  #xdata_field{type = 'text-multi',
			       label = translate:translate(
					 Lang, <<"Connections parameters">>),
			       var = <<"connections_params">>,
			       values = str:tokens(str:format(
						     "~p.",
						     [conn_params_to_list(
							ConnectionsParams)]),
						   <<"\n">>)}],
	    X = #xdata{type = form,
		       title = <<(translate:translate(
				    Lang, <<"Registration in mod_irc for ">>))/binary,
				 User/binary, "@", Server/binary>>,
		       instructions =
			   [translate:translate(
			      Lang,
			      <<"Enter username, encodings, ports and "
				"passwords you wish to use for connecting "
				"to IRC servers">>)],
		       fields = Fs},
	    {result,
	     #register{instructions = 
			   translate:translate(Lang,
					       <<"You need an x:data capable client to "
						 "configure mod_irc settings">>),
		       xdata = X}}
    end.

set_data(ServerHost, Host, From, Data) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_data(LServer, Host, From, data_to_binary(From, Data)).

set_form(ServerHost, Host, From, Lang, XData) ->
    case {xmpp_util:get_xdata_values(<<"username">>, XData),
	  xmpp_util:get_xdata_values(<<"connections_params">>, XData)} of
	{[Username], [_|_] = Strings} ->
	    EncString = lists:foldl(fun (S, Res) ->
					    <<Res/binary, S/binary, "\n">>
				    end, <<"">>, Strings),
	    case erl_scan:string(binary_to_list(EncString)) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, ConnectionsParams} ->
			    case set_data(ServerHost, Host, From,
					  [{username, Username},
					   {connections_params, ConnectionsParams}]) of
				{atomic, _} ->
				    {result, undefined};
				_ ->
				    Txt = <<"Database failure">>,
				    {error, xmpp:err_internal_server_error(Txt, Lang)}
			    end;
			_ ->
			    Txt = <<"Parse error">>,
			    {error, xmpp:err_not_acceptable(Txt, Lang)}
		    end;
		_ ->
		    {error, xmpp:err_not_acceptable(<<"Scan error">>, Lang)}
	    end;
	_ ->
	    Txt = <<"Incorrect value in data form">>,
	    {error, xmpp:err_not_acceptable(Txt, Lang)}
    end.

get_connection_params(Host, From, IRCServer) ->
    [_ | HostTail] = str:tokens(Host, <<".">>),
    ServerHost = str:join(HostTail, <<".">>),
    get_connection_params(Host, ServerHost, From,
			  IRCServer).

get_default_encoding(ServerHost) ->
    Result = gen_mod:get_module_opt(ServerHost, ?MODULE, default_encoding,
                                    fun iolist_to_binary/1,
                                    ?DEFAULT_IRC_ENCODING),
    ?INFO_MSG("The default_encoding configured for "
	      "host ~p is: ~p~n",
	      [ServerHost, Result]),
    Result.

get_realname(ServerHost) ->
    gen_mod:get_module_opt(ServerHost, ?MODULE, realname, fun iolist_to_binary/1, ?DEFAULT_REALNAME).

get_webirc_password(ServerHost) ->
    gen_mod:get_module_opt(ServerHost, ?MODULE, webirc_password, fun iolist_to_binary/1, ?DEFAULT_WEBIRC_PASSWORD).

get_connection_params(Host, ServerHost, From,
		      IRCServer) ->
    #jid{user = User, server = _Server} = From,
    DefaultEncoding = get_default_encoding(ServerHost),
    case get_data(ServerHost, Host, From) of
      error ->
	  {User, DefaultEncoding, ?DEFAULT_IRC_PORT, <<"">>};
      empty ->
	  {User, DefaultEncoding, ?DEFAULT_IRC_PORT, <<"">>};
      Data ->
          {Username, ConnParams} = get_username_and_connection_params(Data),
	  {NewUsername, NewEncoding, NewPort, NewPassword} = case
							       lists:keysearch(IRCServer,
									       1,
									       ConnParams)
								 of
							       {value,
								{_, Encoding,
								 Port,
								 Password}} ->
								   {Username,
								    Encoding,
								    Port,
								    Password};
							       {value,
								{_, Encoding,
								 Port}} ->
								   {Username,
								    Encoding,
								    Port,
								    <<"">>};
							       {value,
								{_,
								 Encoding}} ->
								   {Username,
								    Encoding,
								    ?DEFAULT_IRC_PORT,
								    <<"">>};
							       _ ->
								   {Username,
								    DefaultEncoding,
								    ?DEFAULT_IRC_PORT,
								    <<"">>}
							     end,
	  {iolist_to_binary(NewUsername),
           iolist_to_binary(NewEncoding),
	   if NewPort >= 0 andalso NewPort =< 65535 -> NewPort;
	      true -> ?DEFAULT_IRC_PORT
	   end,
	   iolist_to_binary(NewPassword)}
    end.

adhoc_join(_From, _To, #adhoc_command{action = cancel} = Request) ->
    xmpp_util:make_adhoc_response(Request, #adhoc_command{status = canceled});
adhoc_join(_From, _To, #adhoc_command{lang = Lang, xdata = undefined} = Request) ->
    X = #xdata{type = form,
	       title = translate:translate(Lang, <<"Join IRC channel">>),
	       fields = [#xdata_field{var = <<"channel">>,
				      type = 'text-single',
				      label = translate:translate(
						Lang, <<"IRC channel (don't put the first #)">>),
				      required = true},
			 #xdata_field{var = <<"server">>,
				      type = 'text-single',
				      label = translate:translate(Lang, <<"IRC server">>),
				      required = true}]},
    xmpp_util:make_adhoc_response(
      Request, #adhoc_command{status = executing, xdata = X});
adhoc_join(From, To, #adhoc_command{lang = Lang, xdata = X} = Request) ->
    Channel = case xmpp_util:get_xdata_values(<<"channel">>, X) of
		  [C] -> C;
		  _ -> false
	      end,
    Server = case xmpp_util:get_xdata_values(<<"server">>, X) of
		 [S] -> S;
		 _ -> false
	     end,
    if Channel /= false, Server /= false ->
	    RoomJID = jid:make(<<Channel/binary, "%", Server/binary>>,
			       To#jid.server),
	    Reason = translate:translate(Lang, <<"Join the IRC channel here.">>),
	    BodyTxt = {<<"Join the IRC channel in this Jabber ID: ~s">>,
		       [jid:encode(RoomJID)]},
	    Invite = #message{
			from = RoomJID, to = From,
			body = xmpp:mk_text(BodyTxt, Lang),
			sub_els = [#muc_user{
				      invites = [#muc_invite{from = From,
							     reason = Reason}]},
				   #x_conference{reason = Reason,
						 jid = RoomJID}]},
	    ejabberd_router:route(Invite),
	    xmpp_util:make_adhoc_response(
	      Request, #adhoc_command{status = completed});
       true ->
	    Txt = <<"Missing 'channel' or 'server' in the data form">>,
	    {error, xmpp:err_bad_request(Txt, Lang)}
    end.

-spec adhoc_register(binary(), jid(), jid(), adhoc_command()) ->
			    adhoc_command() | {error, stanza_error()}.
adhoc_register(_ServerHost, _From, _To,
	       #adhoc_command{action = cancel} = Request) ->
    xmpp_util:make_adhoc_response(Request, #adhoc_command{status = canceled});
adhoc_register(ServerHost, From, To,
	       #adhoc_command{lang = Lang, xdata = X,
			      action = Action} = Request) ->
    #jid{user = User} = From,
    #jid{lserver = Host} = To,
    {Username, ConnectionsParams} =
	if X == undefined ->
		case get_data(ServerHost, Host, From) of
		    error -> {User, []};
		    empty -> {User, []};
		    Data -> get_username_and_connection_params(Data)
		end;
	   true ->
		{case xmpp_util:get_xdata_values(<<"username">>, X) of
		     [U] -> U;
		     _ -> User
		 end, parse_connections_params(X)}
	end,
    if Action == complete ->
	    case set_data(ServerHost, Host, From,
			  [{username, Username},
			   {connections_params, ConnectionsParams}]) of
		{atomic, _} ->
		    xmpp_util:make_adhoc_response(
		      Request, #adhoc_command{status = completed});
		_ ->
		    Txt = <<"Database failure">>,
		    {error, xmpp:err_internal_server_error(Txt, Lang)}
	    end;
       true ->
	    Form = generate_adhoc_register_form(Lang, Username,
						ConnectionsParams),
	    xmpp_util:make_adhoc_response(
	      Request, #adhoc_command{
			  status = executing,
			  xdata = Form,
			  actions = #adhoc_actions{next = true,
						   complete = true}})
    end.

generate_adhoc_register_form(Lang, Username,
			     ConnectionsParams) ->
    #xdata{type = form,
	   title = translate:translate(Lang, <<"IRC settings">>),
	   instructions = [translate:translate(
			     Lang,
			     <<"Enter username and encodings you wish "
			       "to use for connecting to IRC servers. "
			       " Press 'Next' to get more fields to "
			       "fill in.  Press 'Complete' to save settings.">>)],
	   fields = [#xdata_field{
			var = <<"username">>,
			type = 'text-single',
			label = translate:translate(Lang, <<"IRC username">>),
			required = true,
			values = [Username]}
		     | generate_connection_params_fields(
			 Lang, ConnectionsParams, 1, [])]}.

generate_connection_params_fields(Lang, [], Number,
				  Acc) ->
    Field = generate_connection_params_field(Lang, <<"">>,
					     <<"">>, -1, <<"">>, Number),
    lists:reverse(Field ++ Acc);
generate_connection_params_fields(Lang,
				  [ConnectionParams | ConnectionsParams],
				  Number, Acc) ->
    case ConnectionParams of
      {Server, Encoding, Port, Password} ->
	  Field = generate_connection_params_field(Lang, Server,
						   Encoding, Port, Password,
						   Number),
	  generate_connection_params_fields(Lang,
					    ConnectionsParams, Number + 1,
					    Field ++ Acc);
      {Server, Encoding, Port} ->
	  Field = generate_connection_params_field(Lang, Server,
						   Encoding, Port, <<"">>, Number),
	  generate_connection_params_fields(Lang,
					    ConnectionsParams, Number + 1,
					    Field ++ Acc);
      {Server, Encoding} ->
	  Field = generate_connection_params_field(Lang, Server,
						   Encoding, -1, <<"">>, Number),
	  generate_connection_params_fields(Lang,
					    ConnectionsParams, Number + 1,
					    Field ++ Acc);
      _ -> []
    end.

generate_connection_params_field(Lang, Server, Encoding,
				 Port, Password, Number) ->
    EncodingUsed = case Encoding of
		     <<>> -> get_default_encoding(Server);
		     _ -> Encoding
		   end,
    PortUsedInt = if Port >= 0 andalso Port =< 65535 ->
			 Port;
		     true -> ?DEFAULT_IRC_PORT
		  end,
    PortUsed = integer_to_binary(PortUsedInt),
    PasswordUsed = case Password of
		     <<>> -> <<>>;
		     _ -> Password
		   end,
    NumberString = integer_to_binary(Number),
    [#xdata_field{var = <<"password", NumberString/binary>>,
		  type = 'text-single',
		  label = str:format(
			    translate:translate(Lang, <<"Password ~b">>),
			    [Number]),
		  values = [PasswordUsed]},
     #xdata_field{var = <<"port", NumberString/binary>>,
		  type = 'text-single',
		  label = str:format(
			    translate:translate(Lang, <<"Port ~b">>),
			    [Number]),
		  values = [PortUsed]},
     #xdata_field{var = <<"encoding", NumberString/binary>>,
		  type = 'list-single',
		  label = str:format(
			    translate:translate(Lang, <<"Encoding for server ~b">>),
			    [Number]),
		  values = [EncodingUsed],
		  options = [#xdata_option{label = E, value = E}
			     || E <- ?POSSIBLE_ENCODINGS]},
     #xdata_field{var = <<"server", NumberString/binary>>,
		  type = 'text-single',
		  label = str:format(
			    translate:translate(Lang, <<"Server ~b">>),
			    [Number]),
		  values = [Server]}].

parse_connections_params(#xdata{fields = Fields}) ->
    Servers = lists:flatmap(
                fun(#xdata_field{var = <<"server", Var/binary>>,
				 values = Values}) ->
                        [{Var, Values}];
                   (_) ->
                        []
                end, Fields),
    Encodings = lists:flatmap(
                  fun(#xdata_field{var = <<"encoding", Var/binary>>,
				   values = Values}) ->
                          [{Var, Values}];
                     (_) ->
                          []
                  end, Fields),
    Ports = lists:flatmap(
              fun(#xdata_field{var = <<"port", Var/binary>>,
			       values = Values}) ->
                      [{Var, Values}];
                 (_) ->
                      []
              end, Fields),
    Passwords = lists:flatmap(
                  fun(#xdata_field{var = <<"password", Var/binary>>,
				   values = Values}) ->
                          [{Var, Values}];
                     (_) ->
                          []
                  end, Fields),
    parse_connections_params(Servers, Encodings, Ports, Passwords).

retrieve_connections_params(ConnectionParams,
			    ServerN) ->
    case ConnectionParams of
      [{ConnectionParamN, ConnectionParam}
       | ConnectionParamsTail] ->
	  if ServerN == ConnectionParamN ->
		 {ConnectionParam, ConnectionParamsTail};
	     ServerN < ConnectionParamN ->
		 {[],
		  [{ConnectionParamN, ConnectionParam}
		   | ConnectionParamsTail]};
	     ServerN > ConnectionParamN -> {[], ConnectionParamsTail}
	  end;
      _ -> {[], []}
    end.

parse_connections_params([], _, _, _) -> [];
parse_connections_params(_, [], [], []) -> [];
parse_connections_params([{ServerN, Server} | Servers],
			 Encodings, Ports, Passwords) ->
    {NewEncoding, NewEncodings} =
	retrieve_connections_params(Encodings, ServerN),
    {NewPort, NewPorts} = retrieve_connections_params(Ports,
						      ServerN),
    {NewPassword, NewPasswords} =
	retrieve_connections_params(Passwords, ServerN),
    [{Server, NewEncoding, NewPort, NewPassword}
     | parse_connections_params(Servers, NewEncodings,
				NewPorts, NewPasswords)].

get_username_and_connection_params(Data) ->
    Username = case lists:keysearch(username, 1, Data) of
                   {value, {_, U}} when is_binary(U) ->
                       U;
                   _ ->
                       <<"">>
               end,
    ConnParams = case lists:keysearch(connections_params, 1, Data) of
                     {value, {_, L}} when is_list(L) ->
                         L;
                     _ ->
                         []
                 end,
    {Username, ConnParams}.

data_to_binary(JID, Data) ->
    lists:map(
      fun({username, U}) ->
              {username, iolist_to_binary(U)};
         ({connections_params, Params}) ->
	      {connections_params,
	       lists:flatmap(
		 fun(Param) ->
			 try
			     [conn_param_to_binary(Param)]
			 catch _:_ ->
				 if JID /= error ->
					 ?ERROR_MSG("failed to convert "
						    "parameter ~p for user ~s",
						    [Param,
						     jid:encode(JID)]);
				    true ->
					 ?ERROR_MSG("failed to convert "
						    "parameter ~p",
						    [Param])
				 end,
				 []
			 end
		 end, Params)};
         (Opt) ->
              Opt
      end, Data).

conn_param_to_binary({S}) ->
    {iolist_to_binary(S)};
conn_param_to_binary({S, E}) ->
    {iolist_to_binary(S), iolist_to_binary(E)};
conn_param_to_binary({S, E, Port}) when is_integer(Port) ->
    {iolist_to_binary(S), iolist_to_binary(E), Port};
conn_param_to_binary({S, E, Port, P}) when is_integer(Port) ->
    {iolist_to_binary(S), iolist_to_binary(E), Port, iolist_to_binary(P)}.

conn_params_to_list(Params) ->
    lists:map(
      fun({S}) ->
              {binary_to_list(S)};
         ({S, E}) ->
              {binary_to_list(S), binary_to_list(E)};
         ({S, E, Port}) ->
              {binary_to_list(S), binary_to_list(E), Port};
         ({S, E, Port, P}) ->
              {binary_to_list(S), binary_to_list(E),
               Port, binary_to_list(P)}
      end, Params).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, Data) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Data).

mod_opt_type(access) ->
    fun acl:access_rules_validator/1;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(default_encoding) ->
    fun iolist_to_binary/1;
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(_) ->
    [access, db_type, default_encoding, host].

-spec extract_ident(stanza()) -> binary().
extract_ident(Packet) ->
    Hdrs = extract_headers(Packet),
    proplists:get_value(<<"X-Irc-Ident">>, Hdrs, <<"chatmovil">>).

-spec extract_ip_address(stanza()) -> binary().
extract_ip_address(Packet) ->
    Hdrs = extract_headers(Packet),
    proplists:get_value(<<"X-Forwarded-For">>, Hdrs, <<"127.0.0.1">>).

-spec extract_headers(stanza()) -> [{binary(), binary()}].
extract_headers(Packet) ->
    case xmpp:get_subtag(Packet, #shim{}) of
	#shim{headers = Hs} -> Hs;
	false -> []
    end.
