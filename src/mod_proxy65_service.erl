%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_service.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : SOCKS5 Bytestreams XMPP service.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
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

-module(mod_proxy65_service).

-author('xram@jabber.ru').

-behaviour(gen_server).

%% gen_server callbacks.
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

-export([start_link/2, reload/3, add_listener/2, process_disco_info/1,
	 process_disco_items/1, process_vcard/1, process_bytestreams/1,
	 transform_module_options/1, delete_listener/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(PROCNAME, ejabberd_mod_proxy65_service).

-record(state, {myhost = <<"">> :: binary()}).

%%%------------------------
%%% gen_server callbacks
%%%------------------------

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

reload(Host, NewOpts, OldOpts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {reload, Host, NewOpts, OldOpts}).

init([Host, Opts]) ->
    process_flag(trap_exit, true),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    MyHost = gen_mod:get_opt_host(Host, Opts, <<"proxy.@HOST@">>),
    gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_DISCO_INFO,
				  ?MODULE, process_disco_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_DISCO_ITEMS,
				  ?MODULE, process_disco_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_VCARD,
				  ?MODULE, process_vcard, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_BYTESTREAMS,
				  ?MODULE, process_bytestreams, IQDisc),
    ejabberd_router:register_route(MyHost, Host),
    {ok, #state{myhost = MyHost}}.

terminate(_Reason, #state{myhost = MyHost}) ->
    ejabberd_router:unregister_route(MyHost),
    gen_iq_handler:remove_iq_handler(ejabberd_local, MyHost, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_local, MyHost, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, MyHost, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_local, MyHost, ?NS_BYTESTREAMS).

handle_info({route, #iq{} = Packet}, State) ->
    ejabberd_router:process_iq(Packet),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({reload, ServerHost, NewOpts, OldOpts}, State) ->
    NewHost = gen_mod:get_opt_host(ServerHost, NewOpts, <<"proxy.@HOST@">>),
    OldHost = gen_mod:get_opt_host(ServerHost, OldOpts, <<"proxy.@HOST@">>),
    NewIQDisc = gen_mod:get_opt(iqdisc, NewOpts,
				fun gen_iq_handler:check_type/1,
				one_queue),
    OldIQDisc = gen_mod:get_opt(iqdisc, OldOpts,
				fun gen_iq_handler:check_type/1,
				one_queue),
    if (NewIQDisc /= OldIQDisc) or (NewHost /= OldHost) ->
	    gen_iq_handler:add_iq_handler(ejabberd_local, NewHost, ?NS_DISCO_INFO,
					  ?MODULE, process_disco_info, NewIQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_local, NewHost, ?NS_DISCO_ITEMS,
					  ?MODULE, process_disco_items, NewIQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_local, NewHost, ?NS_VCARD,
					  ?MODULE, process_vcard, NewIQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_local, NewHost, ?NS_BYTESTREAMS,
					  ?MODULE, process_bytestreams, NewIQDisc);
       true ->
	    ok
    end,
    if NewHost /= OldHost ->
	    ejabberd_router:register_route(NewHost, ServerHost),
	    ejabberd_router:unregister_route(OldHost),
	    gen_iq_handler:remove_iq_handler(ejabberd_local, OldHost, ?NS_DISCO_INFO),
	    gen_iq_handler:remove_iq_handler(ejabberd_local, OldHost, ?NS_DISCO_ITEMS),
	    gen_iq_handler:remove_iq_handler(ejabberd_local, OldHost, ?NS_VCARD),
	    gen_iq_handler:remove_iq_handler(ejabberd_local, OldHost, ?NS_BYTESTREAMS);
       true ->
	    ok
    end,
    {noreply, State#state{myhost = NewHost}};
handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%------------------------
%%% Listener management
%%%------------------------

add_listener(Host, Opts) ->
    NewOpts = [Host | Opts],
    ejabberd_listener:add_listener(get_port_ip(Host),
				   mod_proxy65_stream, NewOpts).

delete_listener(Host) ->
    catch ejabberd_listener:delete_listener(get_port_ip(Host),
					    mod_proxy65_stream).

%%%------------------------
%%% IQ Processing
%%%------------------------
-spec process_disco_info(iq()) -> iq().
process_disco_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_info(#iq{type = get, to = To, lang = Lang} = IQ) ->
    Host = ejabberd_router:host_of_route(To#jid.lserver),
    Name = gen_mod:get_module_opt(Host, mod_proxy65, name,
				  fun iolist_to_binary/1,
				  <<"SOCKS5 Bytestreams">>),
    Info = ejabberd_hooks:run_fold(disco_info, Host,
				   [], [Host, ?MODULE, <<"">>, <<"">>]),
    xmpp:make_iq_result(
      IQ, #disco_info{xdata = Info,
		      identities = [#identity{category = <<"proxy">>,
					      type = <<"bytestreams">>,
					      name =  translate:translate(Lang, Name)}],
		      features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS,
				  ?NS_VCARD, ?NS_BYTESTREAMS]}).

-spec process_disco_items(iq()) -> iq().
process_disco_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get} = IQ) ->
    xmpp:make_iq_result(IQ, #disco_items{}).

-spec process_vcard(iq()) -> iq().
process_vcard(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_vcard(#iq{type = get, lang = Lang} = IQ) ->
    Desc = translate:translate(Lang, <<"ejabberd SOCKS5 Bytestreams module">>),
    xmpp:make_iq_result(
      IQ, #vcard_temp{fn = <<"ejabberd/mod_proxy65">>,
		      url = ?EJABBERD_URI,
		      desc = <<Desc/binary, $\n, ?COPYRIGHT>>}).

-spec process_bytestreams(iq()) -> iq().
process_bytestreams(#iq{type = get, from = JID, to = To, lang = Lang} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    ACL = gen_mod:get_module_opt(ServerHost, mod_proxy65, access,
				 fun acl:access_rules_validator/1,
				 all),
    case acl:match_rule(ServerHost, ACL, JID) of
	allow ->
	    StreamHost = get_streamhost(Host, ServerHost),
	    xmpp:make_iq_result(IQ, #bytestreams{hosts = [StreamHost]});
	deny ->
	    xmpp:make_error(IQ, xmpp:err_forbidden(<<"Denied by ACL">>, Lang))
    end;
process_bytestreams(#iq{type = set, lang = Lang,
			sub_els = [#bytestreams{sid = SID}]} = IQ)
  when SID == <<"">> orelse size(SID) > 128 ->
    Why = {bad_attr_value, <<"sid">>, <<"query">>, ?NS_BYTESTREAMS},
    Txt = xmpp:format_error(Why),
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
process_bytestreams(#iq{type = set, lang = Lang, 
			sub_els = [#bytestreams{activate = undefined}]} = IQ) ->
    Why = {missing_cdata, <<"">>, <<"activate">>, ?NS_BYTESTREAMS},
    Txt = xmpp:format_error(Why),
    xmpp:make_error(IQ, xmpp:err_jid_malformed(Txt, Lang));
process_bytestreams(#iq{type = set, lang = Lang, from = InitiatorJID, to = To,
			sub_els = [#bytestreams{activate = TargetJID,
						sid = SID}]} = IQ) ->
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    ACL = gen_mod:get_module_opt(ServerHost, mod_proxy65, access,
				 fun acl:access_rules_validator/1,
				 all),
    case acl:match_rule(ServerHost, ACL, InitiatorJID) of
	allow ->
	    Node = ejabberd_cluster:get_node_by_id(To#jid.lresource),
	    Target = jid:encode(jid:tolower(TargetJID)),
	    Initiator = jid:encode(jid:tolower(InitiatorJID)),
	    SHA1 = str:sha(<<SID/binary, Initiator/binary, Target/binary>>),
	    Mod = gen_mod:ram_db_mod(global, mod_proxy65),
	    MaxConnections = max_connections(ServerHost),
	    case Mod:activate_stream(SHA1, Initiator, MaxConnections, Node) of
		{ok, InitiatorPid, TargetPid} ->
		    mod_proxy65_stream:activate(
		      {InitiatorPid, InitiatorJID}, {TargetPid, TargetJID}),
		    xmpp:make_iq_result(IQ);
		{error, notfound} ->
		    Txt = <<"Failed to activate bytestream">>,
		    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
		{error, {limit, InitiatorPid, TargetPid}} ->
		    mod_proxy65_stream:stop(InitiatorPid),
		    mod_proxy65_stream:stop(TargetPid),
		    Txt = <<"Too many active bytestreams">>,
		    xmpp:make_error(IQ, xmpp:err_resource_constraint(Txt, Lang));
		{error, conflict} ->
		    Txt = <<"Bytestream already activated">>,
		    xmpp:make_error(IQ, xmpp:err_conflict(Txt, Lang));
		{error, Err} ->
		    ?ERROR_MSG("failed to activate bytestream from ~s to ~s: ~p",
			       [Initiator, Target, Err]),
		    Txt = <<"Database failure">>,
		    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
	    end;
	deny ->
	    Txt = <<"Denied by ACL">>,
	    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang))
    end.

%%%-------------------------
%%% Auxiliary functions.
%%%-------------------------
transform_module_options(Opts) ->
    lists:map(
      fun({ip, IP}) when is_tuple(IP) ->
              {ip, misc:ip_to_list(IP)};
         ({hostname, IP}) when is_tuple(IP) ->
              {hostname, misc:ip_to_list(IP)};
         (Opt) ->
              Opt
      end, Opts).

-spec get_streamhost(binary(), binary()) -> streamhost().
get_streamhost(Host, ServerHost) ->
    {Port, IP} = get_port_ip(ServerHost),
    HostName = gen_mod:get_module_opt(ServerHost, mod_proxy65, hostname,
				      fun iolist_to_binary/1,
				      misc:ip_to_list(IP)),
    Resource = ejabberd_cluster:node_id(),
    #streamhost{jid = jid:make(<<"">>, Host, Resource),
		host = HostName,
		port = Port}.

-spec get_port_ip(binary()) -> {pos_integer(), inet:ip_address()}.
get_port_ip(Host) ->
    Port = gen_mod:get_module_opt(Host, mod_proxy65, port,
				  fun(P) when is_integer(P), P>0, P<65536 ->
					  P
				  end,
				  7777),
    IP = gen_mod:get_module_opt(Host, mod_proxy65, ip,
				fun(S) ->
					{ok, Addr} = inet_parse:address(
						       binary_to_list(
							 iolist_to_binary(S))),
					Addr
				end, get_my_ip()),
    {Port, IP}.

-spec get_my_ip() -> inet:ip_address().
get_my_ip() ->
    {ok, MyHostName} = inet:gethostname(),
    case inet:getaddr(MyHostName, inet) of
      {ok, Addr} -> Addr;
      {error, _} -> {127, 0, 0, 1}
    end.

max_connections(ServerHost) ->
    gen_mod:get_module_opt(ServerHost, mod_proxy65, max_connections,
			   fun(I) when is_integer(I), I>0 -> I;
			      (infinity) -> infinity
			   end, infinity).
