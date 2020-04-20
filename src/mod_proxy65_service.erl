%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_service.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : SOCKS5 Bytestreams XMPP service.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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

-export([start_link/1, reload/3, add_listener/2, process_disco_info/1,
	 process_disco_items/1, process_vcard/1, process_bytestreams/1,
	 delete_listener/1, route/1]).

-include("logger.hrl").
-include("xmpp.hrl").
-include("translate.hrl").
-include("ejabberd_stacktrace.hrl").

-define(PROCNAME, ejabberd_mod_proxy65_service).

-record(state, {myhosts = [] :: [binary()]}).

%%%------------------------
%%% gen_server callbacks
%%%------------------------

start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host], []).

reload(Host, NewOpts, OldOpts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {reload, Host, NewOpts, OldOpts}).

init([Host]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(Host, mod_proxy65),
    MyHosts = gen_mod:get_opt_hosts(Opts),
    lists:foreach(
      fun(MyHost) ->
	      gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_DISCO_INFO,
					    ?MODULE, process_disco_info),
	      gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_DISCO_ITEMS,
					    ?MODULE, process_disco_items),
	      gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_VCARD,
					    ?MODULE, process_vcard),
	      gen_iq_handler:add_iq_handler(ejabberd_local, MyHost, ?NS_BYTESTREAMS,
					    ?MODULE, process_bytestreams),
	      ejabberd_router:register_route(
		MyHost, Host, {apply, ?MODULE, route})
      end, MyHosts),
    {ok, #state{myhosts = MyHosts}}.

terminate(_Reason, #state{myhosts = MyHosts}) ->
    lists:foreach(
      fun(MyHost) ->
	      ejabberd_router:unregister_route(MyHost),
	      unregister_handlers(MyHost)
      end, MyHosts).

handle_info({route, Packet}, State) ->
    try route(Packet)
    catch ?EX_RULE(Class, Reason, St) ->
            StackTrace = ?EX_STACK(St),
            ?ERROR_MSG("Failed to route packet:~n~ts~n** ~ts",
                       [xmpp:pp(Packet),
                        misc:format_exception(2, Class, Reason, StackTrace)])
    end,
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast({reload, ServerHost, NewOpts, OldOpts}, State) ->
    NewHosts = gen_mod:get_opt_hosts(NewOpts),
    OldHosts = gen_mod:get_opt_hosts(OldOpts),
    lists:foreach(
      fun(NewHost) ->
	      ejabberd_router:register_route(NewHost, ServerHost),
	      register_handlers(NewHost)
      end, NewHosts -- OldHosts),
    lists:foreach(
      fun(OldHost) ->
	      ejabberd_router:unregister_route(OldHost),
	      unregister_handlers(OldHost)
      end, OldHosts -- NewHosts),
    {noreply, State#state{myhosts = NewHosts}};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec route(stanza()) -> ok.
route(#iq{} = IQ) ->
    ejabberd_router:process_iq(IQ);
route(_) ->
    ok.

%%%------------------------
%%% Listener management
%%%------------------------

add_listener(Host, Opts) ->
    {_, IP, _} = EndPoint = get_endpoint(Host),
    Opts1 = gen_mod:set_opt(server_host, Host, Opts),
    Opts2 = gen_mod:set_opt(ip, IP, Opts1),
    ejabberd_listener:add_listener(EndPoint, mod_proxy65_stream, Opts2).

delete_listener(Host) ->
    ejabberd_listener:delete_listener(get_endpoint(Host), mod_proxy65_stream).

%%%------------------------
%%% IQ Processing
%%%------------------------
-spec process_disco_info(iq()) -> iq().
process_disco_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_info(#iq{type = get, to = To, lang = Lang} = IQ) ->
    Host = ejabberd_router:host_of_route(To#jid.lserver),
    Name = mod_proxy65_opt:name(Host),
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
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get} = IQ) ->
    xmpp:make_iq_result(IQ, #disco_items{}).

-spec process_vcard(iq()) -> iq().
process_vcard(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_vcard(#iq{type = get, to = To, lang = Lang} = IQ) ->
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    VCard = case mod_proxy65_opt:vcard(ServerHost) of
		undefined ->
		    #vcard_temp{fn = <<"ejabberd/mod_proxy65">>,
				url = ejabberd_config:get_uri(),
				desc = misc:get_descr(
					 Lang, ?T("ejabberd SOCKS5 Bytestreams module"))};
		V ->
		    V
	    end,
    xmpp:make_iq_result(IQ, VCard).

-spec process_bytestreams(iq()) -> iq().
process_bytestreams(#iq{type = get, from = JID, to = To, lang = Lang} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    ACL = mod_proxy65_opt:access(ServerHost),
    case acl:match_rule(ServerHost, ACL, JID) of
	allow ->
	    StreamHost = get_streamhost(Host, ServerHost),
	    xmpp:make_iq_result(IQ, #bytestreams{hosts = [StreamHost]});
	deny ->
	    xmpp:make_error(IQ, xmpp:err_forbidden(?T("Access denied by service policy"), Lang))
    end;
process_bytestreams(#iq{type = set, lang = Lang,
			sub_els = [#bytestreams{sid = SID}]} = IQ)
  when SID == <<"">> orelse size(SID) > 128 ->
    Why = {bad_attr_value, <<"sid">>, <<"query">>, ?NS_BYTESTREAMS},
    Txt = xmpp:io_format_error(Why),
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
process_bytestreams(#iq{type = set, lang = Lang,
			sub_els = [#bytestreams{activate = undefined}]} = IQ) ->
    Why = {missing_cdata, <<"">>, <<"activate">>, ?NS_BYTESTREAMS},
    Txt = xmpp:io_format_error(Why),
    xmpp:make_error(IQ, xmpp:err_jid_malformed(Txt, Lang));
process_bytestreams(#iq{type = set, lang = Lang, from = InitiatorJID, to = To,
			sub_els = [#bytestreams{activate = TargetJID,
						sid = SID}]} = IQ) ->
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    ACL = mod_proxy65_opt:access(ServerHost),
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
		    Txt = ?T("Failed to activate bytestream"),
		    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
		{error, {limit, InitiatorPid, TargetPid}} ->
		    mod_proxy65_stream:stop(InitiatorPid),
		    mod_proxy65_stream:stop(TargetPid),
		    Txt = ?T("Too many active bytestreams"),
		    xmpp:make_error(IQ, xmpp:err_resource_constraint(Txt, Lang));
		{error, conflict} ->
		    Txt = ?T("Bytestream already activated"),
		    xmpp:make_error(IQ, xmpp:err_conflict(Txt, Lang));
		{error, Err} ->
		    ?ERROR_MSG("Failed to activate bytestream from ~ts to ~ts: ~p",
			       [Initiator, Target, Err]),
		    Txt = ?T("Database failure"),
		    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
	    end;
	deny ->
	    Txt = ?T("Access denied by service policy"),
	    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang))
    end.

%%%-------------------------
%%% Auxiliary functions.
%%%-------------------------
-spec get_streamhost(binary(), binary()) -> streamhost().
get_streamhost(Host, ServerHost) ->
    {Port, IP, _} = get_endpoint(ServerHost),
    HostName = case mod_proxy65_opt:hostname(ServerHost) of
		   undefined -> misc:ip_to_list(IP);
		   Val -> Val
	       end,
    Resource = ejabberd_cluster:node_id(),
    #streamhost{jid = jid:make(<<"">>, Host, Resource),
		host = HostName,
		port = Port}.

-spec get_endpoint(binary()) -> {inet:port_number(), inet:ip_address(), tcp}.
get_endpoint(Host) ->
    Port = mod_proxy65_opt:port(Host),
    IP = case mod_proxy65_opt:ip(Host) of
	     undefined -> misc:get_my_ip();
	     Addr -> Addr
	 end,
    {Port, IP, tcp}.

max_connections(ServerHost) ->
    mod_proxy65_opt:max_connections(ServerHost).

register_handlers(Host) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
				  ?MODULE, process_disco_info),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
				  ?MODULE, process_disco_items),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
				  ?MODULE, process_vcard),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_BYTESTREAMS,
				  ?MODULE, process_bytestreams).

unregister_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_BYTESTREAMS).
