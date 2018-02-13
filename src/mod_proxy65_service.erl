%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_service.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : SOCKS5 Bytestreams XMPP service.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-include("translate.hrl").

-define(PROCNAME, ejabberd_mod_proxy65_service).

-record(state, {myhosts = [] :: [binary()]}).

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
    MyHosts = gen_mod:get_opt_hosts(Host, Opts),
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
	      ejabberd_router:register_route(MyHost, Host)
      end, MyHosts),
    {ok, #state{myhosts = MyHosts}}.

terminate(_Reason, #state{myhosts = MyHosts}) ->
    lists:foreach(
      fun(MyHost) ->
	      ejabberd_router:unregister_route(MyHost),
	      unregister_handlers(MyHost)
      end, MyHosts).

handle_info({route, #iq{} = Packet}, State) ->
    ejabberd_router:process_iq(Packet),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({reload, ServerHost, NewOpts, OldOpts}, State) ->
    NewHosts = gen_mod:get_opt_hosts(ServerHost, NewOpts),
    OldHosts = gen_mod:get_opt_hosts(ServerHost, OldOpts),
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
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%------------------------
%%% Listener management
%%%------------------------

add_listener(Host, Opts) ->
    NewOpts = [{server_host, Host} | Opts],
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
    Name = gen_mod:get_module_opt(Host, mod_proxy65, name),
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
    ACL = gen_mod:get_module_opt(ServerHost, mod_proxy65, access),
    case acl:match_rule(ServerHost, ACL, JID) of
	allow ->
	    StreamHost = get_streamhost(Host, ServerHost),
	    xmpp:make_iq_result(IQ, #bytestreams{hosts = [StreamHost]});
	deny ->
	    xmpp:make_error(IQ, xmpp:err_forbidden(<<"Access denied by service policy">>, Lang))
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
    ACL = gen_mod:get_module_opt(ServerHost, mod_proxy65, access),
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
	    Txt = <<"Access denied by service policy">>,
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
    HostName0 = case gen_mod:get_module_opt(ServerHost, mod_proxy65, hostname) of
		    undefined -> misc:ip_to_list(IP);
		    Val -> Val
		end,
    HostName = misc:expand_keyword(<<"@HOST@">>, HostName0, ServerHost),
    Resource = ejabberd_cluster:node_id(),
    #streamhost{jid = jid:make(<<"">>, Host, Resource),
		host = HostName,
		port = Port}.

-spec get_port_ip(binary()) -> {pos_integer(), inet:ip_address()}.
get_port_ip(Host) ->
    Port = gen_mod:get_module_opt(Host, mod_proxy65, port),
    IP = case gen_mod:get_module_opt(Host, mod_proxy65, ip) of
	     undefined -> get_my_ip();
	     Addr -> Addr
	 end,
    {Port, IP}.

-spec get_my_ip() -> inet:ip_address().
get_my_ip() ->
    {ok, MyHostName} = inet:gethostname(),
    case inet:getaddr(MyHostName, inet) of
      {ok, Addr} -> Addr;
      {error, _} -> {127, 0, 0, 1}
    end.

max_connections(ServerHost) ->
    gen_mod:get_module_opt(ServerHost, mod_proxy65, max_connections).

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
