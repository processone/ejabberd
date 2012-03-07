%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_service.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : SOCKS5 Bytestreams XMPP service.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
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

-module(mod_proxy65_service).
-author('xram@jabber.ru').

-behaviour(gen_server).

%% gen_server callbacks.
-export([init/1,
	 handle_info/2,
	 handle_call/3,
	 handle_cast/2,
	 terminate/2,
	 code_change/3
	]).

%% API.
-export([start_link/2, add_listener/2, delete_listener/1]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

-define(PROCNAME, ejabberd_mod_proxy65_service).

-record(state, {
	  myhost,
	  serverhost,
	  name,
	  stream_addr,
	  port,
	  ip,
	  acl
	 }).


%%%------------------------
%%% gen_server callbacks
%%%------------------------

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

init([Host, Opts]) ->
    State = parse_options(Host, Opts),
    ejabberd_router:register_route(State#state.myhost),
    {ok, State}.

terminate(_Reason, #state{myhost=MyHost}) ->
    ejabberd_router:unregister_route(MyHost),
    ok.

handle_info({route, From, To, Packet}, State) when ?IS_IQ(Packet) ->
    IQ_Rec = exmpp_iq:xmlel_to_iq(Packet),
    case catch process_iq(From, IQ_Rec, State) of
	Result when ?IS_IQ_RECORD(Result) ->
	    ejabberd_router:route(To, From,
	      exmpp_iq:iq_to_xmlel(Result, To, From));
	{'EXIT', Reason} ->
	    ?ERROR_MSG("Error when processing IQ stanza: ~p", [Reason]),
	    Err = exmpp_iq:error(Packet, 'internal-server-error'),
	    ejabberd_router:route(To, From, Err);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(get_port_ip, _From, State) ->
    {reply, {port_ip, State#state.port, State#state.ip}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------
%%% Listener management
%%%------------------------

add_listener(Host, Opts) ->
    State = parse_options(Host, Opts),
    NewOpts = [Host | Opts],
    ejabberd_listener:add_listener({State#state.port, State#state.ip}, mod_proxy65_stream, NewOpts).

delete_listener(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    {port_ip, Port, IP} = gen_server:call(Proc, get_port_ip),
    catch ejabberd_listener:delete_listener({Port, IP}, mod_proxy65_stream).

%%%------------------------
%%% IQ Processing
%%%------------------------

%% disco#info request
process_iq(_, #iq{type = get, ns = ?NS_DISCO_INFO, lang = Lang} = IQ_Rec,
    #state{name=Name, serverhost = ServerHost}) ->
    ServerHostB = list_to_binary(ServerHost),
    Info = ejabberd_hooks:run_fold(
	     disco_info, ServerHostB, [], [ServerHost, ?MODULE, <<>>, ""]),
    Result = #xmlel{ns = ?NS_DISCO_INFO, name = 'query',
      children = iq_disco_info(Lang, Name)++Info},
    exmpp_iq:result(IQ_Rec, Result);

%% disco#items request
process_iq(_, #iq{type = get, ns = ?NS_DISCO_ITEMS} = IQ_Rec, _) ->
    Result = #xmlel{ns = ?NS_DISCO_ITEMS, name = 'query',
      children = []},
    exmpp_iq:result(IQ_Rec, Result);

%% vCard request
process_iq(_, #iq{type = get, ns = ?NS_VCARD, lang = Lang} = IQ_Rec, _) ->
    Result = #xmlel{ns = ?NS_VCARD, name = 'vCard',
      children = iq_vcard(Lang)},
    exmpp_iq:result(IQ_Rec, Result);

%% bytestreams info request
process_iq(JID, #iq{type = get, ns = ?NS_BYTESTREAMS} = IQ_Rec,
	   #state{acl = ACL, stream_addr = StreamAddr, serverhost = ServerHost}) ->
    case acl:match_rule(ServerHost, ACL, JID) of
	allow ->
	    StreamHostEl = [#xmlel{ns = ?NS_BYTESTREAMS, name = 'streamhost',
		attrs = StreamAddr}],
	    Result = #xmlel{ns = ?NS_BYTESTREAMS, name = 'query',
	      children = StreamHostEl},
	    exmpp_iq:result(IQ_Rec, Result);
	deny ->
	    exmpp_iq:error(IQ_Rec, 'forbidden')
    end;

%% bytestream activation request
process_iq(InitiatorJID, #iq{type = set, payload = SubEl, ns = ?NS_BYTESTREAMS} = IQ_Rec,
	   #state{acl = ACL, serverhost = ServerHost}) ->
    case acl:match_rule(ServerHost, ACL, InitiatorJID) of
	allow ->
	    ActivateEl = exmpp_xml:get_path(SubEl, [{element, 'activate'}]),
	    SID = exmpp_xml:get_attribute_as_list(SubEl, <<"sid">>, ""),
	    case catch exmpp_jid:parse(exmpp_xml:get_cdata_as_list(ActivateEl)) of
		TargetJID when ?IS_JID(TargetJID), SID /= "",
		               length(SID) =< 128, TargetJID /= InitiatorJID ->
		    Target = exmpp_jid:prep_to_list(TargetJID),
		    Initiator = exmpp_jid:prep_to_list(InitiatorJID),
		    SHA1 = sha:sha(SID ++ Initiator ++ Target),
		    case mod_proxy65_sm:activate_stream(SHA1, InitiatorJID, TargetJID, ServerHost) of
			ok ->
			    exmpp_iq:result(IQ_Rec);
			false ->
			    exmpp_iq:error(IQ_Rec, 'item-not-found');
			limit ->
			    exmpp_iq:error(IQ_Rec, 'resource-constraint');
			conflict ->
			    exmpp_iq:error(IQ_Rec, 'conflict');
			_ ->
			    exmpp_iq:error(IQ_Rec, 'internal-server-error')
		    end;
		_ ->
		    exmpp_iq:error(IQ_Rec, 'bad-request')
	    end;
	deny ->
	    exmpp_iq:error(IQ_Rec, 'forbidden')
    end;

%% Unknown "set" or "get" request
process_iq(_, #iq{kind=request} = IQ_Rec, _) ->
    exmpp_iq:error(IQ_Rec, 'service-unavailable');

%% IQ "result" or "error".
process_iq(_, _, _) ->
    ok.

%%%-------------------------
%%% Auxiliary functions.
%%%-------------------------
-define(FEATURE(Feat), #xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
    attrs = [?XMLATTR(<<"var">>, Feat)]}).

iq_disco_info(Lang, Name) ->
    [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
      [?XMLATTR(<<"category">>, <<"proxy">>),
       ?XMLATTR(<<"type">>, <<"bytestreams">>),
       ?XMLATTR(<<"name">>, translate:translate(Lang, Name))]},
     ?FEATURE(?NS_DISCO_INFO_s),
     ?FEATURE(?NS_VCARD_s),
     ?FEATURE(?NS_BYTESTREAMS_s)].

iq_vcard(Lang) ->
    [#xmlel{ns = ?NS_VCARD, name = 'FN', children =
      [#xmlcdata{cdata = <<"ejabberd/mod_proxy65">>}]},
     #xmlel{ns = ?NS_VCARD, name = 'URL', children =
      [#xmlcdata{cdata = list_to_binary(?EJABBERD_URI)}]},
     #xmlel{ns = ?NS_VCARD, name = 'DESC', children =
      [#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "ejabberd SOCKS5 Bytestreams module") ++
       "\nCopyright (c) 2002-2012 ProcessOne")}]}].

parse_options(ServerHost, Opts) ->
    MyHost = gen_mod:get_opt_host(ServerHost, Opts, "proxy.@HOST@"),
    Port = gen_mod:get_opt(port, Opts, 7777),
    ACL = gen_mod:get_opt(access, Opts, all),
    Name = gen_mod:get_opt(name, Opts, "SOCKS5 Bytestreams"),
    IP = case gen_mod:get_opt(ip, Opts, none) of
	     none -> get_my_ip();
	     Addr -> Addr
	 end,
    HostName = case gen_mod:get_opt(hostname, Opts, none) of
		   none ->
		       inet_parse:ntoa(IP);
		   HostAddr when is_tuple(HostAddr) ->
		       inet_parse:ntoa(HostAddr);
		   HostNameStr ->
		       HostNameStr
	     end,
    StreamAddr = [?XMLATTR(<<"jid">>, MyHost), ?XMLATTR(<<"host">>, HostName),
		  ?XMLATTR(<<"port">>, Port)],
    #state{myhost      = MyHost,
	   serverhost  = ServerHost,
	   name        = Name,
	   port        = Port,
	   ip          = IP,
	   stream_addr = StreamAddr, 
	   acl         = ACL}.

get_my_ip() ->
    {ok, MyHostName} = inet:gethostname(),
    case inet:getaddr(MyHostName, inet) of
        {ok, Addr} -> Addr;
        {error, _} -> {127,0,0,1}
    end.
