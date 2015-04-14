%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_service.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : SOCKS5 Bytestreams XMPP service.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
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

-module(mod_proxy65_service).

-author('xram@jabber.ru').

-behaviour(gen_server).

%% gen_server callbacks.
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

%% API.
-export([start_link/2, add_listener/2, transform_module_options/1,
	 delete_listener/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_proxy65_service).

-record(state,
	{myhost = <<"">>     :: binary(),
         serverhost = <<"">> :: binary(),
         name = <<"">>       :: binary(),
         stream_addr = []    :: [attr()],
         port = 0            :: inet:port_number(),
         ip = {127,0,0,1}    :: inet:ip_address(),
         acl = none          :: atom()}).

%%%------------------------
%%% gen_server callbacks
%%%------------------------

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

init([Host, Opts]) ->
    State = parse_options(Host, Opts),
    ejabberd_router:register_route(State#state.myhost),
    {ok, State}.

terminate(_Reason, #state{myhost = MyHost}) ->
    ejabberd_router:unregister_route(MyHost), ok.

handle_info({route, From, To,
	     #xmlel{name = <<"iq">>} = Packet},
	    State) ->
    IQ = jlib:iq_query_info(Packet),
    case catch process_iq(From, IQ, State) of
      Result when is_record(Result, iq) ->
	  ejabberd_router:route(To, From, jlib:iq_to_xml(Result));
      {'EXIT', Reason} ->
	  ?ERROR_MSG("Error when processing IQ stanza: ~p",
		     [Reason]),
	  Err = jlib:make_error_reply(Packet,
				      ?ERR_INTERNAL_SERVER_ERROR),
	  ejabberd_router:route(To, From, Err);
      _ -> ok
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_call(get_port_ip, _From, State) ->
    {reply, {port_ip, State#state.port, State#state.ip},
     State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%------------------------
%%% Listener management
%%%------------------------

add_listener(Host, Opts) ->
    State = parse_options(Host, Opts),
    NewOpts = [Host | Opts],
    ejabberd_listener:add_listener({State#state.port,
				    State#state.ip},
				   mod_proxy65_stream, NewOpts).

delete_listener(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    {port_ip, Port, IP} = gen_server:call(Proc,
					  get_port_ip),
    catch ejabberd_listener:delete_listener({Port, IP},
					    mod_proxy65_stream).

%%%------------------------
%%% IQ Processing
%%%------------------------

%% disco#info request
process_iq(_,
	   #iq{type = get, xmlns = ?NS_DISCO_INFO, lang = Lang} =
	       IQ,
	   #state{name = Name, serverhost = ServerHost}) ->
    Info = ejabberd_hooks:run_fold(disco_info, ServerHost,
				   [], [ServerHost, ?MODULE, <<"">>, <<"">>]),
    IQ#iq{type = result,
	  sub_el =
	      [#xmlel{name = <<"query">>,
		      attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}],
		      children = iq_disco_info(Lang, Name) ++ Info}]};
%% disco#items request
process_iq(_,
	   #iq{type = get, xmlns = ?NS_DISCO_ITEMS} = IQ, _) ->
    IQ#iq{type = result,
	  sub_el =
	      [#xmlel{name = <<"query">>,
		      attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}],
		      children = []}]};
%% vCard request
process_iq(_,
	   #iq{type = get, xmlns = ?NS_VCARD, lang = Lang} = IQ,
	   _) ->
    IQ#iq{type = result,
	  sub_el =
	      [#xmlel{name = <<"vCard">>,
		      attrs = [{<<"xmlns">>, ?NS_VCARD}],
		      children = iq_vcard(Lang)}]};
%% bytestreams info request
process_iq(JID,
	   #iq{type = get, sub_el = SubEl,
	       xmlns = ?NS_BYTESTREAMS} =
	       IQ,
	   #state{acl = ACL, stream_addr = StreamAddr,
		  serverhost = ServerHost}) ->
    case acl:match_rule(ServerHost, ACL, JID) of
      allow ->
	  StreamHostEl = [#xmlel{name = <<"streamhost">>,
				 attrs = StreamAddr, children = []}],
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs = [{<<"xmlns">>, ?NS_BYTESTREAMS}],
			    children = StreamHostEl}]};
      deny ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
    end;
%% bytestream activation request
process_iq(InitiatorJID,
	   #iq{type = set, sub_el = SubEl,
	       xmlns = ?NS_BYTESTREAMS} =
	       IQ,
	   #state{acl = ACL, serverhost = ServerHost}) ->
    case acl:match_rule(ServerHost, ACL, InitiatorJID) of
      allow ->
	  ActivateEl = xml:get_path_s(SubEl,
				      [{elem, <<"activate">>}]),
	  SID = xml:get_tag_attr_s(<<"sid">>, SubEl),
	  case catch
		 jlib:string_to_jid(xml:get_tag_cdata(ActivateEl))
	      of
	    TargetJID
		when is_record(TargetJID, jid), SID /= <<"">>,
		     byte_size(SID) =< 128, TargetJID /= InitiatorJID ->
		Target =
		    jlib:jid_to_string(jlib:jid_tolower(TargetJID)),
		Initiator =
		    jlib:jid_to_string(jlib:jid_tolower(InitiatorJID)),
		SHA1 = p1_sha:sha(<<SID/binary, Initiator/binary, Target/binary>>),
		case mod_proxy65_sm:activate_stream(SHA1, InitiatorJID,
						    TargetJID, ServerHost)
		    of
		  ok -> IQ#iq{type = result, sub_el = []};
		  false ->
		      IQ#iq{type = error,
			    sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
		  limit ->
		      IQ#iq{type = error,
			    sub_el = [SubEl, ?ERR_RESOURCE_CONSTRAINT]};
		  conflict ->
		      IQ#iq{type = error, sub_el = [SubEl, ?ERR_CONFLICT]};
		  _ ->
		      IQ#iq{type = error,
			    sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
		end;
	    _ ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
	  end;
      deny ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
    end;
%% Unknown "set" or "get" request
process_iq(_, #iq{type = Type, sub_el = SubEl} = IQ, _)
    when Type == get; Type == set ->
    IQ#iq{type = error,
	  sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
%% IQ "result" or "error".
process_iq(_, _, _) -> ok.

%%%-------------------------
%%% Auxiliary functions.
%%%-------------------------
-define(FEATURE(Feat),
	#xmlel{name = <<"feature">>,
	       attrs = [{<<"var">>, Feat}], children = []}).

iq_disco_info(Lang, Name) ->
    [#xmlel{name = <<"identity">>,
	    attrs =
		[{<<"category">>, <<"proxy">>},
		 {<<"type">>, <<"bytestreams">>},
		 {<<"name">>, translate:translate(Lang, Name)}],
	    children = []},
     ?FEATURE((?NS_DISCO_INFO)), ?FEATURE((?NS_VCARD)),
     ?FEATURE((?NS_BYTESTREAMS))].

iq_vcard(Lang) ->
    [#xmlel{name = <<"FN">>, attrs = [],
	    children = [{xmlcdata, <<"ejabberd/mod_proxy65">>}]},
     #xmlel{name = <<"URL">>, attrs = [],
	    children = [{xmlcdata, ?EJABBERD_URI}]},
     #xmlel{name = <<"DESC">>, attrs = [],
	    children =
		[{xmlcdata,
		  <<(translate:translate(Lang,
					 <<"ejabberd SOCKS5 Bytestreams module">>))/binary,
		    "\nCopyright (c) 2003-2015 ProcessOne">>}]}].

parse_options(ServerHost, Opts) ->
    MyHost = gen_mod:get_opt_host(ServerHost, Opts,
				  <<"proxy.@HOST@">>),
    Port = gen_mod:get_opt(port, Opts,
                           fun(P) when is_integer(P), P>0, P<65536 -> P end,
                           7777),
    ACL = gen_mod:get_opt(access, Opts, fun(A) when is_atom(A) -> A end,
                          all),
    Name = gen_mod:get_opt(name, Opts, fun iolist_to_binary/1,
			   <<"SOCKS5 Bytestreams">>),
    IP = gen_mod:get_opt(ip, Opts,
                         fun(S) ->
                                 {ok, Addr} = inet_parse:address(
                                                binary_to_list(
                                                  iolist_to_binary(S))),
                                 Addr
                         end, get_my_ip()),
    HostName = gen_mod:get_opt(hostname, Opts,
                               fun iolist_to_binary/1,
                               jlib:ip_to_list(IP)),
    StreamAddr = [{<<"jid">>, MyHost},
		  {<<"host">>, HostName},
		  {<<"port">>, jlib:integer_to_binary(Port)}],
    #state{myhost = MyHost, serverhost = ServerHost,
	   name = Name, port = Port, ip = IP,
	   stream_addr = StreamAddr, acl = ACL}.

transform_module_options(Opts) ->
    lists:map(
      fun({ip, IP}) when is_tuple(IP) ->
              {ip, jlib:ip_to_list(IP)};
         ({hostname, IP}) when is_tuple(IP) ->
              {hostname, jlib:ip_to_list(IP)};
         (Opt) ->
              Opt
      end, Opts).

get_my_ip() ->
    {ok, MyHostName} = inet:gethostname(),
    case inet:getaddr(MyHostName, inet) of
      {ok, Addr} -> Addr;
      {error, _} -> {127, 0, 0, 1}
    end.
