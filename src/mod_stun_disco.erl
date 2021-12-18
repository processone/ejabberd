%%%----------------------------------------------------------------------
%%% File    : mod_stun_disco.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : External Service Discovery (XEP-0215)
%%% Created : 18 Apr 2020 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2020-2021   ProcessOne
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

-module(mod_stun_disco).
-author('holger@zedat.fu-berlin.de').
-protocol({xep, 215, '0.7'}).

-behaviour(gen_server).
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2,
	 stop/1,
	 reload/3,
	 mod_opt_type/1,
	 mod_options/1,
	 depends/2]).
-export([mod_doc/0]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% ejabberd_hooks callbacks.
-export([disco_local_features/5, stun_get_password/3]).

%% gen_iq_handler callback.
-export([process_iq/1]).

-include("logger.hrl").
-include("translate.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(STUN_MODULE, ejabberd_stun).

-type host_or_hash() :: binary() | {hash, binary()}.
-type service_type() :: stun | stuns | turn | turns | undefined.

-record(request,
	{host       :: binary() | inet:ip_address() | undefined,
	 port       :: 0..65535 | undefined,
	 transport  :: udp | tcp | undefined,
	 type       :: service_type(),
	 restricted :: true | undefined}).

-record(state,
	{host     :: binary(),
	 services :: [service()],
	 secret   :: binary(),
	 ttl      :: non_neg_integer()}).

-type request() :: #request{}.
-type state() :: #state{}.

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> {ok, pid()} | {error, any()}.
start(Host, Opts) ->
    Proc = get_proc_name(Host),
    gen_mod:start_child(?MODULE, Host, Opts, Proc).

-spec stop(binary()) -> ok | {error, any()}.
stop(Host) ->
    Proc = get_proc_name(Host),
    gen_mod:stop_child(Proc).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, OldOpts) ->
    cast(Host, {reload, NewOpts, OldOpts}).

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(access) ->
    econf:acl();
mod_opt_type(credentials_lifetime) ->
    econf:timeout(second);
mod_opt_type(offer_local_services) ->
    econf:bool();
mod_opt_type(secret) ->
    econf:binary();
mod_opt_type(services) ->
    econf:list(
      econf:and_then(
	econf:options(
	  #{host => econf:either(econf:ip(), econf:binary()),
	    port => econf:port(),
	    type => econf:enum([stun, turn, stuns, turns]),
	    transport => econf:enum([tcp, udp]),
	    restricted => econf:bool()},
	  [{required, [host]}]),
	  fun(Opts) ->
		  DefPort = fun(stun) -> 3478;
			       (turn) -> 3478;
			       (stuns) -> 5349;
			       (turns) -> 5349
			    end,
		  DefTrns = fun(stun) -> udp;
			       (turn) -> udp;
			       (stuns) -> tcp;
			       (turns) -> tcp
			    end,
		  DefRstr = fun(stun) -> false;
			       (turn) -> true;
			       (stuns) -> false;
			       (turns) -> true
			    end,
		  Host = proplists:get_value(host, Opts),
		  Type = proplists:get_value(type, Opts, stun),
		  Port = proplists:get_value(port, Opts, DefPort(Type)),
		  Trns = proplists:get_value(transport, Opts, DefTrns(Type)),
		  Rstr = proplists:get_value(restricted, Opts, DefRstr(Type)),
		  #service{host = Host,
			   port = Port,
			   type = Type,
			   transport = Trns,
			   restricted = Rstr}
	  end)).

-spec mod_options(binary()) -> [{services, [tuple()]} | {atom(), any()}].
mod_options(_Host) ->
    [{access, local},
     {credentials_lifetime, timer:hours(12)},
     {offer_local_services, true},
     {secret, undefined},
     {services, []}].

mod_doc() ->
    #{desc =>
	  ?T("This module allows XMPP clients to discover STUN/TURN services "
	     "and to obtain temporary credentials for using them as per "
	     "https://xmpp.org/extensions/xep-0215.html"
	     "[XEP-0215: External Service Discovery]. "
	     "This module is included in ejabberd since version 20.04."),
      opts =>
	  [{access,
	    #{value => ?T("AccessName"),
	      desc =>
		  ?T("This option defines which access rule will be used to "
		     "control who is allowed to discover STUN/TURN services "
		     "and to request temporary credentials. The default value "
		     "is 'local'.")}},
	   {credentials_lifetime,
	    #{value => "timeout()",
	      desc =>
		  ?T("The lifetime of temporary credentials offered to "
		     "clients. If ejabberd's built-in TURN service is used, "
		     "TURN relays allocated using temporary credentials will "
		     "be terminated shortly after the credentials expired. The "
		     "default value is '12 hours'. Note that restarting the "
		     "ejabberd node invalidates any temporary credentials "
		     "offered before the restart unless a 'secret' is "
		     "specified (see below).")}},
	   {offer_local_services,
	    #{value => "true | false",
	      desc =>
		  ?T("This option specifies whether local STUN/TURN services "
		     "configured as ejabberd listeners should be announced "
		     "automatically. Note that this will not include "
		     "TLS-enabled services, which must be configured manually "
		     "using the 'services' option (see below). For "
		     "non-anonymous TURN services, temporary credentials will "
		     "be offered to the client. The default value is "
		     "'true'.")}},
	   {secret,
	    #{value => ?T("Text"),
	      desc =>
		  ?T("The secret used for generating temporary credentials. If "
		     "this option isn't specified, a secret will be "
		     "auto-generated. However, a secret must be specified "
		     "explicitly if non-anonymous TURN services running on "
		     "other ejabberd nodes and/or external TURN 'services' are "
		     "configured. Also note that auto-generated secrets are "
		     "lost when the node is restarted, which invalidates any "
		     "credentials offered before the restart. Therefore, it's "
		     "recommended to explicitly specify a secret if clients "
		     "cache retrieved credentials (for later use) across "
		     "service restarts.")}},
	   {services,
	    #{value => "[Service, ...]",
	      example =>
		  ["services:",
		   "  -",
		   "    host: 203.0.113.3",
		   "    port: 3478",
		   "    type: stun",
		   "    transport: udp",
		   "    restricted: false",
		   "  -",
		   "    host: 203.0.113.3",
		   "    port: 3478",
		   "    type: turn",
		   "    transport: udp",
		   "    restricted: true",
		   "  -",
		   "    host: 2001:db8::3",
		   "    port: 3478",
		   "    type: stun",
		   "    transport: udp",
		   "    restricted: false",
		   "  -",
		   "    host: 2001:db8::3",
		   "    port: 3478",
		   "    type: turn",
		   "    transport: udp",
		   "    restricted: true",
		   "  -",
		   "    host: server.example.com",
		   "    port: 5349",
		   "    type: turns",
		   "    transport: tcp",
		   "    restricted: true"],
	      desc =>
		  ?T("The list of services offered to clients. This list can "
		     "include STUN/TURN services running on any ejabberd node "
		     "and/or external services. However, if any listed TURN "
		     "service not running on the local ejabberd node requires "
		     "authentication, a 'secret' must be specified explicitly, "
		     "and must be shared with that service. This will only "
		     "work with ejabberd's built-in STUN/TURN server and with "
		     "external servers that support the same "
		     "https://tools.ietf.org/html/draft-uberti-behave-turn-rest-00"
		     "[REST API For Access To TURN Services]. Unless the "
		     "'offer_local_services' is set to 'false', the explicitly "
		     "listed services will be offered in addition to those "
		     "announced automatically.")},
	    [{host,
	      #{value => ?T("Host"),
		desc =>
		    ?T("The hostname or IP address the STUN/TURN service is "
		       "listening on. For non-TLS services, it's recommended "
		       "to specify an IP address (to avoid additional DNS "
		       "lookup latency on the client side). For TLS services, "
		       "the hostname (or IP address) should match the "
		       "certificate. Specifying the 'host' option is "
		       "mandatory.")}},
	     {port,
	      #{value => "1..65535",
		desc =>
		    ?T("The port number the STUN/TURN service is listening "
		       "on. The default port number is 3478 for non-TLS "
		       "services and 5349 for TLS services.")}},
	     {type,
	      #{value => "stun | turn | stuns | turns",
		desc =>
		    ?T("The type of service. Must be 'stun' or 'turn' for "
		       "non-TLS services, 'stuns' or 'turns' for TLS services. "
		       "The default type is 'stun'.")}},
	     {transport,
	      #{value => "tcp | udp",
		desc =>
		    ?T("The transport protocol supported by the service. The "
		       "default is 'udp' for non-TLS services and 'tcp' for "
		       "TLS services.")}},
	     {restricted,
	      #{value => "true | false",
		desc =>
		    ?T("This option determines whether temporary credentials "
		       "for accessing the service are offered. The default is "
		       "'false' for STUN/STUNS services and 'true' for "
		       "TURN/TURNS services.")}}]}]}.

%%--------------------------------------------------------------------
%% gen_server callbacks.
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, state()}.
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    Services = get_configured_services(Opts),
    Secret = get_configured_secret(Opts),
    TTL = get_configured_ttl(Opts),
    register_iq_handlers(Host),
    register_hooks(Host),
    {ok, #state{host = Host, services = Services, secret = Secret, ttl = TTL}}.

-spec handle_call(term(), {pid(), term()}, state())
      -> {reply, {turn_disco, [service()] | binary()}, state()} |
	 {noreply, state()}.
handle_call({get_services, JID, #request{host = ReqHost,
					 port = ReqPort,
					 type = ReqType,
					 transport = ReqTrns,
					 restricted = ReqRstr}}, _From,
	    #state{host = Host,
		   services = List0,
		   secret = Secret,
		   ttl = TTL} = State) ->
    ?DEBUG("Getting STUN/TURN service list for ~ts", [jid:encode(JID)]),
    Hash = <<(hash(jid:encode(JID)))/binary, (hash(Host))/binary>>,
    List = lists:filtermap(
	     fun(#service{host = H, port = P, type = T, restricted = R})
		   when (ReqHost /= undefined) and (H /= ReqHost);
			(ReqPort /= undefined) and (P /= ReqPort);
			(ReqType /= undefined) and (T /= ReqType);
			(ReqTrns /= undefined) and (T /= ReqTrns);
			(ReqRstr /= undefined) and (R /= ReqRstr) ->
		     false;
		(#service{restricted = false}) ->
		     true;
		(#service{restricted = true} = Service) ->
		     {true, add_credentials(Service, Hash, Secret, TTL)}
	     end, List0),
    ?INFO_MSG("Offering STUN/TURN services to ~ts (~s)",
	      [jid:encode(JID), Hash]),
    {reply, {turn_disco, List}, State};
handle_call({get_password, Username}, _From, #state{secret = Secret} = State) ->
    ?DEBUG("Getting STUN/TURN password for ~ts", [Username]),
    Password = make_password(Username, Secret),
    {reply, {turn_disco, Password}, State};
handle_call(Request, From, State) ->
    ?ERROR_MSG("Got unexpected request from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({reload, NewOpts, _OldOpts}, #state{host = Host} = State) ->
    ?DEBUG("Reloading STUN/TURN discovery configuration for ~ts", [Host]),
    Services = get_configured_services(NewOpts),
    Secret = get_configured_secret(NewOpts),
    TTL = get_configured_ttl(NewOpts),
    {noreply, State#state{services = Services, secret = Secret, ttl = TTL}};
handle_cast(Request, State) ->
    ?ERROR_MSG("Got unexpected request: ~p", [Request]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    ?ERROR_MSG("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> ok.
terminate(Reason, #state{host = Host}) ->
    ?DEBUG("Stopping STUN/TURN discovery process for ~ts: ~p",
	   [Host, Reason]),
    unregister_hooks(Host),
    unregister_iq_handlers(Host).

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{host = Host} = State, _Extra) ->
    ?DEBUG("Updating STUN/TURN discovery process for ~ts", [Host]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Register/unregister hooks.
%%--------------------------------------------------------------------
-spec register_hooks(binary()) -> ok.
register_hooks(Host) ->
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
		       disco_local_features, 50),
    ejabberd_hooks:add(stun_get_password, ?MODULE,
		       stun_get_password, 50).

-spec unregister_hooks(binary()) -> ok.
unregister_hooks(Host) ->
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE,
			  disco_local_features, 50),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	false ->
	    ejabberd_hooks:delete(stun_get_password, ?MODULE,
				  stun_get_password, 50);
	true ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------
-spec disco_local_features(mod_disco:features_acc(), jid(), jid(), binary(),
			   binary()) -> mod_disco:features_acc().
disco_local_features(empty, From, To, Node, Lang) ->
    disco_local_features({result, []}, From, To, Node, Lang);
disco_local_features({result, OtherFeatures} = Acc, From,
		     #jid{lserver = LServer}, <<"">>, _Lang) ->
    Access = mod_stun_disco_opt:access(LServer),
    case acl:match_rule(LServer, Access, From) of
	allow ->
	    ?DEBUG("Announcing feature to ~ts", [jid:encode(From)]),
	    {result, [?NS_EXTDISCO_2 | OtherFeatures]};
	deny ->
	    ?DEBUG("Not announcing feature to ~ts", [jid:encode(From)]),
	    Acc
    end;
disco_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec stun_get_password(any(), binary(), binary())
      -> binary() | {stop, binary()}.
stun_get_password(<<>>, Username, _Realm) ->
    case binary:split(Username, <<$:>>) of
	[Expiration, <<_UserHash:8/binary, HostHash:8/binary>>] ->
	    try binary_to_integer(Expiration) of
		ExpireTime ->
		    case erlang:system_time(second) of
			Now when Now < ExpireTime ->
			    ?DEBUG("Looking up password for: ~ts", [Username]),
			    {stop, get_password(Username, HostHash)};
			Now when Now >= ExpireTime ->
			    ?INFO_MSG("Credentials expired: ~ts", [Username]),
			    {stop, <<>>}
		    end
	    catch _:badarg ->
		    ?DEBUG("Non-numeric expiration field: ~ts", [Username]),
		    <<>>
	    end;
	_ ->
	    ?DEBUG("Not an ephemeral username: ~ts", [Username]),
	    <<>>
    end;
stun_get_password(Acc, _Username, _Realm) ->
    Acc.

%%--------------------------------------------------------------------
%% IQ handlers.
%%--------------------------------------------------------------------
-spec register_iq_handlers(binary()) -> ok.
register_iq_handlers(Host) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_EXTDISCO_2, ?MODULE, process_iq).

-spec unregister_iq_handlers(binary()) -> ok.
unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_EXTDISCO_2).

-spec process_iq(iq()) -> iq().
process_iq(#iq{type = get,
	       sub_els = [#services{type = ReqType}]} = IQ) ->
    Request = #request{type = ReqType},
    process_iq_get(IQ, Request);
process_iq(#iq{type = get,
	       sub_els = [#credentials{
			     services = [#service{
					    host = ReqHost,
					    port = ReqPort,
					    type = ReqType,
					    transport = ReqTrns,
					    name = <<>>,
					    username = <<>>,
					    password = <<>>,
					    expires = undefined,
					    restricted = undefined,
					    action = undefined,
					    xdata = undefined}]}]} = IQ) ->
    % Accepting the 'transport' request attribute is an ejabberd extension.
    Request = #request{host = ReqHost,
		       port = ReqPort,
		       type = ReqType,
		       transport = ReqTrns,
		       restricted = true},
    process_iq_get(IQ, Request);
process_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_iq(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_iq_get(iq(), request()) -> iq().
process_iq_get(#iq{from = From, to = #jid{lserver = Host}, lang = Lang} = IQ,
	       Request) ->
    Access = mod_stun_disco_opt:access(Host),
    case acl:match_rule(Host, Access, From) of
        allow ->
	    ?DEBUG("Performing external service discovery for ~ts",
		   [jid:encode(From)]),
	    case get_services(Host, From, Request) of
		{ok, Services} ->
		    xmpp:make_iq_result(IQ, #services{list = Services});
		{error, timeout} -> % Has been logged already.
		    Txt = ?T("Service list retrieval timed out"),
		    Err = xmpp:err_internal_server_error(Txt, Lang),
		    xmpp:make_error(IQ, Err)
	    end;
	deny ->
	    ?DEBUG("Won't perform external service discovery for ~ts",
		   [jid:encode(From)]),
	    Txt = ?T("Access denied by service policy"),
	    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang))
    end.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec get_configured_services(gen_mod:opts()) -> [service()].
get_configured_services(Opts) ->
    LocalServices = case mod_stun_disco_opt:offer_local_services(Opts) of
			true ->
			    ?DEBUG("Discovering local services", []),
			    find_local_services();
			false ->
			    ?DEBUG("Won't discover local services", []),
			    []
		    end,
    dedup(LocalServices ++ mod_stun_disco_opt:services(Opts)).

-spec get_configured_secret(gen_mod:opts()) -> binary().
get_configured_secret(Opts) ->
    case mod_stun_disco_opt:secret(Opts) of
	undefined ->
	    ?DEBUG("Auto-generating secret", []),
	    new_secret();
	Secret ->
	    ?DEBUG("Using configured secret", []),
	    Secret
    end.

-spec get_configured_ttl(gen_mod:opts()) -> non_neg_integer().
get_configured_ttl(Opts) ->
    mod_stun_disco_opt:credentials_lifetime(Opts) div 1000.

-spec new_secret() -> binary().
new_secret() ->
    p1_rand:bytes(20).

-spec add_credentials(service(), binary(), binary(), non_neg_integer())
      -> service().
add_credentials(Service, Hash, Secret, TTL) ->
    ExpireAt = erlang:system_time(second) + TTL,
    Username = make_username(ExpireAt, Hash),
    Password = make_password(Username, Secret),
    ?DEBUG("Created ephemeral credentials: ~s | ~s", [Username, Password]),
    Service#service{username = Username,
		    password = Password,
		    expires = seconds_to_timestamp(ExpireAt)}.

-spec make_username(non_neg_integer(), binary()) -> binary().
make_username(ExpireAt, Hash) ->
    <<(integer_to_binary(ExpireAt))/binary, $:, Hash/binary>>.

-spec make_password(binary(), binary()) -> binary().
make_password(Username, Secret) ->
    base64:encode(misc:crypto_hmac(sha, Secret, Username)).

-spec get_password(binary(), binary()) -> binary().
get_password(Username, HostHash) ->
    try call({hash, HostHash}, {get_password, Username}) of
	{turn_disco, Password} ->
	    Password
    catch
	exit:{timeout, _} ->
	    ?ERROR_MSG("Asking ~ts for password timed out", [HostHash]),
	    <<>>;
	exit:{noproc, _} -> % Can be triggered by bogus Username.
	    ?DEBUG("Cannot retrieve password for ~ts", [Username]),
	    <<>>
    end.

-spec get_services(binary(), jid(), request())
      -> {ok, [service()]} | {error, timeout}.
get_services(Host, JID, Request) ->
    try call(Host, {get_services, JID, Request}) of
	{turn_disco, Services} ->
	    {ok, Services}
    catch
	exit:{timeout, _} ->
	    ?ERROR_MSG("Asking ~ts for services timed out", [Host]),
	    {error, timeout}
    end.

-spec find_local_services() -> [service()].
find_local_services() ->
    ParseListener = fun(Listener) -> parse_listener(Listener) end,
    lists:flatmap(ParseListener, ejabberd_option:listen()).

-spec parse_listener(ejabberd_listener:listener()) -> [service()].
parse_listener({_EndPoint, ?STUN_MODULE, #{tls := true}}) ->
    ?DEBUG("Ignoring TLS-enabled STUN/TURN listener", []),
    []; % Avoid certificate hostname issues.
parse_listener({{Port, _Addr, Transport}, ?STUN_MODULE, Opts}) ->
    case get_listener_ips(Opts) of
	{undefined, undefined} ->
	    ?INFO_MSG("Won't auto-announce STUN/TURN service on port ~B (~s) "
		      "without public IP address, please specify "
		      "'turn_ipv4_address' and optionally 'turn_ipv6_address'",
		      [Port, Transport]),
	    [];
	{IPv4Addr, IPv6Addr}  ->
	    lists:flatmap(
	      fun(undefined) ->
		      [];
		 (Addr) ->
		      StunService = #service{host = Addr,
					     port = Port,
					     transport = Transport,
					     restricted = false,
					     type = stun},
		      case Opts of
			  #{use_turn := true} ->
			      ?INFO_MSG("Going to offer STUN/TURN service: "
					"~s (~s)",
					[addr_to_str(Addr, Port), Transport]),
			      [StunService,
			       #service{host = Addr,
					port = Port,
					transport = Transport,
					restricted = is_restricted(Opts),
					type = turn}];
			  #{use_turn := false} ->
			      ?INFO_MSG("Going to offer STUN service: "
					"~s (~s)",
					[addr_to_str(Addr, Port), Transport]),
			      [StunService]
		      end
	      end, [IPv4Addr, IPv6Addr])
    end;
parse_listener({_EndPoint, Module, _Opts}) ->
    ?DEBUG("Ignoring ~s listener", [Module]),
    [].

-spec get_listener_ips(map()) -> {inet:ip4_address() | undefined,
				  inet:ip6_address() | undefined}.
get_listener_ips(#{ip := {0, 0, 0, 0}} = Opts) ->
    {get_turn_ipv4_addr(Opts), undefined};
get_listener_ips(#{ip := {0, 0, 0, 0, 0, 0, 0, 0}} = Opts) ->
    {get_turn_ipv4_addr(Opts), get_turn_ipv6_addr(Opts)}; % Assume dual-stack.
get_listener_ips(#{ip := {127, _, _, _}} = Opts) ->
    {get_turn_ipv4_addr(Opts), undefined};
get_listener_ips(#{ip := {0, 0, 0, 0, 0, 0, 0, 1}} = Opts) ->
    {undefined, get_turn_ipv6_addr(Opts)};
get_listener_ips(#{ip := {_, _, _, _} = IP}) ->
    {IP, undefined};
get_listener_ips(#{ip := {_, _, _, _, _, _, _, _} = IP}) ->
    {undefined, IP}.

-spec get_turn_ipv4_addr(map()) -> inet:ip4_address() | undefined.
get_turn_ipv4_addr(#{turn_ipv4_address := {_, _, _, _} = TurnIP}) ->
    TurnIP;
get_turn_ipv4_addr(#{turn_ipv4_address := undefined}) ->
    case misc:get_my_ipv4_address() of
	{127, _, _, _} ->
	    undefined;
	IP ->
	    IP
    end.

-spec get_turn_ipv6_addr(map()) -> inet:ip6_address() | undefined.
get_turn_ipv6_addr(#{turn_ipv6_address := {_, _, _, _, _, _, _, _} = TurnIP}) ->
    TurnIP;
get_turn_ipv6_addr(#{turn_ipv6_address := undefined}) ->
    case misc:get_my_ipv6_address() of
	{0, 0, 0, 0, 0, 0, 0, 1} ->
	    undefined;
	IP ->
	    IP
    end.

-spec is_restricted(map()) -> boolean().
is_restricted(#{auth_type := user}) ->
    true;
is_restricted(#{auth_type := anonymous}) ->
    false.

-spec call(host_or_hash(), term()) -> term().
call(Host, Request) ->
    Proc = get_proc_name(Host),
    gen_server:call(Proc, Request, timer:seconds(15)).

-spec cast(host_or_hash(), term()) -> ok.
cast(Host, Request) ->
    Proc = get_proc_name(Host),
    gen_server:cast(Proc, Request).

-spec get_proc_name(host_or_hash()) -> atom().
get_proc_name(Host) when is_binary(Host) ->
    get_proc_name({hash, hash(Host)});
get_proc_name({hash, HostHash}) ->
    gen_mod:get_module_proc(HostHash, ?MODULE).

-spec hash(binary()) -> binary().
hash(Host) ->
    str:to_hexlist(binary_part(crypto:hash(sha, Host), 0, 4)).

-spec dedup(list()) -> list().
dedup([]) -> [];
dedup([H | T]) -> [H | [E || E <- dedup(T), E /= H]].

-spec seconds_to_timestamp(non_neg_integer()) -> erlang:timestamp().
seconds_to_timestamp(Seconds) ->
    {Seconds div 1000000, Seconds rem 1000000, 0}.

-spec addr_to_str(inet:ip_address(), 0..65535) -> iolist().
addr_to_str({_, _, _, _, _, _, _, _} = Addr, Port) ->
    [$[, inet_parse:ntoa(Addr), $], $:, integer_to_list(Port)];
addr_to_str({_, _, _, _} = Addr, Port) ->
    [inet_parse:ntoa(Addr), $:, integer_to_list(Port)].
