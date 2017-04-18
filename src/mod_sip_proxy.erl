%%%-------------------------------------------------------------------
%%% File    : mod_sip_proxy.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : 
%%% Created : 21 Apr 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2014-2017   ProcessOne
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

%%%-------------------------------------------------------------------
-module(mod_sip_proxy).

-behaviour(ejabberd_config).

-define(GEN_FSM, p1_fsm).
-behaviour(?GEN_FSM).

%% API
-export([start/2, start_link/2, route/3, route/4]).

-export([init/1, wait_for_request/2,
	 wait_for_response/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3,
	 code_change/4, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("esip/include/esip.hrl").

-define(SIGN_LIFETIME, 300). %% in seconds.

-record(state, {host = <<"">>  :: binary(),
		opts = []      :: [{certfile, binary()}],
		orig_trid,
		responses = [] :: [#sip{}],
		tr_ids = []    :: list(),
		orig_req = #sip{} :: #sip{}}).

%%%===================================================================
%%% API
%%%===================================================================
start(LServer, Opts) ->
    supervisor:start_child(mod_sip_proxy_sup, [LServer, Opts]).

start_link(LServer, Opts) ->
    ?GEN_FSM:start_link(?MODULE, [LServer, Opts], []).

route(SIPMsg, _SIPSock, TrID, Pid) ->
    ?GEN_FSM:send_event(Pid, {SIPMsg, TrID}).

route(#sip{hdrs = Hdrs} = Req, LServer, Opts) ->
    case proplists:get_bool(authenticated, Opts) of
	true ->
	    route_statelessly(Req, LServer, Opts);
	false ->
	    ConfiguredRRoute = get_configured_record_route(LServer),
	    case esip:get_hdrs('route', Hdrs) of
		[{_, URI, _}|_] ->
		    case cmp_uri(URI, ConfiguredRRoute) of
			true ->
			    case is_signed_by_me(URI#uri.user, Hdrs) of
				true ->
				    route_statelessly(Req, LServer, Opts);
				false ->
				    error
			    end;
			false ->
			    error
		    end;
		[] ->
		    error
	    end
    end.

route_statelessly(Req, LServer, Opts) ->
    Req1 = prepare_request(LServer, Req),
    case connect(Req1, add_certfile(LServer, Opts)) of
	{ok, SIPSocketsWithURIs} ->
	    lists:foreach(
	      fun({SIPSocket, _URI}) ->
		      Req2 = add_via(SIPSocket, LServer, Req1),
		      esip:send(SIPSocket, Req2)
	      end, SIPSocketsWithURIs);
	_ ->
	    error
    end.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([Host, Opts]) ->
    Opts1 = add_certfile(Host, Opts),
    {ok, wait_for_request, #state{opts = Opts1, host = Host}}.

wait_for_request({#sip{type = request} = Req, TrID}, State) ->
    Opts = State#state.opts,
    Req1 = prepare_request(State#state.host, Req),
    case connect(Req1, Opts) of
	{ok, SIPSocketsWithURIs} ->
	    NewState =
		lists:foldl(
		  fun(_SIPSocketWithURI, {error, _} = Err) ->
			  Err;
		     ({SIPSocket, URI}, #state{tr_ids = TrIDs} = AccState) ->
			  Req2 = add_record_route_and_set_uri(
				   URI, State#state.host, Req1),
			  Req3 = add_via(SIPSocket, State#state.host, Req2),
			  case esip:request(SIPSocket, Req3,
					    {?MODULE, route, [self()]}) of
			      {ok, ClientTrID} ->
				  NewTrIDs = [ClientTrID|TrIDs],
				  AccState#state{tr_ids = NewTrIDs};
			      Err ->
				  cancel_pending_transactions(AccState),
				  Err
			  end
		  end, State, SIPSocketsWithURIs),
	    case NewState of
		{error, _} = Err ->
		    {Status, Reason} = esip:error_status(Err),
		    esip:reply(TrID, mod_sip:make_response(
				       Req, #sip{type = response,
						 status = Status,
						 reason = Reason})),
		    {stop, normal, State};
		_ ->
		    {next_state, wait_for_response,
		     NewState#state{orig_req = Req, orig_trid = TrID}}
	    end;
	{error, notfound} ->
	    esip:reply(TrID, mod_sip:make_response(
			       Req, #sip{type = response,
					 status = 480,
					 reason = esip:reason(480)})),
	    {stop, normal, State};
	Err ->
	    {Status, Reason} = esip:error_status(Err),
	    esip:reply(TrID, mod_sip:make_response(
			       Req, #sip{type = response,
					 status = Status,
					 reason = Reason})),
	    {stop, normal, State}
    end;
wait_for_request(_Event, State) ->
    {next_state, wait_for_request, State}.

wait_for_response({#sip{method = <<"CANCEL">>, type = request}, _TrID}, State) ->
    cancel_pending_transactions(State),
    {next_state, wait_for_response, State};
wait_for_response({Resp, TrID},
		  #state{orig_req = #sip{method = Method} = Req} = State) ->
    case Resp of
	{error, timeout} when Method /= <<"INVITE">> ->
	    %% Absorb useless 408. See RFC4320
	    choose_best_response(State),
	    esip:stop_transaction(State#state.orig_trid),
	    {stop, normal, State};
	{error, _} ->
	    {Status, Reason} = esip:error_status(Resp),
	    State1 = mark_transaction_as_complete(TrID, State),
	    SIPResp = mod_sip:make_response(Req,
					    #sip{type = response,
						 status = Status,
						 reason = Reason}),
	    State2 = collect_response(SIPResp, State1),
	    case State2#state.tr_ids of
		[] ->
		    choose_best_response(State2),
		    {stop, normal, State2};
		_ ->
		    {next_state, wait_for_response, State2}
	    end;
        #sip{status = 100} ->
            {next_state, wait_for_response, State};
        #sip{status = Status} ->
            {[_|Vias], NewHdrs} = esip:split_hdrs('via', Resp#sip.hdrs),
	    NewResp = case Vias of
			  [] ->
			      Resp#sip{hdrs = NewHdrs};
			  _ ->
			      Resp#sip{hdrs = [{'via', Vias}|NewHdrs]}
		      end,
	    if Status < 300 ->
		    esip:reply(State#state.orig_trid, NewResp);
	       true ->
		    ok
	    end,
	    State1 = if Status >= 200 ->
			     mark_transaction_as_complete(TrID, State);
			true ->
			     State
		     end,
	    State2 = if Status >= 300 ->
			     collect_response(NewResp, State1);
			true ->
			     State1
		     end,
	    if Status >= 600 ->
		    cancel_pending_transactions(State2);
	       true ->
		    ok
	    end,
	    case State2#state.tr_ids of
		[] ->
		    choose_best_response(State2),
		    {stop, normal, State2};
		_ ->
		    {next_state, wait_for_response, State2}
	    end
    end;
wait_for_response(_Event, State) ->
    {next_state, wait_for_response, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
connect(#sip{hdrs = Hdrs} = Req, Opts) ->
    {_, ToURI, _} = esip:get_hdr('to', Hdrs),
    case mod_sip:at_my_host(ToURI) of
	true ->
	    LUser = jid:nodeprep(ToURI#uri.user),
	    LServer = jid:nameprep(ToURI#uri.host),
	    case mod_sip_registrar:find_sockets(LUser, LServer) of
		[_|_] = SIPSocks ->
		    {ok, SIPSocks};
		[] ->
		    {error, notfound}
	    end;
	false ->
	    case esip:connect(Req, Opts) of
		{ok, SIPSock} ->
		    {ok, [{SIPSock, Req#sip.uri}]};
		{error, _} = Err ->
		    Err
	    end
    end.

cancel_pending_transactions(State) ->
    lists:foreach(fun esip:cancel/1, State#state.tr_ids).

add_certfile(LServer, Opts) ->
    case ejabberd_config:get_option({domain_certfile, LServer},
				    fun iolist_to_binary/1) of
	CertFile when is_binary(CertFile), CertFile /= <<"">> ->
	    [{certfile, CertFile}|Opts];
	_ ->
	    Opts
    end.

add_via(#sip_socket{type = Transport}, LServer, #sip{hdrs = Hdrs} = Req) ->
    ConfiguredVias = get_configured_vias(LServer),
    {ViaHost, ViaPort} = proplists:get_value(
			   Transport, ConfiguredVias, {LServer, undefined}),
    ViaTransport = case Transport of
		       tls -> <<"TLS">>;
		       tcp -> <<"TCP">>;
		       udp -> <<"UDP">>
		   end,
    Via = #via{transport = ViaTransport,
	       host = ViaHost,
	       port = ViaPort,
	       params = [{<<"branch">>, esip:make_branch()}]},
    Req#sip{hdrs = [{'via', [Via]}|Hdrs]}.

add_record_route_and_set_uri(URI, LServer, #sip{hdrs = Hdrs} = Req) ->
    case is_request_within_dialog(Req) of
	false ->
	    case need_record_route(LServer) of
		true ->
		    RR_URI = get_configured_record_route(LServer),
		    TS = (integer_to_binary(p1_time_compat:system_time(seconds))),
		    Sign = make_sign(TS, Hdrs),
		    User = <<TS/binary, $-, Sign/binary>>,
		    NewRR_URI = RR_URI#uri{user = User},
		    Hdrs1 = [{'record-route', [{<<>>, NewRR_URI, []}]}|Hdrs],
		    Req#sip{uri = URI, hdrs = Hdrs1};
		false ->
		    Req
	    end;
	true ->
	    Req
    end.

is_request_within_dialog(#sip{hdrs = Hdrs}) ->
    {_, _, Params} = esip:get_hdr('to', Hdrs),
    esip:has_param(<<"tag">>, Params).

need_record_route(LServer) ->
    gen_mod:get_module_opt(
      LServer, mod_sip, always_record_route,
      fun(true) -> true;
	 (false) -> false
      end, true).

make_sign(TS, Hdrs) ->
    {_, #uri{user = FUser, host = FServer}, FParams} = esip:get_hdr('from', Hdrs),
    {_, #uri{user = TUser, host = TServer}, _} = esip:get_hdr('to', Hdrs),
    LFUser = safe_nodeprep(FUser),
    LTUser = safe_nodeprep(TUser),
    LFServer = safe_nameprep(FServer),
    LTServer = safe_nameprep(TServer),
    FromTag = esip:get_param(<<"tag">>, FParams),
    CallID = esip:get_hdr('call-id', Hdrs),
    SharedKey = ejabberd_config:get_option(shared_key, fun(V) -> V end),
    str:sha([SharedKey, LFUser, LFServer, LTUser, LTServer,
		FromTag, CallID, TS]).

is_signed_by_me(TS_Sign, Hdrs) ->
    try
	[TSBin, Sign] = str:tokens(TS_Sign, <<"-">>),
	TS = (binary_to_integer(TSBin)),
	NowTS = p1_time_compat:system_time(seconds),
	true = (NowTS - TS) =< ?SIGN_LIFETIME,
	Sign == make_sign(TSBin, Hdrs)
    catch _:_ ->
	    false
    end.

get_configured_vias(LServer) ->
    gen_mod:get_module_opt(
      LServer, mod_sip, via,
      fun(L) ->
	      lists:map(
		fun(Opts) ->
			Type = proplists:get_value(type, Opts),
			Host = proplists:get_value(host, Opts),
			Port = proplists:get_value(port, Opts),
			true = (Type == tcp) or (Type == tls) or (Type == udp),
			true = is_binary(Host) and (Host /= <<"">>),
			true = (is_integer(Port)
				and (Port > 0) and (Port < 65536))
			    or (Port == undefined),
			{Type, {Host, Port}}
		end, L)
      end, []).

get_configured_record_route(LServer) ->
    gen_mod:get_module_opt(
      LServer, mod_sip, record_route,
      fun(IOList) ->
	      S = iolist_to_binary(IOList),
	      #uri{} = esip:decode_uri(S)
      end, #uri{host = LServer, params = [{<<"lr">>, <<"">>}]}).

get_configured_routes(LServer) ->
    gen_mod:get_module_opt(
      LServer, mod_sip, routes,
      fun(L) ->
	      lists:map(
		fun(IOList) ->
			S = iolist_to_binary(IOList),
			#uri{} = esip:decode_uri(S)
		end, L)
      end, [#uri{host = LServer, params = [{<<"lr">>, <<"">>}]}]).

mark_transaction_as_complete(TrID, State) ->
    NewTrIDs = lists:delete(TrID, State#state.tr_ids),
    State#state{tr_ids = NewTrIDs}.

collect_response(Resp, #state{responses = Resps} = State) ->
    State#state{responses = [Resp|Resps]}.

choose_best_response(#state{responses = Responses} = State) ->
    SortedResponses = lists:keysort(#sip.status, Responses),
    case lists:filter(
	   fun(#sip{status = Status}) ->
		   Status >= 600
	   end, SortedResponses) of
	[Resp|_] ->
	    esip:reply(State#state.orig_trid, Resp);
	[] ->
	    case SortedResponses of
		[Resp|_] ->
		    esip:reply(State#state.orig_trid, Resp);
		[] ->
		    ok
	    end
    end.

%% Just compare host part only.
cmp_uri(#uri{host = H1}, #uri{host = H2}) ->
    jid:nameprep(H1) == jid:nameprep(H2).

is_my_route(URI, URIs) ->
    lists:any(fun(U) -> cmp_uri(URI, U) end, URIs).

prepare_request(LServer, #sip{hdrs = Hdrs} = Req) ->
    ConfiguredRRoute = get_configured_record_route(LServer),
    ConfiguredRoutes = get_configured_routes(LServer),
    Hdrs1 = lists:flatmap(
	      fun({Hdr, HdrList}) when Hdr == 'route';
				       Hdr == 'record-route' ->
		      case lists:filter(
			     fun({_, URI, _}) ->
				     not cmp_uri(URI, ConfiguredRRoute)
					 and not is_my_route(URI, ConfiguredRoutes)
			     end, HdrList) of
			  [] ->
			      [];
			  HdrList1 ->
			      [{Hdr, HdrList1}]
		      end;
		 (Hdr) ->
		      [Hdr]
	      end, Hdrs),
    MF = esip:get_hdr('max-forwards', Hdrs1),
    Hdrs2 = esip:set_hdr('max-forwards', MF-1, Hdrs1),
    Hdrs3 = lists:filter(
              fun({'proxy-authorization', {_, Params}}) ->
                      Realm = esip:unquote(esip:get_param(<<"realm">>, Params)),
		      not mod_sip:is_my_host(jid:nameprep(Realm));
                 (_) ->
                      true
              end, Hdrs2),
    Req#sip{hdrs = Hdrs3}.

safe_nodeprep(S) ->
    case jid:nodeprep(S) of
	error -> S;
	S1 -> S1
    end.

safe_nameprep(S) ->
    case jid:nameprep(S) of
	error -> S;
	S1 -> S1
    end.

opt_type(domain_certfile) -> fun iolist_to_binary/1;
opt_type(shared_key) -> fun (V) -> V end;
opt_type(_) -> [domain_certfile, shared_key].
