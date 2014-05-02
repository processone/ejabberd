%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_sip_proxy).

-define(GEN_FSM, p1_fsm).
-behaviour(?GEN_FSM).

%% API
-export([start/2, start_link/2, route/4]).

%% gen_fsm callbacks
-export([init/1, wait_for_request/2, wait_for_response/2,
	 handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("esip.hrl").

-record(state, {host = <<"">>  :: binary(),
		opts = []      :: [{certfile, binary()}],
		orig_trid,
		responses = [] :: [#sip{}],
		tr_ids = []    :: list(),
		orig_req       :: #sip{}}).

%%%===================================================================
%%% API
%%%===================================================================
start(LServer, Opts) ->
    supervisor:start_child(mod_sip_proxy_sup, [LServer, Opts]).

start_link(LServer, Opts) ->
    ?GEN_FSM:start_link(?MODULE, [LServer, Opts], []).

route(SIPMsg, _SIPSock, TrID, Pid) ->
    ?GEN_FSM:send_event(Pid, {SIPMsg, TrID}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([Host, Opts]) ->
    Opts1 = add_certfile(Host, Opts),
    {ok, wait_for_request, #state{opts = Opts1, host = Host}}.

wait_for_request({#sip{type = request} = Req, TrID}, State) ->
    Opts = State#state.opts,
    Req1 = mod_sip:prepare_request(Req),
    case connect(Req1, Opts) of
	{ok, SIPSockets} ->
	    NewState =
		lists:foldl(
		  fun(_SIPSocket, {error, _} = Err) ->
			  Err;
		     (SIPSocket, #state{tr_ids = TrIDs} = AccState) ->
			  Req2 = add_via(SIPSocket, State#state.host, Req1),
			  case esip:request(SIPSocket, Req2,
					    {?MODULE, route, [self()]}) of
			      {ok, ClientTrID} ->
				  NewTrIDs = [ClientTrID|TrIDs],
				  AccState#state{tr_ids = NewTrIDs};
			      Err ->
				  cancel_pending_transactions(AccState),
				  Err
			  end
		  end, State, SIPSockets),
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
	    LUser = jlib:nodeprep(ToURI#uri.user),
	    LServer = jlib:nameprep(ToURI#uri.host),
	    case mod_sip_registrar:find_sockets(LUser, LServer) of
		[_|_] = SIPSocks ->
		    {ok, SIPSocks};
		[] ->
		    {error, notfound}
	    end;
	false ->
	    case esip:connect(Req, Opts) of
		{ok, SIPSock} ->
		    {ok, [SIPSock]};
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
	       params = [{<<"branch">>, esip:make_branch()},
			 {<<"rport">>, <<"">>}]},
    Req#sip{hdrs = [{'via', [Via]}|Hdrs]}.

get_configured_vias(LServer) ->
    gen_mod:get_module_opt(
      LServer, ?MODULE, via,
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
