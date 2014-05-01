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
-export([start/2, start_link/2, route/4, route/5]).

%% gen_fsm callbacks
-export([init/1, wait_for_request/2, wait_for_response/2,
	 handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("esip.hrl").

-record(state, {host = <<"">> :: binary(),
		opts = []     :: [{certfile, binary()}],
		orig_trid,
		orig_req      :: #sip{},
		client_trid}).

%%%===================================================================
%%% API
%%%===================================================================
start(LServer, Opts) ->
    supervisor:start_child(mod_sip_proxy_sup, [LServer, Opts]).

start_link(LServer, Opts) ->
    ?GEN_FSM:start_link(?MODULE, [LServer, Opts], []).

route(Resp, Req, _SIPSock, TrID, Pid) ->
    ?GEN_FSM:send_event(Pid, {Resp, Req, TrID}).

route(SIPMsg, _SIPSock, TrID, Pid) ->
    ?GEN_FSM:send_event(Pid, {SIPMsg, TrID}),
    wait.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([Host, Opts]) ->
    {ok, wait_for_request, #state{opts = Opts, host = Host}}.

wait_for_request({#sip{type = request} = Req, TrID}, State) ->
    Opts = State#state.opts,
    Req1 = mod_sip:prepare_request(Req),
    case connect(Req1, Opts) of
	{ok, SIPSocket} ->
	    Req2 = mod_sip:add_via(SIPSocket, State#state.host, Req1),
	    case esip:request(SIPSocket, Req2, {?MODULE, route, [self()]}) of
		{ok, ClientTrID} ->
		    {next_state, wait_for_response,
		     State#state{orig_trid = TrID,
				 orig_req = Req,
				 client_trid = ClientTrID}};
		Err ->
		    {Status, Reason} = esip:error_status(Err),
		    esip:reply(TrID, mod_sip:make_response(
				       Req, #sip{type = response,
						 status = Status,
						 reason = Reason})),
		    {stop, normal, State}
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
    esip:cancel(State#state.client_trid),
    {next_state, wait_for_response, State};
wait_for_response({Resp, _TrID}, State) ->
    case Resp of
        {error, _} ->
	    Req = State#state.orig_req,
            {Status, Reason} = esip:error_status(Resp),
            case Status of
                408 when Req#sip.method /= <<"INVITE">> ->
                    %% Absorb useless 408. See RFC4320
                    esip:stop_transaction(State#state.orig_trid);
                _ ->
                    ErrResp = mod_sip:make_response(
				Req,
				#sip{type = response,
				     status = Status,
				     reason = Reason}),
		    esip:reply(State#state.orig_trid, ErrResp)
            end,
            {stop, normal, State};
        #sip{status = 100} ->
            {next_state, wait_for_response, State};
        #sip{status = Status} ->
            case esip:split_hdrs('via', Resp#sip.hdrs) of
                {[_], _} ->
                    {stop, normal, State};
                {[_|Vias], NewHdrs} ->
                    esip:reply(State#state.orig_trid,
                               Resp#sip{hdrs = [{'via', Vias}|NewHdrs]}),
                    if Status < 200 ->
                            {next_state, wait_for_response, State};
                       true ->
                            {stop, normal, State}
                    end
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
		[SIPSock|_] ->
		    {ok, SIPSock};
		[] ->
		    {error, notfound}
	    end;
	false ->
	    esip:connect(Req, Opts)
    end.
