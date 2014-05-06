%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_sip_registrar).

-define(GEN_SERVER, p1_server).
-behaviour(?GEN_SERVER).

%% API
-export([start_link/0, request/2, find_sockets/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("esip.hrl").

-define(CALL_TIMEOUT, timer:seconds(30)).

-record(binding, {socket = #sip_socket{},
		  call_id = <<"">> :: binary(),
		  cseq = 0 :: non_neg_integer(),
		  timestamp = now() :: erlang:timestamp(),
		  tref = make_ref() :: reference(),
		  expires = 0 :: non_neg_integer()}).

-record(sip_session, {us = {<<"">>, <<"">>} :: {binary(), binary()},
		      bindings = [] :: [#binding{}]}).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    ?GEN_SERVER:start_link({local, ?MODULE}, ?MODULE, [], []).

request(#sip{hdrs = Hdrs} = Req, SIPSock) ->
    {_, #uri{user = U, host = S}, _} = esip:get_hdr('to', Hdrs),
    LUser = jlib:nodeprep(U),
    LServer = jlib:nameprep(S),
    {PeerIP, _} = SIPSock#sip_socket.peer,
    US = {LUser, LServer},
    CallID = esip:get_hdr('call-id', Hdrs),
    CSeq = esip:get_hdr('cseq', Hdrs),
    Expires = esip:get_hdr('expires', Hdrs, 0),
    case esip:get_hdrs('contact', Hdrs) of
        [<<"*">>] when Expires == 0 ->
            case unregister_session(US, SIPSock, CallID, CSeq) of
		ok ->
		    ?INFO_MSG("unregister SIP session for user ~s@~s from ~s",
			      [LUser, LServer, inet_parse:ntoa(PeerIP)]),
		    mod_sip:make_response(
		      Req, #sip{type = response, status = 200});
		{error, Why} ->
		    {Status, Reason} = make_status(Why),
		    mod_sip:make_response(
		      Req, #sip{type = response,
				status = Status,
				reason = Reason})
	    end;
        [{_, _URI, _Params}|_] = Contacts ->
            ExpiresList = lists:map(
			    fun({_, _, Params}) ->
				    case to_integer(
					   esip:get_param(
					     <<"expires">>, Params),
					   0, (1 bsl 32)-1) of
					{ok, E} -> E;
					_ -> Expires
				    end
			    end, Contacts),
	    Expires1 = lists:max(ExpiresList),
	    Contact = {<<"">>, #uri{user = LUser, host = LServer},
		       [{<<"expires">>, jlib:integer_to_binary(Expires1)}]},
	    MinExpires = min_expires(),
            if Expires1 >= MinExpires ->
		    case register_session(US, SIPSock, CallID, CSeq, Expires1) of
			ok ->
			    ?INFO_MSG("register SIP session for user ~s@~s from ~s",
				      [LUser, LServer, inet_parse:ntoa(PeerIP)]),
			    mod_sip:make_response(
			      Req,
			      #sip{type = response,
				   status = 200,
				   hdrs = [{'contact', [Contact]}]});
			{error, Why} ->
			    {Status, Reason} = make_status(Why),
			    mod_sip:make_response(
			      Req, #sip{type = response,
					status = Status,
					reason = Reason})
		    end;
               Expires1 > 0, Expires1 < MinExpires ->
                    mod_sip:make_response(
		      Req, #sip{type = response,
				status = 423,
				hdrs = [{'min-expires', MinExpires}]});
               true ->
                    case unregister_session(US, SIPSock, CallID, CSeq) of
			ok ->
			    ?INFO_MSG("unregister SIP session for user ~s@~s from ~s",
				      [LUser, LServer, inet_parse:ntoa(PeerIP)]),
			    mod_sip:make_response(
			      Req,
			      #sip{type = response, status = 200,
				   hdrs = [{'contact', [Contact]}]});
			{error, Why} ->
			    {Status, Reason} = make_status(Why),
			    mod_sip:make_response(
			      Req, #sip{type = response,
					status = Status,
					reason = Reason})
		    end
            end;
	[] ->
	    case mnesia:dirty_read(sip_session, US) of
		[#sip_session{bindings = Bindings}] ->
		    case pop_previous_binding(SIPSock, Bindings) of
			{ok, #binding{expires = Expires1}, _} ->
			    Contact = {<<"">>,
				       #uri{user = LUser, host = LServer},
				       [{<<"expires">>,
					 jlib:integer_to_binary(Expires1)}]},
			    mod_sip:make_response(
			      Req, #sip{type = response, status = 200,
					hdrs = [{'contact', [Contact]}]});
			{error, notfound} ->
			    {Status, Reason} = make_status(notfound),
			    mod_sip:make_response(
			      Req, #sip{type = response,
					status = Status,
					reason = Reason})
		    end;
		[] ->
		    {Status, Reason} = make_status(notfound),
		    mod_sip:make_response(
		      Req, #sip{type = response,
				status = Status,
				reason = Reason})
	    end;
        _ ->
            mod_sip:make_response(Req, #sip{type = response, status = 400})
    end.

find_sockets(U, S) ->
    case mnesia:dirty_read(sip_session, {U, S}) of
	[#sip_session{bindings = Bindings}] ->
	    [Binding#binding.socket || Binding <- Bindings];
	[] ->
	    []
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    mnesia:create_table(sip_session,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, sip_session)}]),
    mnesia:add_table_copy(sip_session, node(), ram_copies),
    {ok, #state{}}.

handle_call({write, Session}, _From, State) ->
    Res = write_session(Session),
    {reply, Res, State};
handle_call({delete, US, SIPSocket, CallID, CSeq}, _From, State) ->
    Res = delete_session(US, SIPSocket, CallID, CSeq),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({write, Session}, State) ->
    write_session(Session),
    {noreply, State};
handle_info({delete, US, SIPSocket, CallID, CSeq}, State) ->
    delete_session(US, SIPSocket, CallID, CSeq),
    {noreply, State};
handle_info({timeout, TRef, US}, State) ->
    delete_expired_session(US, TRef),
    {noreply, State};
handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
register_session(US, SIPSocket, CallID, CSeq, Expires) ->
    Session = #sip_session{us = US,
			   bindings = [#binding{socket = SIPSocket,
						call_id = CallID,
						cseq = CSeq,
						timestamp = now(),
						expires = Expires}]},
    call({write, Session}).

unregister_session(US, SIPSocket, CallID, CSeq) ->
    Msg = {delete, US, SIPSocket, CallID, CSeq},
    call(Msg).

write_session(#sip_session{us = {U, S} = US,
			   bindings = [#binding{socket = SIPSocket,
						call_id = CallID,
						expires = Expires,
						cseq = CSeq} = Binding]}) ->
    case mnesia:dirty_read(sip_session, US) of
	[#sip_session{bindings = Bindings}] ->
	    case pop_previous_binding(SIPSocket, Bindings) of
		{ok, #binding{call_id = CallID, cseq = PrevCSeq}, _}
		  when PrevCSeq > CSeq ->
		    {error, cseq_out_of_order};
		{ok, #binding{tref = Tref}, Bindings1} ->
		    erlang:cancel_timer(Tref),
		    NewTRef = erlang:start_timer(Expires * 1000, self(), US),
		    NewBindings = [Binding#binding{tref = NewTRef}|Bindings1],
		    mnesia:dirty_write(
		      #sip_session{us = US, bindings = NewBindings});
		{error, notfound} ->
		    MaxSessions = ejabberd_sm:get_max_user_sessions(U, S),
		    if length(Bindings) < MaxSessions ->
			    NewTRef = erlang:start_timer(Expires * 1000, self(), US),
			    NewBindings = [Binding#binding{tref = NewTRef}|Bindings],
			    mnesia:dirty_write(
			      #sip_session{us = US, bindings = NewBindings});
		       true ->
			    {error, too_many_sessions}
		    end
	    end;
	[] ->
	    NewTRef = erlang:start_timer(Expires * 1000, self(), US),
	    NewBindings = [Binding#binding{tref = NewTRef}],
	    mnesia:dirty_write(#sip_session{us = US, bindings = NewBindings})
    end.

delete_session(US, SIPSocket, CallID, CSeq) ->
    case mnesia:dirty_read(sip_session, US) of
	[#sip_session{bindings = Bindings}] ->
	    case pop_previous_binding(SIPSocket, Bindings) of
		{ok, #binding{call_id = CallID, cseq = PrevCSeq}, _}
		  when PrevCSeq > CSeq ->
		    {error, cseq_out_of_order};
		{ok, #binding{tref = TRef}, []} ->
		    erlang:cancel_timer(TRef),
		    mnesia:dirty_delete(sip_session, US);
		{ok, #binding{tref = TRef}, NewBindings} ->
		    erlang:cancel_timer(TRef),
		    mnesia:dirty_write(sip_session,
				       #sip_session{us = US,
						    bindings = NewBindings});
		{error, notfound} ->
		    {error, notfound}
	    end;
	[] ->
	    {error, notfound}
    end.

delete_expired_session(US, TRef) ->
    case mnesia:dirty_read(sip_session, US) of
	[#sip_session{bindings = Bindings}] ->
	    case lists:filter(
		   fun(#binding{tref = TRef1}) when TRef1 == TRef ->
			   false;
		      (_) ->
			   true
		   end, Bindings) of
		[] ->
		    mnesia:dirty_delete(sip_session, US);
		NewBindings ->
		    mnesia:dirty_write(sip_session,
				       #sip_session{us = US,
						    bindings = NewBindings})
	    end;
	[] ->
	    ok
    end.

min_expires() ->
    60.

to_integer(Bin, Min, Max) ->
    case catch list_to_integer(binary_to_list(Bin)) of
        N when N >= Min, N =< Max ->
            {ok, N};
        _ ->
            error
    end.

pop_previous_binding(#sip_socket{peer = Peer}, Bindings) ->
    case lists:partition(
	   fun(#binding{socket = #sip_socket{peer = Peer1}}) ->
		   Peer1 == Peer
	   end, Bindings) of
	{[Binding], RestBindings} ->
	    {ok, Binding, RestBindings};
	_ ->
	    {error, notfound}
    end.

call(Msg) ->
    case catch ?GEN_SERVER:call(?MODULE, Msg, ?CALL_TIMEOUT) of
	{'EXIT', {timeout, _}} ->
	    {error, timeout};
	{'EXIT', Why} ->
	    {error, Why};
	Reply ->
	    Reply
    end.

make_status(notfound) ->
    {404, esip:reason(404)};
make_status(cseq_out_of_order) ->
    {500, <<"CSeq is Out of Order">>};
make_status(timeout) ->
    {408, esip:reason(408)};
make_status(too_many_sessions) ->
    {503, <<"Too Many Registered Sessions">>};
make_status(_) ->
    {500, esip:reason(500)}.
