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
-define(DEFAULT_EXPIRES, 3600).

-record(binding, {socket = #sip_socket{},
		  call_id = <<"">> :: binary(),
		  cseq = 0 :: non_neg_integer(),
		  timestamp = now() :: erlang:timestamp(),
		  contact :: {binary(), #uri{}, [{binary(), binary()}]},
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
    Expires = esip:get_hdr('expires', Hdrs, ?DEFAULT_EXPIRES),
    case esip:get_hdrs('contact', Hdrs) of
        [<<"*">>] when Expires == 0 ->
            case unregister_session(US, CallID, CSeq) of
		{ok, ContactsWithExpires} ->
		    ?INFO_MSG("unregister SIP session for user ~s@~s from ~s",
			      [LUser, LServer, inet_parse:ntoa(PeerIP)]),
		    Cs = prepare_contacts_to_send(ContactsWithExpires),
		    mod_sip:make_response(
		      Req,
		      #sip{type = response,
			   status = 200,
			   hdrs = [{'contact', Cs}]});
		{error, Why} ->
		    {Status, Reason} = make_status(Why),
		    mod_sip:make_response(
		      Req, #sip{type = response,
				status = Status,
				reason = Reason})
	    end;
        [{_, _URI, _Params}|_] = Contacts ->
	    ContactsWithExpires = make_contacts_with_expires(Contacts, Expires),
	    Expires1 = lists:max([E || {_, E} <- ContactsWithExpires]),
	    MinExpires = min_expires(),
	    if Expires1 > 0, Expires1 < MinExpires ->
		    mod_sip:make_response(
		      Req, #sip{type = response,
				status = 423,
				hdrs = [{'min-expires', MinExpires}]});
	       true ->
		    case register_session(US, SIPSock, CallID, CSeq,
					  ContactsWithExpires) of
			{ok, Res} ->
			    ?INFO_MSG("~s SIP session for user ~s@~s from ~s",
				      [Res, LUser, LServer,
				       inet_parse:ntoa(PeerIP)]),
			    Cs = prepare_contacts_to_send(ContactsWithExpires),
			    mod_sip:make_response(
			      Req,
			      #sip{type = response,
				   status = 200,
				   hdrs = [{'contact', Cs}]});
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
		    ContactsWithExpires =
			lists:map(
			  fun(#binding{contact = Contact, expires = Es}) ->
				  {Contact, Es}
			  end, Bindings),
		    Cs = prepare_contacts_to_send(ContactsWithExpires),
		    mod_sip:make_response(
		      Req, #sip{type = response, status = 200,
				hdrs = [{'contact', Cs}]});
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
	    lists:map(
	      fun(#binding{contact = {_, URI, _},
			   socket = Socket}) ->
		      {Socket, URI}
	      end, Bindings);
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
handle_call({delete, US, CallID, CSeq}, _From, State) ->
    Res = delete_session(US, CallID, CSeq),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({write, Session}, State) ->
    write_session(Session),
    {noreply, State};
handle_info({delete, US, CallID, CSeq}, State) ->
    delete_session(US, CallID, CSeq),
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
register_session(US, SIPSocket, CallID, CSeq, ContactsWithExpires) ->
    Bindings = lists:map(
		 fun({Contact, Expires}) ->
			 #binding{socket = SIPSocket,
				  call_id = CallID,
				  cseq = CSeq,
				  timestamp = now(),
				  contact = Contact,
				  expires = Expires}
		 end, ContactsWithExpires),
    Session = #sip_session{us = US, bindings = Bindings},
    call({write, Session}).

unregister_session(US, CallID, CSeq) ->
    Msg = {delete, US, CallID, CSeq},
    call(Msg).

write_session(#sip_session{us = {U, S} = US, bindings = NewBindings}) ->
    PrevBindings = case mnesia:dirty_read(sip_session, US) of
		       [#sip_session{bindings = PrevBindings1}] ->
			   PrevBindings1;
		       [] ->
			   []
		   end,
    Res = lists:foldl(
	    fun(_, {error, _} = Err) ->
		    Err;
	       (#binding{call_id = CallID,
			 expires = Expires,
			 cseq = CSeq} = Binding, {Add, Keep, Del}) ->
		    case find_binding(Binding, PrevBindings) of
			{ok, #binding{call_id = CallID, cseq = PrevCSeq}}
			  when PrevCSeq > CSeq ->
			    {error, cseq_out_of_order};
			{ok, PrevBinding} when Expires == 0 ->
			    {Add, Keep -- [PrevBinding], [PrevBinding|Del]};
			{ok, PrevBinding} ->
			    {[Binding|Add], Keep -- [PrevBinding], Del};
			{error, notfound} when Expires == 0 ->
			    {error, notfound};
			{error, notfound} ->
			    {[Binding|Add], Keep, Del}
		    end
	    end, {[], PrevBindings, []}, NewBindings),
    MaxSessions = ejabberd_sm:get_max_user_sessions(U, S),
    case Res of
	{error, Why} ->
	    {error, Why};
	{AddBindings, KeepBindings, DelBindings} ->
	    MaxSessions = ejabberd_sm:get_max_user_sessions(U, S),
	    AllBindings = AddBindings ++ KeepBindings,
	    if length(AllBindings) > MaxSessions ->
		    {error, too_many_sessions};
	       true ->
		    lists:foreach(
		      fun(#binding{tref = TRef}) ->
			      erlang:cancel_timer(TRef)
		      end, DelBindings),
		    AddBindings1 = lists:map(
				     fun(#binding{tref = TRef,
						  expires = Expires} = Binding) ->
					     erlang:cancel_timer(TRef),
					     NewTRef = erlang:start_timer(
							 Expires * 1000, self(), US),
					     Binding#binding{tref = NewTRef}
				     end, AddBindings),
		    AllBindings1 = AddBindings1 ++ KeepBindings,
		    case AllBindings1 of
			[] ->
			    mnesia:dirty_delete(sip_session, US),
			    {ok, unregister};
			_ ->
			    mnesia:dirty_write(
			      #sip_session{us = US, bindings = AllBindings1}),
			    if length(DelBindings) == length(NewBindings) ->
				    {ok, unregister};
			       true ->
				    {ok, register}
			    end
		    end
	    end
    end.

delete_session(US, CallID, CSeq) ->
    case mnesia:dirty_read(sip_session, US) of
	[#sip_session{bindings = Bindings}] ->
	    case lists:all(
		   fun(B) when B#binding.call_id == CallID,
			       B#binding.cseq > CSeq ->
			   false;
		      (_) ->
			   true
		   end, Bindings) of
		true ->
		    ContactsWithExpires =
			lists:map(
			  fun(#binding{contact = Contact,
				       tref = TRef}) ->
				  erlang:cancel_timer(TRef),
				  {Contact, 0}
			  end, Bindings),
		    mnesia:dirty_delete(sip_session, US),
		    {ok, ContactsWithExpires};
		false ->
		    {error, cseq_out_of_order}
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

call(Msg) ->
    case catch ?GEN_SERVER:call(?MODULE, Msg, ?CALL_TIMEOUT) of
	{'EXIT', {timeout, _}} ->
	    {error, timeout};
	{'EXIT', Why} ->
	    {error, Why};
	Reply ->
	    Reply
    end.

make_contacts_with_expires(Contacts, Expires) ->
    lists:map(
      fun({Name, URI, Params}) ->
	      E1 = case to_integer(esip:get_param(<<"expires">>, Params),
				   0, (1 bsl 32)-1) of
		       {ok, E} -> E;
		       _ -> Expires
		   end,
	      Params1 = lists:keydelete(<<"expires">>, 1, Params),
	      {{Name, URI, Params1}, E1}
      end, Contacts).

prepare_contacts_to_send(ContactsWithExpires) ->
    lists:map(
      fun({{Name, URI, Params}, Expires}) ->
	      Params1 = esip:set_param(<<"expires">>,
				       list_to_binary(
					 integer_to_list(Expires)),
				       Params),
	      {Name, URI, Params1}
      end, ContactsWithExpires).

find_binding(#binding{contact = {_, URI1, _}} = OrigBinding,
	     [#binding{contact = {_, URI2, _}} = Binding|Bindings]) ->
    case cmp_uri(URI1, URI2) of
	true ->
	    {ok, Binding};
	false ->
	    find_binding(OrigBinding, Bindings)
    end;
find_binding(_, []) ->
    {error, notfound}.

%% TODO: this is *totally* wrong.
%% Rewrite this using URI comparison rules
cmp_uri(#uri{user = U, host = H, port = P},
	#uri{user = U, host = H, port = P}) ->
    true;
cmp_uri(_, _) ->
    false.

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
