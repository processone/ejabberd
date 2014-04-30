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
-export([start_link/0, request/2, find_session/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("esip.hrl").

-record(sip_session, {us = {<<"">>, <<"">>} :: {binary(), binary()},
		      socket = #sip_socket{},
		      timestamp = now() :: erlang:timestamp(),
		      tref = make_ref() :: reference(),
		      expires = 0 :: non_neg_integer()}).

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
    Expires = esip:get_hdr('expires', Hdrs, 0),
    case esip:get_hdrs('contact', Hdrs) of
        [<<"*">>] when Expires == 0 ->
	    ?INFO_MSG("unregister SIP session for user ~s@~s from ~s",
		      [LUser, LServer, inet_parse:ntoa(PeerIP)]),
            unregister_session(US),
            mod_sip:make_response(Req, #sip{type = response, status = 200});
        [{_, _URI, _Params}|_] = Contacts ->
            ContactsWithExpires =
		lists:map(
		  fun({Name, URI, Params}) ->
			  Exp = case to_integer(
				       esip:get_param(
					 <<"expires">>, Params),
				       0, (1 bsl 32)-1) of
				    {ok, E} -> E;
				    _ -> Expires
				end,
			  NewParams = esip:set_param(
					<<"expires">>,
					erlang:integer_to_binary(Exp),
					Params),
			  {Exp, {Name, URI, NewParams}}
		  end, Contacts),
	    [{Expires1, _}|_] = lists:keysort(1, ContactsWithExpires),
	    MinExpires = min_expires(),
            if Expires1 >= MinExpires ->
		    ?INFO_MSG("register SIP session for user ~s@~s from ~s",
			      [LUser, LServer, inet_parse:ntoa(PeerIP)]),
		    register_session(US, SIPSock, Expires1),
                    mod_sip:make_response(
		      Req,
		      #sip{type = response,
			   status = 200,
			   hdrs = [{'contact',
				    [C || {_, C} <- ContactsWithExpires]}]});
               Expires1 > 0, Expires1 < MinExpires ->
                    mod_sip:make_response(
		      Req, #sip{type = response,
				status = 423,
				hdrs = [{'min-expires', MinExpires}]});
               true ->
		    ?INFO_MSG("unregister SIP session for user ~s@~s from ~s",
			      [LUser, LServer, inet_parse:ntoa(PeerIP)]),
                    unregister_session(US),
                    mod_sip:make_response(
		      Req,
		      #sip{type = response, status = 200,
			   hdrs = [{'contact',
				    [C || {_, C} <- ContactsWithExpires]}]})
            end;
        _ ->
            mod_sip:make_response(Req, #sip{type = response, status = 400})
    end.

find_session(U, S) ->
    case mnesia:dirty_read(sip_session, {U, S}) of
	[Session] ->
	    {ok, Session};
	[] ->
	    error
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
handle_call({delete, US}, _From, State) ->
    Res = delete_session(US),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({write, Session}, State) ->
    write_session(Session),
    {noreply, State};
handle_info({delete, US}, State) ->
    delete_session(US),
    {noreply, State};
handle_info({timeout, TRef, US}, State) ->
    case mnesia:dirty_read(sip_session, US) of
	[#sip_session{tref = TRef}] ->
	    mnesia:dirty_delete(sip_session, US);
	[] ->
	    ok
    end,
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
register_session(US, SIPSocket, Expires) ->
    Session = #sip_session{us = US,
			   socket = SIPSocket,
			   timestamp = now(),
			   expires = Expires},
    gen_server:call(?MODULE, {write, Session}).

unregister_session(US) ->
    gen_server:call(?MODULE, {delete, US}).

write_session(#sip_session{us = US, expires = Expires} = Session) ->
    case mnesia:dirty_read(sip_session, US) of
	[#sip_session{tref = TRef}] ->
	    erlang:cancel_timer(TRef);
	[] ->
	    ok
    end,
    NewTRef = erlang:start_timer(Expires * 1000, self(), US),
    mnesia:dirty_write(Session#sip_session{tref = NewTRef}).

delete_session(US) ->
    case mnesia:dirty_read(sip_session, US) of
	[#sip_session{tref = TRef}] ->
	    erlang:cancel_timer(TRef),
	    mnesia:dirty_delete(sip_session, US);
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
