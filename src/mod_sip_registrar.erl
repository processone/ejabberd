%%%-------------------------------------------------------------------
%%% File    : mod_sip_registrar.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : 
%%% Created : 23 Apr 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(mod_sip_registrar).

-ifndef(GEN_SERVER).
-define(GEN_SERVER, gen_server).
-endif.
-behaviour(?GEN_SERVER).

%% API
-export([start_link/0, request/2, find_sockets/2, ping/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("esip/include/esip.hrl").

-define(CALL_TIMEOUT, timer:seconds(30)).
-define(DEFAULT_EXPIRES, 3600).
-define(FLOW_TIMEOUT_UDP, 29).
-define(FLOW_TIMEOUT_TCP, 120).

-record(sip_session, {us = {<<"">>, <<"">>} :: {binary(), binary()},
		      socket = #sip_socket{} :: #sip_socket{},
		      call_id = <<"">> :: binary(),
		      cseq = 0 :: non_neg_integer(),
		      timestamp = p1_time_compat:timestamp() :: erlang:timestamp(),
		      contact :: {binary(), #uri{}, [{binary(), binary()}]},
		      flow_tref :: reference() | undefined,
		      reg_tref = make_ref() :: reference(),
		      conn_mref = make_ref() :: reference(),
		      expires = 0 :: non_neg_integer()}).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    ?GEN_SERVER:start_link({local, ?MODULE}, ?MODULE, [], []).

request(#sip{hdrs = Hdrs} = Req, SIPSock) ->
    {_, #uri{user = U, host = S}, _} = esip:get_hdr('to', Hdrs),
    LUser = jid:nodeprep(U),
    LServer = jid:nameprep(S),
    {PeerIP, _} = SIPSock#sip_socket.peer,
    US = {LUser, LServer},
    CallID = esip:get_hdr('call-id', Hdrs),
    CSeq = esip:get_hdr('cseq', Hdrs),
    Expires = esip:get_hdr('expires', Hdrs, ?DEFAULT_EXPIRES),
    Supported = esip:get_hdrs('supported', Hdrs),
    IsOutboundSupported = lists:member(<<"outbound">>, Supported),
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
	    ContactsHaveManyRegID = contacts_have_many_reg_id(Contacts),
	    Expires1 = lists:max([E || {_, E} <- ContactsWithExpires]),
	    MinExpires = min_expires(),
	    if Expires1 > 0, Expires1 < MinExpires ->
		    mod_sip:make_response(
		      Req, #sip{type = response,
				status = 423,
				hdrs = [{'min-expires', MinExpires}]});
	       ContactsHaveManyRegID ->
		    mod_sip:make_response(
		      Req, #sip{type = response, status = 400,
				reason = <<"Multiple 'reg-id' parameter">>});
	       true ->
		    case register_session(US, SIPSock, CallID, CSeq,
					  IsOutboundSupported,
					  ContactsWithExpires) of
			{ok, Res} ->
			    ?INFO_MSG("~s SIP session for user ~s@~s from ~s",
				      [Res, LUser, LServer,
				       inet_parse:ntoa(PeerIP)]),
			    Cs = prepare_contacts_to_send(ContactsWithExpires),
			    Require = case need_ob_hdrs(
					     Contacts, IsOutboundSupported) of
					  true -> [{'require', [<<"outbound">>]},
						   {'flow-timer',
						    get_flow_timeout(LServer, SIPSock)}];
					  false -> []
				      end,
			    mod_sip:make_response(
			      Req,
			      #sip{type = response,
				   status = 200,
				   hdrs = [{'contact', Cs}|Require]});
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
		[_|_] = Sessions ->
		    ContactsWithExpires =
			lists:map(
			  fun(#sip_session{contact = Contact, expires = Es}) ->
				  {Contact, Es}
			  end, Sessions),
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
	[_|_] = Sessions ->
	    lists:map(
	      fun(#sip_session{contact = {_, URI, _},
			   socket = Socket}) ->
		      {Socket, URI}
	      end, Sessions);
	[] ->
	    []
    end.

ping(SIPSocket) ->
    call({ping, SIPSocket}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    update_table(),
    ejabberd_mnesia:create(?MODULE, sip_session,
			[{ram_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, sip_session)},
			 {index, [conn_mref,socket]}]),
    {ok, #state{}}.

handle_call({write, Sessions, Supported}, _From, State) ->
    Res = write_session(Sessions, Supported),
    {reply, Res, State};
handle_call({delete, US, CallID, CSeq}, _From, State) ->
    Res = delete_session(US, CallID, CSeq),
    {reply, Res, State};
handle_call({ping, SIPSocket}, _From, State) ->
    Res = process_ping(SIPSocket),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({write, Sessions, Supported}, State) ->
    write_session(Sessions, Supported),
    {noreply, State};
handle_info({delete, US, CallID, CSeq}, State) ->
    delete_session(US, CallID, CSeq),
    {noreply, State};
handle_info({timeout, TRef, US}, State) ->
    delete_expired_session(US, TRef),
    {noreply, State};
handle_info({'DOWN', MRef, process, _Pid, _Reason}, State) ->
    case mnesia:dirty_index_read(sip_session, MRef, #sip_session.conn_mref) of
	[Session] ->
	    mnesia:dirty_delete_object(Session);
	_ ->
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
register_session(US, SIPSocket, CallID, CSeq, IsOutboundSupported,
		 ContactsWithExpires) ->
    Sessions = lists:map(
		 fun({Contact, Expires}) ->
			 #sip_session{us = US,
				      socket = SIPSocket,
				      call_id = CallID,
				      cseq = CSeq,
				      timestamp = p1_time_compat:timestamp(),
				      contact = Contact,
				      expires = Expires}
		 end, ContactsWithExpires),
    Msg = {write, Sessions, IsOutboundSupported},
    call(Msg).

unregister_session(US, CallID, CSeq) ->
    Msg = {delete, US, CallID, CSeq},
    call(Msg).

write_session([#sip_session{us = {U, S} = US}|_] = NewSessions,
	      IsOutboundSupported) ->
    PrevSessions = mnesia:dirty_read(sip_session, US),
    Res = lists:foldl(
	    fun(_, {error, _} = Err) ->
		    Err;
	       (#sip_session{call_id = CallID,
			     expires = Expires,
			     cseq = CSeq} = Session, {Add, Del}) ->
		    case find_session(Session, PrevSessions,
				      IsOutboundSupported) of
			{ok, normal, #sip_session{call_id = CallID,
						  cseq = PrevCSeq}}
			  when PrevCSeq > CSeq ->
			    {error, cseq_out_of_order};
			{ok, _Type, PrevSession} when Expires == 0 ->
			    {Add, [PrevSession|Del]};
			{ok, _Type, PrevSession} ->
			    {[Session|Add], [PrevSession|Del]};
			{error, notfound} when Expires == 0 ->
			    {error, notfound};
			{error, notfound} ->
			    {[Session|Add], Del}
		    end
	    end, {[], []}, NewSessions),
    MaxSessions = ejabberd_sm:get_max_user_sessions(U, S),
    case Res of
	{error, Why} ->
	    {error, Why};
	{AddSessions, DelSessions} ->
	    MaxSessions = ejabberd_sm:get_max_user_sessions(U, S),
	    AllSessions = AddSessions ++ PrevSessions -- DelSessions,
	    if length(AllSessions) > MaxSessions ->
		    {error, too_many_sessions};
	       true ->
		    lists:foreach(fun delete_session/1, DelSessions),
		    lists:foreach(
		      fun(Session) ->
			      NewSession = set_monitor_and_timer(
					     Session, IsOutboundSupported),
			      mnesia:dirty_write(NewSession)
		      end, AddSessions),
		    case {AllSessions, AddSessions} of
			{[], _} ->
			    {ok, unregister};
			{_, []} ->
			    {ok, unregister};
			_ ->
			    {ok, register}
		    end
	    end
    end.

delete_session(US, CallID, CSeq) ->
    case mnesia:dirty_read(sip_session, US) of
	[_|_] = Sessions ->
	    case lists:all(
		   fun(S) when S#sip_session.call_id == CallID,
			       S#sip_session.cseq > CSeq ->
			   false;
		      (_) ->
			   true
		   end, Sessions) of
		true ->
		    ContactsWithExpires =
			lists:map(
			  fun(#sip_session{contact = Contact} = Session) ->
				  delete_session(Session),
				  {Contact, 0}
			  end, Sessions),
		    {ok, ContactsWithExpires};
		false ->
		    {error, cseq_out_of_order}
	    end;
	[] ->
	    {error, notfound}
    end.

delete_expired_session(US, TRef) ->
    case mnesia:dirty_read(sip_session, US) of
	[_|_] = Sessions ->
	    lists:foreach(
	      fun(#sip_session{reg_tref = T1,
			       flow_tref = T2} = Session)
		    when T1 == TRef; T2 == TRef ->
		      if T2 /= undefined ->
			      close_socket(Session);
			 true ->
			      ok
		      end,
		      delete_session(Session);
		 (_) ->
		      ok
	      end, Sessions);
	[] ->
	    ok
    end.

min_expires() ->
    60.

to_integer(Bin, Min, Max) ->
    case catch (binary_to_integer(Bin)) of
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

contacts_have_many_reg_id(Contacts) ->
    Sum = lists:foldl(
	    fun({_Name, _URI, Params}, Acc) ->
		    case get_ob_params(Params) of
			error ->
			    Acc;
			{_, _} ->
			    Acc + 1
		    end
	    end, 0, Contacts),
    if Sum > 1 ->
	    true;
       true ->
	    false
    end.

find_session(#sip_session{contact = {_, URI, Params}}, Sessions,
	     IsOutboundSupported) ->
    if IsOutboundSupported ->
	    case get_ob_params(Params) of
		{InstanceID, RegID} ->
		    find_session_by_ob({InstanceID, RegID}, Sessions);
		error ->
		    find_session_by_uri(URI, Sessions)
	    end;
       true ->
	    find_session_by_uri(URI, Sessions)
    end.

find_session_by_ob({InstanceID, RegID},
		   [#sip_session{contact = {_, _, Params}} = Session|Sessions]) ->
    case get_ob_params(Params) of
	{InstanceID, RegID} ->
	    {ok, flow, Session};
	_ ->
	    find_session_by_ob({InstanceID, RegID}, Sessions)
    end;
find_session_by_ob(_, []) ->
    {error, notfound}.

find_session_by_uri(URI1,
		    [#sip_session{contact = {_, URI2, _}} = Session|Sessions]) ->
    case cmp_uri(URI1, URI2) of
	true ->
	    {ok, normal, Session};
	false ->
	    find_session_by_uri(URI1, Sessions)
    end;
find_session_by_uri(_, []) ->
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

get_ob_params(Params) ->
    case esip:get_param(<<"+sip.instance">>, Params) of
	<<>> ->
	    error;
	InstanceID ->
	    case to_integer(esip:get_param(<<"reg-id">>, Params),
			    0, (1 bsl 32)-1) of
		{ok, RegID} ->
		    {InstanceID, RegID};
		error ->
		    error
	    end
    end.

need_ob_hdrs(_Contacts, _IsOutboundSupported = false) ->
    false;
need_ob_hdrs(Contacts, _IsOutboundSupported = true) ->
    lists:any(
      fun({_Name, _URI, Params}) ->
	      case get_ob_params(Params) of
		  error -> false;
		  {_, _} -> true
	      end
      end, Contacts).

get_flow_timeout(LServer, #sip_socket{type = Type}) ->
    case Type of
	udp ->
	    gen_mod:get_module_opt(
	      LServer, mod_sip, flow_timeout_udp,
	      fun(I) when is_integer(I), I>0 -> I end,
	      ?FLOW_TIMEOUT_UDP);
	_ ->
	    gen_mod:get_module_opt(
	      LServer, mod_sip, flow_timeout_tcp,
	      fun(I) when is_integer(I), I>0 -> I end,
	      ?FLOW_TIMEOUT_TCP)
    end.

update_table() ->
    Fields = record_info(fields, sip_session),
    case catch mnesia:table_info(sip_session, attributes) of
	Fields ->
	    ok;
	[_|_] ->
	    mnesia:delete_table(sip_session);
	{'EXIT', _} ->
	    ok
    end.

set_monitor_and_timer(#sip_session{socket = #sip_socket{type = Type,
							pid = Pid} = SIPSock,
				   conn_mref = MRef,
				   expires = Expires,
				   us = {_, LServer},
				   contact = {_, _, Params}} = Session,
		      IsOutboundSupported) ->
    RegTRef = set_timer(Session, Expires),
    Session1 = Session#sip_session{reg_tref = RegTRef},
    if IsOutboundSupported ->
	    case get_ob_params(Params) of
		error ->
		    Session1;
		{_, _} ->
		    FlowTimeout = get_flow_timeout(LServer, SIPSock),
		    FlowTRef = set_timer(Session1, FlowTimeout),
		    NewMRef = if Type == udp -> MRef;
				 true -> erlang:monitor(process, Pid)
			      end,
		    Session1#sip_session{conn_mref = NewMRef,
					 flow_tref = FlowTRef}
	    end;
       true ->
	    Session1
    end.

set_timer(#sip_session{us = US}, Timeout) ->
    erlang:start_timer(Timeout * 1000, self(), US).

close_socket(#sip_session{socket = SIPSocket}) ->
    if SIPSocket#sip_socket.type /= udp ->
	    esip_socket:close(SIPSocket);
       true ->
	    ok
    end.

delete_session(#sip_session{reg_tref = RegTRef,
			    flow_tref = FlowTRef,
			    conn_mref = MRef} = Session) ->
    erlang:cancel_timer(RegTRef),
    catch erlang:cancel_timer(FlowTRef),
    catch erlang:demonitor(MRef, [flush]),
    mnesia:dirty_delete_object(Session).

process_ping(SIPSocket) ->
    ErrResponse = if SIPSocket#sip_socket.type == udp -> pang;
		     true -> drop
		  end,
    Sessions = mnesia:dirty_index_read(
		 sip_session, SIPSocket, #sip_session.socket),
    lists:foldl(
      fun(#sip_session{flow_tref = TRef,
		       us = {_, LServer}} = Session, _)
	    when TRef /= undefined ->
	      erlang:cancel_timer(TRef),
	      mnesia:dirty_delete_object(Session),
	      Timeout = get_flow_timeout(LServer, SIPSocket),
	      NewTRef = set_timer(Session, Timeout),
	      case mnesia:dirty_write(
		     Session#sip_session{flow_tref = NewTRef}) of
		  ok ->
		      pong;
		  _Err ->
		      pang
	      end;
	 (_, Acc) ->
	      Acc
      end, ErrResponse, Sessions).
