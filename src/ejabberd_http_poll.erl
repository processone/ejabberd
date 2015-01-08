%%%----------------------------------------------------------------------
%%% File    : ejabberd_http_poll.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : HTTP Polling support (XEP-0025)
%%% Created :  4 Mar 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_http_poll).

-author('alexey@process-one.net').

-behaviour(gen_fsm).

%% External exports
-export([start_link/3, init/1, handle_event/3,
	 handle_sync_event/4, code_change/4, handle_info/3,
	 terminate/3, send/2, setopts/2, sockname/1, peername/1,
	 controlling_process/2, close/1, process/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

-record(http_poll, {id :: pid() | binary(), pid :: pid()}).

-type poll_socket() :: #http_poll{}.
-export_type([poll_socket/0]).

-record(state,
	{id, key, socket, output = [], input = <<"">>,
	 waiting_input = false, last_receiver, http_poll_timeout,
	 timer}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

-define(HTTP_POLL_TIMEOUT, 300).

-define(CT,
	{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}).

-define(BAD_REQUEST,
	[?CT, {<<"Set-Cookie">>, <<"ID=-3:0; expires=-1">>}]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(ID, Key, IP) ->
    mnesia:create_table(http_poll,
        		[{ram_copies, [node()]},
        		 {attributes, record_info(fields, http_poll)}]),
    supervisor:start_child(ejabberd_http_poll_sup, [ID, Key, IP]).

start_link(ID, Key, IP) ->
    gen_fsm:start_link(?MODULE, [ID, Key, IP], ?FSMOPTS).

send({http_poll, FsmRef, _IP}, Packet) ->
    gen_fsm:sync_send_all_state_event(FsmRef,
				      {send, Packet}).

setopts({http_poll, FsmRef, _IP}, Opts) ->
    case lists:member({active, once}, Opts) of
      true ->
	  gen_fsm:send_all_state_event(FsmRef,
				       {activate, self()});
      _ -> ok
    end.

sockname(_Socket) -> {ok, {{0, 0, 0, 0}, 0}}.

peername({http_poll, _FsmRef, IP}) -> {ok, IP}.

controlling_process(_Socket, _Pid) -> ok.

close({http_poll, FsmRef, _IP}) ->
    catch gen_fsm:sync_send_all_state_event(FsmRef, close).

process([],
	#request{data = Data, ip = IP} = _Request) ->
    case catch parse_request(Data) of
	{ok, ID1, Key, NewKey, Packet} ->
	    ID = if
		     (ID1 == <<"0">>) or (ID1 == <<"mobile">>) ->
			 NewID = p1_sha:sha(term_to_binary({now(), make_ref()})),
			 {ok, Pid} = start(NewID, <<"">>, IP),
			 mnesia:transaction(
			   fun() ->
				   mnesia:write(#http_poll{id = NewID, pid = Pid})
			   end),
			 NewID;
		     true ->
			 ID1
		 end,
	    case http_put(ID, Key, NewKey, Packet) of
		{error, not_exists} ->
		    {200, ?BAD_REQUEST, <<"">>};
		{error, bad_key} ->
		    {200, ?BAD_REQUEST, <<"">>};
		ok ->
		    receive
		    after 100 -> ok
		    end,
		    case http_get(ID) of
			{error, not_exists} ->
			    {200, ?BAD_REQUEST, <<"">>};
			{ok, OutPacket} ->
			    if
				ID == ID1 ->
				    Cookie = <<"ID=", ID/binary, "; expires=-1">>,
				    {200, [?CT, {<<"Set-Cookie">>, Cookie}],
				     OutPacket};
				ID1 == <<"mobile">> ->
				    {200, [?CT], [ID, $\n, OutPacket]};
				true ->
				    Cookie = <<"ID=", ID/binary, "; expires=-1">>,
				    {200, [?CT, {<<"Set-Cookie">>, Cookie}],
				     OutPacket}
			    end
		    end
	    end;
	_ ->
	    HumanHTMLxmlel = get_human_html_xmlel(),
	    {200, [?CT, {<<"Set-Cookie">>, <<"ID=-2:0; expires=-1">>}], HumanHTMLxmlel}
    end;
process(_, _Request) ->
    {400, [],
     #xmlel{name = <<"h1">>, attrs = [],
	    children = [{xmlcdata, <<"400 Bad Request">>}]}}.

%% Code copied from mod_http_bind.erl and customized
get_human_html_xmlel() ->
    Heading = <<"ejabberd ",
		(iolist_to_binary(atom_to_list(?MODULE)))/binary>>,
    #xmlel{name = <<"html">>,
	   attrs =
	       [{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>}],
	   children =
	       [#xmlel{name = <<"head">>, attrs = [],
		       children =
			   [#xmlel{name = <<"title">>, attrs = [],
				   children = [{xmlcdata, Heading}]}]},
		#xmlel{name = <<"body">>, attrs = [],
		       children =
			   [#xmlel{name = <<"h1">>, attrs = [],
				   children = [{xmlcdata, Heading}]},
			    #xmlel{name = <<"p">>, attrs = [],
				   children =
				       [{xmlcdata, <<"An implementation of ">>},
					#xmlel{name = <<"a">>,
					       attrs =
						   [{<<"href">>,
						     <<"http://xmpp.org/extensions/xep-0025.html">>}],
					       children =
						   [{xmlcdata,
						     <<"Jabber HTTP Polling (XEP-0025)">>}]}]},
			    #xmlel{name = <<"p">>, attrs = [],
				   children =
				       [{xmlcdata,
					 <<"This web page is only informative. To "
					   "use HTTP-Poll you need a Jabber/XMPP "
					   "client that supports it.">>}]}]}]}.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}                   
%%----------------------------------------------------------------------
init([ID, Key, IP]) ->
    ?INFO_MSG("started: ~p", [{ID, Key, IP}]),
    Opts = ejabberd_c2s_config:get_c2s_limits(),
    HTTPPollTimeout = ejabberd_config:get_option(
                        {http_poll_timeout, ?MYNAME},
                        fun(I) when is_integer(I), I>0 -> I end,
                        ?HTTP_POLL_TIMEOUT) * 1000,
    Socket = {http_poll, self(), IP},
    ejabberd_socket:start(ejabberd_c2s, ?MODULE, Socket,
			  Opts),
    Timer = erlang:start_timer(HTTPPollTimeout, self(), []),
    {ok, loop,
     #state{id = ID, key = Key, socket = Socket,
	    http_poll_timeout = HTTPPollTimeout, timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event({activate, From}, StateName, StateData) ->
    case StateData#state.input of
      <<"">> ->
	  {next_state, StateName,
	   StateData#state{waiting_input = {From, ok}}};
      Input ->
	  Receiver = From,
	  Receiver !
	    {tcp, StateData#state.socket, Input},
	  {next_state, StateName,
	   StateData#state{input = <<"">>, waiting_input = false,
			   last_receiver = Receiver}}
    end;
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
handle_sync_event({send, Packet}, _From, StateName,
		  StateData) ->
    Packet2 = iolist_to_binary(Packet),
    Output = StateData#state.output ++ [Packet2],
    Reply = ok,
    {reply, Reply, StateName,
     StateData#state{output = Output}};
handle_sync_event(stop, _From, _StateName, StateData) ->
    Reply = ok, {stop, normal, Reply, StateData};
handle_sync_event({http_put, Key, NewKey, Packet},
		  _From, StateName, StateData) ->
    Allow = case StateData#state.key of
	      <<"">> -> true;
	      OldKey ->
		  NextKey = jlib:encode_base64((p1_sha:sha1(Key))),
		  if OldKey == NextKey -> true;
		     true -> false
		  end
	    end,
    if Allow ->
	   case StateData#state.waiting_input of
	     false ->
		 Input = <<(StateData#state.input)/binary, Packet/binary>>,
		 Reply = ok,
		 {reply, Reply, StateName,
		  StateData#state{input = Input, key = NewKey}};
	     {Receiver, _Tag} ->
		 Receiver !
		   {tcp, StateData#state.socket, iolist_to_binary(Packet)},
		 cancel_timer(StateData#state.timer),
		 Timer =
		     erlang:start_timer(StateData#state.http_poll_timeout,
					self(), []),
		 Reply = ok,
		 {reply, Reply, StateName,
		  StateData#state{waiting_input = false,
				  last_receiver = Receiver, key = NewKey,
				  timer = Timer}}
	   end;
       true ->
	   Reply = {error, bad_key},
	   {reply, Reply, StateName, StateData}
    end;
handle_sync_event(http_get, _From, StateName,
		  StateData) ->
    Reply = {ok, StateData#state.output},
    {reply, Reply, StateName,
     StateData#state{output = []}};
handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info({timeout, Timer, _}, _StateName,
	    #state{timer = Timer} = StateData) ->
    {stop, normal, StateData};
handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, StateData) ->
    mnesia:transaction(
      fun() ->
	      mnesia:delete({http_poll, StateData#state.id})
      end),
    case StateData#state.waiting_input of
      false ->
	  case StateData#state.last_receiver of
	    undefined -> ok;
	    Receiver ->
		Receiver ! {tcp_closed, StateData#state.socket}
	  end;
      {Receiver, _Tag} ->
	  Receiver ! {tcp_closed, StateData#state.socket}
    end,
    catch resend_messages(StateData#state.output),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

http_put(ID, Key, NewKey, Packet) ->
    case mnesia:dirty_read({http_poll, ID}) of
	[] ->
	    {error, not_exists};
	[#http_poll{pid = FsmRef}] ->
	    gen_fsm:sync_send_all_state_event(
	      FsmRef, {http_put, Key, NewKey, Packet})
    end.

http_get(ID) ->
    case mnesia:dirty_read({http_poll, ID}) of
	[] ->
	    {error, not_exists};
	[#http_poll{pid = FsmRef}] ->
	    gen_fsm:sync_send_all_state_event(FsmRef, http_get)
    end.

parse_request(Data) ->
    Comma = str:chr(Data, $,),
    Header = str:substr(Data, 1, Comma - 1),
    Packet = str:substr(Data, Comma + 1, byte_size(Data)),
    {ID, Key, NewKey} = case str:tokens(Header, <<";">>) of
			  [ID1] -> {ID1, <<"">>, <<"">>};
			  [ID1, Key1] -> {ID1, Key1, Key1};
			  [ID1, Key1, NewKey1] -> {ID1, Key1, NewKey1}
			end,
    {ok, ID, Key, NewKey, Packet}.

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive {timeout, Timer, _} -> ok after 0 -> ok end.

%% Resend the polled messages
resend_messages(Messages) ->
%% This function is used to resend messages that have been polled but not
%% delivered.
    lists:foreach(fun (Packet) -> resend_message(Packet)
		  end,
		  Messages).

resend_message(Packet) ->
    #xmlel{name = Name} = ParsedPacket =
			      xml_stream:parse_element(Packet),
    if Name == <<"iq">>;
       Name == <<"message">>;
       Name == <<"presence">> ->
	   From = get_jid(<<"from">>, ParsedPacket),
	   To = get_jid(<<"to">>, ParsedPacket),
	   ?DEBUG("Resend ~p ~p ~p~n", [From, To, ParsedPacket]),
	   ejabberd_router:route(From, To, ParsedPacket);
       true -> ok
    end.

%% Type can be "from" or "to"
%% Parsed packet is a parsed Jabber packet.
get_jid(Type, ParsedPacket) ->
    case xml:get_tag_attr(Type, ParsedPacket) of
      {value, StringJid} -> jlib:string_to_jid(StringJid);
      false -> jlib:make_jid(<<"">>, <<"">>, <<"">>)
    end.
