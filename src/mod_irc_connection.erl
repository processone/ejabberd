%%%----------------------------------------------------------------------
%%% File    : mod_irc_connection.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 15 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(mod_irc_connection).

-author('alexey@process-one.net').

-behaviour(gen_fsm).

%% External exports
-export([start_link/12, start/13, route_chan/4,
	 route_nick/3]).

%% gen_fsm callbacks
-export([init/1, open_socket/2, wait_for_registration/2,
	 stream_established/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3,
	 code_change/4]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(SETS, gb_sets).

-record(state,
	{socket                :: inet:socket() | undefined,
         encoding = <<"">>     :: binary(),
         port = 0              :: inet:port_number(),
         password = <<"">>     :: binary(),
         user = #jid{}         :: jid(),
         host = <<"">>         :: binary(),
	 server = <<"">>       :: binary(),
	 remoteAddr = <<"">>   :: binary(),
	 ident = <<"">>        :: binary(),
	 realname = <<"">>        :: binary(),
         nick = <<"">>         :: binary(),
         channels = dict:new() :: ?TDICT,
         nickchannel           :: binary() | undefined,
	 webirc_password       :: binary(),
         mod = mod_irc         :: atom(),
	 inbuf = <<"">>        :: binary(),
         outbuf = <<"">>       :: binary()}).

-type state() :: #state{}.

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-endif.

start(From, Host, ServerHost, Server, Username,
      Encoding, Port, Password, Ident, RemoteAddr, RealName, WebircPassword, Mod) ->
    Supervisor = gen_mod:get_module_proc(ServerHost,
					 ejabberd_mod_irc_sup),
    supervisor:start_child(Supervisor,
			   [From, Host, Server, Username, Encoding, Port,
			    Password, Ident, RemoteAddr, RealName, WebircPassword, Mod]).

start_link(From, Host, Server, Username, Encoding, Port,
	   Password, Ident, RemoteAddr, RealName, WebircPassword, Mod) ->
    gen_fsm:start_link(?MODULE,
		       [From, Host, Server, Username, Encoding, Port, Password,
			Ident, RemoteAddr, RealName, WebircPassword, Mod],
		       ?FSMOPTS).

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
init([From, Host, Server, Username, Encoding, Port,
      Password, Ident, RemoteAddr, RealName, WebircPassword, Mod]) ->
    gen_fsm:send_event(self(), init),
    {ok, open_socket,
     #state{mod = Mod,
	    encoding = Encoding, port = Port, password = Password,
	    user = From, nick = Username, host = Host,
	    server = Server, ident = Ident, realname = RealName,
		remoteAddr = RemoteAddr, webirc_password = WebircPassword }}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
open_socket(init, StateData) ->
    Addr = StateData#state.server,
    Port = StateData#state.port,
    ?DEBUG("Connecting with IPv6 to ~s:~p", [Addr, Port]),
    From = StateData#state.user,
    #jid{user = JidUser, server = JidServer, resource = JidResource} = From,
    UserIP = ejabberd_sm:get_user_ip(JidUser, JidServer, JidResource),
    UserIPStr = inet_parse:ntoa(element(1, UserIP)),
    Connect6 = gen_tcp:connect(binary_to_list(Addr), Port,
			       [inet6, binary, {packet, 0}]),
    Connect = case Connect6 of
		{error, _} ->
		    ?DEBUG("Connection with IPv6 to ~s:~p failed. "
			   "Now using IPv4.",
			   [Addr, Port]),
		    gen_tcp:connect(binary_to_list(Addr), Port,
				    [inet, binary, {packet, 0}]);
		_ -> Connect6
	      end,
    case Connect of
      {ok, Socket} ->
	  NewStateData = StateData#state{socket = Socket},
	  send_text(NewStateData,
	  io_lib:format("WEBIRC ~s ~s ~s ~s\r\n", [StateData#state.webirc_password, JidResource, UserIPStr, UserIPStr])),
	  if StateData#state.password /= <<"">> ->
		 send_text(NewStateData,
			   io_lib:format("PASS ~s\r\n",
					 [StateData#state.password]));
	     true -> true
	  end,
	  send_text(NewStateData,
		    io_lib:format("NICK ~s\r\n", [StateData#state.nick])),
	  send_text(NewStateData,
		    io_lib:format("USER ~s ~s ~s :~s\r\n",
				  [StateData#state.ident,
				   StateData#state.nick,
				   StateData#state.host,
				   StateData#state.realname])),
	  {next_state, wait_for_registration, NewStateData};
      {error, Reason} ->
	  ?DEBUG("connect return ~p~n", [Reason]),
	  Text = case Reason of
		   timeout -> <<"Server Connect Timeout">>;
		   _ -> <<"Server Connect Failed">>
		 end,
	  bounce_messages(Text),
	  {stop, normal, StateData}
    end.

wait_for_registration(closed, StateData) ->
    {stop, normal, StateData}.

stream_established({xmlstreamend, _Name}, StateData) ->
    {stop, normal, StateData};
stream_established(timeout, StateData) ->
    {stop, normal, StateData};
stream_established(closed, StateData) ->
    {stop, normal, StateData}.

%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
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
handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

-define(SEND(S),
	if StateName == stream_established ->
	       send_text(StateData, S), StateData;
	   true ->
	       StateData#state{outbuf = <<(StateData#state.outbuf)/binary,
                                          (iolist_to_binary(S))/binary>>}
	end).

-spec get_password_from_presence(presence()) -> {true, binary()} | false.
get_password_from_presence(#presence{} = Pres) ->
    case xmpp:get_subtag(Pres, #muc{}) of
	#muc{password = Password} ->
	    {true, Password};
	_ ->
	    false
    end.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({route_chan, _, _, #presence{type = error}}, _, StateData) ->
    {stop, normal, StateData};
handle_info({route_chan, Channel, _, #presence{type = unavailable}},
	    StateName, StateData) ->
    send_stanza_unavailable(Channel, StateData),
    S1 = (?SEND((io_lib:format("PART #~s\r\n", [Channel])))),
    S2 = S1#state{channels = dict:erase(Channel, S1#state.channels)},
    {next_state, StateName, S2};
handle_info({route_chan, Channel, Resource,
	     #presence{type = available} = Presence},
	    StateName, StateData) ->
    Nick = case Resource of
	       <<"">> -> StateData#state.nick;
	       _ -> Resource
	   end,
    S1 = if Nick /= StateData#state.nick ->
		 S11 = (?SEND((io_lib:format("NICK ~s\r\n", [Nick])))),
		 S11#state{nickchannel = Channel};
	    true -> StateData
	 end,
    {next_state, StateName,
     case dict:is_key(Channel, S1#state.channels) of
	 true -> S1;
	 _ ->
	     case get_password_from_presence(Presence) of
		 {true, Password} ->
		     S2 = ?SEND((io_lib:format("JOIN #~s ~s\r\n",
					       [Channel, Password])));
		 _ ->
		     S2 = ?SEND((io_lib:format("JOIN #~s\r\n", [Channel])))
	     end,
	     S2#state{channels = dict:store(Channel, ?SETS:new(),
					    S1#state.channels)}
     end};
handle_info({route_chan, Channel, _Resource, #message{type = groupchat} = Msg},
	    StateName, StateData) ->
    {next_state, StateName,
    case xmpp:get_text(Msg#message.subject) of
	<<"">> ->
	    ejabberd_router:route(
	      xmpp:set_from_to(
		Msg,
		jid:make(
		  iolist_to_binary([Channel, <<"%">>, StateData#state.server]),
		  StateData#state.host,
		  StateData#state.nick),
		StateData#state.user)),
	    Body = xmpp:get_text(Msg#message.body),
	    case Body of
		<<"/quote ", Rest/binary>> ->
		    ?SEND(<<Rest/binary, "\r\n">>);
		<<"/msg ", Rest/binary>> ->
		    ?SEND(<<"PRIVMSG ", Rest/binary, "\r\n">>);
		<<"/me ", Rest/binary>> ->
		    Strings = str:tokens(Rest, <<"\n">>),
		    Res = iolist_to_binary(
			    lists:map(
			      fun (S) ->
				      io_lib:format(
					"PRIVMSG #~s :\001ACTION ~s\001\r\n",
					[Channel, S])
			      end,
			      Strings)),
		    ?SEND(Res);
		<<"/ctcp ", Rest/binary>> ->
		    Words = str:tokens(Rest, <<" ">>),
		    case Words of
			[CtcpDest | _] ->
			    CtcpCmd = str:to_upper(
					str:substr(
					  Rest, str:str(Rest, <<" ">>) + 1)),
			    Res = io_lib:format("PRIVMSG ~s :\001~s\001\r\n",
						[CtcpDest, CtcpCmd]),
			    ?SEND(Res);
			_ -> ok
		    end;
		_ ->
		    Strings = str:tokens(Body, <<"\n">>),
		    Res = iolist_to_binary(
			    lists:map(
			      fun (S) ->
				      io_lib:format("PRIVMSG #~s :~s\r\n",
						    [Channel, S])
			      end, Strings)),
		    ?SEND(Res)
	    end;
	Subject ->
	    Strings = str:tokens(Subject, <<"\n">>),
	    Res = iolist_to_binary(
		    lists:map(
		      fun (S) ->
			      io_lib:format("TOPIC #~s :~s\r\n",
					    [Channel, S])
		      end,
		      Strings)),
	    ?SEND(Res)
    end};
handle_info({route_chan, _Channel, Resource, #message{type = Type} = Msg},
	    StateName, StateData) when Type == chat; Type == normal ->
    Body = xmpp:get_text(Msg#message.body),
    {next_state, StateName,
     case Body of
	 <<"/quote ", Rest/binary>> ->
	     ?SEND(<<Rest/binary, "\r\n">>);
	 <<"/msg ", Rest/binary>> ->
	     ?SEND(<<"PRIVMSG ", Rest/binary, "\r\n">>);
	 <<"/me ", Rest/binary>> ->
	     Strings = str:tokens(Rest, <<"\n">>),
	     Res = iolist_to_binary(
		     lists:map(
		       fun (S) ->
			       io_lib:format(
				 "PRIVMSG ~s :\001ACTION ~s\001\r\n",
				 [Resource, S])
		       end, Strings)),
	     ?SEND(Res);
	 <<"/ctcp ", Rest/binary>> ->
	     Words = str:tokens(Rest, <<" ">>),
	     case Words of
		 [CtcpDest | _] ->
		     CtcpCmd = str:to_upper(
				 str:substr(
				   Rest, str:str(Rest, <<" ">>) + 1)),
		     Res = io_lib:format("PRIVMSG ~s :~s\r\n",
					 [CtcpDest,
					  <<"\001", CtcpCmd/binary, "\001">>]),
		     ?SEND(Res);
		 _ -> ok
	     end;
	 _ ->
	     Strings = str:tokens(Body, <<"\n">>),
	     Res = iolist_to_binary(
		     lists:map(
		       fun (S) ->
			       io_lib:format("PRIVMSG ~s :~s\r\n",
					     [Resource, S])
		       end, Strings)),
	     ?SEND(Res)
     end};
handle_info({route_chan, _, _, #message{type = error}}, _, StateData) ->
    {stop, normal, StateData};
handle_info({route_chan, Channel, Resource,
	     #iq{type = T, sub_els = [_]} = Packet},
	    StateName, StateData) when T == set; T == get ->
    From = StateData#state.user,
    To = jid:make(iolist_to_binary([Channel, <<"%">>, StateData#state.server]),
		  StateData#state.host, StateData#state.nick),
    try xmpp:decode_els(Packet) of
	#iq{sub_els = [SubEl]} = IQ ->
	    case xmpp:get_ns(SubEl) of
		?NS_MUC_ADMIN ->
		    iq_admin(StateData, Channel, From, To, IQ);
		?NS_VERSION ->
		    Res = io_lib:format("PRIVMSG ~s :\001VERSION\001\r\n",
					[Resource]),
		    _ = (?SEND(Res)),
		    Err = xmpp:err_feature_not_implemented(),
		    ejabberd_router:route_error(Packet, Err);
		?NS_TIME ->
		    Res = io_lib:format("PRIVMSG ~s :\001TIME\001\r\n",
					[Resource]),
		    _ = (?SEND(Res)),
		    Err = xmpp:err_feature_not_implemented(),
		    ejabberd_router:route_error(Packet, Err);
		?NS_VCARD ->
		    Res = io_lib:format("WHOIS ~s \r\n", [Resource]),
		    _ = (?SEND(Res)),
		    Err = xmpp:err_feature_not_implemented(),
		    ejabberd_router:route_error(Packet, Err);
		_ ->
		    Err = xmpp:err_feature_not_implemented(),
		    ejabberd_router:route_error(Packet, Err)
	    end
    catch _:{xmpp_codec, Why} ->
	    Err = xmpp:err_bad_request(
		    xmpp:format_error(Why), xmpp:get_lang(Packet)),
	    ejabberd_router:route_error(Packet, Err)
    end,
    {next_state, StateName, StateData};
handle_info({route_chan, _Channel, _, #iq{} = IQ}, StateName, StateData) ->
    Err = xmpp:err_feature_not_implemented(),
    ejabberd_router:route_error(IQ, Err),
    {next_state, StateName, StateData};
handle_info({route_nick, Nick, #message{type = chat} = Msg},
	    StateName, StateData) ->
    Body = xmpp:get_text(Msg#message.body),
    {next_state, StateName,
     case Body of
	 <<"/quote ", Rest/binary>> ->
	     ?SEND(<<Rest/binary, "\r\n">>);
	 <<"/msg ", Rest/binary>> ->
	     ?SEND(<<"PRIVMSG ", Rest/binary, "\r\n">>);
	 <<"/me ", Rest/binary>> ->
	     Strings = str:tokens(Rest, <<"\n">>),
	     Res = iolist_to_binary(
		     lists:map(
		       fun (S) ->
			       io_lib:format(
				 "PRIVMSG ~s :\001ACTION ~s\001\r\n",
				 [Nick, S])
		       end,
		       Strings)),
	     ?SEND(Res);
	 <<"/ctcp ", Rest/binary>> ->
	     Words = str:tokens(Rest, <<" ">>),
	     case Words of
		 [CtcpDest | _] ->
		     CtcpCmd = str:to_upper(
				 str:substr(Rest,
					    str:str(Rest,
						    <<" ">>)
					    + 1)),
		     Res = io_lib:format("PRIVMSG ~s :~s\r\n",
					 [CtcpDest,
					  <<"\001",
					    CtcpCmd/binary,
					    "\001">>]),
		     ?SEND(Res);
		 _ -> ok
	     end;
	 _ ->
	     Strings = str:tokens(Body, <<"\n">>),
	     Res = iolist_to_binary(
		     lists:map(
		       fun (S) ->
			       io_lib:format(
				 "PRIVMSG ~s :~s\r\n",
				 [Nick, S])
		       end,
		       Strings)),
	     ?SEND(Res)
     end};
handle_info({route_nick, _, #message{type = error}}, _, StateData) ->
    {stop, normal, StateData};
handle_info({route_nick, _Nick, _Packet}, StateName,
	    StateData) ->
    {next_state, StateName, StateData};
handle_info({ircstring,
	     <<$P, $I, $N, $G, $\s, ID/binary>>},
	    StateName, StateData) ->
    send_text(StateData, <<"PONG ", ID/binary, "\r\n">>),
    {next_state, StateName, StateData};
handle_info({ircstring, <<$:, String/binary>>},
	    wait_for_registration, StateData) ->
    Words = str:tokens(String, <<" ">>),
    {NewState, NewStateData} = case Words of
				 [_, <<"001">> | _] ->
				     send_text(StateData,
					       io_lib:format("CODEPAGE ~s\r\n",
							     [StateData#state.encoding])),
				     {stream_established, StateData};
				 [_, <<"433">> | _] ->
				     {error,
				      {error,
				       error_nick_in_use(StateData, String),
				       StateData}};
				 [_, <<$4, _, _>> | _] ->
				     {error,
				      {error,
				       error_unknown_num(StateData, String,
							 cancel),
				       StateData}};
				 [_, <<$5, _, _>> | _] ->
				     {error,
				      {error,
				       error_unknown_num(StateData, String,
							 cancel),
				       StateData}};
				 _ ->
				     ?DEBUG("unknown irc command '~s'~n",
					    [String]),
				     {wait_for_registration, StateData}
			       end,
    if NewState == error -> {stop, normal, NewStateData};
       true -> {next_state, NewState, NewStateData}
    end;
handle_info({ircstring, <<$:, String/binary>>},
	    _StateName, StateData) ->
    Words = str:tokens(String, <<" ">>),
    NewStateData = case Words of
		     [_, <<"353">> | Items] ->
			 process_channel_list(StateData, Items);
		     [_, <<"332">>, _Nick, <<$#, Chan/binary>> | _] ->
			 process_channel_topic(StateData, Chan, String),
			 StateData;
		     [_, <<"333">>, _Nick, <<$#, Chan/binary>> | _] ->
			 process_channel_topic_who(StateData, Chan, String),
			 StateData;
		     [_, <<"318">>, _, Nick | _] ->
			 process_endofwhois(StateData, String, Nick), StateData;
		     [_, <<"311">>, _, Nick, Ident, Irchost | _] ->
			 process_whois311(StateData, String, Nick, Ident,
					  Irchost),
			 StateData;
		     [_, <<"312">>, _, Nick, Ircserver | _] ->
			 process_whois312(StateData, String, Nick, Ircserver),
			 StateData;
		     [_, <<"319">>, _, Nick | _] ->
			 process_whois319(StateData, String, Nick), StateData;
		     [_, <<"433">> | _] ->
			 process_nick_in_use(StateData, String);
		     % CODEPAGE isn't standard, so don't complain if it's not there.
		     [_, <<"421">>, _, <<"CODEPAGE">> | _] -> StateData;
		     [_, <<$4, _, _>> | _] ->
			 process_num_error(StateData, String);
		     [_, <<$5, _, _>> | _] ->
			 process_num_error(StateData, String);
		     [From, <<"PRIVMSG">>, <<$#, Chan/binary>> | _] ->
			 process_chanprivmsg(StateData, Chan, From, String),
			 StateData;
		     [From, <<"NOTICE">>, <<$#, Chan/binary>> | _] ->
			 process_channotice(StateData, Chan, From, String),
			 StateData;
		     [From, <<"PRIVMSG">>, Nick, <<":\001VERSION\001">>
		      | _] ->
			 process_version(StateData, Nick, From), StateData;
		     [From, <<"PRIVMSG">>, Nick, <<":\001USERINFO\001">>
		      | _] ->
			 process_userinfo(StateData, Nick, From), StateData;
		     [From, <<"PRIVMSG">>, Nick | _] ->
			 process_privmsg(StateData, Nick, From, String),
			 StateData;
		     [From, <<"NOTICE">>, Nick | _] ->
			 process_notice(StateData, Nick, From, String),
			 StateData;
		     [From, <<"TOPIC">>, <<$#, Chan/binary>> | _] ->
			 process_topic(StateData, Chan, From, String),
			 StateData;
		     [From, <<"PART">>, <<$#, Chan/binary>> | _] ->
			 process_part(StateData, Chan, From, String);
		     [From, <<"QUIT">> | _] ->
			 process_quit(StateData, From, String);
		     [From, <<"JOIN">>, Chan | _] ->
			 process_join(StateData, Chan, From, String);
		     [From, <<"MODE">>, <<$#, Chan/binary>>, <<"+o">>, Nick
		      | _] ->
			 process_mode_o(StateData, Chan, From, Nick,
					admin, moderator),
			 StateData;
		     [From, <<"MODE">>, <<$#, Chan/binary>>, <<"-o">>, Nick
		      | _] ->
			 process_mode_o(StateData, Chan, From, Nick,
					member, participant),
			 StateData;
		     [From, <<"KICK">>, <<$#, Chan/binary>>, Nick | _] ->
			 process_kick(StateData, Chan, From, Nick, String),
			 StateData;
		     [From, <<"NICK">>, Nick | _] ->
			 process_nick(StateData, From, Nick);
		     _ ->
			 ?DEBUG("unknown irc command '~s'~n", [String]),
			 StateData
		   end,
    NewStateData1 = case StateData#state.outbuf of
		      <<"">> -> NewStateData;
		      Data ->
			  send_text(NewStateData, Data),
			  NewStateData#state{outbuf = <<"">>}
		    end,
    {next_state, stream_established, NewStateData1};
handle_info({ircstring,
	     <<$E, $R, $R, $O, $R, _/binary>> = String},
	    StateName, StateData) ->
    process_error(StateData, String),
    {next_state, StateName, StateData};
handle_info({ircstring, String}, StateName,
	    StateData) ->
    ?DEBUG("unknown irc command '~s'~n", [String]),
    {next_state, StateName, StateData};
handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({tcp, _Socket, Data}, StateName,
	    StateData) ->
    Buf = <<(StateData#state.inbuf)/binary, Data/binary>>,
    Strings = ejabberd_regexp:split(<< <<C>>
                                       || <<C>> <= Buf, C /= $\r >>,
				    <<"\n">>),
    ?DEBUG("strings=~p~n", [Strings]),
    NewBuf = process_lines(StateData#state.encoding,
			   Strings),
    {next_state, StateName,
     StateData#state{inbuf = NewBuf}};
handle_info({tcp_closed, _Socket}, StateName,
	    StateData) ->
    gen_fsm:send_event(self(), closed),
    {next_state, StateName, StateData};
handle_info({tcp_error, _Socket, _Reason}, StateName,
	    StateData) ->
    gen_fsm:send_event(self(), closed),
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, FullStateData) ->
    {Error, StateData} = case FullStateData of
			   {error, SError, SStateData} -> {SError, SStateData};
			   _ ->
			       {xmpp:err_internal_server_error(
				  <<"Server Connect Failed">>, ?MYLANG),
				FullStateData}
			 end,
    (StateData#state.mod):closed_connection(StateData#state.host,
                                            StateData#state.user,
                                            StateData#state.server),
    bounce_messages(<<"Server Connect Failed">>),
    lists:foreach(fun (Chan) ->
			  Stanza = xmpp:make_error(#presence{}, Error),
			  send_stanza(Chan, StateData, Stanza)
		  end,
		  dict:fetch_keys(StateData#state.channels)),
    case StateData#state.socket of
      undefined -> ok;
      Socket -> gen_tcp:close(Socket)
    end,
    ok.

-spec send_stanza(binary(), state(), stanza()) -> ok.
send_stanza(Chan, StateData, Stanza) ->
    ejabberd_router:route(
      xmpp:set_from_to(
	Stanza,
	jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
		 StateData#state.host,
		 StateData#state.nick),
	StateData#state.user)).

-spec send_stanza_unavailable(binary(), state()) -> ok.
send_stanza_unavailable(Chan, StateData) ->
    Affiliation = member,
    Role = none,
    Stanza = #presence{
		type = unavailable,
		sub_els = [#muc_user{
			      items = [#muc_item{affiliation = Affiliation,
						 role = Role}],
			      status_codes = [110]}]},
    send_stanza(Chan, StateData, Stanza).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(#state{socket = Socket, encoding = Encoding},
	  Text) ->
    CText = iconv:convert(<<"utf-8">>, Encoding, iolist_to_binary(Text)),
    gen_tcp:send(Socket, CText).

bounce_messages(Reason) ->
    receive
	{send_element, El} ->
	    Lang = xmpp:get_lang(El),
	    Err = xmpp:err_internal_server_error(Reason, Lang),
	    ejabberd_router:route_error(El, Err),
	    bounce_messages(Reason)
    after 0 -> ok
    end.

route_chan(Pid, Channel, Resource, Packet) ->
    Pid ! {route_chan, Channel, Resource, Packet}.

route_nick(Pid, Nick, Packet) ->
    Pid ! {route_nick, Nick, Packet}.

process_lines(_Encoding, [S]) -> S;
process_lines(Encoding, [S | Ss]) ->
    self() !
      {ircstring, iconv:convert(Encoding, <<"utf-8">>, S)},
    process_lines(Encoding, Ss).

process_channel_list(StateData, Items) ->
    process_channel_list_find_chan(StateData, Items).

process_channel_list_find_chan(StateData, []) ->
    StateData;
process_channel_list_find_chan(StateData,
			       [<<$#, Chan/binary>> | Items]) ->
    process_channel_list_users(StateData, Chan, Items);
process_channel_list_find_chan(StateData,
			       [_ | Items]) ->
    process_channel_list_find_chan(StateData, Items).

process_channel_list_users(StateData, _Chan, []) ->
    StateData;
process_channel_list_users(StateData, Chan,
			   [User | Items]) ->
    NewStateData = process_channel_list_user(StateData,
					     Chan, User),
    process_channel_list_users(NewStateData, Chan, Items).

process_channel_list_user(StateData, Chan, User) ->
    User1 = case User of
	      <<$:, U1/binary>> -> U1;
	      _ -> User
	    end,
    {User2, Affiliation, Role} = case User1 of
				   <<$@, U2/binary>> ->
				       {U2, admin, moderator};
				   <<$+, U2/binary>> ->
				       {U2, member, participant};
				   <<$%, U2/binary>> ->
				       {U2, admin, moderator};
				   <<$&, U2/binary>> ->
				       {U2, admin, moderator};
				   <<$~, U2/binary>> ->
				       {U2, admin, moderator};
				   _ -> {User1, member, participant}
				 end,
    ejabberd_router:route(
      #presence{
	 from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			 StateData#state.host, User2),
	 to = StateData#state.user,
	 sub_els = [#muc_user{items = [#muc_item{affiliation = Affiliation,
						 role = Role}]}]}),
    case catch dict:update(Chan,
			   fun (Ps) -> (?SETS):add_element(User2, Ps) end,
			   StateData#state.channels) of
	{'EXIT', _} -> StateData;
	NS -> StateData#state{channels = NS}
    end.

process_channel_topic(StateData, Chan, String) ->
    Msg = ejabberd_regexp:replace(String, <<".*332[^:]*:">>,
				  <<"">>),
    Subject = filter_message(Msg),
    Body = <<"Topic for #", Chan/binary, ": ", Subject/binary>>,
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			       StateData#state.host),
	       to = StateData#state.user,
	       type = groupchat,
	       subject = xmpp:mk_text(Subject),
	       body = xmpp:mk_text(Body)}).

process_channel_topic_who(StateData, Chan, String) ->
    Words = str:tokens(String, <<" ">>),
    Msg1 = case Words of
	     [_, <<"333">>, _, _Chan, Whoset, Timeset] ->
		 {Unixtimeset, _Rest} = str:to_integer(Timeset),
		 <<"Topic for #", Chan/binary, " set by ", Whoset/binary,
		   " at ", (unixtime2string(Unixtimeset))/binary>>;
	     [_, <<"333">>, _, _Chan, Whoset | _] ->
		 <<"Topic for #", Chan/binary, " set by ",
		   Whoset/binary>>;
	     _ -> String
	   end,
    Msg2 = filter_message(Msg1),
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			       StateData#state.host, <<"">>),
	       to = StateData#state.user,
	       type = groupchat, body = xmpp:mk_text(Msg2)}).

error_nick_in_use(_StateData, String) ->
    Msg = ejabberd_regexp:replace(String,
				  <<".*433 +[^ ]* +">>, <<"">>),
    Msg1 = filter_message(Msg),
    xmpp:err_conflict(Msg1, ?MYLANG).

process_nick_in_use(StateData, String) ->
    Error = error_nick_in_use(StateData, String),
    case StateData#state.nickchannel of
      undefined ->
	  % Shouldn't happen with a well behaved server
	  StateData;
      Chan ->
	  ejabberd_router:route(
	    #presence{from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
				      StateData#state.host,
				      StateData#state.nick),
		      to = StateData#state.user,
		      type = error, sub_els = [Error]}),
	    StateData#state{nickchannel = undefined}
    end.

process_num_error(StateData, String) ->
    Error = error_unknown_num(StateData, String, continue),
    lists:foreach(
      fun(Chan) ->
	      ejabberd_router:route(
		#message{from = jid:make(iolist_to_binary([Chan, $%, StateData#state.server]),
					 StateData#state.host,
					 StateData#state.nick),
			 to = StateData#state.user,
			 type = error, sub_els = [Error]})
      end, dict:fetch_keys(StateData#state.channels)),
    StateData.

process_endofwhois(StateData, _String, Nick) ->
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([Nick, <<"!">>, StateData#state.server]),
			       StateData#state.host),
	       to = StateData#state.user,
	       type = chat, body = xmpp:mk_text(<<"End of WHOIS">>)}).

process_whois311(StateData, String, Nick, Ident,
		 Irchost) ->
    Fullname = ejabberd_regexp:replace(String,
				       <<".*311[^:]*:">>, <<"">>),
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([Nick, <<"!">>, StateData#state.server]),
			       StateData#state.host, <<"">>),
	       to = StateData#state.user,
	       type = chat,
	       body = xmpp:mk_text(
			iolist_to_binary(
			  [<<"WHOIS: ">>, Nick, <<" is ">>, Ident,
			   <<"@">>, Irchost, <<" : ">>, Fullname]))}).

process_whois312(StateData, String, Nick, Ircserver) ->
    Ircserverdesc = ejabberd_regexp:replace(String,
					    <<".*312[^:]*:">>, <<"">>),
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([Nick, <<"!">>, StateData#state.server]),
			       StateData#state.host, <<"">>),
	       to = StateData#state.user,
	       type = chat,
	       body = xmpp:mk_text(
			iolist_to_binary(
			  [<<"WHOIS: ">>, Nick, <<" use ">>, Ircserver,
			   <<" : ">>, Ircserverdesc]))}).

process_whois319(StateData, String, Nick) ->
    Chanlist = ejabberd_regexp:replace(String,
				       <<".*319[^:]*:">>, <<"">>),
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([Nick, <<"!">>, StateData#state.server]),
			       StateData#state.host, <<"">>),
	       to = StateData#state.user,
	       type = chat,
	       body = xmpp:mk_text(
			iolist_to_binary(
			  [<<"WHOIS: ">>, Nick, <<" is on ">>, Chanlist]))}).

process_chanprivmsg(StateData, Chan, From, String) ->
    [FromUser | _] = str:tokens(From, <<"!">>),
    Msg = ejabberd_regexp:replace(String,
				  <<".*PRIVMSG[^:]*:">>, <<"">>),
    Msg1 = case Msg of
	     <<1, $A, $C, $T, $I, $O, $N, $\s, Rest/binary>> ->
		 <<"/me ", Rest/binary>>;
	     _ -> Msg
	   end,
    Msg2 = filter_message(Msg1),
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			       StateData#state.host, FromUser),
	       to = StateData#state.user,
	       type = groupchat, body = xmpp:mk_text(Msg2)}).

process_channotice(StateData, Chan, From, String) ->
    [FromUser | _] = str:tokens(From, <<"!">>),
    Msg = ejabberd_regexp:replace(String,
				  <<".*NOTICE[^:]*:">>, <<"">>),
    Msg1 = case Msg of
	     <<1, $A, $C, $T, $I, $O, $N, $\s, Rest/binary>> ->
		 <<"/me ", Rest/binary>>;
	     _ -> <<"/me NOTICE: ", Msg/binary>>
	   end,
    Msg2 = filter_message(Msg1),
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			       StateData#state.host, FromUser),
	       to = StateData#state.user,
	       type = groupchat, body = xmpp:mk_text(Msg2)}).

process_privmsg(StateData, _Nick, From, String) ->
    [FromUser | _] = str:tokens(From, <<"!">>),
    Msg = ejabberd_regexp:replace(String,
				  <<".*PRIVMSG[^:]*:">>, <<"">>),
    Msg1 = case Msg of
	     <<1, $A, $C, $T, $I, $O, $N, $\s, Rest/binary>> ->
		 <<"/me ", Rest/binary>>;
	     _ -> Msg
	   end,
    Msg2 = filter_message(Msg1),
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([FromUser, <<"!">>, StateData#state.server]),
			       StateData#state.host, <<"">>),
	       to = StateData#state.user,
	       type = chat, body = xmpp:mk_text(Msg2)}).

process_notice(StateData, _Nick, From, String) ->
    [FromUser | _] = str:tokens(From, <<"!">>),
    Msg = ejabberd_regexp:replace(String,
				  <<".*NOTICE[^:]*:">>, <<"">>),
    Msg1 = case Msg of
	     <<1, $A, $C, $T, $I, $O, $N, $\s, Rest/binary>> ->
		 <<"/me ", Rest/binary>>;
	     _ -> <<"/me NOTICE: ", Msg/binary>>
	   end,
    Msg2 = filter_message(Msg1),
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([FromUser, <<"!">>, StateData#state.server]),
			       StateData#state.host),
	       to = StateData#state.user,
	       type = chat, body = xmpp:mk_text(Msg2)}).

process_version(StateData, _Nick, From) ->
    [FromUser | _] = str:tokens(From, <<"!">>),
    send_text(StateData,
	      io_lib:format("NOTICE ~s :\001VERSION ejabberd IRC "
			    "transport ~s (c) Alexey Shchepin\001\r\n",
			    [FromUser, ?VERSION])
		++
		io_lib:format("NOTICE ~s :\001VERSION http://ejabberd.jabber"
			      "studio.org/\001\r\n",
			      [FromUser])).

process_userinfo(StateData, _Nick, From) ->
    [FromUser | _] = str:tokens(From, <<"!">>),
    send_text(StateData,
	      io_lib:format("NOTICE ~s :\001USERINFO xmpp:~s\001\r\n",
			    [FromUser,
			     jid:encode(StateData#state.user)])).

process_topic(StateData, Chan, From, String) ->
    [FromUser | _] = str:tokens(From, <<"!">>),
    Msg = ejabberd_regexp:replace(String,
				  <<".*TOPIC[^:]*:">>, <<"">>),
    Msg1 = filter_message(Msg),
    ejabberd_router:route(
      #message{from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			       StateData#state.host, FromUser),
	       to = StateData#state.user,
	       type = groupchat,
	       subject = xmpp:mk_text(Msg1),
	       body = xmpp:mk_text(<<"/me has changed the subject to: ",
				     Msg1/binary>>)}).

process_part(StateData, Chan, From, String) ->
    [FromUser | FromIdent] = str:tokens(From, <<"!">>),
    Msg = ejabberd_regexp:replace(String,
				  <<".*PART[^:]*:">>, <<"">>),
    Msg1 = filter_message(Msg),
    ejabberd_router:route(
      #presence{from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
				StateData#state.host, FromUser),
		to = StateData#state.user,
		type = unavailable,
		sub_els = [#muc_user{
			      items = [#muc_item{affiliation = member,
						 role = none}]}],
		status = xmpp:mk_text(
			   list_to_binary([Msg1, " (", FromIdent, ")"]))}),
    case catch dict:update(Chan,
			   fun (Ps) -> remove_element(FromUser, Ps) end,
			   StateData#state.channels)
	of
      {'EXIT', _} -> StateData;
      NS -> StateData#state{channels = NS}
    end.

process_quit(StateData, From, String) ->
    [FromUser | FromIdent] = str:tokens(From, <<"!">>),
    Msg = ejabberd_regexp:replace(String,
				  <<".*QUIT[^:]*:">>, <<"">>),
    Msg1 = filter_message(Msg),
    dict:map(
      fun(Chan, Ps) ->
	      case (?SETS):is_member(FromUser, Ps) of
		  true ->
		      ejabberd_router:route(
			#presence{from = jid:make(iolist_to_binary([Chan, $%, StateData#state.server]),
						  StateData#state.host,
						  FromUser),
				  to = StateData#state.user,
				  type = unavailable,
				  sub_els = [#muc_user{
						items = [#muc_item{
							    affiliation = member,
							    role = none}]}],
				  status = xmpp:mk_text(
					     list_to_binary([Msg1, " (", FromIdent, ")"]))}),
		      remove_element(FromUser, Ps);
		  _ ->
		      Ps
	      end
      end, StateData#state.channels),
    StateData.

process_join(StateData, Channel, From, _String) ->
    [FromUser | FromIdent] = str:tokens(From, <<"!">>),
    [Chan | _] = binary:split(Channel, <<":#">>),
    ejabberd_router:route(
      #presence{
	 from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			 StateData#state.host, FromUser),
	 to = StateData#state.user,
	 sub_els = [#muc_user{items = [#muc_item{affiliation = member,
						 role = participant}]}],
	 status = xmpp:mk_text(list_to_binary(FromIdent))}),
    case catch dict:update(Chan,
			   fun (Ps) -> (?SETS):add_element(FromUser, Ps) end,
			   StateData#state.channels)
	of
      {'EXIT', _} -> StateData;
      NS -> StateData#state{channels = NS}
    end.

process_mode_o(StateData, Chan, _From, Nick,
	       Affiliation, Role) ->
    ejabberd_router:route(
      #presence{
	 from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			 StateData#state.host, Nick),
	 to = StateData#state.user,
	 sub_els = [#muc_user{items = [#muc_item{affiliation = Affiliation,
						 role = Role}]}]}).

process_kick(StateData, Chan, From, Nick, String) ->
    Msg = lists:last(str:tokens(String, <<":">>)),
    Msg2 = <<Nick/binary, " kicked by ", From/binary, " (",
	     (filter_message(Msg))/binary, ")">>,
    ejabberd_router:route(
      #message{
	 from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			 StateData#state.host),
	 to = StateData#state.user,
	 type = groupchat, body = xmpp:mk_text(Msg2)}),
    ejabberd_router:route(
      #presence{
	 from = jid:make(iolist_to_binary([Chan, <<"%">>, StateData#state.server]),
			 StateData#state.host, Nick),
	 to = StateData#state.user,
	 type = unavailable,
		sub_els = [#muc_user{items = [#muc_item{
						 affiliation = none,
						 role = none}],
				     status_codes = [307]}]}).

process_nick(StateData, From, NewNick) ->
    [FromUser | _] = str:tokens(From, <<"!">>),
    [Nick | _] = binary:split(NewNick, <<":">>),
    NewChans =
	dict:map(
	  fun(Chan, Ps) ->
		  case (?SETS):is_member(FromUser, Ps) of
		      true ->
			  ejabberd_router:route(
			    #presence{
			       from = jid:make(iolist_to_binary([Chan, $%, StateData#state.server]),
					       StateData#state.host,
					       FromUser),
			       to = StateData#state.user,
			       type = unavailable,
			       sub_els = [#muc_user{
					     items = [#muc_item{
							 affiliation = member,
							 role = participant,
							 nick = Nick}],
					     status_codes = [303]}]}),
			  ejabberd_router:route(
			    #presence{
			       from = jid:make(iolist_to_binary([Chan, $%, StateData#state.server]),
					       StateData#state.host, Nick),
			       to = StateData#state.user,
			       sub_els = [#muc_user{
					     items = [#muc_item{
							 affiliation = member,
							 role = participant}]}]}),
			  (?SETS):add_element(Nick, remove_element(FromUser, Ps));
		      _ -> Ps
		  end
	  end, StateData#state.channels),
    if FromUser == StateData#state.nick ->
	   StateData#state{nick = Nick, nickchannel = undefined,
			   channels = NewChans};
       true -> StateData#state{channels = NewChans}
    end.

process_error(StateData, String) ->
    lists:foreach(
      fun(Chan) ->
	      ejabberd_router:route(
		#presence{
		   from = jid:make(iolist_to_binary([Chan, $%, StateData#state.server]),
				   StateData#state.host,
				   StateData#state.nick),
		   to = StateData#state.user,
		   type = error,
		   sub_els = [xmpp:err_internal_server_error(String, ?MYLANG)]})
      end, dict:fetch_keys(StateData#state.channels)).

error_unknown_num(_StateData, String, Type) ->
    Msg = ejabberd_regexp:replace(String,
				  <<".*[45][0-9][0-9] +[^ ]* +">>, <<"">>),
    Msg1 = filter_message(Msg),
    xmpp:err_undefined_condition(Type, Msg1, ?MYLANG).

remove_element(E, Set) ->
    case (?SETS):is_element(E, Set) of
      true -> (?SETS):del_element(E, Set);
      _ -> Set
    end.

iq_admin(StateData, Channel, From, _To,
	 #iq{type = Type, sub_els = [SubEl]} = IQ) ->
    try process_iq_admin(StateData, Channel, Type, SubEl) of
	{result, Result} ->
	    ejabberd_router:route(xmpp:make_iq_result(IQ, Result));
	{error, Error} ->
	    ejabberd_router:route_error(IQ, Error)
    catch E:R ->
	    ?ERROR_MSG("failed to process admin query from ~s: ~p",
		       [jid:encode(From), {E, {R, erlang:get_stacktrace()}}]),
	    ejabberd_router:route_error(
	      IQ, xmpp:err_internal_server_error())
    end.

process_iq_admin(_StateData, _Channel, set, #muc_admin{items = []}) ->
    {error, xmpp:err_bad_request()};
process_iq_admin(StateData, Channel, set, #muc_admin{items = [Item|_]}) ->
    process_admin(StateData, Channel, Item);
process_iq_admin(_StateData, _Channel, _, _SubEl) ->
    {error, xmpp:err_feature_not_implemented()}.

process_admin(_StateData, _Channel, #muc_item{nick = <<"">>}) ->
    {error, xmpp:err_feature_not_implemented()};
process_admin(StateData, Channel, #muc_item{nick = Nick,
					    reason = Reason,
					    role = none}) ->
    case Reason of
      <<"">> ->
	  send_text(StateData,
		    io_lib:format("KICK #~s ~s\r\n", [Channel, Nick]));
      _ ->
	  send_text(StateData,
		    io_lib:format("KICK #~s ~s :~s\r\n",
				  [Channel, Nick, Reason]))
    end,
    {result, undefined};
process_admin(_StateData, _Channel, _Item) ->
    {error, xmpp:err_feature_not_implemented()}.

filter_message(Msg) ->
    list_to_binary(
      lists:filter(fun (C) ->
                           if (C < 32) and (C /= 9) and (C /= 10) and (C /= 13) ->
                                   false;
                              true -> true
                           end
                   end,
                   binary_to_list(filter_mirc_colors(Msg)))).

filter_mirc_colors(Msg) ->
    ejabberd_regexp:greplace(Msg,
			     <<"(\\003[0-9]+)(,[0-9]+)?">>, <<"">>).

unixtime2string(Unixtime) ->
    Secs = Unixtime +
	     calendar:datetime_to_gregorian_seconds({{1970, 1, 1},
						     {0, 0, 0}}),
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(Secs)),
    (str:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
				   [Year, Month, Day, Hour, Minute, Second])).
