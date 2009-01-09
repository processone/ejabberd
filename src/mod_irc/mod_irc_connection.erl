%%%----------------------------------------------------------------------
%%% File    : mod_irc_connection.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Created : 15 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_irc_connection).
-author('alexey@process-one.net').

-behaviour(gen_fsm).

%% External exports
-export([start_link/5, start/6, route_chan/4, route_nick/3]).

%% gen_fsm callbacks
-export([init/1,
	 open_socket/2,
	 wait_for_registration/2,
	 stream_established/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

-define(SETS, gb_sets).

-record(state, {socket, encoding, queue,
		user, host, server, nick,
		channels = dict:new(),
		inbuf = "", outbuf = ""}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(From, Host, ServerHost, Server, Username, Encoding) ->
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_irc_sup),
    supervisor:start_child(
      Supervisor, [From, Host, Server, Username, Encoding]).

start_link(From, Host, Server, Username, Encoding) ->
    gen_fsm:start_link(?MODULE, [From, Host, Server, Username, Encoding],
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
init([From, Host, Server, Username, Encoding]) ->
    gen_fsm:send_event(self(), init),
    {ok, open_socket, #state{queue = queue:new(),
			     encoding = Encoding,
			     user = From,
			     nick = Username,
			     host = Host,
			     server = Server}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
open_socket(init, StateData) ->
    Addr = StateData#state.server,
    Port = 6667,
    ?DEBUG("connecting to ~s:~p~n", [Addr, Port]),
    case gen_tcp:connect(Addr, Port, [binary, {packet, 0}]) of
	{ok, Socket} ->
	    NewStateData = StateData#state{socket = Socket},
	    send_text(NewStateData,
		      io_lib:format("NICK ~s\r\n", [StateData#state.nick])),
	    send_text(NewStateData,
		      io_lib:format(
			"USER ~s ~s ~s :~s\r\n",
			[StateData#state.nick,
			 StateData#state.nick,
			 StateData#state.host,
			 StateData#state.nick])),
	    send_text(NewStateData,
		      io_lib:format("CODEPAGE ~s\r\n", [StateData#state.encoding])),
	    {next_state, wait_for_registration,
	     NewStateData};
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
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

-define(SEND(S),
	if
	    StateName == stream_established ->
		send_text(StateData, S),
		StateData;
	    true ->
		StateData#state{outbuf = StateData#state.outbuf ++ S}
	end).

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info({route_chan, Channel, Resource,
	     El},
	    StateName, StateData) when ?IS_PRESENCE(El) ->
    NewStateData =
	case exmpp_presence:get_type(El) of
	    'unavailable' ->
		S1 = ?SEND(io_lib:format("PART #~s\r\n", [Channel])),
		S1#state{channels =
			 dict:erase(Channel, S1#state.channels)};
	    'subscribe' -> StateData;
	    'subscribed' -> StateData;
	    'unsubscribe' -> StateData;
	    'unsubscribed' -> StateData;
	    'error' -> stop;
	    _ ->
		Nick = case Resource of
			   undefined ->
			       StateData#state.nick;
			   _ ->
			       Resource
		       end,
		S1 = ?SEND(io_lib:format("NICK ~s\r\n"
					 "JOIN #~s\r\n",
					 [Nick, Channel])),
		case dict:is_key(Channel, S1#state.channels) of
		    true ->
			S1#state{nick = Nick};
		    _ ->
			S1#state{nick = Nick,
				 channels =
				 dict:store(Channel, ?SETS:new(),
					    S1#state.channels)}
		end
	end,
    if
	NewStateData == stop ->
	    {stop, normal, StateData};
	true ->
	    case length(dict:fetch_keys(NewStateData#state.channels)) of
		0 ->
		    {stop, normal, NewStateData};
		_ ->
		    {next_state, StateName, NewStateData}
	    end
    end;

handle_info({route_chan, Channel, Resource,
	     El},
	    StateName, StateData) when ?IS_MESSAGE(El) ->
    NewStateData =
	case exmpp_message:get_type(El) of
	    'groupchat' ->
		case exmpp_message:get_subject(El) of
		    undefined ->
			ejabberd_router:route(
			  exmpp_jid:make_jid(
			    lists:concat(
			      [Channel, "%", StateData#state.server]),
			    StateData#state.host, StateData#state.nick),
			  StateData#state.user, El),
			Body = binary_to_list(exmpp_message:get_body(El)),
			case Body of
			    "/quote " ++ Rest ->
				?SEND(Rest ++ "\r\n");
			    "/msg " ++ Rest ->
				?SEND("PRIVMSG " ++ Rest ++ "\r\n");
			    "/me " ++ Rest ->
				Strings = string:tokens(Rest, "\n"),
				Res = lists:concat(
					lists:map(
					  fun(S) ->
						  io_lib:format(
						    "PRIVMSG #~s :\001ACTION ~s\001\r\n",
						    [Channel, S])
					  end, Strings)),
				?SEND(Res);
			    "/ctcp " ++ Rest ->
				Words = string:tokens(Rest, " "),
				case Words of
				    [CtcpDest | _] ->
					CtcpCmd =
					    toupper(
					      string:substr(
						Rest,
						string:str(Rest, " ") + 1)),
					Res = io_lib:format(
						"PRIVMSG ~s :\001~s\001\r\n",
						[CtcpDest, CtcpCmd]),
					?SEND(Res);
				    _ ->
					ok
				end;
			    _ ->
				Strings = string:tokens(Body, "\n"),
				Res = lists:concat(
					lists:map(
					  fun(S) ->
						  io_lib:format(
						    "PRIVMSG #~s :~s\r\n",
						    [Channel, S])
					  end, Strings)),
				?SEND(Res)
			end;
		    Subject ->
			Strings = string:tokens(Subject, "\n"),
			Res = lists:concat(
				lists:map(
				  fun(S) ->
					  io_lib:format("TOPIC #~s :~s\r\n",
							[Channel, S])
				  end, Strings)),
			?SEND(Res)
		end;
	    Type when Type == 'chat'; Type == 'normal' ->
		Body = binary_to_list(exmpp_message:get_body(El)),
		case Body of
		    "/quote " ++ Rest ->
			?SEND(Rest ++ "\r\n");
		    "/msg " ++ Rest ->
			?SEND("PRIVMSG " ++ Rest ++ "\r\n");
		    "/me " ++ Rest ->
			Strings = string:tokens(Rest, "\n"),
			Res = lists:concat(
				lists:map(
				  fun(S) ->
					  io_lib:format(
					    "PRIVMSG ~s :\001ACTION ~s\001\r\n",
					    [Resource, S])
				  end, Strings)),
			?SEND(Res);
		    "/ctcp " ++ Rest ->
		    	Words = string:tokens(Rest, " "),
			case Words of
			    [CtcpDest | _ ] ->
				CtcpCmd =
				    toupper(
				      string:substr(
					Rest, string:str(Rest, " ") + 1)),
				Res = io_lib:format(
					"PRIVMSG ~s :~s\r\n",
					[CtcpDest, "\001" ++ CtcpCmd ++ "\001"]),
				?SEND(Res);
			    _ ->
			        ok
			end;
		    _ ->
			Strings = string:tokens(Body, "\n"),
			Res = lists:concat(
				lists:map(
				  fun(S) ->
					  io_lib:format("PRIVMSG ~s :~s\r\n",
							[Resource, S])
				  end, Strings)),
			?SEND(Res)
		end;
	    'error' ->
		stop;
	    _ ->
		StateData
	end,
    if
	NewStateData == stop ->
	    {stop, normal, StateData};
	true ->
	    {next_state, StateName, NewStateData}
    end;


handle_info({route_chan, Channel, Resource,
	     El},
	    StateName, StateData) when ?IS_IQ(El) ->
    From = StateData#state.user,
    To = exmpp_jid:make_jid(lists:concat([Channel, "%", StateData#state.server]),
		       StateData#state.host, StateData#state.nick),
    case exmpp_iq:xmlel_to_iq(El) of
	#iq{kind = request, ns = ?NS_MUC_ADMIN} = IQ_Rec ->
	    iq_admin(StateData, Channel, From, To, IQ_Rec);
	#iq{kind = request, ns = ?NS_SOFT_VERSION} ->
	    Res = io_lib:format("PRIVMSG ~s :\001VERSION\001\r\n",
				[Resource]),
	    ?SEND(Res),
	    Err = exmpp_iq:error(El, 'feature-not-implemented'),
	    ejabberd_router:route(To, From, Err);
	#iq{kind = request, ns = ?NS_TIME_OLD} ->
	    Res = io_lib:format("PRIVMSG ~s :\001TIME\001\r\n",
				[Resource]),
	    ?SEND(Res),
	    Err = exmpp_iq:error(El, 'feature-not-implemented'),
	    ejabberd_router:route(To, From, Err);
	#iq{kind = request, ns = ?NS_VCARD} ->
	    Res = io_lib:format("WHOIS ~s \r\n",
				[Resource]),
	    ?SEND(Res),
	    Err = exmpp_iq:error(El, 'feature-not-implemented'),
	    ejabberd_router:route(To, From, Err);
	#iq{} ->
	    Err = exmpp_iq:error(El, 'feature-not-implemented'),
	    ejabberd_router:route(To, From, Err);
	_ ->
	    ok
    end,
    {next_state, StateName, StateData};

handle_info({route_chan, _Channel, _Resource, _Packet}, StateName, StateData) ->
    {next_state, StateName, StateData};


handle_info({route_nick, Nick,
	     El},
	    StateName, StateData) when ?IS_MESSAGE(El) ->
    NewStateData =
	case exmpp_message:get_type(El) of
	    'chat' ->
		Body = binary_to_list(exmpp_message:get_body(El)),
		case Body of
		    "/quote " ++ Rest ->
			?SEND(Rest ++ "\r\n");
		    "/msg " ++ Rest ->
			?SEND("PRIVMSG " ++ Rest ++ "\r\n");
		    "/me " ++ Rest ->
			Strings = string:tokens(Rest, "\n"),
			Res = lists:concat(
				lists:map(
				  fun(S) ->
					  io_lib:format(
					    "PRIVMSG ~s :\001ACTION ~s\001\r\n",
					    [Nick, S])
				  end, Strings)),
			?SEND(Res);
		    "/ctcp " ++ Rest ->
		    	Words = string:tokens(Rest, " "),
			case Words of
			    [CtcpDest | _ ] ->
				CtcpCmd = toupper(string:substr(Rest, string:str(Rest, " ")+1 )),
				Res = io_lib:format(
				    "PRIVMSG ~s :~s\r\n",
				    [CtcpDest, "\001" ++ CtcpCmd ++ "\001"]),
				?SEND(Res);
			     _ ->
			        ok
			end;
		    _ ->
			Strings = string:tokens(Body, "\n"),
			Res = lists:concat(
				lists:map(
				  fun(S) ->
					  io_lib:format("PRIVMSG ~s :~s\r\n",
							[Nick, S])
				  end, Strings)),
			?SEND(Res)
		end;
	    'error' ->
		stop;
	    _ ->
		StateData
	end,
    if
	NewStateData == stop ->
	    {stop, normal, StateData};
	true ->
	    {next_state, StateName, NewStateData}
    end;

handle_info({route_nick, _Nick, _Packet}, StateName, StateData) ->
    {next_state, StateName, StateData};


handle_info({ircstring, [$P, $I, $N, $G, $  | ID]}, StateName, StateData) ->
    send_text(StateData, "PONG " ++ ID ++ "\r\n"),
    {next_state, StateName, StateData};

handle_info({ircstring, [$: | String]}, _StateName, StateData) ->
    Words = string:tokens(String, " "),
    NewStateData =
	case Words of
	    [_, "353" | Items] ->
		process_channel_list(StateData, Items);
	    [_, "332", _Nick, [$# | Chan] | _] ->
		process_channel_topic(StateData, Chan, String),
		StateData;
	    [_, "333", _Nick, [$# | Chan] | _] ->
		process_channel_topic_who(StateData, Chan, String),
		StateData;
	    [_, "318", _, Nick | _] ->
		process_endofwhois(StateData, String, Nick),
		StateData;
	    [_, "311", _, Nick, Ident, Irchost | _ ] ->
		process_whois311(StateData, String, Nick, Ident, Irchost),
		StateData;
	    [_, "312", _, Nick, Ircserver  | _ ] ->
		process_whois312(StateData, String, Nick, Ircserver),
		StateData;
	    [_, "319", _, Nick | _ ] ->
		process_whois319(StateData, String, Nick),
		StateData;
	    [From, "PRIVMSG", [$# | Chan] | _] ->
		process_chanprivmsg(StateData, Chan, From, String),
		StateData;
	    [From, "NOTICE", [$# | Chan] | _] ->
		process_channotice(StateData, Chan, From, String),
		StateData;
	    [From, "PRIVMSG", Nick, ":\001VERSION\001" | _] ->
		process_version(StateData, Nick, From),
		StateData;
	    [From, "PRIVMSG", Nick, ":\001USERINFO\001" | _] ->
		process_userinfo(StateData, Nick, From),
		StateData;
	    [From, "PRIVMSG", Nick | _] ->
		process_privmsg(StateData, Nick, From, String),
		StateData;
	    [From, "NOTICE", Nick | _] ->
		process_notice(StateData, Nick, From, String),
		StateData;
	    [From, "TOPIC", [$# | Chan] | _] ->
		process_topic(StateData, Chan, From, String),
		StateData;
	    [From, "PART", [$# | Chan] | _] ->
		process_part(StateData, Chan, From, String);
	    [From, "QUIT" | _] ->
		process_quit(StateData, From, String);
	    [From, "JOIN", Chan | _] ->
		process_join(StateData, Chan, From, String);
	    [From, "MODE", [$# | Chan], "+o", Nick | _] ->
		process_mode_o(StateData, Chan, From, Nick,
			       "admin", "moderator"),
		StateData;
	    [From, "MODE", [$# | Chan], "-o", Nick | _] ->
		process_mode_o(StateData, Chan, From, Nick,
			       "member", "participant"),
		StateData;
	    [From, "KICK", [$# | Chan], Nick | _] ->
		process_kick(StateData, Chan, From, Nick, String),
		StateData;
	    [From, "NICK", Nick | _] ->
		process_nick(StateData, From, Nick);
	    _ ->
		?DEBUG("unknown irc command '~s'~n", [String]),
		StateData
	end,
    NewStateData1 =
	case StateData#state.outbuf of
	    "" ->
		NewStateData;
	    Data ->
		send_text(NewStateData, Data),
		NewStateData#state{outbuf = ""}
	end,
    {next_state, stream_established, NewStateData1};

handle_info({ircstring, [$E, $R, $R, $O, $R | _] = String},
	    StateName, StateData) ->
    process_error(StateData, String),
    {next_state, StateName, StateData};


handle_info({ircstring, String}, StateName, StateData) ->
    ?DEBUG("unknown irc command '~s'~n", [String]),
    {next_state, StateName, StateData};


handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    {next_state, StateName, StateData};
handle_info({tcp, _Socket, Data}, StateName, StateData) ->
    Buf = StateData#state.inbuf ++ binary_to_list(Data),
    {ok, Strings} = regexp:split([C || C <- Buf, C /= $\r], "\n"),
    ?DEBUG("strings=~p~n", [Strings]),
    NewBuf = process_lines(StateData#state.encoding, Strings),
    {next_state, StateName, StateData#state{inbuf = NewBuf}};
handle_info({tcp_closed, _Socket}, StateName, StateData) ->
    gen_fsm:send_event(self(), closed),
    {next_state, StateName, StateData};
handle_info({tcp_error, _Socket, _Reason}, StateName, StateData) ->
    gen_fsm:send_event(self(), closed),
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, StateData) ->
    mod_irc:closed_connection(StateData#state.host,
			      StateData#state.user,
			      StateData#state.server),
    bounce_messages(<<"Server Connect Failed">>),
    lists:foreach(
      fun(Chan) ->
	      ejabberd_router:route(
		exmpp_jid:make_jid(
		  lists:concat([Chan, "%", StateData#state.server]),
		  StateData#state.host, StateData#state.nick),
		StateData#state.user,
		#xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', attrs = [#xmlattr{name = 'type', value = <<"error">>}], children =
		 [#xmlel{ns = ?NS_JABBER_CLIENT, name = 'error', attrs = [#xmlattr{name = 'code', value = <<"502">>}], children =
		   [#xmlcdata{cdata =  <<"Server Connect Failed">>}]}]})
      end, dict:fetch_keys(StateData#state.channels)),
    case StateData#state.socket of
	undefined ->
	    ok;
	Socket ->
	    gen_tcp:close(Socket)
    end,
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

send_text(#state{socket = Socket, encoding = Encoding}, Text) ->
    CText = iconv:convert("utf-8", Encoding, lists:flatten(Text)),
    %?DEBUG("IRC OUTu: ~s~nIRC OUTk: ~s~n", [Text, CText]),
    gen_tcp:send(Socket, CText).


%send_queue(Socket, Q) ->
%    case queue:out(Q) of
%	{{value, El}, Q1} ->
%	    send_element(Socket, El),
%	    send_queue(Socket, Q1);
%	{empty, Q1} ->
%	    ok
%    end.

bounce_messages(Reason) ->
    receive
	{send_element, El} ->
	    case exmpp_stanza:get_type(El) of
	        <<"error">> ->
	            ok;
	        _ ->
                    Error = #xmlel{ns = ?NS_JABBER_CLIENT, name = 'error',
                      attrs = [#xmlattr{name = 'code', value = <<"502">>}],
                      children = [#xmlcdata{cdata = Reason}]},
	            Err = exmpp_stanza:reply_with_error(El, Error),
		    From = exmpp_jid:binary_to_jid(exmpp_stanza:get_sender(El)),
		    To = exmpp_jid:binary_to_jid(exmpp_stanza:get_recipient(El)),
		    ejabberd_router:route(To, From, Err)
	    end,
	    bounce_messages(Reason)
    after 0 ->
	    ok
    end.


route_chan(Pid, Channel, Resource, Packet) ->
    Pid ! {route_chan, Channel, Resource, Packet}.

route_nick(Pid, Nick, Packet) ->
    Pid ! {route_nick, Nick, Packet}.


process_lines(_Encoding, [S]) ->
    S;
process_lines(Encoding, [S | Ss]) ->
    self() ! {ircstring, iconv:convert(Encoding, "utf-8", S)},
    process_lines(Encoding, Ss).

process_channel_list(StateData, Items) ->
    process_channel_list_find_chan(StateData, Items).

process_channel_list_find_chan(StateData, []) ->
    StateData;
process_channel_list_find_chan(StateData, [[$# | Chan] | Items]) ->
    process_channel_list_users(StateData, Chan, Items);
process_channel_list_find_chan(StateData, [_ | Items]) ->
    process_channel_list_find_chan(StateData, Items).

process_channel_list_users(StateData, _Chan, []) ->
    StateData;
process_channel_list_users(StateData, Chan, [User | Items]) ->
    NewStateData = process_channel_list_user(StateData, Chan, User),
    process_channel_list_users(NewStateData, Chan, Items).

process_channel_list_user(StateData, Chan, User) ->
    User1 = case User of
		[$: | U1] -> U1;
		_ -> User
	    end,
    {User2, Affiliation, Role} =
	case User1 of
	    [$@ | U2] -> {U2, <<"admin">>, <<"moderator">>};
	    [$+ | U2] -> {U2, <<"member">>, <<"participant">>};
	    [$\% | U2] -> {U2, <<"admin">>, <<"moderator">>};
	    [$& | U2] -> {U2, <<"admin">>, <<"moderator">>};
	    [$~ | U2] -> {U2, <<"admin">>, <<"moderator">>};
	    _ -> {User1, <<"member">>, <<"participant">>}
	end,
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, User2),
      StateData#state.user,
      #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', children =
       [#xmlel{ns = ?NS_MUC_USER, name = 'x', children =
	 [#xmlel{ns = ?NS_MUC_USER, name = 'item', attrs =
	   [#xmlattr{name = 'affiliation', value = Affiliation},
	    #xmlattr{name = 'role', value = Role}]}]}]}),
    case catch dict:update(Chan,
			   fun(Ps) ->
				   ?SETS:add_element(User2, Ps)
			   end, StateData#state.channels) of
	{'EXIT', _} ->
	    StateData;
	NS ->
	    StateData#state{channels = NS}
    end.


process_channel_topic(StateData, Chan, String) ->
    {ok, Msg, _} = regexp:sub(String, ".*332[^:]*:", ""),
    Msg1 = filter_message(Msg),
    ejabberd_router:route(
      exmpp_jid:make_jid(
	lists:concat([Chan, "%", StateData#state.server]),
	StateData#state.host, ""),
      StateData#state.user,
      exmpp_message:groupchat(Msg1, "Topic for #" ++ Chan ++ ": " ++ Msg1)).

process_channel_topic_who(StateData, Chan, String) ->
    Words = string:tokens(String, " "),
    Msg1 = case Words of
	       [_, "333", _, _Chan, Whoset , Timeset] ->
		   case string:to_integer(Timeset) of
		       {Unixtimeset, _Rest} -> 
			   "Topic for #" ++ Chan ++ " set by " ++ Whoset ++
			       " at " ++ unixtime2string(Unixtimeset);
		       _->
			   "Topic for #" ++ Chan ++ " set by " ++ Whoset
		   end;
	       [_, "333", _, _Chan, Whoset | _] ->
                   "Topic for #" ++ Chan ++ " set by " ++ Whoset;
	       _ ->
		   String
	   end,
    Msg2 = filter_message(Msg1),

    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, ""),
      StateData#state.user,
      exmpp_message:groupchat(Msg2)).



process_endofwhois(StateData, _String, Nick) ->
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Nick, "!", StateData#state.server]),
		    StateData#state.host, ""),
      StateData#state.user,
      exmpp_message:chat("End of WHOIS")).

process_whois311(StateData, String, Nick, Ident, Irchost) ->
    {ok, Fullname, _} = regexp:sub(String, ".*311[^:]*:", ""),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Nick, "!", StateData#state.server]),
		    StateData#state.host, ""),
      StateData#state.user,
      exmpp_message:chat(lists:concat(
		       ["WHOIS: ", Nick, " is ",
			Ident, "@" , Irchost, " : " , Fullname]))).

process_whois312(StateData, String, Nick, Ircserver) ->
    {ok, Ircserverdesc, _} = regexp:sub(String, ".*312[^:]*:", ""),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Nick, "!", StateData#state.server]),
		    StateData#state.host, ""),
      StateData#state.user,
      exmpp_message:chat(lists:concat(["WHOIS: ", Nick, " use ",
				   Ircserver, " : ", Ircserverdesc]))).

process_whois319(StateData, String, Nick) ->
    {ok, Chanlist, _} = regexp:sub(String, ".*319[^:]*:", ""),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Nick, "!", StateData#state.server]),
		    StateData#state.host, ""),
      StateData#state.user,
      exmpp_message:chat(lists:concat(["WHOIS: ", Nick, " is on ",
				   Chanlist]))).



process_chanprivmsg(StateData, Chan, From, String) ->
    [FromUser | _] = string:tokens(From, "!"),
    {ok, Msg, _} = regexp:sub(String, ".*PRIVMSG[^:]*:", ""),
    Msg1 = case Msg of
	       [1, $A, $C, $T, $I, $O, $N, $  | Rest] ->
		   "/me " ++ Rest;
	       _ ->
		   Msg
	   end,
    Msg2 = filter_message(Msg1),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, FromUser),
      StateData#state.user,
      exmpp_message:groupchat(Msg2)).



process_channotice(StateData, Chan, From, String) ->
    [FromUser | _] = string:tokens(From, "!"),
    {ok, Msg, _} = regexp:sub(String, ".*NOTICE[^:]*:", ""),
    Msg1 = case Msg of
	       [1, $A, $C, $T, $I, $O, $N, $  | Rest] ->
		   "/me " ++ Rest;
	       _ ->
		   "/me NOTICE: " ++ Msg
	   end,
    Msg2 = filter_message(Msg1),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, FromUser),
      StateData#state.user,
      exmpp_message:groupchat(Msg2)).




process_privmsg(StateData, _Nick, From, String) ->
    [FromUser | _] = string:tokens(From, "!"),
    {ok, Msg, _} = regexp:sub(String, ".*PRIVMSG[^:]*:", ""),
    Msg1 = case Msg of
	       [1, $A, $C, $T, $I, $O, $N, $  | Rest] ->
		   "/me " ++ Rest;
	       _ ->
		   Msg
	   end,
    Msg2 = filter_message(Msg1),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([FromUser, "!", StateData#state.server]),
		    StateData#state.host, undefined),
      StateData#state.user,
      exmpp_message:chat(Msg2)).


process_notice(StateData, _Nick, From, String) ->
    [FromUser | _] = string:tokens(From, "!"),
    {ok, Msg, _} = regexp:sub(String, ".*NOTICE[^:]*:", ""),
    Msg1 = case Msg of
	       [1, $A, $C, $T, $I, $O, $N, $  | Rest] ->
		   "/me " ++ Rest;
	       _ ->
		   "/me NOTICE: " ++ Msg
	   end,
    Msg2 = filter_message(Msg1),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([FromUser, "!", StateData#state.server]),
		    StateData#state.host, undefined),
      StateData#state.user,
      exmpp_message:chat(Msg2)).


process_version(StateData, _Nick, From) ->
    [FromUser | _] = string:tokens(From, "!"),
    send_text(
      StateData,
      io_lib:format("NOTICE ~s :\001VERSION "
		    "ejabberd IRC transport ~s (c) Alexey Shchepin"
		    "\001\r\n",
		    [FromUser, ?VERSION]) ++
      io_lib:format("NOTICE ~s :\001VERSION "
		    ?EJABBERD_URI
		    "\001\r\n",
		    [FromUser])).


process_userinfo(StateData, _Nick, From) ->
    [FromUser | _] = string:tokens(From, "!"),
    send_text(
      StateData,
      io_lib:format("NOTICE ~s :\001USERINFO "
		    "xmpp:~s"
		    "\001\r\n",
		    [FromUser,
		     exmpp_jid:jid_to_list(StateData#state.user)])).


process_topic(StateData, Chan, From, String) ->
    [FromUser | _] = string:tokens(From, "!"),
    {ok, Msg, _} = regexp:sub(String, ".*TOPIC[^:]*:", ""),
    Msg1 = filter_message(Msg),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, FromUser),
      StateData#state.user,
      exmpp_message:groupchat(Msg1,
        "/me has changed the subject to: " ++ Msg1)).

process_part(StateData, Chan, From, String) ->
    [FromUser | FromIdent] = string:tokens(From, "!"),
    {ok, Msg, _} = regexp:sub(String, ".*PART[^:]*:", ""),    
    Msg1 = filter_message(Msg),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, FromUser),
      StateData#state.user,
      #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', attrs = [#xmlattr{name = 'type', value = <<"unavailable">>}], children =
       [#xmlel{ns = ?NS_MUC_USER, name = 'x', children =
	 [#xmlel{ns = ?NS_MUC_USER, name = 'item', attrs =
	   [#xmlattr{name = 'affiliation', value = <<"member">>},
	    #xmlattr{name = 'role', value = <<"none">>}]}]},
	#xmlel{ns = ?NS_MUC_USER, name = 'status', children =
	 [#xmlcdata{cdata = list_to_binary(Msg1 ++ " ("  ++ FromIdent ++ ")")}]}]
      }),
    case catch dict:update(Chan,
			   fun(Ps) ->
				   remove_element(FromUser, Ps)
			   end, StateData#state.channels) of
	{'EXIT', _} ->
	    StateData;
	NS ->
	    StateData#state{channels = NS}
    end.


process_quit(StateData, From, String) ->
    [FromUser | FromIdent] = string:tokens(From, "!"),
    
    {ok, Msg, _} = regexp:sub(String, ".*QUIT[^:]*:", ""),
    Msg1 = filter_message(Msg),
    %%NewChans =
	dict:map(
	  fun(Chan, Ps) ->
		  case ?SETS:is_member(FromUser, Ps) of
		      true ->
			  ejabberd_router:route(
			    exmpp_jid:make_jid(
			      lists:concat([Chan, "%", StateData#state.server]),
			      StateData#state.host, FromUser),
			    StateData#state.user,
			    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', attrs = [#xmlattr{name = 'type', value = <<"unavailable">>}], children =
			     [#xmlel{ns = ?NS_MUC_USER, name = 'x', children =
			       [#xmlel{ns = ?NS_MUC_USER, name = 'item', attrs =
				 [#xmlattr{name = 'affiliation', value = <<"member">>},
				  #xmlattr{name = 'role', value = <<"none">>}]}]},
			      #xmlel{ns = ?NS_MUC_USER, name = 'status', children =
			       [#xmlcdata{cdata = list_to_binary(Msg1 ++ " ("  ++ FromIdent ++ ")")}]}
			     ]}),
			  remove_element(FromUser, Ps);
		      _ ->
			  Ps
		  end
	  end, StateData#state.channels),
    StateData.


process_join(StateData, Channel, From, _String) ->
    [FromUser | FromIdent] = string:tokens(From, "!"),
    Chan = lists:subtract(Channel, ":#"),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, FromUser),
      StateData#state.user,
      #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', children =
       [#xmlel{ns = ?NS_MUC_USER, name = 'x', children =
	 [#xmlel{ns = ?NS_MUC_USER, name = 'item', attrs =
	   [#xmlattr{name = 'affiliation', value = <<"member">>},
	    #xmlattr{name = 'role', value = <<"participant">>}]}]},
	#xmlel{ns = ?NS_MUC_USER, name = 'status', children =
	 [#xmlcdata{cdata = list_to_binary(FromIdent)}]}]}),

    case catch dict:update(Chan,
			   fun(Ps) ->
				   ?SETS:add_element(FromUser, Ps)
			   end, StateData#state.channels) of
	{'EXIT', _} ->
	    StateData;
	NS ->
	    StateData#state{channels = NS}
    end.



process_mode_o(StateData, Chan, _From, Nick, Affiliation, Role) ->
    %Msg = lists:last(string:tokens(String, ":")),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, Nick),
      StateData#state.user,
      #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', children =
       [#xmlel{ns = ?NS_MUC_USER, name = 'x', children =
	 [#xmlel{ns = ?NS_MUC_USER, name = 'item', attrs =
	   [#xmlattr{name = 'affiliation', value = list_to_binary(Affiliation)},
	    #xmlattr{name = 'role', value = list_to_binary(Role)}]}]}]}).

process_kick(StateData, Chan, From, Nick, String) ->
    Msg = lists:last(string:tokens(String, ":")),
    Msg2 = Nick ++ " kicked by " ++ From ++ " (" ++ filter_message(Msg) ++ ")",
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, undefined),
      StateData#state.user,
      exmpp_message:groupchat(Msg2)),
    ejabberd_router:route(
      exmpp_jid:make_jid(lists:concat([Chan, "%", StateData#state.server]),
		    StateData#state.host, Nick),
      StateData#state.user,
      #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', attrs = [#xmlattr{name = 'type', value = <<"unavailable">>}], children =
       [#xmlel{ns = ?NS_MUC_USER, name = 'x', children =
	 [#xmlel{ns = ?NS_MUC_USER, name = 'item', attrs =
	   [#xmlattr{name = 'affiliation', value = <<"none">>},
	    #xmlattr{name = 'role', value = <<"none">>}]},
	  #xmlel{ns = ?NS_MUC_USER, name = 'status', attrs = [#xmlattr{name = 'code', value = <<"307">>}]}
	 ]}]}).

process_nick(StateData, From, NewNick) ->
    [FromUser | _] = string:tokens(From, "!"),
    Nick = lists:subtract(NewNick, ":"),
    NewChans =
	dict:map(
	  fun(Chan, Ps) ->
		  case ?SETS:is_member(FromUser, Ps) of
		      true ->
			  ejabberd_router:route(
			    exmpp_jid:make_jid(
			      lists:concat([Chan, "%", StateData#state.server]),
			      StateData#state.host, FromUser),
			    StateData#state.user,
			    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', attrs = [#xmlattr{name = 'type', value = <<"unavailable">>}], children =
			     [#xmlel{ns = ?NS_MUC_USER, name = 'x', children =
			       [#xmlel{ns = ?NS_MUC_USER, name = 'item', attrs =
				 [#xmlattr{name = 'affiliation', value = <<"member">>},
				  #xmlattr{name = 'role', value = <<"participant">>},
				  #xmlattr{name = 'nick', value = list_to_binary(Nick)}]},
				#xmlel{ns = ?NS_MUC_USER, name = 'status', attrs = [#xmlattr{name = 'code', value = <<"303">>}]}
			       ]}]}),
			  ejabberd_router:route(
			    exmpp_jid:make_jid(
			      lists:concat([Chan, "%", StateData#state.server]),
			      StateData#state.host, Nick),
			    StateData#state.user,
			    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', children =
			     [#xmlel{ns = ?NS_MUC_USER, name = 'x', children =
			       [#xmlel{ns = ?NS_MUC_USER, name = 'item', attrs =
				 [#xmlattr{name = 'affiliation', value = <<"member">>},
				  #xmlattr{name = 'role', value = <<"participant">>}]}
			       ]}]}),
			  ?SETS:add_element(Nick,
					    remove_element(FromUser, Ps));
		      _ ->
			  Ps
		  end
	  end, StateData#state.channels),
    StateData#state{channels = NewChans}.


process_error(StateData, String) ->
    lists:foreach(
      fun(Chan) ->
	      ejabberd_router:route(
		exmpp_jid:make_jid(
		  lists:concat([Chan, "%", StateData#state.server]),
		  StateData#state.host, StateData#state.nick),
		StateData#state.user,
		#xmlel{ns = ?NS_JABBER_CLIENT, name = 'presence', attrs = [#xmlattr{name = 'type', value = <<"error">>}], children =
		 [#xmlel{ns = ?NS_JABBER_CLIENT, name = 'error', attrs = [#xmlattr{name = 'code', value = <<"502">>}], children =
		   [#xmlcdata{cdata = list_to_binary(String)}]}]})
      end, dict:fetch_keys(StateData#state.channels)).




remove_element(E, Set) ->
    case ?SETS:is_element(E, Set) of
	true ->
	    ?SETS:del_element(E, Set);
	_ ->
	    Set
    end.



iq_admin(StateData, Channel, From, To,
	 #iq{type = Type, ns = XMLNS, payload = SubEl} = IQ_Rec) ->
    case catch process_iq_admin(StateData, Channel, Type, SubEl) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	Res ->
	    if
		Res /= ignore ->
		    ResIQ = case Res of
				{result, ResEls} ->
                                    Result = #xmlel{ns = XMLNS, name = 'query',
                                      children = ResEls},
                                    exmpp_iq:result(IQ_Rec, Result);
				{error, Error} ->
                                    exmpp_iq:error(IQ_Rec, Error)
			    end,
		    ejabberd_router:route(To, From,
					  exmpp_iq:iq_to_xmlel(ResIQ));
		true ->
		    ok
	    end
    end.


process_iq_admin(StateData, Channel, set, SubEl) ->
    case exmpp_xml:get_element(SubEl, 'item') of
	false ->
	    {error, 'bad-request'};
	ItemEl ->
	    Nick = exmpp_xml:get_attribute(ItemEl, 'nick', ""),
	    Affiliation = exmpp_xml:get_attribute(ItemEl, 'affiliation', ""),
	    Role = exmpp_xml:get_attribute(ItemEl, 'role', ""),
	    Reason = exmpp_xml:get_path(ItemEl, [{element, 'reason'}, cdata_as_list]),
	    process_admin(StateData, Channel, Nick, Affiliation, Role, Reason)
    end;
process_iq_admin(_StateData, _Channel, get, _SubEl) ->
    {error, 'feature-not-implemented'}.



process_admin(_StateData, _Channel, "", _Affiliation, _Role, _Reason) ->
    {error, 'feature-not-implemented'};

process_admin(StateData, Channel, Nick, _Affiliation, "none", Reason) ->
    case Reason of
	"" ->
	    send_text(StateData,
		      io_lib:format("KICK #~s ~s\r\n",
				    [Channel, Nick]));
	_ ->
	    send_text(StateData,
		      io_lib:format("KICK #~s ~s :~s\r\n",
				    [Channel, Nick, Reason]))
    end,
    {result, []};



process_admin(_StateData, _Channel, _Nick, _Affiliation, _Role, _Reason) ->
    {error, 'feature-not-implemented'}.



filter_message(Msg) ->
    lists:filter(
      fun(C) ->
	      if (C < 32) and
		 (C /= 9) and
		 (C /= 10) and
		 (C /= 13) ->
		      false;
		 true -> true
	      end
      end, filter_mirc_colors(Msg)).

filter_mirc_colors(Msg) ->
    case regexp:gsub(Msg, "(\\003[0-9]+)(,[0-9]+)?", "") of
	{ok, Msg2, _} ->
	    Msg2;
	_ ->
	    Msg
    end.

unixtime2string(Unixtime) ->
    Secs = Unixtime + calendar:datetime_to_gregorian_seconds(
			{{1970, 1, 1}, {0,0,0}}),
    case calendar:universal_time_to_local_time(
	   calendar:gregorian_seconds_to_datetime(Secs)) of 
	{{Year, Month, Day}, {Hour, Minute, Second}} ->
	    lists:flatten(
	      io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
			    [Year, Month, Day, Hour, Minute, Second]));
	_->
	    "0000-00-00 00:00:00"
    end.

toupper([C | Cs]) ->
    if
	C >= $a, C =< $z ->
	    [C - 32 | toupper(Cs)];
	true ->
	    [C | toupper(Cs)]
    end;
toupper([]) ->
    [].

