%%%-------------------------------------------------------------------
%%% File    : mod_ack.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Description : Implements reliable message delivery
%%%               Note: this module depends on mod_caps
%%% Created : 12 Mar 2010 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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
%%%-------------------------------------------------------------------
-module(mod_ack).

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, start_link/2]).

-export([user_send_packet/4, offline_message/3,
	 delayed_message/3, remove_connection/3,
	 feature_inspect_packet/4]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

-include("jlib.hrl").

-include("ejabberd.hrl").

-define(PROCNAME, ejabberd_mod_ack).

-define(ACK_TIMEOUT, 60).

-define(DICT, dict).

-ifndef(NS_RECEIPTS).

-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).

-endif.

-ifndef(NS_PING).

-define(NS_PING, <<"urn:xmpp:ping">>).

-endif.

-ifndef(NS_P1_PUSHED).

-define(NS_P1_PUSHED, <<"p1:pushed">>).

-endif.

-record(state, {host = <<"">>          :: binary(),
                timers = (?DICT):new() :: dict(),
                timeout = ?ACK_TIMEOUT :: non_neg_integer()}).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

user_send_packet(_Debug, From, To,
		 #xmlel{name = <<"message">>} = Packet) ->
    case has_receipt_request(Packet) of
      {true, _} ->
	  process_ack_request(<<"on-sender-server">>, From, To,
			      Packet);
      false ->
	  case has_receipt_response(Packet) of
	    {true, ID} ->
		Server = From#jid.lserver,
		del_timer(Server, {message, ID}, From);
	    false -> do_nothing
	  end
    end;
user_send_packet(_Debug, From, _To,
		 #xmlel{name = <<"iq">>, attrs = Attrs}) ->
    case xml:get_attr_s(<<"id">>, Attrs) of
      <<"">> -> ok;
      ID ->
	  Server = From#jid.lserver,
	  del_timer(Server, {iq, ID}, From)
    end;
user_send_packet(_Debug, _From, _To, _Packet) -> do_nothing.

offline_message(From, To, Packet) ->
    process_ack_request(<<"offline">>, From, To, Packet),
    ok.

delayed_message(From, To, Packet) ->
    process_ack_request(<<"delayed">>, From, To, Packet),
    ok.

feature_inspect_packet(JID, Server,
		       #xmlel{name = <<"presence">>} = Pres,
		       #xmlel{name = <<"message">>, attrs = Attrs} = El) ->
    HasReceipts = has_receipt_request(El),
    ReceiptsSupported = are_receipts_supported(Server, Pres),
    ?DEBUG("feature_inspect_packet:~n** JID: ~p~n** "
	   "Has receipts: ~p~n** Receipts supported: "
	   "~p~n** Pres: ~p~n** El: ~p",
	   [JID, HasReceipts, ReceiptsSupported, Pres, El]),
    Type = xml:get_attr_s(<<"type">>, Attrs),
    case HasReceipts of
      _ when Type == <<"error">> -> ok;
      {true, ID} ->
	  case {jlib:string_to_jid(xml:get_attr_s(<<"from">>,
						  Attrs)),
		jlib:string_to_jid(xml:get_attr_s(<<"to">>, Attrs))}
	      of
	    {#jid{} = From, #jid{} = To} ->
		Pkt = {From, To, El},
		case ReceiptsSupported of
		  true -> add_timer(Server, {message, ID}, JID, Pkt);
		  false -> ping(From, To, Server, JID, El);
		  unknown ->
		      process_ack_request(<<"unreliable">>, From, To, El)
		end;
	    _ ->
		?WARNING_MSG("message doesn't have 'from' or 'to' "
			     "attribute:~n** El: ~p",
			     [El])
	  end;
      _ -> ok
    end;
feature_inspect_packet(_User, _Server, _Pres, _El) ->
    ok.

remove_connection({_, C2SPid}, #jid{lserver = Host},
		  _Info) ->
    gen_server:cast(gen_mod:get_module_proc(Host,
					    ?PROCNAME),
		    {del, C2SPid}).

init([Host, Opts]) ->
    Timeout = timer:seconds(gen_mod:get_opt(timeout, Opts,
                                            fun(I) when is_integer(I), I>0 ->
                                                    I
                                            end,
					    ?ACK_TIMEOUT)),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 20),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       offline_message, 20),
    ejabberd_hooks:add(delayed_message_hook, Host, ?MODULE,
		       delayed_message, 20),
    ejabberd_hooks:add(feature_inspect_packet, Host,
		       ?MODULE, feature_inspect_packet, 150),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
		       ?MODULE, remove_connection, 20),
    ejabberd_hooks:add(sm_remove_migrated_connection_hook,
		       Host, ?MODULE, remove_connection, 20),
    {ok, #state{host = Host, timeout = Timeout}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({add, ID, Pid, Packet}, State) ->
    TRef = erlang:start_timer(State#state.timeout, self(),
			      {ID, Pid}),
    Timers = insert(Pid, ID, {TRef, Packet},
		    State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({del, ID, Pid}, State) ->
    case lookup(Pid, ID, State#state.timers) of
      {ok, {TRef, {From, To, #xmlel{attrs = Attrs}}}} ->
	  cancel_timer(TRef),
	  Timers = delete(Pid, ID, State#state.timers),
	  case ID of
	    {iq, _} ->
		MsgID = xml:get_attr_s(<<"id">>, Attrs),
		Message = #xmlel{name = <<"message">>,
				 attrs = [{<<"id">>, MsgID}],
				 children =
				     [#xmlel{name = <<"received">>,
					     attrs =
						 [{<<"xmlns">>, ?NS_RECEIPTS},
						  {<<"id">>, MsgID}],
					     children = []}]},
		ejabberd_router:route(To, From, Message);
	    _ -> ok
	  end,
	  {noreply, State#state{timers = Timers}};
      error -> {noreply, State}
    end;
handle_cast({del, Pid}, State) ->
    lists:foreach(fun ({_, _, {TRef, {From, To, El}}}) ->
			  cancel_timer(TRef),
			  El1 = xml:remove_subtags(El, <<"x">>,
						   {<<"xmlns">>,
						    ?NS_P1_PUSHED}),
			  El2 = xml:append_subtags(El1,
						   [#xmlel{name = <<"x">>,
							   attrs =
							       [{<<"xmlns">>,
								 ?NS_P1_PUSHED}],
							   children = []}]),
			  ?DEBUG("Resending message:~n** From: ~p~n** "
				 "To: ~p~n** El: ~p",
				 [From, To, El2]),
			  ejabberd_router:route(From, To, El2)
		  end,
		  to_list(Pid, State#state.timers)),
    Timers = delete(Pid, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({timeout, _TRef, {ID, Pid}}, State) ->
    case lookup(Pid, ID, State#state.timers) of
      {ok, _} ->
	  catch ejabberd_c2s:stop_or_detach(Pid),
	  handle_cast({del, Pid}, State);
      error -> 
	?WARNING_MSG("ack ID not found :~p", [ID]),
	{noreply, State}
    end;
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet, 20),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, offline_message, 20),
    ejabberd_hooks:delete(delayed_message_hook, Host,
			  ?MODULE, delayed_message, 20),
    ejabberd_hooks:delete(feature_inspect_packet, Host,
			  ?MODULE, feature_inspect_packet, 150),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
			  ?MODULE, remove_connection, 20),
    ejabberd_hooks:delete(sm_remove_migrated_connection_hook,
			  Host, ?MODULE, remove_connection, 20),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

process_ack_request(AckTagName,
		    #jid{lserver = LServer} = From, To,
		    #xmlel{name = <<"message">>} = Packet) ->
    case has_receipt_request(Packet) of
      {true, ID} ->
	  BareTo = jlib:jid_remove_resource(To),
	  Message = #xmlel{name = <<"message">>,
			   attrs = [{<<"id">>, ID}],
			   children =
			       [#xmlel{name = AckTagName,
				       attrs =
					   [{<<"xmlns">>, ?NS_RECEIPTS},
					    {<<"server">>, LServer},
					    {<<"id">>, ID}],
				       children = []}]},
	  ejabberd_router:route(BareTo, From, Message);
      false -> do_nothing
    end.

has_receipt_request(Packet) ->
    has_receipt(Packet, <<"request">>).

has_receipt_response(Packet) ->
    has_receipt(Packet, <<"received">>).

has_receipt(#xmlel{name = <<"message">>,
		   attrs = MsgAttrs} =
		Packet,
	    Type) ->
    case xml:get_attr_s(<<"id">>, MsgAttrs) of
      <<"">> ->
	  case Type of
	    <<"request">> ->
		false; %% Message must have an ID to ask a request for ack.
	    <<"received">> ->
		case xml:get_subtag(Packet, <<"received">>) of
		  false -> false;
		  #xmlel{attrs = Attrs} ->
		      case xml:get_attr_s(<<"xmlns">>, Attrs) of
			?NS_RECEIPTS ->
			    case xml:get_attr_s(<<"id">>, Attrs) of
			      <<"">> -> false;
			      SubTagID -> {true, SubTagID}
			    end;
			_ -> false
		      end
		end
	  end;
      ID ->
	  case xml:get_subtag(Packet, Type) of
	    false -> false;
	    #xmlel{attrs = Attrs} ->
		case xml:get_attr_s(<<"xmlns">>, Attrs) of
		  ?NS_RECEIPTS ->
		      case xml:get_attr_s(<<"id">>, Attrs) of
			<<"">> -> {true, ID};
			SubTagID -> {true, SubTagID}
		      end;
		  _ -> false
		end
	  end
    end.

are_receipts_supported(Server,
                       #xmlel{name = <<"presence">>,
			      children = Els}) ->
    case mod_caps:read_caps(Els) of
      nothing -> unknown;
      Caps ->
	  lists:member(?NS_RECEIPTS, mod_caps:get_features(Server, Caps))
    end.

ping(From, To, Server, JID, El) ->
    ID = randoms:get_string(),
    add_timer(Server, {iq, ID}, JID, {From, To, El}),
    ejabberd_router:route(jlib:make_jid(<<"">>, Server,
					<<"">>),
			  JID,
			  #xmlel{name = <<"iq">>,
				 attrs =
				     [{<<"type">>, <<"get">>}, {<<"id">>, ID}],
				 children =
				     [#xmlel{name = <<"query">>,
					     attrs = [{<<"xmlns">>, ?NS_PING}],
					     children = []}]}).

add_timer(Host, ID, JID, Packet) ->
    {U, S, R} = jlib:jid_tolower(JID),
    C2SPid = ejabberd_sm:get_session_pid(U, S, R),
    gen_server:cast(gen_mod:get_module_proc(Host,
					    ?PROCNAME),
		    {add, ID, C2SPid, Packet}).

del_timer(Host, ID, JID) ->
    {U, S, R} = jlib:jid_tolower(JID),
    C2SPid = ejabberd_sm:get_session_pid(U, S, R),
    gen_server:cast(gen_mod:get_module_proc(Host,
					    ?PROCNAME),
		    {del, ID, C2SPid}).

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end.

lookup(Pid, Key, Queue) ->
    case (?DICT):find(Pid, Queue) of
      {ok, Treap} ->
	  case treap:lookup(Key, Treap) of
	    {ok, _, Val} -> {ok, Val};
	    error -> error
	  end;
      error -> error
    end.

insert(Pid, Key, Val, Queue) ->
    Treap = case (?DICT):find(Pid, Queue) of
	      {ok, Treap1} -> Treap1;
	      error -> nil
	    end,
    (?DICT):store(Pid, treap:insert(Key, now(), Val, Treap),
		  Queue).

delete(Pid, Key, Queue) ->
    case (?DICT):find(Pid, Queue) of
      {ok, Treap} ->
	  NewTreap = treap:delete(Key, Treap),
	  case treap:is_empty(NewTreap) of
	    true -> (?DICT):erase(Pid, Queue);
	    false -> (?DICT):store(Pid, NewTreap, Queue)
	  end;
      error -> Queue
    end.

delete(Pid, Queue) -> (?DICT):erase(Pid, Queue).

to_list(Pid, Queue) ->
    case (?DICT):find(Pid, Queue) of
      {ok, Treap} -> treap:to_list(Treap);
      error -> []
    end.
