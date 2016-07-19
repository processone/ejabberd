%%%----------------------------------------------------------------------
%%% File    : mod_offline.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages.
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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

-module(mod_offline).

-author('alexey@process-one.net').

-protocol({xep, 13, '1.2'}).
-protocol({xep, 22, '1.4'}).
-protocol({xep, 23, '1.3'}).
-protocol({xep, 160, '1.0'}).
-protocol({xep, 334, '0.2'}).

-define(GEN_SERVER, p1_server).
-behaviour(?GEN_SERVER).

-behaviour(gen_mod).

-export([start/2,
	 start_link/2,
	 stop/1,
	 store_packet/3,
	 store_offline_msg/5,
	 resend_offline_messages/2,
	 pop_offline_messages/3,
	 get_sm_features/5,
	 get_sm_identity/5,
	 get_sm_items/5,
	 get_info/5,
	 handle_offline_query/1,
	 remove_expired_messages/1,
	 remove_old_messages/2,
	 remove_user/2,
	 import/1,
	 import/3,
	 export/1,
	 get_queue_length/2,
	 count_offline_messages/2,
	 get_offline_els/2,
	 find_x_expire/2,
	 webadmin_page/3,
	 webadmin_user/4,
	 webadmin_user_parse_query/5]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1, depends/2]).

-deprecated({get_queue_length,2}).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-include("mod_offline.hrl").

-define(PROCNAME, ejabberd_offline).

-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

%% default value for the maximum number of user messages
-define(MAX_USER_MESSAGES, infinity).

-type us() :: {binary(), binary()}.
-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #offline_msg{}) -> ok | pass.
-callback store_messages(binary(), us(), [#offline_msg{}],
			 non_neg_integer(), non_neg_integer()) ->
    {atomic, any()}.
-callback pop_messages(binary(), binary()) ->
    {atomic, [#offline_msg{}]} | {aborted, any()}.
-callback remove_expired_messages(binary()) -> {atomic, any()}.
-callback remove_old_messages(non_neg_integer(), binary()) -> {atomic, any()}.
-callback remove_user(binary(), binary()) -> {atomic, any()}.
-callback read_message_headers(binary(), binary()) -> any().
-callback read_message(binary(), binary(), non_neg_integer()) ->
    {ok, #offline_msg{}} | error.
-callback remove_message(binary(), binary(), non_neg_integer()) -> ok.
-callback read_all_messages(binary(), binary()) -> [#offline_msg{}].
-callback remove_all_messages(binary(), binary()) -> {atomic, any()}.
-callback count_messages(binary(), binary()) -> non_neg_integer().

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ?GEN_SERVER:start_link({local, Proc}, ?MODULE,
                           [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    catch ?GEN_SERVER:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

depends(_Host, _Opts) ->
    [].

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Opts]) ->
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
			     no_queue),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       store_packet, 50),
    ejabberd_hooks:add(resend_offline_messages_hook, Host,
		       ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(disco_sm_features, Host,
		       ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(disco_local_features, Host,
		       ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(disco_sm_identity, Host,
		       ?MODULE, get_sm_identity, 50),
    ejabberd_hooks:add(disco_sm_items, Host,
		       ?MODULE, get_sm_items, 50),
    ejabberd_hooks:add(disco_info, Host, ?MODULE, get_info, 50),
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),
    ejabberd_hooks:add(webadmin_user_parse_query, Host,
		       ?MODULE, webadmin_user_parse_query, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_FLEX_OFFLINE,
				  ?MODULE, handle_offline_query, IQDisc),
    AccessMaxOfflineMsgs =
	gen_mod:get_opt(access_max_user_messages, Opts,
			fun acl:shaper_rules_validator/1,
			max_user_offline_messages),
    {ok,
     #state{host = Host,
            access_max_offline_messages = AccessMaxOfflineMsgs}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


handle_cast(_Msg, State) -> {noreply, State}.


handle_info(#offline_msg{us = UserServer} = Msg, State) ->
    #state{host = Host,
           access_max_offline_messages = AccessMaxOfflineMsgs} = State,
    DBType = gen_mod:db_type(Host, ?MODULE),
    Msgs = receive_all(UserServer, [Msg], DBType),
    Len = length(Msgs),
    MaxOfflineMsgs = get_max_user_messages(AccessMaxOfflineMsgs,
                                           UserServer, Host),
    store_offline_msg(Host, UserServer, Msgs, Len, MaxOfflineMsgs),
    {noreply, State};

handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, store_packet, 50),
    ejabberd_hooks:delete(resend_offline_messages_hook,
			  Host, ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE, get_sm_identity, 50),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE, get_sm_items, 50),
    ejabberd_hooks:delete(disco_info, Host, ?MODULE, get_info, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host,
			  ?MODULE, webadmin_user, 50),
    ejabberd_hooks:delete(webadmin_user_parse_query, Host,
			  ?MODULE, webadmin_user_parse_query, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_FLEX_OFFLINE),
    ok.


code_change(_OldVsn, State, _Extra) -> {ok, State}.

store_offline_msg(Host, US, Msgs, Len, MaxOfflineMsgs) ->
    Mod = gen_mod:db_mod(Host, ?MODULE),
    case Mod:store_messages(Host, US, Msgs, Len, MaxOfflineMsgs) of
	{atomic, discard} ->
	    discard_warn_sender(Msgs);
	_ ->
	    ok
    end.

get_max_user_messages(AccessRule, {User, Server}, Host) ->
    case acl:match_rule(
	   Host, AccessRule, jid:make(User, Server, <<"">>)) of
	Max when is_integer(Max) -> Max;
	infinity -> infinity;
	_ -> ?MAX_USER_MESSAGES
    end.

receive_all(US, Msgs, DBType) ->
    receive
      #offline_msg{us = US} = Msg ->
	  receive_all(US, [Msg | Msgs], DBType)
      after 0 ->
		case DBType of
		  mnesia -> Msgs;
		  sql -> lists:reverse(Msgs);
		  riak -> Msgs
		end
    end.

get_sm_features(Acc, _From, _To, undefined, _Lang) ->
    Feats = case Acc of
		{result, I} -> I;
		_ -> []
	    end,
    {result, Feats ++ [?NS_FEATURE_MSGOFFLINE, ?NS_FLEX_OFFLINE]};

get_sm_features(_Acc, _From, _To, ?NS_FEATURE_MSGOFFLINE, _Lang) ->
    %% override all lesser features...
    {result, []};

get_sm_features(_Acc, #jid{luser = U, lserver = S}, #jid{luser = U, lserver = S},
		?NS_FLEX_OFFLINE, _Lang) ->
    {result, [?NS_FLEX_OFFLINE]};

get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_sm_identity(Acc, #jid{luser = U, lserver = S}, #jid{luser = U, lserver = S},
		?NS_FLEX_OFFLINE, _Lang) ->
    [#identity{category = <<"automation">>,
	       type = <<"message-list">>}|Acc];
get_sm_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_sm_items(_Acc, #jid{luser = U, lserver = S, lresource = R} = JID,
	     #jid{luser = U, lserver = S},
	     ?NS_FLEX_OFFLINE, _Lang) ->
    case ejabberd_sm:get_session_pid(U, S, R) of
	Pid when is_pid(Pid) ->
	    Mod = gen_mod:db_mod(S, ?MODULE),
	    Hdrs = Mod:read_message_headers(U, S),
	    BareJID = jid:remove_resource(JID),
	    Pid ! dont_ask_offline,
	    {result, lists:map(
		       fun({Seq, From, _To, _El}) ->
			       Node = integer_to_binary(Seq),
			       #disco_item{jid = BareJID,
					   node = Node,
					   name = jid:to_string(From)}
		       end, Hdrs)};
	none ->
	    {result, []}
    end;
get_sm_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec get_info([xdata()], jid(), jid(),
	       undefined | binary(), undefined | binary()) -> [xdata()].
get_info(_Acc, #jid{luser = U, lserver = S, lresource = R},
	 #jid{luser = U, lserver = S}, ?NS_FLEX_OFFLINE, _Lang) ->
    N = jlib:integer_to_binary(count_offline_messages(U, S)),
    case ejabberd_sm:get_session_pid(U, S, R) of
	Pid when is_pid(Pid) ->
	    Pid ! dont_ask_offline;
	none ->
	    ok
    end,
    [#xdata{type = result,
	    fields = [#xdata_field{var = <<"FORM_TYPE">>,
				   type = hidden,
				   values = [?NS_FLEX_OFFLINE]},
		      #xdata_field{var = <<"number_of_messages">>,
				   values = [N]}]}];
get_info(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec handle_offline_query(iq()) -> iq().
handle_offline_query(#iq{from = #jid{luser = U, lserver = S} = From,
			 to = #jid{luser = U, lserver = S} = _To,
			 type = Type,
			 sub_els = [#offline{purge = Purge,
					     items = Items,
					     fetch = Fetch}]} = IQ) ->
    case Type of
	get ->
	    if Fetch -> handle_offline_fetch(From);
	       true -> handle_offline_items_view(From, Items)
	    end;
	set ->
	    if Purge -> delete_all_msgs(U, S);
	       true -> handle_offline_items_remove(From, Items)
	    end
    end,
    xmpp:make_iq_result(IQ);
handle_offline_query(#iq{lang = Lang} = IQ) ->
    Txt = <<"Query to another users is forbidden">>,
    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang)).

-spec handle_offline_items_view(jid(), [offline_item()]) -> ok.
handle_offline_items_view(JID, Items) ->
    {U, S, R} = jid:tolower(JID),
    lists:foreach(
      fun(#offline_item{node = Node, action = view}) ->
	      case fetch_msg_by_node(JID, Node) of
		  {ok, OfflineMsg} ->
		      case offline_msg_to_route(S, OfflineMsg) of
			  {route, From, To, El} ->
			      NewEl = set_offline_tag(El, Node),
			      case ejabberd_sm:get_session_pid(U, S, R) of
				  Pid when is_pid(Pid) ->
				      Pid ! {route, From, To, NewEl};
				  none ->
				      ok
			      end;
			  error ->
			      ok
		      end;
		  error ->
		      ok
	      end;
	 (_) ->
	      ok
      end, Items).

-spec handle_offline_items_remove(jid(), [offline_item()]) -> ok.
handle_offline_items_remove(JID, Items) ->
    lists:foreach(
      fun(#offline_item{node = Node, action = remove}) ->
	      remove_msg_by_node(JID, Node);
	 (_) ->
	      ok
      end, Items).

-spec set_offline_tag(message(), binary()) -> message().
set_offline_tag(Msg, Node) ->
    xmpp:set_subtag(Msg, #offline{items = [#offline_item{node = Node}]}).

-spec handle_offline_fetch(jid()) -> ok.
handle_offline_fetch(#jid{luser = U, lserver = S, lresource = R}) ->
    case ejabberd_sm:get_session_pid(U, S, R) of
	none ->
	    ok;
	Pid when is_pid(Pid) ->
	    Pid ! dont_ask_offline,
	    lists:foreach(
	      fun({Node, From, To, El}) ->
		      NewEl = set_offline_tag(El, Node),
		      Pid ! {route, From, To, NewEl}
	      end, read_message_headers(U, S))
    end.

-spec fetch_msg_by_node(jid(), binary()) -> error | {ok, #offline_msg{}}.
fetch_msg_by_node(To, Seq) ->
    case catch binary_to_integer(Seq) of
	I when is_integer(I), I >= 0 ->
	    LUser = To#jid.luser,
	    LServer = To#jid.lserver,
	    Mod = gen_mod:db_mod(LServer, ?MODULE),
	    Mod:read_message(LUser, LServer, I);
	_ ->
	    error
    end.

-spec remove_msg_by_node(jid(), binary()) -> ok.
remove_msg_by_node(To, Seq) ->
    case catch binary_to_integer(Seq) of
	I when is_integer(I), I>= 0 ->
	    LUser = To#jid.luser,
	    LServer = To#jid.lserver,
	    Mod = gen_mod:db_mod(LServer, ?MODULE),
	    Mod:remove_message(LUser, LServer, I);
	_ ->
	    ok
    end.

-spec need_to_store(binary(), message()) -> boolean().
need_to_store(_LServer, #message{type = error}) -> false;
need_to_store(_LServer, #message{type = groupchat}) -> false;
need_to_store(_LServer, #message{type = headline}) -> false;
need_to_store(LServer, Packet) ->
    case xmpp:has_subtag(Packet, #offline{}) of
	false ->
	    case check_store_hint(Packet) of
		store ->
		    true;
		no_store ->
		    false;
		none ->
		    case gen_mod:get_module_opt(
			   LServer, ?MODULE, store_empty_body,
			   fun(V) when is_boolean(V) -> V;
			      (unless_chat_state) -> unless_chat_state
			   end,
			   unless_chat_state) of
			false ->
			    Packet#message.body /= [];
			unless_chat_state ->
			    not xmpp_util:is_standalone_chat_state(Packet);
			true ->
			    true
		    end
	    end;
	true ->
	    false
    end.

-spec store_packet(jid(), jid(), message()) -> ok | stop.
store_packet(From, To, Packet) ->
    case need_to_store(To#jid.lserver, Packet) of
	true ->
	    case check_event(From, To, Packet) of
		true ->
		    #jid{luser = LUser, lserver = LServer} = To,
		    TimeStamp = p1_time_compat:timestamp(),
		    Expire = find_x_expire(TimeStamp, Packet),
		    El = xmpp:encode(Packet),
		    gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME) !
		      #offline_msg{us = {LUser, LServer},
				   timestamp = TimeStamp, expire = Expire,
				   from = From, to = To, packet = El},
		    stop;
		_ -> ok
	    end;
	false -> ok
    end.

-spec check_store_hint(message()) -> store | no_store | none.
check_store_hint(Packet) ->
    case has_store_hint(Packet) of
	true ->
	    store;
	false ->
	    case has_no_store_hint(Packet) of
		true ->
		    no_store;
		false ->
		    none
	    end
    end.

-spec has_store_hint(message()) -> boolean().
has_store_hint(Packet) ->
    xmpp:has_subtag(Packet, #hint{type = 'store'}).

-spec has_no_store_hint(message()) -> boolean().
has_no_store_hint(Packet) ->
    xmpp:has_subtag(Packet, #hint{type = 'no-store'})
	orelse
	xmpp:has_subtag(Packet, #hint{type = 'no-storage'}).

%% Check if the packet has any content about XEP-0022
-spec check_event(jid(), jid(), message()) -> boolean().
check_event(From, To, #message{id = ID} = Msg) ->
    case xmpp:get_subtag(Msg, #xevent{}) of
	false ->
	    true;
	#xevent{id = undefined, offline = false} ->
	    true;
	#xevent{id = undefined, offline = true} ->
	    NewMsg = Msg#message{sub_els = [#xevent{id = ID, offline = true}]},
	    ejabberd_router:route(To, From, xmpp:set_from_to(NewMsg, To, From)),
	    true;
	_ ->
	    false
    end.

-spec find_x_expire(erlang:timestamp(), message()) -> erlang:timestamp() | never.
find_x_expire(TimeStamp, Msg) ->
    case xmpp:get_subtag(Msg, #expire{}) of
	#expire{seconds = Int} ->
	    {MegaSecs, Secs, MicroSecs} = TimeStamp,
	    S = MegaSecs * 1000000 + Secs + Int,
	    MegaSecs1 = S div 1000000,
	    Secs1 = S rem 1000000,
	    {MegaSecs1, Secs1, MicroSecs};
	false ->
	    never
    end.

resend_offline_messages(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:pop_messages(LUser, LServer) of
      {ok, Rs} ->
	  lists:foreach(fun (R) ->
				ejabberd_sm ! offline_msg_to_route(LServer, R)
			end,
			lists:keysort(#offline_msg.timestamp, Rs));
      _ -> ok
    end.

pop_offline_messages(Ls, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:pop_messages(LUser, LServer) of
	{ok, Rs} ->
	    TS = p1_time_compat:timestamp(),
	    Ls ++
		lists:map(fun (R) ->
				  offline_msg_to_route(LServer, R)
			  end,
			  lists:filter(
			    fun(#offline_msg{packet = Pkt} = R) ->
				    Expire = case R#offline_msg.expire of
						 undefined ->
						     find_x_expire(TS, Pkt);
						 Exp ->
						     Exp
					     end,
				    case Expire of
					never -> true;
					TimeStamp -> TS < TimeStamp
				    end
			    end, Rs));
	_ ->
	    Ls
    end.

remove_expired_messages(Server) ->
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_expired_messages(LServer).

remove_old_messages(Days, Server) ->
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_old_messages(Days, LServer).

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer).

%% Helper functions:

%% Warn senders that their messages have been discarded:
discard_warn_sender(Msgs) ->
    lists:foreach(
      fun(#offline_msg{from = From, to = To, packet = Packet}) ->
	      ErrText = <<"Your contact offline message queue is "
			  "full. The message has been discarded.">>,
	      Lang = xmpp:get_lang(Packet),
	      Err = xmpp:make_error(
		      Packet, xmpp:err_resource_constraint(ErrText, Lang)),
	      ejabberd_router:route(To, From, Err)
      end, Msgs).

webadmin_page(_, Host,
	      #request{us = _US, path = [<<"user">>, U, <<"queue">>],
		       q = Query, lang = Lang} =
		  _Request) ->
    Res = user_queue(U, Host, Query, Lang), {stop, Res};
webadmin_page(Acc, _, _) -> Acc.

get_offline_els(LUser, LServer) ->
    Hdrs = read_message_headers(LUser, LServer),
    lists:map(
      fun({_Seq, From, To, Packet}) ->
	      xmpp:set_from_to(Packet, From, To)
      end, Hdrs).

offline_msg_to_route(LServer, #offline_msg{} = R) ->
    Pkt = xmpp:decode(R#offline_msg.packet, [ignore_els]),
    Pkt1 = case R#offline_msg.timestamp of
	       undefined ->
		   Pkt;
	       TS ->
		   xmpp_util:add_delay_info(Pkt, LServer, TS,
					    <<"Offline Storage">>)
	   end,
    {route, R#offline_msg.from, R#offline_msg.to, Pkt1}.

read_message_headers(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    lists:map(
      fun({Seq, From, To, El}) ->
	      Node = integer_to_binary(Seq),
	      Packet = xmpp:decode(El, [ignore_els]),
	      {Node, From, To, Packet}
      end, Mod:read_message_headers(LUser, LServer)).

format_user_queue(Hdrs) ->
    lists:map(
      fun({Seq, From, To, El}) ->
	      ID = integer_to_binary(Seq),
	      FPacket = ejabberd_web_admin:pretty_print_xml(El),
	      SFrom = jid:to_string(From),
	      STo = jid:to_string(To),
	      Stamp = fxml:get_path_s(El, [{elem, <<"delay">>},
					   {attr, <<"stamp">>}]),
	      Time = case jlib:datetime_string_to_timestamp(Stamp) of
			 {_, _, _} = Now ->
			     {{Year, Month, Day}, {Hour, Minute, Second}} =
				 calendar:now_to_local_time(Now),
			     iolist_to_binary(
			       io_lib:format(
				 "~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
				 [Year, Month, Day, Hour, Minute,
				  Second]));
			 _ ->
			     <<"">>
		     end,
	      ?XE(<<"tr">>,
		  [?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
			[?INPUT(<<"checkbox">>, <<"selected">>, ID)]),
		   ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}], Time),
		   ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}], SFrom),
		   ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}], STo),
		   ?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
			[?XC(<<"pre">>, FPacket)])])
      end, Hdrs).

user_queue(User, Server, Query, Lang) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Res = user_queue_parse_query(LUser, LServer, Query),
    HdrsAll = Mod:read_message_headers(LUser, LServer),
    Hdrs = get_messages_subset(US, Server, HdrsAll),
    FMsgs = format_user_queue(Hdrs),
    [?XC(<<"h1">>,
	 list_to_binary(io_lib:format(?T(<<"~s's Offline Messages Queue">>),
                                      [us_to_list(US)])))]
      ++
      case Res of
	ok -> [?XREST(<<"Submitted">>)];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      [?XE(<<"table">>,
		   [?XE(<<"thead">>,
			[?XE(<<"tr">>,
			     [?X(<<"td">>), ?XCT(<<"td">>, <<"Time">>),
			      ?XCT(<<"td">>, <<"From">>),
			      ?XCT(<<"td">>, <<"To">>),
			      ?XCT(<<"td">>, <<"Packet">>)])]),
		    ?XE(<<"tbody">>,
			if FMsgs == [] ->
			       [?XE(<<"tr">>,
				    [?XAC(<<"td">>, [{<<"colspan">>, <<"4">>}],
					  <<" ">>)])];
			   true -> FMsgs
			end)]),
	       ?BR,
	       ?INPUTT(<<"submit">>, <<"delete">>,
		       <<"Delete Selected">>)])].

user_queue_parse_query(LUser, LServer, Query) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case lists:keysearch(<<"delete">>, 1, Query) of
	{value, _} ->
	    case lists:keyfind(<<"selected">>, 1, Query) of
		{_, Seq} ->
		    case catch binary_to_integer(Seq) of
			I when is_integer(I), I>=0 ->
			    Mod:remove_message(LUser, LServer, I),
			    ok;
			_ ->
			    nothing
		    end;
		false ->
		    nothing
	    end;
	_ ->
	    nothing
    end.

us_to_list({User, Server}) ->
    jid:to_string({User, Server, <<"">>}).

get_queue_length(LUser, LServer) ->
    count_offline_messages(LUser, LServer).

get_messages_subset(User, Host, MsgsAll) ->
    Access = gen_mod:get_module_opt(Host, ?MODULE, access_max_user_messages,
                                    fun(A) when is_atom(A) -> A end,
				    max_user_offline_messages),
    MaxOfflineMsgs = case get_max_user_messages(Access,
						User, Host)
			 of
		       Number when is_integer(Number) -> Number;
		       _ -> 100
		     end,
    Length = length(MsgsAll),
    get_messages_subset2(MaxOfflineMsgs, Length, MsgsAll).

get_messages_subset2(Max, Length, MsgsAll) when Length =< Max * 2 ->
    MsgsAll;
get_messages_subset2(Max, Length, MsgsAll) ->
    FirstN = Max,
    {MsgsFirstN, Msgs2} = lists:split(FirstN, MsgsAll),
    MsgsLastN = lists:nthtail(Length - FirstN - FirstN,
			      Msgs2),
    NoJID = jid:make(<<"...">>, <<"...">>, <<"">>),
    Seq = <<"0">>,
    IntermediateMsg = #xmlel{name = <<"...">>, attrs = [],
			     children = []},
    MsgsFirstN ++ [{Seq, NoJID, NoJID, IntermediateMsg}] ++ MsgsLastN.

webadmin_user(Acc, User, Server, Lang) ->
    QueueLen = count_offline_messages(jid:nodeprep(User),
				jid:nameprep(Server)),
    FQueueLen = [?AC(<<"queue/">>,
		     (iolist_to_binary(integer_to_list(QueueLen))))],
    Acc ++
      [?XCT(<<"h3">>, <<"Offline Messages:">>)] ++
	FQueueLen ++
	  [?C(<<" ">>),
	   ?INPUTT(<<"submit">>, <<"removealloffline">>,
		   <<"Remove All Offline Messages">>)].

-spec delete_all_msgs(binary(), binary()) -> {atomic, any()}.
delete_all_msgs(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_all_messages(LUser, LServer).

webadmin_user_parse_query(_, <<"removealloffline">>,
			  User, Server, _Query) ->
    case delete_all_msgs(User, Server) of
      {aborted, Reason} ->
	  ?ERROR_MSG("Failed to remove offline messages: ~p",
		     [Reason]),
	  {stop, error};
      {atomic, ok} ->
	  ?INFO_MSG("Removed all offline messages for ~s@~s",
		    [User, Server]),
	  {stop, ok}
    end;
webadmin_user_parse_query(Acc, _Action, _User, _Server,
			  _Query) ->
    Acc.

%% Returns as integer the number of offline messages for a given user
-spec count_offline_messages(binary(), binary()) -> non_neg_integer().
count_offline_messages(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:count_messages(LUser, LServer).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, Data) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Data).

mod_opt_type(access_max_user_messages) ->
    fun acl:shaper_rules_validator/1;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(store_empty_body) ->
    fun (V) when is_boolean(V) -> V;
        (unless_chat_state) -> unless_chat_state
    end;
mod_opt_type(_) ->
    [access_max_user_messages, db_type, store_empty_body].
