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
	 handle_offline_query/3,
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
	 mod_opt_type/1]).

-deprecated({get_queue_length,2}).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

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
			fun(A) when is_atom(A) -> A end,
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
		  odbc -> lists:reverse(Msgs);
		  riak -> Msgs
		end
    end.

get_sm_features(Acc, _From, _To, <<"">>, _Lang) ->
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

get_sm_identity(_Acc, #jid{luser = U, lserver = S}, #jid{luser = U, lserver = S},
		?NS_FLEX_OFFLINE, _Lang) ->
    Identity = #xmlel{name = <<"identity">>,
		      attrs = [{<<"category">>, <<"automation">>},
			       {<<"type">>, <<"message-list">>}]},
    [Identity];
get_sm_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_sm_items(_Acc, #jid{luser = U, lserver = S, lresource = R} = JID,
	     #jid{luser = U, lserver = S},
	     ?NS_FLEX_OFFLINE, _Lang) ->
    case ejabberd_sm:get_session_pid(U, S, R) of
	Pid when is_pid(Pid) ->
	    Hdrs = read_message_headers(U, S),
	    BareJID = jid:to_string(jid:remove_resource(JID)),
	    Pid ! dont_ask_offline,
	    {result, lists:map(
		       fun({Node, From, _To, _El}) ->
			       #xmlel{name = <<"item">>,
				      attrs = [{<<"jid">>, BareJID},
					       {<<"node">>, Node},
					       {<<"name">>, jid:to_string(From)}]}
		       end, Hdrs)};
	none ->
	    {result, []}
    end;
get_sm_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_info(_Acc, #jid{luser = U, lserver = S, lresource = R},
	 #jid{luser = U, lserver = S}, ?NS_FLEX_OFFLINE, _Lang) ->
    N = jlib:integer_to_binary(count_offline_messages(U, S)),
    case ejabberd_sm:get_session_pid(U, S, R) of
	Pid when is_pid(Pid) ->
	    Pid ! dont_ask_offline;
	none ->
	    ok
    end,
    [#xmlel{name = <<"x">>,
	    attrs = [{<<"xmlns">>, ?NS_XDATA},
		     {<<"type">>, <<"result">>}],
	    children = [#xmlel{name = <<"field">>,
			       attrs = [{<<"var">>, <<"FORM_TYPE">>},
					{<<"type">>, <<"hidden">>}],
			       children = [#xmlel{name = <<"value">>,
						  children = [{xmlcdata,
							       ?NS_FLEX_OFFLINE}]}]},
			#xmlel{name = <<"field">>,
			       attrs = [{<<"var">>, <<"number_of_messages">>}],
			       children = [#xmlel{name = <<"value">>,
						  children = [{xmlcdata, N}]}]}]}];
get_info(Acc, _From, _To, _Node, _Lang) ->
    Acc.

handle_offline_query(#jid{luser = U, lserver = S} = From,
		     #jid{luser = U, lserver = S} = _To,
		     #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	get ->
	    case fxml:get_subtag(SubEl, <<"fetch">>) of
		#xmlel{} ->
		    handle_offline_fetch(From);
		false ->
		    handle_offline_items_view(From, SubEl)
	    end;
	set ->
	    case fxml:get_subtag(SubEl, <<"purge">>) of
		#xmlel{} ->
		    delete_all_msgs(U, S);
		false ->
		    handle_offline_items_remove(From, SubEl)
	    end
    end,
    IQ#iq{type = result, sub_el = []};
handle_offline_query(_From, _To, #iq{sub_el = SubEl, lang = Lang} = IQ) ->
    Txt = <<"Query to another users is forbidden">>,
    IQ#iq{type = error, sub_el = [SubEl, ?ERRT_FORBIDDEN(Lang, Txt)]}.

handle_offline_items_view(JID, #xmlel{children = Items}) ->
    {U, S, R} = jid:tolower(JID),
    lists:foreach(
      fun(Node) ->
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
	      end
      end, get_nodes_from_items(Items, <<"view">>)).

handle_offline_items_remove(JID, #xmlel{children = Items}) ->
    lists:foreach(
      fun(Node) ->
	      remove_msg_by_node(JID, Node)
      end, get_nodes_from_items(Items, <<"remove">>)).

get_nodes_from_items(Items, Action) ->
    lists:flatmap(
      fun(#xmlel{name = <<"item">>, attrs = Attrs}) ->
	      case fxml:get_attr_s(<<"action">>, Attrs) of
		  Action ->
		      case fxml:get_attr_s(<<"node">>, Attrs) of
			  <<"">> ->
			      [];
			  TS ->
			      [TS]
		      end;
		  _ ->
		      []
	      end;
	 (_) ->
	      []
      end, Items).

set_offline_tag(#xmlel{children = Els} = El, Node) ->
    OfflineEl = #xmlel{name = <<"offline">>,
		       attrs = [{<<"xmlns">>, ?NS_FLEX_OFFLINE}],
		       children = [#xmlel{name = <<"item">>,
					  attrs = [{<<"node">>, Node}]}]},
    El#xmlel{children = [OfflineEl|Els]}.

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

need_to_store(LServer, Packet) ->
    Type = fxml:get_tag_attr_s(<<"type">>, Packet),
    if (Type /= <<"error">>) and (Type /= <<"groupchat">>)
       and (Type /= <<"headline">>) ->
	    case has_offline_tag(Packet) of
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
				    fxml:get_subtag(Packet, <<"body">>) /= false;
				unless_chat_state ->
				    not jlib:is_standalone_chat_state(Packet);
				true ->
				    true
			    end
		    end;
		true ->
		    false
	    end;
       true ->
	    false
    end.

store_packet(From, To, Packet) ->
    case need_to_store(To#jid.lserver, Packet) of
	true ->
	    case check_event(From, To, Packet) of
		true ->
		    #jid{luser = LUser, lserver = LServer} = To,
		    TimeStamp = p1_time_compat:timestamp(),
		    #xmlel{children = Els} = Packet,
		    Expire = find_x_expire(TimeStamp, Els),
		    gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME) !
		      #offline_msg{us = {LUser, LServer},
				   timestamp = TimeStamp, expire = Expire,
				   from = From, to = To, packet = Packet},
		    stop;
		_ -> ok
	    end;
	false -> ok
    end.

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

has_store_hint(Packet) ->
    fxml:get_subtag_with_xmlns(Packet, <<"store">>, ?NS_HINTS) =/= false.

has_no_store_hint(Packet) ->
    fxml:get_subtag_with_xmlns(Packet, <<"no-store">>, ?NS_HINTS) =/= false
      orelse
      fxml:get_subtag_with_xmlns(Packet, <<"no-storage">>, ?NS_HINTS) =/= false.

has_offline_tag(Packet) ->
    fxml:get_subtag_with_xmlns(Packet, <<"offline">>, ?NS_FLEX_OFFLINE) =/= false.

%% Check if the packet has any content about XEP-0022
check_event(From, To, Packet) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} =
	Packet,
    case find_x_event(Els) of
      false -> true;
      El ->
	  case fxml:get_subtag(El, <<"id">>) of
	    false ->
		case fxml:get_subtag(El, <<"offline">>) of
		  false -> true;
		  _ ->
		      ID = case fxml:get_tag_attr_s(<<"id">>, Packet) of
			     <<"">> ->
				 #xmlel{name = <<"id">>, attrs = [],
					children = []};
			     S ->
				 #xmlel{name = <<"id">>, attrs = [],
					children = [{xmlcdata, S}]}
			   end,
		      ejabberd_router:route(To, From,
					    #xmlel{name = Name, attrs = Attrs,
						   children =
						       [#xmlel{name = <<"x">>,
							       attrs =
								   [{<<"xmlns">>,
								     ?NS_EVENT}],
							       children =
								   [ID,
								    #xmlel{name
									       =
									       <<"offline">>,
									   attrs
									       =
									       [],
									   children
									       =
									       []}]}]}),
		      true
		end;
	    _ -> false
	  end
    end.

%% Check if the packet has subelements about XEP-0022
find_x_event([]) -> false;
find_x_event([{xmlcdata, _} | Els]) ->
    find_x_event(Els);
find_x_event([El | Els]) ->
    case fxml:get_tag_attr_s(<<"xmlns">>, El) of
      ?NS_EVENT -> El;
      _ -> find_x_event(Els)
    end.

find_x_expire(_, []) -> never;
find_x_expire(TimeStamp, [{xmlcdata, _} | Els]) ->
    find_x_expire(TimeStamp, Els);
find_x_expire(TimeStamp, [El | Els]) ->
    case fxml:get_tag_attr_s(<<"xmlns">>, El) of
      ?NS_EXPIRE ->
	  Val = fxml:get_tag_attr_s(<<"seconds">>, El),
	  case catch jlib:binary_to_integer(Val) of
	    {'EXIT', _} -> never;
	    Int when Int > 0 ->
		{MegaSecs, Secs, MicroSecs} = TimeStamp,
		S = MegaSecs * 1000000 + Secs + Int,
		MegaSecs1 = S div 1000000,
		Secs1 = S rem 1000000,
		{MegaSecs1, Secs1, MicroSecs};
	    _ -> never
	  end;
      _ -> find_x_expire(TimeStamp, Els)
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
				    #xmlel{children = Els} = Pkt,
				    Expire = case R#offline_msg.expire of
						 undefined ->
						     find_x_expire(TS, Els);
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
    lists:foreach(fun (#offline_msg{from = From, to = To,
				    packet = Packet}) ->
			  ErrText = <<"Your contact offline message queue is "
				      "full. The message has been discarded.">>,
			  Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
			  Err = jlib:make_error_reply(Packet,
						      ?ERRT_RESOURCE_CONSTRAINT(Lang,
										ErrText)),
			  ejabberd_router:route(To, From, Err)
		  end,
		  Msgs).

webadmin_page(_, Host,
	      #request{us = _US, path = [<<"user">>, U, <<"queue">>],
		       q = Query, lang = Lang} =
		  _Request) ->
    Res = user_queue(U, Host, Query, Lang), {stop, Res};
webadmin_page(Acc, _, _) -> Acc.

get_offline_els(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Hdrs = Mod:read_message_headers(LUser, LServer),
    lists:map(
      fun({_Seq, From, To, Packet}) ->
	      jlib:replace_from_to(From, To, Packet)
      end, Hdrs).

offline_msg_to_route(LServer, #offline_msg{} = R) ->
    El = case R#offline_msg.timestamp of
	     undefined ->
		 R#offline_msg.packet;
	     TS ->
		 jlib:add_delay_info(R#offline_msg.packet, LServer, TS,
				     <<"Offline Storage">>)
	 end,
    {route, R#offline_msg.from, R#offline_msg.to, El}.

read_message_headers(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    lists:map(
      fun({Seq, From, To, El}) ->
	      Node = integer_to_binary(Seq),
	      {Node, From, To, El}
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
    fun (A) -> A end;
mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(store_empty_body) ->
    fun (V) when is_boolean(V) -> V;
        (unless_chat_state) -> unless_chat_state
    end;
mod_opt_type(_) ->
    [access_max_user_messages, db_type, store_empty_body].
