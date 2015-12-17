%%%----------------------------------------------------------------------
%%% File    : mod_offline.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages.
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_offline).

-author('alexey@process-one.net').

-protocol({xep, 22, '1.4'}).
-protocol({xep, 23, '1.3'}).
-protocol({xep, 160, '1.0'}).
-protocol({xep, 334, '0.2'}).

-define(GEN_SERVER, p1_server).
-behaviour(?GEN_SERVER).

-behaviour(gen_mod).

-export([count_offline_messages/2]).

-export([start/2,
	 start_link/2,
	 stop/1,
	 store_packet/3,
	 resend_offline_messages/2,
	 pop_offline_messages/3,
	 get_sm_features/5,
	 remove_expired_messages/1,
	 remove_old_messages/2,
	 remove_user/2,
	 import/1,
	 import/3,
	 export/1,
	 get_queue_length/2,
	 get_offline_els/2,
	 webadmin_page/3,
	 webadmin_user/4,
	 webadmin_user_parse_query/5]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1]).

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
    case gen_mod:db_type(Host, Opts) of
      mnesia ->
	  mnesia:create_table(offline_msg,
			      [{disc_only_copies, [node()]}, {type, bag},
			       {attributes, record_info(fields, offline_msg)}]),
	  update_table();
      _ -> ok
    end,
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
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),
    ejabberd_hooks:add(webadmin_user_parse_query, Host,
		       ?MODULE, webadmin_user_parse_query, 50),
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
    store_offline_msg(Host, UserServer, Msgs, Len, MaxOfflineMsgs, DBType),
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
    ejabberd_hooks:delete(webadmin_page_host, Host,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host,
			  ?MODULE, webadmin_user, 50),
    ejabberd_hooks:delete(webadmin_user_parse_query, Host,
			  ?MODULE, webadmin_user_parse_query, 50),
    ok.


code_change(_OldVsn, State, _Extra) -> {ok, State}.


store_offline_msg(_Host, US, Msgs, Len, MaxOfflineMsgs,
		  mnesia) ->
    F = fun () ->
		Count = if MaxOfflineMsgs =/= infinity ->
			       Len + count_mnesia_records(US);
			   true -> 0
			end,
		if Count > MaxOfflineMsgs -> discard_warn_sender(Msgs);
		   true ->
		       if Len >= (?OFFLINE_TABLE_LOCK_THRESHOLD) ->
			      mnesia:write_lock_table(offline_msg);
			  true -> ok
		       end,
		       lists:foreach(fun (M) -> mnesia:write(M) end, Msgs)
		end
	end,
    mnesia:transaction(F);
store_offline_msg(Host, {User, _Server}, Msgs, Len, MaxOfflineMsgs, odbc) ->
    Count = if MaxOfflineMsgs =/= infinity ->
		   Len + count_offline_messages(User, Host);
	       true -> 0
	    end,
    if Count > MaxOfflineMsgs -> discard_warn_sender(Msgs);
       true ->
	   Query = lists:map(fun (M) ->
				     Username =
					 ejabberd_odbc:escape((M#offline_msg.to)#jid.luser),
				     From = M#offline_msg.from,
				     To = M#offline_msg.to,
				     Packet =
					 jlib:replace_from_to(From, To,
							      M#offline_msg.packet),
				     NewPacket =
					 jlib:add_delay_info(Packet, Host,
							     M#offline_msg.timestamp,
							     <<"Offline Storage">>),
				     XML =
					 ejabberd_odbc:escape(xml:element_to_binary(NewPacket)),
				     odbc_queries:add_spool_sql(Username, XML)
			     end,
			     Msgs),
	   odbc_queries:add_spool(Host, Query)
    end;
store_offline_msg(Host, {User, _}, Msgs, Len, MaxOfflineMsgs,
		  riak) ->
    Count = if MaxOfflineMsgs =/= infinity ->
                    Len + count_offline_messages(User, Host);
               true -> 0
            end,
    if
        Count > MaxOfflineMsgs ->
            discard_warn_sender(Msgs);
        true ->
            lists:foreach(
              fun(#offline_msg{us = US,
                               timestamp = TS} = M) ->
                      ejabberd_riak:put(M, offline_msg_schema(),
					[{i, TS}, {'2i', [{<<"us">>, US}]}])
              end, Msgs)
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
    {result, Feats ++ [?NS_FEATURE_MSGOFFLINE]};

get_sm_features(_Acc, _From, _To, ?NS_FEATURE_MSGOFFLINE, _Lang) ->
    %% override all lesser features...
    {result, []};

get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

need_to_store(LServer, Packet) ->
    Type = xml:get_tag_attr_s(<<"type">>, Packet),
    if (Type /= <<"error">>) and (Type /= <<"groupchat">>)
       and (Type /= <<"headline">>) ->
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
			    xml:get_subtag(Packet, <<"body">>) /= false;
			unless_chat_state ->
			    not jlib:is_standalone_chat_state(Packet);
			true ->
			    true
		    end
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
    xml:get_subtag_with_xmlns(Packet, <<"store">>, ?NS_HINTS) =/= false.

has_no_store_hint(Packet) ->
    xml:get_subtag_with_xmlns(Packet, <<"no-store">>, ?NS_HINTS) =/= false
      orelse
      xml:get_subtag_with_xmlns(Packet, <<"no-storage">>, ?NS_HINTS) =/= false.

%% Check if the packet has any content about XEP-0022
check_event(From, To, Packet) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} =
	Packet,
    case find_x_event(Els) of
      false -> true;
      El ->
	  case xml:get_subtag(El, <<"id">>) of
	    false ->
		case xml:get_subtag(El, <<"offline">>) of
		  false -> true;
		  _ ->
		      ID = case xml:get_tag_attr_s(<<"id">>, Packet) of
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
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
      ?NS_EVENT -> El;
      _ -> find_x_event(Els)
    end.

find_x_expire(_, []) -> never;
find_x_expire(TimeStamp, [{xmlcdata, _} | Els]) ->
    find_x_expire(TimeStamp, Els);
find_x_expire(TimeStamp, [El | Els]) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
      ?NS_EXPIRE ->
	  Val = xml:get_tag_attr_s(<<"seconds">>, El),
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
    US = {LUser, LServer},
    F = fun () ->
		Rs = mnesia:wread({offline_msg, US}),
		mnesia:delete({offline_msg, US}),
		Rs
	end,
    case mnesia:transaction(F) of
      {atomic, Rs} ->
	  lists:foreach(fun (R) ->
				ejabberd_sm !
				  {route, R#offline_msg.from, R#offline_msg.to,
				   jlib:add_delay_info(R#offline_msg.packet,
						       LServer,
						       R#offline_msg.timestamp,
						       <<"Offline Storage">>)}
			end,
			lists:keysort(#offline_msg.timestamp, Rs));
      _ -> ok
    end.

pop_offline_messages(Ls, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    pop_offline_messages(Ls, LUser, LServer,
			 gen_mod:db_type(LServer, ?MODULE)).

pop_offline_messages(Ls, LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () ->
		Rs = mnesia:wread({offline_msg, US}),
		mnesia:delete({offline_msg, US}),
		Rs
	end,
    case mnesia:transaction(F) of
      {atomic, Rs} ->
	  TS = p1_time_compat:timestamp(),
	  Ls ++
	    lists:map(fun (R) ->
			      offline_msg_to_route(LServer, R)
		      end,
		      lists:filter(fun (R) ->
					   case R#offline_msg.expire of
					     never -> true;
					     TimeStamp -> TS < TimeStamp
					   end
				   end,
				   lists:keysort(#offline_msg.timestamp, Rs)));
      _ -> Ls
    end;
pop_offline_messages(Ls, LUser, LServer, odbc) ->
    EUser = ejabberd_odbc:escape(LUser),
    case odbc_queries:get_and_del_spool_msg_t(LServer,
					      EUser)
	of
      {atomic, {selected, [<<"username">>, <<"xml">>], Rs}} ->
	  Ls ++
	    lists:flatmap(fun ([_, XML]) ->
				  case xml_stream:parse_element(XML) of
				    {error, _Reason} ->
                                          [];
				    El ->
                                          case offline_msg_to_route(LServer, El) of
                                              error ->
                                                  [];
                                              RouteMsg ->
                                                  [RouteMsg]
                                          end
				  end
			  end,
			  Rs);
      _ -> Ls
    end;
pop_offline_messages(Ls, LUser, LServer, riak) ->
    case ejabberd_riak:get_by_index(offline_msg, offline_msg_schema(),
                                    <<"us">>, {LUser, LServer}) of
        {ok, Rs} ->
            try
                lists:foreach(
                  fun(#offline_msg{timestamp = T}) ->
                          ok = ejabberd_riak:delete(offline_msg, T)
                  end, Rs),
                TS = p1_time_compat:timestamp(),
                Ls ++ lists:map(
                        fun (R) ->
                                offline_msg_to_route(LServer, R)
                        end,
                        lists:filter(
                          fun(R) ->
                                  case R#offline_msg.expire of
                                      never -> true;
                                      TimeStamp -> TS < TimeStamp
                                  end
                          end,
                          lists:keysort(#offline_msg.timestamp, Rs)))
            catch _:{badmatch, _} ->
                    Ls
            end;
	_ ->
	    Ls
    end.

remove_expired_messages(Server) ->
    LServer = jid:nameprep(Server),
    remove_expired_messages(LServer,
			    gen_mod:db_type(LServer, ?MODULE)).

remove_expired_messages(_LServer, mnesia) ->
    TimeStamp = p1_time_compat:timestamp(),
    F = fun () ->
		mnesia:write_lock_table(offline_msg),
		mnesia:foldl(fun (Rec, _Acc) ->
				     case Rec#offline_msg.expire of
				       never -> ok;
				       TS ->
					   if TS < TimeStamp ->
						  mnesia:delete_object(Rec);
					      true -> ok
					   end
				     end
			     end,
			     ok, offline_msg)
	end,
    mnesia:transaction(F);
remove_expired_messages(_LServer, odbc) -> {atomic, ok};
remove_expired_messages(_LServer, riak) -> {atomic, ok}.

remove_old_messages(Days, Server) ->
    LServer = jid:nameprep(Server),
    remove_old_messages(Days, LServer,
			gen_mod:db_type(LServer, ?MODULE)).

remove_old_messages(Days, _LServer, mnesia) ->
    S = p1_time_compat:system_time(seconds) - 60 * 60 * 24 * Days,
    MegaSecs1 = S div 1000000,
    Secs1 = S rem 1000000,
    TimeStamp = {MegaSecs1, Secs1, 0},
    F = fun () ->
		mnesia:write_lock_table(offline_msg),
		mnesia:foldl(fun (#offline_msg{timestamp = TS} = Rec,
				  _Acc)
				     when TS < TimeStamp ->
				     mnesia:delete_object(Rec);
				 (_Rec, _Acc) -> ok
			     end,
			     ok, offline_msg)
	end,
    mnesia:transaction(F);

remove_old_messages(Days, LServer, odbc) ->
    case catch ejabberd_odbc:sql_query(
		 LServer,
		 [<<"DELETE FROM spool"
		   " WHERE created_at < "
		   "DATE_SUB(CURDATE(), INTERVAL ">>,
		  integer_to_list(Days), <<" DAY);">>]) of
	{updated, N} ->
	    ?INFO_MSG("~p message(s) deleted from offline spool", [N]);
	_Error ->
	    ?ERROR_MSG("Cannot delete message in offline spool: ~p", [_Error])
    end,
    {atomic, ok};
remove_old_messages(_Days, _LServer, riak) ->
    {atomic, ok}.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    remove_user(LUser, LServer,
		gen_mod:db_type(LServer, ?MODULE)).

remove_user(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () -> mnesia:delete({offline_msg, US}) end,
    mnesia:transaction(F);
remove_user(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_spool_msg(LServer, Username);
remove_user(LUser, LServer, riak) ->
    {atomic, ejabberd_riak:delete_by_index(offline_msg,
                                           <<"us">>, {LUser, LServer})}.

jid_to_binary(#jid{user = U, server = S, resource = R,
                   luser = LU, lserver = LS, lresource = LR}) ->
    #jid{user = iolist_to_binary(U),
         server = iolist_to_binary(S),
         resource = iolist_to_binary(R),
         luser = iolist_to_binary(LU),
         lserver = iolist_to_binary(LS),
         lresource = iolist_to_binary(LR)}.

update_table() ->
    Fields = record_info(fields, offline_msg),
    case mnesia:table_info(offline_msg, attributes) of
        Fields ->
            ejabberd_config:convert_table_to_binary(
              offline_msg, Fields, bag,
              fun(#offline_msg{us = {U, _}}) -> U end,
              fun(#offline_msg{us = {U, S},
                               from = From,
                               to = To,
                               packet = El} = R) ->
                      R#offline_msg{us = {iolist_to_binary(U),
                                          iolist_to_binary(S)},
                                    from = jid_to_binary(From),
                                    to = jid_to_binary(To),
                                    packet = xml:to_xmlel(El)}
              end);
        _ ->
            ?INFO_MSG("Recreating offline_msg table", []),
            mnesia:transform_table(offline_msg, ignore, Fields)
    end.

%% Helper functions:

%% Warn senders that their messages have been discarded:
discard_warn_sender(Msgs) ->
    lists:foreach(fun (#offline_msg{from = From, to = To,
				    packet = Packet}) ->
			  ErrText = <<"Your contact offline message queue is "
				      "full. The message has been discarded.">>,
			  Lang = xml:get_tag_attr_s(<<"xml:lang">>, Packet),
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
    get_offline_els(LUser, LServer, gen_mod:db_type(LServer, ?MODULE)).

get_offline_els(LUser, LServer, DBType)
  when DBType == mnesia; DBType == riak ->
    Msgs = read_all_msgs(LUser, LServer, DBType),
    lists:map(
      fun(Msg) ->
              {route, From, To, Packet} = offline_msg_to_route(LServer, Msg),
              jlib:replace_from_to(From, To, Packet)
      end, Msgs);
get_offline_els(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select xml from spool  where username='">>,
					Username, <<"'  order by seq;">>]) of
        {selected, [<<"xml">>], Rs} ->
            lists:flatmap(
              fun([XML]) ->
                      case xml_stream:parse_element(XML) of
                          #xmlel{} = El ->
                              case offline_msg_to_route(LServer, El) of
                                  {route, _, _, NewEl} ->
                                      [NewEl];
                                  error ->
                                      []
                              end;
                          _ ->
                              []
                      end
              end, Rs);
        _ ->
            []
    end.

offline_msg_to_route(LServer, #offline_msg{} = R) ->
    {route, R#offline_msg.from, R#offline_msg.to,
     jlib:add_delay_info(R#offline_msg.packet, LServer, R#offline_msg.timestamp,
			 <<"Offline Storage">>)};
offline_msg_to_route(_LServer, #xmlel{} = El) ->
    To = jid:from_string(xml:get_tag_attr_s(<<"to">>, El)),
    From = jid:from_string(xml:get_tag_attr_s(<<"from">>, El)),
    if (To /= error) and (From /= error) ->
            {route, From, To, El};
       true ->
            error
    end.

read_all_msgs(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    lists:keysort(#offline_msg.timestamp,
		  mnesia:dirty_read({offline_msg, US}));
read_all_msgs(LUser, LServer, riak) ->
    case ejabberd_riak:get_by_index(
           offline_msg, offline_msg_schema(),
	   <<"us">>, {LUser, LServer}) of
        {ok, Rs} ->
            lists:keysort(#offline_msg.timestamp, Rs);
        _Err ->
            []
    end;
read_all_msgs(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select xml from spool  where username='">>,
					Username, <<"'  order by seq;">>])
	of
      {selected, [<<"xml">>], Rs} ->
	  lists:flatmap(fun ([XML]) ->
				case xml_stream:parse_element(XML) of
				  {error, _Reason} -> [];
				  El -> [El]
				end
			end,
			Rs);
      _ -> []
    end.

format_user_queue(Msgs, DBType) when DBType == mnesia; DBType == riak ->
    lists:map(fun (#offline_msg{timestamp = TimeStamp,
				from = From, to = To,
				packet =
				    #xmlel{name = Name, attrs = Attrs,
					   children = Els}} =
		       Msg) ->
		      ID = jlib:encode_base64((term_to_binary(Msg))),
		      {{Year, Month, Day}, {Hour, Minute, Second}} =
			  calendar:now_to_local_time(TimeStamp),
		      Time =
			  iolist_to_binary(io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
							 [Year, Month, Day,
							  Hour, Minute,
							  Second])),
		      SFrom = jid:to_string(From),
		      STo = jid:to_string(To),
		      Attrs2 = jlib:replace_from_to_attrs(SFrom, STo, Attrs),
		      Packet = #xmlel{name = Name, attrs = Attrs2,
				      children = Els},
		      FPacket = ejabberd_web_admin:pretty_print_xml(Packet),
		      ?XE(<<"tr">>,
			  [?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
				[?INPUT(<<"checkbox">>, <<"selected">>, ID)]),
			   ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}], Time),
			   ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}], SFrom),
			   ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}], STo),
			   ?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
				[?XC(<<"pre">>, FPacket)])])
	      end,
	      Msgs);
format_user_queue(Msgs, odbc) ->
    lists:map(fun (#xmlel{} = Msg) ->
		      ID = jlib:encode_base64((term_to_binary(Msg))),
		      Packet = Msg,
		      FPacket = ejabberd_web_admin:pretty_print_xml(Packet),
		      ?XE(<<"tr">>,
			  [?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
				[?INPUT(<<"checkbox">>, <<"selected">>, ID)]),
			   ?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
				[?XC(<<"pre">>, FPacket)])])
	      end,
	      Msgs).

user_queue(User, Server, Query, Lang) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    DBType = gen_mod:db_type(LServer, ?MODULE),
    Res = user_queue_parse_query(LUser, LServer, Query,
				 DBType),
    MsgsAll = read_all_msgs(LUser, LServer, DBType),
    Msgs = get_messages_subset(US, Server, MsgsAll,
			       DBType),
    FMsgs = format_user_queue(Msgs, DBType),
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

user_queue_parse_query(LUser, LServer, Query, mnesia) ->
    US = {LUser, LServer},
    case lists:keysearch(<<"delete">>, 1, Query) of
      {value, _} ->
	  Msgs = lists:keysort(#offline_msg.timestamp,
			       mnesia:dirty_read({offline_msg, US})),
	  F = fun () ->
		      lists:foreach(fun (Msg) ->
					    ID =
						jlib:encode_base64((term_to_binary(Msg))),
					    case lists:member({<<"selected">>,
							       ID},
							      Query)
						of
					      true -> mnesia:delete_object(Msg);
					      false -> ok
					    end
				    end,
				    Msgs)
	      end,
	  mnesia:transaction(F),
	  ok;
      false -> nothing
    end;
user_queue_parse_query(LUser, LServer, Query, riak) ->
    case lists:keysearch(<<"delete">>, 1, Query) of
        {value, _} ->
            Msgs = read_all_msgs(LUser, LServer, riak),
            lists:foreach(
              fun (Msg) ->
                      ID = jlib:encode_base64((term_to_binary(Msg))),
                      case lists:member({<<"selected">>, ID}, Query) of
                          true ->
                              ejabberd_riak:delete(offline_msg,
                                                   Msg#offline_msg.timestamp);
                          false ->
                              ok
                      end
              end,
              Msgs),
            ok;
        false ->
            nothing
    end;
user_queue_parse_query(LUser, LServer, Query, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case lists:keysearch(<<"delete">>, 1, Query) of
      {value, _} ->
	  Msgs = case catch ejabberd_odbc:sql_query(LServer,
						    [<<"select xml, seq from spool  where username='">>,
						     Username,
						     <<"'  order by seq;">>])
		     of
		   {selected, [<<"xml">>, <<"seq">>], Rs} ->
		       lists:flatmap(fun ([XML, Seq]) ->
					     case xml_stream:parse_element(XML)
						 of
					       {error, _Reason} -> [];
					       El -> [{El, Seq}]
					     end
				     end,
				     Rs);
		   _ -> []
		 end,
	  F = fun () ->
		      lists:foreach(fun ({Msg, Seq}) ->
					    ID =
						jlib:encode_base64((term_to_binary(Msg))),
					    case lists:member({<<"selected">>,
							       ID},
							      Query)
						of
					      true ->
						  SSeq =
						      ejabberd_odbc:escape(Seq),
						  catch
						    ejabberd_odbc:sql_query(LServer,
									    [<<"delete from spool  where username='">>,
									     Username,
									     <<"'  and seq='">>,
									     SSeq,
									     <<"';">>]);
					      false -> ok
					    end
				    end,
				    Msgs)
	      end,
	  mnesia:transaction(F),
	  ok;
      false -> nothing
    end.

us_to_list({User, Server}) ->
    jid:to_string({User, Server, <<"">>}).

get_queue_length(LUser, LServer) ->
    get_queue_length(LUser, LServer,
		     gen_mod:db_type(LServer, ?MODULE)).

get_queue_length(LUser, LServer, mnesia) ->
    length(mnesia:dirty_read({offline_msg,
			       {LUser, LServer}}));
get_queue_length(LUser, LServer, riak) ->
    case ejabberd_riak:count_by_index(offline_msg,
                                      <<"us">>, {LUser, LServer}) of
        {ok, N} ->
            N;
        _ ->
            0
    end;
get_queue_length(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select count(*) from spool  where username='">>,
					Username, <<"';">>])
	of
      {selected, [_], [[SCount]]} ->
	  jlib:binary_to_integer(SCount);
      _ -> 0
    end.

get_messages_subset(User, Host, MsgsAll, DBType) ->
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
    get_messages_subset2(MaxOfflineMsgs, Length, MsgsAll,
			 DBType).

get_messages_subset2(Max, Length, MsgsAll, _DBType)
    when Length =< Max * 2 ->
    MsgsAll;
get_messages_subset2(Max, Length, MsgsAll, DBType)
  when DBType == mnesia; DBType == riak ->
    FirstN = Max,
    {MsgsFirstN, Msgs2} = lists:split(FirstN, MsgsAll),
    MsgsLastN = lists:nthtail(Length - FirstN - FirstN,
			      Msgs2),
    NoJID = jid:make(<<"...">>, <<"...">>, <<"">>),
    IntermediateMsg = #offline_msg{timestamp = p1_time_compat:timestamp(),
				   from = NoJID, to = NoJID,
				   packet =
				       #xmlel{name = <<"...">>, attrs = [],
					      children = []}},
    MsgsFirstN ++ [IntermediateMsg] ++ MsgsLastN;
get_messages_subset2(Max, Length, MsgsAll, odbc) ->
    FirstN = Max,
    {MsgsFirstN, Msgs2} = lists:split(FirstN, MsgsAll),
    MsgsLastN = lists:nthtail(Length - FirstN - FirstN,
			      Msgs2),
    IntermediateMsg = #xmlel{name = <<"...">>, attrs = [],
			     children = []},
    MsgsFirstN ++ [IntermediateMsg] ++ MsgsLastN.

webadmin_user(Acc, User, Server, Lang) ->
    QueueLen = get_queue_length(jid:nodeprep(User),
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
    delete_all_msgs(LUser, LServer,
		    gen_mod:db_type(LServer, ?MODULE)).

delete_all_msgs(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:write_lock_table(offline_msg),
		lists:foreach(fun (Msg) -> mnesia:delete_object(Msg)
			      end,
			      mnesia:dirty_read({offline_msg, US}))
	end,
    mnesia:transaction(F);
delete_all_msgs(LUser, LServer, riak) ->
    Res = ejabberd_riak:delete_by_index(offline_msg,
                                        <<"us">>, {LUser, LServer}),
    {atomic, Res};
delete_all_msgs(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_spool_msg(LServer, Username),
    {atomic, ok}.

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
    DBType = gen_mod:db_type(LServer, ?MODULE),
    count_offline_messages(LUser, LServer, DBType).

count_offline_messages(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () ->
		count_mnesia_records(US)
	end,
    case catch mnesia:async_dirty(F) of
      I when is_integer(I) -> I;
      _ -> 0
    end;
count_offline_messages(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:count_records_where(LServer,
						<<"spool">>,
						<<"where username='",
						  Username/binary, "'">>)
	of
      {selected, [_], [[Res]]} ->
	  jlib:binary_to_integer(Res);
      _ -> 0
    end;
count_offline_messages(LUser, LServer, riak) ->
    case ejabberd_riak:count_by_index(
           offline_msg, <<"us">>, {LUser, LServer}) of
        {ok, Res} ->
            Res;
        _ ->
            0
    end.

%% Return the number of records matching a given match expression.
%% This function is intended to be used inside a Mnesia transaction.
%% The count has been written to use the fewest possible memory by
%% getting the record by small increment and by using continuation.
-define(BATCHSIZE, 100).

count_mnesia_records(US) ->
    MatchExpression = #offline_msg{us = US,  _ = '_'},
    case mnesia:select(offline_msg, [{MatchExpression, [], [[]]}],
		       ?BATCHSIZE, read) of
	{Result, Cont} ->
	    Count = length(Result),
	    count_records_cont(Cont, Count);
	'$end_of_table' ->
	    0
    end.

count_records_cont(Cont, Count) ->
    case mnesia:select(Cont) of
	{Result, Cont} ->
	    NewCount = Count + length(Result),
	    count_records_cont(Cont, NewCount);
	'$end_of_table' ->
	    Count
    end.

offline_msg_schema() ->
    {record_info(fields, offline_msg), #offline_msg{}}.

export(_Server) ->
    [{offline_msg,
      fun(Host, #offline_msg{us = {LUser, LServer},
                             timestamp = TimeStamp, from = From, to = To,
                             packet = Packet})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              Packet1 = jlib:replace_from_to(From, To, Packet),
              Packet2 = jlib:add_delay_info(Packet1, LServer, TimeStamp,
                                            <<"Offline Storage">>),
              XML = ejabberd_odbc:escape(xml:element_to_binary(Packet2)),
              [[<<"delete from spool where username='">>, Username, <<"';">>],
               [<<"insert into spool(username, xml) values ('">>,
                Username, <<"', '">>, XML, <<"');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, xml from spool;">>,
      fun([LUser, XML]) ->
              El = #xmlel{} = xml_stream:parse_element(XML),
              From = #jid{} = jid:from_string(
                                xml:get_attr_s(<<"from">>, El#xmlel.attrs)),
              To = #jid{} = jid:from_string(
                              xml:get_attr_s(<<"to">>, El#xmlel.attrs)),
              Stamp = xml:get_path_s(El, [{elem, <<"delay">>},
                                          {attr, <<"stamp">>}]),
              TS = case jlib:datetime_string_to_timestamp(Stamp) of
                       {_, _, _} = Now ->
                           Now;
                       undefined ->
                           p1_time_compat:timestamp()
                   end,
              Expire = find_x_expire(TS, El#xmlel.children),
              #offline_msg{us = {LUser, LServer},
                           from = From, to = To,
                           timestamp = TS, expire = Expire}
      end}].

import(_LServer, mnesia, #offline_msg{} = Msg) ->
    mnesia:dirty_write(Msg);
import(_LServer, riak, #offline_msg{us = US, timestamp = TS} = M) ->
    ejabberd_riak:put(M, offline_msg_schema(),
		      [{i, TS}, {'2i', [{<<"us">>, US}]}]);
import(_, _, _) ->
    pass.

mod_opt_type(access_max_user_messages) ->
    fun (A) -> A end;
mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(store_empty_body) ->
    fun (V) when is_boolean(V) -> V;
        (unless_chat_state) -> unless_chat_state
    end;
mod_opt_type(_) ->
    [access_max_user_messages, db_type, store_empty_body].
