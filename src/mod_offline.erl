%%%----------------------------------------------------------------------
%%% File    : mod_offline.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages in Mnesia database.
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_offline).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 init/0,
	 stop/1,
	 store_packet/3,
	 resend_offline_messages/2,
	 pop_offline_messages/3,
	 remove_expired_messages/0,
	 remove_old_messages/1,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(offline_msg, {us, timestamp, expire, from, to, packet}).

-define(PROCNAME, ejabberd_offline).
-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

%% TODO: Move this part as a module config file parameter:
%% Can be an integer > 0 or infinity:
-define(MAX_OFFLINE_MSGS, infinity).

start(Host, _Opts) ->
    mnesia:create_table(offline_msg,
			[{disc_only_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, offline_msg)}]),
    update_table(),
    ejabberd_hooks:add(offline_message_hook, Host,
		       ?MODULE, store_packet, 50),
    ejabberd_hooks:add(resend_offline_messages_hook, Host,
		       ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [])).

init() ->
    loop().

loop() ->
    receive
	#offline_msg{us=US} = Msg ->
	    Msgs = receive_all(US, [Msg]),
	    Len = length(Msgs),
	    F = fun() ->
			Count = Len + p1_mnesia:count_records(
					offline_msg, 
					#offline_msg{us=US, _='_'}),
			if
			    Count > ?MAX_OFFLINE_MSGS ->
				%% TODO: Warn that messages have been discarded
				ok;
			    true ->
				if
				    Len >= ?OFFLINE_TABLE_LOCK_THRESHOLD ->
					mnesia:write_lock_table(offline_msg);
				    true ->
					ok
				end,
				lists:foreach(fun(M) ->
						      mnesia:write(M)
					      end, Msgs)
			end
		end,
	    mnesia:transaction(F),
	    loop();
	_ ->
	    loop()
    end.

receive_all(US, Msgs) ->
    receive
	#offline_msg{us=US} = Msg ->
	    receive_all(US, [Msg | Msgs])
    after 0 ->
	    Msgs
    end.


stop(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, store_packet, 50),
    ejabberd_hooks:delete(resend_offline_messages_hook, Host,
			  ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    exit(whereis(Proc), stop),
    {wait, Proc}.

store_packet(From, To, Packet) ->
    Type = xml:get_tag_attr_s("type", Packet),
    if
	(Type /= "error") and (Type /= "groupchat") and
	(Type /= "headline") ->
	    case check_event(From, To, Packet) of
		true ->
		    #jid{luser = LUser, lserver = LServer} = To,
		    TimeStamp = now(),
		    {xmlelement, _Name, _Attrs, Els} = Packet,
		    Expire = find_x_expire(TimeStamp, Els),
		    gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME) !
			#offline_msg{us = {LUser, LServer},
				     timestamp = TimeStamp,
				     expire = Expire,
				     from = From,
				     to = To,
				     packet = Packet},
		    stop;
		_ ->
		    ok
	    end;
	true ->
	    ok
    end.

check_event(From, To, Packet) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    case find_x_event(Els) of
	false ->
	    true;
	El ->
	    case xml:get_subtag(El, "id") of
		false ->
		    case xml:get_subtag(El, "offline") of
			false ->
			    true;
			_ ->
			    ID = case xml:get_tag_attr_s("id", Packet) of
				     "" ->
					 {xmlelement, "id", [], []};
				     S ->
					 {xmlelement, "id", [],
					  [{xmlcdata, S}]}
				 end,
			    ejabberd_router:route(
			      To, From, {xmlelement, Name, Attrs,
					 [{xmlelement, "x",
					   [{"xmlns", ?NS_EVENT}],
					   [ID,
					    {xmlelement, "offline", [], []}]}]
					}),
			    true
		    end;
		_ ->
		    false
	    end
    end.

find_x_event([]) ->
    false;
find_x_event([{xmlcdata, _} | Els]) ->
    find_x_event(Els);
find_x_event([El | Els]) ->
    case xml:get_tag_attr_s("xmlns", El) of
	?NS_EVENT ->
	    El;
	_ ->
	    find_x_event(Els)
    end.

find_x_expire(_, []) ->
    never;
find_x_expire(TimeStamp, [{xmlcdata, _} | Els]) ->
    find_x_expire(TimeStamp, Els);
find_x_expire(TimeStamp, [El | Els]) ->
    case xml:get_tag_attr_s("xmlns", El) of
	?NS_EXPIRE ->
	    Val = xml:get_tag_attr_s("seconds", El),
	    case catch list_to_integer(Val) of
		{'EXIT', _} ->
		    never;
		Int when Int > 0 ->
		    {MegaSecs, Secs, MicroSecs} = TimeStamp,
		    S = MegaSecs * 1000000 + Secs + Int,
		    MegaSecs1 = S div 1000000,
		    Secs1 = S rem 1000000,
		    {MegaSecs1, Secs1, MicroSecs};
		_ ->
		    never
	    end;
	_ ->
	    find_x_expire(TimeStamp, Els)
    end.


resend_offline_messages(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		Rs = mnesia:wread({offline_msg, US}),
		mnesia:delete({offline_msg, US}),
		Rs
	end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:foreach(
	      fun(R) ->
		      {xmlelement, Name, Attrs, Els} = R#offline_msg.packet,
		      ejabberd_sm !
			  {route,
			   R#offline_msg.from,
			   R#offline_msg.to,
			   {xmlelement, Name, Attrs,
			    Els ++
			    [jlib:timestamp_to_xml(
			       calendar:now_to_universal_time(
				 R#offline_msg.timestamp))]}}
	      end,
	      lists:keysort(#offline_msg.timestamp, Rs));
	_ ->
	    ok
    end.

pop_offline_messages(Ls, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		Rs = mnesia:wread({offline_msg, US}),
		mnesia:delete({offline_msg, US}),
		Rs
	end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    TS = now(),
	    Ls ++ lists:map(
		    fun(R) ->
			    {xmlelement, Name, Attrs, Els} = R#offline_msg.packet,
			    {route,
			     R#offline_msg.from,
			     R#offline_msg.to,
			     {xmlelement, Name, Attrs,
			      Els ++
			      [jlib:timestamp_to_xml(
				 calendar:now_to_universal_time(
				   R#offline_msg.timestamp))]}}
		    end,
		    lists:filter(
		      fun(R) ->
			    case R#offline_msg.expire of
				never ->
				    true;
				TimeStamp ->
				    TS < TimeStamp
			    end
		      end,
		      lists:keysort(#offline_msg.timestamp, Rs)));
	_ ->
	    Ls
    end.

remove_expired_messages() ->
    TimeStamp = now(),
    F = fun() ->
		mnesia:write_lock_table(offline_msg),
		mnesia:foldl(
		  fun(Rec, _Acc) ->
			  case Rec#offline_msg.expire of
			      never ->
				  ok;
			      TS ->
				  if
				      TS < TimeStamp ->
					  mnesia:delete_object(Rec);
				      true ->
					  ok
				  end
			  end
		  end, ok, offline_msg)
	end,
    mnesia:transaction(F).

remove_old_messages(Days) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    S = MegaSecs * 1000000 + Secs - 60 * 60 * 24 * Days,
    MegaSecs1 = S div 1000000,
    Secs1 = S rem 1000000,
    TimeStamp = {MegaSecs1, Secs1, 0},
    F = fun() ->
		mnesia:write_lock_table(offline_msg),
		mnesia:foldl(
		  fun(#offline_msg{timestamp = TS} = Rec, _Acc)
		     when TS < TimeStamp ->
			  mnesia:delete_object(Rec);
		     (_Rec, _Acc) -> ok
		  end, ok, offline_msg)
	end,
    mnesia:transaction(F).

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		mnesia:delete({offline_msg, US})
	end,
    mnesia:transaction(F).

update_table() ->
    Fields = record_info(fields, offline_msg),
    case mnesia:table_info(offline_msg, attributes) of
	Fields ->
	    ok;
	[user, timestamp, expire, from, to, packet] ->
	    ?INFO_MSG("Converting offline_msg table from "
		      "{user, timestamp, expire, from, to, packet} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_offline_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, offline_msg},
			      {attributes, record_info(fields, offline_msg)}]),
	    mnesia:transform_table(offline_msg, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_offline_tmp_table),
			 mnesia:foldl(
			   fun(#offline_msg{us = U} = R, _) ->
				   mnesia:dirty_write(
				     mod_offline_tmp_table,
				     R#offline_msg{us = {U, Host}})
			   end, ok, offline_msg)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(offline_msg),
	    F2 = fun() ->
			 mnesia:write_lock_table(offline_msg),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_offline_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_offline_tmp_table);
	[user, timestamp, from, to, packet] ->
	    ?INFO_MSG("Converting offline_msg table from "
		      "{user, timestamp, from, to, packet} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_offline_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, offline_msg},
			      {attributes, record_info(fields, offline_msg)}]),
	    mnesia:transform_table(
	      offline_msg,
	      fun({_, U, TS, F, T, P}) ->
		      {xmlelement, _Name, _Attrs, Els} = P,
		      Expire = find_x_expire(TS, Els),
		      #offline_msg{us = U,
				   timestamp = TS,
				   expire = Expire,
				   from = F,
				   to = T,
				   packet = P}
	      end, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_offline_tmp_table),
			 mnesia:foldl(
			   fun(#offline_msg{us = U} = R, _) ->
				   mnesia:dirty_write(
				     mod_offline_tmp_table,
				     R#offline_msg{us = {U, Host}})
			   end, ok, offline_msg)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(offline_msg),
	    F2 = fun() ->
			 mnesia:write_lock_table(offline_msg),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_offline_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_offline_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating offline_msg table", []),
	    mnesia:transform_table(offline_msg, ignore, Fields)
    end.
