%%%----------------------------------------------------------------------
%%% File    : mod_offline.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_offline).
-author('alexey@sevcom.net').

-behaviour(gen_mod).

-export([start/1,
	 init/0,
	 stop/0,
	 store_packet/3,
	 resend_offline_messages/1,
	 pop_offline_messages/1,
	 remove_old_messages/1,
	 remove_user/1]).

-include("jlib.hrl").

-record(offline_msg, {user, timestamp, from, to, packet}).

-define(PROCNAME, ejabberd_offline).

start(_) ->
    mnesia:create_table(offline_msg,
			[{disc_only_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, offline_msg)}]),
    register(?PROCNAME, spawn(?MODULE, init, [])).

init() ->
    loop().

loop() ->
    receive
	#offline_msg{} = Msg ->
	    Msgs = receive_all([Msg]),
	    F = fun() ->
			lists:foreach(fun(M) ->
					      mnesia:write(M)
				      end, Msgs)
		end,
	    mnesia:transaction(F),
	    loop();
	_ ->
	    loop()
    end.

receive_all(Msgs) ->
    receive
	#offline_msg{} = Msg ->
	    receive_all([Msg | Msgs])
    after 0 ->
	    Msgs
    end.


stop() ->
    exit(whereis(?PROCNAME), stop),
    ok.

store_packet(From, To, Packet) ->
    true = is_process_alive(whereis(?PROCNAME)),
    Type = xml:get_tag_attr_s("type", Packet),
    true = Type /= "error" andalso Type /= "groupchat",
    case check_event(From, To, Packet) of
	true ->
	    #jid{luser = LUser} = To,
	    TimeStamp = now(),
	    ?PROCNAME ! #offline_msg{user = LUser,
				     timestamp = TimeStamp,
				     from = From,
				     to = To,
				     packet = Packet};
	_ ->
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


resend_offline_messages(User) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
		Rs = mnesia:wread({offline_msg, LUser}),
		mnesia:delete({offline_msg, LUser}),
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

pop_offline_messages(User) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
		Rs = mnesia:wread({offline_msg, LUser}),
		mnesia:delete({offline_msg, LUser}),
		Rs
	end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    lists:map(
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
	      lists:keysort(#offline_msg.timestamp, Rs));
	_ ->
	    []
    end.

remove_old_messages(Days) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    S = MegaSecs * 1000000 + Secs - 60 * 60 * 24 * Days,
    MegaSecs1 = S div 1000000,
    Secs1 = S rem 1000000,
    TimeStamp = {MegaSecs1, Secs1, 0},
    F = fun() ->
		mnesia:foldl(
		  fun(#offline_msg{timestamp = TS} = Rec, _Acc)
		     when TS < TimeStamp ->
			  mnesia:delete_object(Rec);
		     (_Rec, _Acc) -> ok
		  end, ok, offline_msg)
	end,
    mnesia:transaction(F).

remove_user(User) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
		mnesia:delete({offline_msg, LUser})
	end,
    mnesia:transaction(F).
