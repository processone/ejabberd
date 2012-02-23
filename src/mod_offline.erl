%%%----------------------------------------------------------------------
%%% File    : mod_offline.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages in Mnesia database.
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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

-module(mod_offline).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 loop/1,
	 stop/1,
	 store_packet/3,
	 resend_offline_messages/2,
	 pop_offline_messages/3,
	 get_sm_features/5,
	 remove_expired_messages/0,
	 remove_old_messages/1,
	 remove_user/2,
	 get_queue_length/2,
	 webadmin_page/3,
	 webadmin_user/4,
	 webadmin_user_parse_query/5]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-record(offline_msg, {us, timestamp, expire, from, to, packet}).

-define(PROCNAME, ejabberd_offline).
-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

%% default value for the maximum number of user messages
-define(MAX_USER_MESSAGES, infinity).

start(Host, Opts) ->
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
    AccessMaxOfflineMsgs = gen_mod:get_opt(access_max_user_messages, Opts, max_user_offline_messages),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, loop, [AccessMaxOfflineMsgs])).

loop(AccessMaxOfflineMsgs) ->
    receive
	#offline_msg{us=US} = Msg ->
	    Msgs = receive_all(US, [Msg]),
	    Len = length(Msgs),
	    {User, Host} = US,
	    MaxOfflineMsgs = get_max_user_messages(AccessMaxOfflineMsgs,
						   User, Host),
	    F = fun() ->
			%% Only count messages if needed:
			Count = if MaxOfflineMsgs =/= infinity ->
					Len + p1_mnesia:count_records(
						offline_msg, 
						#offline_msg{us=US, _='_'});
				   true -> 
					0
				end,
			if
			    Count > MaxOfflineMsgs ->
				discard_warn_sender(Msgs);
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
	    loop(AccessMaxOfflineMsgs);
	_ ->
	    loop(AccessMaxOfflineMsgs)
    end.

%% Function copied from ejabberd_sm.erl:
get_max_user_messages(AccessRule, LUser, Host) ->
    case acl:match_rule(
	   Host, AccessRule, jlib:make_jid(LUser, Host, "")) of
	Max when is_integer(Max) -> Max;
	infinity -> infinity;
	_ -> ?MAX_USER_MESSAGES
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
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host,
			  ?MODULE, webadmin_user, 50),
    ejabberd_hooks:delete(webadmin_user_parse_query, Host,
                          ?MODULE, webadmin_user_parse_query, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    exit(whereis(Proc), stop),
    {wait, Proc}.

get_sm_features(Acc, _From, _To, "", _Lang) ->
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


store_packet(From, To, Packet) ->
    Type = xml:get_tag_attr_s("type", Packet),
    if
	(Type /= "error") and (Type /= "groupchat") and
	(Type /= "headline") ->
	    case check_event_chatstates(From, To, Packet) of
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

%% Check if the packet has any content about XEP-0022 or XEP-0085
check_event_chatstates(From, To, Packet) ->
    {xmlelement, Name, Attrs, Els} = Packet,
    case find_x_event_chatstates(Els, {false, false, false}) of
	%% There wasn't any x:event or chatstates subelements
	{false, false, _} ->
	    true;
	%% There a chatstates subelement and other stuff, but no x:event
	{false, CEl, true} when CEl /= false ->
	    true;
	%% There was only a subelement: a chatstates
	{false, CEl, false} when CEl /= false ->
	    %% Don't allow offline storage
	    false;
	%% There was an x:event element, and maybe also other stuff
	{El, _, _} when El /= false ->
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

%% Check if the packet has subelements about XEP-0022, XEP-0085 or other
find_x_event_chatstates([], Res) ->
    Res;
find_x_event_chatstates([{xmlcdata, _} | Els], Res) ->
    find_x_event_chatstates(Els, Res);
find_x_event_chatstates([El | Els], {A, B, C}) ->
    case xml:get_tag_attr_s("xmlns", El) of
	?NS_EVENT ->
	    find_x_event_chatstates(Els, {El, B, C});
	?NS_CHATSTATES ->
	    find_x_event_chatstates(Els, {A, El, C});
	_ ->
	    find_x_event_chatstates(Els, {A, B, true})
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
				 R#offline_msg.timestamp),
			       utc,
			       jlib:make_jid("", Server, ""),
			       "Offline Storage"),
			     %% TODO: Delete the next three lines once XEP-0091 is Obsolete
			     jlib:timestamp_to_xml(
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
				   R#offline_msg.timestamp),
				 utc,
				 jlib:make_jid("", Server, ""),
				 "Offline Storage"),
			       %% TODO: Delete the next three lines once XEP-0091 is Obsolete
			       jlib:timestamp_to_xml(
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


%% Helper functions:

%% Warn senders that their messages have been discarded:
discard_warn_sender(Msgs) ->
    lists:foreach(
      fun(#offline_msg{from=From, to=To, packet=Packet}) ->
	      ErrText = "Your contact offline message queue is full. The message has been discarded.",
	      Lang = xml:get_tag_attr_s("xml:lang", Packet),
	      Err = jlib:make_error_reply(
		      Packet, ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)),
	      ejabberd_router:route(
		To,
		From, Err)
      end, Msgs).


webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "queue"],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = user_queue(U, Host, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

user_queue(User, Server, Query, Lang) ->
    US = {jlib:nodeprep(User), jlib:nameprep(Server)},
    Res = user_queue_parse_query(US, Query),
    MsgsAll = lists:keysort(#offline_msg.timestamp,
			    mnesia:dirty_read({offline_msg, US})),
    Msgs = get_messages_subset(User, Server, MsgsAll),
    FMsgs =
	lists:map(
	  fun(#offline_msg{timestamp = TimeStamp, from = From, to = To,
			   packet = {xmlelement, Name, Attrs, Els}} = Msg) ->
		  ID = jlib:encode_base64(binary_to_list(term_to_binary(Msg))),
		  {{Year, Month, Day}, {Hour, Minute, Second}} =
		      calendar:now_to_local_time(TimeStamp),
		  Time = lists:flatten(
			   io_lib:format(
			     "~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
			     [Year, Month, Day, Hour, Minute, Second])),
		  SFrom = jlib:jid_to_string(From),
		  STo = jlib:jid_to_string(To),
		  Attrs2 = jlib:replace_from_to_attrs(SFrom, STo, Attrs),
		  Packet = {xmlelement, Name, Attrs2, Els},
		  FPacket = ejabberd_web_admin:pretty_print_xml(Packet),
		  ?XE("tr",
		      [?XAE("td", [{"class", "valign"}], [?INPUT("checkbox", "selected", ID)]),
		       ?XAC("td", [{"class", "valign"}], Time),
		       ?XAC("td", [{"class", "valign"}], SFrom),
		       ?XAC("td", [{"class", "valign"}], STo),
		       ?XAE("td", [{"class", "valign"}], [?XC("pre", FPacket)])]
		     )
	  end, Msgs),
    [?XC("h1", io_lib:format(?T("~s's Offline Messages Queue"),
			     [us_to_list(US)]))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [?XE("table",
		   [?XE("thead",
			[?XE("tr",
			     [?X("td"),
			      ?XCT("td", "Time"),
			      ?XCT("td", "From"),
			      ?XCT("td", "To"),
			      ?XCT("td", "Packet")
			     ])]),
		    ?XE("tbody",
			if
			    FMsgs == [] ->
				[?XE("tr",
				     [?XAC("td", [{"colspan", "4"}], " ")]
				    )];
			    true ->
				FMsgs
			end
		       )]),
	       ?BR,
	       ?INPUTT("submit", "delete", "Delete Selected")
	      ])].

user_queue_parse_query(US, Query) ->
    case lists:keysearch("delete", 1, Query) of
	{value, _} ->
	    Msgs = lists:keysort(#offline_msg.timestamp,
				 mnesia:dirty_read({offline_msg, US})),
	    F = fun() ->
			lists:foreach(
			  fun(Msg) ->
				  ID = jlib:encode_base64(
					 binary_to_list(term_to_binary(Msg))),
				  case lists:member({"selected", ID}, Query) of
				      true ->
					  mnesia:delete_object(Msg);
				      false ->
					  ok
				  end
			  end, Msgs)
		end,
	    mnesia:transaction(F),
	    ok;
	false ->
	    nothing
    end.

us_to_list({User, Server}) ->
    jlib:jid_to_string({User, Server, ""}).

get_queue_length(User, Server) ->
    length(mnesia:dirty_read({offline_msg, {User, Server}})).

get_messages_subset(User, Host, MsgsAll) ->
    Access = gen_mod:get_module_opt(Host, ?MODULE, access_max_user_messages,
				    max_user_offline_messages),
    MaxOfflineMsgs = case get_max_user_messages(Access, User, Host) of
			 Number when is_integer(Number) -> Number;
			 _ -> 100
		     end,
    Length = length(MsgsAll),
    get_messages_subset2(MaxOfflineMsgs, Length, MsgsAll).

get_messages_subset2(Max, Length, MsgsAll) when Length =< Max*2 ->
    MsgsAll;
get_messages_subset2(Max, Length, MsgsAll) ->
    FirstN = Max,
    {MsgsFirstN, Msgs2} = lists:split(FirstN, MsgsAll),
    MsgsLastN = lists:nthtail(Length - FirstN - FirstN, Msgs2),
    NoJID = jlib:make_jid("...", "...", ""),
    IntermediateMsg = #offline_msg{timestamp = now(), from = NoJID, to = NoJID,
				   packet = {xmlelement, "...", [], []}},
    MsgsFirstN ++ [IntermediateMsg] ++ MsgsLastN.

webadmin_user(Acc, User, Server, Lang) ->
    QueueLen = get_queue_length(jlib:nodeprep(User), jlib:nameprep(Server)),
    FQueueLen = [?AC("queue/",
		     integer_to_list(QueueLen))],
    Acc ++ [?XCT("h3", "Offline Messages:")] ++ FQueueLen ++ [?C(" "), ?INPUTT("submit", "removealloffline", "Remove All Offline Messages")].

webadmin_user_parse_query(_, "removealloffline", User, Server, _Query) ->
    US = {User, Server},
    F = fun() ->
            mnesia:write_lock_table(offline_msg),
            lists:foreach(
              fun(Msg) ->
                      mnesia:delete_object(Msg)
              end, mnesia:dirty_read({offline_msg, US}))
        end,
    case mnesia:transaction(F) of
         {aborted, Reason} ->
            ?ERROR_MSG("Failed to remove offline messages: ~p", [Reason]),
            {stop, error};
         {atomic, ok} ->
            ?INFO_MSG("Removed all offline messages for ~s@~s", [User, Server]),
            {stop, ok}
    end;
webadmin_user_parse_query(Acc, _Action, _User, _Server, _Query) ->
    Acc.
