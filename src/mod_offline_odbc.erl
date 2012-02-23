%%%----------------------------------------------------------------------
%%% File    : mod_offline_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages in relational database.
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

-module(mod_offline_odbc).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([count_offline_messages/2]).

-export([start/2,
	 loop/2,
	 stop/1,
	 store_packet/3,
	 pop_offline_messages/3,
	 get_sm_features/5,
	 remove_user/2,
	 get_queue_length/2,
	 webadmin_page/3,
	 webadmin_user/4,
	 webadmin_user_parse_query/5]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-record(offline_msg, {user, timestamp, expire, from, to, packet}).

-define(PROCNAME, ejabberd_offline).
-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

%% default value for the maximum number of user messages
-define(MAX_USER_MESSAGES, infinity).

start(Host, Opts) ->
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
	     spawn(?MODULE, loop, [Host, AccessMaxOfflineMsgs])).

loop(Host, AccessMaxOfflineMsgs) ->
    receive
	#offline_msg{user = User} = Msg ->
	    Msgs = receive_all(User, [Msg]),
	    Len = length(Msgs),
	    MaxOfflineMsgs = get_max_user_messages(AccessMaxOfflineMsgs,
						   User, Host),

	    %% Only count existing messages if needed:
	    Count = if MaxOfflineMsgs =/= infinity ->
			    Len + count_offline_messages(User, Host);
		       true -> 0
		    end,
	    if 
		Count > MaxOfflineMsgs ->
		    discard_warn_sender(Msgs);
		true ->
		    Query = lists:map(
			      fun(M) ->
				      Username =
					  ejabberd_odbc:escape(
					    (M#offline_msg.to)#jid.luser),
				      From = M#offline_msg.from,
				      To = M#offline_msg.to,
				      {xmlelement, Name, Attrs, Els} =
					  M#offline_msg.packet,
				      Attrs2 = jlib:replace_from_to_attrs(
						 jlib:jid_to_string(From),
						 jlib:jid_to_string(To),
						 Attrs),
				      Packet = {xmlelement, Name, Attrs2,
						Els ++
						[jlib:timestamp_to_xml(
						   calendar:now_to_universal_time(
					     M#offline_msg.timestamp),
					   utc,
					   jlib:make_jid("", Host, ""),
					   "Offline Storage"),
					 %% TODO: Delete the next three lines once XEP-0091 is Obsolete
					 jlib:timestamp_to_xml( 
					   calendar:now_to_universal_time(
						     M#offline_msg.timestamp))]},
				      XML =
					  ejabberd_odbc:escape(
					    xml:element_to_binary(Packet)),
				      odbc_queries:add_spool_sql(Username, XML)
			      end, Msgs),
		    case catch odbc_queries:add_spool(Host, Query) of
			{'EXIT', Reason} ->
			    ?ERROR_MSG("~p~n", [Reason]);
			{error, Reason} ->
			    ?ERROR_MSG("~p~n", [Reason]);
			_ ->
			    ok
		    end
	    end,
	    loop(Host, AccessMaxOfflineMsgs);
	_ ->
	    loop(Host, AccessMaxOfflineMsgs)
    end.

%% Function copied from ejabberd_sm.erl:
get_max_user_messages(AccessRule, LUser, Host) ->
    case acl:match_rule(
	   Host, AccessRule, jlib:make_jid(LUser, Host, "")) of
	Max when is_integer(Max) -> Max;
	infinity -> infinity;
	_ -> ?MAX_USER_MESSAGES
    end.

receive_all(Username, Msgs) ->
    receive
	#offline_msg{user=Username} = Msg ->
	    receive_all(Username, [Msg | Msgs])
    after 0 ->
	    lists:reverse(Msgs)
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
    ok.

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
		    #jid{luser = LUser} = To,
		    TimeStamp = now(),
		    {xmlelement, _Name, _Attrs, Els} = Packet,
		    Expire = find_x_expire(TimeStamp, Els),
		    gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME) !
			#offline_msg{user = LUser,
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


pop_offline_messages(Ls, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    EUser = ejabberd_odbc:escape(LUser),
    case odbc_queries:get_and_del_spool_msg_t(LServer, EUser) of
	{atomic, {selected, ["username","xml"], Rs}} ->
	    Ls ++ lists:flatmap(
		    fun({_, XML}) ->
			    case xml_stream:parse_element(XML) of
				{error, _Reason} ->
				    [];
				El ->
				    To = jlib:string_to_jid(
					   xml:get_tag_attr_s("to", El)),
				    From = jlib:string_to_jid(
					     xml:get_tag_attr_s("from", El)),
				    if
					(To /= error) and
					(From /= error) ->
					    [{route, From, To, El}];
					true ->
					    []
				    end
			    end
		    end, Rs);
	_ ->
	    Ls
    end.


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_spool_msg(LServer, Username).


%% Helper functions:

%% TODO: Warning - This function is a duplicate from mod_offline.erl
%% It is duplicate to stay consistent (many functions are duplicated
%% in this module). It will be refactored later on.
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
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    US = {LUser, LServer},
    Res = user_queue_parse_query(Username, LServer, Query),
    MsgsAll = case catch ejabberd_odbc:sql_query(
			LServer,
			["select username, xml from spool"
			 "  where username='", Username, "'"
			 "  order by seq;"]) of
	       {selected, ["username", "xml"], Rs} ->
		   lists:flatmap(
		     fun({_, XML}) ->
			     case xml_stream:parse_element(XML) of
				 {error, _Reason} ->
				     [];
				 El ->
				     [El]
			     end
		     end, Rs);
	       _ ->
		   []
	   end,
    Msgs = get_messages_subset(User, Server, MsgsAll),
    FMsgs =
	lists:map(
	  fun({xmlelement, _Name, _Attrs, _Els} = Msg) ->
		  ID = jlib:encode_base64(binary_to_list(term_to_binary(Msg))),
		  Packet = Msg,
		  FPacket = ejabberd_web_admin:pretty_print_xml(Packet),
		  ?XE("tr",
		      [?XAE("td", [{"class", "valign"}], [?INPUT("checkbox", "selected", ID)]),
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

user_queue_parse_query(Username, LServer, Query) ->
    case lists:keysearch("delete", 1, Query) of
	{value, _} ->
	    Msgs = case catch ejabberd_odbc:sql_query(
				LServer,
				["select xml, seq from spool"
				 "  where username='", Username, "'"
				 "  order by seq;"]) of
		       {selected, ["xml", "seq"], Rs} ->
			   lists:flatmap(
			     fun({XML, Seq}) ->
				     case xml_stream:parse_element(XML) of
					 {error, _Reason} ->
					     [];
					 El ->
					     [{El, Seq}]
				     end
			     end, Rs);
		       _ ->
			   []
		   end,
	    F = fun() ->
			lists:foreach(
			  fun({Msg, Seq}) ->
				  ID = jlib:encode_base64(
					 binary_to_list(term_to_binary(Msg))),
				  case lists:member({"selected", ID}, Query) of
				      true ->
					  SSeq = ejabberd_odbc:escape(Seq),
					  catch ejabberd_odbc:sql_query(
						  LServer,
						  ["delete from spool"
						   "  where username='", Username, "'"
						   "  and seq='", SSeq, "';"]);
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

get_queue_length(Username, LServer) ->
    case catch ejabberd_odbc:sql_query(
			    LServer,
			    ["select count(*) from spool"
			     "  where username='", Username, "';"]) of
		   {selected, [_], [{SCount}]} ->
		       SCount;
		   _ ->
		       0
	       end.

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
    IntermediateMsg = {xmlelement, "...", [], []},
    MsgsFirstN ++ [IntermediateMsg] ++ MsgsLastN.

webadmin_user(Acc, User, Server, Lang) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    QueueLen = get_queue_length(Username, LServer),
    FQueueLen = [?AC("queue/", QueueLen)],
    Acc ++ [?XCT("h3", "Offline Messages:")] ++ FQueueLen ++ [?C(" "), ?INPUTT("submit", "removealloffline", "Remove All Offline Messages")].

webadmin_user_parse_query(_, "removealloffline", User, Server, _Query) ->
    case catch odbc_queries:del_spool_msg(Server, User) of
         {'EXIT', Reason} ->
            ?ERROR_MSG("Failed to remove offline messages: ~p", [Reason]),
            {stop, error};
         {error, Reason} ->
            ?ERROR_MSG("Failed to remove offline messages: ~p", [Reason]),
            {stop, error};
         _ ->
            ?INFO_MSG("Removed all offline messages for ~s@~s", [User, Server]),
            {stop, ok}
    end;
webadmin_user_parse_query(Acc, _Action, _User, _Server, _Query) ->
    Acc.

%% ------------------------------------------------
%% mod_offline: number of messages quota management

%% Returns as integer the number of offline messages for a given user
count_offline_messages(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:count_records_where(
		 LServer, "spool", "where username='" ++ Username ++ "'") of
        {selected, [_], [{Res}]} ->
            list_to_integer(Res);
        _ ->
            0
    end.
