%%%----------------------------------------------------------------------
%%% File    : mod_offline_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages in relational database.
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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
	 webadmin_page/3,
	 webadmin_user/4,
	 webadmin_user_parse_query/5]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-record(offline_msg, {user, timestamp, expire, from, to, packet}).

-define(PROCNAME, ejabberd_offline).
-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

% These are the namespace already declared by the stream opening. This is
% used at serialization time.
-define(DEFAULT_NS, ?NS_JABBER_CLIENT).
-define(PREFIXED_NS, [{?NS_XMPP, ?NS_XMPP_pfx}]).

%% default value for the maximum number of user messages
-define(MAX_USER_MESSAGES, infinity).

start(Host, Opts) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:add(offline_message_hook, HostB,
		       ?MODULE, store_packet, 50),
    ejabberd_hooks:add(resend_offline_messages_hook, HostB,
		       ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(disco_sm_features, HostB,
		       ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(disco_local_features, HostB,
		       ?MODULE, get_sm_features, 50),
    ejabberd_hooks:add(webadmin_page_host, HostB,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, HostB,
		       ?MODULE, webadmin_user, 50),
    ejabberd_hooks:add(webadmin_user_parse_query, HostB,
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
					    exmpp_jid:prep_node_as_list(M#offline_msg.to)),
				      From = M#offline_msg.from,
				      To = M#offline_msg.to,
				      Packet0 = exmpp_stanza:set_jids(
						 M#offline_msg.packet,
						 From,
						 To),
				      Packet1 = exmpp_xml:append_child(Packet0,
						jlib:timestamp_to_xml(
						   calendar:now_to_universal_time(
						     M#offline_msg.timestamp))),
				      XML =
					  ejabberd_odbc:escape(
					    exmpp_xml:document_to_list(Packet1)),
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
	   Host, AccessRule, exmpp_jid:make(LUser, Host, "")) of
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
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(offline_message_hook, HostB,
			  ?MODULE, store_packet, 50),
    ejabberd_hooks:delete(resend_offline_messages_hook, HostB,
			  ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(disco_sm_features, HostB, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(disco_local_features, HostB, ?MODULE, get_sm_features, 50),
    ejabberd_hooks:delete(webadmin_page_host, HostB,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, HostB,
			  ?MODULE, webadmin_user, 50),
    ejabberd_hooks:delete(webadmin_user_parse_query, HostB,
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
    Type = exmpp_stanza:get_type(Packet),
    if
	(Type /= <<"error">>) and (Type /= <<"groupchat">>) and
	(Type /= <<"headline">>) ->
	    case check_event(From, To, Packet) of
		true ->
            LUser = exmpp_jid:prep_node_as_list(To),
		    TimeStamp = now(),
		    Expire = find_x_expire(TimeStamp, Packet#xmlel.children),
		    gen_mod:get_module_proc(exmpp_jid:prep_domain_as_list(To), ?PROCNAME) !
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

check_event(From, To, Packet) ->
    case find_x_event(Packet#xmlel.children) of
	false ->
	    true;
	El ->
	    case exmpp_xml:get_element(El, 'id') of
		undefined ->
		    case exmpp_xml:get_element(El, 'offline') of
			undefined ->
			    true;
			_ ->
			    ID = case exmpp_xml:get_attribute_as_list(Packet, 'id', "") of
				     "" ->
					 #xmlel{ns = ?NS_MESSAGE_EVENT, name = 'id'};
				     S ->
					 #xmlel{ns = ?NS_MESSAGE_EVENT, name = 'id',
					  children = [#xmlcdata{cdata = list_to_binary(S)}]}
				 end,
			    X = #xmlel{ns = ?NS_MESSAGE_EVENT, name = 'x', children =
			      [ID, #xmlel{ns = ?NS_MESSAGE_EVENT, name = 'offline'}]},
			    ejabberd_router:route(
			      To, From, exmpp_xml:set_children(Packet, [X])),
			    true
			end;
		_ ->
		    false
	    end
    end.

find_x_event([]) ->
    false;
find_x_event([#xmlel{ns = ?NS_MESSAGE_EVENT} = El | _Els]) ->
    El;
find_x_event([_ | Els]) ->
    find_x_event(Els).

find_x_expire(_, []) ->
    never;
find_x_expire(TimeStamp, [#xmlel{ns = ?NS_MESSAGE_EXPIRE} = El | _Els]) ->
    Val = exmpp_xml:get_attribute_as_list(El, 'seconds', ""),
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
find_x_expire(TimeStamp, [_ | Els]) ->
    find_x_expire(TimeStamp, Els).


pop_offline_messages(Ls, User, Server) 
        when is_binary(User), is_binary(Server) ->
    try
	LUser = binary_to_list(User),
	LServer = binary_to_list(Server),
	EUser = ejabberd_odbc:escape(LUser),
	case odbc_queries:get_and_del_spool_msg_t(LServer, EUser) of
	    {atomic, {selected, ["username","xml"], Rs}} ->
		Ls ++ lists:flatmap(
			fun({_, XML}) ->
				try
				    [El] = exmpp_xml:parse_document(XML, 
                         [names_as_atom, {check_elems, xmpp}, 
                          {check_nss,xmpp}, {check_attrs,xmpp}]),
				    To = exmpp_jid:parse(
				      exmpp_stanza:get_recipient(El)),
				    From = exmpp_jid:parse(
				      exmpp_stanza:get_sender(El)),
				    [{route, From, To, El}]
				catch
				    _ ->
					[]
				end
			end, Rs);
	    _ ->
		Ls
	end
    catch
	_ ->
	    []
    end.


remove_user(User, Server) 
        when is_binary(User), is_binary(Server) ->
    try
	LUser = binary_to_list(exmpp_stringprep:nodeprep(User)),
	LServer = binary_to_list(exmpp_stringprep:nameprep(Server)),
	Username = ejabberd_odbc:escape(LUser),
	odbc_queries:del_spool_msg(LServer, Username)
    catch
	_ ->
	    ok
    end.


%% Helper functions:

%% TODO: Warning - This function is a duplicate from mod_offline.erl
%% It is duplicate to stay consistent (many functions are duplicated
%% in this module). It will be refactored later on.
%% Warn senders that their messages have been discarded:
discard_warn_sender(Msgs) ->
    lists:foreach(
      fun(#offline_msg{from=From, to=To, packet=Packet}) ->
	      ErrText = "Your contact offline message queue is full. The message has been discarded.",
	      Error = exmpp_stanza:error('resource-constraint',
		{"en", ErrText}),
	      Err = exmpp_stanza:reply_with_error(Packet, Error),
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
    {US, Msgs, Res} = try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	Username = ejabberd_odbc:escape(LUser),
	US0 = {LUser, LServer},
	R = user_queue_parse_query(Username, LServer, Query),
	M = case catch ejabberd_odbc:sql_query(
			    LServer,
			    ["select username, xml from spool"
			     "  where username='", Username, "'"
			     "  order by seq;"]) of
		   {selected, ["username", "xml"], Rs} ->
		       lists:flatmap(
			 fun({_, XML}) ->
				 try exmpp_xml:parse_document(XML, 
                         [names_as_atom, {check_elems, xmpp}, 
                          {check_nss,xmpp}, {check_attrs,xmpp}]) of
				     [El] ->
					 [El]
				 catch
				     _ ->
					 []
				 end
			 end, Rs);
		   _ ->
		       []
	       end,
	{US0, M, R}
    catch
	_ ->
	    {{"invalid", "invalid"}, [], nothing}
    end,
    FMsgs =
	lists:map(
	  fun(#xmlel{} = Msg) ->
		  ID = jlib:encode_base64(binary_to_list(term_to_binary(Msg))),
		  Packet = Msg,
		  FPacket = exmpp_xml:node_to_list(
		    exmpp_xml:indent_document(Packet, <<"  ">>),
		    [?DEFAULT_NS], ?PREFIXED_NS),
		  ?XE("tr",
		      [?XAE("td", [?XMLATTR('class', <<"valign">>)], [?INPUT("checkbox", "selected", ID)]),
		       ?XAE("td", [?XMLATTR('class', <<"valign">>)], [?XC("pre", FPacket)])]
		     )
	  end, Msgs),
    [?XC("h1", io_lib:format(?T("~s's Offline Messages Queue"),
			     [us_to_list(US)]))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    nothing -> []
	end ++
	[?XAE("form", [?XMLATTR('action', <<"">>), ?XMLATTR('method', <<"post">>)],
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
				     [?XAC("td", [?XMLATTR('colspan', <<"4">>)], " ")]
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
				     try exmpp_xml:parse_document(XML, 
                         [names_as_atom, {check_elems, xmpp}, 
                          {check_nss,xmpp}, {check_attrs,xmpp}]) of
					 [El] ->
					     [{El, Seq}]
				     catch
					 _ ->
					     []
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
    exmpp_jid:to_list(User, Server).

webadmin_user(Acc, User, Server, Lang) ->
    FQueueLen = try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	Username = ejabberd_odbc:escape(LUser),
	QueueLen = case catch ejabberd_odbc:sql_query(
				LServer,
				["select count(*) from spool"
				 "  where username='", Username, "';"]) of
		       {selected, [_], [{SCount}]} ->
			   SCount;
		       _ ->
			   0
		   end,
	[?AC("queue/", QueueLen)]
    catch
	_ ->
	    [?C("?")]
    end,
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
