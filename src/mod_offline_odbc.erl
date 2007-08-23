%%%----------------------------------------------------------------------
%%% File    : mod_offline_odbc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%%----------------------------------------------------------------------

-module(mod_offline_odbc).
-author('alexey@sevcom.net').

-behaviour(gen_mod).

-export([start/2,
	 init/1,
	 stop/1,
	 store_packet/3,
	 pop_offline_messages/3,
	 remove_user/2,
	 webadmin_page/3,
	 webadmin_user/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-record(offline_msg, {user, timestamp, expire, from, to, packet}).

-define(PROCNAME, ejabberd_offline).
-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

start(Host, _Opts) ->
    ejabberd_hooks:add(offline_message_hook, Host,
		       ?MODULE, store_packet, 50),
    ejabberd_hooks:add(resend_offline_messages_hook, Host,
		       ?MODULE, pop_offline_messages, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [Host])).

init(Host) ->
    loop(Host).

loop(Host) ->
    receive
	#offline_msg{} = Msg ->
	    Msgs = receive_all([Msg]),
	    % TODO
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
					     M#offline_msg.timestamp))]},
			      XML =
				  ejabberd_odbc:escape(
				    lists:flatten(
				      xml:element_to_string(Packet))),
			      odbc_queries:add_spool_sql(Username, XML)
		      end, Msgs),
	    case catch odbc_queries:add_spool(Host, Query) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p~n", [Reason]);
		_ ->
		    ok
	    end,
	    loop(Host);
	_ ->
	    loop(Host)
    end.

receive_all(Msgs) ->
    receive
	#offline_msg{} = Msg ->
	    receive_all([Msg | Msgs])
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
    ejabberd_hooks:delete(webadmin_page_host, Host,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host,
			  ?MODULE, webadmin_user, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    exit(whereis(Proc), stop),
    ok.

store_packet(From, To, Packet) ->
    Type = xml:get_tag_attr_s("type", Packet),
    if
	(Type /= "error") and (Type /= "groupchat") and
	(Type /= "headline") ->
	    case check_event(From, To, Packet) of
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
    Msgs = case catch ejabberd_odbc:sql_query(
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
    FMsgs =
	lists:map(
	  fun({xmlelement, Name, Attrs, Els} = Msg) ->
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
	    ok -> [?CT("Submitted"), ?P];
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

webadmin_user(Acc, User, Server, Lang) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
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
    FQueueLen = [?AC("queue/", QueueLen)],
    Acc ++ [?XCT("h3", "Offline Messages:")] ++ FQueueLen.
