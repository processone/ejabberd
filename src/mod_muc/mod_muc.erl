%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : MUC support (JEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_muc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1,
	 init/1,
	 stop/0,
	 room_destroyed/1,
	 store_room/2,
	 restore_room/1,
	 forget_room/1,
	 process_iq_disco_items/5,
	 can_use_nick/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-record(muc_room, {name, opts}).
-record(muc_online_room, {name, pid}).
-record(muc_registered, {user, nick}).


start(Opts) ->
    mnesia:create_table(muc_room,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, muc_room)}]),
    mnesia:create_table(muc_registered,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, muc_registered)}]),
    mnesia:add_table_index(muc_registered, nick),
    Host = gen_mod:get_opt(host, Opts, "conference." ++ ?MYNAME),
    register(ejabberd_mod_muc, spawn(?MODULE, init, [Host])).



init(Host) ->
    catch ets:new(muc_online_room, [named_table,
				    public,
				    {keypos, #muc_online_room.name}]),
    ejabberd_router:register_route(Host),
    load_permanent_rooms(Host),
    loop(Host).

loop(Host) ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(Host, From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop(Host);
	{room_destroyed, Room} ->
	    ets:delete(muc_online_room, Room);
	stop ->
	    % TODO
	    ejabberd_router:unregister_global_route(Host),
	    ok;
	_ ->
	    loop(Host)
    end.


do_route(Host, From, To, Packet) ->
    {Room, _, Nick} = To,
    {xmlelement, Name, Attrs, Els} = Packet,
    case Room of
	"" ->
	    case Nick of
		"" ->
		    case Name of
			"iq" ->
			    case jlib:iq_query_info(Packet) of
				{iq, ID, get, ?NS_DISCO_INFO = XMLNS, SubEl} ->
				    Res = {iq, ID, result, XMLNS,
					   [{xmlelement, "query",
					     [{"xmlns", XMLNS}],
					     iq_disco_info()}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				{iq, ID, get, ?NS_DISCO_ITEMS = XMLNS, SubEl} ->
				    spawn(?MODULE,
					  process_iq_disco_items,
					  [Host, From, To, ID, SubEl]);
				{iq, ID, get, ?NS_REGISTER = XMLNS, SubEl} ->
				    Lang = xml:get_tag_attr_s(
					     "xml:lang", SubEl),
				    Res = {iq, ID, result, XMLNS,
					   [{xmlelement, "query",
					     [{"xmlns", XMLNS}],
					     iq_get_register_info(
					       From, Lang)}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				{iq, ID, set, ?NS_REGISTER = XMLNS, SubEl} ->
				    case process_iq_register_set(From, SubEl) of
					{result, IQRes} ->
					    Res = {iq, ID, result, XMLNS,
						   [{xmlelement, "query",
						     [{"xmlns", XMLNS}],
						     IQRes}]},
					    ejabberd_router:route(
					      To, From, jlib:iq_to_xml(Res));
					{error, Error} ->
					    Err = jlib:make_error_reply(
						    Packet, Error),
					    ejabberd_router:route(
					      To, From, Err)
				    end;
				{iq, ID, get, ?NS_VCARD = XMLNS, SubEl} ->
				    Lang = xml:get_tag_attr_s(
					     "xml:lang", SubEl),
				    Res = {iq, ID, result, XMLNS,
					   [{xmlelement, "query",
					     [{"xmlns", XMLNS}],
					     iq_get_vcard(Lang)}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				reply ->
				    ok;
				_ ->
				    Err = jlib:make_error_reply(
					    Packet,
					    ?ERR_FEATURE_NOT_IMPLEMENTED),
				    ejabberd_router:route(To, From, Err)
			    end;
			"message" ->
			    case xml:get_attr_s("type", Attrs) of
				"error" ->
				    ok;
				_ ->
				    case acl:match_rule(muc_admin, From) of
					allow ->
					    Msg = xml:get_path_s(
						    Packet,
						    [{elem, "body"}, cdata]),
					    broadcast_service_message(Msg);
					_ ->
					    Err = jlib:make_error_reply(
						    Packet,
						    ?ERR_NOT_ALLOWED),
					    ejabberd_router:route(
					      To, From, Err)
				    end
			    end;
			"presence" ->
			    ok
		    end;
		_ ->
		    case xml:get_attr_s("type", Attrs) of
			"error" ->
			    ok;
			"result" ->
			    ok;
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_JID_NOT_FOUND),
			    ejabberd_router:route(To, From, Err)
		    end
	    end;
	_ ->
	    case ets:lookup(muc_online_room, Room) of
		[] ->
		    Type = xml:get_attr_s("type", Attrs),
		    case {Name, Type} of
			{"presence", ""} ->
			    io:format("MUC: open new room '~s'~n", [Room]),
			    {ok, Pid} = mod_muc_room:start(
					  Host, Room, From, Nick),
			    ets:insert(
			      muc_online_room,
			      #muc_online_room{name = Room, pid = Pid}),
			    mod_muc_room:route(Pid, From, Nick, Packet),
			    ok;
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err)
		    end;
		[R] ->
		    Pid = R#muc_online_room.pid,
		    io:format("MUC: send to process ~p~n", [Pid]),
		    mod_muc_room:route(Pid, From, Nick, Packet),
		    ok
	    end
    end.




room_destroyed(Room) ->
    ejabberd_mod_muc ! {room_destroyed, Room},
    ok.

stop() ->
    ejabberd_mod_muc ! stop,
    ok.


store_room(Name, Opts) ->
    F = fun() ->
		mnesia:write(#muc_room{name = Name,
				       opts = Opts})
	end,
    mnesia:transaction(F).

restore_room(Name) ->
    case catch mnesia:dirty_read(muc_room, Name) of
	[#muc_room{opts = Opts}] ->
	    Opts;
	_ ->
	    error
    end.


forget_room(Name) ->
    F = fun() ->
		mnesia:delete({muc_room, Name})
	end,
    mnesia:transaction(F).


load_permanent_rooms(Host) ->
    case catch mnesia:dirty_select(muc_room, [{'_', [], ['$_']}]) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    ok;
	Rs ->
	    lists:foreach(fun(R) ->
				  Room = R#muc_room.name,
				  {ok, Pid} = mod_muc_room:start(
						Host,
						Room,
						R#muc_room.opts),
				  ets:insert(
				    muc_online_room,
				    #muc_online_room{name = Room, pid = Pid})
			  end, Rs)
    end.


iq_disco_info() ->
    [{xmlelement, "identity",
      [{"category", "conference"},
       {"type", "text"},
       {"name", "ejabberd/mod_muc"}], []},
     {xmlelement, "feature", [{"var", ?NS_MUC}], []},
     {xmlelement, "feature", [{"var", ?NS_REGISTER}], []},
     {xmlelement, "feature", [{"var", ?NS_VCARD}], []}].


process_iq_disco_items(Host, From, To, ID, SubEl) ->
    Res = {iq, ID, result, ?NS_DISCO_ITEMS,
	   [{xmlelement, "query",
	     [{"xmlns", ?NS_DISCO_ITEMS}],
	     iq_disco_items(Host, From)}]},
    ejabberd_router:route(To,
			  From,
			  jlib:iq_to_xml(Res)).

iq_disco_items(Host, From) ->
    lists:zf(fun(#muc_online_room{name = Name, pid = Pid}) ->
		     case catch gen_fsm:sync_send_all_state_event(
				  Pid, {get_disco_item, From}, 100) of
			 {item, Desc} ->
			     {true,
			      {xmlelement, "item",
			       [{"jid", jlib:jid_to_string({Name, Host, ""})},
				{"name", Desc}], []}};
			 _ ->
			     false
		     end
	     end, ets:tab2list(muc_online_room)).


-define(XFIELD(Type, Label, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

iq_get_register_info(From, Lang) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    Nick = case catch mnesia:dirty_read(muc_registered, LUS) of
	       {'EXIT', Reason} ->
		   "";
	       [] ->
		   "";
	       [#muc_registered{nick = N}] ->
		   N
	   end,
    [{xmlelement, "instructions", [],
      [{xmlcdata, translate:translate(
		    Lang, "You need a x:data capable client to register.")}]},
     {xmlelement, "x",
      [{"xmlns", ?NS_XDATA}],
      [{xmlelement, "title", [],
	[{xmlcdata,
	  translate:translate(
	    Lang, "Nick Registration")}]},
       {xmlelement, "instructions", [],
	[{xmlcdata,
	  translate:translate(
	    Lang, "Enter nick you want to register.")}]},
       ?XFIELD("text-single", "Nick", "nick", Nick)]}].

iq_set_register_info(From, XData) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    case lists:keysearch("nick", 1, XData) of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	{value, {_, [Nick]}} ->
	    F = fun() ->
			case Nick of
			    "" ->
				mnesia:delete({muc_registered, LUS}),
				ok;
			    _ ->
				Allow = case mnesia:index_read(
					       muc_registered,
					       Nick,
					       #muc_registered.nick) of
					    [] ->
						true;
					    [#muc_registered{user = U}] ->
						U == LUS
					end,
				if
				    Allow ->
					mnesia:write(
					  #muc_registered{user = LUS,
							  nick = Nick}),
					ok;
				    true ->
					false
				end
			end
		end,
	    case mnesia:transaction(F) of
		{atomic, ok} ->
		    {result, []};
		{atomic, false} ->
		    {error, ?ERR_NOT_ALLOWED};
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end
    end.

process_iq_register_set(From, SubEl) ->
    {xmlelement, Name, Attrs, Els} = SubEl,
    case xml:remove_cdata(Els) of
	[{xmlelement, "x", Attrs1, Els1} = XEl] ->
	    case {xml:get_tag_attr_s("xmlns", XEl),
		  xml:get_tag_attr_s("type", XEl)} of
		{?NS_XDATA, "cancel"} ->
		    {result, []};
		{?NS_XDATA, "submit"} ->
		    XData = jlib:parse_xdata_submit(XEl),
		    case XData of
			invalid ->
			    {error, ?ERR_BAD_REQUEST};
			_ ->
			    iq_set_register_info(From, XData)
		    end;
		_ ->
		    {error, ?ERR_BAD_REQUEST}
	    end;
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end.

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_muc"}]},
     {xmlelement, "URL", [],
      [{xmlcdata,
	"http://ejabberd.jabberstudio.org/"}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, "ejabberd MUC module\n"
	"Copyright (c) 2003 Alexey Shchepin"}]}].


broadcast_service_message(Msg) ->
    lists:foreach(
      fun(#muc_online_room{name = Name, pid = Pid}) ->
	      gen_fsm:send_all_state_event(
		Pid, {service_message, Msg})
      end, ets:tab2list(muc_online_room)).



can_use_nick(JID, "") ->
    false;
can_use_nick(JID, Nick) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    LUS = {LUser, LServer},
    case catch mnesia:dirty_index_read(muc_registered,
				       Nick,
				       #muc_registered.nick) of
	{'EXIT', Reason} ->
	    true;
	[] ->
	    true;
	[#muc_registered{user = U}] ->
	    U == LUS
    end.



