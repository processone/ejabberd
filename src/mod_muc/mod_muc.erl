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

-export([start/2,
	 init/3,
	 stop/1,
	 room_destroyed/3,
	 store_room/3,
	 restore_room/2,
	 forget_room/2,
	 process_iq_disco_items/4,
	 can_use_nick/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-record(muc_room, {name_host, opts}).
-record(muc_online_room, {name_host, pid}).
-record(muc_registered, {us_host, nick}).

-define(PROCNAME, ejabberd_mod_muc).

start(Host, Opts) ->
    mnesia:create_table(muc_room,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, muc_room)}]),
    mnesia:create_table(muc_registered,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, muc_registered)}]),
    MyHost = gen_mod:get_opt(host, Opts, "conference." ++ Host),
    update_tables(MyHost),
    mnesia:add_table_index(muc_registered, nick),
    Access = gen_mod:get_opt(access, Opts, all),
    AccessCreate = gen_mod:get_opt(access_create, Opts, all),
    AccessAdmin = gen_mod:get_opt(access_admin, Opts, none),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init,
		   [MyHost, Host, {Access, AccessCreate, AccessAdmin}])).



init(Host, ServerHost, Access) ->
    catch ets:new(muc_online_room, [named_table,
				    public,
				    {keypos, #muc_online_room.name_host}]),
    ejabberd_router:register_route(Host),
    load_permanent_rooms(Host, ServerHost, Access),
    loop(Host, ServerHost, Access).

loop(Host, ServerHost, Access) ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(Host, ServerHost, Access, From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop(Host, ServerHost, Access);
	{room_destroyed, RoomHost} ->
	    ets:delete(muc_online_room, RoomHost),
	    loop(Host, ServerHost, Access);
	stop ->
	    ejabberd_router:unregister_route(Host),
	    ok;
	_ ->
	    loop(Host, ServerHost, Access)
    end.


do_route(Host, ServerHost, Access, From, To, Packet) ->
    {AccessRoute, _AccessCreate, _AccessAdmin} = Access,
    case acl:match_rule(ServerHost, AccessRoute, From) of
	allow ->
	    do_route1(Host, ServerHost, Access, From, To, Packet);
	_ ->
	    {xmlelement, _Name, Attrs, _Els} = Packet,
	    Lang = xml:get_attr_s("xml:lang", Attrs),
	    ErrText = "Access denied by service policy",
	    Err = jlib:make_error_reply(Packet,
					?ERRT_FORBIDDEN(Lang, ErrText)),
	    ejabberd_router:route(To, From, Err)
    end.


do_route1(Host, ServerHost, Access, From, To, Packet) ->
    {_AccessRoute, AccessCreate, AccessAdmin} = Access,
    {Room, _, Nick} = jlib:jid_tolower(To),
    {xmlelement, Name, Attrs, _Els} = Packet,
    case Room of
	"" ->
	    case Nick of
		"" ->
		    case Name of
			"iq" ->
			    case jlib:iq_query_info(Packet) of
				#iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS,
				    sub_el = SubEl} = IQ ->
				    Res = IQ#iq{type = result,
						sub_el = [{xmlelement, "query",
							   [{"xmlns", XMLNS}],
							   iq_disco_info()}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				#iq{type = get,
				    xmlns = ?NS_DISCO_ITEMS} = IQ ->
				    spawn(?MODULE,
					  process_iq_disco_items,
					  [Host, From, To, IQ]);
				#iq{type = get,
				    xmlns = ?NS_REGISTER = XMLNS,
				    lang = Lang,
				    sub_el = SubEl} = IQ ->
				    Res = IQ#iq{type = result,
						sub_el =
						[{xmlelement, "query",
						  [{"xmlns", XMLNS}],
						  iq_get_register_info(
						    Host, From, Lang)}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				#iq{type = set,
				    xmlns = ?NS_REGISTER = XMLNS,
				    lang = Lang,
				    sub_el = SubEl} = IQ ->
				    case process_iq_register_set(Host, From, SubEl, Lang) of
					{result, IQRes} ->
					    Res = IQ#iq{type = result,
							sub_el =
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
				#iq{type = get,
				    xmlns = ?NS_VCARD = XMLNS,
				    lang = Lang,
				    sub_el = SubEl} = IQ ->
				    Res = IQ#iq{type = result,
						sub_el =
						[{xmlelement, "vCard",
						  [{"xmlns", XMLNS}],
						  iq_get_vcard(Lang)}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				#iq{} ->
				    Err = jlib:make_error_reply(
					    Packet,
					    ?ERR_FEATURE_NOT_IMPLEMENTED),
				    ejabberd_router:route(To, From, Err);
				_ ->
				    ok
			    end;
			"message" ->
			    case xml:get_attr_s("type", Attrs) of
				"error" ->
				    ok;
				_ ->
				    case acl:match_rule(ServerHost, AccessAdmin, From) of
					allow ->
					    Msg = xml:get_path_s(
						    Packet,
						    [{elem, "body"}, cdata]),
					    broadcast_service_message(Host, Msg);
					_ ->
					    Lang = xml:get_attr_s("xml:lang", Attrs),
					    ErrText = "Only service administrators "
						      "are allowed to send service messages",
					    Err = jlib:make_error_reply(
						    Packet,
						    ?ERRT_FORBIDDEN(Lang, ErrText)),
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
				    Packet, ?ERR_ITEM_NOT_FOUND),
			    ejabberd_router:route(To, From, Err)
		    end
	    end;
	_ ->
	    case ets:lookup(muc_online_room, {Room, Host}) of
		[] ->
		    Type = xml:get_attr_s("type", Attrs),
		    case {Name, Type} of
			{"presence", ""} ->
			    case acl:match_rule(ServerHost, AccessCreate, From) of
				allow ->
				    ?DEBUG("MUC: open new room '~s'~n", [Room]),
				    {ok, Pid} = mod_muc_room:start(
						  Host, ServerHost, Access,
						  Room, From, Nick),
				    ets:insert(
				      muc_online_room,
				      #muc_online_room{name_host = {Room, Host},
						       pid = Pid}),
				    mod_muc_room:route(Pid, From, Nick, Packet),
				    ok;
				_ ->
				    Lang = xml:get_attr_s("xml:lang", Attrs),
				    ErrText = "Room creation is denied by service policy",
				    Err = jlib:make_error_reply(
					    Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
				    ejabberd_router:route(To, From, Err)
			    end;
			_ ->
			    Lang = xml:get_attr_s("xml:lang", Attrs),
			    ErrText = "Conference room does not exist",
			    Err = jlib:make_error_reply(
				    Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
			    ejabberd_router:route(To, From, Err)
		    end;
		[R] ->
		    Pid = R#muc_online_room.pid,
		    ?DEBUG("MUC: send to process ~p~n", [Pid]),
		    mod_muc_room:route(Pid, From, Nick, Packet),
		    ok
	    end
    end.




room_destroyed(Host, Room, ServerHost) ->
    gen_mod:get_module_proc(ServerHost, ?PROCNAME) !
	{room_destroyed, {Room, Host}},
    ok.

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! stop,
    {wait, Proc}.


store_room(Host, Name, Opts) ->
    F = fun() ->
		mnesia:write(#muc_room{name_host = {Name, Host},
				       opts = Opts})
	end,
    mnesia:transaction(F).

restore_room(Host, Name) ->
    case catch mnesia:dirty_read(muc_room, {Name, Host}) of
	[#muc_room{opts = Opts}] ->
	    Opts;
	_ ->
	    error
    end.


forget_room(Host, Name) ->
    F = fun() ->
		mnesia:delete({muc_room, {Name, Host}})
	end,
    mnesia:transaction(F).


load_permanent_rooms(Host, ServerHost, Access) ->
    case catch mnesia:dirty_select(
		 muc_room, [{#muc_room{name_host = {'_', Host}, _ = '_'},
			     [],
			     ['$_']}]) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    ok;
	Rs ->
	    lists:foreach(fun(R) ->
				  {Room, Host} = R#muc_room.name_host,
				  {ok, Pid} = mod_muc_room:start(
						Host,
						ServerHost,
						Access,
						Room,
						R#muc_room.opts),
				  ets:insert(
				    muc_online_room,
				    #muc_online_room{name_host = {Room, Host},
						     pid = Pid})
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


process_iq_disco_items(Host, From, To, #iq{lang = Lang} = IQ) ->
    Res = IQ#iq{type = result,
		sub_el = [{xmlelement, "query",
			   [{"xmlns", ?NS_DISCO_ITEMS}],
			   iq_disco_items(Host, From, Lang)}]},
    ejabberd_router:route(To,
			  From,
			  jlib:iq_to_xml(Res)).

iq_disco_items(Host, From, Lang) ->
    lists:zf(fun(#muc_online_room{name_host = {Name, _Host}, pid = Pid}) ->
		     case catch gen_fsm:sync_send_all_state_event(
				  Pid, {get_disco_item, From, Lang}, 100) of
			 {item, Desc} ->
			     {true,
			      {xmlelement, "item",
			       [{"jid", jlib:jid_to_string({Name, Host, ""})},
				{"name", Desc}], []}};
			 _ ->
			     false
		     end
	     end, get_vh_rooms(Host)).


-define(XFIELD(Type, Label, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

iq_get_register_info(Host, From, Lang) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    {Nick, Registered} =
	case catch mnesia:dirty_read(muc_registered, {LUS, Host}) of
	    {'EXIT', _Reason} ->
		{"", []};
	    [] ->
		{"", []};
	    [#muc_registered{nick = N}] ->
		{N, [{xmlelement, "registered", [], []}]}
	end,
    Registered ++
	[{xmlelement, "instructions", [],
	  [{xmlcdata,
	    translate:translate(
	      Lang, "You need an x:data capable client to register nickname")}]},
	 {xmlelement, "x",
	  [{"xmlns", ?NS_XDATA}],
	  [{xmlelement, "title", [],
	    [{xmlcdata,
	      translate:translate(
		Lang, "Nickname Registration at ") ++ Host}]},
	   {xmlelement, "instructions", [],
	    [{xmlcdata,
	      translate:translate(
		Lang, "Enter nickname you want to register")}]},
	   ?XFIELD("text-single", "Nickname", "nick", Nick)]}].

iq_set_register_info(Host, From, XData, Lang) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    case lists:keysearch("nick", 1, XData) of
	false ->
	    ErrText = "You must fill in field \"nick\" in the form",
	    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
	{value, {_, [Nick]}} ->
	    F = fun() ->
			case Nick of
			    "" ->
				mnesia:delete({muc_registered, {LUS, Host}}),
				ok;
			    _ ->
				Allow =
				    case mnesia:select(
					   muc_registered,
					   [{#muc_registered{us_host = '$1',
							     nick = Nick,
							     _ = '_'},
					     [{'==', {element, 2, '$1'}, Host}],
					     ['$_']}]) of
					[] ->
					    true;
					[#muc_registered{us_host = {U, _Host}}] ->
					    U == LUS
				    end,
				if
				    Allow ->
					mnesia:write(
					  #muc_registered{us_host = {LUS, Host},
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
		    ErrText = "Specified nickname is already registered",
		    {error, ?ERRT_CONFLICT(Lang, ErrText)};
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end
    end.

process_iq_register_set(Host, From, SubEl, Lang) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    case xml:remove_cdata(Els) of
	[{xmlelement, "x", _Attrs1, _Els1} = XEl] ->
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
			    iq_set_register_info(Host, From, XData, Lang)
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
      [{xmlcdata, translate:translate(Lang, "ejabberd MUC module\n"
	"Copyright (c) 2003-2005 Alexey Shchepin")}]}].


broadcast_service_message(Host, Msg) ->
    lists:foreach(
      fun(#muc_online_room{pid = Pid}) ->
	      gen_fsm:send_all_state_event(
		Pid, {service_message, Msg})
      end, get_vh_rooms(Host)).

get_vh_rooms(Host) ->
    ets:select(muc_online_room,
	       [{#muc_online_room{name_host = '$1', _ = '_'},
		 [{'==', {element, 2, '$1'}, Host}],
		 ['$_']}]).


can_use_nick(_Host, _JID, "") ->
    false;
can_use_nick(Host, JID, Nick) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    LUS = {LUser, LServer},
    case catch mnesia:dirty_select(
		 muc_registered,
		 [{#muc_registered{us_host = '$1',
				   nick = Nick,
				   _ = '_'},
		   [{'==', {element, 2, '$1'}, Host}],
		   ['$_']}]) of
	{'EXIT', _Reason} ->
	    true;
	[] ->
	    true;
	[#muc_registered{us_host = {U, _Host}}] ->
	    U == LUS
    end.


update_tables(Host) ->
    update_muc_room_table(Host),
    update_muc_registered_table(Host).

update_muc_room_table(Host) ->
    Fields = record_info(fields, muc_room),
    case mnesia:table_info(muc_room, attributes) of
	Fields ->
	    ok;
	[name, opts] ->
	    ?INFO_MSG("Converting muc_room table from "
		      "{name, opts} format", []),
	    {atomic, ok} = mnesia:create_table(
			     mod_muc_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, muc_room},
			      {attributes, record_info(fields, muc_room)}]),
	    mnesia:transform_table(muc_room, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_muc_tmp_table),
			 mnesia:foldl(
			   fun(#muc_room{name_host = Name} = R, _) ->
				   mnesia:dirty_write(
				     mod_muc_tmp_table,
				     R#muc_room{name_host = {Name, Host}})
			   end, ok, muc_room)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(muc_room),
	    F2 = fun() ->
			 mnesia:write_lock_table(muc_room),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_muc_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_muc_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating muc_room table", []),
	    mnesia:transform_table(muc_room, ignore, Fields)
    end.


update_muc_registered_table(Host) ->
    Fields = record_info(fields, muc_registered),
    case mnesia:table_info(muc_registered, attributes) of
	Fields ->
	    ok;
	[user, nick] ->
	    ?INFO_MSG("Converting muc_registered table from "
		      "{user, nick} format", []),
	    {atomic, ok} = mnesia:create_table(
			     mod_muc_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, muc_registered},
			      {attributes, record_info(fields, muc_registered)}]),
	    mnesia:del_table_index(muc_registered, nick),
	    mnesia:transform_table(muc_registered, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_muc_tmp_table),
			 mnesia:foldl(
			   fun(#muc_registered{us_host = US} = R, _) ->
				   mnesia:dirty_write(
				     mod_muc_tmp_table,
				     R#muc_registered{us_host = {US, Host}})
			   end, ok, muc_registered)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(muc_registered),
	    F2 = fun() ->
			 mnesia:write_lock_table(muc_registered),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_muc_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_muc_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating muc_registered table", []),
	    mnesia:transform_table(muc_registered, ignore, Fields)
    end.
