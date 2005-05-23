%%%----------------------------------------------------------------------
%%% File    : mod_announce.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Manage announce messages
%%% Created : 11 Aug 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_announce).
-author('alexey@sevcom.net').

-behaviour(gen_mod).

-export([start/1,
	 init/0,
	 stop/0,
	 announce/3,
	 send_motd/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(motd, {server, packet}).
-record(motd_users, {us, dummy = []}).

-define(PROCNAME, ejabberd_announce).

start(_) ->
    mnesia:create_table(motd, [{disc_copies, [node()]},
			       {attributes, record_info(fields, motd)}]),
    mnesia:create_table(motd_users, [{disc_copies, [node()]},
				     {attributes, record_info(fields, motd_users)}]),
    update_tables(),
    ejabberd_hooks:add(local_send_to_resource_hook,
		       ?MODULE, announce, 50),
    ejabberd_hooks:add(user_available_hook,
		       ?MODULE, send_motd, 50),
    register(?PROCNAME, proc_lib:spawn(?MODULE, init, [])).

init() ->
    loop().

loop() ->
    receive
	{announce_all, From, To, Packet} ->
	    announce_all(From, To, Packet),
	    loop();
	{announce_online, From, To, Packet} ->
	    announce_online(From, To, Packet),
	    loop();
	{announce_all_hosts_online, From, To, Packet} ->
	    announce_all_hosts_online(From, To, Packet),
	    loop();
	{announce_motd, From, To, Packet} ->
	    announce_motd(From, To, Packet),
	    loop();
	{announce_motd_update, From, To, Packet} ->
	    announce_motd_update(From, To, Packet),
	    loop();
	{announce_motd_delete, From, To, Packet} ->
	    announce_motd_delete(From, To, Packet),
	    loop();
	_ ->
	    loop()
    end.

stop() ->
    ejabberd_hooks:delete(local_send_to_resource_hook,
			  ?MODULE, announce, 50),
    ejabberd_hooks:delete(sm_register_connection_hook,
			  ?MODULE, send_motd, 50),
    exit(whereis(?PROCNAME), stop),
    {wait, ?PROCNAME}.

announce(From, To, Packet) ->
    case To of
	#jid{luser = "", lresource = Res} ->
	    {xmlelement, Name, _Attrs, _Els} = Packet,
		case {Res, Name} of
		    {"announce/all", "message"} ->
			?PROCNAME ! {announce_all, From, To, Packet},
			stop;
		    {"announce/online", "message"} ->
			?PROCNAME ! {announce_online, From, To, Packet},
			stop;
		    {"announce/all-hosts/online", "message"} ->
			?PROCNAME ! {announce_all_hosts_online, From, To, Packet},
			stop;
		    {"announce/motd", "message"} ->
			?PROCNAME ! {announce_motd, From, To, Packet},
			stop;
		    {"announce/motd/update", "message"} ->
			?PROCNAME ! {announce_motd_update, From, To, Packet},
			stop;
		    {"announce/motd/delete", "message"} ->
			?PROCNAME ! {announce_motd_delete, From, To, Packet},
			stop;
		    _ ->
			ok
	    end;
	_ ->
	    ok
    end.

announce_all(From, To, Packet) ->
    Access = gen_mod:get_module_opt(?MODULE, access, none),
    case acl:match_rule(Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Local = jlib:make_jid("", To#jid.server, ""),
	    lists:foreach(
		fun({User, Server}) ->
			Dest = jlib:make_jid(User, Server, ""),
			ejabberd_router:route(Local, Dest, Packet)
		end, ejabberd_auth:get_vh_registered_users(To#jid.lserver))
    end.

announce_online(From, To, Packet) ->
    Access = gen_mod:get_module_opt(?MODULE, access, none),
    case acl:match_rule(Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_online1(ejabberd_sm:get_vh_session_list(To#jid.lserver),
			     To#jid.server,
			     Packet)
    end.

announce_all_hosts_online(From, To, Packet) ->
    Access = gen_mod:get_module_opt(?MODULE, access, none),
    case acl:match_rule(Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_online1(ejabberd_sm:dirty_get_sessions_list(),
			     To#jid.server,
			     Packet)
    end.

announce_online1(Sessions, Server, Packet) ->
    Local = jlib:make_jid("", Server, ""),
    lists:foreach(
      fun({U, S, R}) ->
	      Dest = jlib:make_jid(U, S, R),
	      ejabberd_router:route(Local, Dest, Packet)
      end, Sessions).

announce_motd(From, To, Packet) ->
    Access = gen_mod:get_module_opt(?MODULE, access, none),
    case acl:match_rule(Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_update(To#jid.lserver, Packet),
	    Sessions = ejabberd_sm:get_vh_session_list(To#jid.lserver),
	    announce_online1(Sessions, To#jid.server, Packet),
	    F = fun() ->
			lists:foreach(
			  fun({U, S, _R}) ->
				  mnesia:write(#motd_users{us = {U, S}})
			  end, Sessions)
		end,
	    mnesia:transaction(F)
    end.

announce_motd_update(From, To, Packet) ->
    Access = gen_mod:get_module_opt(?MODULE, access, none),
    case acl:match_rule(Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_update(To#jid.lserver, Packet)
    end.

announce_motd_update(LServer, Packet) ->
    announce_motd_delete(LServer),
    F = fun() ->
		mnesia:write(#motd{server = LServer, packet = Packet})
	end,
    mnesia:transaction(F).

announce_motd_delete(From, To, Packet) ->
    Access = gen_mod:get_module_opt(?MODULE, access, none),
    case acl:match_rule(Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_delete(To#jid.lserver)
    end.

announce_motd_delete(LServer) ->
    F = fun() ->
		mnesia:delete({motd, LServer}),
		mnesia:write_lock_table(motd_users),
		Users = mnesia:select(
			  motd_users,
			  [{#motd_users{us = '$1', _ = '_'},
			    [{'==', {element, 2, '$1'}, LServer}],
			    ['$1']}]),
		lists:foreach(fun(US) ->
				      mnesia:delete({motd_users, US})
			      end, Users)
	end,
    mnesia:transaction(F).

send_motd(#jid{luser = LUser, lserver = LServer} = JID) ->
    case catch mnesia:dirty_read({motd, LServer}) of
	[#motd{packet = Packet}] ->
	    US = {LUser, LServer},
	    case catch mnesia:dirty_read({motd_users, US}) of
		[#motd_users{}] ->
		    ok;
		_ ->
		    Local = jlib:make_jid("", LServer, ""),
		    ejabberd_router:route(Local, JID, Packet),
		    F = fun() ->
				mnesia:write(#motd_users{us = US})
			end,
		    mnesia:transaction(F)
	    end;
	_ ->
	    ok
    end.


update_tables() ->
    update_motd_table(),
    update_motd_users_table().

update_motd_table() ->
    Fields = record_info(fields, motd),
    case mnesia:table_info(motd, attributes) of
	Fields ->
	    ok;
	[id, packet] ->
	    ?INFO_MSG("Converting motd table from "
		      "{id, packet} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_announce_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, motd},
			      {attributes, record_info(fields, motd)}]),
	    mnesia:transform_table(motd, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_announce_tmp_table),
			 mnesia:foldl(
			   fun(#motd{server = _} = R, _) ->
				   mnesia:dirty_write(
				     mod_announce_tmp_table,
				     R#motd{server = Host})
			   end, ok, motd)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(motd),
	    F2 = fun() ->
			 mnesia:write_lock_table(motd),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_announce_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_announce_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating motd table", []),
	    mnesia:transform_table(motd, ignore, Fields)
    end.


update_motd_users_table() ->
    Fields = record_info(fields, motd_users),
    case mnesia:table_info(motd_users, attributes) of
	Fields ->
	    ok;
	[luser, dummy] ->
	    ?INFO_MSG("Converting motd_users table from "
		      "{luser, dummy} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_announce_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, motd_users},
			      {attributes, record_info(fields, motd_users)}]),
	    mnesia:transform_table(motd_users, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_announce_tmp_table),
			 mnesia:foldl(
			   fun(#motd_users{us = U} = R, _) ->
				   mnesia:dirty_write(
				     mod_announce_tmp_table,
				     R#motd_users{us = {U, Host}})
			   end, ok, motd_users)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(motd_users),
	    F2 = fun() ->
			 mnesia:write_lock_table(motd_users),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_announce_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_announce_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating motd_users table", []),
	    mnesia:transform_table(motd_users, ignore, Fields)
    end.
