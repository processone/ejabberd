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

-record(motd, {id, packet}).
-record(motd_users, {luser, dummy = []}).

-define(PROCNAME, ejabberd_announce).

start(_) ->
    mnesia:create_table(motd, [{disc_copies, [node()]},
			       {attributes, record_info(fields, motd)}]),
    mnesia:create_table(motd_users, [{disc_copies, [node()]},
				     {attributes, record_info(fields, motd_users)}]),
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
    ok.

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
	    Server = ?MYNAME,
	    Local = jlib:make_jid("", Server, ""),
	    lists:foreach(
		fun(U) ->
			Dest = jlib:make_jid(U, Server, ""),
			ejabberd_router:route(Local, Dest, Packet)
		end, ejabberd_auth:dirty_get_registered_users())
    end.

announce_online(From, To, Packet) ->
    Access = gen_mod:get_module_opt(?MODULE, access, none),
    case acl:match_rule(Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_online1(ejabberd_sm:dirty_get_sessions_list(), Packet)
    end.

announce_online1(Sessions, Packet) ->
    Server = ?MYNAME,
    Local = jlib:make_jid("", Server, ""),
    lists:foreach(
      fun({U, R}) ->
	      Dest = jlib:make_jid(U, Server, R),
	      ejabberd_router:route(Local, Dest, Packet)
      end, Sessions).

announce_motd(From, To, Packet) ->
    Access = gen_mod:get_module_opt(?MODULE, access, none),
    case acl:match_rule(Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_update(Packet),
	    Sessions = ejabberd_sm:dirty_get_sessions_list(),
	    announce_online1(Sessions, Packet),
	    F = fun() ->
			lists:foreach(
			  fun({U, _R}) ->
				  mnesia:write(#motd_users{luser = U})
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
	    announce_motd_update(Packet)
    end.

announce_motd_update(Packet) ->
    announce_motd_delete(),
    F = fun() ->
		mnesia:write(#motd{id = motd, packet = Packet})
	end,
    mnesia:transaction(F).

announce_motd_delete(From, To, Packet) ->
    Access = gen_mod:get_module_opt(?MODULE, access, none),
    case acl:match_rule(Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_delete()
    end.

announce_motd_delete() ->
    mnesia:clear_table(motd),
    mnesia:clear_table(motd_users).

send_motd(#jid{luser = LUser} = JID) ->
    case catch mnesia:dirty_read({motd, motd}) of
	[#motd{packet = Packet}] ->
	    case catch mnesia:dirty_read({motd_users, LUser}) of
		[#motd_users{}] ->
		    ok;
		_ ->
		    Local = jlib:make_jid("", ?MYNAME, ""),
		    ejabberd_router:route(Local, JID, Packet),
		    F = fun() ->
				mnesia:write(#motd_users{luser = LUser})
			end,
		    mnesia:transaction(F)
	    end;
	_ ->
	    ok
    end.

