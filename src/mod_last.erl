%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:last support (XEP-0012)
%%% Created : 24 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_last).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_local_iq/3,
	 process_sm_iq/3,
	 on_presence_update/4,
	 store_last_info/4,
	 get_last_info/2,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-record(last_activity, {us, timestamp, status}).


start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(last_activity,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, last_activity)}]),
    update_table(),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_LAST,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_LAST,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook, Host,
		       ?MODULE, on_presence_update, 50).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host,
			  ?MODULE, on_presence_update, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_LAST),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_LAST).

%%%
%%% Uptime of ejabberd node
%%%

process_local_iq(_From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    Sec = get_node_uptime(),
	    IQ#iq{type = result,
		  sub_el =  [{xmlelement, "query",
			      [{"xmlns", ?NS_LAST},
			       {"seconds", integer_to_list(Sec)}],
			      []}]}
    end.

%% @spec () -> integer()
%% @doc Get the uptime of the ejabberd node, expressed in seconds.
%% When ejabberd is starting, ejabberd_config:start/0 stores the datetime.
get_node_uptime() ->
    case ejabberd_config:get_local_option(node_start) of
	{_, _, _} = StartNow ->
	    now_to_seconds(now()) - now_to_seconds(StartNow);
	_undefined ->
	    trunc(element(1, erlang:statistics(wall_clock))/1000)
    end.

now_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.


%%%
%%% Serve queries about user last online
%%%

process_sm_iq(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    User = To#jid.luser,
	    Server = To#jid.lserver,
	    {Subscription, _Groups} =
		ejabberd_hooks:run_fold(
		  roster_get_jid_info, Server,
		  {none, []}, [User, Server, From]),
	    if
		(Subscription == both) or (Subscription == from)
		or ((From#jid.luser == To#jid.luser)
		    and (From#jid.lserver == To#jid.lserver)) ->
		    UserListRecord = ejabberd_hooks:run_fold(
				       privacy_get_user_list, Server,
				       #userlist{},
				       [User, Server]),
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, Server,
			   allow,
			   [User, Server, UserListRecord,
			    {To, From,
			     {xmlelement, "presence", [], []}},
			    out]) of
			allow ->
			    get_last_iq(IQ, SubEl, User, Server);
			deny ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_FORBIDDEN]}
		    end;
		true ->
		    IQ#iq{type = error,
			  sub_el = [SubEl, ?ERR_FORBIDDEN]}
	    end
    end.

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, TimeStamp::integer(), Status::string()} | not_found | {error, Reason}
get_last(LUser, LServer) ->
    case catch mnesia:dirty_read(last_activity, {LUser, LServer}) of
	{'EXIT', Reason} ->
	    {error, Reason};
	[] ->
	    not_found;
	[#last_activity{timestamp = TimeStamp, status = Status}] ->
	    {ok, TimeStamp, Status}
    end.

get_last_iq(IQ, SubEl, LUser, LServer) ->
    case ejabberd_sm:get_user_resources(LUser, LServer) of
	[] ->
	    case get_last(LUser, LServer) of
		{error, _Reason} ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
		not_found ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
		{ok, TimeStamp, Status} ->
		    TimeStamp2 = now_to_seconds(now()),
		    Sec = TimeStamp2 - TimeStamp,
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", ?NS_LAST},
				      {"seconds", integer_to_list(Sec)}],
				     [{xmlcdata, Status}]}]}
	    end;
	_ ->
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_LAST},
			      {"seconds", "0"}],
			     []}]}
    end.

on_presence_update(User, Server, _Resource, Status) ->
    TimeStamp = now_to_seconds(now()),
    store_last_info(User, Server, TimeStamp, Status).

store_last_info(User, Server, TimeStamp, Status) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		mnesia:write(#last_activity{us = US,
					    timestamp = TimeStamp,
					    status = Status})
	end,
    mnesia:transaction(F).

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, TimeStamp::integer(), Status::string()} | not_found
get_last_info(LUser, LServer) ->
    case get_last(LUser, LServer) of
	{error, _Reason} ->
	    not_found;
	Res ->
	    Res
    end.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		mnesia:delete({last_activity, US})
	end,
    mnesia:transaction(F).


update_table() ->
    Fields = record_info(fields, last_activity),
    case mnesia:table_info(last_activity, attributes) of
	Fields ->
	    ok;
	[user, timestamp, status] ->
	    ?INFO_MSG("Converting last_activity table from {user, timestamp, status} format", []),
	    Host = ?MYNAME,
	    mnesia:transform_table(last_activity, ignore, Fields),
	    F = fun() ->
			mnesia:write_lock_table(last_activity),
			mnesia:foldl(
			  fun({_, U, T, S} = R, _) ->
				  mnesia:delete_object(R),
				  mnesia:write(
				    #last_activity{us = {U, Host},
						   timestamp = T,
						   status = S})
			  end, ok, last_activity)
		end,
	    mnesia:transaction(F);
	[user, timestamp] ->
	    ?INFO_MSG("Converting last_activity table from {user, timestamp} format", []),
	    Host = ?MYNAME,
	    mnesia:transform_table(
	      last_activity,
	      fun({_, U, T}) ->
		      #last_activity{us = U,
				     timestamp = T,
				     status = ""}
	      end, Fields),
	    F = fun() ->
			mnesia:write_lock_table(last_activity),
			mnesia:foldl(
			  fun({_, U, T, S} = R, _) ->
				  mnesia:delete_object(R),
				  mnesia:write(
				    #last_activity{us = {U, Host},
						   timestamp = T,
						   status = S})
			  end, ok, last_activity)
		end,
	    mnesia:transaction(F);
	_ ->
	    ?INFO_MSG("Recreating last_activity table", []),
	    mnesia:transform_table(last_activity, ignore, Fields)
    end.

