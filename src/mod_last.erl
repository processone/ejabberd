%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : jabber:iq:last support (JEP-0012)
%%% Created : 24 Oct 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_last).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1,
	 stop/0,
	 process_local_iq/3,
	 process_sm_iq/3,
	 on_presence_update/4,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(last_activity, {us, timestamp, status}).


start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(last_activity,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, last_activity)}]),
    update_table(),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_LAST,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_LAST,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(remove_user,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook,
		       ?MODULE, on_presence_update, 50).

stop() ->
    ejabberd_hooks:delete(remove_user,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook,
			  ?MODULE, on_presence_update, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_LAST),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ?NS_LAST).

process_local_iq(_From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    Sec = trunc(element(1, erlang:statistics(wall_clock))/1000),
	    IQ#iq{type = result,
		  sub_el =  [{xmlelement, "query",
			      [{"xmlns", ?NS_LAST},
			       {"seconds", integer_to_list(Sec)}],
			      []}]}
    end.


process_sm_iq(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    User = To#jid.luser,
	    Server = To#jid.lserver,
	    {Subscription, _Groups} =
		ejabberd_hooks:run_fold(
		  roster_get_jid_info, {none, []}, [User, Server, From]),
	    if
		(Subscription == both) or (Subscription == from) ->
		    case catch mod_privacy:get_user_list(User, Server) of
			{'EXIT', _Reason} ->
			    get_last(IQ, SubEl, User, Server);
			List ->
			    case catch mod_privacy:check_packet(
					 User, Server, List,
					 {From, To,
					  {xmlelement, "presence", [], []}},
					 out) of
				{'EXIT', _Reason} ->
				    get_last(IQ, SubEl, User, Server);
				allow ->
				    get_last(IQ, SubEl, User, Server);
				deny ->
				    IQ#iq{type = error,
					  sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
			    end
		    end;
		true ->
		    IQ#iq{type = error,
			  sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
	    end
    end.

get_last(IQ, SubEl, LUser, LServer) ->
    case catch mnesia:dirty_read(last_activity, {LUser, LServer}) of
	{'EXIT', _Reason} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
	[] ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
	[#last_activity{timestamp = TimeStamp, status = Status}] ->
	    {MegaSecs, Secs, _MicroSecs} = now(),
	    TimeStamp2 = MegaSecs * 1000000 + Secs,
	    Sec = TimeStamp2 - TimeStamp,
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_LAST},
			      {"seconds", integer_to_list(Sec)}],
			     [{xmlcdata, Status}]}]}
    end.



on_presence_update(User, Server, _Resource, Status) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    F = fun() ->
		mnesia:write(#last_activity{us = US,
					    timestamp = TimeStamp,
					    status = Status})
	end,
    mnesia:transaction(F).


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

