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
	 on_presence_update/1,
	 remove_user/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(last_activity, {user, timestamp}).


start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(last_activity,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, last_activity)}]),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_LAST,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_LAST,
				  ?MODULE, process_sm_iq, IQDisc).

stop() ->
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
	    {Subscription, _Groups} =
		mod_roster:get_jid_info(User, From),
	    if
		(Subscription == both) or (Subscription == from) ->
		    case catch mod_privacy:get_user_list(User) of
			{'EXIT', _Reason} ->
			    get_last(IQ, SubEl, User);
			List ->
			    case catch mod_privacy:check_packet(
					 User, List,
					 {From, To,
					  {xmlelement, "presence", [], []}},
					 out) of
				{'EXIT', _Reason} ->
				    get_last(IQ, SubEl, User);
				allow ->
				    get_last(IQ, SubEl, User);
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

get_last(IQ, SubEl, LUser) ->
    case catch mnesia:dirty_read(last_activity, LUser) of
	{'EXIT', _Reason} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
	[] ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
	[#last_activity{timestamp = TimeStamp}] ->
	    {MegaSecs, Secs, _MicroSecs} = now(),
	    TimeStamp2 = MegaSecs * 1000000 + Secs,
	    Sec = TimeStamp2 - TimeStamp,
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_LAST},
			      {"seconds", integer_to_list(Sec)}],
			     []}]}
    end.



on_presence_update(LUser) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    F = fun() ->
		mnesia:write(#last_activity{user = LUser,
					    timestamp = TimeStamp})
	end,
    mnesia:transaction(F).


remove_user(User) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
		mnesia:delete({last_activity, LUser})
	end,
    mnesia:transaction(F).
