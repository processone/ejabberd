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
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_LAST).

process_local_iq(_From, _To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    {iq, ID, error, XMLNS,
	     [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    Sec = trunc(element(1, erlang:statistics(wall_clock))/1000),
	    {iq, ID, result, XMLNS,
	     [{xmlelement, "query",
	       [{"xmlns", ?NS_LAST},
		{"seconds", integer_to_list(Sec)}],
	       []}]}
    end.


process_sm_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    {iq, ID, error, XMLNS, [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    User = To#jid.luser,
	    {Subscription, _Groups} =
		mod_roster:get_jid_info(User, From),
	    if
		(Subscription == both) or (Subscription == from) ->
		    case catch mod_privacy:get_user_list(User) of
			{'EXIT', _Reason} ->
			    get_last(ID, XMLNS, SubEl, User);
			List ->
			    case mod_privacy:check_packet(
				   User, List,
				   {From, To,
				    {xmlelement, "presence", [], []}},
				   out) of
				allow ->
				    get_last(ID, XMLNS, SubEl, User);
				deny ->
				    {iq, ID, error, XMLNS,
				     [SubEl, ?ERR_NOT_ALLOWED]}
			    end
		    end;
		true ->
		    {iq, ID, error, XMLNS, [SubEl, ?ERR_NOT_ALLOWED]}
	    end
    end.

get_last(ID, XMLNS, SubEl, LUser) ->
    case catch mnesia:dirty_read(last_activity, LUser) of
	{'EXIT', _Reason} ->
	    {iq, ID, error, XMLNS, [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
	[] ->
	    {iq, ID, error, XMLNS, [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
	[#last_activity{timestamp = TimeStamp}] ->
	    {MegaSecs, Secs, _MicroSecs} = now(),
	    TimeStamp2 = MegaSecs * 1000000 + Secs,
	    Sec = TimeStamp2 - TimeStamp,
	    {iq, ID, result, XMLNS,
	     [{xmlelement, "query",
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
