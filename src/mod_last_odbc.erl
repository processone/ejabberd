%%%----------------------------------------------------------------------
%%% File    : mod_last_odbc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : jabber:iq:last support (JEP-0012)
%%% Created : 24 Oct 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_last_odbc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1,
	 stop/0,
	 process_local_iq/3,
	 process_sm_iq/3,
	 on_presence_update/3,
	 remove_user/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").


start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
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
	    {Subscription, _Groups} =
		ejabberd_hooks:run_fold(
		  roster_get_jid_info, {none, []}, [User, From]),
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
    Username = ejabberd_odbc:escape(LUser),
    case catch ejabberd_odbc:sql_query(
		 ["select seconds, state from last "
		  "where username='", Username, "'"]) of
	{'EXIT', _Reason} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
	{selected, ["seconds","state"], []} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
	{selected, ["seconds","state"], [{STimeStamp, Status}]} ->
	    case catch list_to_integer(STimeStamp) of
		TimeStamp when is_integer(TimeStamp) ->
		    {MegaSecs, Secs, _MicroSecs} = now(),
		    TimeStamp2 = MegaSecs * 1000000 + Secs,
		    Sec = TimeStamp2 - TimeStamp,
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", ?NS_LAST},
				      {"seconds", integer_to_list(Sec)}],
				     [{xmlcdata, Status}]}]};
		_ ->
		    IQ#iq{type = error,
			  sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
	    end
    end.



on_presence_update(User, _Resource, Status) ->
    LUser = jlib:nodeprep(User),
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    Username = ejabberd_odbc:escape(LUser),
    Seconds = ejabberd_odbc:escape(integer_to_list(TimeStamp)),
    State = ejabberd_odbc:escape(Status),
    ejabberd_odbc:sql_query(
      ["begin;"
       "delete from last where username='", Username, "';"
       "insert into last(username, seconds, state) "
       "values ('", Username, "', '", Seconds, "', '", State, "');",
       "commit"]).


remove_user(User) ->
    LUser = jlib:nodeprep(User),
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query(
      ["delete from last where username='", Username, "'"]).

