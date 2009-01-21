%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:last support (JEP-0012)
%%% Created : 24 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_privacy.hrl").

-record(last_activity, {us, timestamp, status}).


start(Host, Opts) ->
    HostB = list_to_binary(Host),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(last_activity,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, last_activity)}]),
    update_table(),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_LAST_ACTIVITY,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_LAST_ACTIVITY,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook, HostB,
		       ?MODULE, on_presence_update, 50).

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, HostB,
			  ?MODULE, on_presence_update, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_LAST_ACTIVITY),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_LAST_ACTIVITY).

%%%
%%% Uptime of ejabberd node
%%%

process_local_iq(_From, _To, #iq{type = get} = IQ_Rec) ->
    Sec = get_node_uptime(),
    Response = #xmlel{ns = ?NS_LAST_ACTIVITY, name = 'query', attrs =
      [?XMLATTR('seconds', Sec)]},
    exmpp_iq:result(IQ_Rec, Response);
process_local_iq(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

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

process_sm_iq(From, To, #iq{type = get} = IQ_Rec) ->
    {Subscription, _Groups} =
	ejabberd_hooks:run_fold(
	  roster_get_jid_info, exmpp_jid:ldomain(To),
	  {none, []}, [exmpp_jid:lnode(To), exmpp_jid:ldomain(To), From]),
    if
	(Subscription == both) or (Subscription == from) ->
	    UserListRecord = ejabberd_hooks:run_fold(
			       privacy_get_user_list, exmpp_jid:ldomain(To),
			       #userlist{},
			       [exmpp_jid:lnode(To), exmpp_jid:ldomain(To)]),
	    case ejabberd_hooks:run_fold(
		   privacy_check_packet, exmpp_jid:ldomain(To),
		   allow,
		   [exmpp_jid:lnode(To), exmpp_jid:ldomain(To), UserListRecord,
		    {From, To,
		     exmpp_presence:available()},
		    out]) of
		allow ->
		    get_last(IQ_Rec, exmpp_jid:lnode(To), exmpp_jid:ldomain(To));
		deny ->
		    exmpp_iq:error(IQ_Rec, 'not-allowed')
	    end;
	true ->
	    exmpp_iq:error(IQ_Rec, 'not-allowed')
    end;
process_sm_iq(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

%% TODO: This function could use get_last_info/2
get_last(IQ_Rec, LUser, LServer) ->
    case catch mnesia:dirty_read(last_activity, {LUser, LServer}) of
	{'EXIT', _Reason} ->
	    exmpp_iq:error(IQ_Rec, 'internal-server-error');
	[] ->
	    exmpp_iq:error(IQ_Rec, 'service-unavailable');
	[#last_activity{timestamp = TimeStamp, status = Status}] ->
	    TimeStamp2 = now_to_seconds(now()),
	    Sec = TimeStamp2 - TimeStamp,
	    Response = #xmlel{ns = ?NS_LAST_ACTIVITY, name = 'query',
	      attrs = [?XMLATTR('seconds', Sec)],
	      children = [#xmlcdata{cdata = Status}]},
	    exmpp_iq:result(IQ_Rec, Response)
    end.



on_presence_update(User, Server, _Resource, Status) ->
    TimeStamp = now_to_seconds(now()),
    store_last_info(User, Server, TimeStamp, Status).

store_last_info(User, Server, TimeStamp, Status) 
        when is_binary(User), is_binary(Server) ->
    try
	US = {User, Server},
	F = fun() ->
		    mnesia:write(#last_activity{us = US,
						timestamp = TimeStamp,
						status = Status})
	    end,
	mnesia:transaction(F)
    catch
	_ ->
	    ok
    end.

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, Timestamp::integer(), Status::string()} | not_found
get_last_info(LUser, LServer) when is_binary(LUser), is_binary(LServer) ->
    case catch mnesia:dirty_read(last_activity, {LUser, LServer}) of
	{'EXIT', _Reason} ->
	    not_found;
	[] ->
	    not_found;
	[#last_activity{timestamp = TimeStamp, status = Status}] ->
	    {ok, TimeStamp, Status}
    end.

remove_user(User, Server) when is_binary(User), is_binary(Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	US = {LUser, LServer},
	F = fun() ->
		    mnesia:delete({last_activity, US})
	    end,
	mnesia:transaction(F)
    catch
	_ ->
	    ok
    end.


update_table() ->
    Fields = record_info(fields, last_activity),
    case mnesia:table_info(last_activity, attributes) of
	Fields ->
            convert_to_exmpp();
	[user, timestamp, status] ->
	    ?INFO_MSG("Converting last_activity table from {user, timestamp, status} format", []),
	    Host = ?MYNAME,
	    mnesia:transform_table(last_activity, ignore, Fields),
	    F = fun() ->
			mnesia:write_lock_table(last_activity),
			mnesia:foldl(
			  fun({_, U, T, S} = R, _) ->
				  U1 = convert_jid_to_exmpp(U),
				  mnesia:delete_object(R),
				  mnesia:write(
				    #last_activity{us = {U1, Host},
						   timestamp = T,
						   status = list_to_binary(S)})
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
				     status = <<>>}
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

convert_to_exmpp() ->
    Fun = fun() ->
	case mnesia:first(last_activity) of
	    '$end_of_table' ->
		none;
	    Key ->
		case mnesia:read({last_activity, Key}) of
		    [#last_activity{status = Status}] when is_binary(Status) ->
			none;
		    [#last_activity{}] ->
			mnesia:foldl(fun convert_to_exmpp2/2,
			  done, last_activity, write)
		end
	end
    end,
    mnesia:transaction(Fun).

convert_to_exmpp2(#last_activity{us = {U, S} = Key, status = Status} = LA,
  Acc) ->
    % Remove old entry.
    mnesia:delete({last_activity, Key}),
    % Convert "" to undefined in JIDs.
    U1 = convert_jid_to_exmpp(U),
    % Convert status.
    Status1 = list_to_binary(Status),
    % Prepare the new record.
    New_LA = LA#last_activity{us = {list_to_binary(U1), list_to_binary(S)}, 
                              status = Status1},
    % Write the new record.
    mnesia:write(New_LA),
    Acc.

convert_jid_to_exmpp("") -> undefined;
convert_jid_to_exmpp(V)  -> V.
