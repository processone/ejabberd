%%%----------------------------------------------------------------------
%%% File    : mod_last_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:last support (JEP-0012)
%%% Created : 24 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(mod_last_odbc).
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

start(Host, Opts) ->
    HostB = list_to_binary(Host),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
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
      [#xmlattr{name = 'seconds', value = list_to_binary(integer_to_list(Sec))}]},
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
    User = exmpp_jid:lnode_as_list(To),
    Server = exmpp_jid:ldomain_as_list(To),
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
                    get_last(IQ_Rec, User, Server);
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
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_last(LServer, Username) of
	{selected, ["seconds","state"], []} ->
	    exmpp_iq:error(IQ_Rec, 'service-unavailable');
	{selected, ["seconds","state"], [{STimeStamp, Status}]} ->
	    case catch list_to_integer(STimeStamp) of
		TimeStamp when is_integer(TimeStamp) ->
		    TimeStamp2 = now_to_seconds(now()),
		    Sec = TimeStamp2 - TimeStamp,
		    Response = #xmlel{ns = ?NS_LAST_ACTIVITY, name = 'query',
		      attrs = [#xmlattr{name = 'seconds', value = list_to_binary(integer_to_list(Sec))}],
		      children = [#xmlcdata{cdata = list_to_binary(Status)}]},
		    exmpp_iq:result(IQ_Rec, Response);
		_ ->
		    exmpp_iq:error(IQ_Rec, 'internal-server-error')
	    end;
	_ ->
	    exmpp_iq:error(IQ_Rec, 'internal-server-error')
    end.

on_presence_update(User, Server, _Resource, Status) ->
    TimeStamp = now_to_seconds(now()),
    store_last_info(User, Server, TimeStamp, Status).

store_last_info(User, Server, TimeStamp, Status)
        when is_binary(User), is_binary(Server) ->
    try
        %LUser = exmpp_stringprep:nodeprep(User),
        %LServer = exmpp_stringprep:nameprep(Server),
        LUser = binary_to_list(User),
        LServer = binary_to_list(Server),

        Username = ejabberd_odbc:escape(LUser),
        Seconds = ejabberd_odbc:escape(integer_to_list(TimeStamp)),
        State = ejabberd_odbc:escape(Status),
        odbc_queries:set_last_t(LServer, Username, Seconds, State)
    catch
        _ ->
            ok
    end.

%% @spec (LUser::string(), LServer::string() ->
%%      {ok, Timestamp::integer(), Status::string()} | not_found
get_last_info(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_last(LServer, Username) of
	{selected, ["seconds","state"], []} ->
	    not_found;
	{selected, ["seconds","state"], [{STimeStamp, Status}]} ->
	    case catch list_to_integer(STimeStamp) of
		TimeStamp when is_integer(TimeStamp) ->
		    {ok, TimeStamp, Status};
		_ ->
		    not_found
	    end;
	_ ->
	    not_found
    end.

remove_user(User, Server) ->
    try
        LUser = exmpp_stringprep:nodeprep(User),
        LServer = exmpp_stringprep:nameprep(Server),
        Username = ejabberd_odbc:escape(LUser),
        odbc_queries:del_last(LServer, Username)
    catch
        _ ->
            ok
    end.
