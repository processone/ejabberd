%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:last support (XEP-0012)
%%% Created : 24 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_last).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-protocol({xep, 12, '2.0'}).

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq/3, export/1,
	 process_sm_iq/3, on_presence_update/4, import/1,
	 import/3, store_last_info/4, get_last_info/2,
	 remove_user/2, transform_options/1, mod_opt_type/1,
	 opt_type/1, register_user/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").
-include("mod_last.hrl").

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #last_activity{}) -> ok | pass.
-callback get_last(binary(), binary()) ->
    {ok, non_neg_integer(), binary()} | not_found | {error, any()}.
-callback store_last_info(binary(), binary(), non_neg_integer(), binary()) ->
    {atomic, any()}.
-callback remove_user(binary(), binary()) -> {atomic, any()}.

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_LAST, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_LAST, ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(register_user, Host, ?MODULE,
		       register_user, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE,
		       on_presence_update, 50).

stop(Host) ->
    ejabberd_hooks:delete(register_user, Host, ?MODULE,
			  register_user, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host,
			  ?MODULE, on_presence_update, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_LAST),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_LAST).

%%%
%%% Uptime of ejabberd node
%%%

process_local_iq(_From, _To,
		 #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
	  IQ#iq{type = error, sub_el = [SubEl, ?ERRT_NOT_ALLOWED(Lang, Txt)]};
      get ->
	  Sec = get_node_uptime(),
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs =
				[{<<"xmlns">>, ?NS_LAST},
				 {<<"seconds">>,
				  iolist_to_binary(integer_to_list(Sec))}],
			    children = []}]}
    end.

%% @spec () -> integer()
%% @doc Get the uptime of the ejabberd node, expressed in seconds.
%% When ejabberd is starting, ejabberd_config:start/0 stores the datetime.
get_node_uptime() ->
    case ejabberd_config:get_option(
           node_start,
           fun(S) when is_integer(S), S >= 0 -> S end) of
        undefined ->
            trunc(element(1, erlang:statistics(wall_clock)) / 1000);
        Now ->
            p1_time_compat:system_time(seconds) - Now
    end.

now_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

%%%
%%% Serve queries about user last online
%%%

process_sm_iq(From, To,
	      #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
	  IQ#iq{type = error, sub_el = [SubEl, ?ERRT_NOT_ALLOWED(Lang, Txt)]};
      get ->
	  User = To#jid.luser,
	  Server = To#jid.lserver,
	  {Subscription, _Groups} =
	      ejabberd_hooks:run_fold(roster_get_jid_info, Server,
				      {none, []}, [User, Server, From]),
	  if (Subscription == both) or (Subscription == from) or
	       (From#jid.luser == To#jid.luser) and
		 (From#jid.lserver == To#jid.lserver) ->
		 UserListRecord =
		     ejabberd_hooks:run_fold(privacy_get_user_list, Server,
					     #userlist{}, [User, Server]),
		 case ejabberd_hooks:run_fold(privacy_check_packet,
					      Server, allow,
					      [User, Server, UserListRecord,
					       {To, From,
						#xmlel{name = <<"presence">>,
						       attrs = [],
						       children = []}},
					       out])
		     of
		   allow -> get_last_iq(IQ, SubEl, User, Server);
		   deny ->
		       IQ#iq{type = error, sub_el = [SubEl, ?ERR_FORBIDDEN]}
		 end;
	     true ->
		 Txt = <<"Not subscribed">>,
		 IQ#iq{type = error, sub_el = [SubEl, ?ERRT_FORBIDDEN(Lang, Txt)]}
	  end
    end.

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, TimeStamp::integer(), Status::string()} | not_found | {error, Reason}
get_last(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_last(LUser, LServer).

get_last_iq(#iq{lang = Lang} = IQ, SubEl, LUser, LServer) ->
    case ejabberd_sm:get_user_resources(LUser, LServer) of
      [] ->
	  case get_last(LUser, LServer) of
	    {error, _Reason} ->
		Txt = <<"Database failure">>,
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERRT_INTERNAL_SERVER_ERROR(Lang, Txt)]};
	    not_found ->
		Txt = <<"No info about last activity found">>,
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERRT_SERVICE_UNAVAILABLE(Lang, Txt)]};
	    {ok, TimeStamp, Status} ->
		TimeStamp2 = p1_time_compat:system_time(seconds),
		Sec = TimeStamp2 - TimeStamp,
		IQ#iq{type = result,
		      sub_el =
			  [#xmlel{name = <<"query">>,
				  attrs =
				      [{<<"xmlns">>, ?NS_LAST},
				       {<<"seconds">>,
					iolist_to_binary(integer_to_list(Sec))}],
				  children = [{xmlcdata, Status}]}]}
	  end;
      _ ->
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs =
				[{<<"xmlns">>, ?NS_LAST},
				 {<<"seconds">>, <<"0">>}],
			    children = []}]}
    end.

register_user(User, Server) ->
    on_presence_update(
       User,
       Server,
       <<"RegisterResource">>,
       <<"Registered but didn't login">>).

on_presence_update(User, Server, _Resource, Status) ->
    TimeStamp = p1_time_compat:system_time(seconds),
    store_last_info(User, Server, TimeStamp, Status).

store_last_info(User, Server, TimeStamp, Status) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:store_last_info(LUser, LServer, TimeStamp, Status).

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, TimeStamp::integer(), Status::string()} | not_found
get_last_info(LUser, LServer) ->
    case get_last(LUser, LServer) of
      {error, _Reason} -> not_found;
      Res -> Res
    end.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, LA) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, LA).

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({node_start, {_, _, _} = Now}, Opts) ->
    ?WARNING_MSG("Old 'node_start' format detected. This is still supported "
                 "but it is better to fix your config.", []),
    [{node_start, now_to_seconds(Now)}|Opts];
transform_options(Opt, Opts) ->
    [Opt|Opts].

mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [db_type, iqdisc].

opt_type(node_start) ->
    fun (S) when is_integer(S), S >= 0 -> S end;
opt_type(_) -> [node_start].
