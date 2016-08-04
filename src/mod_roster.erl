%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

%%% @doc Roster management (Mnesia storage).
%%%
%%% Includes support for XEP-0237: Roster Versioning.
%%% The roster versioning follows an all-or-nothing strategy:
%%%  - If the version supplied by the client is the latest, return an empty response.
%%%  - If not, return the entire new roster (with updated version string).
%%% Roster version is a hash digest of the entire roster.
%%% No additional data is stored in DB.

-module(mod_roster).

-protocol({xep, 237, '1.3'}).
-protocol({xep, 321, '0.1'}).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/1, export/1,
	 import/1, process_local_iq/1, get_user_roster/2,
	 import/3, get_subscription_lists/3, get_roster/2,
	 get_in_pending_subscriptions/3, in_subscription/6,
	 out_subscription/4, set_items/3, remove_user/2,
	 get_jid_info/4, encode_item/1, webadmin_page/3,
	 webadmin_user/4, get_versioning_feature/2,
	 roster_versioning_enabled/1, roster_version/2,
	 mod_opt_type/1, set_roster/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("mod_roster.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-export_type([subscription/0]).

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #roster{} | #roster_version{}) -> ok | pass.
-callback read_roster_version(binary(), binary()) -> binary() | error.
-callback write_roster_version(binary(), binary(), boolean(), binary()) -> any().
-callback get_roster(binary(), binary()) -> [#roster{}].
-callback get_roster_by_jid(binary(), binary(), ljid()) -> #roster{}.
-callback get_only_items(binary(), binary()) -> [#roster{}].
-callback roster_subscribe(binary(), binary(), ljid(), #roster{}) -> any().
-callback transaction(binary(), function()) -> {atomic, any()} | {aborted, any()}.
-callback get_roster_by_jid_with_groups(binary(), binary(), ljid()) -> #roster{}.
-callback remove_user(binary(), binary()) -> {atomic, any()}.
-callback update_roster(binary(), binary(), ljid(), #roster{}) -> any().
-callback del_roster(binary(), binary(), ljid()) -> any().
-callback read_subscription_and_groups(binary(), binary(), ljid()) ->
    {subscription(), [binary()]}.

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    ejabberd_hooks:add(roster_get, Host, ?MODULE,
		       get_user_roster, 50),
    ejabberd_hooks:add(roster_in_subscription, Host,
		       ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, Host,
		       ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(roster_get_subscription_lists, Host,
		       ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:add(roster_get_jid_info, Host, ?MODULE,
		       get_jid_info, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(resend_subscription_requests_hook,
		       Host, ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:add(roster_get_versioning_feature, Host,
		       ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:add(webadmin_page_host, Host, ?MODULE,
		       webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host, ?MODULE,
		       webadmin_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_ROSTER, ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(roster_get, Host, ?MODULE,
			  get_user_roster, 50),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, Host,
			  ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(roster_get_subscription_lists,
			  Host, ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
			  ?MODULE, get_jid_info, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(resend_subscription_requests_hook,
			  Host, ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:delete(roster_get_versioning_feature,
			  Host, ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host, ?MODULE,
			  webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host, ?MODULE,
			  webadmin_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_ROSTER).

depends(_Host, _Opts) ->
    [].

process_iq(#iq{from = #jid{luser = <<"">>},
	       to = #jid{resource = <<"">>}} = IQ) ->
    process_iq_manager(IQ);

process_iq(#iq{from = From, lang = Lang} = IQ) ->
    #jid{lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
      true -> process_local_iq(IQ);
      _ ->
	  Txt = <<"The query is only allowed from local users">>,
	  xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang))
    end.

process_local_iq(#iq{type = Type} = IQ) ->
    case Type of
      set -> try_process_iq_set(IQ);
      get -> process_iq_get(IQ)
    end.

roster_hash(Items) ->
    p1_sha:sha(term_to_binary(lists:sort([R#roster{groups =
						    lists:sort(Grs)}
				       || R = #roster{groups = Grs}
					      <- Items]))).

roster_versioning_enabled(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, versioning,
                           fun(B) when is_boolean(B) -> B end,
			   false).

roster_version_on_db(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, store_current_id,
                           fun(B) when is_boolean(B) -> B end,
			   false).

%% Returns a list that may contain an xmlelement with the XEP-237 feature if it's enabled.
get_versioning_feature(Acc, Host) ->
    case roster_versioning_enabled(Host) of
      true ->
	  [#rosterver_feature{}|Acc];
      false -> []
    end.

roster_version(LServer, LUser) ->
    US = {LUser, LServer},
    case roster_version_on_db(LServer) of
      true ->
	  case read_roster_version(LUser, LServer) of
	    error -> not_found;
	    V -> V
	  end;
      false ->
	  roster_hash(ejabberd_hooks:run_fold(roster_get, LServer,
					      [], [US]))
    end.

read_roster_version(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:read_roster_version(LUser, LServer).

write_roster_version(LUser, LServer) ->
    write_roster_version(LUser, LServer, false).

write_roster_version_t(LUser, LServer) ->
    write_roster_version(LUser, LServer, true).

write_roster_version(LUser, LServer, InTransaction) ->
    Ver = p1_sha:sha(term_to_binary(p1_time_compat:unique_integer())),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:write_roster_version(LUser, LServer, InTransaction, Ver),
    Ver.

%% Load roster from DB only if neccesary.
%% It is neccesary if
%%     - roster versioning is disabled in server OR
%%     - roster versioning is not used by the client OR
%%     - roster versioning is used by server and client, BUT the server isn't storing versions on db OR
%%     - the roster version from client don't match current version.
process_iq_get(#iq{from = From, to = To, lang = Lang,
		   sub_els = [#roster_query{ver = RequestedVersion}]} = IQ) ->
    LUser = From#jid.luser,
    LServer = From#jid.lserver,
    US = {LUser, LServer},
    try {ItemsToSend, VersionToSend} =
	     case {roster_versioning_enabled(LServer),
		   roster_version_on_db(LServer)} of
		 {true, true} when RequestedVersion /= undefined ->
		     case read_roster_version(LUser, LServer) of
			 error ->
			     RosterVersion = write_roster_version(LUser, LServer),
			     {lists:map(fun encode_item/1,
					ejabberd_hooks:run_fold(
					  roster_get, To#jid.lserver, [], [US])),
			      RosterVersion};
			 RequestedVersion ->
			     {false, false};
			 NewVersion ->
			     {lists:map(fun encode_item/1,
					ejabberd_hooks:run_fold(
					  roster_get, To#jid.lserver, [], [US])),
			      NewVersion}
		     end;
		 {true, false} when RequestedVersion /= undefined ->
		     RosterItems = ejabberd_hooks:run_fold(
				     roster_get, To#jid.lserver, [], [US]),
		     case roster_hash(RosterItems) of
			 RequestedVersion ->
			     {false, false};
			 New ->
			     {lists:map(fun encode_item/1, RosterItems), New}
		     end;
		 _ ->
		     {lists:map(fun encode_item/1,
				ejabberd_hooks:run_fold(
				  roster_get, To#jid.lserver, [], [US])),
		      false}
	     end,
	 xmpp:make_iq_result(
	   IQ,
	   case {ItemsToSend, VersionToSend} of
	       {false, false} ->
		   undefined;
	       {Items, false} ->
		   #roster_query{items = Items};
	       {Items, Version} ->
		   #roster_query{items = Items,
				 ver = Version}
	   end)
    catch E:R ->
	    ?ERROR_MSG("failed to process roster get for ~s: ~p",
		       [jid:to_string(From), {E, {R, erlang:get_stacktrace()}}]),
	    Txt = <<"Roster module has failed">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end.

get_user_roster(Acc, {LUser, LServer}) ->
    Items = get_roster(LUser, LServer),
    lists:filter(fun (#roster{subscription = none,
			      ask = in}) ->
			 false;
		     (_) -> true
		 end,
		 Items)
      ++ Acc.

get_roster(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_roster(LUser, LServer).

set_roster(#roster{us = {LUser, LServer}, jid = LJID} = Item) ->
    transaction(
      LServer,
      fun() ->
	      roster_subscribe_t(LUser, LServer, LJID, Item)
      end).

encode_item(Item) ->
    #roster_item{jid = jid:make(Item#roster.jid),
		 name = Item#roster.name,
		 subscription = Item#roster.subscription,
		 ask = case ask_to_pending(Item#roster.ask) of
			   out -> subscribe;
			   both -> subscribe;
			   _ -> undefined
		       end,
		 groups = Item#roster.groups}.

decode_item(#roster_item{} = Item, R, Managed) ->
    R#roster{jid = jid:tolower(Item#roster_item.jid),
	     name = Item#roster_item.name,
	     subscription = case Item#roster_item.subscription of
				remove -> remove;
				Sub when Managed -> Sub;
				_ -> undefined
			    end,
	     groups = Item#roster_item.groups}.

get_roster_by_jid_t(LUser, LServer, LJID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_roster_by_jid(LUser, LServer, LJID).

try_process_iq_set(#iq{from = From, lang = Lang} = IQ) ->
    #jid{server = Server} = From,
    Access = gen_mod:get_module_opt(Server, ?MODULE, access, fun(A) -> A end, all),
    case acl:match_rule(Server, Access, From) of
	deny ->
	    Txt = <<"Denied by ACL">>,
	    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
	allow ->
	    process_iq_set(IQ)
    end.

process_iq_set(#iq{from = From, to = To, id = Id,
		   sub_els = [#roster_query{items = Items}]} = IQ) ->
    Managed = is_managed_from_id(Id),
    lists:foreach(fun (Item) -> process_item_set(From, To, Item, Managed)
		  end,
		  Items),
    xmpp:make_iq_result(IQ).

process_item_set(From, To, #roster_item{jid = JID1} = QueryItem, Managed) ->
    #jid{user = User, luser = LUser, lserver = LServer} = From,
    LJID = jid:tolower(JID1),
    F = fun () ->
		Item = get_roster_by_jid_t(LUser, LServer, LJID),
		Item2 = decode_item(QueryItem, Item, Managed),
		Item3 = ejabberd_hooks:run_fold(roster_process_item,
						LServer, Item2,
						[LServer]),
		case Item3#roster.subscription of
		    remove -> del_roster_t(LUser, LServer, LJID);
		    _ -> update_roster_t(LUser, LServer, LJID, Item3)
		end,
		send_itemset_to_managers(From, Item3, Managed),
		case roster_version_on_db(LServer) of
		    true -> write_roster_version_t(LUser, LServer);
		    false -> ok
		end,
		{Item, Item3}
	end,
    case transaction(LServer, F) of
	{atomic, {OldItem, Item}} ->
	    push_item(User, LServer, To, Item),
	    case Item#roster.subscription of
		remove ->
		    send_unsubscribing_presence(From, OldItem), ok;
		_ -> ok
	    end;
	E ->
	    ?DEBUG("ROSTER: roster item set error: ~p~n", [E]), ok
    end;
process_item_set(_From, _To, _, _Managed) -> ok.

push_item(User, Server, From, Item) ->
    ejabberd_sm:route(jid:make(<<"">>, <<"">>, <<"">>),
		      jid:make(User, Server, <<"">>),
                      {broadcast, {item, Item#roster.jid,
				   Item#roster.subscription}}),
    case roster_versioning_enabled(Server) of
      true ->
	  push_item_version(Server, User, From, Item,
			    roster_version(Server, User));
      false ->
	  lists:foreach(fun (Resource) ->
				push_item(User, Server, Resource, From, Item)
			end,
			ejabberd_sm:get_user_resources(User, Server))
    end.

push_item(User, Server, Resource, From, Item) ->
    push_item(User, Server, Resource, From, Item,
	      not_found).

push_item(User, Server, Resource, From, Item,
	  RosterVersion) ->
    Ver = case RosterVersion of
	      not_found -> undefined;
	      _ -> RosterVersion
	  end,
    ResIQ = #iq{type = set,
%% @doc Roster push, calculate and include the version attribute.
%% TODO: don't push to those who didn't load roster
		id = <<"push", (randoms:get_string())/binary>>,
		sub_els = [#roster_query{ver = Ver,
					 items = [encode_item(Item)]}]},
    ejabberd_router:route(From,
			  jid:make(User, Server, Resource),
			  ResIQ).

push_item_version(Server, User, From, Item,
		  RosterVersion) ->
    lists:foreach(fun (Resource) ->
			  push_item(User, Server, Resource, From, Item,
				    RosterVersion)
		  end,
		  ejabberd_sm:get_user_resources(User, Server)).

get_subscription_lists(_Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Items = Mod:get_only_items(LUser, LServer),
    fill_subscription_lists(LServer, Items, [], []).

fill_subscription_lists(LServer, [I | Is], F, T) ->
    J = element(3, I#roster.usj),
    case I#roster.subscription of
	both ->
	    fill_subscription_lists(LServer, Is, [J | F], [J | T]);
	from ->
	    fill_subscription_lists(LServer, Is, [J | F], T);
	to -> fill_subscription_lists(LServer, Is, F, [J | T]);
	_ -> fill_subscription_lists(LServer, Is, F, T)
    end;
fill_subscription_lists(_LServer, [], F, T) ->
    {F, T}.

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.

roster_subscribe_t(LUser, LServer, LJID, Item) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:roster_subscribe(LUser, LServer, LJID, Item).

transaction(LServer, F) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:transaction(LServer, F).

in_subscription(_, User, Server, JID, Type, Reason) ->
    process_subscription(in, User, Server, JID, Type,
			 Reason).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, <<"">>).

get_roster_by_jid_with_groups_t(LUser, LServer, LJID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_roster_by_jid_with_groups(LUser, LServer, LJID).

process_subscription(Direction, User, Server, JID1,
		     Type, Reason) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LJID = jid:tolower(JID1),
    F = fun () ->
		Item = get_roster_by_jid_with_groups_t(LUser, LServer,
						       LJID),
		NewState = case Direction of
			     out ->
				 out_state_change(Item#roster.subscription,
						  Item#roster.ask, Type);
			     in ->
				 in_state_change(Item#roster.subscription,
						 Item#roster.ask, Type)
			   end,
		AutoReply = case Direction of
			      out -> none;
			      in ->
				  in_auto_reply(Item#roster.subscription,
						Item#roster.ask, Type)
			    end,
		AskMessage = case NewState of
			       {_, both} -> Reason;
			       {_, in} -> Reason;
			       _ -> <<"">>
			     end,
		case NewState of
		  none -> {none, AutoReply};
		  {none, none}
		      when Item#roster.subscription == none,
			   Item#roster.ask == in ->
		      del_roster_t(LUser, LServer, LJID), {none, AutoReply};
		  {Subscription, Pending} ->
		      NewItem = Item#roster{subscription = Subscription,
					    ask = Pending,
					    askmessage =
						iolist_to_binary(AskMessage)},
		      roster_subscribe_t(LUser, LServer, LJID, NewItem),
		      case roster_version_on_db(LServer) of
			true -> write_roster_version_t(LUser, LServer);
			false -> ok
		      end,
		      {{push, NewItem}, AutoReply}
		end
	end,
    case transaction(LServer, F) of
      {atomic, {Push, AutoReply}} ->
	  case AutoReply of
	    none -> ok;
	    _ ->
		ejabberd_router:route(jid:make(User, Server, <<"">>),
				      JID1, #presence{type = AutoReply})
	  end,
	  case Push of
	    {push, Item} ->
		if Item#roster.subscription == none,
		   Item#roster.ask == in ->
		       ok;
		   true ->
		       push_item(User, Server,
				 jid:make(User, Server, <<"">>), Item)
		end,
		true;
	    none -> false
	  end;
      _ -> false
    end.

%% in_state_change(Subscription, Pending, Type) -> NewState
%% NewState = none | {NewSubscription, NewPending}
-ifdef(ROSTER_GATEWAY_WORKAROUND).

-define(NNSD, {to, none}).

-define(NISD, {to, in}).

-else.

-define(NNSD, none).

-define(NISD, none).

-endif.

in_state_change(none, none, subscribe) -> {none, in};
in_state_change(none, none, subscribed) -> ?NNSD;
in_state_change(none, none, unsubscribe) -> none;
in_state_change(none, none, unsubscribed) -> none;
in_state_change(none, out, subscribe) -> {none, both};
in_state_change(none, out, subscribed) -> {to, none};
in_state_change(none, out, unsubscribe) -> none;
in_state_change(none, out, unsubscribed) ->
    {none, none};
in_state_change(none, in, subscribe) -> none;
in_state_change(none, in, subscribed) -> ?NISD;
in_state_change(none, in, unsubscribe) -> {none, none};
in_state_change(none, in, unsubscribed) -> none;
in_state_change(none, both, subscribe) -> none;
in_state_change(none, both, subscribed) -> {to, in};
in_state_change(none, both, unsubscribe) -> {none, out};
in_state_change(none, both, unsubscribed) -> {none, in};
in_state_change(to, none, subscribe) -> {to, in};
in_state_change(to, none, subscribed) -> none;
in_state_change(to, none, unsubscribe) -> none;
in_state_change(to, none, unsubscribed) -> {none, none};
in_state_change(to, in, subscribe) -> none;
in_state_change(to, in, subscribed) -> none;
in_state_change(to, in, unsubscribe) -> {to, none};
in_state_change(to, in, unsubscribed) -> {none, in};
in_state_change(from, none, subscribe) -> none;
in_state_change(from, none, subscribed) -> {both, none};
in_state_change(from, none, unsubscribe) ->
    {none, none};
in_state_change(from, none, unsubscribed) -> none;
in_state_change(from, out, subscribe) -> none;
in_state_change(from, out, subscribed) -> {both, none};
in_state_change(from, out, unsubscribe) -> {none, out};
in_state_change(from, out, unsubscribed) ->
    {from, none};
in_state_change(both, none, subscribe) -> none;
in_state_change(both, none, subscribed) -> none;
in_state_change(both, none, unsubscribe) -> {to, none};
in_state_change(both, none, unsubscribed) ->
    {from, none}.

out_state_change(none, none, subscribe) -> {none, out};
out_state_change(none, none, subscribed) -> none;
out_state_change(none, none, unsubscribe) -> none;
out_state_change(none, none, unsubscribed) -> none;
out_state_change(none, out, subscribe) ->
    {none,
     out}; %% We need to resend query (RFC3921, section 9.2)
out_state_change(none, out, subscribed) -> none;
out_state_change(none, out, unsubscribe) ->
    {none, none};
out_state_change(none, out, unsubscribed) -> none;
out_state_change(none, in, subscribe) -> {none, both};
out_state_change(none, in, subscribed) -> {from, none};
out_state_change(none, in, unsubscribe) -> none;
out_state_change(none, in, unsubscribed) ->
    {none, none};
out_state_change(none, both, subscribe) -> none;
out_state_change(none, both, subscribed) -> {from, out};
out_state_change(none, both, unsubscribe) -> {none, in};
out_state_change(none, both, unsubscribed) ->
    {none, out};
out_state_change(to, none, subscribe) -> none;
out_state_change(to, none, subscribed) -> {both, none};
out_state_change(to, none, unsubscribe) -> {none, none};
out_state_change(to, none, unsubscribed) -> none;
out_state_change(to, in, subscribe) -> none;
out_state_change(to, in, subscribed) -> {both, none};
out_state_change(to, in, unsubscribe) -> {none, in};
out_state_change(to, in, unsubscribed) -> {to, none};
out_state_change(from, none, subscribe) -> {from, out};
out_state_change(from, none, subscribed) -> none;
out_state_change(from, none, unsubscribe) -> none;
out_state_change(from, none, unsubscribed) ->
    {none, none};
out_state_change(from, out, subscribe) -> none;
out_state_change(from, out, subscribed) -> none;
out_state_change(from, out, unsubscribe) ->
    {from, none};
out_state_change(from, out, unsubscribed) ->
    {none, out};
out_state_change(both, none, subscribe) -> none;
out_state_change(both, none, subscribed) -> none;
out_state_change(both, none, unsubscribe) ->
    {from, none};
out_state_change(both, none, unsubscribed) ->
    {to, none}.

in_auto_reply(from, none, subscribe) -> subscribed;
in_auto_reply(from, out, subscribe) -> subscribed;
in_auto_reply(both, none, subscribe) -> subscribed;
in_auto_reply(none, in, unsubscribe) -> unsubscribed;
in_auto_reply(none, both, unsubscribe) -> unsubscribed;
in_auto_reply(to, in, unsubscribe) -> unsubscribed;
in_auto_reply(from, none, unsubscribe) -> unsubscribed;
in_auto_reply(from, out, unsubscribe) -> unsubscribed;
in_auto_reply(both, none, unsubscribe) -> unsubscribed;
in_auto_reply(_, _, _) -> none.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    send_unsubscription_to_rosteritems(LUser, LServer),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer).

%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
send_unsubscription_to_rosteritems(LUser, LServer) ->
    RosterItems = get_user_roster([], {LUser, LServer}),
    From = jid:make({LUser, LServer, <<"">>}),
    lists:foreach(fun (RosterItem) ->
			  send_unsubscribing_presence(From, RosterItem)
		  end,
		  RosterItems).

send_unsubscribing_presence(From, Item) ->
    IsTo = case Item#roster.subscription of
	     both -> true;
	     to -> true;
	     _ -> false
	   end,
    IsFrom = case Item#roster.subscription of
	       both -> true;
	       from -> true;
	       _ -> false
	     end,
    if IsTo ->
	    ejabberd_router:route(jid:remove_resource(From),
				  jid:make(Item#roster.jid),
				  #presence{type = unsubscribe});
       true -> ok
    end,
    if IsFrom ->
	    ejabberd_router:route(jid:remove_resource(From),
				  jid:make(Item#roster.jid),
				  #presence{type = unsubscribed});
       true -> ok
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec set_items(binary(), binary(), roster_query()) -> any().
set_items(User, Server, #roster_query{items = Items}) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    F = fun () ->
		lists:foreach(fun (Item) ->
				      process_item_set_t(LUser, LServer, Item)
			      end, Items)
	end,
    transaction(LServer, F).

update_roster_t(LUser, LServer, LJID, Item) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:update_roster(LUser, LServer, LJID, Item).

del_roster_t(LUser, LServer, LJID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:del_roster(LUser, LServer, LJID).

process_item_set_t(LUser, LServer, #roster_item{jid = JID1} = QueryItem) ->
    JID = {JID1#jid.user, JID1#jid.server,
	   JID1#jid.resource},
    LJID = {JID1#jid.luser, JID1#jid.lserver,
	    JID1#jid.lresource},
    Item = #roster{usj = {LUser, LServer, LJID},
		   us = {LUser, LServer}, jid = JID},
    Item2 = decode_item(QueryItem, Item, _Managed = true),
    case Item2#roster.subscription of
	remove -> del_roster_t(LUser, LServer, LJID);
	_ -> update_roster_t(LUser, LServer, LJID, Item2)
    end;
process_item_set_t(_LUser, _LServer, _) -> ok.

get_in_pending_subscriptions(Ls, User, Server) ->
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    get_in_pending_subscriptions(Ls, User, Server, Mod).

get_in_pending_subscriptions(Ls, User, Server, Mod) ->
    JID = jid:make(User, Server, <<"">>),
    Result = Mod:get_only_items(JID#jid.luser, JID#jid.lserver),
    Ls ++ lists:flatmap(
	    fun(#roster{ask = Ask} = R) when Ask == in; Ask == both ->
		    Message = R#roster.askmessage,
		    Status = if is_binary(Message) -> (Message);
				true -> <<"">>
			     end,
		    [#presence{from = R#roster.jid, to = JID,
			       type = subscribe,
			       status = xmpp:mk_text(Status)}];
	       (_) ->
		    []
	    end, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_subscription_and_groups(User, Server, LJID) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:read_subscription_and_groups(LUser, LServer, LJID).

get_jid_info(_, User, Server, JID) ->
    LJID = jid:tolower(JID),
    case read_subscription_and_groups(User, Server, LJID) of
      {Subscription, Groups} -> {Subscription, Groups};
      error ->
	  LRJID = jid:tolower(jid:remove_resource(JID)),
	  if LRJID == LJID -> {none, []};
	     true ->
		 case read_subscription_and_groups(User, Server, LRJID)
		     of
		   {Subscription, Groups} -> {Subscription, Groups};
		   error -> {none, []}
		 end
	  end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

webadmin_page(_, Host,
	      #request{us = _US, path = [<<"user">>, U, <<"roster">>],
		       q = Query, lang = Lang} =
		  _Request) ->
    Res = user_roster(U, Host, Query, Lang), {stop, Res};
webadmin_page(Acc, _, _) -> Acc.

user_roster(User, Server, Query, Lang) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    US = {LUser, LServer},
    Items1 = get_roster(LUser, LServer),
    Res = user_roster_parse_query(User, Server, Items1,
				  Query),
    Items = get_roster(LUser, LServer),
    SItems = lists:sort(Items),
    FItems = case SItems of
	       [] -> [?CT(<<"None">>)];
	       _ ->
		   [?XE(<<"table">>,
			[?XE(<<"thead">>,
			     [?XE(<<"tr">>,
				  [?XCT(<<"td">>, <<"Jabber ID">>),
				   ?XCT(<<"td">>, <<"Nickname">>),
				   ?XCT(<<"td">>, <<"Subscription">>),
				   ?XCT(<<"td">>, <<"Pending">>),
				   ?XCT(<<"td">>, <<"Groups">>)])]),
			 ?XE(<<"tbody">>,
			     (lists:map(fun (R) ->
						Groups = lists:flatmap(fun
									 (Group) ->
									     [?C(Group),
									      ?BR]
								       end,
								       R#roster.groups),
						Pending =
						    ask_to_pending(R#roster.ask),
						TDJID =
						    build_contact_jid_td(R#roster.jid),
						?XE(<<"tr">>,
						    [TDJID,
						     ?XAC(<<"td">>,
							  [{<<"class">>,
							    <<"valign">>}],
							  (R#roster.name)),
						     ?XAC(<<"td">>,
							  [{<<"class">>,
							    <<"valign">>}],
							  (iolist_to_binary(atom_to_list(R#roster.subscription)))),
						     ?XAC(<<"td">>,
							  [{<<"class">>,
							    <<"valign">>}],
							  (iolist_to_binary(atom_to_list(Pending)))),
						     ?XAE(<<"td">>,
							  [{<<"class">>,
							    <<"valign">>}],
							  Groups),
						     if Pending == in ->
							    ?XAE(<<"td">>,
								 [{<<"class">>,
								   <<"valign">>}],
								 [?INPUTT(<<"submit">>,
									  <<"validate",
									    (ejabberd_web_admin:term_to_id(R#roster.jid))/binary>>,
									  <<"Validate">>)]);
							true -> ?X(<<"td">>)
						     end,
						     ?XAE(<<"td">>,
							  [{<<"class">>,
							    <<"valign">>}],
							  [?INPUTT(<<"submit">>,
								   <<"remove",
								     (ejabberd_web_admin:term_to_id(R#roster.jid))/binary>>,
								   <<"Remove">>)])])
					end,
					SItems)))])]
	     end,
    [?XC(<<"h1">>,
	 (<<(?T(<<"Roster of ">>))/binary, (us_to_list(US))/binary>>))]
      ++
      case Res of
	ok -> [?XREST(<<"Submitted">>)];
	error -> [?XREST(<<"Bad format">>)];
	nothing -> []
      end
	++
	[?XAE(<<"form">>,
	      [{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
	      (FItems ++
		 [?P, ?INPUT(<<"text">>, <<"newjid">>, <<"">>),
		  ?C(<<" ">>),
		  ?INPUTT(<<"submit">>, <<"addjid">>,
			  <<"Add Jabber ID">>)]))].

build_contact_jid_td(RosterJID) ->
    ContactJID = jid:make(RosterJID),
    JIDURI = case {ContactJID#jid.luser,
		   ContactJID#jid.lserver}
		 of
	       {<<"">>, _} -> <<"">>;
	       {CUser, CServer} ->
		   case lists:member(CServer, ?MYHOSTS) of
		     false -> <<"">>;
		     true ->
			 <<"/admin/server/", CServer/binary, "/user/",
			   CUser/binary, "/">>
		   end
	     end,
    case JIDURI of
      <<>> ->
	  ?XAC(<<"td">>, [{<<"class">>, <<"valign">>}],
	       (jid:to_string(RosterJID)));
      URI when is_binary(URI) ->
	  ?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
	       [?AC(JIDURI, (jid:to_string(RosterJID)))])
    end.

user_roster_parse_query(User, Server, Items, Query) ->
    case lists:keysearch(<<"addjid">>, 1, Query) of
      {value, _} ->
	  case lists:keysearch(<<"newjid">>, 1, Query) of
	    {value, {_, SJID}} ->
		case jid:from_string(SJID) of
		  JID when is_record(JID, jid) ->
		      user_roster_subscribe_jid(User, Server, JID), ok;
		  error -> error
		end;
	    false -> error
	  end;
      false ->
	  case catch user_roster_item_parse_query(User, Server,
						  Items, Query)
	      of
	    submitted -> ok;
	    {'EXIT', _Reason} -> error;
	    _ -> nothing
	  end
    end.

user_roster_subscribe_jid(User, Server, JID) ->
    out_subscription(User, Server, JID, subscribe),
    UJID = jid:make(User, Server, <<"">>),
    ejabberd_router:route(UJID, JID, #presence{type = subscribe}).

user_roster_item_parse_query(User, Server, Items,
			     Query) ->
    lists:foreach(fun (R) ->
			  JID = R#roster.jid,
			  case lists:keysearch(<<"validate",
						 (ejabberd_web_admin:term_to_id(JID))/binary>>,
					       1, Query)
			      of
			    {value, _} ->
				JID1 = jid:make(JID),
				out_subscription(User, Server, JID1,
						 subscribed),
				UJID = jid:make(User, Server, <<"">>),
				ejabberd_router:route(UJID, JID1,
						      #presence{type = subscribed}),
				throw(submitted);
			    false ->
				case lists:keysearch(<<"remove",
						       (ejabberd_web_admin:term_to_id(JID))/binary>>,
						     1, Query)
				    of
				  {value, _} ->
				      UJID = jid:make(User, Server),
				      RosterItem = #roster_item{
						      jid = jid:make(JID),
						      subscription = remove},
				      process_iq_set(
					#iq{type = set,
					    from = UJID,
					    to = UJID,
					    id = randoms:get_string(),
					    sub_els = [#roster_query{
							  items = [RosterItem]}]}),
				      throw(submitted);
				  false -> ok
				end
			  end
		  end,
		  Items),
    nothing.

us_to_list({User, Server}) ->
    jid:to_string({User, Server, <<"">>}).

webadmin_user(Acc, _User, _Server, Lang) ->
    Acc ++
      [?XE(<<"h3">>, [?ACT(<<"roster/">>, <<"Roster">>)])].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Implement XEP-0321 Remote Roster Management

process_iq_manager(#iq{from = From, to = To, lang = Lang} = IQ) ->
    %% Check what access is allowed for From to To
    MatchDomain = From#jid.lserver,
    case is_domain_managed(MatchDomain, To#jid.lserver) of
	true ->
	    process_iq_manager2(MatchDomain, IQ);
	false ->
	    Txt = <<"Roster management is not allowed from this domain">>,
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end.

process_iq_manager2(MatchDomain, #iq{to = To} = IQ) ->
    %% If IQ is SET, filter the input IQ
    IQFiltered = maybe_filter_request(MatchDomain, IQ),
    %% Call the standard function with reversed JIDs
    IdInitial = IQFiltered#iq.id,
    ResIQ = process_iq(IQFiltered#iq{from = To, to = To,
				     id = <<"roster-remotely-managed">>}),
    %% Filter the output IQ
    filter_stanza(MatchDomain, ResIQ#iq{id = IdInitial}).

is_domain_managed(ContactHost, UserHost) ->
    Managers = gen_mod:get_module_opt(UserHost, ?MODULE, managers,
						fun(B) when is_list(B) -> B end,
						[]),
    lists:member(ContactHost, Managers).

maybe_filter_request(MatchDomain, IQ) when IQ#iq.type == set ->
    filter_stanza(MatchDomain, IQ);
maybe_filter_request(_MatchDomain, IQ) ->
    IQ.

filter_stanza(MatchDomain,
	      #iq{sub_els = [#roster_query{items = Items} = R]} = IQ) ->
    ItemsFiltered = lists:filter(
		      fun(#roster_item{jid = #jid{lserver = S}}) ->
			      S == MatchDomain
		      end, Items),
    IQ#iq{sub_els = [R#roster_query{items = ItemsFiltered}]}.

send_itemset_to_managers(_From, _Item, true) ->
    ok;
send_itemset_to_managers(From, Item, false) ->
    {_, UserHost} = Item#roster.us,
    {_ContactUser, ContactHost, _ContactResource} = Item#roster.jid,
    %% Check if the component is an allowed manager
    IsManager = is_domain_managed(ContactHost, UserHost),
    case IsManager of
	true -> push_item(<<"">>, ContactHost, <<"">>, From, Item);
	false -> ok
    end.

is_managed_from_id(<<"roster-remotely-managed">>) ->
    true;
is_managed_from_id(_Id) ->
    false.

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, R) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, R).

mod_opt_type(access) ->
    fun acl:access_rules_validator/1;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(managers) ->
    fun (B) when is_list(B) -> B end;
mod_opt_type(store_current_id) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(versioning) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) ->
    [access, db_type, iqdisc, managers, store_current_id,
     versioning].
