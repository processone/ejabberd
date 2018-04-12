%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Roster management
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_iq/1, export/1,
	 import_info/0, process_local_iq/1, get_user_roster/2,
	 import/5, get_roster/2, push_item/3,
	 import_start/2, import_stop/2,
	 c2s_self_presence/1, in_subscription/2,
	 out_subscription/1, set_items/3, remove_user/2,
	 get_jid_info/4, encode_item/1, webadmin_page/3,
	 webadmin_user/4, get_versioning_feature/2,
	 roster_versioning_enabled/1, roster_version/2,
	 mod_opt_type/1, mod_options/1, set_roster/1, del_roster/3,
	 depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("mod_roster.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-define(ROSTER_CACHE, roster_cache).
-define(ROSTER_ITEM_CACHE, roster_item_cache).
-define(ROSTER_VERSION_CACHE, roster_version_cache).

-export_type([subscription/0]).

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), #roster{} | [binary()]) -> ok.
-callback read_roster_version(binary(), binary()) -> {ok, binary()} | error.
-callback write_roster_version(binary(), binary(), boolean(), binary()) -> any().
-callback get_roster(binary(), binary()) -> {ok, [#roster{}]} | error.
-callback get_roster_item(binary(), binary(), ljid()) -> {ok, #roster{}} | error.
-callback read_subscription_and_groups(binary(), binary(), ljid())
          -> {ok, {subscription(), ask(), [binary()]}} | error.
-callback roster_subscribe(binary(), binary(), ljid(), #roster{}) -> any().
-callback transaction(binary(), function()) -> {atomic, any()} | {aborted, any()}.
-callback remove_user(binary(), binary()) -> any().
-callback update_roster(binary(), binary(), ljid(), #roster{}) -> any().
-callback del_roster(binary(), binary(), ljid()) -> any().
-callback use_cache(binary(), roster | roster_version) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-optional_callbacks([use_cache/2, cache_nodes/1]).

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    init_cache(Mod, Host, Opts),
    ejabberd_hooks:add(roster_get, Host, ?MODULE,
		       get_user_roster, 50),
    ejabberd_hooks:add(roster_in_subscription, Host,
		       ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, Host,
		       ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(roster_get_jid_info, Host, ?MODULE,
		       get_jid_info, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(c2s_self_presence, Host, ?MODULE,
		       c2s_self_presence, 50),
    ejabberd_hooks:add(c2s_post_auth_features, Host,
		       ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:add(webadmin_page_host, Host, ?MODULE,
		       webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host, ?MODULE,
		       webadmin_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_ROSTER, ?MODULE, process_iq).

stop(Host) ->
    ejabberd_hooks:delete(roster_get, Host, ?MODULE,
			  get_user_roster, 50),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, Host,
			  ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
			  ?MODULE, get_jid_info, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(c2s_self_presence, Host, ?MODULE,
			  c2s_self_presence, 50),
    ejabberd_hooks:delete(c2s_post_auth_features,
			  Host, ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host, ?MODULE,
			  webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host, ?MODULE,
			  webadmin_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_ROSTER).

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end.

depends(_Host, _Opts) ->
    [].

process_iq(#iq{from = #jid{luser = U, lserver = S},
	       to =   #jid{luser = U, lserver = S}} = IQ) ->
    process_local_iq(IQ);
process_iq(#iq{lang = Lang, to = To} = IQ) ->
    case ejabberd_hooks:run_fold(roster_remote_access,
				 To#jid.lserver, false, [IQ]) of
	false ->
	    Txt = <<"Query to another users is forbidden">>,
	    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang));
	true ->
	    process_local_iq(IQ)
    end.

process_local_iq(#iq{type = set,lang = Lang,
		     sub_els = [#roster_query{
				   items = [#roster_item{ask = Ask}]}]} = IQ)
  when Ask /= undefined ->
    Txt = <<"Possessing 'ask' attribute is not allowed by RFC6121">>,
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
process_local_iq(#iq{type = set, from = From, lang = Lang,
		     sub_els = [#roster_query{
				   items = [#roster_item{} = Item]}]} = IQ) ->
    case has_duplicated_groups(Item#roster_item.groups) of
	true ->
	    Txt = <<"Duplicated groups are not allowed by RFC6121">>,
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	false ->
	    #jid{lserver = LServer} = From,
	    Access = gen_mod:get_module_opt(LServer, ?MODULE, access),
	    case acl:match_rule(LServer, Access, From) of
		deny ->
		    Txt = <<"Access denied by service policy">>,
		    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
		allow ->
		    process_iq_set(IQ)
	    end
    end;
process_local_iq(#iq{type = set, lang = Lang,
		     sub_els = [#roster_query{items = [_|_]}]} = IQ) ->
    Txt = <<"Multiple <item/> elements are not allowed by RFC6121">>,
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
process_local_iq(#iq{type = get, lang = Lang,
		     sub_els = [#roster_query{items = Items}]} = IQ) ->
    case Items of
	[] ->
	    process_iq_get(IQ);
	[_|_] ->
	    Txt = <<"The query must not contain <item/> elements">>,
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end;
process_local_iq(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

roster_hash(Items) ->
    str:sha(term_to_binary(lists:sort([R#roster{groups =
						    lists:sort(Grs)}
				       || R = #roster{groups = Grs}
					      <- Items]))).

roster_versioning_enabled(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, versioning).

roster_version_on_db(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, store_current_id).

%% Returns a list that may contain an xmlelement with the XEP-237 feature if it's enabled.
-spec get_versioning_feature([xmpp_element()], binary()) -> [xmpp_element()].
get_versioning_feature(Acc, Host) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
	true ->
    case roster_versioning_enabled(Host) of
      true ->
	  [#rosterver_feature{}|Acc];
		false ->
		    Acc
	    end;
	false ->
	    Acc
    end.

roster_version(LServer, LUser) ->
    US = {LUser, LServer},
    case roster_version_on_db(LServer) of
      true ->
	  case read_roster_version(LUser, LServer) of
	    error -> not_found;
	    {ok, V} -> V
	  end;
      false ->
	  roster_hash(ejabberd_hooks:run_fold(roster_get, LServer,
					      [], [US]))
    end.

read_roster_version(LUser, LServer) ->
    ets_cache:lookup(
      ?ROSTER_VERSION_CACHE, {LUser, LServer},
      fun() ->
	      Mod = gen_mod:db_mod(LServer, ?MODULE),
	      Mod:read_roster_version(LUser, LServer)
      end).

write_roster_version(LUser, LServer) ->
    write_roster_version(LUser, LServer, false).

write_roster_version_t(LUser, LServer) ->
    write_roster_version(LUser, LServer, true).

write_roster_version(LUser, LServer, InTransaction) ->
    Ver = str:sha(term_to_binary(p1_time_compat:unique_integer())),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:write_roster_version(LUser, LServer, InTransaction, Ver),
    if InTransaction -> ok;
       true ->
	    ets_cache:delete(?ROSTER_VERSION_CACHE, {LUser, LServer},
			     cache_nodes(Mod, LServer))
    end,
    Ver.

%% Load roster from DB only if necessary.
%% It is necessary if
%%     - roster versioning is disabled in server OR
%%     - roster versioning is not used by the client OR
%%     - roster versioning is used by server and client, BUT the server isn't storing versions on db OR
%%     - the roster version from client don't match current version.
process_iq_get(#iq{to = To, lang = Lang,
		   sub_els = [#roster_query{ver = RequestedVersion}]} = IQ) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
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
			 {ok, RequestedVersion} ->
			     {false, false};
			 {ok, NewVersion} ->
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
		       [jid:encode(To), {E, {R, erlang:get_stacktrace()}}]),
	    Txt = <<"Roster module has failed">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end.

-spec get_user_roster([#roster{}], {binary(), binary()}) -> [#roster{}].
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
    R = case use_cache(Mod, LServer, roster) of
	    true ->
		ets_cache:lookup(
		  ?ROSTER_CACHE, {LUser, LServer},
		  fun() -> Mod:get_roster(LUser, LServer) end);
	    false ->
		Mod:get_roster(LUser, LServer)
	end,
    case R of
	{ok, Items} -> Items;
	error -> []
    end.

get_roster_item(LUser, LServer, LJID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:get_roster_item(LUser, LServer, LJID) of
	{ok, Item} ->
	    Item;
	error ->
	    LBJID = jid:remove_resource(LJID),
	    #roster{usj = {LUser, LServer, LBJID},
		    us = {LUser, LServer}, jid = LBJID}
    end.

get_subscription_and_groups(LUser, LServer, LJID) ->
    LBJID = jid:remove_resource(LJID),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Res = case use_cache(Mod, LServer, roster) of
	      true ->
		  ets_cache:lookup(
		    ?ROSTER_ITEM_CACHE, {LUser, LServer, LBJID},
		    fun() ->
			    Items = get_roster(LUser, LServer),
			    case lists:keyfind(LBJID, #roster.jid, Items) of
				#roster{subscription = Sub,
					ask = Ask,
					groups = Groups} ->
				    {ok, {Sub, Ask, Groups}};
				false ->
				    error
			    end
		    end);
	      false ->
		  case Mod:read_subscription_and_groups(LUser, LServer, LBJID) of
		      {ok, {Sub, Groups}} ->
			  %% Backward compatibility for third-party backends
			  {ok, {Sub, none, Groups}};
		      Other ->
			  Other
		  end
	  end,
    case Res of
	{ok, SubAndGroups} ->
	    SubAndGroups;
	error ->
	    {none, none, []}
    end.

set_roster(#roster{us = {LUser, LServer}, jid = LJID} = Item) ->
    transaction(
      LUser, LServer, [LJID],
      fun() ->
	      update_roster_t(LUser, LServer, LJID, Item)
      end).

del_roster(LUser, LServer, LJID) ->
    transaction(
      LUser, LServer, [LJID],
      fun() ->
	      del_roster_t(LUser, LServer, LJID)
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

decode_item(#roster_item{subscription = remove} = Item, R, _) ->
    R#roster{jid = jid:tolower(Item#roster_item.jid),
	     name = <<"">>,
	     subscription = remove,
	     ask = none,
	     groups = [],
	     askmessage = <<"">>,
	     xs = []};
decode_item(Item, R, Managed) ->
    R#roster{jid = jid:tolower(Item#roster_item.jid),
	     name = Item#roster_item.name,
	     subscription = case Item#roster_item.subscription of
				Sub when Managed -> Sub;
				_ -> R#roster.subscription
			    end,
	     groups = Item#roster_item.groups}.

process_iq_set(#iq{from = _From, to = To,
		   sub_els = [#roster_query{items = [QueryItem]}]} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = To,
    LJID = jid:tolower(QueryItem#roster_item.jid),
    F = fun () ->
		Item = get_roster_item(LUser, LServer, LJID),
		Item2 = decode_item(QueryItem, Item, false),
		Item3 = ejabberd_hooks:run_fold(roster_process_item,
						LServer, Item2,
						[LServer]),
		case Item3#roster.subscription of
		    remove -> del_roster_t(LUser, LServer, LJID);
		    _ -> update_roster_t(LUser, LServer, LJID, Item3)
		end,
		case roster_version_on_db(LServer) of
		    true -> write_roster_version_t(LUser, LServer);
		    false -> ok
		end,
		{Item, Item3}
	end,
    case transaction(LUser, LServer, [LJID], F) of
	{atomic, {OldItem, Item}} ->
	    push_item(To, OldItem, Item),
	    case Item#roster.subscription of
		remove ->
		    send_unsubscribing_presence(To, OldItem);
		_ ->
		    ok
	    end,
	    xmpp:make_iq_result(IQ);
	E ->
	    ?ERROR_MSG("roster set failed:~nIQ = ~s~nError = ~p",
		       [xmpp:pp(IQ), E]),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error())
    end.

push_item(To, OldItem, NewItem) ->
    #jid{luser = LUser, lserver = LServer} = To,
    Ver = case roster_versioning_enabled(LServer) of
	      true -> roster_version(LServer, LUser);
	      false -> undefined
	  end,
    lists:foreach(
      fun(Resource) ->
	      To1 = jid:replace_resource(To, Resource),
	      push_item(To1, OldItem, NewItem, Ver)
      end, ejabberd_sm:get_user_resources(LUser, LServer)).

push_item(To, OldItem, NewItem, Ver) ->
    route_presence_change(To, OldItem, NewItem),
    IQ = #iq{type = set, to = To,
	     from = jid:remove_resource(To),
	     id = <<"push", (randoms:get_string())/binary>>,
	     sub_els = [#roster_query{ver = Ver,
				      items = [encode_item(NewItem)]}]},
    ejabberd_router:route(IQ).

-spec route_presence_change(jid(), #roster{}, #roster{}) -> ok.
route_presence_change(From, OldItem, NewItem) ->
    OldSub = OldItem#roster.subscription,
    NewSub = NewItem#roster.subscription,
    To = jid:make(NewItem#roster.jid),
    NewIsFrom = NewSub == both orelse NewSub == from,
    OldIsFrom = OldSub == both orelse OldSub == from,
    if NewIsFrom andalso not OldIsFrom ->
	    case ejabberd_sm:get_session_pid(
		   From#jid.luser, From#jid.lserver, From#jid.lresource) of
		none ->
		    ok;
		Pid ->
		    ejabberd_c2s:resend_presence(Pid, To)
	    end;
       OldIsFrom andalso not NewIsFrom ->
	    PU = #presence{from = From, to = To, type = unavailable},
	    case ejabberd_hooks:run_fold(
		   privacy_check_packet, allow,
		   [From, PU, out]) of
		deny ->
		    ok;
		allow ->
		    ejabberd_router:route(PU)
	    end;
       true ->
	    ok
    end.

ask_to_pending(subscribe) -> out;
ask_to_pending(unsubscribe) -> none;
ask_to_pending(Ask) -> Ask.

roster_subscribe_t(LUser, LServer, LJID, Item) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:roster_subscribe(LUser, LServer, LJID, Item).

transaction(LUser, LServer, LJIDs, F) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:transaction(LServer, F) of
	{atomic, _} = Result ->
	    delete_cache(Mod, LUser, LServer, LJIDs),
	    Result;
	Err ->
	    Err
    end.

-spec in_subscription(boolean(), presence()) -> boolean().
in_subscription(_, #presence{from = JID, to = To,
			     type = Type, status = Status}) ->
    #jid{user = User, server = Server} = To,
    Reason = if Type == subscribe -> xmpp:get_text(Status);
		true -> <<"">>
	     end,
    process_subscription(in, User, Server, JID, Type,
			 Reason).

-spec out_subscription(presence()) -> boolean().
out_subscription(#presence{from = From, to = JID, type = Type}) ->
    #jid{user = User, server = Server} = From,
    process_subscription(out, User, Server, JID, Type, <<"">>).

process_subscription(Direction, User, Server, JID1,
		     Type, Reason) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LJID = jid:tolower(jid:remove_resource(JID1)),
    F = fun () ->
		Item = get_roster_item(LUser, LServer, LJID),
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
		    none ->
			{none, AutoReply};
		    {none, none} when Item#roster.subscription == none,
				      Item#roster.ask == in ->
			del_roster_t(LUser, LServer, LJID), {none, AutoReply};
		    {Subscription, Pending} ->
			NewItem = Item#roster{subscription = Subscription,
					      ask = Pending,
					      askmessage = AskMessage},
			roster_subscribe_t(LUser, LServer, LJID, NewItem),
			case roster_version_on_db(LServer) of
			    true -> write_roster_version_t(LUser, LServer);
			    false -> ok
			end,
			{{push, Item, NewItem}, AutoReply}
		end
	end,
    case transaction(LUser, LServer, [LJID], F) of
	{atomic, {Push, AutoReply}} ->
	    case AutoReply of
		none -> ok;
		_ ->
		    ejabberd_router:route(
		      #presence{type = AutoReply,
				from = jid:make(User, Server),
				to = JID1})
	    end,
	    case Push of
		{push, OldItem, NewItem} ->
		    if NewItem#roster.subscription == none,
		       NewItem#roster.ask == in ->
			    ok;
		       true ->
			    push_item(jid:make(User, Server), OldItem, NewItem)
		    end,
		    true;
		none ->
		    false
	    end;
	_ ->
	    false
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

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Items = get_user_roster([], {LUser, LServer}),
    send_unsubscription_to_rosteritems(LUser, LServer, Items),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer),
    delete_cache(Mod, LUser, LServer, [Item#roster.jid || Item <- Items]).

%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
send_unsubscription_to_rosteritems(LUser, LServer, RosterItems) ->
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
	    ejabberd_router:route(
	      #presence{type = unsubscribe,
			from = jid:remove_resource(From),
			to = jid:make(Item#roster.jid)});
       true -> ok
    end,
    if IsFrom ->
	    ejabberd_router:route(
	      #presence{type = unsubscribed,
			from = jid:remove_resource(From),
			to = jid:make(Item#roster.jid)});
       true -> ok
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec set_items(binary(), binary(), roster_query()) -> any().
set_items(User, Server, #roster_query{items = Items}) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LJIDs = [jid:tolower(Item#roster_item.jid) || Item <- Items],
    F = fun () ->
		lists:foreach(
		  fun(Item) ->
			  process_item_set_t(LUser, LServer, Item)
		  end, Items)
	end,
    transaction(LUser, LServer, LJIDs, F).

update_roster_t(LUser, LServer, LJID, Item) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:update_roster(LUser, LServer, LJID, Item).

del_roster_t(LUser, LServer, LJID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:del_roster(LUser, LServer, LJID).

process_item_set_t(LUser, LServer, #roster_item{jid = JID1} = QueryItem) ->
    JID = {JID1#jid.user, JID1#jid.server, <<>>},
    LJID = {JID1#jid.luser, JID1#jid.lserver, <<>>},
    Item = #roster{usj = {LUser, LServer, LJID},
		   us = {LUser, LServer}, jid = JID},
    Item2 = decode_item(QueryItem, Item, _Managed = true),
    case Item2#roster.subscription of
	remove -> del_roster_t(LUser, LServer, LJID);
	_ -> update_roster_t(LUser, LServer, LJID, Item2)
    end;
process_item_set_t(_LUser, _LServer, _) -> ok.

-spec c2s_self_presence({presence(), ejabberd_c2s:state()})
      -> {presence(), ejabberd_c2s:state()}.
c2s_self_presence({_, #{pres_last := _}} = Acc) ->
    Acc;
c2s_self_presence({#presence{type = available} = Pkt, State}) ->
    Prio = get_priority_from_presence(Pkt),
    if Prio >= 0 ->
	    State1 = resend_pending_subscriptions(State),
	    {Pkt, State1};
       true ->
	    {Pkt, State}
    end;
c2s_self_presence(Acc) ->
    Acc.

-spec resend_pending_subscriptions(ejabberd_c2s:state()) -> ejabberd_c2s:state().
resend_pending_subscriptions(#{jid := JID} = State) ->
    BareJID = jid:remove_resource(JID),
    Result = get_roster(JID#jid.luser, JID#jid.lserver),
    lists:foldl(
      fun(#roster{ask = Ask} = R, AccState) when Ask == in; Ask == both ->
		    Message = R#roster.askmessage,
		    Status = if is_binary(Message) -> (Message);
				true -> <<"">>
			     end,
	      Sub = #presence{from = jid:make(R#roster.jid),
			      to = BareJID,
			      type = subscribe,
			      status = xmpp:mk_text(Status)},
	      ejabberd_c2s:send(AccState, Sub);
	 (_, AccState) ->
	      AccState
      end, State, Result).

-spec get_priority_from_presence(presence()) -> integer().
get_priority_from_presence(#presence{priority = Prio}) ->
    case Prio of
	undefined -> 0;
	_ -> Prio
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_jid_info({subscription(), ask(), [binary()]}, binary(), binary(), jid())
      -> {subscription(), ask(), [binary()]}.
get_jid_info(_, User, Server, JID) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LJID = jid:tolower(JID),
    get_subscription_and_groups(LUser, LServer, LJID).

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
	       (jid:encode(RosterJID)));
      URI when is_binary(URI) ->
	  ?XAE(<<"td">>, [{<<"class">>, <<"valign">>}],
	       [?AC(JIDURI, (jid:encode(RosterJID)))])
    end.

user_roster_parse_query(User, Server, Items, Query) ->
    case lists:keysearch(<<"addjid">>, 1, Query) of
      {value, _} ->
	  case lists:keysearch(<<"newjid">>, 1, Query) of
	    {value, {_, SJID}} ->
		try jid:decode(SJID) of
		  JID ->
		      user_roster_subscribe_jid(User, Server, JID), ok
		catch _:{bad_jid, _} ->
			error
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
    UJID = jid:make(User, Server),
    Presence = #presence{from = UJID, to = JID, type = subscribe},
    out_subscription(Presence),
    ejabberd_router:route(Presence).

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
				UJID = jid:make(User, Server),
				Pres = #presence{from = UJID, to = JID1,
						 type = subscribed},
				out_subscription(Pres),
				ejabberd_router:route(Pres),
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
    jid:encode({User, Server, <<"">>}).

webadmin_user(Acc, _User, _Server, Lang) ->
    Acc ++
      [?XE(<<"h3">>, [?ACT(<<"roster/">>, <<"Roster">>)])].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_duplicated_groups(Groups) ->
    GroupsPrep = lists:usort([jid:resourceprep(G) || G <- Groups]),
    not (length(GroupsPrep) == length(Groups)).

-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
    CacheOpts = cache_opts(Opts),
    case use_cache(Mod, Host, roster_version) of
	true ->
	    ets_cache:new(?ROSTER_VERSION_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?ROSTER_VERSION_CACHE)
    end,
    case use_cache(Mod, Host, roster) of
	true ->
	    ets_cache:new(?ROSTER_CACHE, CacheOpts),
	    ets_cache:new(?ROSTER_ITEM_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?ROSTER_CACHE),
	    ets_cache:delete(?ROSTER_ITEM_CACHE)
    end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = gen_mod:get_opt(cache_size, Opts),
    CacheMissed = gen_mod:get_opt(cache_missed, Opts),
    LifeTime = case gen_mod:get_opt(cache_life_time, Opts) of
		   infinity -> infinity;
		   I -> timer:seconds(I)
	       end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary(), roster | roster_version) -> boolean().
use_cache(Mod, Host, Table) ->
    case erlang:function_exported(Mod, use_cache, 2) of
	true -> Mod:use_cache(Host, Table);
	false -> gen_mod:get_module_opt(Host, ?MODULE, use_cache)
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
	true -> Mod:cache_nodes(Host);
	false -> ejabberd_cluster:get_nodes()
    end.

-spec delete_cache(module(), binary(), binary(), [ljid()]) -> ok.
delete_cache(Mod, LUser, LServer, LJIDs) ->
    case use_cache(Mod, LServer, roster_version) of
	true ->
	    ets_cache:delete(?ROSTER_VERSION_CACHE, {LUser, LServer},
			     cache_nodes(Mod, LServer));
	false ->
	    ok
    end,
    case use_cache(Mod, LServer, roster) of
	true ->
	    Nodes = cache_nodes(Mod, LServer),
	    ets_cache:delete(?ROSTER_CACHE, {LUser, LServer}, Nodes),
	    lists:foreach(
	      fun(LJID) ->
		      ets_cache:delete(
			?ROSTER_ITEM_CACHE,
			{LUser, LServer, jid:remove_resource(LJID)},
			Nodes)
	      end, LJIDs);
	false ->
	    ok
    end.

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import_info() ->
    [{<<"roster_version">>, 2},
     {<<"rostergroups">>, 3},
     {<<"rosterusers">>, 10}].

import_start(LServer, DBType) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    ets:new(rostergroups_tmp, [private, named_table, bag]),
    Mod:init(LServer, []),
    ok.

import_stop(_LServer, _DBType) ->
    ets:delete(rostergroups_tmp),
    ok.

row_length() ->
    case ejabberd_sql:use_new_schema() of
        true -> 10;
        false -> 9
    end.

import(LServer, {sql, _}, _DBType, <<"rostergroups">>, [LUser, SJID, Group]) ->
    LJID = jid:tolower(jid:decode(SJID)),
    ets:insert(rostergroups_tmp, {{LUser, LServer, LJID}, Group}),
    ok;
import(LServer, {sql, _}, DBType, <<"rosterusers">>, Row) ->
    I = mod_roster_sql:raw_to_record(LServer, lists:sublist(Row, row_length())),
    Groups = [G || {_, G} <- ets:lookup(rostergroups_tmp, I#roster.usj)],
    RosterItem = I#roster{groups = Groups},
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, <<"rosterusers">>, RosterItem);
import(LServer, {sql, _}, DBType, <<"roster_version">>, [LUser, Ver]) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, <<"roster_version">>, [LUser, Ver]).

mod_opt_type(access) ->
    fun acl:access_rules_validator/1;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(store_current_id) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(versioning) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(O) when O == cache_life_time; O == cache_size ->
    fun (I) when is_integer(I), I > 0 -> I;
        (infinity) -> infinity
    end;
mod_opt_type(O) when O == use_cache; O == cache_missed ->
    fun (B) when is_boolean(B) -> B end.

mod_options(Host) ->
    [{access, all},
     {store_current_id, false},
     {versioning, false},
     {db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {use_cache, ejabberd_config:use_cache(Host)},
     {cache_size, ejabberd_config:cache_size(Host)},
     {cache_missed, ejabberd_config:cache_missed(Host)},
     {cache_life_time, ejabberd_config:cache_life_time(Host)}].
