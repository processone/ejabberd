%%%----------------------------------------------------------------------
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-module(mod_privacy).

-author('alexey@process-one.net').

-protocol({xep, 16, '1.6'}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_iq/1, export/1,
	 c2s_copy_session/2, push_list_update/2, disco_features/5,
	 check_packet/4, remove_user/2, encode_list_item/1,
         get_user_lists/2, get_user_list/3,
	 set_list/1, set_list/4, set_default_list/3,
	 user_send_packet/1, mod_doc/0,
	 import_start/2, import_stop/2, import/5, import_info/0,
	 mod_opt_type/1, mod_options/1, depends/2]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("mod_privacy.hrl").
-include("translate.hrl").

-define(PRIVACY_CACHE, privacy_cache).
-define(PRIVACY_LIST_CACHE, privacy_list_cache).

-type c2s_state() :: ejabberd_c2s:state().
-callback init(binary(), gen_mod:opts()) -> any().
-callback import(#privacy{}) -> ok.
-callback set_default(binary(), binary(), binary()) ->
          ok | {error, notfound | any()}.
-callback unset_default(binary(), binary()) -> ok | {error, any()}.
-callback remove_list(binary(), binary(), binary()) ->
          ok | {error, notfound | conflict | any()}.
-callback remove_lists(binary(), binary()) -> ok | {error, any()}.
-callback set_lists(#privacy{}) -> ok | {error, any()}.
-callback set_list(binary(), binary(), binary(), [listitem()]) ->
          ok | {error, any()}.
-callback get_list(binary(), binary(), binary() | default) ->
          {ok, {binary(), [listitem()]}} | error | {error, any()}.
-callback get_lists(binary(), binary()) ->
          {ok, #privacy{}} | error | {error, any()}.
-callback use_cache(binary()) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-optional_callbacks([use_cache/1, cache_nodes/1]).

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    Mod:init(Host, Opts),
    init_cache(Mod, Host, Opts),
    {ok, [{hook, disco_local_features, disco_features, 50},
          {hook, c2s_copy_session, c2s_copy_session, 50},
          {hook, user_send_packet, user_send_packet, 50},
          {hook, privacy_check_packet, check_packet, 50},
          {hook, remove_user, remove_user, 50},
          {iq_handler, ejabberd_sm, ?NS_PRIVACY, process_iq}]}.

stop(_Host) ->
    ok.

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    init_cache(NewMod, Host, NewOpts).

-spec disco_features({error, stanza_error()} | {result, [binary()]} | empty,
		     jid(), jid(), binary(), binary()) ->
			    {error, stanza_error()} | {result, [binary()]}.
disco_features({error, Err}, _From, _To, _Node, _Lang) ->
    {error, Err};
disco_features(empty, _From, _To, <<"">>, _Lang) ->
    {result, [?NS_PRIVACY]};
disco_features({result, Feats}, _From, _To, <<"">>, _Lang) ->
    {result, [?NS_PRIVACY|Feats]};
disco_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec process_iq(iq()) -> iq().
process_iq(#iq{type = Type,
	       from = #jid{luser = U, lserver = S},
	       to = #jid{luser = U, lserver = S}} = IQ) ->
    case Type of
	get -> process_iq_get(IQ);
	set -> process_iq_set(IQ)
    end;
process_iq(#iq{lang = Lang} = IQ) ->
    Txt = ?T("Query to another users is forbidden"),
    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang)).

-spec process_iq_get(iq()) -> iq().
process_iq_get(#iq{lang = Lang,
		      sub_els = [#privacy_query{default = Default,
					     active = Active}]} = IQ)
  when Default /= undefined; Active /= undefined ->
    Txt = ?T("Only <list/> element is allowed in this query"),
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
process_iq_get(#iq{lang = Lang,
		   sub_els = [#privacy_query{lists = Lists}]} = IQ) ->
    case Lists of
	[] ->
	    process_lists_get(IQ);
	[#privacy_list{name = ListName}] ->
	    process_list_get(IQ, ListName);
	_ ->
	    Txt = ?T("Too many <list/> elements"),
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end;
process_iq_get(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_lists_get(iq()) -> iq().
process_lists_get(#iq{from = #jid{luser = LUser, lserver = LServer},
		      lang = Lang} = IQ) ->
    case get_user_lists(LUser, LServer) of
	{ok, #privacy{default = Default, lists = Lists}} ->
	    Active = xmpp:get_meta(IQ, privacy_active_list, none),
	    xmpp:make_iq_result(
	      IQ, #privacy_query{active = Active,
				 default = Default,
				 lists = [#privacy_list{name = Name}
					  || {Name, _} <- Lists]});
	error ->
	    xmpp:make_iq_result(
	      IQ, #privacy_query{active = none, default = none});
	{error, _} ->
	    Txt = ?T("Database failure"),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end.

-spec process_list_get(iq(), binary()) -> iq().
process_list_get(#iq{from = #jid{luser = LUser, lserver = LServer},
		     lang = Lang} = IQ, Name) ->
    case get_user_list(LUser, LServer, Name) of
	{ok, {_, List}} ->
	    Items = lists:map(fun encode_list_item/1, List),
	    xmpp:make_iq_result(
	      IQ,
	      #privacy_query{
		 lists = [#privacy_list{name = Name, items = Items}]});
	error ->
	    Txt = ?T("No privacy list with this name found"),
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
	{error, _} ->
	    Txt = ?T("Database failure"),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end.

-spec encode_list_item(listitem()) -> privacy_item().
encode_list_item(#listitem{action = Action,
			   order = Order,
			   type = Type,
			   match_all = MatchAll,
			   match_iq = MatchIQ,
			   match_message = MatchMessage,
			   match_presence_in = MatchPresenceIn,
			   match_presence_out = MatchPresenceOut,
			   value = Value}) ->
    Item = #privacy_item{action = Action,
			 order = Order,
			 type = case Type of
				    none -> undefined;
				    Type -> Type
				end,
			 value = encode_value(Type, Value)},
    case MatchAll of
	true ->
	    Item;
	false ->
	    Item#privacy_item{message = MatchMessage,
			      iq = MatchIQ,
			      presence_in = MatchPresenceIn,
			      presence_out = MatchPresenceOut}
    end.

-spec encode_value(listitem_type(), listitem_value()) -> binary().
encode_value(Type, Val) ->
    case Type of
	jid -> jid:encode(Val);
	group -> Val;
	subscription ->
	    case Val of
		both -> <<"both">>;
		to -> <<"to">>;
		from -> <<"from">>;
		none -> <<"none">>
	    end;
	none -> <<"">>
    end.

-spec decode_value(jid | subscription | group | undefined, binary()) ->
			  listitem_value().
decode_value(Type, Value) ->
    case Type of
	jid -> jid:tolower(jid:decode(Value));
	subscription ->
	    case Value of
		<<"from">> -> from;
		<<"to">> -> to;
		<<"both">> -> both;
		<<"none">> -> none
	    end;
	group when Value /= <<"">> -> Value;
	undefined -> none
    end.

-spec process_iq_set(iq()) -> iq().
process_iq_set(#iq{lang = Lang,
		      sub_els = [#privacy_query{default = Default,
						active = Active,
					     lists = Lists}]} = IQ) ->
    case Lists of
	[#privacy_list{items = Items, name = ListName}]
	  when Default == undefined, Active == undefined ->
	    process_lists_set(IQ, ListName, Items);
	[] when Default == undefined, Active /= undefined ->
	    process_active_set(IQ, Active);
	[] when Active == undefined, Default /= undefined ->
	    process_default_set(IQ, Default);
	_ ->
	    Txt = ?T("The stanza MUST contain only one <active/> element, "
		     "one <default/> element, or one <list/> element"),
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end;
process_iq_set(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_default_set(iq(), none | binary()) -> iq().
process_default_set(#iq{from = #jid{luser = LUser, lserver = LServer},
			lang = Lang} = IQ, Value) ->
    case set_default_list(LUser, LServer, Value) of
	ok ->
	    xmpp:make_iq_result(IQ);
	{error, notfound} ->
	    Txt = ?T("No privacy list with this name found"),
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
	{error, _} ->
	    Txt = ?T("Database failure"),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end.

-spec process_active_set(IQ, none | binary()) -> IQ.
process_active_set(IQ, none) ->
    xmpp:make_iq_result(xmpp:put_meta(IQ, privacy_active_list, none));
process_active_set(#iq{from = #jid{luser = LUser, lserver = LServer},
		       lang = Lang} = IQ, Name) ->
    case get_user_list(LUser, LServer, Name) of
	{ok, _} ->
	    xmpp:make_iq_result(xmpp:put_meta(IQ, privacy_active_list, Name));
	error ->
	    Txt = ?T("No privacy list with this name found"),
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
	{error, _} ->
	    Txt = ?T("Database failure"),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end.

-spec set_list(privacy()) -> ok | {error, any()}.
set_list(#privacy{us = {LUser, LServer}, lists = Lists} = Privacy) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:set_lists(Privacy) of
	ok ->
	    Names = [Name || {Name, _} <- Lists],
	    delete_cache(Mod, LUser, LServer, Names);
	{error, _} = Err ->
	    Err
    end.

-spec process_lists_set(iq(), binary(), [privacy_item()]) -> iq().
process_lists_set(#iq{from = #jid{luser = LUser, lserver = LServer},
		      lang = Lang} = IQ, Name, []) ->
    case xmpp:get_meta(IQ, privacy_active_list, none) of
	Name ->
	    Txt = ?T("Cannot remove active list"),
	    xmpp:make_error(IQ, xmpp:err_conflict(Txt, Lang));
	_ ->
	    case remove_list(LUser, LServer, Name) of
		ok ->
		    xmpp:make_iq_result(IQ);
		{error, conflict} ->
		    Txt = ?T("Cannot remove default list"),
		    xmpp:make_error(IQ, xmpp:err_conflict(Txt, Lang));
		{error, notfound} ->
		    Txt = ?T("No privacy list with this name found"),
		    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
		{error, _} ->
		    Txt = ?T("Database failure"),
		    Err = xmpp:err_internal_server_error(Txt, Lang),
		    xmpp:make_error(IQ, Err)
	    end
    end;
process_lists_set(#iq{from = #jid{luser = LUser, lserver = LServer} = From,
		      lang = Lang} = IQ, Name, Items) ->
    case catch lists:map(fun decode_item/1, Items) of
	{error, Why} ->
	    Txt = xmpp:io_format_error(Why),
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	List ->
	    case set_list(LUser, LServer, Name, List) of
		ok ->
		    push_list_update(From, Name),
		    xmpp:make_iq_result(IQ);
		{error, _} ->
		    Txt = ?T("Database failure"),
		    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
	    end
    end.

-spec push_list_update(jid(), binary()) -> ok.
push_list_update(From, Name) ->
    BareFrom = jid:remove_resource(From),
    lists:foreach(
      fun(R) ->
	      To = jid:replace_resource(From, R),
	      IQ = #iq{type = set, from = BareFrom, to = To,
		       id = <<"push", (p1_rand:get_string())/binary>>,
		       sub_els = [#privacy_query{
				     lists = [#privacy_list{name = Name}]}]},
	      ejabberd_router:route(IQ)
      end, ejabberd_sm:get_user_resources(From#jid.luser, From#jid.lserver)).

-spec decode_item(privacy_item()) -> listitem().
decode_item(#privacy_item{order = Order,
			  action = Action,
			  type = T,
			  value = V,
			  message = MatchMessage,
			  iq = MatchIQ,
			  presence_in = MatchPresenceIn,
			  presence_out = MatchPresenceOut}) ->
    Value = try decode_value(T, V)
	    catch _:_ ->
		    throw({error, {bad_attr_value, <<"value">>,
				   <<"item">>, ?NS_PRIVACY}})
	    end,
    Type = case T of
	       undefined -> none;
	       _ -> T
	   end,
    ListItem = #listitem{order = Order,
			 action = Action,
			 type = Type,
			 value = Value},
    if not (MatchMessage or MatchIQ or MatchPresenceIn or MatchPresenceOut) ->
	    ListItem#listitem{match_all = true};
       true ->
	    ListItem#listitem{match_iq = MatchIQ,
			      match_message = MatchMessage,
			      match_presence_in = MatchPresenceIn,
			      match_presence_out = MatchPresenceOut}
    end.

-spec c2s_copy_session(c2s_state(), c2s_state()) -> c2s_state().
c2s_copy_session(State, #{privacy_active_list := List}) ->
    State#{privacy_active_list => List};
c2s_copy_session(State, _) ->
    State.

%% Adjust the client's state, so next packets (which can be already queued)
%% will take the active list into account.
-spec update_c2s_state_with_privacy_list(stanza(), c2s_state()) -> c2s_state().
update_c2s_state_with_privacy_list(#iq{type = set,
				       to = #jid{luser = U, lserver = S,
						 lresource = <<"">>} = To} = IQ,
				   State) ->
    %% Match a IQ set containing a new active privacy list
    case xmpp:get_subtag(IQ, #privacy_query{}) of
	#privacy_query{default = undefined, active = Active} ->
	    case Active of
		none ->
		    ?DEBUG("Removing active privacy list for user: ~ts",
			   [jid:encode(To)]),
		    State#{privacy_active_list => none};
		undefined ->
		    State;
		_ ->
		    case get_user_list(U, S, Active) of
			{ok, _} ->
			    ?DEBUG("Setting active privacy list '~ts' for user: ~ts",
				   [Active, jid:encode(To)]),
			    State#{privacy_active_list => Active};
			_ ->
			    %% unknown privacy list name
			    State
		    end
	    end;
	_ ->
	    State
    end;
update_c2s_state_with_privacy_list(_Packet, State) ->
    State.

%% Add the active privacy list to packet metadata
-spec user_send_packet({stanza(), c2s_state()}) -> {stanza(), c2s_state()}.
user_send_packet({#iq{type = Type,
		      to = #jid{luser = U, lserver = S, lresource = <<"">>},
		      from = #jid{luser = U, lserver = S},
		      sub_els = [_]} = IQ,
		  #{privacy_active_list := Name} = State})
  when Type == get; Type == set ->
    NewIQ = case xmpp:has_subtag(IQ, #privacy_query{}) of
		true -> xmpp:put_meta(IQ, privacy_active_list, Name);
		false -> IQ
	    end,
    {NewIQ, update_c2s_state_with_privacy_list(IQ, State)};
%% For client with no active privacy list, see if there is
%% one about to be activated in this packet and update client state
user_send_packet({Packet, State}) ->
    {Packet, update_c2s_state_with_privacy_list(Packet, State)}.

-spec set_list(binary(), binary(), binary(), [listitem()]) -> ok | {error, any()}.
set_list(LUser, LServer, Name, List) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:set_list(LUser, LServer, Name, List) of
	ok ->
	    delete_cache(Mod, LUser, LServer, [Name]);
	{error, _} = Err ->
	    Err
    end.

-spec remove_list(binary(), binary(), binary()) ->
      ok | {error, conflict | notfound | any()}.
remove_list(LUser, LServer, Name) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:remove_list(LUser, LServer, Name) of
	ok ->
	    delete_cache(Mod, LUser, LServer, [Name]);
	Err ->
	    Err
    end.

-spec get_user_lists(binary(), binary()) -> {ok, privacy()} | error | {error, any()}.
get_user_lists(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:lookup(
	      ?PRIVACY_CACHE, {LUser, LServer},
	      fun() -> Mod:get_lists(LUser, LServer) end);
	false ->
	    Mod:get_lists(LUser, LServer)
    end.

-spec get_user_list(binary(), binary(), binary() | default) ->
      {ok, {binary(), [listitem()]}} | error | {error, any()}.
get_user_list(LUser, LServer, Name) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:lookup(
	      ?PRIVACY_LIST_CACHE, {LUser, LServer, Name},
	      fun() ->
		      case ets_cache:lookup(
			     ?PRIVACY_CACHE, {LUser, LServer}) of
			  {ok, Privacy} ->
			      get_list_by_name(Privacy, Name);
			  error ->
			      Mod:get_list(LUser, LServer, Name)
		      end
	      end);
	false ->
	    Mod:get_list(LUser, LServer, Name)
    end.

-spec get_list_by_name(#privacy{}, binary() | default) ->
      {ok, {binary(), [listitem()]}} | error.
get_list_by_name(#privacy{default = Default} = Privacy, default) ->
    get_list_by_name(Privacy, Default);
get_list_by_name(#privacy{lists = Lists}, Name) ->
    case lists:keyfind(Name, 1, Lists) of
	{_, List} -> {ok, {Name, List}};
	false -> error
    end.

-spec set_default_list(binary(), binary(), binary() | none) ->
      ok | {error, notfound | any()}.
set_default_list(LUser, LServer, Name) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Res = case Name of
	      none -> Mod:unset_default(LUser, LServer);
	      _ -> Mod:set_default(LUser, LServer, Name)
	  end,
    case Res of
	ok ->
	    delete_cache(Mod, LUser, LServer, []);
	Err ->
	    Err
    end.

-spec check_packet(allow | deny, c2s_state() | jid(), stanza(), in | out) -> allow | deny.
check_packet(Acc, #{jid := JID} = State, Packet, Dir) ->
    case maps:get(privacy_active_list, State, none) of
	none ->
	    check_packet(Acc, JID, Packet, Dir);
	ListName ->
	    #jid{luser = LUser, lserver = LServer} = JID,
	    case get_user_list(LUser, LServer, ListName) of
		{ok, {_, List}} ->
		    do_check_packet(JID, List, Packet, Dir);
		_ ->
		    ?DEBUG("Non-existing active list '~ts' is set "
			   "for user '~ts'", [ListName, jid:encode(JID)]),
		    check_packet(Acc, JID, Packet, Dir)
	    end
    end;
check_packet(_, JID, Packet, Dir) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    case get_user_list(LUser, LServer, default) of
	{ok, {_, List}} ->
	    do_check_packet(JID, List, Packet, Dir);
	_ ->
	    allow
    end.

%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
-spec do_check_packet(jid(), [listitem()], stanza(), in | out) -> allow | deny.
do_check_packet(_, [], _, _) ->
    allow;
do_check_packet(#jid{luser = LUser, lserver = LServer}, List, Packet, Dir) ->
    From = xmpp:get_from(Packet),
    To = xmpp:get_to(Packet),
    case {From, To} of
	{#jid{luser = <<"">>, lserver = LServer},
	 #jid{lserver = LServer}} when Dir == in ->
	    %% Allow any packets from local server
	    allow;
	{#jid{lserver = LServer},
	 #jid{luser = <<"">>, lserver = LServer}} when Dir == out ->
	    %% Allow any packets to local server
    allow;
	{#jid{luser = LUser, lserver = LServer, lresource = <<"">>},
	 #jid{luser = LUser, lserver = LServer}} when Dir == in ->
	    %% Allow incoming packets from user's bare jid to his full jid
    allow;
	{#jid{luser = LUser, lserver = LServer},
	 #jid{luser = LUser, lserver = LServer, lresource = <<"">>}} when Dir == out ->
	    %% Allow outgoing packets from user's full jid to his bare JID
	    allow;
      _ ->
	  PType = case Packet of
		    #message{} -> message;
		    #iq{} -> iq;
		    #presence{type = available} -> presence;
		    #presence{type = unavailable} -> presence;
		    _ -> other
		  end,
	  PType2 = case {PType, Dir} of
		     {message, in} -> message;
		     {iq, in} -> iq;
		     {presence, in} -> presence_in;
		     {presence, out} -> presence_out;
		     {_, _} -> other
		   end,
	  LJID = case Dir of
		   in -> jid:tolower(From);
		   out -> jid:tolower(To)
		 end,
	  check_packet_aux(List, PType2, LJID, [LUser, LServer])
    end.

-spec check_packet_aux([listitem()],
		       message | iq | presence_in | presence_out | other,
		       ljid(), [binary()] | {none | both | from | to, [binary()]}) ->
			      allow | deny.
%% Ptype = message | iq | presence_in | presence_out | other
check_packet_aux([], _PType, _JID, _RosterInfo) ->
    allow;
check_packet_aux([Item | List], PType, JID, RosterInfo) ->
    #listitem{type = Type, value = Value, action = Action} =
	Item,
    case is_ptype_match(Item, PType) of
      true ->
	    case is_type_match(Type, Value, JID, RosterInfo) of
		{true, _} -> Action;
		{false, RI} ->
		    check_packet_aux(List, PType, JID, RI)
	    end;
      false ->
	  check_packet_aux(List, PType, JID, RosterInfo)
    end.

-spec is_ptype_match(listitem(),
		     message | iq | presence_in | presence_out | other) ->
			    boolean().
is_ptype_match(Item, PType) ->
    case Item#listitem.match_all of
      true -> true;
      false ->
	  case PType of
	    message -> Item#listitem.match_message;
	    iq -> Item#listitem.match_iq;
	    presence_in -> Item#listitem.match_presence_in;
	    presence_out -> Item#listitem.match_presence_out;
	    other -> false
	  end
    end.

-spec is_type_match(none | jid | subscription | group, listitem_value(),
		    ljid(), [binary()] | {none | both | from | to, [binary()]}) ->
    {boolean(), [binary()] | {none | both | from | to, [binary()]}}.
is_type_match(none, _Value, _JID, RosterInfo) ->
    {true, RosterInfo};
is_type_match(jid, Value, JID, RosterInfo) ->
    case Value of
	{<<"">>, Server, <<"">>} ->
	    case JID of
		{_, Server, _} -> {true, RosterInfo};
		_ -> {false, RosterInfo}
	    end;
	{User, Server, <<"">>} ->
	    case JID of
		{User, Server, _} -> {true, RosterInfo};
		_ -> {false, RosterInfo}
	    end;
	{<<"">>, Server, Resource} ->
	    case JID of
		{_, Server, Resource} -> {true, RosterInfo};
		_ -> {false, RosterInfo}
	    end;
	_ -> {Value == JID, RosterInfo}
    end;
is_type_match(subscription, Value, JID, RosterInfo) ->
    {Subscription, _} = RI = resolve_roster_info(JID, RosterInfo),
    {Value == Subscription, RI};
is_type_match(group, Group, JID, RosterInfo) ->
    {_, Groups} = RI = resolve_roster_info(JID, RosterInfo),
    {lists:member(Group, Groups), RI}.

-spec resolve_roster_info(ljid(), [binary()] | {none | both | from | to, [binary()]}) ->
    {none | both | from | to, [binary()]}.
resolve_roster_info(JID, [LUser, LServer]) ->
    {Subscription, _Ask, Groups} =
    ejabberd_hooks:run_fold(
	roster_get_jid_info, LServer,
	{none, none, []},
	[LUser, LServer, JID]),
    {Subscription, Groups};
resolve_roster_info(_, RosterInfo) ->
    RosterInfo.

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Privacy = get_user_lists(LUser, LServer),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_lists(LUser, LServer),
    case Privacy of
	{ok, #privacy{lists = Lists}} ->
	    Names = [Name || {Name, _} <- Lists],
	    delete_cache(Mod, LUser, LServer, Names);
	_ ->
	    ok
    end.

-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
    case use_cache(Mod, Host) of
	true ->
	    CacheOpts = cache_opts(Opts),
	    ets_cache:new(?PRIVACY_CACHE, CacheOpts),
	    ets_cache:new(?PRIVACY_LIST_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?PRIVACY_CACHE),
	    ets_cache:delete(?PRIVACY_LIST_CACHE)
    end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = mod_privacy_opt:cache_size(Opts),
    CacheMissed = mod_privacy_opt:cache_missed(Opts),
    LifeTime = mod_privacy_opt:cache_life_time(Opts),
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 1) of
	true -> Mod:use_cache(Host);
	false -> mod_privacy_opt:use_cache(Host)
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
	true -> Mod:cache_nodes(Host);
	false -> ejabberd_cluster:get_nodes()
    end.

-spec delete_cache(module(), binary(), binary(), [binary()]) -> ok.
delete_cache(Mod, LUser, LServer, Names) ->
    case use_cache(Mod, LServer) of
	true ->
	    Nodes = cache_nodes(Mod, LServer),
	    ets_cache:delete(?PRIVACY_CACHE, {LUser, LServer}, Nodes),
	    lists:foreach(
	      fun(Name) ->
		      ets_cache:delete(
			?PRIVACY_LIST_CACHE,
			{LUser, LServer, Name},
			Nodes)
	      end, [default|Names]);
	false ->
	    ok
    end.

numeric_to_binary(<<0, 0, _/binary>>) ->
    <<"0">>;
numeric_to_binary(<<0, _, _:6/binary, T/binary>>) ->
    Res = lists:foldl(
            fun(X, Sum) ->
                    Sum*10000 + X
            end, 0, [X || <<X:16>> <= T]),
    integer_to_binary(Res).

bool_to_binary(<<0>>) -> <<"0">>;
bool_to_binary(<<1>>) -> <<"1">>.

prepare_list_data(mysql, [ID|Row]) ->
    [binary_to_integer(ID)|Row];
prepare_list_data(pgsql, [<<ID:64>>,
                          SType, SValue, SAction, SOrder, SMatchAll,
                          SMatchIQ, SMatchMessage, SMatchPresenceIn,
                          SMatchPresenceOut]) ->
    [ID, SType, SValue, SAction,
     numeric_to_binary(SOrder),
     bool_to_binary(SMatchAll),
     bool_to_binary(SMatchIQ),
     bool_to_binary(SMatchMessage),
     bool_to_binary(SMatchPresenceIn),
     bool_to_binary(SMatchPresenceOut)].

prepare_id(mysql, ID) ->
    binary_to_integer(ID);
prepare_id(pgsql, <<ID:32>>) ->
    ID.

import_info() ->
    [{<<"privacy_default_list">>, 2},
     {<<"privacy_list_data">>, 10},
     {<<"privacy_list">>, 4}].

import_start(LServer, DBType) ->
    ets:new(privacy_default_list_tmp, [private, named_table]),
    ets:new(privacy_list_data_tmp, [private, named_table, bag]),
    ets:new(privacy_list_tmp, [private, named_table, bag,
                               {keypos, #privacy.us}]),
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:init(LServer, []).

import(LServer, {sql, _}, _DBType, <<"privacy_default_list">>, [LUser, Name]) ->
    US = {LUser, LServer},
    ets:insert(privacy_default_list_tmp, {US, Name}),
    ok;
import(LServer, {sql, SQLType}, _DBType, <<"privacy_list_data">>, Row1) ->
    [ID|Row] = prepare_list_data(SQLType, Row1),
    case mod_privacy_sql:raw_to_item(Row) of
        [Item] ->
            IS = {ID, LServer},
            ets:insert(privacy_list_data_tmp, {IS, Item}),
            ok;
        [] ->
            ok
    end;
import(LServer, {sql, SQLType}, _DBType, <<"privacy_list">>,
       [LUser, Name, ID, _TimeStamp]) ->
    US = {LUser, LServer},
    IS = {prepare_id(SQLType, ID), LServer},
    Default = case ets:lookup(privacy_default_list_tmp, US) of
                  [{_, Name}] -> Name;
                  _ -> none
              end,
    case [Item || {_, Item} <- ets:lookup(privacy_list_data_tmp, IS)] of
        [_|_] = Items ->
            Privacy = #privacy{us = {LUser, LServer},
                               default = Default,
                               lists = [{Name, Items}]},
            ets:insert(privacy_list_tmp, Privacy),
            ets:delete(privacy_list_data_tmp, IS),
            ok;
        _ ->
            ok
    end.

import_stop(_LServer, DBType) ->
    import_next(DBType, ets:first(privacy_list_tmp)),
    ets:delete(privacy_default_list_tmp),
    ets:delete(privacy_list_data_tmp),
    ets:delete(privacy_list_tmp),
    ok.

import_next(_DBType, '$end_of_table') ->
    ok;
import_next(DBType, US) ->
    [P|_] = Ps = ets:lookup(privacy_list_tmp, US),
    Lists = lists:flatmap(
              fun(#privacy{lists = Lists}) ->
                      Lists
              end, Ps),
    Privacy = P#privacy{lists = Lists},
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(Privacy),
    import_next(DBType, ets:next(privacy_list_tmp, US)).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

depends(_Host, _Opts) ->
    [].

mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(use_cache) ->
    econf:bool();
mod_opt_type(cache_size) ->
    econf:pos_int(infinity);
mod_opt_type(cache_missed) ->
    econf:bool();
mod_opt_type(cache_life_time) ->
    econf:timeout(second, infinity).

mod_options(Host) ->
    [{db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {use_cache, ejabberd_option:use_cache(Host)},
     {cache_size, ejabberd_option:cache_size(Host)},
     {cache_missed, ejabberd_option:cache_missed(Host)},
     {cache_life_time, ejabberd_option:cache_life_time(Host)}].

mod_doc() ->
    #{desc =>
          [?T("This module implements "
              "https://xmpp.org/extensions/xep-0016.html"
              "[XEP-0016: Privacy Lists]."), "",
           ?T("NOTE: Nowadays modern XMPP clients rely on "
              "https://xmpp.org/extensions/xep-0191.html"
              "[XEP-0191: Blocking Command] which is implemented by "
              "'mod_blocking' module. However, you still need "
              "'mod_privacy' loaded in order for _`mod_blocking`_ to work.")],
      opts =>
          [{db_type,
            #{value => "mnesia | sql",
              desc =>
                  ?T("Same as top-level _`default_db`_ option, but applied to this module only.")}},
           {use_cache,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`use_cache`_ option, but applied to this module only.")}},
           {cache_size,
            #{value => "pos_integer() | infinity",
              desc =>
                  ?T("Same as top-level _`cache_size`_ option, but applied to this module only.")}},
           {cache_missed,
            #{value => "true | false",
              desc =>
                  ?T("Same as top-level _`cache_missed`_ option, but applied to this module only.")}},
           {cache_life_time,
            #{value => "timeout()",
              desc =>
                  ?T("Same as top-level _`cache_life_time`_ option, but applied to this module only.")}}]}.
