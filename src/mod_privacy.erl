%%%----------------------------------------------------------------------
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-export([start/2, stop/1, reload/3, process_iq/1, export/1, import_info/0,
	 c2s_session_opened/1, c2s_copy_session/2, push_list_update/3,
	 user_send_packet/1, user_receive_packet/1, disco_features/5,
	 check_packet/4, remove_user/2, encode_list_item/1,
	 is_list_needdb/1, import_start/2, import_stop/2,
         item_to_xml/1, get_user_lists/2, import/5,
	 set_privacy_list/1, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("mod_privacy.hrl").

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(#privacy{}) -> ok.
-callback process_lists_get(binary(), binary()) -> {none | binary(), [binary()]} | error.
-callback process_list_get(binary(), binary(), binary()) -> [listitem()] | error | not_found.
-callback process_default_set(binary(), binary(), binary() | none) -> {atomic, any()}.
-callback process_active_set(binary(), binary(), binary()) -> [listitem()] | error.
-callback remove_privacy_list(binary(), binary(), binary()) -> {atomic, any()}.
-callback set_privacy_list(#privacy{}) -> any().
-callback set_privacy_list(binary(), binary(), binary(), [listitem()]) -> {atomic, any()}.
-callback get_user_list(binary(), binary()) -> {none | binary(), [listitem()]}.
-callback get_user_lists(binary(), binary()) -> {ok, #privacy{}} | error.
-callback remove_user(binary(), binary()) -> any().

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
		       disco_features, 50),
    ejabberd_hooks:add(c2s_session_opened, Host, ?MODULE,
		       c2s_session_opened, 50),
    ejabberd_hooks:add(c2s_copy_session, Host, ?MODULE,
		       c2s_copy_session, 50),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 50),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       user_receive_packet, 50),
    ejabberd_hooks:add(privacy_check_packet, Host, ?MODULE,
		       check_packet, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_PRIVACY, ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE,
			  disco_features, 50),
    ejabberd_hooks:delete(c2s_session_opened, Host, ?MODULE,
			  c2s_session_opened, 50),
    ejabberd_hooks:delete(c2s_copy_session, Host, ?MODULE,
			  c2s_copy_session, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet, 50),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  user_receive_packet, 50),
    ejabberd_hooks:delete(privacy_check_packet, Host,
			  ?MODULE, check_packet, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_PRIVACY).

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
	    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVACY,
					  ?MODULE, process_iq, IQDisc);
	true ->
	    ok
    end.

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
    Txt = <<"Query to another users is forbidden">>,
    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang)).

-spec process_iq_get(iq()) -> iq().
process_iq_get(#iq{lang = Lang,
		      sub_els = [#privacy_query{default = Default,
					     active = Active}]} = IQ)
  when Default /= undefined; Active /= undefined ->
    Txt = <<"Only <list/> element is allowed in this query">>,
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
process_iq_get(#iq{lang = Lang,
		   sub_els = [#privacy_query{lists = Lists}]} = IQ) ->
    case Lists of
	[] ->
	    process_lists_get(IQ);
	[#privacy_list{name = ListName}] ->
	    process_list_get(IQ, ListName);
	_ ->
	    Txt = <<"Too many <list/> elements">>,
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end;
process_iq_get(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_lists_get(iq()) -> iq().
process_lists_get(#iq{from = #jid{luser = LUser, lserver = LServer},
		      lang = Lang,
		      meta = #{privacy_active_list := Active}} = IQ) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_lists_get(LUser, LServer) of
	error ->
	    Txt = <<"Database failure">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang));
	{_Default, []} ->
	    xmpp:make_iq_result(IQ, #privacy_query{});
	{Default, ListNames} ->
	    xmpp:make_iq_result(
	      IQ,
	     #privacy_query{active = Active,
			    default = Default,
			    lists = [#privacy_list{name = ListName}
				      || ListName <- ListNames]})
    end.

-spec process_list_get(iq(), binary()) -> iq().
process_list_get(#iq{from = #jid{luser = LUser, lserver = LServer},
		     lang = Lang} = IQ, Name) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_list_get(LUser, LServer, Name) of
	error ->
	    Txt = <<"Database failure">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang));
	not_found ->
	    Txt = <<"No privacy list with this name found">>,
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
	Items ->
	    LItems = lists:map(fun encode_list_item/1, Items),
	    xmpp:make_iq_result(
	      IQ,
	     #privacy_query{
		 lists = [#privacy_list{name = Name, items = LItems}]})
    end.

-spec item_to_xml(listitem()) -> xmlel().
item_to_xml(ListItem) ->
    xmpp:encode(encode_list_item(ListItem)).

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
	    Txt = <<"The stanza MUST contain only one <active/> element, "
		    "one <default/> element, or one <list/> element">>,
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end;
process_iq_set(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_default_set(iq(), binary()) -> iq().
process_default_set(#iq{from = #jid{luser = LUser, lserver = LServer},
			lang = Lang} = IQ, Value) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_default_set(LUser, LServer, Value) of
	{atomic, error} ->
	    Txt = <<"Database failure">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang));
	{atomic, not_found} ->
	    Txt = <<"No privacy list with this name found">>,
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
	{atomic, ok} ->
	    xmpp:make_iq_result(IQ);
	Err ->
	    ?ERROR_MSG("failed to set default list '~s' for user ~s@~s: ~p",
		       [Value, LUser, LServer, Err]),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error())
    end.

-spec process_active_set(IQ, none | binary()) -> IQ.
process_active_set(IQ, none) ->
    xmpp:make_iq_result(xmpp:put_meta(IQ, privacy_list, #userlist{}));
process_active_set(#iq{from = #jid{luser = LUser, lserver = LServer},
		       lang = Lang} = IQ, Name) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_active_set(LUser, LServer, Name) of
	error ->
	    Txt = <<"No privacy list with this name found">>,
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
	Items ->
	    NeedDb = is_list_needdb(Items),
	    List = #userlist{name = Name, list = Items, needdb = NeedDb},
	    xmpp:make_iq_result(xmpp:put_meta(IQ, privacy_list, List))
    end.

-spec set_privacy_list(privacy()) -> any().
set_privacy_list(#privacy{us = {_, LServer}} = Privacy) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_privacy_list(Privacy).

-spec process_lists_set(iq(), binary(), [privacy_item()]) -> iq().
process_lists_set(#iq{meta = #{privacy_active_list := Name},
		      lang = Lang} = IQ, Name, []) ->
    Txt = <<"Cannot remove active list">>,
    xmpp:make_error(IQ, xmpp:err_conflict(Txt, Lang));
process_lists_set(#iq{from = #jid{luser = LUser, lserver = LServer} = From,
		      lang = Lang} = IQ, Name, []) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:remove_privacy_list(LUser, LServer, Name) of
	{atomic, conflict} ->
	    Txt = <<"Cannot remove default list">>,
	    xmpp:make_error(IQ, xmpp:err_conflict(Txt, Lang));
	{atomic, not_found} ->
	    Txt = <<"No privacy list with this name found">>,
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
	{atomic, ok} ->
	    push_list_update(From, #userlist{name = Name}, Name),
	    xmpp:make_iq_result(IQ);
	Err ->
	    ?ERROR_MSG("failed to remove privacy list '~s' for user ~s@~s: ~p",
		       [Name, LUser, LServer, Err]),
	    Txt = <<"Database failure">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end;
process_lists_set(#iq{from = #jid{luser = LUser, lserver = LServer} = From,
		      lang = Lang} = IQ, Name, Items) ->
    case catch lists:map(fun decode_item/1, Items) of
	{error, Why} ->
	    Txt = xmpp:format_error(Why),
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	List ->
	    Mod = gen_mod:db_mod(LServer, ?MODULE),
	    case Mod:set_privacy_list(LUser, LServer, Name, List) of
		{atomic, ok} ->
		    UserList = #userlist{name = Name, list = List,
					 needdb = is_list_needdb(List)},
		    push_list_update(From, UserList, Name),
		    xmpp:make_iq_result(IQ);
		Err ->
		    ?ERROR_MSG("failed to set privacy list '~s' "
			       "for user ~s@~s: ~p",
			       [Name, LUser, LServer, Err]),
		    Txt = <<"Database failure">>,
		    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
	    end
    end.

-spec push_list_update(jid(), #userlist{}, binary() | none) -> ok.
push_list_update(From, List, Name) ->
    BareFrom = jid:remove_resource(From),
    lists:foreach(
      fun(R) ->
	      To = jid:replace_resource(From, R),
	      IQ = #iq{type = set, from = BareFrom, to = To,
		       id = <<"push", (randoms:get_string())/binary>>,
		       sub_els = [#privacy_query{
				     lists = [#privacy_list{name = Name}]}],
		       meta = #{privacy_updated_list => List}},
	      ejabberd_router:route(IQ)
      end, ejabberd_sm:get_user_resources(From#jid.luser, From#jid.lserver)).

-spec user_send_packet({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_send_packet({#iq{type = Type,
		      to = #jid{luser = U, lserver = S, lresource = <<"">>},
		      from = #jid{luser = U, lserver = S},
		      sub_els = [_]} = IQ,
		  #{privacy_list := #userlist{name = Name}} = State})
  when Type == get; Type == set ->
    NewIQ = case xmpp:has_subtag(IQ, #privacy_query{}) of
		true -> xmpp:put_meta(IQ, privacy_active_list, Name);
		false -> IQ
	    end,
    {NewIQ, State};
user_send_packet(Acc) ->
    Acc.

-spec user_receive_packet({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_receive_packet({#iq{type = result, meta = #{privacy_list := List}} = IQ,
		     State}) ->
    {IQ, State#{privacy_list => List}};
user_receive_packet({#iq{type = set, meta = #{privacy_updated_list := New}} = IQ,
		     #{user := U, server := S, resource := R,
		       privacy_list := Old} = State}) ->
    State1 = if Old#userlist.name == New#userlist.name ->
		     State#{privacy_list => New};
		true ->
		     State
	     end,
    From = jid:make(U, S),
    To = jid:make(U, S, R),
    {xmpp:set_from_to(IQ, From, To), State1};
user_receive_packet(Acc) ->
    Acc.

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

-spec is_list_needdb([listitem()]) -> boolean().
is_list_needdb(Items) ->
    lists:any(fun (X) ->
		      case X#listitem.type of
			subscription -> true;
			group -> true;
			_ -> false
		      end
	      end,
	      Items).

-spec get_user_list(binary(), binary()) -> #userlist{}.
get_user_list(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    {Default, Items} = Mod:get_user_list(LUser, LServer),
    NeedDb = is_list_needdb(Items),
    #userlist{name = Default, list = Items, needdb = NeedDb}.

-spec c2s_session_opened(ejabberd_c2s:state()) -> ejabberd_c2s:state().
c2s_session_opened(#{jid := #jid{luser = LUser, lserver = LServer}} = State) ->
    State#{privacy_list => get_user_list(LUser, LServer)}.

-spec c2s_copy_session(ejabberd_c2s:state(), ejabberd_c2s:state()) -> ejabberd_c2s:state().
c2s_copy_session(State, #{privacy_list := List}) ->
    State#{privacy_list => List}.

-spec get_user_lists(binary(), binary()) -> {ok, privacy()} | error.
get_user_lists(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_user_lists(LUser, LServer).

%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
-spec check_packet(allow | deny, ejabberd_c2s:state() | jid(),
		   stanza(), in | out) -> allow | deny.
check_packet(_, #{jid := #jid{luser = LUser, lserver = LServer},
		  privacy_list := #userlist{list = List, needdb = NeedDb}},
	     Packet, Dir) ->
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
	_ when List == [] ->
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
	    {Subscription, Groups} =
		case NeedDb of
				     true ->
					 ejabberd_hooks:run_fold(roster_get_jid_info,
						LServer,
								 {none, []},
						[LUser, LServer, LJID]);
		    false ->
			{[], []}
				   end,
	    check_packet_aux(List, PType2, LJID, Subscription, Groups)
    end;
check_packet(Acc, #jid{luser = LUser, lserver = LServer} = JID, Packet, Dir) ->
    List = get_user_list(LUser, LServer),
    check_packet(Acc, #{jid => JID, privacy_list => List}, Packet, Dir).

-spec check_packet_aux([listitem()],
		       message | iq | presence_in | presence_out | other,
		       ljid(), none | both | from | to, [binary()]) ->
			      allow | deny.
%% Ptype = mesage | iq | presence_in | presence_out | other
check_packet_aux([], _PType, _JID, _Subscription,
		 _Groups) ->
    allow;
check_packet_aux([Item | List], PType, JID,
		 Subscription, Groups) ->
    #listitem{type = Type, value = Value, action = Action} =
	Item,
    case is_ptype_match(Item, PType) of
      true ->
	    case is_type_match(Type, Value, JID, Subscription, Groups) of
		true -> Action;
		false ->
		    check_packet_aux(List, PType, JID, Subscription, Groups)
	    end;
      false ->
	  check_packet_aux(List, PType, JID, Subscription, Groups)
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
		    ljid(), none | both | from | to, [binary()]) -> boolean().
is_type_match(none, _Value, _JID, _Subscription, _Groups) ->
    true;
is_type_match(Type, Value, JID, Subscription, Groups) ->
    case Type of
      jid ->
	  case Value of
	    {<<"">>, Server, <<"">>} ->
		case JID of
		  {_, Server, _} -> true;
		  _ -> false
		end;
	    {User, Server, <<"">>} ->
		case JID of
		  {User, Server, _} -> true;
		  _ -> false
		end;
	    _ -> Value == JID
	  end;
      subscription -> Value == Subscription;
      group -> lists:member(Value, Groups)
    end.

-spec remove_user(binary(), binary()) -> any().
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer).

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

mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [db_type, iqdisc].
