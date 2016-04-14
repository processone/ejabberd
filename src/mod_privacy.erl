%%%----------------------------------------------------------------------
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_privacy).

-author('alexey@process-one.net').

-protocol({xep, 16, '1.6'}).

-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/3, export/1, import/1,
	 process_iq_set/4, process_iq_get/5, get_user_list/3,
	 check_packet/6, remove_user/2,
	 is_list_needdb/1, updated_list/3,
         item_to_xml/1, get_user_lists/2, import/3,
	 set_privacy_list/1, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #privacy{}) -> ok | pass.
-callback process_lists_get(binary(), binary()) -> {none | binary(), [xmlel()]} | error.
-callback process_list_get(binary(), binary(), binary()) -> [listitem()] | error | not_found.
-callback process_default_set(binary(), binary(), {value, binary()} | false) -> {atomic, any()}.
-callback process_active_set(binary(), binary(), binary()) -> [listitem()] | error.
-callback remove_privacy_list(binary(), binary(), binary()) -> {atomic, any()}.
-callback set_privacy_list(#privacy{}) -> any().
-callback set_privacy_list(binary(), binary(), binary(), [listitem()]) -> {atomic, any()}.
-callback get_user_list(binary(), binary()) -> {none | binary(), [listitem()]}.
-callback get_user_lists(binary(), binary()) -> {ok, #privacy{}} | error.
-callback remove_user(binary(), binary()) -> {atomic, any()}.

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    mod_disco:register_feature(Host, ?NS_PRIVACY),
    ejabberd_hooks:add(privacy_iq_get, Host, ?MODULE,
		       process_iq_get, 50),
    ejabberd_hooks:add(privacy_iq_set, Host, ?MODULE,
		       process_iq_set, 50),
    ejabberd_hooks:add(privacy_get_user_list, Host, ?MODULE,
		       get_user_list, 50),
    ejabberd_hooks:add(privacy_check_packet, Host, ?MODULE,
		       check_packet, 50),
    ejabberd_hooks:add(privacy_updated_list, Host, ?MODULE,
		       updated_list, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_PRIVACY, ?MODULE, process_iq, IQDisc).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_PRIVACY),
    ejabberd_hooks:delete(privacy_iq_get, Host, ?MODULE,
			  process_iq_get, 50),
    ejabberd_hooks:delete(privacy_iq_set, Host, ?MODULE,
			  process_iq_set, 50),
    ejabberd_hooks:delete(privacy_get_user_list, Host,
			  ?MODULE, get_user_list, 50),
    ejabberd_hooks:delete(privacy_check_packet, Host,
			  ?MODULE, check_packet, 50),
    ejabberd_hooks:delete(privacy_updated_list, Host,
			  ?MODULE, updated_list, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_PRIVACY).

process_iq(_From, _To, IQ) ->
    SubEl = IQ#iq.sub_el,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

process_iq_get(_, From, _To, #iq{lang = Lang, sub_el = SubEl},
	       #userlist{name = Active}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    #xmlel{children = Els} = SubEl,
    case fxml:remove_cdata(Els) of
      [] -> process_lists_get(LUser, LServer, Active, Lang);
      [#xmlel{name = Name, attrs = Attrs}] ->
	  case Name of
	    <<"list">> ->
		ListName = fxml:get_attr(<<"name">>, Attrs),
		process_list_get(LUser, LServer, ListName, Lang);
	    _ ->
		  Txt = <<"Unsupported tag name">>,
		  {error, ?ERRT_BAD_REQUEST(Lang, Txt)}
	  end;
      _ -> {error, ?ERRT_BAD_REQUEST(Lang, <<"Too many elements">>)}
    end.

process_lists_get(LUser, LServer, Active, Lang) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_lists_get(LUser, LServer) of
      error -> {error, ?ERRT_INTERNAL_SERVER_ERROR(Lang, <<"Database failure">>)};
      {_Default, []} ->
	  {result,
	   [#xmlel{name = <<"query">>,
		   attrs = [{<<"xmlns">>, ?NS_PRIVACY}], children = []}]};
      {Default, LItems} ->
	  DItems = case Default of
		     none -> LItems;
		     _ ->
			 [#xmlel{name = <<"default">>,
				 attrs = [{<<"name">>, Default}], children = []}
			  | LItems]
		   end,
	  ADItems = case Active of
		      none -> DItems;
		      _ ->
			  [#xmlel{name = <<"active">>,
				  attrs = [{<<"name">>, Active}], children = []}
			   | DItems]
		    end,
	  {result,
	   [#xmlel{name = <<"query">>,
		   attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
		   children = ADItems}]}
    end.

process_list_get(LUser, LServer, {value, Name}, Lang) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_list_get(LUser, LServer, Name) of
      error -> {error, ?ERRT_INTERNAL_SERVER_ERROR(Lang, <<"Database failure">>)};
      not_found -> {error, ?ERR_ITEM_NOT_FOUND};
      Items ->
	  LItems = lists:map(fun item_to_xml/1, Items),
	  {result,
	   [#xmlel{name = <<"query">>,
		   attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
		   children =
		       [#xmlel{name = <<"list">>, attrs = [{<<"name">>, Name}],
			       children = LItems}]}]}
    end;
process_list_get(_LUser, _LServer, false, _Lang) ->
    {error, ?ERR_BAD_REQUEST}.

item_to_xml(Item) ->
    Attrs1 = [{<<"action">>,
	       action_to_list(Item#listitem.action)},
	      {<<"order">>, order_to_list(Item#listitem.order)}],
    Attrs2 = case Item#listitem.type of
	       none -> Attrs1;
	       Type ->
		   [{<<"type">>, type_to_list(Item#listitem.type)},
		    {<<"value">>, value_to_list(Type, Item#listitem.value)}
		    | Attrs1]
	     end,
    SubEls = case Item#listitem.match_all of
	       true -> [];
	       false ->
		   SE1 = case Item#listitem.match_iq of
			   true ->
			       [#xmlel{name = <<"iq">>, attrs = [],
				       children = []}];
			   false -> []
			 end,
		   SE2 = case Item#listitem.match_message of
			   true ->
			       [#xmlel{name = <<"message">>, attrs = [],
				       children = []}
				| SE1];
			   false -> SE1
			 end,
		   SE3 = case Item#listitem.match_presence_in of
			   true ->
			       [#xmlel{name = <<"presence-in">>, attrs = [],
				       children = []}
				| SE2];
			   false -> SE2
			 end,
		   SE4 = case Item#listitem.match_presence_out of
			   true ->
			       [#xmlel{name = <<"presence-out">>, attrs = [],
				       children = []}
				| SE3];
			   false -> SE3
			 end,
		   SE4
	     end,
    #xmlel{name = <<"item">>, attrs = Attrs2,
	   children = SubEls}.

action_to_list(Action) ->
    case Action of
      allow -> <<"allow">>;
      deny -> <<"deny">>
    end.

order_to_list(Order) ->
    iolist_to_binary(integer_to_list(Order)).

type_to_list(Type) ->
    case Type of
      jid -> <<"jid">>;
      group -> <<"group">>;
      subscription -> <<"subscription">>
    end.

value_to_list(Type, Val) ->
    case Type of
      jid -> jid:to_string(Val);
      group -> Val;
      subscription ->
	  case Val of
	    both -> <<"both">>;
	    to -> <<"to">>;
	    from -> <<"from">>;
	    none -> <<"none">>
	  end
    end.

list_to_action(S) ->
    case S of
      <<"allow">> -> allow;
      <<"deny">> -> deny
    end.

process_iq_set(_, From, _To, #iq{lang = Lang, sub_el = SubEl}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    #xmlel{children = Els} = SubEl,
    case fxml:remove_cdata(Els) of
      [#xmlel{name = Name, attrs = Attrs,
	      children = SubEls}] ->
	  ListName = fxml:get_attr(<<"name">>, Attrs),
	  case Name of
	    <<"list">> ->
		process_list_set(LUser, LServer, ListName,
				 fxml:remove_cdata(SubEls), Lang);
	    <<"active">> ->
		process_active_set(LUser, LServer, ListName);
	    <<"default">> ->
		process_default_set(LUser, LServer, ListName, Lang);
	    _ ->
		Txt = <<"Unsupported tag name">>,
		{error, ?ERRT_BAD_REQUEST(Lang, Txt)}
	  end;
      _ -> {error, ?ERR_BAD_REQUEST}
    end.

process_default_set(LUser, LServer, Value, Lang) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_default_set(LUser, LServer, Value) of
      {atomic, error} ->
	    {error, ?ERRT_INTERNAL_SERVER_ERROR(Lang, <<"Database failure">>)};
      {atomic, not_found} -> {error, ?ERR_ITEM_NOT_FOUND};
      {atomic, ok} -> {result, []};
      _ -> {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_active_set(LUser, LServer, {value, Name}) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_active_set(LUser, LServer, Name) of
      error -> {error, ?ERR_ITEM_NOT_FOUND};
      Items ->
	  NeedDb = is_list_needdb(Items),
	  {result, [],
	   #userlist{name = Name, list = Items, needdb = NeedDb}}
    end;
process_active_set(_LUser, _LServer, false) ->
    {result, [], #userlist{}}.

set_privacy_list(#privacy{us = {_, LServer}} = Privacy) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_privacy_list(Privacy).

process_list_set(LUser, LServer, {value, Name}, Els, Lang) ->
    case parse_items(Els) of
      false -> {error, ?ERR_BAD_REQUEST};
      remove ->
	  Mod = gen_mod:db_mod(LServer, ?MODULE),
	  case Mod:remove_privacy_list(LUser, LServer, Name) of
	    {atomic, conflict} ->
		  Txt = <<"Cannot remove default list">>,
		  {error, ?ERRT_CONFLICT(Lang, Txt)};
	    {atomic, ok} ->
		ejabberd_sm:route(jid:make(LUser, LServer,
                                                <<"">>),
                                  jid:make(LUser, LServer, <<"">>),
                                  {broadcast, {privacy_list,
                                               #userlist{name = Name,
                                                         list = []},
                                               Name}}),
		{result, []};
	    _ -> {error, ?ERRT_INTERNAL_SERVER_ERROR(Lang, <<"Database failure">>)}
	  end;
      List ->
	  Mod = gen_mod:db_mod(LServer, ?MODULE),
	  case Mod:set_privacy_list(LUser, LServer, Name, List) of
	    {atomic, ok} ->
		NeedDb = is_list_needdb(List),
		ejabberd_sm:route(jid:make(LUser, LServer,
                                                <<"">>),
                                  jid:make(LUser, LServer, <<"">>),
                                  {broadcast, {privacy_list,
                                               #userlist{name = Name,
                                                         list = List,
                                                         needdb = NeedDb},
                                               Name}}),
		{result, []};
	    _ -> {error, ?ERRT_INTERNAL_SERVER_ERROR(Lang, <<"Database failure">>)}
	  end
    end;
process_list_set(_LUser, _LServer, false, _Els, _Lang) ->
    {error, ?ERR_BAD_REQUEST}.

parse_items([]) -> remove;
parse_items(Els) -> parse_items(Els, []).

parse_items([], Res) ->
    lists:keysort(#listitem.order, Res);
parse_items([#xmlel{name = <<"item">>, attrs = Attrs,
		    children = SubEls}
	     | Els],
	    Res) ->
    Type = fxml:get_attr(<<"type">>, Attrs),
    Value = fxml:get_attr(<<"value">>, Attrs),
    SAction = fxml:get_attr(<<"action">>, Attrs),
    SOrder = fxml:get_attr(<<"order">>, Attrs),
    Action = case catch list_to_action(element(2, SAction))
		 of
	       {'EXIT', _} -> false;
	       Val -> Val
	     end,
    Order = case catch jlib:binary_to_integer(element(2,
							SOrder))
		of
	      {'EXIT', _} -> false;
	      IntVal ->
		  if IntVal >= 0 -> IntVal;
		     true -> false
		  end
	    end,
    if (Action /= false) and (Order /= false) ->
	   I1 = #listitem{action = Action, order = Order},
	   I2 = case {Type, Value} of
		  {{value, T}, {value, V}} ->
		      case T of
			<<"jid">> ->
			    case jid:from_string(V) of
			      error -> false;
			      JID ->
				  I1#listitem{type = jid,
					      value = jid:tolower(JID)}
			    end;
			<<"group">> -> I1#listitem{type = group, value = V};
			<<"subscription">> ->
			    case V of
			      <<"none">> ->
				  I1#listitem{type = subscription,
					      value = none};
			      <<"both">> ->
				  I1#listitem{type = subscription,
					      value = both};
			      <<"from">> ->
				  I1#listitem{type = subscription,
					      value = from};
			      <<"to">> ->
				  I1#listitem{type = subscription, value = to};
			      _ -> false
			    end
		      end;
		  {{value, _}, false} -> false;
		  _ -> I1
		end,
	   case I2 of
	     false -> false;
	     _ ->
		 case parse_matches(I2, fxml:remove_cdata(SubEls)) of
		   false -> false;
		   I3 -> parse_items(Els, [I3 | Res])
		 end
	   end;
       true -> false
    end;
parse_items(_, _Res) -> false.

parse_matches(Item, []) ->
    Item#listitem{match_all = true};
parse_matches(Item, Els) -> parse_matches1(Item, Els).

parse_matches1(Item, []) -> Item;
parse_matches1(Item,
	       [#xmlel{name = <<"message">>} | Els]) ->
    parse_matches1(Item#listitem{match_message = true},
		   Els);
parse_matches1(Item, [#xmlel{name = <<"iq">>} | Els]) ->
    parse_matches1(Item#listitem{match_iq = true}, Els);
parse_matches1(Item,
	       [#xmlel{name = <<"presence-in">>} | Els]) ->
    parse_matches1(Item#listitem{match_presence_in = true},
		   Els);
parse_matches1(Item,
	       [#xmlel{name = <<"presence-out">>} | Els]) ->
    parse_matches1(Item#listitem{match_presence_out = true},
		   Els);
parse_matches1(_Item, [#xmlel{} | _Els]) -> false.

is_list_needdb(Items) ->
    lists:any(fun (X) ->
		      case X#listitem.type of
			subscription -> true;
			group -> true;
			_ -> false
		      end
	      end,
	      Items).

get_user_list(_Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    {Default, Items} = Mod:get_user_list(LUser, LServer),
    NeedDb = is_list_needdb(Items),
    #userlist{name = Default, list = Items,
	      needdb = NeedDb}.

get_user_lists(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_user_lists(LUser, LServer).

%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
check_packet(_, _User, _Server, _UserList,
	     {#jid{luser = <<"">>, lserver = Server} = _From,
	      #jid{lserver = Server} = _To, _},
	     in) ->
    allow;
check_packet(_, _User, _Server, _UserList,
	     {#jid{lserver = Server} = _From,
	      #jid{luser = <<"">>, lserver = Server} = _To, _},
	     out) ->
    allow;
check_packet(_, _User, _Server, _UserList,
	     {#jid{luser = User, lserver = Server} = _From,
	      #jid{luser = User, lserver = Server} = _To, _},
	     _Dir) ->
    allow;
check_packet(_, User, Server,
	     #userlist{list = List, needdb = NeedDb},
	     {From, To, #xmlel{name = PName, attrs = Attrs}}, Dir) ->
    case List of
      [] -> allow;
      _ ->
	  PType = case PName of
		    <<"message">> -> message;
		    <<"iq">> -> iq;
		    <<"presence">> ->
			case fxml:get_attr_s(<<"type">>, Attrs) of
			  %% notification
			  <<"">> -> presence;
			  <<"unavailable">> -> presence;
			  %% subscribe, subscribed, unsubscribe,
			  %% unsubscribed, error, probe, or other
			  _ -> other
			end
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
	  {Subscription, Groups} = case NeedDb of
				     true ->
					 ejabberd_hooks:run_fold(roster_get_jid_info,
								 jid:nameprep(Server),
								 {none, []},
								 [User, Server,
								  LJID]);
				     false -> {[], []}
				   end,
	  check_packet_aux(List, PType2, LJID, Subscription,
			   Groups)
    end.

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
	  case Type of
	    none -> Action;
	    _ ->
		case is_type_match(Type, Value, JID, Subscription,
				   Groups)
		    of
		  true -> Action;
		  false ->
		      check_packet_aux(List, PType, JID, Subscription, Groups)
		end
	  end;
      false ->
	  check_packet_aux(List, PType, JID, Subscription, Groups)
    end.

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

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer).

updated_list(_, #userlist{name = OldName} = Old,
	     #userlist{name = NewName} = New) ->
    if OldName == NewName -> New;
       true -> Old
    end.

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, Data) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Data).

mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [db_type, iqdisc].
