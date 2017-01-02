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

-export([start/2, stop/1, process_iq/1, export/1, import_info/0,
	 process_iq_set/3, process_iq_get/3, get_user_list/3,
	 check_packet/6, remove_user/2, encode_list_item/1,
	 is_list_needdb/1, updated_list/3,
	 import_start/2, import_stop/2,
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

-spec process_iq(iq()) -> iq().
process_iq(IQ) ->
    xmpp:make_error(IQ, xmpp:err_not_allowed()).

-spec process_iq_get({error, stanza_error()} | {result, xmpp_element() | undefined},
		     iq(), userlist()) -> {error, stanza_error()} |
					  {result, xmpp_element() | undefined}.
process_iq_get(_, #iq{lang = Lang,
		      sub_els = [#privacy_query{default = Default,
						active = Active}]},
	       _) when Default /= undefined; Active /= undefined ->
    Txt = <<"Only <list/> element is allowed in this query">>,
    {error, xmpp:err_bad_request(Txt, Lang)};
process_iq_get(_, #iq{from = From, lang = Lang,
		      sub_els = [#privacy_query{lists = Lists}]},
	       #userlist{name = Active}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case Lists of
	[] ->
	    process_lists_get(LUser, LServer, Active, Lang);
	[#privacy_list{name = ListName}] ->
	    process_list_get(LUser, LServer, ListName, Lang);
	_ ->
	    Txt = <<"Too many <list/> elements">>,
	    {error, xmpp:err_bad_request(Txt, Lang)}
    end;
process_iq_get(Acc, _, _) ->
    Acc.

-spec process_lists_get(binary(), binary(), binary(), binary()) ->
			       {error, stanza_error()} | {result, privacy_query()}.
process_lists_get(LUser, LServer, Active, Lang) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_lists_get(LUser, LServer) of
	error ->
	    Txt = <<"Database failure">>,
	    {error, xmpp:err_internal_server_error(Txt, Lang)};
	{_Default, []} ->
	    {result, #privacy_query{}};
	{Default, ListNames} ->
	    {result,
	     #privacy_query{active = Active,
			    default = Default,
			    lists = [#privacy_list{name = ListName}
				     || ListName <- ListNames]}}
    end.

-spec process_list_get(binary(), binary(), binary(), binary()) ->
			      {error, stanza_error()} | {result, privacy_query()}.
process_list_get(LUser, LServer, Name, Lang) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_list_get(LUser, LServer, Name) of
	error ->
	    Txt = <<"Database failure">>,
	    {error, xmpp:err_internal_server_error(Txt, Lang)};
	not_found ->
	    Txt = <<"No privacy list with this name found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Items ->
	    LItems = lists:map(fun encode_list_item/1, Items),
	    {result,
	     #privacy_query{
		lists = [#privacy_list{name = Name, items = LItems}]}}
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
	jid -> jid:to_string(Val);
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
	jid -> jid:tolower(jid:from_string(Value));
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

-spec process_iq_set({error, stanza_error()} |
		     {result, xmpp_element() | undefined} |
		     {result, xmpp_element() | undefined, userlist()},
		     iq(), #userlist{}) ->
			    {error, stanza_error()} |
			    {result, xmpp_element() | undefined} |
			    {result, xmpp_element() | undefined, userlist()}.
process_iq_set(_, #iq{from = From, lang = Lang,
		      sub_els = [#privacy_query{default = Default,
						active = Active,
						lists = Lists}]},
	      #userlist{} = UserList) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case Lists of
	[#privacy_list{items = Items, name = ListName}]
	  when Default == undefined, Active == undefined ->
	    process_lists_set(LUser, LServer, ListName, Items, UserList, Lang);
	[] when Default == undefined, Active /= undefined ->
	    process_active_set(LUser, LServer, Active, Lang);
	[] when Active == undefined, Default /= undefined ->
	    process_default_set(LUser, LServer, Default, Lang);
	_ ->
	    Txt = <<"The stanza MUST contain only one <active/> element, "
		    "one <default/> element, or one <list/> element">>,
	    {error, xmpp:err_bad_request(Txt, Lang)}
    end;
process_iq_set(Acc, _, _) ->
    Acc.

-spec process_default_set(binary(), binary(), none | binary(),
			  binary()) -> {error, stanza_error()} | {result, undefined}.
process_default_set(LUser, LServer, Value, Lang) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_default_set(LUser, LServer, Value) of
	{atomic, error} ->
	    Txt = <<"Database failure">>,
	    {error, xmpp:err_internal_server_error(Txt, Lang)};
	{atomic, not_found} ->
	    Txt = <<"No privacy list with this name found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	{atomic, ok} ->
	    {result, undefined};
	Err ->
	    ?ERROR_MSG("failed to set default list '~s' for user ~s@~s: ~p",
		       [Value, LUser, LServer, Err]),
	    {error, xmpp:err_internal_server_error()}
    end.

-spec process_active_set(binary(), binary(), none | binary(), binary()) ->
				{error, stanza_error()} |
				{result, undefined, userlist()}.
process_active_set(_LUser, _LServer, none, _Lang) ->
    {result, undefined, #userlist{}};
process_active_set(LUser, LServer, Name, Lang) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:process_active_set(LUser, LServer, Name) of
	error ->
	    Txt = <<"No privacy list with this name found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	Items ->
	    NeedDb = is_list_needdb(Items),
	    {result, undefined,
	     #userlist{name = Name, list = Items, needdb = NeedDb}}
    end.

-spec set_privacy_list(privacy()) -> any().
set_privacy_list(#privacy{us = {_, LServer}} = Privacy) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_privacy_list(Privacy).

-spec process_lists_set(binary(), binary(), binary(), [privacy_item()],
			#userlist{}, binary()) -> {error, stanza_error()} |
						  {result, undefined}.
process_lists_set(_LUser, _LServer, Name, [], #userlist{name = Name}, Lang) ->
    Txt = <<"Cannot remove active list">>,
    {error, xmpp:err_conflict(Txt, Lang)};
process_lists_set(LUser, LServer, Name, [], _UserList, Lang) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:remove_privacy_list(LUser, LServer, Name) of
	{atomic, conflict} ->
	    Txt = <<"Cannot remove default list">>,
	    {error, xmpp:err_conflict(Txt, Lang)};
	{atomic, not_found} ->
	    Txt = <<"No privacy list with this name found">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)};
	{atomic, ok} ->
	    ejabberd_sm:route(jid:make(LUser, LServer,
				       <<"">>),
			      jid:make(LUser, LServer, <<"">>),
			      {broadcast, {privacy_list,
					   #userlist{name = Name,
						     list = []},
					   Name}}),
	    {result, undefined};
	Err ->
	    ?ERROR_MSG("failed to remove privacy list '~s' for user ~s@~s: ~p",
		       [Name, LUser, LServer, Err]),
	    Txt = <<"Database failure">>,
	    {error, xmpp:err_internal_server_error(Txt, Lang)}
    end;
process_lists_set(LUser, LServer, Name, Items, _UserList, Lang) ->
    case catch lists:map(fun decode_item/1, Items) of
	{error, Why} ->
	    Txt = xmpp:format_error(Why),
	    {error, xmpp:err_bad_request(Txt, Lang)};
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
		    {result, undefined};
		Err ->
		    ?ERROR_MSG("failed to set privacy list '~s' "
			       "for user ~s@~s: ~p",
			       [Name, LUser, LServer, Err]),
		    Txt = <<"Database failure">>,
		    {error, xmpp:err_internal_server_error(Txt, Lang)}
	    end
    end.

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

-spec get_user_list(userlist(), binary(), binary()) -> userlist().
get_user_list(_Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    {Default, Items} = Mod:get_user_list(LUser, LServer),
    NeedDb = is_list_needdb(Items),
    #userlist{name = Default, list = Items,
	      needdb = NeedDb}.

-spec get_user_lists(binary(), binary()) -> {ok, privacy()} | error.
get_user_lists(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_user_lists(LUser, LServer).

%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
-spec check_packet(allow | deny, binary(), binary(), userlist(),
		   {jid(), jid(), stanza()}, in | out) -> allow | deny.
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
	     {From, To, Packet}, Dir) ->
    case List of
      [] -> allow;
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

-spec updated_list(userlist(), userlist(), userlist()) -> userlist().
updated_list(_, #userlist{name = OldName} = Old,
	     #userlist{name = NewName} = New) ->
    if OldName == NewName -> New;
       true -> Old
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

mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [db_type, iqdisc].
