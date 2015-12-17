%%%----------------------------------------------------------------------
%%% File    : mod_blocking.erl
%%% Author  : Stephan Maka
%%% Purpose : XEP-0191: Simple Communications Blocking
%%% Created : 24 Aug 2008 by Stephan Maka <stephan@spaceboyz.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(mod_blocking).

-behaviour(gen_mod).

-protocol({xep, 191, '1.2'}).

-export([start/2, stop/1, process_iq/3,
	 process_iq_set/4, process_iq_get/5, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    ejabberd_hooks:add(privacy_iq_get, Host, ?MODULE,
		       process_iq_get, 40),
    ejabberd_hooks:add(privacy_iq_set, Host, ?MODULE,
		       process_iq_set, 40),
    mod_disco:register_feature(Host, ?NS_BLOCKING),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_BLOCKING, ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(privacy_iq_get, Host, ?MODULE,
			  process_iq_get, 40),
    ejabberd_hooks:delete(privacy_iq_set, Host, ?MODULE,
			  process_iq_set, 40),
    mod_disco:unregister_feature(Host, ?NS_BLOCKING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_BLOCKING).

process_iq(_From, _To, IQ) ->
    SubEl = IQ#iq.sub_el,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

process_iq_get(_, From, _To,
	       #iq{xmlns = ?NS_BLOCKING,
		   sub_el = #xmlel{name = <<"blocklist">>}},
	       _) ->
    #jid{luser = LUser, lserver = LServer} = From,
    {stop, process_blocklist_get(LUser, LServer)};
process_iq_get(Acc, _, _, _, _) -> Acc.

process_iq_set(_, From, _To,
	       #iq{xmlns = ?NS_BLOCKING,
		   sub_el =
		       #xmlel{name = SubElName, children = SubEls}}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    Res = case {SubElName, xml:remove_cdata(SubEls)} of
	    {<<"block">>, []} -> {error, ?ERR_BAD_REQUEST};
	    {<<"block">>, Els} ->
		JIDs = parse_blocklist_items(Els, []),
		process_blocklist_block(LUser, LServer, JIDs);
	    {<<"unblock">>, []} ->
		process_blocklist_unblock_all(LUser, LServer);
	    {<<"unblock">>, Els} ->
		JIDs = parse_blocklist_items(Els, []),
		process_blocklist_unblock(LUser, LServer, JIDs);
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end,
    {stop, Res};
process_iq_set(Acc, _, _, _) -> Acc.

list_to_blocklist_jids([], JIDs) -> JIDs;
list_to_blocklist_jids([#listitem{type = jid,
				  action = deny, value = JID} =
			    Item
			| Items],
		       JIDs) ->
    case Item of
      #listitem{match_all = true} -> Match = true;
      #listitem{match_iq = true, match_message = true,
		match_presence_in = true, match_presence_out = true} ->
	  Match = true;
      _ -> Match = false
    end,
    if Match -> list_to_blocklist_jids(Items, [JID | JIDs]);
       true -> list_to_blocklist_jids(Items, JIDs)
    end;
% Skip Privacy List items than cannot be mapped to Blocking items
list_to_blocklist_jids([_ | Items], JIDs) ->
    list_to_blocklist_jids(Items, JIDs).

parse_blocklist_items([], JIDs) -> JIDs;
parse_blocklist_items([#xmlel{name = <<"item">>,
			      attrs = Attrs}
		       | Els],
		      JIDs) ->
    case xml:get_attr(<<"jid">>, Attrs) of
      {value, JID1} ->
	  JID = jid:tolower(jid:from_string(JID1)),
	  parse_blocklist_items(Els, [JID | JIDs]);
      false -> parse_blocklist_items(Els, JIDs)
    end;
parse_blocklist_items([_ | Els], JIDs) ->
    parse_blocklist_items(Els, JIDs).

process_blocklist_block(LUser, LServer, JIDs) ->
    Filter = fun (List) ->
		     AlreadyBlocked = list_to_blocklist_jids(List, []),
		     lists:foldr(fun (JID, List1) ->
					 case lists:member(JID, AlreadyBlocked)
					     of
					   true -> List1;
					   false ->
					       [#listitem{type = jid,
							  value = JID,
							  action = deny,
							  order = 0,
							  match_all = true}
						| List1]
					 end
				 end,
				 List, JIDs)
	     end,
    case process_blocklist_block(LUser, LServer, Filter,
				 gen_mod:db_type(LServer, mod_privacy))
	of
      {atomic, {ok, Default, List}} ->
	  UserList = make_userlist(Default, List),
	  broadcast_list_update(LUser, LServer, Default,
				UserList),
	  broadcast_blocklist_event(LUser, LServer,
				    {block, JIDs}),
	  {result, [], UserList};
      _Err ->
	    ?ERROR_MSG("Error processing ~p: ~p", [{LUser, LServer, JIDs}, _Err]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_blocklist_block(LUser, LServer, Filter,
			mnesia) ->
    F = fun () ->
		case mnesia:wread({privacy, {LUser, LServer}}) of
		  [] ->
		      P = #privacy{us = {LUser, LServer}},
		      NewDefault = <<"Blocked contacts">>,
		      NewLists1 = [],
		      List = [];
		  [#privacy{default = Default, lists = Lists} = P] ->
		      case lists:keysearch(Default, 1, Lists) of
			{value, {_, List}} ->
			    NewDefault = Default,
			    NewLists1 = lists:keydelete(Default, 1, Lists);
			false ->
			    NewDefault = <<"Blocked contacts">>,
			    NewLists1 = Lists,
			    List = []
		      end
		end,
		NewList = Filter(List),
		NewLists = [{NewDefault, NewList} | NewLists1],
		mnesia:write(P#privacy{default = NewDefault,
				       lists = NewLists}),
		{ok, NewDefault, NewList}
	end,
    mnesia:transaction(F);
process_blocklist_block(LUser, LServer, Filter,
			riak) ->
    {atomic,
     begin
         case ejabberd_riak:get(privacy, mod_privacy:privacy_schema(),
				{LUser, LServer}) of
             {ok, #privacy{default = Default, lists = Lists} = P} ->
                 case lists:keysearch(Default, 1, Lists) of
                     {value, {_, List}} ->
                         NewDefault = Default,
                         NewLists1 = lists:keydelete(Default, 1, Lists);
                     false ->
                         NewDefault = <<"Blocked contacts">>,
                         NewLists1 = Lists,
                         List = []
                 end;
             {error, _} ->
                 P = #privacy{us = {LUser, LServer}},
                 NewDefault = <<"Blocked contacts">>,
                 NewLists1 = [],
                 List = []
         end,
         NewList = Filter(List),
         NewLists = [{NewDefault, NewList} | NewLists1],
         case ejabberd_riak:put(P#privacy{default = NewDefault,
                                          lists = NewLists},
				mod_privacy:privacy_schema()) of
             ok ->
                 {ok, NewDefault, NewList};
             Err ->
                 Err
         end
     end};
process_blocklist_block(LUser, LServer, Filter, odbc) ->
    F = fun () ->
		Default = case
			    mod_privacy:sql_get_default_privacy_list_t(LUser)
			      of
			    {selected, [<<"name">>], []} ->
				Name = <<"Blocked contacts">>,
				mod_privacy:sql_add_privacy_list(LUser, Name),
				mod_privacy:sql_set_default_privacy_list(LUser,
									 Name),
				Name;
			    {selected, [<<"name">>], [[Name]]} -> Name
			  end,
		{selected, [<<"id">>], [[ID]]} =
		    mod_privacy:sql_get_privacy_list_id_t(LUser, Default),
		case mod_privacy:sql_get_privacy_list_data_by_id_t(ID)
		    of
		  {selected,
		   [<<"t">>, <<"value">>, <<"action">>, <<"ord">>,
		    <<"match_all">>, <<"match_iq">>, <<"match_message">>,
		    <<"match_presence_in">>, <<"match_presence_out">>],
		   RItems = [_ | _]} ->
		      List = lists:flatmap(fun mod_privacy:raw_to_item/1, RItems);
		  _ -> List = []
		end,
		NewList = Filter(List),
		NewRItems = lists:map(fun mod_privacy:item_to_raw/1,
				      NewList),
		mod_privacy:sql_set_privacy_list(ID, NewRItems),
		{ok, Default, NewList}
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

process_blocklist_unblock_all(LUser, LServer) ->
    Filter = fun (List) ->
		     lists:filter(fun (#listitem{action = A}) -> A =/= deny
				  end,
				  List)
	     end,
    DBType = gen_mod:db_type(LServer, mod_privacy),
    case unblock_by_filter(LUser, LServer, Filter, DBType) of
      {atomic, ok} -> {result, []};
      {atomic, {ok, Default, List}} ->
	  UserList = make_userlist(Default, List),
	  broadcast_list_update(LUser, LServer, Default,
				UserList),
	  broadcast_blocklist_event(LUser, LServer, unblock_all),
	  {result, [], UserList};
      _Err ->
	    ?ERROR_MSG("Error processing ~p: ~p", [{LUser, LServer}, _Err]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

process_blocklist_unblock(LUser, LServer, JIDs) ->
    Filter = fun (List) ->
		     lists:filter(fun (#listitem{action = deny, type = jid,
						 value = JID}) ->
					  not lists:member(JID, JIDs);
				      (_) -> true
				  end,
				  List)
	     end,
    DBType = gen_mod:db_type(LServer, mod_privacy),
    case unblock_by_filter(LUser, LServer, Filter, DBType) of
      {atomic, ok} -> {result, []};
      {atomic, {ok, Default, List}} ->
	  UserList = make_userlist(Default, List),
	  broadcast_list_update(LUser, LServer, Default,
				UserList),
	  broadcast_blocklist_event(LUser, LServer,
				    {unblock, JIDs}),
	  {result, [], UserList};
      _Err ->
	    ?ERROR_MSG("Error processing ~p: ~p", [{LUser, LServer, JIDs}, _Err]),
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

unblock_by_filter(LUser, LServer, Filter, mnesia) ->
    F = fun () ->
		case mnesia:read({privacy, {LUser, LServer}}) of
		  [] ->
		      % No lists, nothing to unblock
		      ok;
		  [#privacy{default = Default, lists = Lists} = P] ->
		      case lists:keysearch(Default, 1, Lists) of
			{value, {_, List}} ->
			    NewList = Filter(List),
			    NewLists1 = lists:keydelete(Default, 1, Lists),
			    NewLists = [{Default, NewList} | NewLists1],
			    mnesia:write(P#privacy{lists = NewLists}),
			    {ok, Default, NewList};
			false ->
			    % No default list, nothing to unblock
			    ok
		      end
		end
	end,
    mnesia:transaction(F);
unblock_by_filter(LUser, LServer, Filter, riak) ->
    {atomic,
     case ejabberd_riak:get(privacy, mod_privacy:privacy_schema(),
			    {LUser, LServer}) of
         {error, _} ->
             %% No lists, nothing to unblock
             ok;
         {ok, #privacy{default = Default, lists = Lists} = P} ->
             case lists:keysearch(Default, 1, Lists) of
                 {value, {_, List}} ->
                     NewList = Filter(List),
                     NewLists1 = lists:keydelete(Default, 1, Lists),
                     NewLists = [{Default, NewList} | NewLists1],
                     case ejabberd_riak:put(P#privacy{lists = NewLists},
					    mod_privacy:privacy_schema()) of
                         ok ->
                             {ok, Default, NewList};
                         Err ->
                             Err
                     end;
                 false ->
                     %% No default list, nothing to unblock
                     ok
             end
     end};
unblock_by_filter(LUser, LServer, Filter, odbc) ->
    F = fun () ->
		case mod_privacy:sql_get_default_privacy_list_t(LUser)
		    of
		  {selected, [<<"name">>], []} -> ok;
		  {selected, [<<"name">>], [[Default]]} ->
		      {selected, [<<"id">>], [[ID]]} =
			  mod_privacy:sql_get_privacy_list_id_t(LUser, Default),
		      case mod_privacy:sql_get_privacy_list_data_by_id_t(ID)
			  of
			{selected,
			 [<<"t">>, <<"value">>, <<"action">>, <<"ord">>,
			  <<"match_all">>, <<"match_iq">>, <<"match_message">>,
			  <<"match_presence_in">>, <<"match_presence_out">>],
			 RItems = [_ | _]} ->
			    List = lists:flatmap(fun mod_privacy:raw_to_item/1,
                                                 RItems),
			    NewList = Filter(List),
			    NewRItems = lists:map(fun mod_privacy:item_to_raw/1,
						  NewList),
			    mod_privacy:sql_set_privacy_list(ID, NewRItems),
			    {ok, Default, NewList};
			_ -> ok
		      end;
		  _ -> ok
		end
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

make_userlist(Name, List) ->
    NeedDb = mod_privacy:is_list_needdb(List),
    #userlist{name = Name, list = List, needdb = NeedDb}.

broadcast_list_update(LUser, LServer, Name, UserList) ->
    ejabberd_sm:route(jid:make(LUser, LServer,
                                    <<"">>),
                      jid:make(LUser, LServer, <<"">>),
                      {broadcast, {privacy_list, UserList, Name}}).

broadcast_blocklist_event(LUser, LServer, Event) ->
    JID = jid:make(LUser, LServer, <<"">>),
    ejabberd_sm:route(JID, JID,
                      {broadcast, {blocking, Event}}).

process_blocklist_get(LUser, LServer) ->
    case process_blocklist_get(LUser, LServer,
			       gen_mod:db_type(LServer, mod_privacy))
	of
      error -> {error, ?ERR_INTERNAL_SERVER_ERROR};
      List ->
	  JIDs = list_to_blocklist_jids(List, []),
	  Items = lists:map(fun (JID) ->
				    ?DEBUG("JID: ~p", [JID]),
				    #xmlel{name = <<"item">>,
					   attrs =
					       [{<<"jid">>,
						 jid:to_string(JID)}],
					   children = []}
			    end,
			    JIDs),
	  {result,
	   [#xmlel{name = <<"blocklist">>,
		   attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
		   children = Items}]}
    end.

process_blocklist_get(LUser, LServer, mnesia) ->
    case catch mnesia:dirty_read(privacy, {LUser, LServer})
	of
      {'EXIT', _Reason} -> error;
      [] -> [];
      [#privacy{default = Default, lists = Lists}] ->
	  case lists:keysearch(Default, 1, Lists) of
	    {value, {_, List}} -> List;
	    _ -> []
	  end
    end;
process_blocklist_get(LUser, LServer, riak) ->
    case ejabberd_riak:get(privacy, mod_privacy:privacy_schema(),
			   {LUser, LServer}) of
        {ok, #privacy{default = Default, lists = Lists}} ->
            case lists:keysearch(Default, 1, Lists) of
                {value, {_, List}} -> List;
                _ -> []
            end;
        {error, notfound} ->
            [];
        {error, _} ->
            error
    end;
process_blocklist_get(LUser, LServer, odbc) ->
    case catch
	   mod_privacy:sql_get_default_privacy_list(LUser, LServer)
	of
      {selected, [<<"name">>], []} -> [];
      {selected, [<<"name">>], [[Default]]} ->
	  case catch mod_privacy:sql_get_privacy_list_data(LUser,
							   LServer, Default)
	      of
	    {selected,
	     [<<"t">>, <<"value">>, <<"action">>, <<"ord">>,
	      <<"match_all">>, <<"match_iq">>, <<"match_message">>,
	      <<"match_presence_in">>, <<"match_presence_out">>],
	     RItems} ->
		lists:flatmap(fun mod_privacy:raw_to_item/1, RItems);
	    {'EXIT', _} -> error
	  end;
      {'EXIT', _} -> error
    end.

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [iqdisc].
