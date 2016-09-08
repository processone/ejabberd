%%%----------------------------------------------------------------------
%%% File    : mod_blocking.erl
%%% Author  : Stephan Maka
%%% Purpose : XEP-0191: Simple Communications Blocking
%%% Created : 24 Aug 2008 by Stephan Maka <stephan@spaceboyz.net>
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

-module(mod_blocking).

-behaviour(gen_mod).

-protocol({xep, 191, '1.2'}).

-export([start/2, stop/1, process_iq/1,
	 process_iq_set/2, process_iq_get/3, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("mod_privacy.hrl").

-callback process_blocklist_block(binary(), binary(), function()) -> {atomic, any()}.
-callback unblock_by_filter(binary(), binary(), function()) -> {atomic, any()}.
-callback process_blocklist_get(binary(), binary()) -> [listitem()] | error.

-type block_event() :: {block, [jid()]} | {unblock, [jid()]} | unblock_all.

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

depends(_Host, _Opts) ->
    [{mod_privacy, hard}].

-spec process_iq(iq()) -> iq().
process_iq(IQ) ->
    xmpp:make_error(IQ, xmpp:err_not_allowed()).

-spec process_iq_get({error, stanza_error()} | {result, xmpp_element() | undefined},
		     iq(), userlist()) ->
			    {error, stanza_error()} | {result, block_list()}.
process_iq_get(_, #iq{lang = Lang, from = From,
		      sub_els = [#block_list{}]}, _) ->
    #jid{luser = LUser, lserver = LServer} = From,
    {stop, process_blocklist_get(LUser, LServer, Lang)};
process_iq_get(Acc, _, _) -> Acc.

-spec process_iq_set({error, stanza_error()} |
		     {result, xmpp_element() | undefined} |
		     {result, xmpp_element() | undefined, userlist()},
		     iq()) -> {error, stanza_error()} |
			      {result, undefined} |
			      {result, undefined, userlist()}.
process_iq_set(_, #iq{from = From, lang = Lang, sub_els = [SubEl]}) ->
    #jid{luser = LUser, lserver = LServer} = From,
    Res = case SubEl of
	      #block{items = []} ->
		  Txt = <<"No items found in this query">>,
		  {error, xmpp:err_bad_request(Txt, Lang)};
	      #block{items = Items} ->
		  JIDs = [jid:tolower(Item) || Item <- Items],
		  process_blocklist_block(LUser, LServer, JIDs, Lang);
	      #unblock{items = []} ->
		  process_blocklist_unblock_all(LUser, LServer, Lang);
	      #unblock{items = Items} ->
		  JIDs = [jid:tolower(Item) || Item <- Items],
		  process_blocklist_unblock(LUser, LServer, JIDs, Lang);
	      _ ->
		  Txt = <<"Only <block/> and <unblock/> are allowed "
			  "in this request">>,
		  {error, xmpp:err_bad_request(Txt, Lang)}
	  end,
    {stop, Res};
process_iq_set(Acc, _) -> Acc.

-spec list_to_blocklist_jids([listitem()], [ljid()]) -> [ljid()].
list_to_blocklist_jids([], JIDs) -> JIDs;
list_to_blocklist_jids([#listitem{type = jid,
				  action = deny, value = JID} =
			    Item
			| Items],
		       JIDs) ->
    Match = case Item of
		#listitem{match_all = true} ->
		    true;
		#listitem{match_iq = true,
			  match_message = true,
			  match_presence_in = true,
			  match_presence_out = true} ->
		    true;
		_ ->
		    false
	    end,
    if Match -> list_to_blocklist_jids(Items, [JID | JIDs]);
       true -> list_to_blocklist_jids(Items, JIDs)
    end;
% Skip Privacy List items than cannot be mapped to Blocking items
list_to_blocklist_jids([_ | Items], JIDs) ->
    list_to_blocklist_jids(Items, JIDs).

-spec process_blocklist_block(binary(), binary(), [ljid()],
			      binary()) ->
				     {error, stanza_error()} |
				     {result, undefined, userlist()}.
process_blocklist_block(LUser, LServer, JIDs, Lang) ->
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
    Mod = db_mod(LServer),
    case Mod:process_blocklist_block(LUser, LServer, Filter) of
      {atomic, {ok, Default, List}} ->
	  UserList = make_userlist(Default, List),
	  broadcast_list_update(LUser, LServer, Default,
				UserList),
	  broadcast_blocklist_event(LUser, LServer,
				    {block, [jid:make(J) || J <- JIDs]}),
	  {result, undefined, UserList};
      _Err ->
	    ?ERROR_MSG("Error processing ~p: ~p", [{LUser, LServer, JIDs}, _Err]),
	    {error, xmpp:err_internal_server_error(<<"Database failure">>, Lang)}
    end.

-spec process_blocklist_unblock_all(binary(), binary(), binary()) ->
					   {error, stanza_error()} |
					   {result, undefined} |
					   {result, undefined, userlist()}.
process_blocklist_unblock_all(LUser, LServer, Lang) ->
    Filter = fun (List) ->
		     lists:filter(fun (#listitem{action = A}) -> A =/= deny
				  end,
				  List)
	     end,
    Mod = db_mod(LServer),
    case Mod:unblock_by_filter(LUser, LServer, Filter) of
      {atomic, ok} -> {result, undefined};
      {atomic, {ok, Default, List}} ->
	  UserList = make_userlist(Default, List),
	  broadcast_list_update(LUser, LServer, Default,
				UserList),
	  broadcast_blocklist_event(LUser, LServer, unblock_all),
	  {result, undefined, UserList};
      _Err ->
	    ?ERROR_MSG("Error processing ~p: ~p", [{LUser, LServer}, _Err]),
	    {error, xmpp:err_internal_server_error(<<"Database failure">>, Lang)}
    end.

-spec process_blocklist_unblock(binary(), binary(), [ljid()], binary()) ->
				       {error, stanza_error()} |
				       {result, undefined} |
				       {result, undefined, userlist()}.
process_blocklist_unblock(LUser, LServer, JIDs, Lang) ->
    Filter = fun (List) ->
		     lists:filter(fun (#listitem{action = deny, type = jid,
						 value = JID}) ->
					  not lists:member(JID, JIDs);
				      (_) -> true
				  end,
				  List)
	     end,
    Mod = db_mod(LServer),
    case Mod:unblock_by_filter(LUser, LServer, Filter) of
      {atomic, ok} -> {result, undefined};
      {atomic, {ok, Default, List}} ->
	  UserList = make_userlist(Default, List),
	  broadcast_list_update(LUser, LServer, Default,
				UserList),
	  broadcast_blocklist_event(LUser, LServer,
				    {unblock, [jid:make(J) || J <- JIDs]}),
	  {result, undefined, UserList};
      _Err ->
	    ?ERROR_MSG("Error processing ~p: ~p", [{LUser, LServer, JIDs}, _Err]),
	    {error, xmpp:err_internal_server_error(<<"Database failure">>, Lang)}
    end.

-spec make_userlist(binary(), [listitem()]) -> userlist().
make_userlist(Name, List) ->
    NeedDb = mod_privacy:is_list_needdb(List),
    #userlist{name = Name, list = List, needdb = NeedDb}.

-spec broadcast_list_update(binary(), binary(), binary(), userlist()) -> ok.
broadcast_list_update(LUser, LServer, Name, UserList) ->
    ejabberd_sm:route(jid:make(LUser, LServer, <<"">>),
                      jid:make(LUser, LServer, <<"">>),
                      {broadcast, {privacy_list, UserList, Name}}).

-spec broadcast_blocklist_event(binary(), binary(), block_event()) -> ok.
broadcast_blocklist_event(LUser, LServer, Event) ->
    JID = jid:make(LUser, LServer, <<"">>),
    ejabberd_sm:route(JID, JID,
                      {broadcast, {blocking, Event}}).

-spec process_blocklist_get(binary(), binary(), binary()) ->
				   {error, stanza_error()} | {result, block_list()}.
process_blocklist_get(LUser, LServer, Lang) ->
    Mod = db_mod(LServer),
    case Mod:process_blocklist_get(LUser, LServer) of
      error ->
	  {error, xmpp:err_internal_server_error(<<"Database failure">>, Lang)};
      List ->
	  LJIDs = list_to_blocklist_jids(List, []),
	  Items = [jid:make(J) || J <- LJIDs],
	  {result, #block_list{items = Items}}
    end.

-spec db_mod(binary()) -> module().
db_mod(LServer) ->
    DBType = gen_mod:db_type(LServer, mod_privacy),
    gen_mod:db_mod(DBType, ?MODULE).

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [iqdisc].
