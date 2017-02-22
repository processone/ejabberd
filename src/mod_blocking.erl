%%%----------------------------------------------------------------------
%%% File    : mod_blocking.erl
%%% Author  : Stephan Maka
%%% Purpose : XEP-0191: Simple Communications Blocking
%%% Created : 24 Aug 2008 by Stephan Maka <stephan@spaceboyz.net>
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

-module(mod_blocking).

-behaviour(gen_mod).

-protocol({xep, 191, '1.2'}).

-export([start/2, stop/1, reload/3, process_iq/1, mod_opt_type/1, depends/2,
	 disco_features/5]).

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
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, disco_features, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_BLOCKING, ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, disco_features, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_BLOCKING).

reload(Host, NewOpts, OldOpts) ->
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
	    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_BLOCKING,
					  ?MODULE, process_iq, IQDisc);
	true ->
	    ok
    end.

depends(_Host, _Opts) ->
    [{mod_privacy, hard}].

-spec disco_features({error, stanza_error()} | {result, [binary()]} | empty,
		     jid(), jid(), binary(), binary()) ->
			    {error, stanza_error()} | {result, [binary()]}.
disco_features({error, Err}, _From, _To, _Node, _Lang) ->
    {error, Err};
disco_features(empty, _From, _To, <<"">>, _Lang) ->
    {result, [?NS_BLOCKING]};
disco_features({result, Feats}, _From, _To, <<"">>, _Lang) ->
    {result, [?NS_BLOCKING|Feats]};
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
process_iq_get(#iq{sub_els = [#block_list{}]} = IQ) ->
    process_get(IQ);
process_iq_get(#iq{lang = Lang} = IQ) ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

-spec process_iq_set(iq()) -> iq().
process_iq_set(#iq{lang = Lang, sub_els = [SubEl]} = IQ) ->
    case SubEl of
	#block{items = []} ->
	    Txt = <<"No items found in this query">>,
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	#block{items = Items} ->
	    JIDs = [jid:tolower(Item) || Item <- Items],
	    process_block(IQ, JIDs);
	#unblock{items = []} ->
	    process_unblock_all(IQ);
	#unblock{items = Items} ->
	    JIDs = [jid:tolower(Item) || Item <- Items],
	    process_unblock(IQ, JIDs);
	_ ->
	    Txt = <<"No module is handling this query">>,
	    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang))
    end.

-spec listitems_to_jids([listitem()], [ljid()]) -> [ljid()].
listitems_to_jids([], JIDs) ->
    JIDs;
listitems_to_jids([#listitem{type = jid,
			     action = deny, value = JID} = Item | Items],
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
    if Match -> listitems_to_jids(Items, [JID | JIDs]);
       true -> listitems_to_jids(Items, JIDs)
    end;
% Skip Privacy List items than cannot be mapped to Blocking items
listitems_to_jids([_ | Items], JIDs) ->
    listitems_to_jids(Items, JIDs).

-spec process_block(iq(), [ljid()]) -> iq().
process_block(#iq{from = #jid{luser = LUser, lserver = LServer},
		  lang = Lang} = IQ, JIDs) ->
    Filter = fun (List) ->
		     AlreadyBlocked = listitems_to_jids(List, []),
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
	    broadcast_list_update(LUser, LServer, UserList, Default),
	    broadcast_event(LUser, LServer,
			    #block{items = [jid:make(J) || J <- JIDs]}),
	    xmpp:make_iq_result(xmpp:put_meta(IQ, privacy_list, UserList));
      _Err ->
	    ?ERROR_MSG("Error processing ~p: ~p", [{LUser, LServer, JIDs}, _Err]),
	    Err = xmpp:err_internal_server_error(<<"Database failure">>, Lang),
	    xmpp:make_error(IQ, Err)
    end.

-spec process_unblock_all(iq()) -> iq().
process_unblock_all(#iq{from = #jid{luser = LUser, lserver = LServer},
			lang = Lang} = IQ) ->
    Filter = fun (List) ->
		     lists:filter(fun (#listitem{action = A}) -> A =/= deny
				  end,
				  List)
	     end,
    Mod = db_mod(LServer),
    case Mod:unblock_by_filter(LUser, LServer, Filter) of
	{atomic, ok} ->
	    xmpp:make_iq_result(IQ);
      {atomic, {ok, Default, List}} ->
	  UserList = make_userlist(Default, List),
	    broadcast_list_update(LUser, LServer, UserList, Default),
	    broadcast_event(LUser, LServer, #unblock{}),
	    xmpp:make_iq_result(xmpp:put_meta(IQ, privacy_list, UserList));
      _Err ->
	    ?ERROR_MSG("Error processing ~p: ~p", [{LUser, LServer}, _Err]),
	    Err = xmpp:err_internal_server_error(<<"Database failure">>, Lang),
	    xmpp:make_error(IQ, Err)
    end.

-spec process_unblock(iq(), [ljid()]) -> iq().
process_unblock(#iq{from = #jid{luser = LUser, lserver = LServer},
		    lang = Lang} = IQ, JIDs) ->
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
	{atomic, ok} ->
	    xmpp:make_iq_result(IQ);
      {atomic, {ok, Default, List}} ->
	  UserList = make_userlist(Default, List),
	    broadcast_list_update(LUser, LServer, UserList, Default),
	    broadcast_event(LUser, LServer,
			    #unblock{items = [jid:make(J) || J <- JIDs]}),
	    xmpp:make_iq_result(xmpp:put_meta(IQ, privacy_list, UserList));
      _Err ->
	    ?ERROR_MSG("Error processing ~p: ~p", [{LUser, LServer, JIDs}, _Err]),
	    Err = xmpp:err_internal_server_error(<<"Database failure">>, Lang),
	    xmpp:make_error(IQ, Err)
    end.

-spec make_userlist(binary(), [listitem()]) -> userlist().
make_userlist(Name, List) ->
    NeedDb = mod_privacy:is_list_needdb(List),
    #userlist{name = Name, list = List, needdb = NeedDb}.

-spec broadcast_list_update(binary(), binary(), userlist(), binary()) -> ok.
broadcast_list_update(LUser, LServer, UserList, Name) ->
    mod_privacy:push_list_update(jid:make(LUser, LServer), UserList, Name).

-spec broadcast_event(binary(), binary(), block_event()) -> ok.
broadcast_event(LUser, LServer, Event) ->
    From = jid:make(LUser, LServer),
    lists:foreach(
      fun(R) ->
	      To = jid:replace_resource(From, R),
	      IQ = #iq{type = set, from = From, to = To,
		       id = <<"push", (randoms:get_string())/binary>>,
		       sub_els = [Event]},
	      ejabberd_router:route(IQ)
      end, ejabberd_sm:get_user_resources(LUser, LServer)).

-spec process_get(iq()) -> iq().
process_get(#iq{from = #jid{luser = LUser, lserver = LServer},
		lang = Lang} = IQ) ->
    Mod = db_mod(LServer),
    case Mod:process_blocklist_get(LUser, LServer) of
      error ->
	    Err = xmpp:err_internal_server_error(<<"Database failure">>, Lang),
	    xmpp:make_error(IQ, Err);
      List ->
	    LJIDs = listitems_to_jids(List, []),
	  Items = [jid:make(J) || J <- LJIDs],
	    xmpp:make_iq_result(IQ, #block_list{items = Items})
    end.

-spec db_mod(binary()) -> module().
db_mod(LServer) ->
    DBType = gen_mod:db_type(LServer, mod_privacy),
    gen_mod:db_mod(DBType, ?MODULE).

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [iqdisc].
