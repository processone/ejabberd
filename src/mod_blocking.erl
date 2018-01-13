%%%----------------------------------------------------------------------
%%% File    : mod_blocking.erl
%%% Author  : Stephan Maka
%%% Purpose : XEP-0191: Simple Communications Blocking
%%% Created : 24 Aug 2008 by Stephan Maka <stephan@spaceboyz.net>
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

-module(mod_blocking).

-behaviour(gen_mod).

-protocol({xep, 191, '1.2'}).

-export([start/2, stop/1, reload/3, process_iq/1, mod_opt_type/1, depends/2,
	 disco_features/5]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

-include("mod_privacy.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, gen_iq_handler:iqdisc(Host)),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, disco_features, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_BLOCKING, ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, disco_features, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_BLOCKING).

reload(Host, NewOpts, OldOpts) ->
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts, gen_iq_handler:iqdisc(Host)) of
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
process_block(#iq{from = From} = IQ, LJIDs) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case mod_privacy:get_user_list(LUser, LServer, default) of
	{error, _} ->
	    err_db_failure(IQ);
	Res ->
	    {Name, List} = case Res of
			       error -> {<<"Blocked contacts">>, []};
			       {ok, NameList} -> NameList
			   end,
	    AlreadyBlocked = listitems_to_jids(List, []),
	    NewList = lists:foldr(
			fun(LJID, List1) ->
				case lists:member(LJID, AlreadyBlocked) of
				    true ->
					List1;
				    false ->
					[#listitem{type = jid,
						   value = LJID,
						   action = deny,
						   order = 0,
						   match_all = true}|List1]
				end
			end, List, LJIDs),
	    case mod_privacy:set_list(LUser, LServer, Name, NewList) of
		ok ->
		    case (if Res == error ->
				  mod_privacy:set_default_list(
				    LUser, LServer, Name);
			     true ->
				  ok
			  end) of
			ok ->
			    mod_privacy:push_list_update(From, Name),
			    Items = [jid:make(LJID) || LJID <- LJIDs],
			    broadcast_event(From, #block{items = Items}),
			    xmpp:make_iq_result(IQ);
			{error, notfound} ->
			    ?ERROR_MSG("Failed to set default list '~s': "
				       "the list should exist, but not found",
				       [Name]),
			    err_db_failure(IQ);
			{error, _} ->
			    err_db_failure(IQ)
		    end;
		{error, _} ->
		    err_db_failure(IQ)
	    end
    end.

-spec process_unblock_all(iq()) -> iq().
process_unblock_all(#iq{from = From} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case mod_privacy:get_user_list(LUser, LServer, default) of
	{ok, {Name, List}} ->
	    NewList = lists:filter(
			fun(#listitem{action = A}) ->
				A /= deny
			end, List),
	    case mod_privacy:set_list(LUser, LServer, Name, NewList) of
		ok ->
		    mod_privacy:push_list_update(From, Name),
		    broadcast_event(From, #unblock{}),
		    xmpp:make_iq_result(IQ);
		{error, _} ->
		    err_db_failure(IQ)
	    end;
	error ->
	    broadcast_event(From, #unblock{}),
	    xmpp:make_iq_result(IQ);
	{error, _} ->
	    err_db_failure(IQ)
    end.

-spec process_unblock(iq(), [ljid()]) -> iq().
process_unblock(#iq{from = From} = IQ, LJIDs) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case mod_privacy:get_user_list(LUser, LServer, default) of
	{ok, {Name, List}} ->
	    NewList = lists:filter(
			fun(#listitem{action = deny, type = jid,
				      value = LJID}) ->
				not lists:member(LJID, LJIDs);
			   (_) ->
				true
			end, List),
	    case mod_privacy:set_list(LUser, LServer, Name, NewList) of
		ok ->
		    mod_privacy:push_list_update(From, Name),
		    Items = [jid:make(LJID) || LJID <- LJIDs],
		    broadcast_event(From, #unblock{items = Items}),
		    xmpp:make_iq_result(IQ);
		{error, _} ->
		    err_db_failure(IQ)
	    end;
	error ->
	    Items = [jid:make(LJID) || LJID <- LJIDs],
	    broadcast_event(From, #unblock{items = Items}),
	    xmpp:make_iq_result(IQ);
	{error, _} ->
	    err_db_failure(IQ)
    end.

-spec broadcast_event(jid(), block() | unblock()) -> ok.
broadcast_event(#jid{luser = LUser, lserver = LServer} = From, Event) ->
    lists:foreach(
      fun(R) ->
	      To = jid:replace_resource(From, R),
	      IQ = #iq{type = set, from = From, to = To,
		       id = <<"push", (randoms:get_string())/binary>>,
		       sub_els = [Event]},
	      ejabberd_router:route(IQ)
      end, ejabberd_sm:get_user_resources(LUser, LServer)).

-spec process_get(iq()) -> iq().
process_get(#iq{from = #jid{luser = LUser, lserver = LServer}} = IQ) ->
    case mod_privacy:get_user_list(LUser, LServer, default) of
	{ok, {_, List}} ->
	    LJIDs = listitems_to_jids(List, []),
	    Items = [jid:make(J) || J <- LJIDs],
	    xmpp:make_iq_result(IQ, #block_list{items = Items});
	error ->
	    xmpp:make_iq_result(IQ, #block_list{});
	{error, _} ->
	    err_db_failure(IQ)
    end.

err_db_failure(#iq{lang = Lang} = IQ) ->
    Txt = <<"Database failure">>,
    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang)).

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [iqdisc].
