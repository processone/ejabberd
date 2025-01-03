%%%----------------------------------------------------------------------
%%% File    : mod_muc_rtbl.erl
%%% Author  : Paweł Chmielowski <pawel@process-one.net>
%%% Purpose :
%%% Created : 17 kwi 2023 by Paweł Chmielowski <pawel@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-module(mod_muc_rtbl).
-author("pawel@process-one.net").

-behaviour(gen_mod).
-behaviour(gen_server).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").
-include("mod_muc_room.hrl").

%% API
-export([start/2, stop/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,
	 mod_options/1, mod_opt_type/1, mod_doc/0, depends/2]).
-export([pubsub_event_handler/1, muc_presence_filter/3, muc_process_iq/2]).

-record(muc_rtbl, {host_id, blank = blank}).
-record(rtbl_state, {host, subscribed = false, retry_timer}).

start(Host, _Opts) ->
    gen_server:start({local, gen_mod:get_module_proc(Host, ?MODULE)}, ?MODULE, [Host], []).

stop(Host) ->
    gen_server:stop(gen_mod:get_module_proc(Host, ?MODULE)).

init([Host]) ->
    ejabberd_mnesia:create(?MODULE, muc_rtbl,
			   [{ram_copies, [node()]},
			    {local_content, true},
			    {attributes, record_info(fields, muc_rtbl)},
			    {type, set}]),
    ejabberd_hooks:add(local_send_to_resource_hook, Host,
		       ?MODULE, pubsub_event_handler, 50),
    ejabberd_hooks:add(muc_filter_presence, Host,
		       ?MODULE, muc_presence_filter, 50),
    ejabberd_hooks:add(muc_process_iq, Host,
		       ?MODULE, muc_process_iq, 50),
    request_initial_items(Host),
    {ok, #rtbl_state{host = Host}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({iq_reply, IQReply, initial_items}, State) ->
    State2 = parse_initial_items(State, IQReply),
    {noreply, State2};
handle_info({iq_reply, IQReply, subscription}, State) ->
    State2 = parse_subscription(State, IQReply),
    {noreply, State2};
handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, #rtbl_state{host = Host, subscribed = Sub, retry_timer = Timer}) ->
    ejabberd_hooks:delete(local_send_to_resource_hook, Host,
			  ?MODULE, pubsub_event_handler, 50),
    ejabberd_hooks:delete(muc_filter_presence, Host,
			  ?MODULE, muc_presence_filter, 50),
    ejabberd_hooks:delete(muc_process_iq, Host,
			  ?MODULE, muc_process_iq, 50),
    case Sub of
	true ->
	    Jid = service_jid(Host),
	    IQ = #iq{type = set, from = Jid, to = jid:make(mod_muc_rtbl_opt:rtbl_server(Host)),
		     sub_els = [
			 #pubsub{unsubscribe =
				 #ps_unsubscribe{jid = Jid, node = mod_muc_rtbl_opt:rtbl_node(Host)}}]},
	    ejabberd_router:route_iq(IQ, fun(_) -> ok end);
	_ ->
	    ok
    end,
    misc:cancel_timer(Timer).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

request_initial_items(Host) ->
    IQ = #iq{type = get, from = service_jid(Host),
	     to = jid:make(mod_muc_rtbl_opt:rtbl_server(Host)),
	     sub_els = [
		 #pubsub{items = #ps_items{node = mod_muc_rtbl_opt:rtbl_node(Host)}}]},
    ejabberd_router:route_iq(IQ, initial_items, self()).

parse_initial_items(State, timeout) ->
    ?WARNING_MSG("Fetching initial list failed: fetch timeout. Retrying in 60 seconds", []),
    State#rtbl_state{retry_timer = erlang:send_after(60000, self(), fetch_list)};
parse_initial_items(State, #iq{type = error} = IQ) ->
    ?WARNING_MSG("Fetching initial list failed: ~p. Retrying in 60 seconds",
		 [xmpp:format_stanza_error(xmpp:get_error(IQ))]),
    State#rtbl_state{retry_timer = erlang:send_after(60000, self(), fetch_list)};
parse_initial_items(State, #iq{from = From, to = #jid{lserver = Host} = To, type = result} = IQ) ->
    case xmpp:get_subtag(IQ, #pubsub{}) of
	#pubsub{items = #ps_items{node = Node, items = Items}} ->
	    Added = lists:foldl(
		fun(#ps_item{id = ID}, Acc) ->
		    mnesia:dirty_write(#muc_rtbl{host_id = {Host, ID}}),
		    maps:put(ID, true, Acc)
		end, #{}, Items),
	    SubIQ = #iq{type = set, from = To, to = From,
			sub_els = [
			    #pubsub{subscribe = #ps_subscribe{jid = To, node = Node}}]},
	    ejabberd_router:route_iq(SubIQ, subscription, self()),
	    notify_rooms(Host, Added),
	    State#rtbl_state{retry_timer = undefined, subscribed = true};
	_ ->
	    ?WARNING_MSG("Fetching initial list failed: invalid result payload", []),
	    State#rtbl_state{retry_timer = undefined}
    end.

parse_subscription(State, timeout) ->
    ?WARNING_MSG("Subscription error: request timeout", []),
    State#rtbl_state{subscribed = false};
parse_subscription(State, #iq{type = error} = IQ) ->
    ?WARNING_MSG("Subscription error: ~p", [xmpp:format_stanza_error(xmpp:get_error(IQ))]),
    State#rtbl_state{subscribed = false};
parse_subscription(State, _) ->
    State.

pubsub_event_handler(#message{from = #jid{luser = <<>>, lserver = SServer},
			      to = #jid{luser = <<>>, lserver = Server,
					lresource = <<"rtbl-", _/binary>>}} = Msg) ->

    SServer2 = mod_muc_rtbl_opt:rtbl_server(Server),
    SNode = mod_muc_rtbl_opt:rtbl_node(Server),
    if SServer == SServer2 ->
	case xmpp:get_subtag(Msg, #ps_event{}) of
	    #ps_event{items = #ps_items{node = Node, retract = Retract}} when Node == SNode,
									      is_binary(Retract) ->
		mnesia:dirty_delete(muc_rtbl, {Server, Retract});
	    #ps_event{items = #ps_items{node = Node, items = Items}} when Node == SNode ->
		Added = lists:foldl(
		    fun(#ps_item{id = ID}, Acc) ->
			mnesia:dirty_write(#muc_rtbl{host_id = {Server, ID}}),
			maps:put(ID, true, Acc)
		    end, #{}, Items),
		case maps:size(Added) of
		    0 -> ok;
		    _ -> notify_rooms(Server, Added)
		end;
	    _ ->
		ok
	end;
	true ->
	    ok
    end,
    stop;
pubsub_event_handler(_) ->
    ok.

muc_presence_filter(#presence{from = #jid{lserver = Server} = From, lang = Lang} = Packet, _State, _Nick) ->
    Blocked =
    case mnesia:dirty_read(muc_rtbl, {Server, sha256(Server)}) of
	[] ->
	    JIDs = sha256(jid:encode(jid:tolower(jid:remove_resource(From)))),
	    case mnesia:dirty_read(muc_rtbl, {Server, JIDs}) of
		[] -> false;
		_ -> true
	    end;
	_ -> true
    end,
    case Blocked of
	false -> Packet;
	_ ->
	    ErrText = ?T("You have been banned from this room"),
	    Err = xmpp:err_forbidden(ErrText, Lang),
	    ejabberd_router:route_error(Packet, Err),
	    drop
    end.

muc_process_iq(#iq{type = set, sub_els = [{rtbl_update, Items}]}, #state{users = Users} = State0) ->
    {NewState, _} =
    maps:fold(
	fun(_, #user{role = moderator}, {State, HostHashes}) ->
	    {State, HostHashes};
	   ({_, S, _} = LJid, #user{jid = JID}, {State, HostHashes}) ->
	       {Ban, HH2} =
	       case maps:find(S, HostHashes) of
		   {ok, Sha} ->
		       {maps:is_key(Sha, Items), HostHashes};
		   _ ->
		       Sha = sha256(S),
		       {maps:is_key(Sha, Items), maps:put(S, Sha, HostHashes)}
	       end,
	       Ban2 =
	       case Ban of
		   false ->
		       Sha2 = sha256(jid:encode(jid:remove_resource(LJid))),
		       maps:is_key(Sha2, Items);
		   _ ->
		       true
	       end,
	       case Ban2 of
		   true ->
		       {_, _, State2} = mod_muc_room:handle_event({process_item_change,
								   {JID, role, none, <<"Banned by RTBL">>},
								   undefined},
								  normal_state, State),
		       {State2, HH2};
		   _ ->
		       {State, HH2}
	       end
	end, {State0, #{}}, Users),
    {stop, {ignore, NewState}};
muc_process_iq(IQ, _State) ->
    IQ.

sha256(Data) ->
    Bin = crypto:hash(sha256, Data),
    str:to_hexlist(Bin).

notify_rooms(Host, Items) ->
    IQ = #iq{type = set, to = jid:make(Host), sub_els = [{rtbl_update, Items}]},
    lists:foreach(
	fun(CHost) ->
	    lists:foreach(
		fun({_, _, Pid}) when node(Pid) == node() ->
		    mod_muc_room:route(Pid, IQ);
		   (_) ->
		       ok
		end, mod_muc:get_online_rooms(CHost))
	end, mod_muc_admin:find_hosts(Host)).


service_jid(Host) ->
    jid:make(<<>>, Host, <<"rtbl-", (ejabberd_cluster:node_id())/binary>>).

mod_opt_type(rtbl_server) ->
    econf:domain();
mod_opt_type(rtbl_node) ->
    econf:non_empty(econf:binary()).

mod_options(_Host) ->
    [{rtbl_server, <<"xmppbl.org">>},
     {rtbl_node, <<"muc_bans_sha256">>}].

mod_doc() ->
    #{desc =>
      [?T("This module implement Real-time blocklists for MUC rooms."), "",
       ?T("It works by observing remote pubsub node conforming with "
	  "specification described in https://xmppbl.org/.")],
      note => "added in 23.04",
      opts =>
      [{rtbl_server,
	#{value => ?T("Domain"),
	  desc =>
	  ?T("Domain of xmpp server that serves block list. "
	     "The default value is 'xmppbl.org'")}},
       {rtbl_node,
	#{value => "PubsubNodeName",
	  desc =>
	  ?T("Name of pubsub node that should be used to track blocked users. "
	     "The default value is 'muc_bans_sha256'.")}}]}.

depends(_, _) ->
    [{mod_muc, hard}, {mod_pubsub, soft}].
