%%%-------------------------------------------------------------------
%%% File    : mod_mix.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  2 Mar 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(mod_mix).

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start/2, stop/1, process_iq/1,
	 disco_items/5, disco_identity/5, disco_info/5,
	 disco_features/5, mod_opt_type/1, depends/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").
-include("xmpp.hrl").

-define(NODES, [?NS_MIX_NODES_MESSAGES,
		?NS_MIX_NODES_PRESENCE,
		?NS_MIX_NODES_PARTICIPANTS,
		?NS_MIX_NODES_SUBJECT,
		?NS_MIX_NODES_CONFIG]).

-record(state, {server_host :: binary(),
		host :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

-spec disco_features({error, stanza_error()} | {result, [binary()]} | empty,
		     jid(), jid(), binary(), binary()) -> {result, [binary()]}.
disco_features(_Acc, _From, _To, _Node, _Lang) ->
    {result, [?NS_MIX_0]}.

disco_items(_Acc, _From, To, _Node, _Lang) when To#jid.luser /= <<"">> ->
    BareTo = jid:remove_resource(To),
    {result, [#disco_item{jid = BareTo, node = Node} || Node <- ?NODES]};
disco_items(_Acc, _From, _To, _Node, _Lang) ->
    {result, []}.

disco_identity(Acc, _From, To, _Node, _Lang) when To#jid.luser == <<"">> ->
    Acc ++ [#identity{category = <<"conference">>,
		      name = <<"MIX service">>,
		      type = <<"text">>}];
disco_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc ++ [#identity{category = <<"conference">>,
		      type = <<"mix">>}].

-spec disco_info([xdata()], binary(), module(), binary(), binary()) -> [xdata()];
		([xdata()], jid(), jid(), binary(), binary()) -> [xdata()].
disco_info(_Acc, _From, To, _Node, _Lang) when is_atom(To) ->
    [#xdata{type = result,
	    fields = [#xdata_field{var = <<"FORM_TYPE">>,
				   type = hidden,
				   values = [?NS_MIX_SERVICEINFO_0]}]}];
disco_info(Acc, _From, _To, _Node, _Lang) ->
    Acc.

process_iq(#iq{type = set, from = From, to = To,
	       sub_els = [#mix_join{subscribe = SubNodes}]} = IQ) ->
    Nodes = [Node || Node <- SubNodes, lists:member(Node, ?NODES)],
    case subscribe_nodes(From, To, Nodes) of
	{result, _} ->
	    case publish_participant(From, To) of
		{result, _} ->
		    BareFrom = jid:remove_resource(From),
		    xmpp:make_iq_result(
		      IQ, #mix_join{jid = BareFrom, subscribe = Nodes});
		{error, Err} ->
		    xmpp:make_error(IQ, Err)
	    end;
	{error, Err} ->
	    xmpp:make_error(IQ, Err)
    end;
process_iq(#iq{type = set, from = From, to = To,
	       sub_els = [#mix_leave{}]} = IQ) ->
    case delete_participant(From, To) of
	{result, _} ->
	    case unsubscribe_nodes(From, To, ?NODES) of
		{result, _} ->
		    xmpp:make_iq_result(IQ);
		{error, Err} ->
		    xmpp:make_error(IQ, Err)
	    end;
	{error, Err} ->
	    xmpp:make_error(IQ, Err)
    end;
process_iq(#iq{lang = Lang} = IQ) ->
    Txt = <<"Unsupported MIX query">>,
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ServerHost, Opts]) ->
    process_flag(trap_exit, true),
    Host = gen_mod:get_opt_host(ServerHost, Opts, <<"mix.@HOST@">>),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    ConfigTab = gen_mod:get_module_proc(Host, config),
    ets:new(ConfigTab, [named_table]),
    ets:insert(ConfigTab, {plugins, [<<"mix">>]}),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, disco_items, 100),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, disco_features, 100),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, disco_identity, 100),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE, disco_items, 100),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, disco_features, 100),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE, disco_identity, 100),
    ejabberd_hooks:add(disco_info, Host, ?MODULE, disco_info, 100),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_DISCO_ITEMS, mod_disco,
				  process_local_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_DISCO_INFO, mod_disco,
				  process_local_iq_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_DISCO_ITEMS, mod_disco,
				  process_local_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_DISCO_INFO, mod_disco,
				  process_local_iq_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_PUBSUB, mod_pubsub, iq_sm, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_MIX_0, ?MODULE, process_iq, IQDisc),
    ejabberd_router:register_route(Host, ServerHost),
    {ok, #state{server_host = ServerHost, host = Host}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({route, Packet}, State) ->
    case catch do_route(State, Packet) of
	{'EXIT', _} = Err ->
	    try
		?ERROR_MSG("failed to route packet:~n~s~nReason: ~p",
			   [xmpp:pp(Packet), Err]),
		Error = xmpp:err_internal_server_error(),
		ejabberd_router:route_error(Packet, Error)
	    catch _:_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, disco_items, 100),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, disco_features, 100),
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE, disco_identity, 100),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE, disco_items, 100),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, disco_features, 100),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE, disco_identity, 100),
    ejabberd_hooks:delete(disco_info, Host, ?MODULE, disco_info, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUBSUB),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MIX_0),
    ejabberd_router:unregister_route(Host),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_route(_State, #iq{} = Packet) ->
    ejabberd_router:process_iq(Packet);
do_route(_State, #presence{from = From, to = To, type = unavailable})
  when To#jid.luser /= <<"">> ->
    delete_presence(From, To);
do_route(_State, _Packet) ->
    ok.

subscribe_nodes(From, To, Nodes) ->
    LTo = jid:tolower(jid:remove_resource(To)),
    LFrom = jid:tolower(jid:remove_resource(From)),
    lists:foldl(
      fun(_Node, {error, _} = Err) ->
	      Err;
	 (Node, {result, _}) ->
	      case mod_pubsub:subscribe_node(LTo, Node, From, From, []) of
		  {error, _} = Err ->
		      case is_item_not_found(Err) of
			  true ->
			      case mod_pubsub:create_node(
				     LTo, To#jid.lserver, Node, LFrom, <<"mix">>) of
				  {result, _} ->
				      mod_pubsub:subscribe_node(LTo, Node, From, From, []);
				  Error ->
				      Error
			      end;
			  false ->
			      Err
		      end;
		  {result, _} = Result ->
		      Result
	      end
      end, {result, []}, Nodes).

unsubscribe_nodes(From, To, Nodes) ->
    LTo = jid:tolower(jid:remove_resource(To)),
    BareFrom = jid:remove_resource(From),
    lists:foldl(
      fun(_Node, {error, _} = Err) ->
	      Err;
	 (Node, {result, _} = Result) ->
	      case mod_pubsub:unsubscribe_node(LTo, Node, From, BareFrom, <<"">>) of
		  {error, _} = Err ->
		      case is_not_subscribed(Err) of
			  true -> Result;
			  _ -> Err
		      end;
		  {result, _} = Res ->
		      Res
	      end
      end, {result, []}, Nodes).

publish_participant(From, To) ->
    BareFrom = jid:remove_resource(From),
    LFrom = jid:tolower(BareFrom),
    LTo = jid:tolower(jid:remove_resource(To)),
    Participant = #mix_participant{jid = BareFrom},
    ItemID = str:sha(jid:encode(LFrom)),
    mod_pubsub:publish_item(
      LTo, To#jid.lserver, ?NS_MIX_NODES_PARTICIPANTS,
      From, ItemID, [xmpp:encode(Participant)]).

delete_presence(From, To) ->
    LFrom = jid:tolower(From),
    LTo = jid:tolower(jid:remove_resource(To)),
    case mod_pubsub:get_items(LTo, ?NS_MIX_NODES_PRESENCE) of
	Items when is_list(Items) ->
	    lists:foreach(
	      fun({pubsub_item, {ItemID, _}, _, {_, LJID}, _})
		    when LJID == LFrom ->
		      delete_item(From, To, ?NS_MIX_NODES_PRESENCE, ItemID);
		 (_) ->
		      ok
	      end, Items);
	_ ->
	    ok
    end.

delete_participant(From, To) ->
    LFrom = jid:tolower(jid:remove_resource(From)),
    ItemID = str:sha(jid:encode(LFrom)),
    delete_presence(From, To),
    delete_item(From, To, ?NS_MIX_NODES_PARTICIPANTS, ItemID).

delete_item(From, To, Node, ItemID) ->
    LTo = jid:tolower(jid:remove_resource(To)),
    case mod_pubsub:delete_item(
	   LTo, Node, From, ItemID, true) of
	{result, _} = Res ->
	    Res;
	{error, _} = Err ->
	    case is_item_not_found(Err) of
		true -> {result, []};
		false -> Err
	    end
    end.

-spec is_item_not_found({error, stanza_error()}) -> boolean().
is_item_not_found({error, #stanza_error{reason = 'item-not-found'}}) -> true;
is_item_not_found({error, _}) -> false.

-spec is_not_subscribed({error, stanza_error()}) -> boolean().
is_not_subscribed({error, #stanza_error{sub_els = Els}}) ->
    %% TODO: make xmpp:get_els function working for any XMPP element
    %% with sub_els field
    xmpp:has_subtag(#message{sub_els = Els},
		    #ps_error{type = 'not-subscribed'}).

depends(_Host, _Opts) ->
    [{mod_pubsub, hard}].

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(_) -> [host, iqdisc].
