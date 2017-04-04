%%%----------------------------------------------------------------------
%%% File    : pubsub_migrate.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Migration/Upgrade code put out of mod_pubsub
%%% Created : 26 Jul 2014 by Christophe Romain <christophe.romain@process-one.net>
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

-module(pubsub_migrate).

-include("pubsub.hrl").
-include("logger.hrl").

-export([update_node_database/2, update_state_database/2]).
-export([update_item_database/2, update_lastitem_database/2]).

update_item_database(_Host, _ServerHost) ->
    convert_list_items().

update_node_database(Host, ServerHost) ->
    mnesia:del_table_index(pubsub_node, type),
    mnesia:del_table_index(pubsub_node, parentid),
    case catch mnesia:table_info(pubsub_node, attributes) of
      [host_node, host_parent, info] ->
	    ?INFO_MSG("Upgrading pubsub nodes table...", []),
	  F = fun () ->
		      {Result, LastIdx} = lists:foldl(fun ({pubsub_node,
							    NodeId, ParentId,
							    {nodeinfo, Items,
							     Options,
							     Entities}},
							   {RecList,
							    NodeIdx}) ->
							      ItemsList =
								  lists:foldl(fun
										({item,
										  IID,
										  Publisher,
										  Payload},
										 Acc) ->
										    C =
											{unknown,
											 Publisher},
										    M =
											{p1_time_compat:timestamp(),
											 Publisher},
										    mnesia:write(#pubsub_item{itemid
														  =
														  {IID,
														   NodeIdx},
													      creation
														  =
														  C,
													      modification
														  =
														  M,
													      payload
														  =
														  Payload}),
										    [{Publisher,
										      IID}
										     | Acc]
									      end,
									      [],
									      Items),
							      Owners =
								  dict:fold(fun
									      (JID,
									       {entity,
										Aff,
										Sub},
									       Acc) ->
										  UsrItems =
										      lists:foldl(fun
												    ({P,
												      I},
												     IAcc) ->
													case
													  P
													    of
													  JID ->
													      [I
													       | IAcc];
													  _ ->
													      IAcc
													end
												  end,
												  [],
												  ItemsList),
										  mnesia:write({pubsub_state,
												{JID,
												 NodeIdx},
												UsrItems,
												Aff,
												Sub}),
										  case
										    Aff
										      of
										    owner ->
											[JID
											 | Acc];
										    _ ->
											Acc
										  end
									    end,
									    [],
									    Entities),
							      mnesia:delete({pubsub_node,
									     NodeId}),
							      {[#pubsub_node{nodeid
										 =
										 NodeId,
									     id
										 =
										 NodeIdx,
									     parents
										 =
										 [element(2,
											  ParentId)],
									     owners
										 =
										 Owners,
									     options
										 =
										 Options}
								| RecList],
							       NodeIdx + 1}
						      end,
						      {[], 1},
						      mnesia:match_object({pubsub_node,
									   {Host,
									    '_'},
									   '_',
									   '_'})),
		      mnesia:write(#pubsub_index{index = node, last = LastIdx,
						 free = []}),
		      Result
	      end,
	  {atomic, NewRecords} = mnesia:transaction(F),
	  {atomic, ok} = mnesia:delete_table(pubsub_node),
	  {atomic, ok} = ejabberd_mnesia:create(?MODULE, pubsub_node,
					     [{disc_copies, [node()]},
					      {attributes,
					       record_info(fields,
							   pubsub_node)}]),
	  FNew = fun () ->
			 lists:foreach(fun (Record) -> mnesia:write(Record) end,
				       NewRecords)
		 end,
	  case mnesia:transaction(FNew) of
	    {atomic, Result} ->
		    ?INFO_MSG("Pubsub nodes table upgraded: ~p",
			  [Result]);
	    {aborted, Reason} ->
		    ?ERROR_MSG("Problem upgrading Pubsub nodes table:~n~p",
			   [Reason])
	  end;
      [nodeid, parentid, type, owners, options] ->
	  F = fun ({pubsub_node, NodeId, {_, Parent}, Type,
		    Owners, Options}) ->
		      #pubsub_node{nodeid = NodeId, id = 0,
				   parents = [Parent], type = Type,
				   owners = Owners, options = Options}
	      end,
	  mnesia:transform_table(pubsub_node, F,
				 [nodeid, id, parents, type, owners, options]),
	  FNew = fun () ->
			 LastIdx = lists:foldl(fun (#pubsub_node{nodeid =
								     NodeId} =
							PubsubNode,
						    NodeIdx) ->
						       mnesia:write(PubsubNode#pubsub_node{id
											       =
											       NodeIdx}),
						       lists:foreach(fun
								       (#pubsub_state{stateid
											  =
											  StateId} =
									    State) ->
									   {JID,
									    _} =
									       StateId,
									   mnesia:delete({pubsub_state,
											  StateId}),
									   mnesia:write(State#pubsub_state{stateid
													       =
													       {JID,
														NodeIdx}})
								     end,
								     mnesia:match_object(#pubsub_state{stateid
													   =
													   {'_',
													    NodeId},
												       _
													   =
													   '_'})),
						       lists:foreach(fun
								       (#pubsub_item{itemid
											 =
											 ItemId} =
									    Item) ->
									   {IID,
									    _} =
									       ItemId,
									   {M1,
									    M2} =
									       Item#pubsub_item.modification,
									   {C1,
									    C2} =
									       Item#pubsub_item.creation,
									   mnesia:delete({pubsub_item,
											  ItemId}),
									   mnesia:write(Item#pubsub_item{itemid
													     =
													     {IID,
													      NodeIdx},
													 modification
													     =
													     {M2,
													      M1},
													 creation
													     =
													     {C2,
													      C1}})
								     end,
								     mnesia:match_object(#pubsub_item{itemid
													  =
													  {'_',
													   NodeId},
												      _
													  =
													  '_'})),
						       NodeIdx + 1
					       end,
					       1,
					       mnesia:match_object({pubsub_node,
								    {Host, '_'},
								    '_', '_',
								    '_', '_',
								    '_'})
						 ++
						 mnesia:match_object({pubsub_node,
								      {{'_',
									ServerHost,
									'_'},
								       '_'},
								      '_', '_',
								      '_', '_',
								      '_'})),
			 mnesia:write(#pubsub_index{index = node,
						    last = LastIdx, free = []})
		 end,
	  case mnesia:transaction(FNew) of
	    {atomic, Result} ->
		rename_default_nodeplugin(),
		    ?INFO_MSG("Pubsub nodes table upgraded: ~p",
			  [Result]);
	    {aborted, Reason} ->
		    ?ERROR_MSG("Problem upgrading Pubsub nodes table:~n~p",
			   [Reason])
	  end;
      [nodeid, id, parent, type, owners, options] ->
	  F = fun ({pubsub_node, NodeId, Id, Parent, Type, Owners,
		    Options}) ->
		      #pubsub_node{nodeid = NodeId, id = Id,
				   parents = [Parent], type = Type,
				   owners = Owners, options = Options}
	      end,
	  mnesia:transform_table(pubsub_node, F,
				 [nodeid, id, parents, type, owners, options]),
	  rename_default_nodeplugin();
      _ -> ok
    end,
    convert_list_nodes().

rename_default_nodeplugin() ->
    lists:foreach(fun (Node) ->
			  mnesia:dirty_write(Node#pubsub_node{type =
								  <<"hometree">>})
		  end,
		  mnesia:dirty_match_object(#pubsub_node{type =
							     <<"default">>,
							 _ = '_'})).

update_state_database(_Host, _ServerHost) ->
% useless starting from ejabberd 17.04
%    case catch mnesia:table_info(pubsub_state, attributes) of
%	[stateid, nodeidx, items, affiliation, subscriptions] ->
%	    ?INFO_MSG("Upgrading pubsub states table...", []),
%	    F = fun ({pubsub_state, {{U,S,R}, NodeID}, _NodeIdx, Items, Aff, Sub}, Acc) ->
%			JID = {U,S,R},
%			Subs = case Sub of
%				   none ->
%				       [];
%				   [] ->
%				       [];
%				   _ ->
%				       SubID = pubsub_subscription:make_subid(),
%				       [{Sub, SubID}]
%			       end,
%			NewState = #pubsub_state{stateid       = {JID, NodeID},
%						 items	 = Items,
%						 affiliation   = Aff,
%						 subscriptions = Subs},
%			[NewState | Acc]
%		end,
%	    {atomic, NewRecs} = mnesia:transaction(fun mnesia:foldl/3,
%						   [F, [], pubsub_state]),
%	    {atomic, ok} = mnesia:delete_table(pubsub_state),
%	    {atomic, ok} = ejabberd_mnesia:create(?MODULE, pubsub_state,
%					       [{disc_copies, [node()]},
%						{attributes, record_info(fields, pubsub_state)}]),
%	    FNew = fun () ->
%			   lists:foreach(fun mnesia:write/1, NewRecs)
%		   end,
%	    case mnesia:transaction(FNew) of
%		{atomic, Result} ->
%		    ?INFO_MSG("Pubsub states table upgraded: ~p",
%			      [Result]);
%		{aborted, Reason} ->
%		    ?ERROR_MSG("Problem upgrading Pubsub states table:~n~p",
%			       [Reason])
%	    end;
%	_ ->
%	    ok
%    end,
    convert_list_subscriptions(),
    convert_list_states().

update_lastitem_database(_Host, _ServerHost) ->
    convert_list_lasts().

%% binarization from old 2.1.x

convert_list_items() ->
    convert_list_records(
	pubsub_item,
	record_info(fields, pubsub_item),
	fun(#pubsub_item{itemid = {I, _}}) -> I end,
	fun(#pubsub_item{itemid = {I, Nidx},
			 creation = {C, CKey},
			 modification = {M, MKey},
			 payload = Els} = R) ->
	    R#pubsub_item{itemid = {bin(I), Nidx},
			  creation = {C, binusr(CKey)},
			  modification = {M, binusr(MKey)},
			  payload = [fxml:to_xmlel(El) || El<-Els]}
	end).

convert_list_states() ->
    convert_list_records(
	pubsub_state,
	record_info(fields, pubsub_state),
	fun(#pubsub_state{stateid = {{U,_,_}, _}}) -> U end,
	fun(#pubsub_state{stateid = {U, Nidx},
			  items = Is,
			  affiliation = A,
			  subscriptions = Ss} = R) ->
	    R#pubsub_state{stateid = {binusr(U), Nidx},
			  items = [bin(I) || I<-Is],
			  affiliation = A,
			  subscriptions = [{S,bin(Sid)} || {S,Sid}<-Ss]}
	end).

convert_list_nodes() ->
    convert_list_records(
	pubsub_node,
	record_info(fields, pubsub_node),
	fun(#pubsub_node{nodeid = {{U,_,_}, _}}) -> U;
	   (#pubsub_node{nodeid = {H, _}}) -> H end,
	fun(#pubsub_node{nodeid = {H, N},
			 id = I,
			 parents = Ps,
			 type = T,
			 owners = Os,
			 options = Opts} = R) ->
	    R#pubsub_node{nodeid = {binhost(H), bin(N)},
			  id = I,
			  parents = [bin(P) || P<-Ps],
			  type = bin(T),
			  owners = [binusr(O) || O<-Os],
			  options = Opts}
	end).

convert_list_subscriptions() ->
    [convert_list_records(
	pubsub_subscription,
	record_info(fields, pubsub_subscription),
	fun(#pubsub_subscription{subid = I}) -> I end,
	fun(#pubsub_subscription{subid = I,
				 options = Opts} = R) ->
	    R#pubsub_subscription{subid = bin(I),
				  options = Opts}
	end) || lists:member(pubsub_subscription, mnesia:system_info(tables))].

convert_list_lasts() ->
    convert_list_records(
	pubsub_last_item,
	record_info(fields, pubsub_last_item),
	fun(#pubsub_last_item{itemid = I}) -> I end,
	fun(#pubsub_last_item{itemid = I,
			      nodeid = Nidx,
			      creation = {C, CKey},
			      payload = Payload} = R) ->
	    R#pubsub_last_item{itemid = bin(I),
			       nodeid = Nidx,
			       creation = {C, binusr(CKey)},
			       payload = fxml:to_xmlel(Payload)}
	end).

%% internal tools

convert_list_records(Tab, Fields, DetectFun, ConvertFun) ->
    case mnesia:table_info(Tab, attributes) of
	Fields ->
	    ejabberd_config:convert_table_to_binary(
	      Tab, Fields, set, DetectFun, ConvertFun);
	_ ->
	    ?INFO_MSG("Recreating ~p table", [Tab]),
	    mnesia:transform_table(Tab, ignore, Fields),
	    convert_list_records(Tab, Fields, DetectFun, ConvertFun)
    end.

binhost({U,S,R}) -> binusr({U,S,R});
binhost(L) -> bin(L).

binusr({U,S,R}) -> {bin(U), bin(S), bin(R)}.

bin(L) -> iolist_to_binary(L).

