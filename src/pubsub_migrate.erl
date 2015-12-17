%%%----------------------------------------------------------------------
%%% File    : pubsub_migrate.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Migration/Upgrade code put out of mod_pubsub
%%% Created : 26 Jul 2014 by Christophe Romain <christophe.romain@process-one.net>
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

-module(pubsub_migrate).

-include("pubsub.hrl").
-include("logger.hrl").

-export([update_node_database/2, update_state_database/2]).
-export([update_item_database/2, update_lastitem_database/2]).

update_item_database_binary() ->
    F = fun () ->
		case catch mnesia:read({pubsub_last_item, mnesia:first(pubsub_last_item)}) of
		    [First] when is_list(First#pubsub_last_item.itemid) ->
			?INFO_MSG("Binarization of pubsub items table...", []),
			lists:foreach(fun (Id) ->
					      [Node] = mnesia:read({pubsub_last_item, Id}),

					      ItemId = iolist_to_binary(Node#pubsub_last_item.itemid),

					      ok = mnesia:delete({pubsub_last_item, Id}),
					      ok = mnesia:write(Node#pubsub_last_item{itemid=ItemId})
				      end,
				      mnesia:all_keys(pubsub_last_item));
		    _-> no_need
		end
	end,
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    ?ERROR_MSG("Failed to binarize pubsub items table: ~p", [Reason]);
	{atomic, no_need} ->
	    ok;
	{atomic, Result} ->
	    ?INFO_MSG("Pubsub items table has been binarized: ~p", [Result])
    end.

update_item_database(_Host, _ServerHost) ->
    F = fun() ->
	    ?INFO_MSG("Migration of old pubsub items...", []),
	    lists:foreach(fun (Key) ->
			[Item] = mnesia:read({pubsub_item, Key}),
			Payload = [xmlelement_to_xmlel(El) || El <- Item#pubsub_item.payload],
			mnesia:write(Item#pubsub_item{payload=Payload})
		end,
		mnesia:all_keys(pubsub_item))
	end,
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    ?ERROR_MSG("Failed to migrate old pubsub items to xmlel: ~p", [Reason]);
	{atomic, Result} ->
	    ?INFO_MSG("Pubsub items has been migrated: ~p", [Result])
    end.

xmlelement_to_xmlel({xmlelement, A, B, C}) when is_list(C) ->
    {xmlel, A, B, [xmlelement_to_xmlel(El) || El <- C]};
xmlelement_to_xmlel(El) ->
    El.

update_node_database_binary() ->
    F = fun () ->
		case catch mnesia:read({pubsub_node, mnesia:first(pubsub_node)}) of
		    [First] when is_list(First#pubsub_node.type) ->
			?INFO_MSG("Binarization of pubsub nodes table...", []),
			lists:foreach(fun ({H, N}) ->
					      [Node] = mnesia:read({pubsub_node, {H, N}}),

					      Type = iolist_to_binary(Node#pubsub_node.type),
					      BN = case N of
						       Binary when is_binary(Binary) ->
							   N;
						       _ ->
							   {result, BN1} = mod_pubsub:node_call(H, Type, path_to_node, [N]),
							   BN1
						   end,
					      BP = case [case P of
							     Binary2 when is_binary(Binary2) -> P;
							     _ -> element(2, mod_pubsub:node_call(H, Type, path_to_node, [P]))
							 end
							 || P <- Node#pubsub_node.parents] of
						       [<<>>] -> [];
						       Parents -> Parents
						   end,

					      BH = case H of
						       {U, S, R} -> {iolist_to_binary(U), iolist_to_binary(S), iolist_to_binary(R)};
						       String -> iolist_to_binary(String)
						   end,

					      Owners = [{iolist_to_binary(U), iolist_to_binary(S), iolist_to_binary(R)} ||
							   {U, S, R} <- Node#pubsub_node.owners],

					      ok = mnesia:delete({pubsub_node, {H, N}}),
					      ok = mnesia:write(Node#pubsub_node{nodeid = {BH, BN},
									    parents = BP,
									    type = Type,
									    owners = Owners});
					  (_) -> ok
				      end,
				      mnesia:all_keys(pubsub_node));
		    _-> no_need
		end
	end,
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    ?ERROR_MSG("Failed to binarize pubsub node table: ~p", [Reason]);
	{atomic, no_need} ->
	    ok;
	{atomic, Result} ->
	    ?INFO_MSG("Pubsub nodes table has been binarized: ~p", [Result])
    end.

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
	  {atomic, ok} = mnesia:create_table(pubsub_node,
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
    update_node_database_binary().

rename_default_nodeplugin() ->
    lists:foreach(fun (Node) ->
			  mnesia:dirty_write(Node#pubsub_node{type =
								  <<"hometree">>})
		  end,
		  mnesia:dirty_match_object(#pubsub_node{type =
							     <<"default">>,
							 _ = '_'})).

update_state_database(_Host, _ServerHost) ->
    case catch mnesia:table_info(pubsub_state, attributes) of
	[stateid, nodeidx, items, affiliation, subscriptions] ->
	    ?INFO_MSG("Upgrading pubsub states table...", []),
	    F = fun ({pubsub_state, {{U,S,R}, NodeID}, _NodeIdx, Items, Aff, Sub}, Acc) ->
			JID = {iolist_to_binary(U), iolist_to_binary(S), iolist_to_binary(R)},
			Subs = case Sub of
				   none ->
				       [];
				   [] ->
				       [];
				   _ ->
				       {result, SubID} = pubsub_subscription:subscribe_node(JID, NodeID, []),
				       [{Sub, SubID}]
			       end,
			NewState = #pubsub_state{stateid       = {JID, NodeID},
						 items	 = Items,
						 affiliation   = Aff,
						 subscriptions = Subs},
			[NewState | Acc]
		end,
	    {atomic, NewRecs} = mnesia:transaction(fun mnesia:foldl/3,
						   [F, [], pubsub_state]),
	    {atomic, ok} = mnesia:delete_table(pubsub_state),
	    {atomic, ok} = mnesia:create_table(pubsub_state,
					       [{disc_copies, [node()]},
						{attributes, record_info(fields, pubsub_state)}]),
	    FNew = fun () ->
			   lists:foreach(fun mnesia:write/1, NewRecs)
		   end,
	    case mnesia:transaction(FNew) of
		{atomic, Result} ->
		    ?INFO_MSG("Pubsub states table upgraded: ~p",
			      [Result]);
		{aborted, Reason} ->
		    ?ERROR_MSG("Problem upgrading Pubsub states table:~n~p",
			       [Reason])
	    end;
	_ ->
	    ok
    end.

update_lastitem_database(_Host, _ServerHost) ->
    update_item_database_binary().
