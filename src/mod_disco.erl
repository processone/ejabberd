%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Service Discovery (JEP-0030) support
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_disco).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_local_iq_items/3,
	 process_local_iq_info/3,
	 get_local_identity/5,
	 get_local_features/5,
	 get_local_services/5,
	 process_sm_iq_items/3,
	 process_sm_iq_info/3,
	 get_sm_identity/5,
	 get_sm_features/5,
	 get_sm_items/5,
	 get_publish_items/5,
	 register_feature/2,
	 unregister_feature/2,
	 register_extra_domain/2,
	 unregister_extra_domain/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(disco_publish, {owner_node, jid, name, node}).

start(Host, Opts) ->
    mnesia:create_table(disco_publish,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, disco_publish)},
			 {type, bag}]),
    mnesia:add_table_index(disco_publish, owner_node),

    ejabberd_local:refresh_iq_handlers(),

    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
				  ?MODULE, process_local_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
				  ?MODULE, process_local_iq_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_DISCO_ITEMS,
				  ?MODULE, process_sm_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_DISCO_INFO,
				  ?MODULE, process_sm_iq_info, IQDisc),

    catch ets:new(disco_features, [named_table, ordered_set, public]),
    register_feature(Host, "iq"),
    register_feature(Host, "presence"),
    register_feature(Host, "presence-invisible"),
    register_feature(Host, "http://jabber.org/protocol/disco#publish"),

    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ExtraDomains = gen_mod:get_opt(extra_domains, Opts, []),
    lists:foreach(fun(Domain) -> register_extra_domain(Host, Domain) end,
		  ExtraDomains),
    catch ets:new(disco_sm_features, [named_table, ordered_set, public]),
    catch ets:new(disco_sm_nodes, [named_table, ordered_set, public]),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_local_services, 100),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, get_local_features, 100),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, get_local_identity, 100),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE, get_sm_items, 100),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 100),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE, get_sm_identity, 100),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE, get_publish_items, 75),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE, get_publish_items, 75),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE, get_sm_identity, 100),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 100),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE, get_sm_items, 100),
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE, get_local_identity, 100),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_local_features, 100),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_local_services, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_INFO),
    catch ets:match_delete(disco_features, {{'_', Host}}),
    catch ets:match_delete(disco_extra_domains, {{'_', Host}}),
    ok.


register_feature(Host, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:insert(disco_features, {{Feature, Host}}).

unregister_feature(Host, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:delete(disco_features, {Feature, Host}).

register_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:insert(disco_extra_domains, {{Domain, Host}}).

unregister_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:delete(disco_extra_domains, {Domain, Host}).

process_local_iq_items(From, To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    Node = xml:get_tag_attr_s("node", SubEl),
	    Host = To#jid.lserver,

	    case ejabberd_hooks:run_fold(disco_local_items,
					 Host,
					 empty,
					 [From, To, Node, Lang]) of
		{result, Items} ->
		    ANode = case Node of
				"" -> [];
				_ -> [{"node", Node}]
		    end,
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", ?NS_DISCO_ITEMS} | ANode],
				     Items
				    }]};
		{error, Error} ->
		    IQ#iq{type = error, sub_el = [SubEl, Error]}
	    end
    end.


process_local_iq_info(From, To, #iq{type = Type, lang = Lang,
				     sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    Host = To#jid.lserver,
	    Node = xml:get_tag_attr_s("node", SubEl),
	    Identity = ejabberd_hooks:run_fold(disco_local_identity,
					       Host,
					       [],
					       [From, To, Node, Lang]),
	    case ejabberd_hooks:run_fold(disco_local_features,
					 Host,
					 empty,
					 [From, To, Node, Lang]) of
		{result, Features} ->
		    ANode = case Node of
				"" -> [];
				_ -> [{"node", Node}]
			    end,
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", ?NS_DISCO_INFO} | ANode],
				     Identity ++ 
				     lists:map(fun feature_to_xml/1, Features)
				    }]};
		{error, Error} ->
		    IQ#iq{type = error, sub_el = [SubEl, Error]}
	    end
    end.

get_local_identity(Acc, _From, _To, [], _Lang) ->
    Acc ++ [{xmlelement, "identity",
	     [{"category", "server"},
	      {"type", "im"},
	      {"name", "ejabberd"}], []}];

get_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_local_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_local_features(Acc, _From, To, [], _Lang) ->
    Feats = case Acc of
		{result, Features} -> Features;
		empty -> []
	    end,
    Host = To#jid.lserver,
    {result,
     ets:select(disco_features, [{{{'_', Host}}, [], ['$_']}]) ++ Feats};

get_local_features(Acc, _From, _To, _Node, _Lang) ->
    case Acc of
	{result, _Features} ->
	    Acc;
	empty ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.


feature_to_xml({{Feature, _Host}}) ->
    feature_to_xml(Feature);
feature_to_xml(Feature) when is_list(Feature) ->
    {xmlelement, "feature", [{"var", Feature}], []}.

domain_to_xml({Domain}) ->
    {xmlelement, "item", [{"jid", Domain}], []};
domain_to_xml(Domain) ->
    {xmlelement, "item", [{"jid", Domain}], []}.

get_local_services({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_local_services(Acc, _From, To, [], _Lang) ->
    Items = case Acc of
		{result, Its} -> Its;
		empty -> []
	    end,
    Host = To#jid.lserver,
    {result,
     lists:usort(
       lists:map(fun domain_to_xml/1,
		 get_vh_services(Host) ++
		 ets:select(disco_extra_domains,
			    [{{{'$1', Host}}, [], ['$1']}]))
       ) ++ Items};

get_local_services({result, _} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_local_services(empty, _From, _To, _Node, _Lang) ->
    {error, ?ERR_ITEM_NOT_FOUND}.

get_vh_services(Host) ->
    Hosts = lists:sort(fun(H1, H2) -> length(H1) >= length(H2) end, ?MYHOSTS),
    lists:filter(fun(H) ->
			 case lists:dropwhile(
				fun(VH) ->
					not lists:suffix("." ++ VH, H)
				end, Hosts) of
			     [] ->
				 false;
			     [VH | _] ->
				 VH == Host
			 end
		 end, ejabberd_router:dirty_get_all_routes()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_sm_iq_items(From, To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    #jid{luser = LTo, lserver = ToServer} = To,
	    #jid{luser = LFrom, lserver = LServer} = From,
	    Self = (LTo == LFrom) andalso (ToServer == LServer),
	    Node = xml:get_tag_attr_s("node", SubEl),
	    if
		Self, Node /= [] ->
		    %% Here, we treat disco publish attempts to your own JID.
		    {xmlelement, _, _, Items} = SubEl,
		    case process_disco_publish({LFrom, LServer}, Node, Items) of
			ok ->
			    IQ#iq{type = result, sub_el = []};
			{error, Err} ->
			    IQ#iq{type = error, sub_el = [SubEl, Err]}
		    end;

		true ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
	    end;
	get ->
	    Host = To#jid.lserver,
	    Node = xml:get_tag_attr_s("node", SubEl),
	    case ejabberd_hooks:run_fold(disco_sm_items,
					 Host,
					 empty,
					 [From, To, Node, Lang]) of
		{result, Items} ->
		    ANode = case Node of
				"" -> [];
				_ -> [{"node", Node}]
			    end,
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", ?NS_DISCO_ITEMS} | ANode],
				     Items
				    }]};
		{error, Error} ->
		    IQ#iq{type = error, sub_el = [SubEl, Error]}
	    end
    end.

get_sm_items({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_sm_items(Acc,
	    #jid{luser = LFrom, lserver = LSFrom},
	    #jid{user = User, server = Server, luser = LTo, lserver = LSTo} = _To,
	    [], _Lang) ->
    Items = case Acc of
		{result, Its} -> Its;
		empty -> []
	    end,
    Items1 = case {LFrom, LSFrom} of
		 {LTo, LSTo} -> get_user_resources(User, Server);
		 _ -> []
	     end,
    {result, Items ++ Items1};
 
get_sm_items({result, _} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_sm_items(empty, From, To, _Node, _Lang) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
	{LTo, LSTo} ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	_ ->
	    {error, ?ERR_NOT_ALLOWED}
    end.

process_sm_iq_info(From, To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    Host = To#jid.lserver,
	    Node = xml:get_tag_attr_s("node", SubEl),
	    Identity = ejabberd_hooks:run_fold(disco_sm_identity,
					       Host,
					       [],
					       [From, To, Node, Lang]),
	    case ejabberd_hooks:run_fold(disco_sm_features,
					 Host,
					 empty,
					 [From, To, Node, Lang]) of
		{result, Features} ->
		    ANode = case Node of
				"" -> [];
				_ -> [{"node", Node}]
			    end,
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", ?NS_DISCO_INFO} | ANode],
				     Identity ++
				     lists:map(fun feature_to_xml/1, Features)
				    }]};
		{error, Error} ->
		    IQ#iq{type = error, sub_el = [SubEl, Error]}
	    end
    end.

get_sm_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_sm_features(empty, From, To, _Node, _Lang) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
	{LTo, LSTo} ->
	    {error, ?ERR_ITEM_NOT_FOUND};
	_ ->
	    {error, ?ERR_NOT_ALLOWED}
    end;
 
get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.



get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    lists:map(fun(R) ->
		      {xmlelement, "item",
		       [{"jid", User ++ "@" ++ Server ++ "/" ++ R},
			{"name", User}], []}
	      end, lists:sort(Rs)).


get_publish_items(empty,
		  #jid{luser = LFrom, lserver = LSFrom},
		  #jid{luser = LTo, lserver = LSTo} = _To,
		  Node, _Lang) ->
    if
	(LFrom == LTo) and (LSFrom == LSTo) ->
	    retrieve_disco_publish({LTo, LSTo}, Node);
	true ->
	    empty
    end;
get_publish_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

process_disco_publish(User, Node, Items) ->
    F = fun() ->
		lists:foreach(
		  fun({xmlelement, _Name, _Attrs, _SubEls} = Item) ->
			  Action = xml:get_tag_attr_s("action", Item),
			  Jid = xml:get_tag_attr_s("jid", Item),
			  PNode = xml:get_tag_attr_s("node", Item),
			  Name = xml:get_tag_attr_s("name", Item),
			  ?INFO_MSG("Disco publish: ~p ~p ~p ~p ~p ~p~n",
				    [User, Action, Node, Jid, PNode, Name]),

			  %% The disco_publish table isn't strictly a "bag" table, as
			  %% entries with same jid and node combination are considered
			  %% the same, even if they have different names.  Therefore,
			  %% we find a list of items to supersede.
			  SupersededItems = mnesia:match_object(
					      #disco_publish{owner_node = {User, Node},
							     jid = Jid,
							     node = PNode,
							     _ = '_'}),
			  case Action of
			      "update" ->
				  lists:map(
				    fun(O) ->
					    mnesia:delete_object(O)
				    end, SupersededItems),
				  mnesia:write(
				    #disco_publish{owner_node = {User, Node},
						   jid = Jid,
						   name = Name,
						   node = PNode});
			      "remove" ->
				  case SupersededItems of
				      [] ->
					  mnesia:abort({error, ?ERR_ITEM_NOT_FOUND});
				      _ ->
					  lists:map(
					    fun(O) ->
						    mnesia:delete_object(O)
					    end, SupersededItems)
				  end;
			      _ ->
				  %% invalid "action" attribute - return an error
				  mnesia:abort({error, ?ERR_BAD_REQUEST})
			  end;
		     ({xmlcdata, _CDATA}) ->
			  ok
		  end, Items)
	end,
    case mnesia:transaction(F) of
	{aborted, {error, _} = Error} ->
	    Error;
	{atomic, _} ->
	    ok;
	_ ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

retrieve_disco_publish(User, Node) ->
    case catch mnesia:dirty_read({disco_publish, {User, Node}}) of
	{'EXIT', _Reason} ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	[] ->
	    empty;
	Items ->
	    {result,
	     lists:map(
	       fun(#disco_publish{jid = Jid,
				  name = Name,
				  node = PNode}) ->
		       {xmlelement, "item",
			lists:append([[{"jid", Jid}],
				      case Name of
					  "" ->
					      [];
					  _ ->
					      [{"name", Name}]
				      end,
				      case PNode of
					  "" ->
					      [];
					  _ ->
					      [{"node", PNode}]
				      end]), []}
	       end, Items)}
    end.

