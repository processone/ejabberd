%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Service Discovery (JEP-0030) support
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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
	 register_feature/2,
	 unregister_feature/2,
	 register_extra_domain/2,
	 unregister_extra_domain/2]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

start(Host, Opts) ->
    HostB = list_to_binary(Host),

    ejabberd_local:refresh_iq_handlers(),

    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_DISCO_ITEMS,
				  ?MODULE, process_local_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_DISCO_INFO,
				  ?MODULE, process_local_iq_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_DISCO_ITEMS,
				  ?MODULE, process_sm_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_DISCO_INFO,
				  ?MODULE, process_sm_iq_info, IQDisc),

    catch ets:new(disco_features, [named_table, ordered_set, public]),
    register_feature(Host, "iq"),
    register_feature(Host, "presence"),
    register_feature(Host, "presence-invisible"),

    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ExtraDomains = gen_mod:get_opt(extra_domains, Opts, []),
    lists:foreach(fun(Domain) -> register_extra_domain(Host, Domain) end,
		  ExtraDomains),
    catch ets:new(disco_sm_features, [named_table, ordered_set, public]),
    catch ets:new(disco_sm_nodes, [named_table, ordered_set, public]),
    ejabberd_hooks:add(disco_local_items, HostB, ?MODULE, get_local_services, 100),
    ejabberd_hooks:add(disco_local_features, HostB, ?MODULE, get_local_features, 100),
    ejabberd_hooks:add(disco_local_identity, HostB, ?MODULE, get_local_identity, 100),
    ejabberd_hooks:add(disco_sm_items, HostB, ?MODULE, get_sm_items, 100),
    ejabberd_hooks:add(disco_sm_features, HostB, ?MODULE, get_sm_features, 100),
    ejabberd_hooks:add(disco_sm_identity, HostB, ?MODULE, get_sm_identity, 100),
    ok.

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(disco_sm_identity, HostB, ?MODULE, get_sm_identity, 100),
    ejabberd_hooks:delete(disco_sm_features, HostB, ?MODULE, get_sm_features, 100),
    ejabberd_hooks:delete(disco_sm_items, HostB, ?MODULE, get_sm_items, 100),
    ejabberd_hooks:delete(disco_local_identity, HostB, ?MODULE, get_local_identity, 100),
    ejabberd_hooks:delete(disco_local_features, HostB, ?MODULE, get_local_features, 100),
    ejabberd_hooks:delete(disco_local_items, HostB, ?MODULE, get_local_services, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_DISCO_INFO),
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

process_local_iq_items(From, To, #iq{type = get, payload = SubEl,
  lang = Lang} = IQ_Rec) ->
    Node = exmpp_xml:get_attribute_as_binary(SubEl, 'node', <<>>),

    case ejabberd_hooks:run_fold(disco_local_items,
				 exmpp_jid:prep_domain(To),
				 empty,
				 [From, To, Node, Lang]) of
	{result, Items} ->
	    ANode = case Node of
			<<>> -> [];
			_ -> [?XMLATTR('node', Node)]
	    end,
	    Result = #xmlel{ns = ?NS_DISCO_ITEMS, name = 'query',
	      attrs = ANode, children = Items},
	    exmpp_iq:result(IQ_Rec, Result);
	{error, Error} ->
	    exmpp_iq:error(IQ_Rec, Error)
    end;
process_local_iq_items(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').


process_local_iq_info(From, To, #iq{type = get, payload = SubEl,
  lang = Lang} = IQ_Rec) ->
    Node = exmpp_xml:get_attribute_as_binary(SubEl, 'node', <<>>),
    Identity = ejabberd_hooks:run_fold(disco_local_identity,
				       exmpp_jid:prep_domain(To),
				       [],
				       [From, To, Node, Lang]),
    case ejabberd_hooks:run_fold(disco_local_features,
				 exmpp_jid:prep_domain(To),
				 empty,
				 [From, To, Node, Lang]) of
	{result, Features} ->
	    ANode = case Node of
			<<>> -> [];
			_ -> [?XMLATTR('node', Node)]
		    end,
	    Result = #xmlel{ns = ?NS_DISCO_INFO, name = 'query',
	      attrs = ANode,
	      children = Identity ++ lists:map(fun feature_to_xml/1,
		Features)},
	    exmpp_iq:result(IQ_Rec, Result);
	{error, Error} ->
	    exmpp_iq:error(IQ_Rec, Error)
    end;
process_local_iq_info(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

get_local_identity(Acc, _From, _To, <<>>, _Lang) ->
    Acc ++ [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs = [
	  ?XMLATTR('category', <<"server">>),
	  ?XMLATTR('type', <<"im">>),
	  ?XMLATTR('name', <<"ejabberd">>)
	]}];

get_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_local_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_local_features(Acc, _From, To, <<>>, _Lang) ->
    Feats = case Acc of
		{result, Features} -> Features;
		empty -> []
	    end,
    Host = exmpp_jid:prep_domain_as_list(To),
    {result,
     ets:select(disco_features, [{{{'_', Host}}, [], ['$_']}]) ++ Feats};

get_local_features(Acc, _From, _To, _Node, _Lang) ->
    case Acc of
	{result, _Features} ->
	    Acc;
	empty ->
	    {error, 'item-not-found'}
    end.


feature_to_xml({{Feature, _Host}}) ->
    feature_to_xml(Feature);

feature_to_xml(Feature) when is_binary(Feature) ->
    #xmlel{ns = ?NS_DISCO_INFO, name = 'feature', attrs = [
	?XMLATTR('var', Feature)
      ]};

feature_to_xml(Feature) when is_list(Feature) ->
    feature_to_xml(list_to_binary(Feature));
feature_to_xml(Feature) when is_atom(Feature) ->
    feature_to_xml(atom_to_list(Feature)).

domain_to_xml({Domain}) ->
    domain_to_xml(Domain);
domain_to_xml(Domain) when is_binary(Domain)->
    #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs = [
	?XMLATTR('jid', Domain)
      ]};
domain_to_xml(Domain) when is_list(Domain) ->
    domain_to_xml(list_to_binary(Domain)).

get_local_services({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_local_services(Acc, _From, To, <<>>, _Lang) ->
    Items = case Acc of
		{result, Its} -> Its;
		empty -> []
	    end,
    Host = exmpp_jid:prep_domain_as_list(To),
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
    {error, 'item-not-found'}.

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

process_sm_iq_items(From, To, #iq{type = get, payload = SubEl,
  lang = Lang} = IQ_Rec) ->
    Node = exmpp_xml:get_attribute_as_binary(SubEl, 'node', <<>>),
    case ejabberd_hooks:run_fold(disco_sm_items,
				 exmpp_jid:prep_domain(To),
				 empty,
				 [From, To, Node, Lang]) of
	{result, Items} ->
	    ANode = case Node of
			<<>> -> [];
			_ -> [?XMLATTR('node', Node)]
		    end,
	    Result = #xmlel{ns = ?NS_DISCO_ITEMS, name = 'query',
	      attrs = ANode, children = Items},
	    exmpp_iq:result(IQ_Rec, Result);
	{error, Error} ->
	    exmpp_iq:error(IQ_Rec, Error)
    end;
process_sm_iq_items(_From, _To, #iq{type = set, payload = _SubEl} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

get_sm_items({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_sm_items(Acc, From, To, <<>>, _Lang) ->
    LFrom = exmpp_jid:prep_node_as_list(From),
    LSFrom = exmpp_jid:prep_domain_as_list(From),
    LTo = exmpp_jid:prep_node_as_list(To),
    LSTo = exmpp_jid:prep_domain_as_list(To),

    Items = case Acc of
		{result, Its} -> Its;
		empty -> []
	    end,
    Items1 = case {LFrom, LSFrom} of
		 {LTo, LSTo} -> [binary_to_list(R) || R <- get_user_resources(To)];
		 _ -> []
	     end,
    {result, Items ++ Items1};
 
get_sm_items({result, _} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_sm_items(empty, From, To, _Node, _Lang) ->
    LFrom = exmpp_jid:prep_node_as_list(From),
    LSFrom = exmpp_jid:prep_domain_as_list(From),
    LTo = exmpp_jid:prep_node_as_list(To),
    LSTo = exmpp_jid:prep_domain_as_list(To),
    case {LFrom, LSFrom} of
	{LTo, LSTo} ->
	    {error, 'item-not-found'};
	_ ->
	    {error, 'not-allowed'}
    end.

process_sm_iq_info(From, To, #iq{type = get, payload = SubEl,
  lang = Lang} = IQ_Rec) ->
    Node = exmpp_xml:get_attribute_as_binary(SubEl, 'node', <<>>),
    Identity = ejabberd_hooks:run_fold(disco_sm_identity,
				       exmpp_jid:prep_domain(To),
				       [],
				       [From, To, Node, Lang]),
    case ejabberd_hooks:run_fold(disco_sm_features,
				 exmpp_jid:prep_domain(To),
				 empty,
				 [From, To, Node, Lang]) of
	{result, Features} ->
	    ANode = case Node of
			<<>> -> [];
			_ -> [?XMLATTR('node', Node)]
		    end,
	    Result = #xmlel{ns = ?NS_DISCO_INFO, name = 'query',
	      attrs = ANode,
	      children = Identity ++ lists:map(fun feature_to_xml/1,
		Features)},
	    exmpp_iq:result(IQ_Rec, Result);
	{error, Error} ->
	    exmpp_iq:error(IQ_Rec, Error)
    end;
process_sm_iq_info(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

get_sm_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_sm_features(empty, From, To, _Node, _Lang) ->
    LFrom = exmpp_jid:prep_node_as_list(From),
    LSFrom = exmpp_jid:prep_domain_as_list(From),
    LTo = exmpp_jid:prep_node_as_list(To),
    LSTo = exmpp_jid:prep_domain_as_list(To),
    case {LFrom, LSFrom} of
	{LTo, LSTo} ->
	    {error, 'item-not-found'};
	_ ->
	    {error, 'not-allowed'}
    end;
 
get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_user_resources(JID) ->
    Rs = ejabberd_sm:get_user_resources(exmpp_jid:prep_node(JID),
                                        exmpp_jid:prep_domain(JID)),
    lists:map(fun(R) ->
		      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs = [
			  ?XMLATTR('jid',
			    exmpp_jid:to_binary(exmpp_jid:full(JID, R))),
			  ?XMLATTR('name', exmpp_jid:prep_node(JID))
			]}
	      end, lists:sort(Rs)).
