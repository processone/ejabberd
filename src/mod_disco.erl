%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Service Discovery (XEP-0030) support
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
	 get_info/5,
	 register_feature/2,
	 unregister_feature/2,
	 register_extra_domain/2,
	 unregister_extra_domain/2,
	 get_vh_services/1]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("mod_roster.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->

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
    register_feature(HostB, "iq"),
    register_feature(HostB, "presence"),

    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ExtraDomains = gen_mod:get_opt(extra_domains, Opts, []),
    lists:foreach(fun(Domain) -> register_extra_domain(HostB, Domain) end,
		  ExtraDomains),
    catch ets:new(disco_sm_features, [named_table, ordered_set, public]),
    catch ets:new(disco_sm_nodes, [named_table, ordered_set, public]),
    ejabberd_hooks:add(disco_local_items, HostB, ?MODULE, get_local_services, 100),
    ejabberd_hooks:add(disco_local_features, HostB, ?MODULE, get_local_features, 100),
    ejabberd_hooks:add(disco_local_identity, HostB, ?MODULE, get_local_identity, 100),
    ejabberd_hooks:add(disco_sm_items, HostB, ?MODULE, get_sm_items, 100),
    ejabberd_hooks:add(disco_sm_features, HostB, ?MODULE, get_sm_features, 100),
    ejabberd_hooks:add(disco_sm_identity, HostB, ?MODULE, get_sm_identity, 100),
    ejabberd_hooks:add(disco_info, HostB, ?MODULE, get_info, 100),
    ok.

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(disco_sm_identity, HostB, ?MODULE, get_sm_identity, 100),
    ejabberd_hooks:delete(disco_sm_features, HostB, ?MODULE, get_sm_features, 100),
    ejabberd_hooks:delete(disco_sm_items, HostB, ?MODULE, get_sm_items, 100),
    ejabberd_hooks:delete(disco_local_identity, HostB, ?MODULE, get_local_identity, 100),
    ejabberd_hooks:delete(disco_local_features, HostB, ?MODULE, get_local_features, 100),
    ejabberd_hooks:delete(disco_local_items, HostB, ?MODULE, get_local_services, 100),
    ejabberd_hooks:delete(disco_info, HostB, ?MODULE, get_info, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_DISCO_INFO),
    catch ets:match_delete(disco_features, {{'_', HostB}}),
    catch ets:match_delete(disco_extra_domains, {{'_', HostB}}),
    ok.


register_feature(HostB, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:insert(disco_features, {{Feature, HostB}}).

unregister_feature(HostB, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:delete(disco_features, {Feature, HostB}).

register_extra_domain(HostB, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:insert(disco_extra_domains, {{Domain, HostB}}).

unregister_extra_domain(HostB, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:delete(disco_extra_domains, {Domain, HostB}).

process_local_iq_items(From, To, #iq{type = get, payload = SubEl,
  lang = Lang} = IQ_Rec) ->
    Node = exmpp_xml:get_attribute_as_binary(SubEl, <<"node">>, <<>>),

    Host = exmpp_jid:prep_domain(To),
    case find_items(disco_local_items, Host, From, To, Node, Lang) of
	{result, Items} ->
	    ANode = case Node of
			<<>> -> [];
			_ -> [?XMLATTR(<<"node">>, Node)]
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
    Node = exmpp_xml:get_attribute_as_binary(SubEl, <<"node">>, <<>>),
    HostB = exmpp_jid:prep_domain(To),
    Identity = ejabberd_hooks:run_fold(disco_local_identity,
				       HostB,
				       [],
				       [From, To, Node, Lang]),
    Host = exmpp_jid:prep_domain_as_list(To),
    Info = ejabberd_hooks:run_fold(disco_info, HostB, [],
				   [Host, ?MODULE, Node, Lang]),
    case find_items(disco_local_features, HostB, From, To, Node, Lang) of
	{result, Features} ->
	    ANode = case Node of
			<<>> -> [];
			_ -> [?XMLATTR(<<"node">>, Node)]
		    end,
	    Result = #xmlel{ns = ?NS_DISCO_INFO, name = 'query',
			    attrs = ANode,
			    children = Identity ++ Info ++ features_to_xml(Features)},
	    exmpp_iq:result(IQ_Rec, Result);
	{error, Error} ->
	    exmpp_iq:error(IQ_Rec, Error)
    end;
process_local_iq_info(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

get_local_identity(Acc, _From, _To, <<>>, _Lang) ->
    Acc ++ [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs = [
	  ?XMLATTR(<<"category">>, <<"server">>),
	  ?XMLATTR(<<"type">>, <<"im">>),
	  ?XMLATTR(<<"name">>, <<"ejabberd">>)
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
    HostB = exmpp_jid:prep_domain(To),
    NHostB = ejabberd:normalize_host(HostB),
    {result,
     ets:select(disco_features, [{{{'_', NHostB}}, [],
				  ['$_']}]) ++
     ets:select(disco_features, [{{{'_', global}}, [],
				  ['$_']}]) ++
     Feats};

get_local_features(Acc, _From, _To, _Node, _Lang) ->
    case Acc of
	{result, _Features} ->
	    Acc;
	empty ->
	    {error, 'item-not-found'}
    end.

features_to_xml(FeatureList) ->
    %% Avoid duplicating features
    [#xmlel{ns = ?NS_DISCO_INFO, name = 'feature',
	    attrs = [?XMLATTR(<<"var">>, Feat)]} ||
	Feat <- lists:usort(
		  lists:map(
		    fun({{Feature, _Host}}) ->
			    feature_to_xml(Feature);
		       (Feature) ->
			    feature_to_xml(Feature)
		    end, FeatureList))].

feature_to_xml(Feature) when is_list(Feature) ->
    feature_to_xml(list_to_binary(Feature));
feature_to_xml(Feature) when is_atom(Feature) ->
    feature_to_xml(atom_to_list(Feature));
feature_to_xml(Feature) when is_binary(Feature) ->
    Feature.

domain_to_xml({Domain}) ->
    domain_to_xml(Domain);
domain_to_xml(Domain) when is_binary(Domain)->
    #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs = [
	?XMLATTR(<<"jid">>, Domain)
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
    NHostB = list_to_binary(ejabberd:normalize_host(Host)),
    {result,
     lists:usort(
       lists:map(fun domain_to_xml/1,
		 get_vh_services(Host) ++
		 ets:select(disco_extra_domains,
			ets:fun2ms(fun ({{Service, H}})
				    when H =:= NHostB;
				         H =:= global ->
					     Service
				end)))
       ) ++ Items};

get_local_services({result, _} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_local_services(empty, _From, _To, _Node, _Lang) ->
    {error, 'item-not-found'}.

%% Note: only first-order subdomains are returned.
%% For example, if Host = "a",
%% and Routes = ["muc.a", "vjud.a", "private.muc.a", "muc.b"]
%% only returns ["muc.a", "vjud.a"]
get_vh_services(Host) ->
    Routes = ejabberd_router:dirty_get_all_routes(),
    HostTokenized = string:tokens(Host, "."),
    VhServices =
	lists:filter(fun(H) ->
			     case string:tokens(H, ".") of
				 [_ | HostTokenized] ->
				     true;
				 _ ->
				     false
			     end
		     end, Routes),
    GlobalServices = ejabberd_global_router:expand_routes(Host),
    VhServices ++ GlobalServices.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_sm_iq_items(From, To, #iq{type = get, payload = SubEl,
  lang = Lang} = IQ_Rec) ->
    Node = exmpp_xml:get_attribute_as_binary(SubEl, <<"node">>, <<>>),
    Host = exmpp_jid:prep_domain(To),
    case find_items(disco_sm_items, Host, From, To, Node, Lang) of
	{result, Items} ->
	    ANode = case Node of
			<<>> -> [];
			_ -> [?XMLATTR(<<"node">>, Node)]
		    end,
            AItems = case Node of
                        <<>> ->
                            case is_presence_subscribed(From, To) of
                                true -> Items;
                                false -> []
                            end;
                        _ -> 
                            []
                    end,
	    Result = #xmlel{ns = ?NS_DISCO_ITEMS, name = 'query',
	      attrs = ANode, children = AItems},
	    exmpp_iq:result(IQ_Rec, Result);
	{error, Error} ->
	    exmpp_iq:error(IQ_Rec, Error)
    end;
process_sm_iq_items(_From, _To, #iq{type = set, payload = _SubEl} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

is_presence_subscribed(From, To) ->
    User = exmpp_jid:prep_node(From),
    Server = exmpp_jid:prep_domain(From),
    LUser = exmpp_jid:prep_node(To),
    LServer = exmpp_jid:prep_domain(To),
    lists:any(fun(#roster{jid = {TUser, TServer, _}, subscription = S}) ->
                            if 
                                LUser == TUser, LServer == TServer, S/=none ->
                                    true;
                                true ->
                                    false
                            end
                    end,
                    ejabberd_hooks:run_fold(roster_get, Server, [], [{User, Server}]))
                orelse User == LUser andalso Server == LServer.


get_sm_items({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_sm_items(Acc, From, To, <<>>, _Lang) ->
    Items = case Acc of
		{result, Its} -> Its;
		empty -> []
	    end,
    Items1 = case is_presence_subscribed(From, To) of
                   true ->
                       get_user_resources(To);
                   _ -> 
                       []
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
    case is_presence_subscribed(From, To) of
        true ->
            Node = exmpp_xml:get_attribute_as_binary(SubEl, <<"node">>, <<>>),
            Identity = ejabberd_hooks:run_fold(disco_sm_identity,
                                               exmpp_jid:prep_domain(To),
                                               [],
                                               [From, To, Node, Lang]),
            Host = exmpp_jid:prep_domain(To),
            case find_items(disco_sm_features, Host, From, To, Node, Lang) of
                {result, Features} ->
                    ANode = case Node of
                                <<>> -> [];
                                _ -> [?XMLATTR(<<"node">>, Node)]
                            end,
                    Result = #xmlel{ns = ?NS_DISCO_INFO, name = 'query',
				    attrs = ANode,
				    children = Identity ++ features_to_xml(Features)},
                    exmpp_iq:result(IQ_Rec, Result);
                {error, Error} ->
                    exmpp_iq:error(IQ_Rec, Error)
            end;
        false ->
            exmpp_iq:error(IQ_Rec, 'service-unavailable')
    end;

process_sm_iq_info(_From, _To, #iq{type = set} = IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').

get_sm_identity(Acc, _From, To, _Node, _Lang) ->
    LUser = exmpp_jid:prep_node_as_list(To),
    LServer = exmpp_jid:prep_domain_as_list(To),
    Acc ++  case ejabberd_auth:is_user_exists(LUser, LServer) of
        true ->
            [{xmlelement, "identity", [{"category", "account"},
            {"type", "registered"}], []}];
        _ ->
            []
     end.

get_sm_features(empty, From, To, _Node, _Lang) ->
    ?DEBUG("DISCO: ~p~n", [empty]),
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
    ?DEBUG("DISCO: ~p~n", [Acc]),
    Acc.

get_user_resources(JID) ->
    Rs = ejabberd_sm:get_user_resources(exmpp_jid:prep_node(JID),
                                        exmpp_jid:prep_domain(JID)),
    lists:map(fun(R) ->
		      #xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs = [
			  ?XMLATTR(<<"jid">>,
			    exmpp_jid:to_binary(exmpp_jid:full(JID, R))),
			  ?XMLATTR(<<"name">>, exmpp_jid:prep_node(JID))
			]}
	      end, lists:sort(Rs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Support for: XEP-0157 Contact Addresses for XMPP Services
 
get_info(Acc, Host, Mod, Node, _Lang) when Node == <<>> ->
    Module = case Mod of
		 undefined -> ?MODULE;
		 _ -> Mod
	     end,
    Serverinfo_fields = get_fields_xml(Host, Module),
    CData1 = #xmlcdata{cdata = list_to_binary(?NS_SERVERINFO_s)},
    Value1 = #xmlel{name = 'value', children = [CData1]},
    Field1 = #xmlel{name = 'field',
		    attrs = [?XMLATTR(<<"type">>, <<"hidden">>),
			     ?XMLATTR(<<"var">>, <<"FORM_TYPE">>)],
		    children = [Value1]
		   },
    X = #xmlel{name = 'x',
	       ns = ?NS_DATA_FORMS,
	       attrs = [?XMLATTR(<<"type">>, <<"result">>)],
	       children = [Field1 | Serverinfo_fields]
	      },
    [X | Acc];

get_info(Acc, _, _, _Node, _) ->
    Acc.

get_fields_xml(Host, Module) ->
    Fields = gen_mod:get_module_opt(Host, ?MODULE, server_info, []),

    %% filter, and get only the ones allowed for this module
    Fields_good = lists:filter(
		    fun({Modules, _, _}) ->
			    case Modules of
				all -> true;
				Modules -> lists:member(Module, Modules)
			    end
		    end,
		    Fields),

    fields_to_xml(Fields_good).

fields_to_xml(Fields) ->
    [ field_to_xml(Field) || Field <- Fields].

field_to_xml({_, Var, Values}) ->
    Values_xml = values_to_xml(Values),
    #xmlel{name = 'field',
	   attrs = [?XMLATTR(<<"var">>, list_to_binary(Var))],
	   children = Values_xml
	  }.

values_to_xml(Values) ->
    lists:map(
      fun(Value) ->
	      CData= #xmlcdata{cdata = list_to_binary(Value)},
	      #xmlel{name = 'value', children = [CData]}
      end,
      Values
     ).

%%%%%% private functions 
find_items(Hook, global, From, To, Node, Lang) ->
    ejabberd_hooks:run_fold(Hook, global, empty, [From, To, Node, Lang]);
find_items(Hook, Host, From, To, Node, Lang) ->
    case ejabberd_hooks:run_fold(Hook, Host, empty, [From, To, Node, Lang]) of
	empty ->
	    find_items(Hook, global, From, To, Node, Lang);
	Items -> Items
    end.
