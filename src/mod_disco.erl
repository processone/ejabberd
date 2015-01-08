%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Service Discovery (XEP-0030) support
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_disco).

-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process_local_iq_items/3,
	 process_local_iq_info/3, get_local_identity/5,
	 get_local_features/5, get_local_services/5,
	 process_sm_iq_items/3, process_sm_iq_info/3,
	 get_sm_identity/5, get_sm_features/5, get_sm_items/5,
	 get_info/5, register_feature/2, unregister_feature/2,
	 register_extra_domain/2, unregister_extra_domain/2,
         transform_module_options/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_roster.hrl").

start(Host, Opts) ->
    ejabberd_local:refresh_iq_handlers(),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_DISCO_ITEMS, ?MODULE,
				  process_local_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_DISCO_INFO, ?MODULE,
				  process_local_iq_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_DISCO_ITEMS, ?MODULE, process_sm_iq_items,
				  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_DISCO_INFO, ?MODULE, process_sm_iq_info,
				  IQDisc),
    catch ets:new(disco_features,
		  [named_table, ordered_set, public]),
    register_feature(Host, <<"iq">>),
    register_feature(Host, <<"presence">>),
    catch ets:new(disco_extra_domains,
		  [named_table, ordered_set, public]),
    ExtraDomains = gen_mod:get_opt(extra_domains, Opts,
                                   fun(Hs) ->
                                           [iolist_to_binary(H) || H <- Hs]
                                   end, []),
    lists:foreach(fun (Domain) ->
			  register_extra_domain(Host, Domain)
		  end,
		  ExtraDomains),
    catch ets:new(disco_sm_features,
		  [named_table, ordered_set, public]),
    catch ets:new(disco_sm_nodes,
		  [named_table, ordered_set, public]),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE,
		       get_local_services, 100),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
		       get_local_features, 100),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE,
		       get_local_identity, 100),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE,
		       get_sm_items, 100),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       get_sm_features, 100),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE,
		       get_sm_identity, 100),
    ejabberd_hooks:add(disco_info, Host, ?MODULE, get_info,
		       100),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE,
			  get_sm_identity, 100),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  get_sm_features, 100),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE,
			  get_sm_items, 100),
    ejabberd_hooks:delete(disco_local_identity, Host,
			  ?MODULE, get_local_identity, 100),
    ejabberd_hooks:delete(disco_local_features, Host,
			  ?MODULE, get_local_features, 100),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE,
			  get_local_services, 100),
    ejabberd_hooks:delete(disco_info, Host, ?MODULE,
			  get_info, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_DISCO_INFO),
    catch ets:match_delete(disco_features, {{'_', Host}}),
    catch ets:match_delete(disco_extra_domains,
			   {{'_', Host}}),
    ok.

register_feature(Host, Feature) ->
    catch ets:new(disco_features,
		  [named_table, ordered_set, public]),
    ets:insert(disco_features, {{Feature, Host}}).

unregister_feature(Host, Feature) ->
    catch ets:new(disco_features,
		  [named_table, ordered_set, public]),
    ets:delete(disco_features, {Feature, Host}).

register_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains,
		  [named_table, ordered_set, public]),
    ets:insert(disco_extra_domains, {{Domain, Host}}).

unregister_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains,
		  [named_table, ordered_set, public]),
    ets:delete(disco_extra_domains, {Domain, Host}).

process_local_iq_items(From, To,
		       #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  Node = xml:get_tag_attr_s(<<"node">>, SubEl),
	  Host = To#jid.lserver,
	  case ejabberd_hooks:run_fold(disco_local_items, Host,
				       empty, [From, To, Node, Lang])
	      of
	    {result, Items} ->
		ANode = case Node of
			  <<"">> -> [];
			  _ -> [{<<"node">>, Node}]
			end,
		IQ#iq{type = result,
		      sub_el =
			  [#xmlel{name = <<"query">>,
				  attrs =
				      [{<<"xmlns">>, ?NS_DISCO_ITEMS} | ANode],
				  children = Items}]};
	    {error, Error} ->
		IQ#iq{type = error, sub_el = [SubEl, Error]}
	  end
    end.

process_local_iq_info(From, To,
		      #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  Host = To#jid.lserver,
	  Node = xml:get_tag_attr_s(<<"node">>, SubEl),
	  Identity = ejabberd_hooks:run_fold(disco_local_identity,
					     Host, [], [From, To, Node, Lang]),
	  Info = ejabberd_hooks:run_fold(disco_info, Host, [],
					 [Host, ?MODULE, Node, Lang]),
	  case ejabberd_hooks:run_fold(disco_local_features, Host,
				       empty, [From, To, Node, Lang])
	      of
	    {result, Features} ->
		ANode = case Node of
			  <<"">> -> [];
			  _ -> [{<<"node">>, Node}]
			end,
		IQ#iq{type = result,
		      sub_el =
			  [#xmlel{name = <<"query">>,
				  attrs =
				      [{<<"xmlns">>, ?NS_DISCO_INFO} | ANode],
				  children =
				      Identity ++
					Info ++ features_to_xml(Features)}]};
	    {error, Error} ->
		IQ#iq{type = error, sub_el = [SubEl, Error]}
	  end
    end.

get_local_identity(Acc, _From, _To, <<>>, _Lang) ->
    Acc ++
      [#xmlel{name = <<"identity">>,
	      attrs =
		  [{<<"category">>, <<"server">>}, {<<"type">>, <<"im">>},
		   {<<"name">>, <<"ejabberd">>}],
	      children = []}];
get_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_local_features({error, _Error} = Acc, _From, _To,
		   _Node, _Lang) ->
    Acc;
get_local_features(Acc, _From, To, <<>>, _Lang) ->
    Feats = case Acc of
	      {result, Features} -> Features;
	      empty -> []
	    end,
    Host = To#jid.lserver,
    {result,
     ets:select(disco_features,
		[{{{'_', Host}}, [], ['$_']}])
       ++ Feats};
get_local_features(Acc, _From, _To, _Node, _Lang) ->
    case Acc of
      {result, _Features} -> Acc;
      empty -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

features_to_xml(FeatureList) ->
    [#xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, Feat}], children = []}
     || Feat
	    <- lists:usort(lists:map(fun ({{Feature, _Host}}) ->
					     Feature;
					 (Feature) when is_binary(Feature) ->
					     Feature
				     end,
				     FeatureList))].

domain_to_xml({Domain}) ->
    #xmlel{name = <<"item">>, attrs = [{<<"jid">>, Domain}],
	   children = []};
domain_to_xml(Domain) ->
    #xmlel{name = <<"item">>, attrs = [{<<"jid">>, Domain}],
	   children = []}.

get_local_services({error, _Error} = Acc, _From, _To,
		   _Node, _Lang) ->
    Acc;
get_local_services(Acc, _From, To, <<>>, _Lang) ->
    Items = case Acc of
	      {result, Its} -> Its;
	      empty -> []
	    end,
    Host = To#jid.lserver,
    {result,
     lists:usort(lists:map(fun domain_to_xml/1,
			   get_vh_services(Host) ++
			     ets:select(disco_extra_domains,
					[{{{'$1', Host}}, [], ['$1']}])))
       ++ Items};
get_local_services({result, _} = Acc, _From, _To, _Node,
		   _Lang) ->
    Acc;
get_local_services(empty, _From, _To, _Node, _Lang) ->
    {error, ?ERR_ITEM_NOT_FOUND}.

get_vh_services(Host) ->
    Hosts = lists:sort(fun (H1, H2) ->
			       byte_size(H1) >= byte_size(H2)
		       end,
		       ?MYHOSTS),
    lists:filter(fun (H) ->
			 case lists:dropwhile(fun (VH) ->
						      not
							str:suffix(
                                                          <<".", VH/binary>>,
                                                          H)
					      end,
					      Hosts)
			     of
			   [] -> false;
			   [VH | _] -> VH == Host
			 end
		 end,
		 ejabberd_router:dirty_get_all_routes()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_sm_iq_items(From, To,
		    #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  case is_presence_subscribed(From, To) of
	    true ->
		Host = To#jid.lserver,
		Node = xml:get_tag_attr_s(<<"node">>, SubEl),
		case ejabberd_hooks:run_fold(disco_sm_items, Host,
					     empty, [From, To, Node, Lang])
		    of
		  {result, Items} ->
		      ANode = case Node of
				<<"">> -> [];
				_ -> [{<<"node">>, Node}]
			      end,
		      IQ#iq{type = result,
			    sub_el =
				[#xmlel{name = <<"query">>,
					attrs =
					    [{<<"xmlns">>, ?NS_DISCO_ITEMS}
					     | ANode],
					children = Items}]};
		  {error, Error} ->
		      IQ#iq{type = error, sub_el = [SubEl, Error]}
		end;
	    false ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]}
	  end
    end.

get_sm_items({error, _Error} = Acc, _From, _To, _Node,
	     _Lang) ->
    Acc;
get_sm_items(Acc, From,
	     #jid{user = User, server = Server} = To, <<>>, _Lang) ->
    Items = case Acc of
	      {result, Its} -> Its;
	      empty -> []
	    end,
    Items1 = case is_presence_subscribed(From, To) of
	       true -> get_user_resources(User, Server);
	       _ -> []
	     end,
    {result, Items ++ Items1};
get_sm_items({result, _} = Acc, _From, _To, _Node,
	     _Lang) ->
    Acc;
get_sm_items(empty, From, To, _Node, _Lang) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
      {LTo, LSTo} -> {error, ?ERR_ITEM_NOT_FOUND};
      _ -> {error, ?ERR_NOT_ALLOWED}
    end.

is_presence_subscribed(#jid{luser = User,
			    lserver = Server},
		       #jid{luser = LUser, lserver = LServer}) ->
    lists:any(fun (#roster{jid = {TUser, TServer, _},
			   subscription = S}) ->
		      if LUser == TUser, LServer == TServer, S /= none ->
			     true;
			 true -> false
		      end
	      end,
	      ejabberd_hooks:run_fold(roster_get, Server, [],
				      [{User, Server}]))
      orelse User == LUser andalso Server == LServer.

process_sm_iq_info(From, To,
		   #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  case is_presence_subscribed(From, To) of
	    true ->
		Host = To#jid.lserver,
		Node = xml:get_tag_attr_s(<<"node">>, SubEl),
		Identity = ejabberd_hooks:run_fold(disco_sm_identity,
						   Host, [],
						   [From, To, Node, Lang]),
		case ejabberd_hooks:run_fold(disco_sm_features, Host,
					     empty, [From, To, Node, Lang])
		    of
		  {result, Features} ->
		      ANode = case Node of
				<<"">> -> [];
				_ -> [{<<"node">>, Node}]
			      end,
		      IQ#iq{type = result,
			    sub_el =
				[#xmlel{name = <<"query">>,
					attrs =
					    [{<<"xmlns">>, ?NS_DISCO_INFO}
					     | ANode],
					children =
					    Identity ++
					      features_to_xml(Features)}]};
		  {error, Error} ->
		      IQ#iq{type = error, sub_el = [SubEl, Error]}
		end;
	    false ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]}
	  end
    end.

get_sm_identity(Acc, _From,
		#jid{luser = LUser, lserver = LServer}, _Node, _Lang) ->
    Acc ++
      case ejabberd_auth:is_user_exists(LUser, LServer) of
	true ->
	    [#xmlel{name = <<"identity">>,
		    attrs =
			[{<<"category">>, <<"account">>},
			 {<<"type">>, <<"registered">>}],
		    children = []}];
	_ -> []
      end.

get_sm_features(empty, From, To, _Node, _Lang) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
      {LTo, LSTo} -> {error, ?ERR_ITEM_NOT_FOUND};
      _ -> {error, ?ERR_NOT_ALLOWED}
    end;
get_sm_features(Acc, _From, _To, _Node, _Lang) -> Acc.

get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    lists:map(fun (R) ->
		      #xmlel{name = <<"item">>,
			     attrs =
				 [{<<"jid">>,
				   <<User/binary, "@", Server/binary, "/",
				     R/binary>>},
				  {<<"name">>, User}],
			     children = []}
	      end,
	      lists:sort(Rs)).

transform_module_options(Opts) ->
    lists:map(
      fun({server_info, Infos}) ->
              NewInfos = lists:map(
                           fun({Modules, Name, URLs}) ->
                                   [[{modules, Modules},
                                     {name, Name},
                                     {urls, URLs}]];
                              (Opt) ->
                                   Opt
                           end, Infos),
              {server_info, NewInfos};
         (Opt) ->
              Opt
      end, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Support for: XEP-0157 Contact Addresses for XMPP Services

get_info(_A, Host, Mod, Node, _Lang) when Node == <<>> ->
    Module = case Mod of
	       undefined -> ?MODULE;
	       _ -> Mod
	     end,
    Serverinfo_fields = get_fields_xml(Host, Module),
    [#xmlel{name = <<"x">>,
	    attrs =
		[{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
	    children =
		[#xmlel{name = <<"field">>,
			attrs =
			    [{<<"var">>, <<"FORM_TYPE">>},
			     {<<"type">>, <<"hidden">>}],
			children =
			    [#xmlel{name = <<"value">>, attrs = [],
				    children = [{xmlcdata, ?NS_SERVERINFO}]}]}]
		  ++ Serverinfo_fields}];
get_info(Acc, _, _, _Node, _) -> Acc.

get_fields_xml(Host, Module) ->
    Fields = gen_mod:get_module_opt(
               Host, ?MODULE, server_info,
               fun(L) ->
                       lists:map(
                         fun(Opts) ->
                                 Mods = proplists:get_value(modules, Opts, all),
                                 Name = proplists:get_value(name, Opts, <<>>),
                                 URLs = proplists:get_value(urls, Opts, []),
                                 {Mods, Name, URLs}
                         end, L)
               end, []),
    Fields_good = lists:filter(fun ({Modules, _, _}) ->
				       case Modules of
					 all -> true;
					 Modules ->
					     lists:member(Module, Modules)
				       end
			       end,
			       Fields),
    fields_to_xml(Fields_good).

fields_to_xml(Fields) ->
    [field_to_xml(Field) || Field <- Fields].

field_to_xml({_, Var, Values}) ->
    Values_xml = values_to_xml(Values),
    #xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}],
	   children = Values_xml}.

values_to_xml(Values) ->
    lists:map(fun (Value) ->
		      #xmlel{name = <<"value">>, attrs = [],
			     children = [{xmlcdata, Value}]}
	      end,
	      Values).
