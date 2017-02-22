%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Service Discovery (XEP-0030) support
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_disco).

-author('alexey@process-one.net').

-protocol({xep, 30, '2.4'}).
-protocol({xep, 157, '1.0'}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_local_iq_items/1,
	 process_local_iq_info/1, get_local_identity/5,
	 get_local_features/5, get_local_services/5,
	 process_sm_iq_items/1, process_sm_iq_info/1,
	 get_sm_identity/5, get_sm_features/5, get_sm_items/5,
	 get_info/5, transform_module_options/1, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("mod_roster.hrl").

-type features_acc() :: {error, stanza_error()} | {result, [binary()]} | empty.
-type items_acc() :: {error, stanza_error()} | {result, [disco_item()]} | empty.

start(Host, Opts) ->
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
    catch ets:new(disco_extra_domains,
		  [named_table, ordered_set, public,
		   {heir, erlang:group_leader(), none}]),
    ExtraDomains = gen_mod:get_opt(extra_domains, Opts,
                                   fun(Hs) ->
                                           [iolist_to_binary(H) || H <- Hs]
                                   end, []),
    lists:foreach(fun (Domain) ->
			  register_extra_domain(Host, Domain)
		  end,
		  ExtraDomains),
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
    catch ets:match_delete(disco_extra_domains,
			   {{'_', Host}}),
    ok.

reload(Host, NewOpts, OldOpts) ->
    case gen_mod:is_equal_opt(extra_domains, NewOpts, OldOpts,
			      fun(Hs) ->
				      [iolist_to_binary(H) || H <- Hs]
			      end, []) of
	{false, NewDomains, OldDomains} ->
	    lists:foreach(
	      fun(Domain) ->
		      register_extra_domain(Host, Domain)
	      end, NewDomains -- OldDomains),
	    lists:foreach(
	      fun(Domain) ->
		      unregister_extra_domain(Host, Domain)
	      end, OldDomains -- NewDomains);
	true ->
	    ok
    end,
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
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
					  IQDisc);
	true ->
	    ok
    end.

-spec register_extra_domain(binary(), binary()) -> true.
register_extra_domain(Host, Domain) ->
    ets:insert(disco_extra_domains, {{Domain, Host}}).

-spec unregister_extra_domain(binary(), binary()) -> true.
unregister_extra_domain(Host, Domain) ->
    ets:delete_object(disco_extra_domains, {{Domain, Host}}).

-spec process_local_iq_items(iq()) -> iq().
process_local_iq_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_local_iq_items(#iq{type = get, lang = Lang,
			   from = From, to = To,
			   sub_els = [#disco_items{node = Node}]} = IQ) ->
    Host = To#jid.lserver,
    case ejabberd_hooks:run_fold(disco_local_items, Host,
				 empty, [From, To, Node, Lang]) of
	{result, Items} ->
	    xmpp:make_iq_result(IQ, #disco_items{node = Node, items = Items});
	{error, Error} ->
	    xmpp:make_error(IQ, Error)
    end.

-spec process_local_iq_info(iq()) -> iq().
process_local_iq_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_local_iq_info(#iq{type = get, lang = Lang,
			  from = From, to = To,
			  sub_els = [#disco_info{node = Node}]} = IQ) ->
    Host = To#jid.lserver,
    Identity = ejabberd_hooks:run_fold(disco_local_identity,
				       Host, [], [From, To, Node, Lang]),
    Info = ejabberd_hooks:run_fold(disco_info, Host, [],
				   [Host, ?MODULE, Node, Lang]),
    case ejabberd_hooks:run_fold(disco_local_features, Host,
				 empty, [From, To, Node, Lang]) of
	{result, Features} ->
	    xmpp:make_iq_result(IQ, #disco_info{node = Node,
						identities = Identity,
						xdata = Info,
						features = Features});
	{error, Error} ->
	    xmpp:make_error(IQ, Error)
    end.

-spec get_local_identity([identity()], jid(), jid(),
			 binary(), binary()) ->	[identity()].
get_local_identity(Acc, _From, _To, <<"">>, _Lang) ->
    Acc ++ [#identity{category = <<"server">>,
		      type = <<"im">>,
		      name = <<"ejabberd">>}];
get_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec get_local_features(features_acc(), jid(), jid(), binary(), binary()) ->
				{error, stanza_error()} | {result, [binary()]}.
get_local_features({error, _Error} = Acc, _From, _To,
		   _Node, _Lang) ->
    Acc;
get_local_features(Acc, _From, To, <<"">>, _Lang) ->
    Feats = case Acc of
		{result, Features} -> Features;
		empty -> []
	    end,
    {result, lists:usort(
	       lists:flatten(
		 [<<"iq">>, <<"presence">>,
		  ?NS_DISCO_INFO, ?NS_DISCO_ITEMS, Feats,
		  ejabberd_local:get_features(To#jid.lserver)]))};
get_local_features(Acc, _From, _To, _Node, Lang) ->
    case Acc of
      {result, _Features} -> Acc;
      empty ->
	    Txt = <<"No features available">>,
	    {error, xmpp:err_item_not_found(Txt, Lang)}
    end.

-spec get_local_services(items_acc(), jid(), jid(), binary(), binary()) ->
				{error, stanza_error()} | {result, [disco_item()]}.
get_local_services({error, _Error} = Acc, _From, _To,
		   _Node, _Lang) ->
    Acc;
get_local_services(Acc, _From, To, <<"">>, _Lang) ->
    Items = case Acc of
	      {result, Its} -> Its;
	      empty -> []
	    end,
    Host = To#jid.lserver,
    {result,
     lists:usort(
       lists:map(
	 fun(Domain) -> #disco_item{jid = jid:make(Domain)} end,
	 get_vh_services(Host) ++
	     ets:select(disco_extra_domains,
			ets:fun2ms(
			  fun({{D, H}}) when H == Host -> D end))))
     ++ Items};
get_local_services({result, _} = Acc, _From, _To, _Node,
		   _Lang) ->
    Acc;
get_local_services(empty, _From, _To, _Node, Lang) ->
    {error, xmpp:err_item_not_found(<<"No services available">>, Lang)}.

-spec get_vh_services(binary()) -> [binary()].
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
		 ejabberd_router:get_all_routes()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_sm_iq_items(iq()) -> iq().
process_sm_iq_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_sm_iq_items(#iq{type = get, lang = Lang,
			from = From, to = To,
			sub_els = [#disco_items{node = Node}]} = IQ) ->
    case is_presence_subscribed(From, To) of
	true ->
	    Host = To#jid.lserver,
	    case ejabberd_hooks:run_fold(disco_sm_items, Host,
					 empty, [From, To, Node, Lang]) of
		{result, Items} ->
		    xmpp:make_iq_result(
		      IQ, #disco_items{node = Node, items = Items});
		{error, Error} ->
		    xmpp:make_error(IQ, Error)
	    end;
	false ->
	    Txt = <<"Not subscribed">>,
	    xmpp:make_error(IQ, xmpp:err_subscription_required(Txt, Lang))
    end.

-spec get_sm_items(items_acc(), jid(), jid(), binary(), binary()) ->
			  {error, stanza_error()} | {result, [disco_item()]}.
get_sm_items({error, _Error} = Acc, _From, _To, _Node,
	     _Lang) ->
    Acc;
get_sm_items(Acc, From,
	     #jid{user = User, server = Server} = To, <<"">>, _Lang) ->
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
get_sm_items(empty, From, To, _Node, Lang) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
      {LTo, LSTo} -> {error, xmpp:err_item_not_found()};
      _ ->
	    Txt = <<"Query to another users is forbidden">>,
	    {error, xmpp:err_not_allowed(Txt, Lang)}
    end.

-spec is_presence_subscribed(jid(), jid()) -> boolean().
is_presence_subscribed(#jid{luser = User, lserver = Server},
		       #jid{luser = User, lserver = Server}) -> true;
is_presence_subscribed(#jid{luser = FromUser, lserver = FromServer},
		       #jid{luser = ToUser, lserver = ToServer}) ->
    lists:any(fun (#roster{jid = {SubUser, SubServer, _}, subscription = Sub})
		      when FromUser == SubUser, FromServer == SubServer,
			   Sub /= none ->
		      true;
		  (_RosterEntry) ->
		      false
	      end,
	      ejabberd_hooks:run_fold(roster_get, ToServer, [],
				      [{ToUser, ToServer}])).

-spec process_sm_iq_info(iq()) -> iq().
process_sm_iq_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_sm_iq_info(#iq{type = get, lang = Lang,
		       from = From, to = To,
		       sub_els = [#disco_info{node = Node}]} = IQ) ->
    case is_presence_subscribed(From, To) of
	true ->
	    Host = To#jid.lserver,
	    Identity = ejabberd_hooks:run_fold(disco_sm_identity,
					       Host, [],
					       [From, To, Node, Lang]),
	    Info = ejabberd_hooks:run_fold(disco_info, Host, [],
					   [From, To, Node, Lang]),
	    case ejabberd_hooks:run_fold(disco_sm_features, Host,
					 empty, [From, To, Node, Lang]) of
		{result, Features} ->
		    xmpp:make_iq_result(IQ, #disco_info{node = Node,
							identities = Identity,
							xdata = Info,
							features = Features});
		{error, Error} ->
		    xmpp:make_error(IQ, Error)
	    end;
	false ->
	    Txt = <<"Not subscribed">>,
	    xmpp:make_error(IQ, xmpp:err_subscription_required(Txt, Lang))
    end.

-spec get_sm_identity([identity()], jid(), jid(),
		      binary(), binary()) -> [identity()].
get_sm_identity(Acc, _From,
		#jid{luser = LUser, lserver = LServer}, _Node, _Lang) ->
    Acc ++
      case ejabberd_auth:is_user_exists(LUser, LServer) of
	true ->
	    [#identity{category = <<"account">>, type = <<"registered">>}];
	_ -> []
      end.

-spec get_sm_features(features_acc(), jid(), jid(), binary(), binary()) ->
			     {error, stanza_error()} | {result, [binary()]}.
get_sm_features(empty, From, To, _Node, Lang) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
      {LTo, LSTo} -> {error, xmpp:err_item_not_found()};
      _ ->
	    Txt = <<"Query to another users is forbidden">>,
	    {error, xmpp:err_not_allowed(Txt, Lang)}
    end;
get_sm_features(Acc, _From, _To, _Node, _Lang) -> Acc.

-spec get_user_resources(binary(), binary()) -> [disco_item()].
get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    [#disco_item{jid = jid:make(User, Server, Resource), name = User}
     || Resource <- lists:sort(Rs)].

-spec transform_module_options(gen_mod:opts()) -> gen_mod:opts().
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

-spec get_info([xdata()], binary(), module(), binary(), binary()) -> [xdata()];
	      ([xdata()], jid(), jid(), binary(), binary()) -> [xdata()].
get_info(_A, Host, Mod, Node, _Lang) when is_atom(Mod), Node == <<"">> ->
    Module = case Mod of
	       undefined -> ?MODULE;
	       _ -> Mod
	     end,
    [#xdata{type = result,
	    fields = [#xdata_field{type = hidden,
				   var = <<"FORM_TYPE">>,
				   values = [?NS_SERVERINFO]}
		      | get_fields(Host, Module)]}];
get_info(Acc, _, _, _Node, _) -> Acc.

-spec get_fields(binary(), module()) -> [xdata_field()].
get_fields(Host, Module) ->
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
    Fields1 = lists:filter(fun ({Modules, _, _}) ->
				   case Modules of
				       all -> true;
				       Modules ->
					   lists:member(Module, Modules)
				   end
			   end,
			   Fields),
    [#xdata_field{var = Var, values = Values} || {_, Var, Values} <- Fields1].

-spec depends(binary(), gen_mod:opts()) -> [].
depends(_Host, _Opts) ->
    [].

mod_opt_type(extra_domains) ->
    fun (Hs) -> [iolist_to_binary(H) || H <- Hs] end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(server_info) ->
    fun (L) ->
	    lists:map(fun (Opts) ->
			      Mods = proplists:get_value(modules, Opts, all),
			      Name = proplists:get_value(name, Opts, <<>>),
			      URLs = proplists:get_value(urls, Opts, []),
			      {Mods, Name, URLs}
		      end,
		      L)
    end;
mod_opt_type(_) -> [extra_domains, iqdisc, server_info].
