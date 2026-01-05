%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Service Discovery (XEP-0030) support
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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

-protocol({xep, 30, '2.5.0', '0.1.0', "complete", ""}).
-protocol({xep, 157, '1.1.1', '2.1.0', "complete", ""}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_local_iq_items/1,
	 process_local_iq_info/1, get_local_identity/5,
	 get_local_features/5, get_local_services/5,
	 process_sm_iq_items/1, process_sm_iq_info/1,
	 get_sm_identity/5, get_sm_features/5, get_sm_items/5,
	 get_info/5, mod_opt_type/1, mod_options/1, depends/2,
         mod_doc/0]).

-include("logger.hrl").
-include("translate.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("mod_roster.hrl").

-type features_acc() :: {error, stanza_error()} | {result, [binary()]} | empty.
-type items_acc() :: {error, stanza_error()} | {result, [disco_item()]} | empty.
-export_type([features_acc/0, items_acc/0]).

start(Host, Opts) ->
    catch ets:new(disco_extra_domains,
		  [named_table, ordered_set, public,
		   {heir, erlang:group_leader(), none}]),
    ExtraDomains = mod_disco_opt:extra_domains(Opts),
    lists:foreach(fun (Domain) ->
			  register_extra_domain(Host, Domain)
		  end,
		  ExtraDomains),
    {ok, [{iq_handler, ejabberd_local, ?NS_DISCO_ITEMS, process_local_iq_items},
          {iq_handler, ejabberd_local, ?NS_DISCO_INFO, process_local_iq_info},
          {iq_handler, ejabberd_sm, ?NS_DISCO_ITEMS, process_sm_iq_items},
          {iq_handler, ejabberd_sm, ?NS_DISCO_INFO, process_sm_iq_info},
          {hook, disco_local_items, get_local_services, 100},
          {hook, disco_local_features, get_local_features, 100},
          {hook, disco_local_identity, get_local_identity, 100},
          {hook, disco_sm_items, get_sm_items, 100},
          {hook, disco_sm_features, get_sm_features, 100},
          {hook, disco_sm_identity, get_sm_identity, 100},
          {hook, disco_info, get_info, 100}]}.

stop(Host) ->
    catch ets:match_delete(disco_extra_domains,
			   {{'_', Host}}),
    ok.

reload(Host, NewOpts, OldOpts) ->
    NewDomains = mod_disco_opt:extra_domains(NewOpts),
    OldDomains = mod_disco_opt:extra_domains(OldOpts),
    lists:foreach(
      fun(Domain) ->
	      register_extra_domain(Host, Domain)
      end, NewDomains -- OldDomains),
    lists:foreach(
      fun(Domain) ->
	      unregister_extra_domain(Host, Domain)
      end, OldDomains -- NewDomains).

-spec register_extra_domain(binary(), binary()) -> true.
register_extra_domain(Host, Domain) ->
    ets:insert(disco_extra_domains, {{Domain, Host}}).

-spec unregister_extra_domain(binary(), binary()) -> true.
unregister_extra_domain(Host, Domain) ->
    ets:delete_object(disco_extra_domains, {{Domain, Host}}).

-spec process_local_iq_items(iq()) -> iq().
process_local_iq_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
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
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
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
get_local_identity(Acc, _From, To, <<"">>, _Lang) ->
    Host = To#jid.lserver,
    Name = mod_disco_opt:name(Host),
    Acc ++ [#identity{category = <<"server">>,
		      type = <<"im">>,
		      name = Name}];
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
		 [?NS_FEATURE_IQ, ?NS_FEATURE_PRESENCE,
		  ?NS_DISCO_INFO, ?NS_DISCO_ITEMS, Feats,
		  ejabberd_local:get_features(To#jid.lserver)]))};
get_local_features(Acc, _From, _To, _Node, Lang) ->
    case Acc of
      {result, _Features} -> Acc;
      empty ->
	    Txt = ?T("No features available"),
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
    {error, xmpp:err_item_not_found(?T("No services available"), Lang)}.

-spec get_vh_services(binary()) -> [binary()].
get_vh_services(Host) ->
    Hosts = lists:sort(fun (H1, H2) ->
			       byte_size(H1) >= byte_size(H2)
		       end,
		       ejabberd_option:hosts()),
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
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_sm_iq_items(#iq{type = get, lang = Lang,
			from = From, to = To,
			sub_els = [#disco_items{node = Node}]} = IQ) ->
    case mod_roster:is_subscribed(From, To) of
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
	    Txt = ?T("Not subscribed"),
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
    Items1 = case mod_roster:is_subscribed(From, To) of
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
	    Txt = ?T("Query to another users is forbidden"),
	    {error, xmpp:err_not_allowed(Txt, Lang)}
    end.

-spec process_sm_iq_info(iq()) -> iq().
process_sm_iq_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_sm_iq_info(#iq{type = get, lang = Lang,
		       from = From, to = To,
		       sub_els = [#disco_info{node = Node}]} = IQ) ->
    case mod_roster:is_subscribed(From, To) of
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
	    Txt = ?T("Not subscribed"),
	    xmpp:make_error(IQ, xmpp:err_subscription_required(Txt, Lang))
    end.

-spec get_sm_identity([identity()], jid(), jid(),
		      binary(), binary()) -> [identity()].
get_sm_identity(Acc, _From,
		#jid{luser = LUser, lserver = LServer}, _Node, _Lang) ->
    Acc ++
      case ejabberd_auth:user_exists(LUser, LServer) of
	true ->
	    [#identity{category = <<"account">>, type = <<"registered">>}];
	_ -> []
      end.

-spec get_sm_features(features_acc(), jid(), jid(), binary(), binary()) ->
			     {error, stanza_error()} | {result, [binary()]}.
get_sm_features(empty, From, To, Node, Lang) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
	{LTo, LSTo} ->
	    case Node of
		<<"">> -> {result, [?NS_DISCO_INFO, ?NS_DISCO_ITEMS]};
		_ -> {error, xmpp:err_item_not_found()}
	    end;
	_ ->
	    Txt = ?T("Query to another users is forbidden"),
	    {error, xmpp:err_not_allowed(Txt, Lang)}
    end;
get_sm_features({result, Features}, _From, _To, <<"">>, _Lang) ->
    {result, [?NS_DISCO_INFO, ?NS_DISCO_ITEMS|Features]};
get_sm_features(Acc, _From, _To, _Node, _Lang) -> Acc.

-spec get_user_resources(binary(), binary()) -> [disco_item()].
get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    [#disco_item{jid = jid:make(User, Server, Resource), name = User}
     || Resource <- lists:sort(Rs)].

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
    Fields = mod_disco_opt:server_info(Host),
    Fields1 = lists:filter(fun ({Modules, _, _}) ->
				   case Modules of
				       all -> true;
				       Modules ->
					   lists:member(Module, Modules)
				   end
			   end,
			   Fields),
    [#xdata_field{var = Var,
		  type = 'list-multi',
		  values = Values} || {_, Var, Values} <- Fields1].

-spec depends(binary(), gen_mod:opts()) -> [].
depends(_Host, _Opts) ->
    [].

mod_opt_type(extra_domains) ->
    econf:list(econf:binary());
mod_opt_type(name) ->
    econf:binary();
mod_opt_type(server_info) ->
    econf:list(
      econf:and_then(
	econf:options(
	  #{name => econf:binary(),
	    urls => econf:list(econf:binary()),
	    modules =>
		econf:either(
		  all,
		  econf:list(econf:beam()))}),
	fun(Opts) ->
		Mods = proplists:get_value(modules, Opts, all),
		Name = proplists:get_value(name, Opts, <<>>),
		URLs = proplists:get_value(urls, Opts, []),
		{Mods, Name, URLs}
	end)).

-spec mod_options(binary()) -> [{server_info,
				 [{all | [module()], binary(), [binary()]}]} |
				{atom(), any()}].
mod_options(_Host) ->
    [{extra_domains, []},
     {server_info, []},
     {name, ?T("ejabberd")}].

mod_doc() ->
    #{desc =>
          ?T("This module adds support for "
             "https://xmpp.org/extensions/xep-0030.html"
             "[XEP-0030: Service Discovery]. With this module enabled, "
             "services on your server can be discovered by XMPP clients."),
      opts =>
          [{extra_domains,
            #{value => "[Domain, ...]",
              desc =>
                  ?T("With this option, you can specify a list of extra "
                     "domains that are added to the Service Discovery item list. "
                     "The default value is an empty list.")}},
           {name,
            #{value => ?T("Name"),
              desc =>
                  ?T("A name of the server in the Service Discovery. "
                     "This will only be displayed by special XMPP clients. "
                     "The default value is 'ejabberd'.")}},
           {server_info,
            #{value => "[Info, ...]",
              example =>
                  ["server_info:",
                   "  -",
                   "    modules: all",
                   "    name: abuse-addresses",
                   "    urls: [\"mailto:abuse@shakespeare.lit\"]",
                   "  -",
                   "    modules: [mod_muc]",
                   "    name: \"Web chatroom logs\"",
                   "    urls: [\"http://www.example.org/muc-logs\"]",
                   "  -",
                   "    modules: [mod_disco]",
                   "    name: feedback-addresses",
                   "    urls:",
                   "      - http://shakespeare.lit/feedback.php",
                   "      - mailto:feedback@shakespeare.lit",
                   "      - xmpp:feedback@shakespeare.lit",
                   "  -",
                   "    modules:",
                   "      - mod_disco",
                   "      - mod_vcard",
                   "    name: admin-addresses",
                   "    urls:",
                   "      - mailto:xmpp@shakespeare.lit",
                   "      - xmpp:admins@shakespeare.lit"],
              desc =>
                  ?T("Specify additional information about the server, "
                     "as described in https://xmpp.org/extensions/xep-0157.html"
                     "[XEP-0157: Contact Addresses for XMPP Services]. Every 'Info' "
                     "element in the list is constructed from the following options:")},
            [{modules,
              #{value => "all | [Module, ...]",
                desc =>
                    ?T("The value can be the keyword 'all', in which case the "
                       "information is reported in all the services, "
                       "or a list of ejabberd modules, in which case the "
                       "information is only specified for the services provided "
                       "by those modules.")}},
             {name,
              #{value => ?T("Name"),
                desc => ?T("The field 'var' name that will be defined. "
                           "See XEP-0157 for some standardized names.")}},
             {urls,
              #{value => "[URI, ...]",
                desc => ?T("A list of contact URIs, such as "
                           "HTTP URLs, XMPP URIs and so on.")}}]}]}.
