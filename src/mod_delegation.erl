%%%-------------------------------------------------------------------
%%% File    : mod_delegation.erl
%%% Author  : Anna Mukharram <amuhar3@gmail.com>
%%% Purpose : XEP-0355: Namespace Delegation
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
%%%-------------------------------------------------------------------
-module(mod_delegation).

-author('amuhar3@gmail.com').

-protocol({xep, 355, '0.4.1', '16.09', "complete", ""}).

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3, mod_opt_type/1, depends/2, mod_options/1]).
-export([mod_doc/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([component_connected/1, component_disconnected/2,
	 ejabberd_local/1, ejabberd_sm/1, decode_iq_subel/1,
	 disco_local_features/5, disco_sm_features/5,
	 disco_local_identity/5, disco_sm_identity/5]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-type route_type() :: ejabberd_sm | ejabberd_local.
-type delegations() :: #{{binary(), route_type()} => {binary(), disco_info()}}.
-record(state, {server_host = <<"">> :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

mod_opt_type(namespaces) ->
    econf:and_then(
      econf:map(
	econf:binary(),
	econf:options(
	  #{filtering => econf:list(econf:binary()),
	    access => econf:acl()})),
      fun(L) ->
	      lists:map(
		fun({NS, Opts}) ->
			Attrs = proplists:get_value(filtering, Opts, []),
			Access = proplists:get_value(access, Opts, none),
			{NS, Attrs, Access}
		end, L)
      end).

-spec mod_options(binary()) -> [{namespaces,
				 [{binary(), [binary()], acl:acl()}]} |
				{atom(), term()}].
mod_options(_Host) ->
    [{namespaces, []}].

mod_doc() ->
    #{desc =>
          [?T("This module is an implementation of "
             "https://xmpp.org/extensions/xep-0355.html"
             "[XEP-0355: Namespace Delegation]. "
             "Only admin mode has been implemented by now. "
             "Namespace delegation allows external services to "
             "handle IQ using specific namespace. This may be applied "
             "for external PEP service."), "",
	   ?T("WARNING: Security issue: Namespace delegation gives components "
	      "access to sensitive data, so permission should be granted "
	      "carefully, only if you trust the component."), "",
	   ?T("NOTE: This module is complementary to _`mod_privilege`_ but can "
	      "also be used separately.")],
      opts =>
          [{namespaces,
            #{value => "{Namespace: Options}",
              desc =>
                  ?T("If you want to delegate namespaces to a component, "
                     "specify them in this option, and associate them "
                     "to an access rule. The 'Options' are:")},
            [{filtering,
              #{value => ?T("Attributes"),
                desc =>
                    ?T("The list of attributes. Currently not used.")}},
             {access,
              #{value => ?T("AccessName"),
                desc =>
                    ?T("The option defines which components are allowed "
                       "for namespace delegation. The default value is 'none'.")}}]}],
      example =>
	  [{?T("Make sure you do not delegate the same namespace to several "
	       "services at the same time. As in the example provided later, "
	       "to have the 'sat-pubsub.example.org' component perform "
	       "correctly disable the _`mod_pubsub`_ module."),
          ["access_rules:",
           "  external_pubsub:",
           "    allow: external_component",
           "  external_mam:",
           "    allow: external_component",
           "",
           "acl:",
           "  external_component:",
           "    server: sat-pubsub.example.org",
           "",
           "modules:",
           "  mod_delegation:",
           "    namespaces:",
           "      urn:xmpp:mam:1:",
           "        access: external_mam",
           "      http://jabber.org/protocol/pubsub:",
           "        access: external_pubsub"]}]}.

depends(_, _) ->
    [].

-spec decode_iq_subel(xmpp_element() | xmlel()) -> xmpp_element() | xmlel().
%% Tell gen_iq_handler not to auto-decode IQ payload
decode_iq_subel(El) ->
    El.

-spec component_connected(binary()) -> ok.
component_connected(Host) ->
    lists:foreach(
      fun(ServerHost) ->
	      Proc = gen_mod:get_module_proc(ServerHost, ?MODULE),
	      gen_server:cast(Proc, {component_connected, Host})
      end, ejabberd_option:hosts()).

-spec component_disconnected(binary(), binary()) -> ok.
component_disconnected(Host, _Reason) ->
    lists:foreach(
      fun(ServerHost) ->
	      Proc = gen_mod:get_module_proc(ServerHost, ?MODULE),
	      gen_server:cast(Proc, {component_disconnected, Host})
      end, ejabberd_option:hosts()).

-spec ejabberd_local(iq()) -> iq().
ejabberd_local(IQ) ->
    process_iq(IQ, ejabberd_local).

-spec ejabberd_sm(iq()) -> iq().
ejabberd_sm(IQ) ->
    process_iq(IQ, ejabberd_sm).

-spec disco_local_features(mod_disco:features_acc(), jid(), jid(),
			   binary(), binary()) -> mod_disco:features_acc().
disco_local_features(Acc, From, To, Node, Lang) ->
    disco_features(Acc, From, To, Node, Lang, ejabberd_local).

-spec disco_sm_features(mod_disco:features_acc(), jid(), jid(),
			binary(), binary()) -> mod_disco:features_acc().
disco_sm_features(Acc, From, To, Node, Lang) ->
    disco_features(Acc, From, To, Node, Lang, ejabberd_sm).

-spec disco_local_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
disco_local_identity(Acc, From, To, Node, Lang) ->
    disco_identity(Acc, From, To, Node, Lang, ejabberd_local).

-spec disco_sm_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
disco_sm_identity(Acc, From, To, Node, Lang) ->
    disco_identity(Acc, From, To, Node, Lang, ejabberd_sm).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Host|_]) ->
    process_flag(trap_exit, true),
    catch ets:new(?MODULE,
                  [named_table, public,
                   {heir, erlang:group_leader(), none}]),
    ejabberd_hooks:add(component_connected, ?MODULE,
		       component_connected, 50),
    ejabberd_hooks:add(component_disconnected, ?MODULE,
		       component_disconnected, 50),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
		       disco_local_features, 50),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       disco_sm_features, 50),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE,
		       disco_local_identity, 50),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE,
		       disco_sm_identity, 50),
    {ok, #state{server_host = Host}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast({component_connected, Host}, State) ->
    ServerHost = State#state.server_host,
    To = jid:make(Host),
    NSAttrsAccessList = mod_delegation_opt:namespaces(ServerHost),
    lists:foreach(
      fun({NS, _Attrs, Access}) ->
	      case acl:match_rule(ServerHost, Access, To) of
		  allow ->
		      send_disco_queries(ServerHost, Host, NS);
		  deny ->
		      ?DEBUG("Denied delegation for ~ts on ~ts", [Host, NS])
	      end
      end, NSAttrsAccessList),
    {noreply, State};
handle_cast({component_disconnected, Host}, State) ->
    ServerHost = State#state.server_host,
    Delegations =
	maps:filter(
	  fun({NS, Type}, {H, _}) when H == Host ->
		  ?INFO_MSG("Remove delegation of namespace '~ts' "
			    "from external component '~ts'",
			    [NS, Host]),
		  gen_iq_handler:remove_iq_handler(Type, ServerHost, NS),
		  false;
	     (_, _) ->
		  true
	  end, get_delegations(ServerHost)),
    set_delegations(ServerHost, Delegations),
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({iq_reply, ResIQ, {disco_info, Type, Host, NS}}, State) ->
    case ResIQ of
	#iq{type = result, sub_els = [SubEl]} ->
	    try xmpp:decode(SubEl) of
		#disco_info{} = Info ->
		    ServerHost = State#state.server_host,
		    process_disco_info(ServerHost, Type, Host, NS, Info)
	    catch _:{xmpp_codec, _} ->
		    ok
	    end;
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({iq_reply, ResIQ, #iq{} = IQ}, State) ->
    process_iq_result(IQ, ResIQ),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    ServerHost = State#state.server_host,
    case gen_mod:is_loaded_elsewhere(ServerHost, ?MODULE) of
	false ->
	    ejabberd_hooks:delete(component_connected, ?MODULE,
				  component_connected, 50),
	    ejabberd_hooks:delete(component_disconnected, ?MODULE,
				  component_disconnected, 50);
	true ->
	    ok
    end,
    ejabberd_hooks:delete(disco_local_features, ServerHost, ?MODULE,
			  disco_local_features, 50),
    ejabberd_hooks:delete(disco_sm_features, ServerHost, ?MODULE,
			  disco_sm_features, 50),
    ejabberd_hooks:delete(disco_local_identity, ServerHost, ?MODULE,
			  disco_local_identity, 50),
    ejabberd_hooks:delete(disco_sm_identity, ServerHost, ?MODULE,
			  disco_sm_identity, 50),
    lists:foreach(
      fun({NS, Type}) ->
	      gen_iq_handler:remove_iq_handler(Type, ServerHost, NS)
      end, maps:keys(get_delegations(ServerHost))),
    ets:delete(?MODULE, ServerHost).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_delegations(binary()) -> delegations().
get_delegations(Host) ->
    try ets:lookup_element(?MODULE, Host, 2)
    catch _:badarg -> #{}
    end.

-spec set_delegations(binary(), delegations()) -> true.
set_delegations(ServerHost, Delegations) ->
    case maps:size(Delegations) of
	0 -> ets:delete(?MODULE, ServerHost);
	_ -> ets:insert(?MODULE, {ServerHost, Delegations})
    end.

-spec process_iq(iq(), route_type()) -> ignore | iq().
process_iq(#iq{to = To, lang = Lang, sub_els = [SubEl]} = IQ, Type) ->
    LServer = To#jid.lserver,
    NS = xmpp:get_ns(SubEl),
    Delegations = get_delegations(LServer),
    case maps:find({NS, Type}, Delegations) of
	{ok, {Host, _}} ->
	    Delegation = #delegation{
			    forwarded = #forwarded{sub_els = [IQ]}},
	    NewFrom = jid:make(LServer),
	    NewTo = jid:make(Host),
	    ejabberd_router:route_iq(
	      #iq{type = set,
		  from = NewFrom,
		  to = NewTo,
		  sub_els = [Delegation]},
	      IQ, gen_mod:get_module_proc(LServer, ?MODULE)),
	    ignore;
	error ->
	    Txt = ?T("Failed to map delegated namespace to external component"),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end.

-spec process_iq_result(iq(), iq()) -> ok.
process_iq_result(#iq{from = From, to = To, id = ID, lang = Lang} = IQ,
		  #iq{type = result} = ResIQ) ->
    try
	CodecOpts = ejabberd_config:codec_options(),
	#delegation{forwarded = #forwarded{sub_els = [SubEl]}} =
	    xmpp:get_subtag(ResIQ, #delegation{}),
	case xmpp:decode(SubEl, ?NS_CLIENT, CodecOpts) of
	    #iq{from = To, to = From, type = Type, id = ID} = Reply
	      when Type == error; Type == result ->
		ejabberd_router:route(Reply)
	end
    catch _:_ ->
	    ?ERROR_MSG("Got iq-result with invalid delegated "
		       "payload:~n~ts", [xmpp:pp(ResIQ)]),
	    Txt = ?T("External component failure"),
	    Err = xmpp:err_internal_server_error(Txt, Lang),
	    ejabberd_router:route_error(IQ, Err)
    end;
process_iq_result(#iq{from = From, to = To}, #iq{type = error} = ResIQ) ->
    Err = xmpp:set_from_to(ResIQ, To, From),
    ejabberd_router:route(Err);
process_iq_result(#iq{lang = Lang} = IQ, timeout) ->
    Txt = ?T("External component timeout"),
    Err = xmpp:err_internal_server_error(Txt, Lang),
    ejabberd_router:route_error(IQ, Err).

-spec process_disco_info(binary(), route_type(),
			 binary(), binary(), disco_info()) -> ok.
process_disco_info(ServerHost, Type, Host, NS, Info) ->
    From = jid:make(ServerHost),
    To = jid:make(Host),
    Delegations = get_delegations(ServerHost),
    case maps:find({NS, Type}, Delegations) of
	error ->
	    Msg = #message{from = From, to = To,
			   sub_els = [#delegation{delegated = [#delegated{ns = NS}]}]},
	    Delegations1 = maps:put({NS, Type}, {Host, Info}, Delegations),
	    gen_iq_handler:add_iq_handler(Type, ServerHost, NS, ?MODULE, Type),
	    ejabberd_router:route(Msg),
	    set_delegations(ServerHost, Delegations1),
	    ?INFO_MSG("Namespace '~ts' is delegated to external component '~ts'",
		      [NS, Host]);
	{ok, {AnotherHost, _}} ->
	    ?WARNING_MSG("Failed to delegate namespace '~ts' to "
			 "external component '~ts' because it's already "
			 "delegated to '~ts'",
			 [NS, Host, AnotherHost])
    end.

-spec send_disco_queries(binary(), binary(), binary()) -> ok.
send_disco_queries(LServer, Host, NS) ->
    From = jid:make(LServer),
    To = jid:make(Host),
    lists:foreach(
      fun({Type, Node}) ->
	      ejabberd_router:route_iq(
		#iq{type = get, from = From, to = To,
		    sub_els = [#disco_info{node = Node}]},
		{disco_info, Type, Host, NS},
		gen_mod:get_module_proc(LServer, ?MODULE))
      end, [{ejabberd_local, <<(?NS_DELEGATION)/binary, "::", NS/binary>>},
	    {ejabberd_sm, <<(?NS_DELEGATION)/binary, ":bare:", NS/binary>>}]).

-spec disco_features(mod_disco:features_acc(), jid(), jid(), binary(), binary(),
		     route_type()) -> mod_disco:features_acc().
disco_features(Acc, _From, To, <<"">>, _Lang, Type) ->
    Delegations = get_delegations(To#jid.lserver),
    Features = my_features(Type) ++
	lists:flatmap(
	  fun({{_, T}, {_, Info}}) when T == Type ->
		  Info#disco_info.features;
	     (_) ->
		  []
	  end, maps:to_list(Delegations)),
    case Acc of
	empty when Features /= [] -> {result, Features};
	{result, Fs} -> {result, Fs ++ Features};
	_ -> Acc
    end;
disco_features(Acc, _, _, _, _, _) ->
    Acc.

-spec disco_identity([identity()], jid(), jid(), binary(), binary(),
		     route_type()) -> [identity()].
disco_identity(Acc, _From, To, <<"">>, _Lang, Type) ->
    Delegations = get_delegations(To#jid.lserver),
    Identities = lists:flatmap(
		   fun({{_, T}, {_, Info}}) when T == Type ->
			   Info#disco_info.identities;
		      (_) ->
			   []
		   end, maps:to_list(Delegations)),
    Acc ++ Identities;
disco_identity(Acc, _From, _To, _Node, _Lang, _Type) ->
    Acc.

my_features(ejabberd_local) -> [?NS_DELEGATION];
my_features(ejabberd_sm) -> [].
