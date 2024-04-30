%%%-------------------------------------------------------------------
%%% File    : mod_mix.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  2 Mar 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
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
-behaviour(gen_mod).
-behaviour(gen_server).
-protocol({xep, 369, '0.14.1', '16.03', "", ""}).

%% API
-export([route/1]).
%% gen_mod callbacks
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1, mod_options/1]).
-export([mod_doc/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% Hooks
-export([process_disco_info/1,
	 process_disco_items/1,
	 process_mix_core/1,
	 process_mam_query/1,
	 process_pubsub_query/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").
-include("ejabberd_stacktrace.hrl").

-callback init(binary(), gen_mod:opts()) -> ok | {error, db_failure}.
-callback set_channel(binary(), binary(), binary(),
		      jid:jid(), boolean(), binary()) ->
    ok | {error, db_failure}.
-callback get_channels(binary(), binary()) ->
    {ok, [binary()]} | {error, db_failure}.
-callback get_channel(binary(), binary(), binary()) ->
    {ok, {jid(), boolean(), binary()}} |
    {error, notfound | db_failure}.
-callback set_participant(binary(), binary(), binary(), jid(), binary(), binary()) ->
    ok | {error, db_failure}.
-callback get_participant(binary(), binary(), binary(), jid()) ->
    {ok, {binary(), binary()}} | {error, notfound | db_failure}.

-record(state, {hosts :: [binary()],
		server_host :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, OldOpts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {reload, Host, NewOpts, OldOpts}).

depends(_Host, _Opts) ->
    [{mod_mam, hard}].

mod_opt_type(access_create) ->
    econf:acl();
mod_opt_type(name) ->
    econf:binary();
mod_opt_type(host) ->
    econf:host();
mod_opt_type(hosts) ->
    econf:hosts();
mod_opt_type(db_type) ->
    econf:db_type(?MODULE).

mod_options(Host) ->
    [{access_create, all},
     {host, <<"mix.", Host/binary>>},
     {hosts, []},
     {name, ?T("Channels")},
     {db_type, ejabberd_config:default_db(Host, ?MODULE)}].

mod_doc() ->
    #{desc =>
          [?T("This module is an experimental implementation of "
              "https://xmpp.org/extensions/xep-0369.html"
              "[XEP-0369: Mediated Information eXchange (MIX)]. "
              "It's asserted that "
              "the MIX protocol is going to replace the MUC protocol "
              "in the future (see _`mod_muc`_)."), "",
           ?T("To learn more about how to use that feature, you can refer to "
	      "our tutorial: _`../../tutorials/mix-010.md|Getting started with MIX`_"), "",
           ?T("The module depends on _`mod_mam`_.")],
      note => "added in 16.03 and improved in 19.02",
      opts =>
          [{access_create,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("An access rule to control MIX channels creations. "
                     "The default value is 'all'.")}},
           {host,
            #{desc => ?T("Deprecated. Use 'hosts' instead.")}},
           {hosts,
            #{value => ?T("[Host, ...]"),
              desc =>
                  ?T("This option defines the Jabber IDs of the service. "
                     "If the 'hosts' option is not specified, the only Jabber ID will "
                     "be the hostname of the virtual host with the prefix \"mix.\". "
                     "The keyword '@HOST@' is replaced with the real virtual host name.")}},
           {name,
            #{value => ?T("Name"),
              desc =>
                  ?T("A name of the service in the Service Discovery. "
                     "This will only be displayed by special XMPP clients. "
                     "The default value is 'Channels'.")}},
           {db_type,
            #{value => "mnesia | sql",
              desc =>
                  ?T("Same as top-level _`default_db`_ option, but applied to this module only.")}}]}.

-spec route(stanza()) -> ok.
route(#iq{} = IQ) ->
    ejabberd_router:process_iq(IQ);
route(#message{type = groupchat, id = ID, lang = Lang,
	       to = #jid{luser = <<_, _/binary>>}} = Msg) ->
    case ID of
	<<>> ->
	    Txt = ?T("Attribute 'id' is mandatory for MIX messages"),
	    Err = xmpp:err_bad_request(Txt, Lang),
	    ejabberd_router:route_error(Msg, Err);
	_ ->
	    process_mix_message(Msg)
    end;
route(Pkt) ->
    ?DEBUG("Dropping packet:~n~ts", [xmpp:pp(Pkt)]).

-spec process_disco_info(iq()) -> iq().
process_disco_info(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_info(#iq{type = get, to = #jid{luser = <<>>} = To,
		       from = _From, lang = Lang,
		       sub_els = [#disco_info{node = <<>>}]} = IQ) ->
    ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
    X = ejabberd_hooks:run_fold(disco_info, ServerHost, [],
				[ServerHost, ?MODULE, <<"">>, Lang]),
    Name = mod_mix_opt:name(ServerHost),
    Identity = #identity{category = <<"conference">>,
			 type = <<"mix">>,
			 name = translate:translate(Lang, Name)},
    Features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS, ?NS_MIX_CORE_0,
		?NS_MIX_CORE_SEARCHABLE_0, ?NS_MIX_CORE_CREATE_CHANNEL_0,
		?NS_MIX_CORE_1, ?NS_MIX_CORE_SEARCHABLE_1,
		?NS_MIX_CORE_CREATE_CHANNEL_1],
    xmpp:make_iq_result(
      IQ, #disco_info{features = Features,
		      identities = [Identity],
		      xdata = X});
process_disco_info(#iq{type = get, to = #jid{luser = <<_, _/binary>>} = To,
		       sub_els = [#disco_info{node = Node}]} = IQ)
  when Node == <<"mix">>; Node == <<>> ->
    {Chan, Host, _} = jid:tolower(To),
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    case Mod:get_channel(ServerHost, Chan, Host) of
	{ok, _} ->
	    Identity = #identity{category = <<"conference">>,
				 type = <<"mix">>},
	    Features = [?NS_DISCO_INFO, ?NS_DISCO_ITEMS,
			?NS_MIX_CORE_0, ?NS_MIX_CORE_1, ?NS_MAM_2],
	    xmpp:make_iq_result(
	      IQ, #disco_info{node = Node,
			      features = Features,
			      identities = [Identity]});
	{error, notfound} ->
	    xmpp:make_error(IQ, no_channel_error(IQ));
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end;
process_disco_info(#iq{type = get, sub_els = [#disco_info{node = Node}]} = IQ) ->
    xmpp:make_iq_result(IQ, #disco_info{node = Node, features = [?NS_DISCO_INFO]});
process_disco_info(IQ) ->
    xmpp:make_error(IQ, unsupported_error(IQ)).

-spec process_disco_items(iq()) -> iq().
process_disco_items(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get, to = #jid{luser = <<>>} = To,
			sub_els = [#disco_items{node = <<>>}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    case Mod:get_channels(ServerHost, Host) of
	{ok, Channels} ->
	    Items = [#disco_item{jid = jid:make(Channel, Host)}
		     || Channel <- Channels],
	    xmpp:make_iq_result(IQ, #disco_items{items = Items});
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end;
process_disco_items(#iq{type = get, to = #jid{luser = <<_, _/binary>>} = To,
			sub_els = [#disco_items{node = Node}]} = IQ)
  when Node == <<"mix">>; Node == <<>> ->
    {Chan, Host, _} = jid:tolower(To),
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    case Mod:get_channel(ServerHost, Chan, Host) of
	{ok, _} ->
	    BTo = jid:remove_resource(To),
	    Items = [#disco_item{jid = BTo, node = N} || N <- known_nodes()],
	    xmpp:make_iq_result(IQ, #disco_items{node = Node, items = Items});
	{error, notfound} ->
	    xmpp:make_error(IQ, no_channel_error(IQ));
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end;
process_disco_items(#iq{type = get, sub_els = [#disco_items{node = Node}]} = IQ) ->
    xmpp:make_iq_result(IQ, #disco_items{node = Node});
process_disco_items(IQ) ->
    xmpp:make_error(IQ, unsupported_error(IQ)).

-spec process_mix_core(iq()) -> iq().
process_mix_core(#iq{type = set, to = #jid{luser = <<>>},
		     sub_els = [#mix_create{}]} = IQ) ->
    process_mix_create(IQ);
process_mix_core(#iq{type = set, to = #jid{luser = <<>>},
		     sub_els = [#mix_destroy{}]} = IQ) ->
    process_mix_destroy(IQ);
process_mix_core(#iq{type = set, to = #jid{luser = <<_, _/binary>>},
		     sub_els = [#mix_join{}]} = IQ) ->
    process_mix_join(IQ);
process_mix_core(#iq{type = set, to = #jid{luser = <<_, _/binary>>},
		     sub_els = [#mix_leave{}]} = IQ) ->
    process_mix_leave(IQ);
process_mix_core(#iq{type = set, to = #jid{luser = <<_, _/binary>>},
		     sub_els = [#mix_setnick{}]} = IQ) ->
    process_mix_setnick(IQ);
process_mix_core(IQ) ->
    xmpp:make_error(IQ, unsupported_error(IQ)).

process_pubsub_query(#iq{type = get,
			 sub_els = [#pubsub{items = #ps_items{node = Node}}]} = IQ)
  when Node == ?NS_MIX_NODES_PARTICIPANTS ->
    process_participants_list(IQ);
process_pubsub_query(IQ) ->
    xmpp:make_error(IQ, unsupported_error(IQ)).

process_mam_query(#iq{from = From, to = To, type = T,
		      sub_els = [#mam_query{}]} = IQ)
  when T == get; T == set ->
    {Chan, Host, _} = jid:tolower(To),
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    case Mod:get_channel(ServerHost, Chan, Host) of
	{ok, _} ->
	    BFrom = jid:remove_resource(From),
	    case Mod:get_participant(ServerHost, Chan, Host, BFrom) of
		{ok, _} ->
		    mod_mam:process_iq(ServerHost, IQ, mix);
		{error, notfound} ->
		    xmpp:make_error(IQ, not_joined_error(IQ));
		{error, db_failure} ->
		    xmpp:make_error(IQ, db_error(IQ))
	    end;
	{error, notfound} ->
	    xmpp:make_error(IQ, no_channel_error(IQ));
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end;
process_mam_query(IQ) ->
    xmpp:make_error(IQ, unsupported_error(IQ)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Host|_]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(Host, ?MODULE),
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    MyHosts = gen_mod:get_opt_hosts(Opts),
    case Mod:init(Host, gen_mod:set_opt(hosts, MyHosts, Opts)) of
	ok ->
	    lists:foreach(
	      fun(MyHost) ->
		      ejabberd_router:register_route(
			MyHost, Host, {apply, ?MODULE, route}),
		      register_iq_handlers(MyHost)
	      end, MyHosts),
	    {ok, #state{hosts = MyHosts, server_host = Host}};
	{error, db_failure} ->
	    {stop, db_failure}
    end.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Request, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info({route, Packet}, State) ->
    try route(Packet)
    catch ?EX_RULE(Class, Reason, St) ->
	    StackTrace = ?EX_STACK(St),
	    ?ERROR_MSG("Failed to route packet:~n~ts~n** ~ts",
		       [xmpp:pp(Packet),
			misc:format_exception(2, Class, Reason, StackTrace)])
    end,
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    lists:foreach(
      fun(MyHost) ->
	      unregister_iq_handlers(MyHost),
	      ejabberd_router:unregister_route(MyHost)
      end, State#state.hosts).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec process_mix_create(iq()) -> iq().
process_mix_create(#iq{to = To, from = From,
		       sub_els = [#mix_create{channel = Chan, xmlns = XmlNs}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    Creator = jid:remove_resource(From),
    Chan1 = case Chan of
		<<>> -> p1_rand:get_string();
		_ -> Chan
	    end,
    Ret = case Mod:get_channel(ServerHost, Chan1, Host) of
	      {ok, {#jid{luser = U, lserver = S}, _, _}} ->
		  case {From#jid.luser, From#jid.lserver} of
		      {U, S} -> ok;
		      _ -> {error, conflict}
		  end;
	      {error, notfound} ->
		  Key = xmpp_util:hex(p1_rand:bytes(20)),
		  Mod:set_channel(ServerHost, Chan1, Host,
				  Creator, Chan == <<>>, Key);
	      {error, db_failure} = Err ->
		  Err
	  end,
    case Ret of
	ok ->
	    xmpp:make_iq_result(IQ, #mix_create{channel = Chan1, xmlns = XmlNs});
	{error, conflict} ->
	    xmpp:make_error(IQ, channel_exists_error(IQ));
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end.

-spec process_mix_destroy(iq()) -> iq().
process_mix_destroy(#iq{to = To,
			from = #jid{luser = U, lserver = S},
			sub_els = [#mix_destroy{channel = Chan, xmlns = XmlNs}]} = IQ) ->
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    case Mod:get_channel(ServerHost, Chan, Host) of
	{ok, {#jid{luser = U, lserver = S}, _, _}} ->
	    case Mod:del_channel(ServerHost, Chan, Host) of
		ok ->
		    xmpp:make_iq_result(IQ, #mix_destroy{channel = Chan, xmlns = XmlNs});
		{error, db_failure} ->
		    xmpp:make_error(IQ, db_error(IQ))
	    end;
	{ok, _} ->
	    xmpp:make_error(IQ, ownership_error(IQ));
	{error, notfound} ->
	    xmpp:make_error(IQ, no_channel_error(IQ));
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end.

-spec process_mix_join(iq()) -> iq().
process_mix_join(#iq{to = To, from = From,
		     sub_els = [#mix_join{xmlns = XmlNs} = JoinReq]} = IQ) ->
    Chan = To#jid.luser,
    Host = To#jid.lserver,
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    case Mod:get_channel(ServerHost, Chan, Host) of
	{ok, {_, _, Key}} ->
	    ID = make_id(From, Key),
	    Nick = JoinReq#mix_join.nick,
	    BFrom = jid:remove_resource(From),
	    Nodes = filter_nodes(JoinReq#mix_join.subscribe),
	    try
		ok = Mod:set_participant(ServerHost, Chan, Host, BFrom, ID, Nick),
		ok = Mod:subscribe(ServerHost, Chan, Host, BFrom, Nodes),
		notify_participant_joined(Mod, ServerHost, To, From, ID, Nick),
		xmpp:make_iq_result(IQ, #mix_join{id = ID,
						  subscribe = Nodes,
						  jid = make_channel_id(To, ID),
						  nick = Nick,
						  xmlns = XmlNs})
	    catch _:{badmatch, {error, db_failure}} ->
		    xmpp:make_error(IQ, db_error(IQ))
	    end;
	{error, notfound} ->
	    xmpp:make_error(IQ, no_channel_error(IQ));
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end.

-spec process_mix_leave(iq()) -> iq().
process_mix_leave(#iq{to = To, from = From,
		      sub_els = [#mix_leave{xmlns = XmlNs}]} = IQ) ->
    {Chan, Host, _} = jid:tolower(To),
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    BFrom = jid:remove_resource(From),
    case Mod:get_channel(ServerHost, Chan, Host) of
	{ok, _} ->
	    case Mod:get_participant(ServerHost, Chan, Host, BFrom) of
		{ok, {ID, _}} ->
		    try
			ok = Mod:unsubscribe(ServerHost, Chan, Host, BFrom),
			ok = Mod:del_participant(ServerHost, Chan, Host, BFrom),
			notify_participant_left(Mod, ServerHost, To, ID),
			xmpp:make_iq_result(IQ, #mix_leave{})
		    catch _:{badmatch, {error, db_failure}} ->
			    xmpp:make_error(IQ, db_error(IQ))
		    end;
		{error, notfound} ->
		    xmpp:make_iq_result(IQ, #mix_leave{xmlns = XmlNs});
		{error, db_failure} ->
		    xmpp:make_error(IQ, db_error(IQ))
	    end;
	{error, notfound} ->
	    xmpp:make_iq_result(IQ, #mix_leave{});
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end.

-spec process_mix_setnick(iq()) -> iq().
process_mix_setnick(#iq{to = To, from = From,
			sub_els = [#mix_setnick{nick = Nick, xmlns = XmlNs}]} = IQ) ->
    {Chan, Host, _} = jid:tolower(To),
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    BFrom = jid:remove_resource(From),
    case Mod:get_channel(ServerHost, Chan, Host) of
	{ok, _} ->
	    case Mod:get_participant(ServerHost, Chan, Host, BFrom) of
		{ok, {_, Nick}} ->
		    xmpp:make_iq_result(IQ, #mix_setnick{nick = Nick, xmlns = XmlNs});
		{ok, {ID, _}} ->
		    case Mod:set_participant(ServerHost, Chan, Host, BFrom, ID, Nick) of
			ok ->
			    notify_participant_joined(Mod, ServerHost, To, From, ID, Nick),
			    xmpp:make_iq_result(IQ, #mix_setnick{nick = Nick, xmlns = XmlNs});
			{error, db_failure} ->
			    xmpp:make_error(IQ, db_error(IQ))
		    end;
		{error, notfound} ->
		    xmpp:make_error(IQ, not_joined_error(IQ));
		{error, db_failure} ->
		    xmpp:make_error(IQ, db_error(IQ))
	    end;
	{error, notfound} ->
	    xmpp:make_error(IQ, no_channel_error(IQ));
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end.

-spec process_mix_message(message()) -> ok.
process_mix_message(#message{from = From, to = To,
			     id = SubmissionID} = Msg) ->
    {Chan, Host, _} = jid:tolower(To),
    {FUser, FServer, _} = jid:tolower(From),
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    case Mod:get_channel(ServerHost, Chan, Host) of
	{ok, _} ->
	    BFrom = jid:remove_resource(From),
	    case Mod:get_participant(ServerHost, Chan, Host, BFrom) of
		{ok, {StableID, Nick}} ->
		    MamID = mod_mam:make_id(),
		    Msg1 = xmpp:set_subtag(
			     Msg#message{from = jid:replace_resource(To, StableID),
					 to = undefined,
					 id = integer_to_binary(MamID)},
			     #mix{jid = BFrom, nick = Nick}),
		    Msg2 = xmpp:put_meta(Msg1, stanza_id, MamID),
		    case ejabberd_hooks:run_fold(
			   store_mam_message, ServerHost, Msg2,
			   [Chan, Host, BFrom, Nick, groupchat, recv]) of
			#message{} ->
			    multicast(Mod, ServerHost, Chan, Host,
				      ?NS_MIX_NODES_MESSAGES,
				      fun(#jid{luser = U, lserver = S})
					    when U == FUser, S == FServer ->
					      xmpp:set_subtag(
						Msg1, #mix{jid = BFrom,
							   nick = Nick,
							   submission_id = SubmissionID});
					 (_) ->
					      Msg1
				      end);
			_ ->
			    ok
		    end;
		{error, notfound} ->
		    ejabberd_router:route_error(Msg, not_joined_error(Msg));
		{error, db_failure} ->
		    ejabberd_router:route_error(Msg, db_error(Msg))
	    end;
	{error, notfound} ->
	    ejabberd_router:route_error(Msg, no_channel_error(Msg));
	{error, db_failure} ->
	    ejabberd_router:route_error(Msg, db_error(Msg))
    end.

-spec process_participants_list(iq()) -> iq().
process_participants_list(#iq{from = From, to = To} = IQ) ->
    {Chan, Host, _} = jid:tolower(To),
    ServerHost = ejabberd_router:host_of_route(Host),
    Mod = gen_mod:db_mod(ServerHost, ?MODULE),
    case Mod:get_channel(ServerHost, Chan, Host) of
	{ok, _} ->
	    BFrom = jid:remove_resource(From),
	    case Mod:get_participant(ServerHost, Chan, Host, BFrom) of
		{ok, _} ->
		    case Mod:get_participants(ServerHost, Chan, Host) of
			{ok, Participants} ->
			    Items = items_of_participants(Participants),
			    Pubsub = #pubsub{
					items = #ps_items{
						   node = ?NS_MIX_NODES_PARTICIPANTS,
						   items = Items}},
			    xmpp:make_iq_result(IQ, Pubsub);
			{error, db_failure} ->
			    xmpp:make_error(IQ, db_error(IQ))
		    end;
		{error, notfound} ->
		    xmpp:make_error(IQ, not_joined_error(IQ));
		{error, db_failure} ->
		    xmpp:make_error(IQ, db_error(IQ))
	    end;
	{error, notfound} ->
	    xmpp:make_error(IQ, no_channel_error(IQ));
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end.

-spec items_of_participants([{jid(), binary(), binary()}]) -> [ps_item()].
items_of_participants(Participants) ->
    lists:map(
      fun({JID, ID, Nick}) ->
	      Participant = #mix_participant{jid = JID, nick = Nick},
	      #ps_item{id = ID,
		       sub_els = [xmpp:encode(Participant)]}
      end, Participants).

-spec known_nodes() -> [binary()].
known_nodes() ->
    [?NS_MIX_NODES_MESSAGES,
     ?NS_MIX_NODES_PARTICIPANTS].

-spec filter_nodes([binary()]) -> [binary()].
filter_nodes(Nodes) ->
    KnownNodes = known_nodes(),
    [Node || KnownNode <- KnownNodes, Node <- Nodes, KnownNode == Node].

-spec multicast(module(), binary(), binary(),
		binary(), binary(), fun((jid()) -> message())) -> ok.
multicast(Mod, LServer, Chan, Service, Node, F) ->
    case Mod:get_subscribed(LServer, Chan, Service, Node) of
	{ok, Subscribers} ->
	    lists:foreach(
	      fun(To) ->
		      Msg = xmpp:set_to(F(To), To),
		      ejabberd_router:route(Msg)
	      end, Subscribers);
	{error, db_failure} ->
	    ok
    end.

-spec notify_participant_joined(module(), binary(),
				jid(), jid(), binary(), binary()) -> ok.
notify_participant_joined(Mod, LServer, To, From, ID, Nick) ->
    {Chan, Host, _} = jid:tolower(To),
    Participant = #mix_participant{jid = jid:remove_resource(From),
				   nick = Nick},
    Item = #ps_item{id = ID,
		    sub_els = [xmpp:encode(Participant)]},
    Items = #ps_items{node = ?NS_MIX_NODES_PARTICIPANTS,
		      items = [Item]},
    Event = #ps_event{items = Items},
    Msg = #message{from = jid:remove_resource(To),
		   id = p1_rand:get_string(),
		   sub_els = [Event]},
    multicast(Mod, LServer, Chan, Host,
	      ?NS_MIX_NODES_PARTICIPANTS,
	      fun(_) -> Msg end).

-spec notify_participant_left(module(), binary(), jid(), binary()) -> ok.
notify_participant_left(Mod, LServer, To, ID) ->
    {Chan, Host, _} = jid:tolower(To),
    Items = #ps_items{node = ?NS_MIX_NODES_PARTICIPANTS,
		      retract = ID},
    Event = #ps_event{items = Items},
    Msg = #message{from = jid:remove_resource(To),
		   id = p1_rand:get_string(),
		   sub_els = [Event]},
    multicast(Mod, LServer, Chan, Host, ?NS_MIX_NODES_PARTICIPANTS,
	     fun(_) -> Msg end).

-spec make_id(jid(), binary()) -> binary().
make_id(JID, Key) ->
    Data = jid:encode(jid:tolower(jid:remove_resource(JID))),
    xmpp_util:hex(misc:crypto_hmac(sha256, Data, Key, 10)).

-spec make_channel_id(jid(), binary()) -> jid().
make_channel_id(JID, ID) ->
	{U, S, R} = jid:split(JID),
	jid:make(<<ID/binary, $#, U/binary>>, S, R).

%%%===================================================================
%%% Error generators
%%%===================================================================
-spec db_error(stanza()) -> stanza_error().
db_error(Pkt) ->
    Txt = ?T("Database failure"),
    xmpp:err_internal_server_error(Txt, xmpp:get_lang(Pkt)).

-spec channel_exists_error(stanza()) -> stanza_error().
channel_exists_error(Pkt) ->
    Txt = ?T("Channel already exists"),
    xmpp:err_conflict(Txt, xmpp:get_lang(Pkt)).

-spec no_channel_error(stanza()) -> stanza_error().
no_channel_error(Pkt) ->
    Txt = ?T("Channel does not exist"),
    xmpp:err_item_not_found(Txt, xmpp:get_lang(Pkt)).

-spec not_joined_error(stanza()) -> stanza_error().
not_joined_error(Pkt) ->
    Txt = ?T("You are not joined to the channel"),
    xmpp:err_forbidden(Txt, xmpp:get_lang(Pkt)).

-spec unsupported_error(stanza()) -> stanza_error().
unsupported_error(Pkt) ->
    Txt = ?T("No module is handling this query"),
    xmpp:err_service_unavailable(Txt, xmpp:get_lang(Pkt)).

-spec ownership_error(stanza()) -> stanza_error().
ownership_error(Pkt) ->
    Txt = ?T("Owner privileges required"),
    xmpp:err_forbidden(Txt, xmpp:get_lang(Pkt)).

%%%===================================================================
%%% IQ handlers
%%%===================================================================
-spec register_iq_handlers(binary()) -> ok.
register_iq_handlers(Host) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
				  ?MODULE, process_disco_info),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
				  ?MODULE, process_disco_items),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MIX_CORE_0,
				  ?MODULE, process_mix_core),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MIX_CORE_1,
				  ?MODULE, process_mix_core),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_DISCO_INFO,
				  ?MODULE, process_disco_info),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_DISCO_ITEMS,
				  ?MODULE, process_disco_items),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MIX_CORE_0,
				  ?MODULE, process_mix_core),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MIX_CORE_1,
				  ?MODULE, process_mix_core),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PUBSUB,
				  ?MODULE, process_pubsub_query),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_2,
				  ?MODULE, process_mam_query).

-spec unregister_iq_handlers(binary()) -> ok.
unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MIX_CORE_0),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MIX_CORE_1),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MIX_CORE_0),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MIX_CORE_1),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUBSUB),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_2).
