%%%-------------------------------------------------------------------
%%% Created : 20 Oct 2024 by Pawel Chmielowski <pawel@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-module(mod_s2s_bidi).
-behaviour(gen_mod).
-protocol({xep, 288, '1.0.1', '24.10', "complete", ""}).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_options/1]).
-export([mod_doc/0]).
%% Hooks
-export([s2s_in_packet/2, s2s_out_packet/2,
	 s2s_in_features/2, s2s_in_auth_result/3, s2s_out_unauthenticated_features/2, s2s_in_handle_info/2]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start(_Host, _Opts) ->
    {ok, [{hook, s2s_in_pre_auth_features, s2s_in_features, 50},
	  {hook, s2s_in_post_auth_features, s2s_in_features, 50},
	  {hook, s2s_in_unauthenticated_packet, s2s_in_packet, 50},
	  {hook, s2s_in_authenticated_packet, s2s_in_packet, 50},
	  {hook, s2s_in_handle_info, s2s_in_handle_info, 50},
	  {hook, s2s_in_auth_result, s2s_in_auth_result, 50},
	  {hook, s2s_out_unauthenticated_features, s2s_out_unauthenticated_features, 50},
	  {hook, s2s_out_packet, s2s_out_packet, 50}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

mod_options(_Host) ->
    [].

mod_doc() ->
    #{desc =>
      [?T("The module adds support for "
	  "https://xmpp.org/extensions/xep-0288.html"
	  "[XEP-0288: Bidirectional Server-to-Server Connections] that allows using "
	  "single s2s connection to communicate in both directions.")],
      note => "added in 24.10",
      opts => [],
      example =>
      ["modules:",
       "  mod_s2s_bidi: {}"]}.

s2s_in_features(Acc, _) ->
    [#s2s_bidi_feature{}|Acc].

s2s_in_packet(State, #s2s_bidi{}) ->
    {stop, State#{bidi_enabled => true}};
s2s_in_packet(State, _) ->
    State.

s2s_out_unauthenticated_features(#{db_verify := _} = State, _) ->
    State;
s2s_out_unauthenticated_features(State, #stream_features{} = Pkt) ->
    try xmpp:try_subtag(Pkt, #s2s_bidi{}) of
	#s2s_bidi{} ->
	    ejabberd_s2s_out:send(State#{bidi_enabled => true}, #s2s_bidi{});
	_ ->
	    State
    catch _:{xmpp_codec, _Why} ->
	State
    end;
s2s_out_unauthenticated_features(State, _Pkt) ->
    State.

s2s_out_packet(#{bidi_enabled := true, ip := {IP, _}} = State, Pkt0)
    when ?is_stanza(Pkt0) ->
    To = xmpp:get_to(Pkt0),
    case check_from_to(State, xmpp:get_from(Pkt0), To) of
	ok ->
	    Pkt = xmpp:put_meta(Pkt0, ip, IP),
	    LServer = ejabberd_router:host_of_route(To#jid.lserver),
	    State1 = ejabberd_hooks:run_fold(s2s_in_authenticated_packet,
					     LServer, State, [Pkt]),
	    {Pkt1, State2} = ejabberd_hooks:run_fold(s2s_receive_packet, LServer,
						     {Pkt, State1}, []),
	    case Pkt1 of
		drop -> ok;
		_ -> ejabberd_router:route(Pkt1)
	    end,
	    {stop, State2};
	{error, Err} ->
	    {stop, ejabberd_s2s_out:send(State, Err)}
    end;
s2s_out_packet(#{db_verify := _} = State, #stream_features{}) ->
    State;
s2s_out_packet(State, #stream_features{} = Pkt) ->
    try xmpp:try_subtag(Pkt, #s2s_bidi_feature{}) of
	#s2s_bidi_feature{} ->
	    ejabberd_s2s_out:send(State#{bidi_enabled => true}, #s2s_bidi{})
    catch _:{xmpp_codec, _Why} ->
	State
    end;
s2s_out_packet(State, _Pkt) ->
    State.

s2s_in_handle_info(State, {route, Pkt}) when ?is_stanza(Pkt) ->
    ejabberd_s2s_in:send(State, Pkt);
s2s_in_handle_info(State, _Info) ->
    State.

check_from_to(#{remote_server := RServer}, #jid{lserver = FromServer},
	      #jid{lserver = ToServer}) ->
    if
	RServer /= FromServer -> {error, xmpp:serr_invalid_from()};
	true ->
	    case ejabberd_router:is_my_route(ToServer) of
		false -> {error, xmpp:serr_host_unknown()};
		_ -> ok
	    end
    end.

s2s_in_auth_result(#{server := LServer, bidi_enabled := true} = State, true, RServer) ->
    ejabberd_s2s:register_connection({LServer, RServer}),
    State;
s2s_in_auth_result(State, _, _) ->
    State.
