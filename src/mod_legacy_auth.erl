%%%-------------------------------------------------------------------
%%% Created : 11 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(mod_legacy_auth).
-behaviour(gen_mod).

-protocol({xep, 78, '2.5'}).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1]).
%% hooks
-export([c2s_unauthenticated_packet/2, c2s_stream_features/2]).

-include("xmpp.hrl").

-type c2s_state() :: ejabberd_c2s:state().

%%%===================================================================
%%% API
%%%===================================================================
start(Host, _Opts) ->
    ejabberd_hooks:add(c2s_unauthenticated_packet, Host, ?MODULE,
		       c2s_unauthenticated_packet, 50),
    ejabberd_hooks:add(c2s_pre_auth_features, Host, ?MODULE,
		       c2s_stream_features, 50).

stop(Host) ->
    ejabberd_hooks:delete(c2s_unauthenticated_packet, Host, ?MODULE,
			  c2s_unauthenticated_packet, 50),
    ejabberd_hooks:delete(c2s_pre_auth_features, Host, ?MODULE,
			  c2s_stream_features, 50).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

mod_opt_type(_) ->
    [].

-spec c2s_unauthenticated_packet(c2s_state(), iq()) ->
      c2s_state() | {stop, c2s_state()}.
c2s_unauthenticated_packet(State, #iq{type = T, sub_els = [_]} = IQ)
  when T == get; T == set ->
    case xmpp:get_subtag(IQ, #legacy_auth{}) of
	#legacy_auth{} = Auth ->
	    {stop, authenticate(State, xmpp:set_els(IQ, [Auth]))};
	false ->
	    State
    end;
c2s_unauthenticated_packet(State, _) ->
    State.

-spec c2s_stream_features([xmpp_element()], binary()) -> [xmpp_element()].
c2s_stream_features(Acc, LServer) ->
    case gen_mod:is_loaded(LServer, ?MODULE) of
	true ->
	    [#legacy_auth_feature{}|Acc];
	false ->
	    Acc
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec authenticate(c2s_state(), iq()) -> c2s_state().
authenticate(#{server := Server} = State,
	     #iq{type = get, sub_els = [#legacy_auth{}]} = IQ) ->
    LServer = jid:nameprep(Server),
    Auth = #legacy_auth{username = <<>>, password = <<>>, resource = <<>>},
    Res = case ejabberd_auth:plain_password_required(LServer) of
	      false ->
		  xmpp:make_iq_result(IQ, Auth#legacy_auth{digest = <<>>});
	      true ->
		  xmpp:make_iq_result(IQ, Auth)
	  end,
    ejabberd_c2s:send(State, Res);
authenticate(State,
	     #iq{type = set, lang = Lang,
		 sub_els = [#legacy_auth{username = U,
					 resource = R}]} = IQ)
  when U == undefined; R == undefined; U == <<"">>; R == <<"">> ->
    Txt = <<"Both the username and the resource are required">>,
    Err = xmpp:make_error(IQ, xmpp:err_not_acceptable(Txt, Lang)),
    ejabberd_c2s:send(State, Err);
authenticate(#{stream_id := StreamID, server := Server,
	       access := Access, ip := IP} = State,
	     #iq{type = set, lang = Lang,
		 sub_els = [#legacy_auth{username = U,
					 password = P0,
					 digest = D0,
					 resource = R}]} = IQ) ->
    P = if is_binary(P0) -> P0; true -> <<>> end,
    D = if is_binary(D0) -> D0; true -> <<>> end,
    DGen = fun (PW) -> str:sha(<<StreamID/binary, PW/binary>>) end,
    JID = jid:make(U, Server, R),
    case JID /= error andalso
	acl:access_matches(Access,
			   #{usr => jid:split(JID), ip => IP},
			   JID#jid.lserver) == allow of
	true ->
	    case ejabberd_auth:check_password_with_authmodule(
		   U, U, JID#jid.lserver, P, D, DGen) of
		{true, AuthModule} ->
		    State1 = ejabberd_c2s:handle_auth_success(
			       U, <<"legacy">>, AuthModule, State),
		    State2 = State1#{user := U},
		    open_session(State2, IQ, R);
		_ ->
		    Err = xmpp:make_error(IQ, xmpp:err_not_authorized()),
		    process_auth_failure(State, U, Err, 'not-authorized')
	    end;
	false when JID == error ->
	    Err = xmpp:make_error(IQ, xmpp:err_jid_malformed()),
	    process_auth_failure(State, U, Err, 'jid-malformed');
	false ->
	    Txt = <<"Denied by ACL">>,
	    Err = xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang)),
	    process_auth_failure(State, U, Err, 'forbidden')
    end.

-spec open_session(c2s_state(), iq(), binary()) -> c2s_state().
open_session(State, IQ, R) ->
    case ejabberd_c2s:bind(R, State) of
	{ok, State1} ->
	    Res = xmpp:make_iq_result(IQ),
	    ejabberd_c2s:send(State1, Res);
	{error, Err, State1} ->
	    Res = xmpp:make_error(IQ, Err),
	    ejabberd_c2s:send(State1, Res)
    end.

-spec process_auth_failure(c2s_state(), binary(), iq(), atom()) -> c2s_state().
process_auth_failure(State, User, StanzaErr, Reason) ->
    State1 = ejabberd_c2s:send(State, StanzaErr),
    ejabberd_c2s:handle_auth_failure(User, <<"legacy">>, Reason, State1).
