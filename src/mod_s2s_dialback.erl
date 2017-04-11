%%%-------------------------------------------------------------------
%%% Created : 16 Dec 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(mod_s2s_dialback).
-behaviour(gen_mod).

-protocol({xep, 220, '1.1.1'}).
-protocol({xep, 185, '1.0'}).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1]).
%% Hooks
-export([s2s_out_auth_result/2, s2s_out_downgraded/2,
	 s2s_in_packet/2, s2s_out_packet/2, s2s_in_recv/3,
	 s2s_in_features/2, s2s_out_init/2, s2s_out_closed/2]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start(Host, _Opts) ->
    case ejabberd_s2s:tls_verify(Host) of
	true ->
	    ?ERROR_MSG("disabling ~s for host ~s because option "
		       "'s2s_use_starttls' is set to 'required_trusted'",
		       [?MODULE, Host]);
	false ->
	    ejabberd_hooks:add(s2s_out_init, Host, ?MODULE, s2s_out_init, 50),
	    ejabberd_hooks:add(s2s_out_closed, Host, ?MODULE, s2s_out_closed, 50),
	    ejabberd_hooks:add(s2s_in_pre_auth_features, Host, ?MODULE,
			       s2s_in_features, 50),
	    ejabberd_hooks:add(s2s_in_post_auth_features, Host, ?MODULE,
			       s2s_in_features, 50),
	    ejabberd_hooks:add(s2s_in_handle_recv, Host, ?MODULE,
			       s2s_in_recv, 50),
	    ejabberd_hooks:add(s2s_in_unauthenticated_packet, Host, ?MODULE,
			       s2s_in_packet, 50),
	    ejabberd_hooks:add(s2s_in_authenticated_packet, Host, ?MODULE,
			       s2s_in_packet, 50),
	    ejabberd_hooks:add(s2s_out_packet, Host, ?MODULE,
			       s2s_out_packet, 50),
	    ejabberd_hooks:add(s2s_out_downgraded, Host, ?MODULE,
			       s2s_out_downgraded, 50),
	    ejabberd_hooks:add(s2s_out_auth_result, Host, ?MODULE,
			       s2s_out_auth_result, 50)
    end.

stop(Host) ->
    ejabberd_hooks:delete(s2s_out_init, Host, ?MODULE, s2s_out_init, 50),
    ejabberd_hooks:delete(s2s_out_closed, Host, ?MODULE, s2s_out_closed, 50),
    ejabberd_hooks:delete(s2s_in_pre_auth_features, Host, ?MODULE,
			  s2s_in_features, 50),
    ejabberd_hooks:delete(s2s_in_post_auth_features, Host, ?MODULE,
			  s2s_in_features, 50),
    ejabberd_hooks:delete(s2s_in_handle_recv, Host, ?MODULE,
			  s2s_in_recv, 50),
    ejabberd_hooks:delete(s2s_in_unauthenticated_packet, Host, ?MODULE,
			  s2s_in_packet, 50),
    ejabberd_hooks:delete(s2s_in_authenticated_packet, Host, ?MODULE,
			  s2s_in_packet, 50),
    ejabberd_hooks:delete(s2s_out_packet, Host, ?MODULE,
			  s2s_out_packet, 50),
    ejabberd_hooks:delete(s2s_out_downgraded, Host, ?MODULE,
			  s2s_out_downgraded, 50),
    ejabberd_hooks:delete(s2s_out_auth_result, Host, ?MODULE,
			  s2s_out_auth_result, 50).

reload(Host, NewOpts, _OldOpts) ->
    case ejabberd_s2s:tls_verify(Host) of
	false ->
	    start(Host, NewOpts);
	true ->
	    stop(Host)
    end.

depends(_Host, _Opts) ->
    [].

mod_opt_type(_) ->
    [].

s2s_in_features(Acc, _) ->
    [#db_feature{errors = true}|Acc].

s2s_out_init({ok, State}, Opts) ->
    case proplists:get_value(db_verify, Opts) of
	{StreamID, Key, Pid} ->
	    %% This is an outbound s2s connection created at step 1.
	    %% The purpose of this connection is to verify dialback key ONLY.
	    %% The connection is not registered in s2s table and thus is not
	    %% seen by anyone.
	    %% The connection will be closed immediately after receiving the
	    %% verification response (at step 3)
	    {ok, State#{db_verify => {StreamID, Key, Pid}}};
	undefined ->
	    {ok, State#{db_enabled => true}}
    end;
s2s_out_init(Acc, _Opts) ->
    Acc.

s2s_out_closed(#{server := LServer,
		 remote_server := RServer,
		 db_verify := {StreamID, _Key, _Pid}} = State, Reason) ->
    %% Outbound s2s verificating connection (created at step 1) is
    %% closed suddenly without receiving the response.
    %% Building a response on our own
    Response = #db_verify{from = RServer, to = LServer,
			  id = StreamID, type = error,
			  sub_els = [mk_error(Reason)]},
    s2s_out_packet(State, Response);
s2s_out_closed(State, _Reason) ->
    State.

s2s_out_auth_result(#{db_verify := _} = State, _) ->
    %% The temporary outbound s2s connect (intended for verification)
    %% has passed authentication state (either successfully or not, no matter)
    %% and at this point we can send verification request as described
    %% in section 2.1.2, step 2
    {stop, send_verify_request(State)};
s2s_out_auth_result(#{db_enabled := true,
		      sockmod := SockMod,
		      socket := Socket, ip := IP,
		      server := LServer,
		      remote_server := RServer} = State, {false, _}) ->
    %% SASL authentication has failed, retrying with dialback
    %% Sending dialback request, section 2.1.1, step 1
    ?INFO_MSG("(~s) Retrying with s2s dialback authentication: ~s -> ~s (~s)",
	      [SockMod:pp(Socket), LServer, RServer,
	       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
    State1 = maps:remove(stop_reason, State#{on_route => queue}),
    {stop, send_db_request(State1)};
s2s_out_auth_result(State, _) ->
    State.

s2s_out_downgraded(#{db_verify := _} = State, _) ->
    %% The verifying outbound s2s connection detected non-RFC compliant
    %% server, send verification request immediately without auth phase,
    %% section 2.1.2, step 2
    {stop, send_verify_request(State)};
s2s_out_downgraded(#{db_enabled := true,
		     sockmod := SockMod,
		     socket := Socket, ip := IP,
		     server := LServer,
		     remote_server := RServer} = State, _) ->
    %% non-RFC compliant server detected, send dialback request instantly,
    %% section 2.1.1, step 1
    ?INFO_MSG("(~s) Trying s2s dialback authentication with "
	      "non-RFC compliant server: ~s -> ~s (~s)",
	      [SockMod:pp(Socket), LServer, RServer,
	       ejabberd_config:may_hide_data(misc:ip_to_list(IP))]),
    {stop, send_db_request(State)};
s2s_out_downgraded(State, _) ->
    State.

s2s_in_packet(#{stream_id := StreamID} = State,
	      #db_result{from = From, to = To, key = Key, type = undefined}) ->
    %% Received dialback request, section 2.2.1, step 1
    try
	ok = check_from_to(From, To),
	%% We're creating a temporary outbound s2s connection to
	%% send verification request and to receive verification response
	{ok, Pid} = ejabberd_s2s_out:start(
		      To, From, [{db_verify, {StreamID, Key, self()}}]),
	ejabberd_s2s_out:connect(Pid),
	{stop, State}
    catch _:{badmatch, {error, Reason}} ->
	    {stop,
	     send_db_result(State,
			    #db_verify{from = From, to = To, type = error,
				       sub_els = [mk_error(Reason)]})}
    end;
s2s_in_packet(State, #db_verify{to = To, from = From, key = Key,
				id = StreamID, type = undefined}) ->
    %% Received verification request, section 2.2.2, step 2
    Type = case make_key(To, From, StreamID) of
	       Key -> valid;
	       _ -> invalid
	   end,
    Response = #db_verify{from = To, to = From, id = StreamID, type = Type},
    {stop, ejabberd_s2s_in:send(State, Response)};
s2s_in_packet(State, Pkt) when is_record(Pkt, db_result);
			       is_record(Pkt, db_verify) ->
    ?WARNING_MSG("Got stray dialback packet:~n~s", [xmpp:pp(Pkt)]),
    State;
s2s_in_packet(State, _) ->
    State.

s2s_in_recv(State, El, {error, Why}) ->
    case xmpp:get_name(El) of
	Tag when Tag == <<"db:result">>;
		 Tag == <<"db:verify">> ->
	    case xmpp:get_type(El) of
		T when T /= <<"valid">>,
		       T /= <<"invalid">>,
		       T /= <<"error">> ->
		    Err = xmpp:make_error(El, mk_error({codec_error, Why})),
		    {stop, ejabberd_s2s_in:send(State, Err)};
		_ ->
		    State
	    end;
	_ ->
	    State
    end;
s2s_in_recv(State, _El, _Pkt) ->
    State.

s2s_out_packet(#{server := LServer,
		 remote_server := RServer,
		 db_verify := {StreamID, _Key, Pid}} = State,
	       #db_verify{from = RServer, to = LServer,
			  id = StreamID, type = Type} = Response)
  when Type /= undefined ->
    %% Received verification response, section 2.1.2, step 3
    %% This is a response for the request sent at step 2
    ejabberd_s2s_in:update_state(
      Pid, fun(S) -> send_db_result(S, Response) end),
    %% At this point the connection is no longer needed and we can terminate it
    ejabberd_s2s_out:stop(State);
s2s_out_packet(#{server := LServer, remote_server := RServer} = State,
	       #db_result{to = LServer, from = RServer,
			  type = Type} = Result) when Type /= undefined ->
    %% Received dialback response, section 2.1.1, step 4
    %% This is a response to the request sent at step 1
    State1 = maps:remove(db_enabled, State),
    case Type of
	valid ->
	    State2 = ejabberd_s2s_out:handle_auth_success(<<"dialback">>, State1),
	    ejabberd_s2s_out:establish(State2);
	_ ->
	    Reason = str:format("Peer responded with error: ~s",
				[format_error(Result)]),
	    ejabberd_s2s_out:handle_auth_failure(
	      <<"dialback">>, {auth, Reason}, State1)
    end;
s2s_out_packet(State, Pkt) when is_record(Pkt, db_result);
				is_record(Pkt, db_verify) ->
    ?WARNING_MSG("Got stray dialback packet:~n~s", [xmpp:pp(Pkt)]),
    State;
s2s_out_packet(State, _) ->
    State.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec make_key(binary(), binary(), binary()) -> binary().
make_key(From, To, StreamID) ->
    Secret = ejabberd_config:get_option(shared_key, fun(V) -> V end),
    str:to_hexlist(
      crypto:hmac(sha256, str:to_hexlist(crypto:hash(sha256, Secret)),
		  [To, " ", From, " ", StreamID])).

-spec send_verify_request(ejabberd_s2s_out:state()) -> ejabberd_s2s_out:state().
send_verify_request(#{server := LServer,
		      remote_server := RServer,
		      db_verify := {StreamID, Key, _Pid}} = State) ->
    Request = #db_verify{from = LServer, to = RServer,
			 key = Key, id = StreamID},
    ejabberd_s2s_out:send(State, Request).

-spec send_db_request(ejabberd_s2s_out:state()) -> ejabberd_s2s_out:state().
send_db_request(#{server := LServer,
		  remote_server := RServer,
		  stream_remote_id := StreamID} = State) ->
    Key = make_key(LServer, RServer, StreamID),
    ejabberd_s2s_out:send(State, #db_result{from = LServer,
					    to = RServer,
					    key = Key}).

-spec send_db_result(ejabberd_s2s_in:state(), db_verify()) -> ejabberd_s2s_in:state().
send_db_result(State, #db_verify{from = From, to = To,
				 type = Type, sub_els = Els}) ->
    %% Sending dialback response, section 2.2.1, step 4
    %% This is a response to the request received at step 1
    Response = #db_result{from = To, to = From, type = Type, sub_els = Els},
    State1 = ejabberd_s2s_in:send(State, Response),
    case Type of
	valid ->
	    State2 = ejabberd_s2s_in:handle_auth_success(
		       From, <<"dialback">>, undefined, State1),
	    ejabberd_s2s_in:establish(State2);
	_ ->
	    Reason = str:format("Verification failed: ~s",
				[format_error(Response)]),
	    ejabberd_s2s_in:handle_auth_failure(
	      From, <<"dialback">>, Reason, State1)
    end.

-spec check_from_to(binary(), binary()) -> ok | {error, forbidden | host_unknown}.
check_from_to(From, To) ->
    case ejabberd_router:is_my_route(To) of
    	false -> {error, host_unknown};
    	true ->
	    LServer = ejabberd_router:host_of_route(To),
    	    case ejabberd_s2s:allow_host(LServer, From) of
    		true -> ok;
    		false -> {error, forbidden}
    	    end
    end.

-spec mk_error(term()) -> stanza_error().
mk_error(forbidden) ->
    xmpp:err_forbidden(<<"Denied by ACL">>, ?MYLANG);
mk_error(host_unknown) ->
    xmpp:err_not_allowed(<<"Host unknown">>, ?MYLANG);
mk_error({codec_error, Why}) ->
    xmpp:err_bad_request(xmpp:io_format_error(Why), ?MYLANG);
mk_error({_Class, _Reason} = Why) ->
    Txt = xmpp_stream_out:format_error(Why),
    xmpp:err_remote_server_not_found(Txt, ?MYLANG);
mk_error(_) ->
    xmpp:err_internal_server_error().

-spec format_error(db_result()) -> binary().
format_error(#db_result{type = invalid}) ->
    <<"invalid dialback key">>;
format_error(#db_result{type = error, sub_els = Els}) ->
    %% TODO: improve xmpp.erl
    case xmpp:get_error(#message{sub_els = Els}) of
	#stanza_error{} = Err ->
	    format_stanza_error(Err);
	undefined ->
	    <<"unrecognized error">>
    end;
format_error(_) ->
    <<"unexpected dialback result">>.

-spec format_stanza_error(stanza_error()) -> binary().
format_stanza_error(#stanza_error{reason = Reason, text = Txt}) ->
    Slogan = case Reason of
		 undefined -> <<"no reason">>;
		 #gone{} -> <<"gone">>;
		 #redirect{} -> <<"redirect">>;
		 _ -> erlang:atom_to_binary(Reason, latin1)
	     end,
    case Txt of
	undefined -> Slogan;
	#text{data = <<"">>} -> Slogan;
	#text{data = Data} ->
	    <<Data/binary, " (", Slogan/binary, ")">>
    end.
