%%%----------------------------------------------------------------------
%%% File    : mod_carboncopy.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : Message Carbons XEP-0280 0.8
%%% Created : 5 May 2008 by Mickael Remond <mremond@process-one.net>
%%% Usage   : Add the following line in modules section of ejabberd.yml:
%%%              {mod_carboncopy, []}
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
%%%----------------------------------------------------------------------
-module (mod_carboncopy).

-author ('ecestari@process-one.net').
-protocol({xep, 280, '1.0.1', '13.06', "complete", ""}).

-behaviour(gen_mod).

%% API:
-export([start/2, stop/1, reload/3]).

-export([user_send_packet/1, user_receive_packet/1,
	 iq_handler/1, disco_features/5,
	 depends/2, mod_options/1, mod_doc/0]).
-export([c2s_copy_session/2, c2s_session_opened/1, c2s_session_resumed/1,
	 c2s_inline_features/3, c2s_handle_bind2_inline/1]).
%% For debugging purposes
-export([list/2]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-type direction() :: sent | received.
-type c2s_state() :: ejabberd_c2s:state().

start(_Host, _Opts) ->
    {ok, [{hook, disco_local_features, disco_features, 50},
          %% why priority 89: to define clearly that we must run BEFORE mod_logdb hook (90)
          {hook, user_send_packet, user_send_packet, 89},
          {hook, user_receive_packet, user_receive_packet, 89},
          {hook, c2s_copy_session, c2s_copy_session, 50},
          {hook, c2s_session_resumed, c2s_session_resumed, 50},
          {hook, c2s_session_opened, c2s_session_opened, 50},
	  {hook, c2s_inline_features, c2s_inline_features, 50},
	  {hook, c2s_handle_bind2_inline, c2s_handle_bind2_inline, 50},
	  {iq_handler, ejabberd_sm, ?NS_CARBONS_2, iq_handler}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec disco_features({error, stanza_error()} | {result, [binary()]} | empty,
		     jid(), jid(), binary(), binary()) ->
			    {error, stanza_error()} | {result, [binary()]}.
disco_features(empty, From, To, <<"">>, Lang) ->
    disco_features({result, []}, From, To, <<"">>, Lang);
disco_features({result, Feats}, _From, _To, <<"">>, _Lang) ->
    {result, [?NS_CARBONS_2,?NS_CARBONS_RULES_0|Feats]};
disco_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec iq_handler(iq()) -> iq().
iq_handler(#iq{type = set, lang = Lang, from = From,
	       sub_els = [El]} = IQ) when is_record(El, carbons_enable);
					  is_record(El, carbons_disable) ->
    {U, S, R} = jid:tolower(From),
    Result = case El of
		 #carbons_enable{} -> enable(S, U, R, ?NS_CARBONS_2);
		 #carbons_disable{} -> disable(S, U, R)
	     end,
    case Result of
	ok ->
	    xmpp:make_iq_result(IQ);
	{error, _} ->
	    Txt = ?T("Database failure"),
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end;
iq_handler(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Only <enable/> or <disable/> tags are allowed"),
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
iq_handler(#iq{type = get, lang = Lang} = IQ)->
    Txt = ?T("Value 'get' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang)).

-spec user_send_packet({stanza(), ejabberd_c2s:state()})
      -> {stanza(), ejabberd_c2s:state()} | {stop, {stanza(), ejabberd_c2s:state()}}.
user_send_packet({#message{meta = #{carbon_copy := true}}, _C2SState} = Acc) ->
    %% Stop the hook chain, we don't want logging modules to duplicate this
    %% message.
    {stop, Acc};
user_send_packet({#message{from = From, to = To} = Msg, C2SState}) ->
    {check_and_forward(From, To, Msg, sent), C2SState};
user_send_packet(Acc) ->
    Acc.

-spec user_receive_packet({stanza(), ejabberd_c2s:state()})
      -> {stanza(), ejabberd_c2s:state()} | {stop, {stanza(), ejabberd_c2s:state()}}.
user_receive_packet({#message{meta = #{carbon_copy := true}}, _C2SState} = Acc) ->
    %% Stop the hook chain, we don't want logging modules to duplicate this
    %% message.
    {stop, Acc};
user_receive_packet({#message{to = To} = Msg, #{jid := JID} = C2SState}) ->
    {check_and_forward(JID, To, Msg, received), C2SState};
user_receive_packet(Acc) ->
    Acc.

-spec c2s_copy_session(c2s_state(), c2s_state()) -> c2s_state().
c2s_copy_session(State, #{user := U, server := S, resource := R}) ->
    case ejabberd_sm:get_user_info(U, S, R) of
	offline -> State;
	Info ->
	    case lists:keyfind(carboncopy, 1, Info) of
		{_, CC} -> State#{carboncopy => CC};
		false -> State
	    end
    end.

-spec c2s_session_resumed(c2s_state()) -> c2s_state().
c2s_session_resumed(#{user := U, server := S, resource := R,
		      carboncopy := CC} = State) ->
    ejabberd_sm:set_user_info(U, S, R, carboncopy, CC),
    maps:remove(carboncopy, State);
c2s_session_resumed(State) ->
    State.

-spec c2s_session_opened(c2s_state()) -> c2s_state().
c2s_session_opened(State) ->
    maps:remove(carboncopy, State).

c2s_inline_features({Sasl, Bind, Extra} = Acc, Host, _State) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
	true ->
	    {Sasl, [#bind2_feature{var = ?NS_CARBONS_2} | Bind], Extra};
	false ->
	    Acc
    end.

c2s_handle_bind2_inline({#{user := U, server := S, resource := R} = State, Els, Results}) ->
    case lists:keyfind(carbons_enable, 1, Els) of
	#carbons_enable{} ->
	    enable(S, U, R, ?NS_CARBONS_2),
	    {State, Els, Results};
	_ ->
	{State, Els, Results}
    end.

% Modified from original version:
%    - registered to the user_send_packet hook, to be called only once even for multicast
%    - do not support "private" message mode, and do not modify the original packet in any way
%    - we also replicate "read" notifications
-spec check_and_forward(jid(), jid(), message(), direction()) -> message().
check_and_forward(JID, To, Msg, Direction)->
    case (is_chat_message(Msg) orelse
	  is_received_muc_invite(Msg, Direction)) andalso
	not is_received_muc_pm(To, Msg, Direction) andalso
	not xmpp:has_subtag(Msg, #carbons_private{}) andalso
	not xmpp:has_subtag(Msg, #hint{type = 'no-copy'}) of
	true ->
	    send_copies(JID, To, Msg, Direction);
	false ->
	    ok
    end,
    Msg.

%%% Internal
%% Direction = received | sent <received xmlns='urn:xmpp:carbons:1'/>
-spec send_copies(jid(), jid(), message(), direction()) -> ok.
send_copies(JID, To, Msg, Direction)->
    {U, S, R} = jid:tolower(JID),
    PrioRes = ejabberd_sm:get_user_present_resources(U, S),
    {_, AvailRs} = lists:unzip(PrioRes),
    {MaxPrio, _MaxRes} = case catch lists:max(PrioRes) of
	{Prio, Res} -> {Prio, Res};
	_ -> {0, undefined}
    end,

    %% unavailable resources are handled like bare JIDs
    IsBareTo = case {Direction, To} of
	{received, #jid{lresource = <<>>}} -> true;
	{received, #jid{lresource = LRes}} -> not lists:member(LRes, AvailRs);
	_ -> false
    end,
    %% list of JIDs that should receive a carbon copy of this message (excluding the
    %% receiver(s) of the original message
    TargetJIDs = case {IsBareTo, Msg} of
	{true, #message{meta = #{sm_copy := true}}} ->
	    %% The message was sent to our bare JID, and we currently have
	    %% multiple resources with the same highest priority, so the session
	    %% manager routes the message to each of them. We create carbon
	    %% copies only from one of those resources in order to avoid
	    %% duplicates.
	    [];
	{true, _} ->
	    OrigTo = fun(Res) -> lists:member({MaxPrio, Res}, PrioRes) end,
	    [ {jid:make({U, S, CCRes}), CC_Version}
	     || {CCRes, CC_Version} <- list(U, S),
		lists:member(CCRes, AvailRs), not OrigTo(CCRes) ];
	{false, _} ->
	    [ {jid:make({U, S, CCRes}), CC_Version}
	     || {CCRes, CC_Version} <- list(U, S),
		lists:member(CCRes, AvailRs), CCRes /= R ]
	    %TargetJIDs = lists:delete(JID, [ jid:make({U, S, CCRes}) || CCRes <- list(U, S) ]),
    end,

    lists:foreach(
      fun({Dest, _Version}) ->
	      {_, _, Resource} = jid:tolower(Dest),
	      ?DEBUG("Sending:  ~p =/= ~p", [R, Resource]),
	      Sender = jid:make({U, S, <<>>}),
	      New = build_forward_packet(Msg, Sender, Dest, Direction),
	      ejabberd_router:route(xmpp:set_from_to(New, Sender, Dest))
      end, TargetJIDs).

-spec build_forward_packet(message(), jid(), jid(), direction()) -> message().
build_forward_packet(#message{type = T} = Msg, Sender, Dest, Direction) ->
    Forwarded = #forwarded{sub_els = [Msg]},
    Carbon = case Direction of
		 sent -> #carbons_sent{forwarded = Forwarded};
		 received -> #carbons_received{forwarded = Forwarded}
	     end,
    #message{from = Sender, to = Dest, type = T, sub_els = [Carbon],
	     meta = #{carbon_copy => true}}.

-spec enable(binary(), binary(), binary(), binary()) -> ok | {error, any()}.
enable(Host, U, R, CC)->
    ?DEBUG("Enabling carbons for ~ts@~ts/~ts", [U, Host, R]),
    case ejabberd_sm:set_user_info(U, Host, R, carboncopy, CC) of
	ok -> ok;
	{error, Reason} = Err ->
	    ?ERROR_MSG("Failed to enable carbons for ~ts@~ts/~ts: ~p",
		       [U, Host, R, Reason]),
	    Err
    end.

-spec disable(binary(), binary(), binary()) -> ok | {error, any()}.
disable(Host, U, R)->
    ?DEBUG("Disabling carbons for ~ts@~ts/~ts", [U, Host, R]),
    case ejabberd_sm:del_user_info(U, Host, R, carboncopy) of
	ok -> ok;
	{error, notfound} -> ok;
	{error, Reason} = Err ->
	    ?ERROR_MSG("Failed to disable carbons for ~ts@~ts/~ts: ~p",
		       [U, Host, R, Reason]),
	    Err
    end.

-spec is_chat_message(message()) -> boolean().
is_chat_message(#message{type = chat}) ->
    true;
is_chat_message(#message{type = normal, body = [_|_]}) ->
    true;
is_chat_message(#message{type = Type} = Msg) when Type == chat;
						  Type == normal ->
    has_chatstate(Msg) orelse xmpp:has_subtag(Msg, #receipt_response{});
is_chat_message(_) ->
    false.

-spec is_received_muc_invite(message(), direction()) -> boolean().
is_received_muc_invite(_Msg, sent) ->
    false;
is_received_muc_invite(Msg, received) ->
    case xmpp:get_subtag(Msg, #muc_user{}) of
	#muc_user{invites = [_|_]} ->
	    true;
	_ ->
	    xmpp:has_subtag(Msg, #x_conference{jid = jid:make(<<"">>)})
    end.

-spec is_received_muc_pm(jid(), message(), direction()) -> boolean().
is_received_muc_pm(#jid{lresource = <<>>}, _Msg, _Direction) ->
    false;
is_received_muc_pm(_To, _Msg, sent) ->
    false;
is_received_muc_pm(_To, Msg, received) ->
    xmpp:has_subtag(Msg, #muc_user{}).

-spec has_chatstate(message()) -> boolean().
has_chatstate(#message{sub_els = Els}) ->
    lists:any(fun(El) -> xmpp:get_ns(El) == ?NS_CHATSTATES end, Els).

-spec list(binary(), binary()) -> [{Resource :: binary(), Namespace :: binary()}].
list(User, Server) ->
    lists:filtermap(
      fun({Resource, Info}) ->
	      case lists:keyfind(carboncopy, 1, Info) of
		  {_, NS} -> {true, {Resource, NS}};
		  false -> false
	      end
      end, ejabberd_sm:get_user_info(User, Server)).

depends(_Host, _Opts) ->
    [].

mod_options(_) ->
    [].

mod_doc() ->
    #{desc =>
          ?T("The module implements https://xmpp.org/extensions/xep-0280.html"
             "[XEP-0280: Message Carbons]. "
             "The module broadcasts messages on all connected "
             "user resources (devices).")}.
