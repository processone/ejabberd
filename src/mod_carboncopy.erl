%%%----------------------------------------------------------------------
%%% File    : mod_carboncopy.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose : Message Carbons XEP-0280 0.8
%%% Created : 5 May 2008 by Mickael Remond <mremond@process-one.net>
%%% Usage   : Add the following line in modules section of ejabberd.yml:
%%%              {mod_carboncopy, []}
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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
-protocol({xep, 280, '0.8'}).

-behavior(gen_mod).

%% API:
-export([start/2,
         stop/1]).

-export([user_send_packet/4, user_receive_packet/5,
	 iq_handler/1, remove_connection/4,
	 is_carbon_copy/1, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-define(PROCNAME, ?MODULE).

-type direction() :: sent | received.

-callback init(binary(), gen_mod:opts()) -> any().
-callback enable(binary(), binary(), binary(), binary()) -> ok | {error, any()}.
-callback disable(binary(), binary(), binary()) -> ok | {error, any()}.
-callback list(binary(), binary()) -> [{binary(), binary()}].

-spec is_carbon_copy(stanza()) -> boolean().
is_carbon_copy(Packet) ->
    xmpp:has_subtag(Packet, #carbons_sent{}) orelse
	xmpp:has_subtag(Packet, #carbons_received{}).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts,fun gen_iq_handler:check_type/1, one_queue),
    mod_disco:register_feature(Host, ?NS_CARBONS_2),
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:init(Host, Opts),
    ejabberd_hooks:add(unset_presence_hook,Host, ?MODULE, remove_connection, 10),
    %% why priority 89: to define clearly that we must run BEFORE mod_logdb hook (90)
    ejabberd_hooks:add(user_send_packet,Host, ?MODULE, user_send_packet, 89),
    ejabberd_hooks:add(user_receive_packet,Host, ?MODULE, user_receive_packet, 89),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CARBONS_2, ?MODULE, iq_handler, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_CARBONS_2),
    mod_disco:unregister_feature(Host, ?NS_CARBONS_2),
    %% why priority 89: to define clearly that we must run BEFORE mod_logdb hook (90)
    ejabberd_hooks:delete(user_send_packet,Host, ?MODULE, user_send_packet, 89),
    ejabberd_hooks:delete(user_receive_packet,Host, ?MODULE, user_receive_packet, 89),
    ejabberd_hooks:delete(unset_presence_hook,Host, ?MODULE, remove_connection, 10).

-spec iq_handler(iq()) -> iq().
iq_handler(#iq{type = set, lang = Lang, from = From,
	       sub_els = [El]} = IQ) when is_record(El, carbons_enable);
					  is_record(El, carbons_disable) ->
    {U, S, R} = jid:tolower(From),
    Result = case El of
		 #carbons_enable{} ->
		     ?INFO_MSG("carbons enabled for user ~s@~s/~s", [U,S,R]),
		     enable(S, U, R, ?NS_CARBONS_2);
		 #carbons_disable{} ->
		     ?INFO_MSG("carbons disabled for user ~s@~s/~s", [U,S,R]),
		     disable(S, U, R)
	     end,
    case Result of
	ok ->
	    ?DEBUG("carbons IQ result: ok", []),
	    xmpp:make_iq_result(IQ);
	{error,_Error} ->
	    ?ERROR_MSG("Error enabling / disabling carbons: ~p", [Result]),
	    Txt = <<"Database failure">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end;
iq_handler(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Only <enable/> or <disable/> tags are allowed">>,
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
iq_handler(#iq{type = get, lang = Lang} = IQ)->
    Txt = <<"Value 'get' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang)).

-spec user_send_packet(stanza(), ejabberd_c2s:state(), jid(), jid()) ->
			      stanza() | {stop, stanza()}.
user_send_packet(Packet, _C2SState, From, To) ->
    check_and_forward(From, To, Packet, sent).

-spec user_receive_packet(stanza(), ejabberd_c2s:state(),
			  jid(), jid(), jid()) ->
				 stanza() | {stop, stanza()}.
user_receive_packet(Packet, _C2SState, JID, _From, To) ->
    check_and_forward(JID, To, Packet, received).

% Modified from original version:
%    - registered to the user_send_packet hook, to be called only once even for multicast
%    - do not support "private" message mode, and do not modify the original packet in any way
%    - we also replicate "read" notifications
-spec check_and_forward(jid(), jid(), stanza(), direction()) ->
			       stanza() | {stop, stanza()}.
check_and_forward(JID, To, Packet, Direction)->
    case is_chat_message(Packet) andalso
	xmpp:has_subtag(Packet, #carbons_private{}) == false andalso
	xmpp:has_subtag(Packet, #hint{type = 'no-copy'}) == false of
	true ->
	    case is_carbon_copy(Packet) of
		false ->
		    send_copies(JID, To, Packet, Direction),
		    Packet;
		true ->
		    %% stop the hook chain, we don't want logging modules to duplicates
		    %% this message
		    {stop, Packet}
	    end;
        _ ->
	    Packet
    end.

-spec remove_connection(binary(), binary(), binary(), binary()) -> ok.
remove_connection(User, Server, Resource, _Status)->
    disable(Server, User, Resource),
    ok.


%%% Internal
%% Direction = received | sent <received xmlns='urn:xmpp:carbons:1'/>
-spec send_copies(jid(), jid(), message(), direction()) -> ok.
send_copies(JID, To, Packet, Direction)->
    {U, S, R} = jid:tolower(JID),
    PrioRes = ejabberd_sm:get_user_present_resources(U, S),
    {_, AvailRs} = lists:unzip(PrioRes),
    {MaxPrio, MaxRes} = case catch lists:max(PrioRes) of
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
    TargetJIDs = case {IsBareTo, R} of
	{true, MaxRes} ->
	    OrigTo = fun(Res) -> lists:member({MaxPrio, Res}, PrioRes) end,
	    [ {jid:make({U, S, CCRes}), CC_Version}
	     || {CCRes, CC_Version} <- list(U, S),
		lists:member(CCRes, AvailRs), not OrigTo(CCRes) ];
	{true, _} ->
	    %% The message was sent to our bare JID, and we currently have
	    %% multiple resources with the same highest priority, so the session
	    %% manager routes the message to each of them. We create carbon
	    %% copies only from one of those resources (the one where R equals
	    %% MaxRes) in order to avoid duplicates.
	    [];
	{false, _} ->
	    [ {jid:make({U, S, CCRes}), CC_Version}
	     || {CCRes, CC_Version} <- list(U, S),
		lists:member(CCRes, AvailRs), CCRes /= R ]
	    %TargetJIDs = lists:delete(JID, [ jid:make({U, S, CCRes}) || CCRes <- list(U, S) ]),
    end,

    lists:map(fun({Dest, _Version}) ->
		    {_, _, Resource} = jid:tolower(Dest),
		    ?DEBUG("Sending:  ~p =/= ~p", [R, Resource]),
		    Sender = jid:make({U, S, <<>>}),
		    %{xmlelement, N, A, C} = Packet,
		    New = build_forward_packet(JID, Packet, Sender, Dest, Direction),
		    ejabberd_router:route(Sender, Dest, New)
	      end, TargetJIDs),
    ok.

-spec build_forward_packet(jid(), message(), jid(), jid(), direction()) -> message().
build_forward_packet(JID, #message{type = T} = Msg, Sender, Dest, Direction) ->
    Forwarded = #forwarded{sub_els = [complete_packet(JID, Msg, Direction)]},
    Carbon = case Direction of
		 sent -> #carbons_sent{forwarded = Forwarded};
		 received -> #carbons_received{forwarded = Forwarded}
	     end,
    #message{from = Sender, to = Dest, type = T, sub_els = [Carbon]}.

-spec enable(binary(), binary(), binary(), binary()) -> ok | {error, any()}.
enable(Host, U, R, CC)->
    ?DEBUG("enabling for ~p", [U]),
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:enable(U, Host, R, CC).

-spec disable(binary(), binary(), binary()) -> ok | {error, any()}.
disable(Host, U, R)->
    ?DEBUG("disabling for ~p", [U]),
    Mod = gen_mod:db_mod(Host, ?MODULE),
    Mod:disable(U, Host, R).

-spec complete_packet(jid(), message(), direction()) -> message().
complete_packet(From, #message{from = undefined} = Msg, sent) ->
    %% if this is a packet sent by user on this host, then Packet doesn't
    %% include the 'from' attribute. We must add it.
    Msg#message{from = From};
complete_packet(_From, Msg, _Direction) ->
    Msg.

-spec is_chat_message(stanza()) -> boolean().
is_chat_message(#message{type = chat}) ->
    true;
is_chat_message(#message{type = normal, body = Body}) ->
    xmpp:get_text(Body) /= <<"">>;
is_chat_message(_) ->
    false.

-spec list(binary(), binary()) -> [{binary(), binary()}].
%% list {resource, cc_version} with carbons enabled for given user and host
list(User, Server) ->
    Mod = gen_mod:db_mod(Server, ?MODULE),
    Mod:list(User, Server).

depends(_Host, _Opts) ->
    [].

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(_) -> [db_type, iqdisc].
