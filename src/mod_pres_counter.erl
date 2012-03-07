%%%----------------------------------------------------------------------
%%% File    : mod_pres_counter.erl
%%% Author  : Ahmed Omar
%%% Purpose : Presence subscription flood prevention
%%% Created : 23 Sep 2010 by Ahmed Omar
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_pres_counter).

-behavior(gen_mod).

-export([start/2,
         stop/1,
	 check_packet/6]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(pres_counter, {dir, start, count, logged = false}).

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, _Opts) ->
    ejabberd_hooks:add(privacy_check_packet, HostB,
                       ?MODULE, check_packet, 25),
    ok.

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(privacy_check_packet, HostB,
                          ?MODULE, check_packet, 25),
    ok.

check_packet(_, _User, Server,
	     _PrivacyList,
	     {From, To, Stanza},
	     Dir) ->
    case exmpp_presence:is_presence(Stanza) of
        true ->
            IsSubscription =
                case exmpp_presence:get_type(Stanza) of
		    subscribe -> true;
		    subscribed -> true;
		    unsubscribe -> true;
		    unsubscribed -> true;
                    _ -> false
                end,
            if
                IsSubscription ->
                    JID = case Dir of
                              in -> To;
                              out -> From
                          end,
                    update(Server, JID, Dir);
                true ->
                    allow
            end;
        false ->
            allow
    end.

update(Server, JID, Dir) ->
    %% get options
    StormCount = gen_mod:get_module_opt(Server, ?MODULE, count, 5),
    TimeInterval = gen_mod:get_module_opt(Server, ?MODULE, interval, 60),
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    case read(Dir) of
        undefined ->
            write(Dir, #pres_counter{dir = Dir,
                                     start = TimeStamp,
                                     count = 1}),
            allow;
        #pres_counter{start = TimeStart, count = Count, logged = Logged} = R ->
            %% record for this key exists, check if we're
            %% within TimeInterval seconds, and whether the StormCount is
            %% high enough.  or else just increment the count.
            if
                TimeStamp - TimeStart > TimeInterval ->
                    write(Dir, R#pres_counter{
                                 start = TimeStamp,
                                 count = 1}),
                    allow;
                (Count =:= StormCount) and Logged ->
                    {stop, deny};
                Count =:= StormCount ->
                    write(Dir, R#pres_counter{logged = true}),
                    case Dir of
                        in ->
                            ?WARNING_MSG(
                               "User ~s is being flooded, "
                               "ignoring received presence subscriptions",
                               [exmpp_jid:to_list(JID)]);
                        out ->
                            {IP, _Port} = ejabberd_sm:get_user_ip(JID),
                            ?WARNING_MSG(
                               "Flooder detected: ~s, on IP: ~s "
                               "ignoring sent presence subscriptions",
                               [exmpp_jid:to_list(JID),
                                inet_parse:ntoa(IP)])
                    end,
                    {stop, deny};
                true ->
                    write(Dir, R#pres_counter{
                                 start = TimeStamp,
                                 count = Count + 1}),
                    allow
            end
    end.

read(K)->
    get({pres_counter, K}).

write(K, V)->
    put({pres_counter, K}, V).
