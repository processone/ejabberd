-include_lib("common_test/include/ct.hrl").
-include("xml.hrl").
-include("ns.hrl").
-include("ejabberd.hrl").
-include("mod_proxy65.hrl").
-include("xmpp_codec.hrl").

-define(STREAM_HEADER,
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns:stream='http://etherx.jabber.org/stream"
	  "s' xmlns='jabber:client' to='~s' version='1.0"
	  "'>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(PUBSUB(Node), <<(?NS_PUBSUB)/binary, "#", Node>>).

-define(recv2(P1, P2),
        (fun() ->
                 case {R1 = recv(), R2 = recv()} of
                     {P1, P2} -> {R1, R2};
                     {P2, P1} -> {R2, R1}
                 end
         end)()).

-define(recv3(P1, P2, P3),
        (fun() ->
                 case R3 = recv() of
                     P1 -> insert(R3, 1, ?recv2(P2, P3));
                     P2 -> insert(R3, 2, ?recv2(P1, P3));
                     P3 -> insert(R3, 3, ?recv2(P1, P2))
                 end
         end)()).

-define(recv4(P1, P2, P3, P4),
        (fun() ->
                 case R4 = recv() of
                     P1 -> insert(R4, 1, ?recv3(P2, P3, P4));
                     P2 -> insert(R4, 2, ?recv3(P1, P3, P4));
                     P3 -> insert(R4, 3, ?recv3(P1, P2, P4));
                     P4 -> insert(R4, 4, ?recv3(P1, P2, P3))
                 end
         end)()).

-define(recv5(P1, P2, P3, P4, P5),
        (fun() ->
                 case R5 = recv() of
                     P1 -> insert(R5, 1, ?recv4(P2, P3, P4, P5));
                     P2 -> insert(R5, 2, ?recv4(P1, P3, P4, P5));
                     P3 -> insert(R5, 3, ?recv4(P1, P2, P4, P5));
                     P4 -> insert(R5, 4, ?recv4(P1, P2, P3, P5));
                     P5 -> insert(R5, 5, ?recv4(P1, P2, P3, P4))
                 end
         end)()).

-define(COMMON_VHOST, <<"localhost">>).
-define(MNESIA_VHOST, <<"mnesia.localhost">>).
-define(MYSQL_VHOST, <<"mysql.localhost">>).
-define(PGSQL_VHOST, <<"pgsql.localhost">>).
-define(LDAP_VHOST, <<"ldap.localhost">>).
-define(EXTAUTH_VHOST, <<"extauth.localhost">>).

insert(Val, N, Tuple) ->
    L = tuple_to_list(Tuple),
    {H, T} = lists:split(N-1, L),
    list_to_tuple(H ++ [Val|T]).
