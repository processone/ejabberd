-include_lib("common_test/include/ct.hrl").
-include_lib("fast_xml/include/fxml.hrl").
-include("ns.hrl").
-include("ejabberd.hrl").
-include("mod_proxy65.hrl").
-include("xmpp_codec.hrl").

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(PUBSUB(Node), <<(?NS_PUBSUB)/binary, "#", Node>>).

-define(EJABBERD_CT_URI, <<"http://www.process-one.net/en/ejabberd_ct/">>).

-define(recv1(P1),
        P1 = (fun() ->
                 V = recv(Config),
                 case V of
                     P1 -> V;
                     _ -> suite:match_failure([V], [??P1])
                 end
         end)()).

-define(recv2(P1, P2),
        (fun() ->
                 case {R1 = recv(Config), R2 = recv(Config)} of
                     {P1, P2} -> {R1, R2};
                     {P2, P1} -> {R2, R1};
                     {P1, V1} -> suite:match_failure([V1], [P2]);
                     {P2, V2} -> suite:match_failure([V2], [P1]);
                     {V3, P1} -> suite:match_failure([V3], [P2]);
                     {V4, P2} -> suite:match_failure([V4], [P1]);
                     {V5, V6} -> suite:match_failure([V5, V6], [P1, P2])
                 end
         end)()).

-define(recv3(P1, P2, P3),
        (fun() ->
                 case R3 = recv(Config) of
                     P1 -> insert(R3, 1, ?recv2(P2, P3));
                     P2 -> insert(R3, 2, ?recv2(P1, P3));
                     P3 -> insert(R3, 3, ?recv2(P1, P2));
                     V -> suite:match_failure([V], [P1, P2, P3])
                 end
         end)()).

-define(recv4(P1, P2, P3, P4),
        (fun() ->
                 case R4 = recv(Config) of
                     P1 -> insert(R4, 1, ?recv3(P2, P3, P4));
                     P2 -> insert(R4, 2, ?recv3(P1, P3, P4));
                     P3 -> insert(R4, 3, ?recv3(P1, P2, P4));
                     P4 -> insert(R4, 4, ?recv3(P1, P2, P3));
                     V -> suite:match_failure([V], [P1, P2, P3, P4])
                 end
         end)()).

-define(recv5(P1, P2, P3, P4, P5),
        (fun() ->
                 case R5 = recv(Config) of
                     P1 -> insert(R5, 1, ?recv4(P2, P3, P4, P5));
                     P2 -> insert(R5, 2, ?recv4(P1, P3, P4, P5));
                     P3 -> insert(R5, 3, ?recv4(P1, P2, P4, P5));
                     P4 -> insert(R5, 4, ?recv4(P1, P2, P3, P5));
                     P5 -> insert(R5, 5, ?recv4(P1, P2, P3, P4));
                     V -> suite:match_failure([V], [P1, P2, P3, P4, P5])
                 end
         end)()).

-define(match(Pattern, Result),
	case Result of
	    Pattern ->
		Pattern;
	    Mismatch ->
		suite:match_failure([Mismatch], [??Pattern])
	end).

-define(COMMON_VHOST, <<"localhost">>).
-define(MNESIA_VHOST, <<"mnesia.localhost">>).
-define(REDIS_VHOST, <<"redis.localhost">>).
-define(MYSQL_VHOST, <<"mysql.localhost">>).
-define(PGSQL_VHOST, <<"pgsql.localhost">>).
-define(SQLITE_VHOST, <<"sqlite.localhost">>).
-define(LDAP_VHOST, <<"ldap.localhost">>).
-define(EXTAUTH_VHOST, <<"extauth.localhost">>).
-define(RIAK_VHOST, <<"riak.localhost">>).
-define(S2S_VHOST, <<"s2s.localhost">>).

insert(Val, N, Tuple) ->
    L = tuple_to_list(Tuple),
    {H, T} = lists:split(N-1, L),
    list_to_tuple(H ++ [Val|T]).
