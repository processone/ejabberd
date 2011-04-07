-module(pubsub_clean).

-define(TIMEOUT, 1000*600). % 1 minute

-export([start/0, loop/0, purge/0, offline/1]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    register(pubsub_clean, Pid),
    Pid.

loop() ->
    receive
    purge -> purge()
    after ?TIMEOUT -> purge()
    end,
    loop().

purge() ->
    Sessions = lists:sum([mnesia:table_info(session,size)|[rpc:call(N,mnesia,table_info,[session,size]) || N <- nodes()]]),
    Subscriptions = mnesia:table_info(pubsub_state,size),
    if Subscriptions > Sessions + 500 ->
        lists:foreach(fun(K) ->
            [N]=mnesia:dirty_read({pubsub_node, K}),
            I=element(3,N),
            lists:foreach(fun(JID) ->
                case mnesia:dirty_read({pubsub_state, {JID, I}}) of
                    [{pubsub_state, K, _, _, _, [{subscribed,S}]}] -> mnesia:dirty_delete({pubsub_subscription, S});
                    _ -> ok
                end,
                mnesia:dirty_delete({pubsub_state, {JID, I}})
            end, offline(pubsub_debug:subscribed(I)))
        end, mnesia:dirty_all_keys(pubsub_node));
    true ->
        ok
    end.

offline(Jids) ->
    lists:filter(fun({U,S,""}) -> ejabberd_sm:get_user_resources(U,S) == [];
                    ({U,S,R}) -> not lists:member(R,ejabberd_sm:get_user_resources(U,S))
    end, Jids).
%%ejabberd_cluster:get_node({LUser, LServer})
