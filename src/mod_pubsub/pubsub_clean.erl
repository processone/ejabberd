-module(pubsub_clean).

-define(TIMEOUT, 1000*600). % 1 minute

-export([start/0, loop/0, subscribed/1, offline/1]).

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
    {Sessions, Subscriptions} = {mnesia:table_info(session,size),mnesia:table_info(pubsub_state,size)},
    if Subscriptions > Sessions + 500 ->
        lists:foreach(fun(K) ->
            [N]=mnesia:dirty_read({pubsub_node, K}),
            I=element(3,N),
            lists:foreach(fun(JID) ->
                mnesia:dirty_delete({pubsub_state, {JID, I}})
            end, offline(subscribed(I)))
        end, mnesia:dirty_all_keys(pubsub_node));
    true ->
        ok
    end.

subscribed(NodeId) ->
    lists:map(fun(S) ->
        element(1,element(2,S))
    end, mnesia:dirty_match_object({pubsub_state, {'_',NodeId},'_',none,subscribed})).

offline(Jids) ->
    lists:filter(fun({U,S,""}) -> ejabberd_sm:get_user_resources(U,S) == [];
                    ({U,S,R}) -> not lists:member(R,ejabberd_sm:get_user_resources(U,S))
    end, Jids).
