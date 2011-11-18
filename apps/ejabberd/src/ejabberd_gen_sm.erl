-module(ejabberd_gen_sm).

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{start, 1},
     {get_sessions, 2},
     {get_sessions, 3},
     {create_session, 4},
     {update_session, 4},
     {delete_session, 4},
     {cleanup, 1},
     {count, 0}];
behaviour_info(_Other) ->
    undefined.
