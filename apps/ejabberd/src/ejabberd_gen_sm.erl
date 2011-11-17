-module(ejabberd_gen_sm).

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{start, 1},
     {set_session, 4},
     {update_session, 4},
     {get_session, 2},
     {get_session, 3},
     {delete_session, 3},
     {cleanup, 1},
     {count, 0}];
behaviour_info(_Other) ->
    undefined.
