-module(mod_bosh_backend).

-export([behaviour_info/1]).

%% TODO: define specific signatures using -callback attributes

-spec behaviour_info(atom()) -> undefined | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{start, 1},
     {create_session, 1},
     {delete_session, 1},
     {get_session, 1}];
behaviour_info(_Other) ->
    undefined.
