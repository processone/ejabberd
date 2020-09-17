-module(configure_deps).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = configure_deps_prv:init(State),
    {ok, State1}.
