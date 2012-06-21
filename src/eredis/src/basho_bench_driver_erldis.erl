-module(basho_bench_driver_erldis).

-export([new/1,
         run/4]).

new(_Id) ->
    case erldis_client:connect() of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

run(get, KeyGen, _ValueGen, Client) ->
    Start = KeyGen(),
    case erldis:mget(Client, lists:seq(Start, Start + 500)) of
        {error, Reason} ->
            {error, Reason, Client};
        _Value ->
            {ok, Client}
    end;

run(put, KeyGen, ValueGen, Client) ->
    case erldis:set(Client, integer_to_list(KeyGen()), ValueGen()) of
        {error, Reason} ->
            {error, Reason, Client};
        _Value ->
            {ok, Client}
    end.
