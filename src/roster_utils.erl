-module(roster_utils).

-export([mnesia2redis/0]).

-include("mod_roster.hrl").

mnesia2redis() ->
    lists:foreach(fun(Id) ->
        case mnesia:dirty_read(roster, Id) of
            [Item] when is_record(Item, roster) ->
                {LUser, LServer, {JUser, JServer, JResource} = LJID} = Item#roster.usj,
                Key = LUser ++ ":" ++ LServer ++ ":roster:" ++ jlib:jid_to_string(LJID),
                ejabberd_redis:multi([
                    ["SADD", Key],
                    ["SET", Key ++ ":juser", JUser],
                    ["SET", Key ++ ":jserver", JServer],
                    ["SET", Key ++ ":jresource", JResource],
                    ["SET", Key ++ ":name", Item#roster.name],
                    ["SET", Key ++ ":ask", erlang:atom_to_list(Item#roster.ask)],
                    ["SET", Key ++ ":askmessage", to_list(Item#roster.askmessage)],
                    ["SET", Key ++ ":xs", Item#roster.xs] |
                    [ ["SADD", Key ++ ":groups", X ] || X <- Item#roster.groups ]
                ]);
            _ -> ok
        end
    end, mnesia:dirty_all_keys(roster)).

to_list(Dato) when is_list(Dato) -> Dato;
to_list(Dato) when is_binary(Dato) -> erlang:binary_to_list(Dato);
to_list(Dato) when is_integer(Dato) -> erlang:integer_to_list(Dato);
to_list(Dato) when is_float(Dato) -> erlang:float_to_list(Dato);
to_list(Dato) when is_atom(Dato) -> erlang:atom_to_list(Dato).

