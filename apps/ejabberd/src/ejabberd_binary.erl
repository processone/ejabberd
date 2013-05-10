-module(ejabberd_binary).

-ifdef(no_binary_to_integer).

-export([binary_to_integer/1,
         integer_to_binary/1]).

binary_to_integer(B) ->
    catch list_to_integer(binary_to_list(B)).

integer_to_binary(I) ->
    catch list_to_binary(integer_to_list(I)).

-endif.
