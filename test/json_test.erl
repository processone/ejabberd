-module(json_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% @format-begin

encode_binary_test() ->
    Binary = <<"This is an error text.">>,
    Encoded = <<"\"This is an error text.\"">>,
    ?assertMatch(Encoded, misc:json_encode(Binary)).

-ifdef(OTP_BELOW_26).

%% OTP 25 or lower
encode_map_test() ->
    Map = #{name => <<"room">>,
            service => <<"conference">>,
            jid => jid:encode({<<"user">>, <<"server">>, <<"">>}),
            affiliation => member},
    Encoded =
        <<"{\"service\":\"conference\",\"name\":\"room\",\"jid\":\"user@server\",\"affiliation\":\"member\"}">>,
    ?assertMatch(Encoded, misc:json_encode(Map)).

-endif.

-ifdef(OTP_BELOW_27).

-ifndef(OTP_BELOW_26).

%% OTP 26
encode_map_test() ->
    Map = #{name => <<"room">>,
            service => <<"conference">>,
            jid => jid:encode({<<"user">>, <<"server">>, <<"">>}),
            affiliation => member},
    Encoded =
        <<"{\"affiliation\":\"member\",\"jid\":\"user@server\",\"service\":\"conference\",\"name\":\"room\"}">>,
    ?assertMatch(Encoded, misc:json_encode(Map)).

-endif.
-endif.

-ifndef(OTP_BELOW_27).

%% OTP 27 or higher or higher
encode_map_test() ->
    Map = #{name => <<"room">>,
            service => <<"conference">>,
            jid => jid:encode({<<"user">>, <<"server">>, <<"">>}),
            affiliation => member},
    Encoded27 =
        <<"{\"name\":\"room\",\"service\":\"conference\",\"jid\":\"user@server\",\"affiliation\":\"member\"}">>,
    ?assertMatch(Encoded27, misc:json_encode(Map)).

-endif.

decode_test() ->
    Encoded =
        <<"{\"affiliation\":\"member\",\"jid\":\"user@server\",\"service\":\"conference\",\"name\":\"room\"}">>,
    TupleList =
        #{<<"affiliation">> => <<"member">>,
          <<"jid">> => <<"user@server">>,
          <<"name">> => <<"room">>,
          <<"service">> => <<"conference">>},
    ?assertMatch(TupleList, misc:json_decode(Encoded)).

decode_maps_test() ->
    Encoded =
        <<"{\"affiliation\":\"member\",\"jid\":\"user@server\",\"service\":\"conference\",\"name\":\"room\"}">>,
    Map = #{<<"affiliation">> => misc:atom_to_binary(member),
            <<"jid">> => jid:encode({<<"user">>, <<"server">>, <<"">>}),
            <<"name">> => <<"room">>,
            <<"service">> => <<"conference">>},
    ?assertMatch(Map, misc:json_decode(Encoded)).
