%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 12 Jul 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(xmpp_util).

%% API
-export([add_delay_info/3, add_delay_info/4, unwrap_carbon/1,
	 is_standalone_chat_state/1, get_xdata_values/2,
	 has_xdata_var/2, make_adhoc_response/1, make_adhoc_response/2,
	 decode_timestamp/1, encode_timestamp/1]).

-include("xmpp.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec add_delay_info(stanza(), jid(), erlang:timestamp()) -> stanza().
add_delay_info(Stz, From, Time) ->
    add_delay_info(Stz, From, Time, <<"">>).

-spec add_delay_info(stanza(), jid(),
		     erlang:timestamp(), binary()) -> stanza().

add_delay_info(Stz, From, Time, Desc) ->
    case xmpp:get_subtag(Stz, #delay{}) of
	#delay{from = OldFrom, desc = OldDesc} = Delay ->
	    case jid:tolower(From) == jid:tolower(OldFrom) of
		true when Desc == <<"">> ->
		    Stz;
		true when OldDesc == <<"">> ->
		    xmpp:set_subtag(Stz, Delay#delay{desc = Desc});
		true ->
		    case binary:match(OldDesc, Desc) of
			nomatch ->
			    NewDesc = <<OldDesc/binary, ", ", Desc/binary>>,
			    xmpp:set_subtag(Stz, Delay#delay{desc = NewDesc});
			_ ->
			    Stz
		    end;
		false ->
		    NewDelay = #delay{stamp = Time, from = From, desc = Desc},
		    xmpp:set_subtag(Stz, NewDelay)
	    end;
	false ->
	    Delay = #delay{stamp = Time, from = From, desc = Desc},
	    xmpp:set_subtag(Stz, Delay)
    end.

-spec unwrap_carbon(stanza()) -> xmpp_element().
unwrap_carbon(#message{} = Msg) ->
    case xmpp:get_subtag(Msg, #carbons_sent{}) of
	#carbons_sent{forwarded = #forwarded{sub_els = [El]}} ->
	    El;
	_ ->
	    case xmpp:get_subtag(Msg, #carbons_received{}) of
		#carbons_received{forwarded = #forwarded{sub_els = [El]}} ->
		    El;
		_ ->
		    Msg
	    end
    end;
unwrap_carbon(Stanza) -> Stanza.

-spec is_standalone_chat_state(stanza()) -> boolean().
is_standalone_chat_state(Stanza) ->
    case unwrap_carbon(Stanza) of
	#message{body = [], subject = [], sub_els = Els} ->
	    IgnoreNS = [?NS_CHATSTATES, ?NS_DELAY],
	    Stripped = [El || El <- Els,
			      not lists:member(xmpp:get_ns(El), IgnoreNS)],
	    Stripped == [];
	_ ->
	    false
    end.

-spec get_xdata_values(binary(), xdata()) -> [binary()].
get_xdata_values(Var, #xdata{fields = Fields}) ->
    case lists:keyfind(Var, #xdata_field.var, Fields) of
	#xdata_field{values = Vals} -> Vals;
	false -> []
    end.

-spec has_xdata_var(binary(), xdata()) -> boolean().
has_xdata_var(Var, #xdata{fields = Fields}) ->
    lists:keymember(Var, #xdata_field.var, Fields).

-spec make_adhoc_response(adhoc_command(), adhoc_command()) -> adhoc_command().
make_adhoc_response(#adhoc_command{lang = Lang, node = Node, sid = SID},
		    Command) ->
    Command#adhoc_command{lang = Lang, node = Node, sid = SID}.

-spec make_adhoc_response(adhoc_command()) -> adhoc_command().
make_adhoc_response(#adhoc_command{sid = <<"">>} = Command) ->
    SID = encode_timestamp(p1_time_compat:timestamp()),
    Command#adhoc_command{sid = SID};
make_adhoc_response(Command) ->
    Command.

-spec decode_timestamp(binary()) -> erlang:timestamp().
decode_timestamp(S) ->
    try try_decode_timestamp(S)
    catch _:_ -> erlang:error({bad_timestamp, S})
    end.

-spec encode_timestamp(erlang:timestamp()) -> binary().
encode_timestamp({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    Fraction = if MicroSecs > 0 ->
		       io_lib:format(".~6..0B", [MicroSecs]);
		  true ->
		       ""
	       end,
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT"
				 "~2..0B:~2..0B:~2..0B~sZ",
				 [Year, Month, Day, Hour, Minute, Second,
				  Fraction])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
try_decode_timestamp(<<Y:4/binary, $-, Mo:2/binary, $-, D:2/binary, $T,
		       H:2/binary, $:, Mi:2/binary, $:, S:2/binary, T/binary>>) ->
    Date = {to_integer(Y, 1970, 9999), to_integer(Mo, 1, 12), to_integer(D, 1, 31)},
    Time = {to_integer(H, 0, 23), to_integer(Mi, 0, 59), to_integer(S, 0, 59)},
    {MS, {TZH, TZM}} = try_decode_fraction(T),
    Seconds = calendar:datetime_to_gregorian_seconds({Date, Time}) -
	calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}) -
	TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS};
try_decode_timestamp(<<Y:4/binary, Mo:2/binary, D:2/binary, $T,
		       H:2/binary, $:, Mi:2/binary, $:, S:2/binary>>) ->
    try_decode_timestamp(<<Y:4/binary, $-, Mo:2/binary, $-, D:2/binary, $T,
			   H:2/binary, $:, Mi:2/binary, $:, S:2/binary, $Z>>).

try_decode_fraction(<<$., T/binary>>) ->
    {match, [V]} = re:run(T, <<"^[0-9]+">>, [{capture, [0], binary}]),
    Size = size(V),
    <<V:Size/binary, TZD/binary>> = T,
    {to_integer(binary:part(V, 0, min(6, Size)), 0, 999999),
     try_decode_tzd(TZD)};
try_decode_fraction(TZD) ->
    {0, try_decode_tzd(TZD)}.

try_decode_tzd(<<$Z>>) ->
    {0, 0};
try_decode_tzd(<<$-, H:2/binary, $:, M:2/binary>>) ->
    {-1 * to_integer(H, 0, 12), to_integer(M, 0, 59)};
try_decode_tzd(<<$+, H:2/binary, $:, M:2/binary>>) ->
    {to_integer(H, 0, 12), to_integer(M, 0, 59)}.

to_integer(S, Min, Max) ->
    case binary_to_integer(S) of
	I when I >= Min, I =< Max ->
	    I
    end.
