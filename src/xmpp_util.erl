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
	 has_xdata_var/2, make_adhoc_response/1, make_adhoc_response/2]).

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
	#message{sub_els = Els} ->
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
make_adhoc_response(#adhoc_command{sid = undefined} = Command) ->
    SID = jlib:now_to_utc_string(p1_time_compat:timestamp()),
    Command#adhoc_command{sid = SID};
make_adhoc_response(Command) ->
    Command.

%%%===================================================================
%%% Internal functions
%%%===================================================================
