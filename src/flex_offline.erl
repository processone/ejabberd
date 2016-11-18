%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: flex_offline.xdata
%% Form type: http://jabber.org/protocol/offline
%% Document: XEP-0013

-module(flex_offline).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("flex_offline.hrl").

-export_type([property/0, result/0, form/0]).

dec_int(Val, Min, Max) ->
    case list_to_integer(binary_to_list(Val)) of
      Int when Int =< Max, Min == infinity -> Int;
      Int when Int =< Max, Int >= Min -> Int
    end.

enc_int(Int) -> integer_to_binary(Int).

format_error({form_type_mismatch, Type}) ->
    <<"FORM_TYPE doesn't match '", Type/binary, "'">>;
format_error({bad_var_value, Var, Type}) ->
    <<"Bad value of field '", Var/binary, "' of type '",
      Type/binary, "'">>;
format_error({missing_value, Var, Type}) ->
    <<"Missing value of field '", Var/binary, "' of type '",
      Type/binary, "'">>;
format_error({too_many_values, Var, Type}) ->
    <<"Too many values for field '", Var/binary,
      "' of type '", Type/binary, "'">>;
format_error({unknown_var, Var, Type}) ->
    <<"Unknown field '", Var/binary, "' of type '",
      Type/binary, "'">>;
format_error({missing_required_var, Var, Type}) ->
    <<"Missing required field '", Var/binary, "' of type '",
      Type/binary, "'">>.

decode(Fs) -> decode(Fs, []).

decode(Fs, Acc) ->
    case lists:keyfind(<<"FORM_TYPE">>, #xdata_field.var,
		       Fs)
	of
      false -> decode(Fs, Acc, []);
      #xdata_field{values =
		       [<<"http://jabber.org/protocol/offline">>]} ->
	  decode(Fs, Acc, []);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/offline">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {number_of_messages, Val} ->
		[encode_number_of_messages(Val, Translate)];
	    {number_of_messages, _, _} ->
		erlang:error({badarg, Opt});
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/offline">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"number_of_messages">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_int(Value, 0, infinity) of
      Result ->
	  decode(Fs, [{number_of_messages, Result} | Acc],
		 lists:delete(<<"number_of_messages">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"number_of_messages">>,
			 <<"http://jabber.org/protocol/offline">>}})
    end;
decode([#xdata_field{var = <<"number_of_messages">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"number_of_messages">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"number_of_messages">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"number_of_messages">>,
		   <<"http://jabber.org/protocol/offline">>}});
decode([#xdata_field{var = Var} | Fs], Acc, Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE,
			 {unknown_var, Var,
			  <<"http://jabber.org/protocol/offline">>}});
       true -> decode(Fs, Acc, Required)
    end;
decode([], _, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var,
		   <<"http://jabber.org/protocol/offline">>}});
decode([], Acc, []) -> Acc.

encode_number_of_messages(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_int(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"number_of_messages">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Number of Offline Messages">>)}.
