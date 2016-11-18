%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: mam_query.xdata
%% Form type: urn:xmpp:mam:1
%% Document: XEP-0313

-module(mam_query).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("mam_query.hrl").

-export_type([property/0, result/0, form/0]).

enc_jid(J) -> jid:to_string(J).

dec_jid(Val) ->
    case jid:from_string(Val) of
      error -> erlang:error(badarg);
      J -> J
    end.

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
      #xdata_field{values = [<<"urn:xmpp:mam:1">>]} ->
	  decode(Fs, Acc, []);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch, <<"urn:xmpp:mam:1">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {with, Val} -> [encode_with(Val, Translate)];
	    {with, _, _} -> erlang:error({badarg, Opt});
	    {start, Val} -> [encode_start(Val, Translate)];
	    {start, _, _} -> erlang:error({badarg, Opt});
	    {'end', Val} -> [encode_end(Val, Translate)];
	    {'end', _, _} -> erlang:error({badarg, Opt});
	    {withtext, Val} -> [encode_withtext(Val, Translate)];
	    {withtext, _, _} -> erlang:error({badarg, Opt});
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden, values = [<<"urn:xmpp:mam:1">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"with">>, values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_jid(Value) of
      Result ->
	  decode(Fs, [{with, Result} | Acc],
		 lists:delete(<<"with">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"with">>, <<"urn:xmpp:mam:1">>}})
    end;
decode([#xdata_field{var = <<"with">>, values = []} = F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"with">>, values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"with">>} | _], _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"with">>, <<"urn:xmpp:mam:1">>}});
decode([#xdata_field{var = <<"start">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try xmpp_util:decode_timestamp(Value) of
      Result ->
	  decode(Fs, [{start, Result} | Acc],
		 lists:delete(<<"start">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"start">>, <<"urn:xmpp:mam:1">>}})
    end;
decode([#xdata_field{var = <<"start">>, values = []} = F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"start">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"start">>} | _], _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"start">>, <<"urn:xmpp:mam:1">>}});
decode([#xdata_field{var = <<"end">>, values = [Value]}
	| Fs],
       Acc, Required) ->
    try xmpp_util:decode_timestamp(Value) of
      Result ->
	  decode(Fs, [{'end', Result} | Acc],
		 lists:delete(<<"end">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"end">>, <<"urn:xmpp:mam:1">>}})
    end;
decode([#xdata_field{var = <<"end">>, values = []} = F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"end">>, values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"end">>} | _], _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"end">>, <<"urn:xmpp:mam:1">>}});
decode([#xdata_field{var = <<"withtext">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{withtext, Result} | Acc],
		 lists:delete(<<"withtext">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"withtext">>, <<"urn:xmpp:mam:1">>}})
    end;
decode([#xdata_field{var = <<"withtext">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"withtext">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"withtext">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"withtext">>,
		   <<"urn:xmpp:mam:1">>}});
decode([#xdata_field{var = Var} | Fs], Acc, Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE,
			 {unknown_var, Var, <<"urn:xmpp:mam:1">>}});
       true -> decode(Fs, Acc, Required)
    end;
decode([], _, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var, <<"urn:xmpp:mam:1">>}});
decode([], Acc, []) -> Acc.

encode_with(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_jid(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"with">>, values = Values,
		 required = false, type = 'jid-single', options = Opts,
		 desc = <<>>, label = Translate(<<"User JID">>)}.

encode_start(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"start">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>,
		 label = Translate(<<"Search from the date">>)}.

encode_end(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"end">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>,
		 label = Translate(<<"Search until the date">>)}.

encode_withtext(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"withtext">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>, label = Translate(<<"Search the text">>)}.
