%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_publish_options.xdata
%% Form type: http://jabber.org/protocol/pubsub#publish-options
%% Document: XEP-0060

-module(pubsub_publish_options).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("pubsub_publish_options.hrl").

-export_type([property/0, result/0, form/0]).

dec_enum(Val, Enums) ->
    AtomVal = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(AtomVal, Enums) of
      true -> AtomVal
    end.

enc_enum(Atom) -> erlang:atom_to_binary(Atom, utf8).

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
		       [<<"http://jabber.org/protocol/pubsub#publish-opt"
			  "ions">>]} ->
	  decode(Fs, Acc, []);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/pubsub#publish-opt"
			   "ions">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {access_model, Val} ->
		[encode_access_model(Val, default, Translate)];
	    {access_model, Val, Opts} ->
		[encode_access_model(Val, Opts, Translate)];
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/pubsub#publish-opt"
				   "ions">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"pubsub#access_model">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value,
		 [authorize, open, presence, roster, whitelist])
    of
      Result ->
	  decode(Fs, [{access_model, Result} | Acc],
		 lists:delete(<<"pubsub#access_model">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#access_model">>,
			 <<"http://jabber.org/protocol/pubsub#publish-opt"
			   "ions">>}})
    end;
decode([#xdata_field{var = <<"pubsub#access_model">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#access_model">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#access_model">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#access_model">>,
		   <<"http://jabber.org/protocol/pubsub#publish-opt"
		     "ions">>}});
decode([#xdata_field{var = Var} | Fs], Acc, Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE,
			 {unknown_var, Var,
			  <<"http://jabber.org/protocol/pubsub#publish-opt"
			    "ions">>}});
       true -> decode(Fs, Acc, Required)
    end;
decode([], _, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var,
		   <<"http://jabber.org/protocol/pubsub#publish-opt"
		     "ions">>}});
decode([], Acc, []) -> Acc.

encode_access_model(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"Access model of authorize">>),
				 value = <<"authorize">>},
		   #xdata_option{label =
				     Translate(<<"Access model of open">>),
				 value = <<"open">>},
		   #xdata_option{label =
				     Translate(<<"Access model of presence">>),
				 value = <<"presence">>},
		   #xdata_option{label =
				     Translate(<<"Access model of roster">>),
				 value = <<"roster">>},
		   #xdata_option{label =
				     Translate(<<"Access model of whitelist">>),
				 value = <<"whitelist">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#access_model">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Specify the access model">>)}.
