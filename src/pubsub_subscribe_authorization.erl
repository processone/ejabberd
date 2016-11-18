%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_subscribe_authorization.xdata
%% Form type: http://jabber.org/protocol/pubsub#subscribe_authorization
%% Document: XEP-0060

-module(pubsub_subscribe_authorization).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("pubsub_subscribe_authorization.hrl").

-export_type([property/0, result/0, form/0]).

dec_bool(<<"1">>) -> true;
dec_bool(<<"0">>) -> false;
dec_bool(<<"true">>) -> true;
dec_bool(<<"false">>) -> false.

enc_bool(true) -> <<"1">>;
enc_bool(false) -> <<"0">>.

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
      false ->
	  decode(Fs, Acc,
		 [<<"pubsub#allow">>, <<"pubsub#node">>,
		  <<"pubsub#subscriber_jid">>]);
      #xdata_field{values =
		       [<<"http://jabber.org/protocol/pubsub#subscribe_a"
			  "uthorization">>]} ->
	  decode(Fs, Acc,
		 [<<"pubsub#allow">>, <<"pubsub#node">>,
		  <<"pubsub#subscriber_jid">>]);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/pubsub#subscribe_a"
			   "uthorization">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {allow, Val} -> [encode_allow(Val, Translate)];
	    {allow, _, _} -> erlang:error({badarg, Opt});
	    {node, Val} -> [encode_node(Val, Translate)];
	    {node, _, _} -> erlang:error({badarg, Opt});
	    {subscriber_jid, Val} ->
		[encode_subscriber_jid(Val, Translate)];
	    {subscriber_jid, _, _} -> erlang:error({badarg, Opt});
	    {subid, Val} -> [encode_subid(Val, Translate)];
	    {subid, _, _} -> erlang:error({badarg, Opt});
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/pubsub#subscribe_a"
				   "uthorization">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"pubsub#allow">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allow, Result} | Acc],
		 lists:delete(<<"pubsub#allow">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#allow">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_a"
			   "uthorization">>}})
    end;
decode([#xdata_field{var = <<"pubsub#allow">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#allow">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#allow">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#allow">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_a"
		     "uthorization">>}});
decode([#xdata_field{var = <<"pubsub#node">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{node, Result} | Acc],
		 lists:delete(<<"pubsub#node">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#node">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_a"
			   "uthorization">>}})
    end;
decode([#xdata_field{var = <<"pubsub#node">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#node">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#node">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#node">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_a"
		     "uthorization">>}});
decode([#xdata_field{var = <<"pubsub#subscriber_jid">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_jid(Value) of
      Result ->
	  decode(Fs, [{subscriber_jid, Result} | Acc],
		 lists:delete(<<"pubsub#subscriber_jid">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#subscriber_jid">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_a"
			   "uthorization">>}})
    end;
decode([#xdata_field{var = <<"pubsub#subscriber_jid">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#subscriber_jid">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#subscriber_jid">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#subscriber_jid">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_a"
		     "uthorization">>}});
decode([#xdata_field{var = <<"pubsub#subid">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{subid, Result} | Acc],
		 lists:delete(<<"pubsub#subid">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#subid">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_a"
			   "uthorization">>}})
    end;
decode([#xdata_field{var = <<"pubsub#subid">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#subid">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#subid">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#subid">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_a"
		     "uthorization">>}});
decode([#xdata_field{var = Var} | Fs], Acc, Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE,
			 {unknown_var, Var,
			  <<"http://jabber.org/protocol/pubsub#subscribe_a"
			    "uthorization">>}});
       true -> decode(Fs, Acc, Required)
    end;
decode([], _, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var,
		   <<"http://jabber.org/protocol/pubsub#subscribe_a"
		     "uthorization">>}});
decode([], Acc, []) -> Acc.

encode_allow(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#allow">>, values = Values,
		 required = false, type = boolean, options = Opts,
		 desc = <<>>,
		 label =
		     Translate(<<"Allow this Jabber ID to subscribe to "
				 "this pubsub node?">>)}.

encode_node(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#node">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>, label = Translate(<<"Node ID">>)}.

encode_subscriber_jid(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_jid(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#subscriber_jid">>,
		 values = Values, required = false, type = 'jid-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Subscriber Address">>)}.

encode_subid(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#subid">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>,
		 label =
		     Translate(<<"The subscription identifier associated "
				 "with the subscription request">>)}.
