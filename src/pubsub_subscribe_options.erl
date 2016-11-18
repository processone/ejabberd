%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_subscribe_options.xdata
%% Form type: http://jabber.org/protocol/pubsub#subscribe_options
%% Document: XEP-0060

-module(pubsub_subscribe_options).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("pubsub_subscribe_options.hrl").

-export_type([property/0, result/0, form/0]).

dec_enum(Val, Enums) ->
    AtomVal = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(AtomVal, Enums) of
      true -> AtomVal
    end.

enc_enum(Atom) -> erlang:atom_to_binary(Atom, utf8).

dec_bool(<<"1">>) -> true;
dec_bool(<<"0">>) -> false;
dec_bool(<<"true">>) -> true;
dec_bool(<<"false">>) -> false.

enc_bool(true) -> <<"1">>;
enc_bool(false) -> <<"0">>.

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
		       [<<"http://jabber.org/protocol/pubsub#subscribe_o"
			  "ptions">>]} ->
	  decode(Fs, Acc, []);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {deliver, Val} -> [encode_deliver(Val, Translate)];
	    {deliver, _, _} -> erlang:error({badarg, Opt});
	    {digest, Val} -> [encode_digest(Val, Translate)];
	    {digest, _, _} -> erlang:error({badarg, Opt});
	    {digest_frequency, Val} ->
		[encode_digest_frequency(Val, Translate)];
	    {digest_frequency, _, _} -> erlang:error({badarg, Opt});
	    {expire, Val} -> [encode_expire(Val, Translate)];
	    {expire, _, _} -> erlang:error({badarg, Opt});
	    {include_body, Val} ->
		[encode_include_body(Val, Translate)];
	    {include_body, _, _} -> erlang:error({badarg, Opt});
	    {'show-values', Val} ->
		['encode_show-values'(Val, default, Translate)];
	    {'show-values', Val, Opts} ->
		['encode_show-values'(Val, Opts, Translate)];
	    {subscription_type, Val} ->
		[encode_subscription_type(Val, default, Translate)];
	    {subscription_type, Val, Opts} ->
		[encode_subscription_type(Val, Opts, Translate)];
	    {subscription_depth, Val} ->
		[encode_subscription_depth(Val, default, Translate)];
	    {subscription_depth, Val, Opts} ->
		[encode_subscription_depth(Val, Opts, Translate)];
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/pubsub#subscribe_o"
				   "ptions">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"pubsub#deliver">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{deliver, Result} | Acc],
		 lists:delete(<<"pubsub#deliver">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#deliver">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end;
decode([#xdata_field{var = <<"pubsub#deliver">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#deliver">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#deliver">>} | _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#deliver">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_o"
		     "ptions">>}});
decode([#xdata_field{var = <<"pubsub#digest">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{digest, Result} | Acc],
		 lists:delete(<<"pubsub#digest">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#digest">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end;
decode([#xdata_field{var = <<"pubsub#digest">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#digest">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#digest">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#digest">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_o"
		     "ptions">>}});
decode([#xdata_field{var =
			 <<"pubsub#digest_frequency">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{digest_frequency, Result} | Acc],
		 lists:delete(<<"pubsub#digest_frequency">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#digest_frequency">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#digest_frequency">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#digest_frequency">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#digest_frequency">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#digest_frequency">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_o"
		     "ptions">>}});
decode([#xdata_field{var = <<"pubsub#expire">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{expire, Result} | Acc],
		 lists:delete(<<"pubsub#expire">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#expire">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end;
decode([#xdata_field{var = <<"pubsub#expire">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#expire">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#expire">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#expire">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_o"
		     "ptions">>}});
decode([#xdata_field{var = <<"pubsub#include_body">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{include_body, Result} | Acc],
		 lists:delete(<<"pubsub#include_body">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#include_body">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end;
decode([#xdata_field{var = <<"pubsub#include_body">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#include_body">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#include_body">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#include_body">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_o"
		     "ptions">>}});
decode([#xdata_field{var = <<"pubsub#show-values">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [dec_enum(Value, [away, chat, dnd, online, xa])
	 || Value <- Values]
    of
      Result ->
	  decode(Fs, [{'show-values', Result} | Acc],
		 lists:delete(<<"pubsub#show-values">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#show-values">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#subscription_type">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, [items, nodes]) of
      Result ->
	  decode(Fs, [{subscription_type, Result} | Acc],
		 lists:delete(<<"pubsub#subscription_type">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#subscription_type">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#subscription_type">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#subscription_type">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#subscription_type">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#subscription_type">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_o"
		     "ptions">>}});
decode([#xdata_field{var =
			 <<"pubsub#subscription_depth">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, ['1', all]) of
      Result ->
	  decode(Fs, [{subscription_depth, Result} | Acc],
		 lists:delete(<<"pubsub#subscription_depth">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#subscription_depth">>,
			 <<"http://jabber.org/protocol/pubsub#subscribe_o"
			   "ptions">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#subscription_depth">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#subscription_depth">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#subscription_depth">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#subscription_depth">>,
		   <<"http://jabber.org/protocol/pubsub#subscribe_o"
		     "ptions">>}});
decode([#xdata_field{var = Var} | Fs], Acc, Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE,
			 {unknown_var, Var,
			  <<"http://jabber.org/protocol/pubsub#subscribe_o"
			    "ptions">>}});
       true -> decode(Fs, Acc, Required)
    end;
decode([], _, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var,
		   <<"http://jabber.org/protocol/pubsub#subscribe_o"
		     "ptions">>}});
decode([], Acc, []) -> Acc.

encode_deliver(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#deliver">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Whether an entity wants to receive or "
				 "disable notifications">>)}.

encode_digest(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#digest">>, values = Values,
		 required = false, type = boolean, options = Opts,
		 desc = <<>>,
		 label =
		     Translate(<<"Whether an entity wants to receive digests "
				 "(aggregations) of notifications or all "
				 "notifications individually">>)}.

encode_digest_frequency(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#digest_frequency">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The minimum number of milliseconds between "
				 "sending any two notification digests">>)}.

encode_expire(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#expire">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>,
		 label =
		     Translate(<<"The DateTime at which a leased subscription "
				 "will end or has ended">>)}.

encode_include_body(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#include_body">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Whether an entity wants to receive an "
				 "XMPP message body in addition to the "
				 "payload format">>)}.

'encode_show-values'(Value, Options, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [enc_enum(V) || V <- Value]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"XMPP Show Value of Away">>),
				 value = <<"away">>},
		   #xdata_option{label =
				     Translate(<<"XMPP Show Value of Chat">>),
				 value = <<"chat">>},
		   #xdata_option{label =
				     Translate(<<"XMPP Show Value of DND (Do Not Disturb)">>),
				 value = <<"dnd">>},
		   #xdata_option{label =
				     Translate(<<"Mere Availability in XMPP (No Show Value)">>),
				 value = <<"online">>},
		   #xdata_option{label =
				     Translate(<<"XMPP Show Value of XA (Extended Away)">>),
				 value = <<"xa">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#show-values">>,
		 values = Values, required = false, type = 'list-multi',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The presence states for which an entity "
				 "wants to receive notifications">>)}.

encode_subscription_type(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"Receive notification of new items only">>),
				 value = <<"items">>},
		   #xdata_option{label =
				     Translate(<<"Receive notification of new nodes only">>),
				 value = <<"nodes">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#subscription_type">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>, label = <<>>}.

encode_subscription_depth(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"Receive notification from direct child "
						 "nodes only">>),
				 value = <<"1">>},
		   #xdata_option{label =
				     Translate(<<"Receive notification from all descendent "
						 "nodes">>),
				 value = <<"all">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#subscription_depth">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>, label = <<>>}.
