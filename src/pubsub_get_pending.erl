%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_get_pending.xdata
%% Form type: http://jabber.org/protocol/pubsub#subscribe_authorization
%% Document: XEP-0060

-module(pubsub_get_pending).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("pubsub_get_pending.hrl").

-export_type([property/0, result/0, form/0]).

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
      false -> decode(Fs, Acc, [<<"pubsub#node">>]);
      #xdata_field{values =
		       [<<"http://jabber.org/protocol/pubsub#subscribe_a"
			  "uthorization">>]} ->
	  decode(Fs, Acc, [<<"pubsub#node">>]);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/pubsub#subscribe_a"
			   "uthorization">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {node, Val} -> [encode_node(Val, default, Translate)];
	    {node, Val, Opts} ->
		[encode_node(Val, Opts, Translate)];
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

encode_node(Value, Options, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = if Options == default -> [];
	      true ->
		  [#xdata_option{label = Translate(L), value = V}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#node">>, values = Values,
		 required = false, type = 'list-single', options = Opts,
		 desc = <<>>,
		 label =
		     Translate(<<"The NodeID of the relevant node">>)}.
