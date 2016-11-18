%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: muc_request.xdata
%% Form type: http://jabber.org/protocol/muc#request
%% Document: XEP-0045

-module(muc_request).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("muc_request.hrl").

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
      false -> decode(Fs, Acc, [<<"muc#role">>]);
      #xdata_field{values =
		       [<<"http://jabber.org/protocol/muc#request">>]} ->
	  decode(Fs, Acc, [<<"muc#role">>]);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/muc#request">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {role, Val} -> [encode_role(Val, default, Translate)];
	    {role, Val, Opts} ->
		[encode_role(Val, Opts, Translate)];
	    {jid, Val} -> [encode_jid(Val, Translate)];
	    {jid, _, _} -> erlang:error({badarg, Opt});
	    {roomnick, Val} -> [encode_roomnick(Val, Translate)];
	    {roomnick, _, _} -> erlang:error({badarg, Opt});
	    {request_allow, Val} ->
		[encode_request_allow(Val, Translate)];
	    {request_allow, _, _} -> erlang:error({badarg, Opt});
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/muc#request">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"muc#role">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, [participant]) of
      Result ->
	  decode(Fs, [{role, Result} | Acc],
		 lists:delete(<<"muc#role">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#role">>,
			 <<"http://jabber.org/protocol/muc#request">>}})
    end;
decode([#xdata_field{var = <<"muc#role">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#role">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#role">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#role">>,
		   <<"http://jabber.org/protocol/muc#request">>}});
decode([#xdata_field{var = <<"muc#jid">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_jid(Value) of
      Result ->
	  decode(Fs, [{jid, Result} | Acc],
		 lists:delete(<<"muc#jid">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#jid">>,
			 <<"http://jabber.org/protocol/muc#request">>}})
    end;
decode([#xdata_field{var = <<"muc#jid">>, values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#jid">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#jid">>} | _], _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#jid">>,
		   <<"http://jabber.org/protocol/muc#request">>}});
decode([#xdata_field{var = <<"muc#roomnick">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{roomnick, Result} | Acc],
		 lists:delete(<<"muc#roomnick">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomnick">>,
			 <<"http://jabber.org/protocol/muc#request">>}})
    end;
decode([#xdata_field{var = <<"muc#roomnick">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#roomnick">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#roomnick">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomnick">>,
		   <<"http://jabber.org/protocol/muc#request">>}});
decode([#xdata_field{var = <<"muc#request_allow">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{request_allow, Result} | Acc],
		 lists:delete(<<"muc#request_allow">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#request_allow">>,
			 <<"http://jabber.org/protocol/muc#request">>}})
    end;
decode([#xdata_field{var = <<"muc#request_allow">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#request_allow">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#request_allow">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#request_allow">>,
		   <<"http://jabber.org/protocol/muc#request">>}});
decode([#xdata_field{var = Var} | Fs], Acc, Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE,
			 {unknown_var, Var,
			  <<"http://jabber.org/protocol/muc#request">>}});
       true -> decode(Fs, Acc, Required)
    end;
decode([], _, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var,
		   <<"http://jabber.org/protocol/muc#request">>}});
decode([], Acc, []) -> Acc.

encode_role(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label = Translate(<<"Participant">>),
				 value = <<"participant">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"muc#role">>, values = Values,
		 required = false, type = 'list-single', options = Opts,
		 desc = <<>>, label = Translate(<<"Requested role">>)}.

encode_jid(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_jid(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#jid">>, values = Values,
		 required = false, type = 'jid-single', options = Opts,
		 desc = <<>>, label = Translate(<<"User JID">>)}.

encode_roomnick(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomnick">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>, label = Translate(<<"Nickname">>)}.

encode_request_allow(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#request_allow">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Grant voice to this person?">>)}.
