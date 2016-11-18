%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: muc_register.xdata
%% Form type: http://jabber.org/protocol/muc#register
%% Document: XEP-0045

-module(muc_register).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("muc_register.hrl").

-export_type([property/0, result/0, form/0]).

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
      false -> decode(Fs, Acc, [<<"muc#register_roomnick">>]);
      #xdata_field{values =
		       [<<"http://jabber.org/protocol/muc#register">>]} ->
	  decode(Fs, Acc, [<<"muc#register_roomnick">>]);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/muc#register">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {allow, Val} -> [encode_allow(Val, Translate)];
	    {allow, _, _} -> erlang:error({badarg, Opt});
	    {email, Val} -> [encode_email(Val, Translate)];
	    {email, _, _} -> erlang:error({badarg, Opt});
	    {faqentry, Val} -> [encode_faqentry(Val, Translate)];
	    {faqentry, _, _} -> erlang:error({badarg, Opt});
	    {first, Val} -> [encode_first(Val, Translate)];
	    {first, _, _} -> erlang:error({badarg, Opt});
	    {last, Val} -> [encode_last(Val, Translate)];
	    {last, _, _} -> erlang:error({badarg, Opt});
	    {roomnick, Val} -> [encode_roomnick(Val, Translate)];
	    {roomnick, _, _} -> erlang:error({badarg, Opt});
	    {url, Val} -> [encode_url(Val, Translate)];
	    {url, _, _} -> erlang:error({badarg, Opt});
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/muc#register">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"muc#register_allow">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allow, Result} | Acc],
		 lists:delete(<<"muc#register_allow">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#register_allow">>,
			 <<"http://jabber.org/protocol/muc#register">>}})
    end;
decode([#xdata_field{var = <<"muc#register_allow">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#register_allow">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#register_allow">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#register_allow">>,
		   <<"http://jabber.org/protocol/muc#register">>}});
decode([#xdata_field{var = <<"muc#register_email">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{email, Result} | Acc],
		 lists:delete(<<"muc#register_email">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#register_email">>,
			 <<"http://jabber.org/protocol/muc#register">>}})
    end;
decode([#xdata_field{var = <<"muc#register_email">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#register_email">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#register_email">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#register_email">>,
		   <<"http://jabber.org/protocol/muc#register">>}});
decode([#xdata_field{var = <<"muc#register_faqentry">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [Value || Value <- Values] of
      Result ->
	  decode(Fs, [{faqentry, Result} | Acc],
		 lists:delete(<<"muc#register_faqentry">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#register_faqentry">>,
			 <<"http://jabber.org/protocol/muc#register">>}})
    end;
decode([#xdata_field{var = <<"muc#register_first">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{first, Result} | Acc],
		 lists:delete(<<"muc#register_first">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#register_first">>,
			 <<"http://jabber.org/protocol/muc#register">>}})
    end;
decode([#xdata_field{var = <<"muc#register_first">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#register_first">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#register_first">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#register_first">>,
		   <<"http://jabber.org/protocol/muc#register">>}});
decode([#xdata_field{var = <<"muc#register_last">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{last, Result} | Acc],
		 lists:delete(<<"muc#register_last">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#register_last">>,
			 <<"http://jabber.org/protocol/muc#register">>}})
    end;
decode([#xdata_field{var = <<"muc#register_last">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#register_last">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#register_last">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#register_last">>,
		   <<"http://jabber.org/protocol/muc#register">>}});
decode([#xdata_field{var = <<"muc#register_roomnick">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{roomnick, Result} | Acc],
		 lists:delete(<<"muc#register_roomnick">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#register_roomnick">>,
			 <<"http://jabber.org/protocol/muc#register">>}})
    end;
decode([#xdata_field{var = <<"muc#register_roomnick">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#register_roomnick">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#register_roomnick">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#register_roomnick">>,
		   <<"http://jabber.org/protocol/muc#register">>}});
decode([#xdata_field{var = <<"muc#register_url">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{url, Result} | Acc],
		 lists:delete(<<"muc#register_url">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#register_url">>,
			 <<"http://jabber.org/protocol/muc#register">>}})
    end;
decode([#xdata_field{var = <<"muc#register_url">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#register_url">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#register_url">>} | _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#register_url">>,
		   <<"http://jabber.org/protocol/muc#register">>}});
decode([#xdata_field{var = Var} | Fs], Acc, Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE,
			 {unknown_var, Var,
			  <<"http://jabber.org/protocol/muc#register">>}});
       true -> decode(Fs, Acc, Required)
    end;
decode([], _, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var,
		   <<"http://jabber.org/protocol/muc#register">>}});
decode([], Acc, []) -> Acc.

encode_allow(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#register_allow">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Allow this person to register with the "
				 "room?">>)}.

encode_email(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#register_email">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Email Address">>)}.

encode_faqentry(Value, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#register_faqentry">>,
		 values = Values, required = false, type = 'text-multi',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"FAQ Entry">>)}.

encode_first(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#register_first">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Given Name">>)}.

encode_last(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#register_last">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Family Name">>)}.

encode_roomnick(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#register_roomnick">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Nickname">>)}.

encode_url(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#register_url">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"A Web Page">>)}.
