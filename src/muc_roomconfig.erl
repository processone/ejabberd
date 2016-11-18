%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: muc_roomconfig.xdata
%% Form type: http://jabber.org/protocol/muc#roomconfig
%% Document: XEP-0045

-module(muc_roomconfig).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("muc_roomconfig.hrl").

-export_type([property/0, result/0, form/0]).

dec_int(Val, Min, Max) ->
    case list_to_integer(binary_to_list(Val)) of
      Int when Int =< Max, Min == infinity -> Int;
      Int when Int =< Max, Int >= Min -> Int
    end.

enc_int(Int) -> integer_to_binary(Int).

dec_enum(Val, Enums) ->
    AtomVal = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(AtomVal, Enums) of
      true -> AtomVal
    end.

enc_enum(Atom) -> erlang:atom_to_binary(Atom, utf8).

dec_enum_int(Val, Enums, Min, Max) ->
    try dec_int(Val, Min, Max) catch
      _:_ -> dec_enum(Val, Enums)
    end.

enc_enum_int(Int) when is_integer(Int) -> enc_int(Int);
enc_enum_int(Atom) -> enc_enum(Atom).

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
      false -> decode(Fs, Acc, []);
      #xdata_field{values =
		       [<<"http://jabber.org/protocol/muc#roomconfig">>]} ->
	  decode(Fs, Acc, []);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {maxhistoryfetch, Val} ->
		[encode_maxhistoryfetch(Val, Translate)];
	    {maxhistoryfetch, _, _} -> erlang:error({badarg, Opt});
	    {allowpm, Val} ->
		[encode_allowpm(Val, default, Translate)];
	    {allowpm, Val, Opts} ->
		[encode_allowpm(Val, Opts, Translate)];
	    {allow_private_messages, Val} ->
		[encode_allow_private_messages(Val, Translate)];
	    {allow_private_messages, _, _} ->
		erlang:error({badarg, Opt});
	    {allow_private_messages_from_visitors, Val} ->
		[encode_allow_private_messages_from_visitors(Val,
							     default,
							     Translate)];
	    {allow_private_messages_from_visitors, Val, Opts} ->
		[encode_allow_private_messages_from_visitors(Val, Opts,
							     Translate)];
	    {allow_visitor_status, Val} ->
		[encode_allow_visitor_status(Val, Translate)];
	    {allow_visitor_status, _, _} ->
		erlang:error({badarg, Opt});
	    {allow_visitor_nickchange, Val} ->
		[encode_allow_visitor_nickchange(Val, Translate)];
	    {allow_visitor_nickchange, _, _} ->
		erlang:error({badarg, Opt});
	    {allow_voice_requests, Val} ->
		[encode_allow_voice_requests(Val, Translate)];
	    {allow_voice_requests, _, _} ->
		erlang:error({badarg, Opt});
	    {allow_subscription, Val} ->
		[encode_allow_subscription(Val, Translate)];
	    {allow_subscription, _, _} ->
		erlang:error({badarg, Opt});
	    {voice_request_min_interval, Val} ->
		[encode_voice_request_min_interval(Val, Translate)];
	    {voice_request_min_interval, _, _} ->
		erlang:error({badarg, Opt});
	    {captcha_protected, Val} ->
		[encode_captcha_protected(Val, Translate)];
	    {captcha_protected, _, _} ->
		erlang:error({badarg, Opt});
	    {captcha_whitelist, Val} ->
		[encode_captcha_whitelist(Val, Translate)];
	    {captcha_whitelist, _, _} ->
		erlang:error({badarg, Opt});
	    {allow_query_users, Val} ->
		[encode_allow_query_users(Val, Translate)];
	    {allow_query_users, _, _} ->
		erlang:error({badarg, Opt});
	    {allowinvites, Val} ->
		[encode_allowinvites(Val, Translate)];
	    {allowinvites, _, _} -> erlang:error({badarg, Opt});
	    {changesubject, Val} ->
		[encode_changesubject(Val, Translate)];
	    {changesubject, _, _} -> erlang:error({badarg, Opt});
	    {enablelogging, Val} ->
		[encode_enablelogging(Val, Translate)];
	    {enablelogging, _, _} -> erlang:error({badarg, Opt});
	    {getmemberlist, Val} ->
		[encode_getmemberlist(Val, default, Translate)];
	    {getmemberlist, Val, Opts} ->
		[encode_getmemberlist(Val, Opts, Translate)];
	    {lang, Val} -> [encode_lang(Val, Translate)];
	    {lang, _, _} -> erlang:error({badarg, Opt});
	    {pubsub, Val} -> [encode_pubsub(Val, Translate)];
	    {pubsub, _, _} -> erlang:error({badarg, Opt});
	    {maxusers, Val} ->
		[encode_maxusers(Val, default, Translate)];
	    {maxusers, Val, Opts} ->
		[encode_maxusers(Val, Opts, Translate)];
	    {membersonly, Val} ->
		[encode_membersonly(Val, Translate)];
	    {membersonly, _, _} -> erlang:error({badarg, Opt});
	    {moderatedroom, Val} ->
		[encode_moderatedroom(Val, Translate)];
	    {moderatedroom, _, _} -> erlang:error({badarg, Opt});
	    {members_by_default, Val} ->
		[encode_members_by_default(Val, Translate)];
	    {members_by_default, _, _} ->
		erlang:error({badarg, Opt});
	    {passwordprotectedroom, Val} ->
		[encode_passwordprotectedroom(Val, Translate)];
	    {passwordprotectedroom, _, _} ->
		erlang:error({badarg, Opt});
	    {persistentroom, Val} ->
		[encode_persistentroom(Val, Translate)];
	    {persistentroom, _, _} -> erlang:error({badarg, Opt});
	    {presencebroadcast, Val} ->
		[encode_presencebroadcast(Val, default, Translate)];
	    {presencebroadcast, Val, Opts} ->
		[encode_presencebroadcast(Val, Opts, Translate)];
	    {publicroom, Val} ->
		[encode_publicroom(Val, Translate)];
	    {publicroom, _, _} -> erlang:error({badarg, Opt});
	    {public_list, Val} ->
		[encode_public_list(Val, Translate)];
	    {public_list, _, _} -> erlang:error({badarg, Opt});
	    {roomadmins, Val} ->
		[encode_roomadmins(Val, Translate)];
	    {roomadmins, _, _} -> erlang:error({badarg, Opt});
	    {roomdesc, Val} -> [encode_roomdesc(Val, Translate)];
	    {roomdesc, _, _} -> erlang:error({badarg, Opt});
	    {roomname, Val} -> [encode_roomname(Val, Translate)];
	    {roomname, _, _} -> erlang:error({badarg, Opt});
	    {roomowners, Val} ->
		[encode_roomowners(Val, Translate)];
	    {roomowners, _, _} -> erlang:error({badarg, Opt});
	    {roomsecret, Val} ->
		[encode_roomsecret(Val, Translate)];
	    {roomsecret, _, _} -> erlang:error({badarg, Opt});
	    {whois, Val} -> [encode_whois(Val, default, Translate)];
	    {whois, Val, Opts} ->
		[encode_whois(Val, Opts, Translate)];
	    {mam, Val} -> [encode_mam(Val, Translate)];
	    {mam, _, _} -> erlang:error({badarg, Opt});
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/muc#roomconfig">>]},
    [FormType | lists:flatten(Fs)].

decode([#xdata_field{var = <<"muc#maxhistoryfetch">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{maxhistoryfetch, Result} | Acc],
		 lists:delete(<<"muc#maxhistoryfetch">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#maxhistoryfetch">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"muc#maxhistoryfetch">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#maxhistoryfetch">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#maxhistoryfetch">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#maxhistoryfetch">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"muc#roomconfig_allowpm">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{allowpm, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_allowpm">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_allowpm">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"muc#roomconfig_allowpm">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_allowpm">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#roomconfig_allowpm">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_allowpm">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"allow_private_messages">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allow_private_messages, Result} | Acc],
		 lists:delete(<<"allow_private_messages">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"allow_private_messages">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"allow_private_messages">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"allow_private_messages">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"allow_private_messages">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"allow_private_messages">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"allow_private_messages_from_visitors">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, [nobody, moderators, anyone]) of
      Result ->
	  decode(Fs,
		 [{allow_private_messages_from_visitors, Result} | Acc],
		 lists:delete(<<"allow_private_messages_from_visitors">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value,
			 <<"allow_private_messages_from_visitors">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"allow_private_messages_from_visitors">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"allow_private_messages_from_visitors">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"allow_private_messages_from_visitors">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values,
		   <<"allow_private_messages_from_visitors">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"allow_visitor_status">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allow_visitor_status, Result} | Acc],
		 lists:delete(<<"allow_visitor_status">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"allow_visitor_status">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"allow_visitor_status">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"allow_visitor_status">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"allow_visitor_status">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"allow_visitor_status">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"allow_visitor_nickchange">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allow_visitor_nickchange, Result} | Acc],
		 lists:delete(<<"allow_visitor_nickchange">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"allow_visitor_nickchange">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"allow_visitor_nickchange">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"allow_visitor_nickchange">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"allow_visitor_nickchange">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"allow_visitor_nickchange">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"allow_voice_requests">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allow_voice_requests, Result} | Acc],
		 lists:delete(<<"allow_voice_requests">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"allow_voice_requests">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"allow_voice_requests">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"allow_voice_requests">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"allow_voice_requests">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"allow_voice_requests">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"allow_subscription">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allow_subscription, Result} | Acc],
		 lists:delete(<<"allow_subscription">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"allow_subscription">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"allow_subscription">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"allow_subscription">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"allow_subscription">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"allow_subscription">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"voice_request_min_interval">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_int(Value, 0, infinity) of
      Result ->
	  decode(Fs, [{voice_request_min_interval, Result} | Acc],
		 lists:delete(<<"voice_request_min_interval">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"voice_request_min_interval">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"voice_request_min_interval">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"voice_request_min_interval">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"voice_request_min_interval">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"voice_request_min_interval">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"captcha_protected">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{captcha_protected, Result} | Acc],
		 lists:delete(<<"captcha_protected">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"captcha_protected">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"captcha_protected">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"captcha_protected">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"captcha_protected">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"captcha_protected">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"captcha_whitelist">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [dec_jid(Value) || Value <- Values] of
      Result ->
	  decode(Fs, [{captcha_whitelist, Result} | Acc],
		 lists:delete(<<"captcha_whitelist">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"captcha_whitelist">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"allow_query_users">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allow_query_users, Result} | Acc],
		 lists:delete(<<"allow_query_users">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"allow_query_users">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"allow_query_users">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"allow_query_users">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"allow_query_users">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"allow_query_users">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_allowinvites">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{allowinvites, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_allowinvites">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_allowinvites">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_allowinvites">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_allowinvites">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_allowinvites">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_allowinvites">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_changesubject">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{changesubject, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_changesubject">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_changesubject">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_changesubject">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_changesubject">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_changesubject">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_changesubject">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_enablelogging">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{enablelogging, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_enablelogging">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_enablelogging">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_enablelogging">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_enablelogging">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_enablelogging">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_enablelogging">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_getmemberlist">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [Value || Value <- Values] of
      Result ->
	  decode(Fs, [{getmemberlist, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_getmemberlist">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_getmemberlist">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"muc#roomconfig_lang">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{lang, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_lang">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_lang">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"muc#roomconfig_lang">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#roomconfig_lang">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#roomconfig_lang">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_lang">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"muc#roomconfig_pubsub">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{pubsub, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_pubsub">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_pubsub">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"muc#roomconfig_pubsub">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#roomconfig_pubsub">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#roomconfig_pubsub">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_pubsub">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_maxusers">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum_int(Value, [none], 0, infinity) of
      Result ->
	  decode(Fs, [{maxusers, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_maxusers">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_maxusers">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_maxusers">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_maxusers">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_maxusers">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_maxusers">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_membersonly">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{membersonly, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_membersonly">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_membersonly">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_membersonly">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_membersonly">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_membersonly">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_membersonly">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_moderatedroom">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{moderatedroom, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_moderatedroom">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_moderatedroom">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_moderatedroom">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_moderatedroom">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_moderatedroom">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_moderatedroom">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"members_by_default">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{members_by_default, Result} | Acc],
		 lists:delete(<<"members_by_default">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"members_by_default">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"members_by_default">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"members_by_default">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"members_by_default">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"members_by_default">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_passwordprotectedroom">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{passwordprotectedroom, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_passwordprotectedroom">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value,
			 <<"muc#roomconfig_passwordprotectedroom">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_passwordprotectedroom">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_passwordprotectedroom">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_passwordprotectedroom">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values,
		   <<"muc#roomconfig_passwordprotectedroom">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_persistentroom">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{persistentroom, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_persistentroom">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_persistentroom">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_persistentroom">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_persistentroom">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_persistentroom">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_persistentroom">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_presencebroadcast">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [dec_enum(Value, [moderator, participant, visitor])
	 || Value <- Values]
    of
      Result ->
	  decode(Fs, [{presencebroadcast, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_presencebroadcast">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_presencebroadcast">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_publicroom">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{publicroom, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_publicroom">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_publicroom">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_publicroom">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_publicroom">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_publicroom">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_publicroom">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"public_list">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{public_list, Result} | Acc],
		 lists:delete(<<"public_list">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"public_list">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"public_list">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"public_list">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"public_list">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"public_list">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomadmins">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [dec_jid(Value) || Value <- Values] of
      Result ->
	  decode(Fs, [{roomadmins, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_roomadmins">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_roomadmins">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomdesc">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{roomdesc, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_roomdesc">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_roomdesc">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomdesc">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_roomdesc">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomdesc">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_roomdesc">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomname">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{roomname, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_roomname">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_roomname">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomname">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_roomname">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomname">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_roomname">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomowners">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [dec_jid(Value) || Value <- Values] of
      Result ->
	  decode(Fs, [{roomowners, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_roomowners">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_roomowners">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomsecret">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{roomsecret, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_roomsecret">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_roomsecret">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomsecret">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"muc#roomconfig_roomsecret">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"muc#roomconfig_roomsecret">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_roomsecret">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"muc#roomconfig_whois">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, [moderators, anyone]) of
      Result ->
	  decode(Fs, [{whois, Result} | Acc],
		 lists:delete(<<"muc#roomconfig_whois">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"muc#roomconfig_whois">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"muc#roomconfig_whois">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"muc#roomconfig_whois">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"muc#roomconfig_whois">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"muc#roomconfig_whois">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = <<"mam">>, values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{mam, Result} | Acc],
		 lists:delete(<<"mam">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"mam">>,
			 <<"http://jabber.org/protocol/muc#roomconfig">>}})
    end;
decode([#xdata_field{var = <<"mam">>, values = []} = F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"mam">>, values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"mam">>} | _], _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"mam">>,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([#xdata_field{var = Var} | Fs], Acc, Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE,
			 {unknown_var, Var,
			  <<"http://jabber.org/protocol/muc#roomconfig">>}});
       true -> decode(Fs, Acc, Required)
    end;
decode([], _, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var,
		   <<"http://jabber.org/protocol/muc#roomconfig">>}});
decode([], Acc, []) -> Acc.

encode_maxhistoryfetch(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#maxhistoryfetch">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Maximum Number of History Messages Returned "
				 "by Room">>)}.

encode_allowpm(Value, Options, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = if Options == default -> [];
	      true ->
		  [#xdata_option{label = Translate(L), value = V}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"muc#roomconfig_allowpm">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Roles that May Send Private Messages">>)}.

encode_allow_private_messages(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"allow_private_messages">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Allow users to send private messages">>)}.

encode_allow_private_messages_from_visitors(Value,
					    Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label = Translate(<<"nobody">>),
				 value = <<"nobody">>},
		   #xdata_option{label = Translate(<<"moderators only">>),
				 value = <<"moderators">>},
		   #xdata_option{label = Translate(<<"anyone">>),
				 value = <<"anyone">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var =
		     <<"allow_private_messages_from_visitors">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Allow visitors to send private messages to">>)}.

encode_allow_visitor_status(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"allow_visitor_status">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Allow visitors to send status text in "
				 "presence updates">>)}.

encode_allow_visitor_nickchange(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"allow_visitor_nickchange">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Allow visitors to change nickname">>)}.

encode_allow_voice_requests(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"allow_voice_requests">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Allow visitors to send voice requests">>)}.

encode_allow_subscription(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"allow_subscription">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Allow subscription">>)}.

encode_voice_request_min_interval(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_int(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"voice_request_min_interval">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Minimum interval between voice requests "
				 "(in seconds)">>)}.

encode_captcha_protected(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"captcha_protected">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Make room CAPTCHA protected">>)}.

encode_captcha_whitelist(Value, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [enc_jid(V) || V <- Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"captcha_whitelist">>,
		 values = Values, required = false, type = 'jid-multi',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Exclude Jabber IDs from CAPTCHA challenge">>)}.

encode_allow_query_users(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"allow_query_users">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Allow users to query other users">>)}.

encode_allowinvites(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_allowinvites">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Allow users to send invites">>)}.

encode_changesubject(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_changesubject">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Allow users to change the subject">>)}.

encode_enablelogging(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_enablelogging">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Enable logging">>)}.

encode_getmemberlist(Value, Options, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [Value]
	     end,
    Opts = if Options == default -> [];
	      true ->
		  [#xdata_option{label = Translate(L), value = V}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"muc#roomconfig_getmemberlist">>,
		 values = Values, required = false, type = 'list-multi',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Roles and Affiliations that May Retrieve "
				 "Member List">>)}.

encode_lang(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_lang">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Natural Language for Room Discussions">>)}.

encode_pubsub(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_pubsub">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"XMPP URI of Associated Publish-Subscribe "
				 "Node">>)}.

encode_maxusers(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum_int(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label = Translate(<<"No limit">>),
				 value = <<"none">>},
		   #xdata_option{value = <<"5">>},
		   #xdata_option{value = <<"10">>},
		   #xdata_option{value = <<"20">>},
		   #xdata_option{value = <<"30">>},
		   #xdata_option{value = <<"50">>},
		   #xdata_option{value = <<"100">>},
		   #xdata_option{value = <<"200">>},
		   #xdata_option{value = <<"500">>},
		   #xdata_option{value = <<"1000">>},
		   #xdata_option{value = <<"2000">>},
		   #xdata_option{value = <<"5000">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum_int(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"muc#roomconfig_maxusers">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Maximum Number of Occupants">>)}.

encode_membersonly(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_membersonly">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Make room members-only">>)}.

encode_moderatedroom(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_moderatedroom">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Make room moderated">>)}.

encode_members_by_default(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"members_by_default">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Default users as participants">>)}.

encode_passwordprotectedroom(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var =
		     <<"muc#roomconfig_passwordprotectedroom">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Make room password protected">>)}.

encode_persistentroom(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_persistentroom">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Make room persistent">>)}.

encode_presencebroadcast(Value, Options, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [enc_enum(V) || V <- Value]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label = Translate(<<"Moderator">>),
				 value = <<"moderator">>},
		   #xdata_option{label = Translate(<<"Participant">>),
				 value = <<"participant">>},
		   #xdata_option{label = Translate(<<"Visitor">>),
				 value = <<"visitor">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var =
		     <<"muc#roomconfig_presencebroadcast">>,
		 values = Values, required = false, type = 'list-multi',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Roles for which Presence is Broadcasted">>)}.

encode_publicroom(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_publicroom">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Make room public searchable">>)}.

encode_public_list(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"public_list">>, values = Values,
		 required = false, type = boolean, options = Opts,
		 desc = <<>>,
		 label = Translate(<<"Make participants list public">>)}.

encode_roomadmins(Value, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [enc_jid(V) || V <- Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_roomadmins">>,
		 values = Values, required = false, type = 'jid-multi',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Full List of Room Admins">>)}.

encode_roomdesc(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_roomdesc">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Room description">>)}.

encode_roomname(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_roomname">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Room title">>)}.

encode_roomowners(Value, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [enc_jid(V) || V <- Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_roomowners">>,
		 values = Values, required = false, type = 'jid-multi',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Full List of Room Owners">>)}.

encode_roomsecret(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"muc#roomconfig_roomsecret">>,
		 values = Values, required = false,
		 type = 'text-private', options = Opts, desc = <<>>,
		 label = Translate(<<"Password">>)}.

encode_whois(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label = Translate(<<"moderators only">>),
				 value = <<"moderators">>},
		   #xdata_option{label = Translate(<<"anyone">>),
				 value = <<"anyone">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"muc#roomconfig_whois">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Present real Jabber IDs to">>)}.

encode_mam(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"mam">>, values = Values,
		 required = false, type = boolean, options = Opts,
		 desc = <<>>,
		 label = Translate(<<"Enable message archiving">>)}.
