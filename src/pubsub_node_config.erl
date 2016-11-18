%% Created automatically by xdata generator (xdata_codec.erl)
%% Source: pubsub_node_config.xdata
%% Form type: http://jabber.org/protocol/pubsub#node_config
%% Document: XEP-0060

-module(pubsub_node_config).

-export([decode/1, decode/2, encode/1, encode/2,
	 format_error/1]).

-include("xmpp_codec.hrl").

-include("pubsub_node_config.hrl").

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
		       [<<"http://jabber.org/protocol/pubsub#node_config">>]} ->
	  decode(Fs, Acc, []);
      _ ->
	  erlang:error({?MODULE,
			{form_type_mismatch,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end.

encode(Cfg) -> encode(Cfg, fun (Text) -> Text end).

encode(List, Translate) when is_list(List) ->
    Fs = [case Opt of
	    {access_model, Val} ->
		[encode_access_model(Val, default, Translate)];
	    {access_model, Val, Opts} ->
		[encode_access_model(Val, Opts, Translate)];
	    {body_xslt, Val} -> [encode_body_xslt(Val, Translate)];
	    {body_xslt, _, _} -> erlang:error({badarg, Opt});
	    {children_association_policy, Val} ->
		[encode_children_association_policy(Val, default,
						    Translate)];
	    {children_association_policy, Val, Opts} ->
		[encode_children_association_policy(Val, Opts,
						    Translate)];
	    {children_association_whitelist, Val} ->
		[encode_children_association_whitelist(Val, Translate)];
	    {children_association_whitelist, _, _} ->
		erlang:error({badarg, Opt});
	    {children, Val} -> [encode_children(Val, Translate)];
	    {children, _, _} -> erlang:error({badarg, Opt});
	    {children_max, Val} ->
		[encode_children_max(Val, Translate)];
	    {children_max, _, _} -> erlang:error({badarg, Opt});
	    {collection, Val} ->
		[encode_collection(Val, Translate)];
	    {collection, _, _} -> erlang:error({badarg, Opt});
	    {contact, Val} -> [encode_contact(Val, Translate)];
	    {contact, _, _} -> erlang:error({badarg, Opt});
	    {dataform_xslt, Val} ->
		[encode_dataform_xslt(Val, Translate)];
	    {dataform_xslt, _, _} -> erlang:error({badarg, Opt});
	    {deliver_notifications, Val} ->
		[encode_deliver_notifications(Val, Translate)];
	    {deliver_notifications, _, _} ->
		erlang:error({badarg, Opt});
	    {deliver_payloads, Val} ->
		[encode_deliver_payloads(Val, Translate)];
	    {deliver_payloads, _, _} -> erlang:error({badarg, Opt});
	    {description, Val} ->
		[encode_description(Val, Translate)];
	    {description, _, _} -> erlang:error({badarg, Opt});
	    {item_expire, Val} ->
		[encode_item_expire(Val, Translate)];
	    {item_expire, _, _} -> erlang:error({badarg, Opt});
	    {itemreply, Val} ->
		[encode_itemreply(Val, default, Translate)];
	    {itemreply, Val, Opts} ->
		[encode_itemreply(Val, Opts, Translate)];
	    {language, Val} ->
		[encode_language(Val, default, Translate)];
	    {language, Val, Opts} ->
		[encode_language(Val, Opts, Translate)];
	    {max_items, Val} -> [encode_max_items(Val, Translate)];
	    {max_items, _, _} -> erlang:error({badarg, Opt});
	    {max_payload_size, Val} ->
		[encode_max_payload_size(Val, Translate)];
	    {max_payload_size, _, _} -> erlang:error({badarg, Opt});
	    {node_type, Val} ->
		[encode_node_type(Val, default, Translate)];
	    {node_type, Val, Opts} ->
		[encode_node_type(Val, Opts, Translate)];
	    {notification_type, Val} ->
		[encode_notification_type(Val, default, Translate)];
	    {notification_type, Val, Opts} ->
		[encode_notification_type(Val, Opts, Translate)];
	    {notify_config, Val} ->
		[encode_notify_config(Val, Translate)];
	    {notify_config, _, _} -> erlang:error({badarg, Opt});
	    {notify_delete, Val} ->
		[encode_notify_delete(Val, Translate)];
	    {notify_delete, _, _} -> erlang:error({badarg, Opt});
	    {notify_retract, Val} ->
		[encode_notify_retract(Val, Translate)];
	    {notify_retract, _, _} -> erlang:error({badarg, Opt});
	    {notify_sub, Val} ->
		[encode_notify_sub(Val, Translate)];
	    {notify_sub, _, _} -> erlang:error({badarg, Opt});
	    {persist_items, Val} ->
		[encode_persist_items(Val, Translate)];
	    {persist_items, _, _} -> erlang:error({badarg, Opt});
	    {presence_based_delivery, Val} ->
		[encode_presence_based_delivery(Val, Translate)];
	    {presence_based_delivery, _, _} ->
		erlang:error({badarg, Opt});
	    {publish_model, Val} ->
		[encode_publish_model(Val, default, Translate)];
	    {publish_model, Val, Opts} ->
		[encode_publish_model(Val, Opts, Translate)];
	    {purge_offline, Val} ->
		[encode_purge_offline(Val, Translate)];
	    {purge_offline, _, _} -> erlang:error({badarg, Opt});
	    {roster_groups_allowed, Val} ->
		[encode_roster_groups_allowed(Val, default, Translate)];
	    {roster_groups_allowed, Val, Opts} ->
		[encode_roster_groups_allowed(Val, Opts, Translate)];
	    {send_last_published_item, Val} ->
		[encode_send_last_published_item(Val, default,
						 Translate)];
	    {send_last_published_item, Val, Opts} ->
		[encode_send_last_published_item(Val, Opts, Translate)];
	    {tempsub, Val} -> [encode_tempsub(Val, Translate)];
	    {tempsub, _, _} -> erlang:error({badarg, Opt});
	    {subscribe, Val} -> [encode_subscribe(Val, Translate)];
	    {subscribe, _, _} -> erlang:error({badarg, Opt});
	    {title, Val} -> [encode_title(Val, Translate)];
	    {title, _, _} -> erlang:error({badarg, Opt});
	    {type, Val} -> [encode_type(Val, Translate)];
	    {type, _, _} -> erlang:error({badarg, Opt});
	    #xdata_field{} -> [Opt];
	    _ -> []
	  end
	  || Opt <- List],
    FormType = #xdata_field{var = <<"FORM_TYPE">>,
			    type = hidden,
			    values =
				[<<"http://jabber.org/protocol/pubsub#node_config">>]},
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
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
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
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#body_xslt">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{body_xslt, Result} | Acc],
		 lists:delete(<<"pubsub#body_xslt">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#body_xslt">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#body_xslt">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#body_xslt">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#body_xslt">>} | _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#body_xslt">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var =
			 <<"pubsub#children_association_policy">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, [all, owners, whitelist]) of
      Result ->
	  decode(Fs,
		 [{children_association_policy, Result} | Acc],
		 lists:delete(<<"pubsub#children_association_policy">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value,
			 <<"pubsub#children_association_policy">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#children_association_policy">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#children_association_policy">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#children_association_policy">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values,
		   <<"pubsub#children_association_policy">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var =
			 <<"pubsub#children_association_whitelist">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [dec_jid(Value) || Value <- Values] of
      Result ->
	  decode(Fs,
		 [{children_association_whitelist, Result} | Acc],
		 lists:delete(<<"pubsub#children_association_whitelist">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value,
			 <<"pubsub#children_association_whitelist">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#children">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [Value || Value <- Values] of
      Result ->
	  decode(Fs, [{children, Result} | Acc],
		 lists:delete(<<"pubsub#children">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#children">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#children_max">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{children_max, Result} | Acc],
		 lists:delete(<<"pubsub#children_max">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#children_max">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#children_max">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#children_max">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#children_max">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#children_max">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#collection">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [Value || Value <- Values] of
      Result ->
	  decode(Fs, [{collection, Result} | Acc],
		 lists:delete(<<"pubsub#collection">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#collection">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#contact">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [dec_jid(Value) || Value <- Values] of
      Result ->
	  decode(Fs, [{contact, Result} | Acc],
		 lists:delete(<<"pubsub#contact">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#contact">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#dataform_xslt">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{dataform_xslt, Result} | Acc],
		 lists:delete(<<"pubsub#dataform_xslt">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#dataform_xslt">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#dataform_xslt">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#dataform_xslt">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#dataform_xslt">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#dataform_xslt">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var =
			 <<"pubsub#deliver_notifications">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{deliver_notifications, Result} | Acc],
		 lists:delete(<<"pubsub#deliver_notifications">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#deliver_notifications">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#deliver_notifications">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#deliver_notifications">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#deliver_notifications">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#deliver_notifications">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var =
			 <<"pubsub#deliver_payloads">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{deliver_payloads, Result} | Acc],
		 lists:delete(<<"pubsub#deliver_payloads">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#deliver_payloads">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#deliver_payloads">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#deliver_payloads">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#deliver_payloads">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#deliver_payloads">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#description">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{description, Result} | Acc],
		 lists:delete(<<"pubsub#description">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#description">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#description">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#description">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#description">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#description">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#item_expire">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{item_expire, Result} | Acc],
		 lists:delete(<<"pubsub#item_expire">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#item_expire">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#item_expire">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#item_expire">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#item_expire">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#item_expire">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#itemreply">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, [owner, publisher, none]) of
      Result ->
	  decode(Fs, [{itemreply, Result} | Acc],
		 lists:delete(<<"pubsub#itemreply">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#itemreply">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#itemreply">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#itemreply">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#itemreply">>} | _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#itemreply">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#language">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{language, Result} | Acc],
		 lists:delete(<<"pubsub#language">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#language">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#language">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#language">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#language">>} | _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#language">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#max_items">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_int(Value, 0, infinity) of
      Result ->
	  decode(Fs, [{max_items, Result} | Acc],
		 lists:delete(<<"pubsub#max_items">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#max_items">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#max_items">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#max_items">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#max_items">>} | _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#max_items">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var =
			 <<"pubsub#max_payload_size">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_int(Value, 0, infinity) of
      Result ->
	  decode(Fs, [{max_payload_size, Result} | Acc],
		 lists:delete(<<"pubsub#max_payload_size">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#max_payload_size">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#max_payload_size">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#max_payload_size">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#max_payload_size">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#max_payload_size">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#node_type">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, [leaf, collection]) of
      Result ->
	  decode(Fs, [{node_type, Result} | Acc],
		 lists:delete(<<"pubsub#node_type">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#node_type">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#node_type">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#node_type">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#node_type">>} | _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#node_type">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var =
			 <<"pubsub#notification_type">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, [normal, headline]) of
      Result ->
	  decode(Fs, [{notification_type, Result} | Acc],
		 lists:delete(<<"pubsub#notification_type">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#notification_type">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#notification_type">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#notification_type">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#notification_type">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#notification_type">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#notify_config">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{notify_config, Result} | Acc],
		 lists:delete(<<"pubsub#notify_config">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#notify_config">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#notify_config">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#notify_config">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#notify_config">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#notify_config">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#notify_delete">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{notify_delete, Result} | Acc],
		 lists:delete(<<"pubsub#notify_delete">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#notify_delete">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#notify_delete">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#notify_delete">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#notify_delete">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#notify_delete">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#notify_retract">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{notify_retract, Result} | Acc],
		 lists:delete(<<"pubsub#notify_retract">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#notify_retract">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#notify_retract">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#notify_retract">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#notify_retract">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#notify_retract">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#notify_sub">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{notify_sub, Result} | Acc],
		 lists:delete(<<"pubsub#notify_sub">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#notify_sub">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#notify_sub">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#notify_sub">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#notify_sub">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#notify_sub">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#persist_items">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{persist_items, Result} | Acc],
		 lists:delete(<<"pubsub#persist_items">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#persist_items">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#persist_items">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#persist_items">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#persist_items">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#persist_items">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var =
			 <<"pubsub#presence_based_delivery">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{presence_based_delivery, Result} | Acc],
		 lists:delete(<<"pubsub#presence_based_delivery">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#presence_based_delivery">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#presence_based_delivery">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#presence_based_delivery">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#presence_based_delivery">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#presence_based_delivery">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#publish_model">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value, [publishers, subscribers, open]) of
      Result ->
	  decode(Fs, [{publish_model, Result} | Acc],
		 lists:delete(<<"pubsub#publish_model">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#publish_model">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#publish_model">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#publish_model">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#publish_model">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#publish_model">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#purge_offline">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{purge_offline, Result} | Acc],
		 lists:delete(<<"pubsub#purge_offline">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#purge_offline">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#purge_offline">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#purge_offline">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#purge_offline">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#purge_offline">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var =
			 <<"pubsub#roster_groups_allowed">>,
		     values = Values}
	| Fs],
       Acc, Required) ->
    try [Value || Value <- Values] of
      Result ->
	  decode(Fs, [{roster_groups_allowed, Result} | Acc],
		 lists:delete(<<"pubsub#roster_groups_allowed">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#roster_groups_allowed">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#send_last_published_item">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_enum(Value,
		 [never, on_sub, on_sub_and_presence])
    of
      Result ->
	  decode(Fs, [{send_last_published_item, Result} | Acc],
		 lists:delete(<<"pubsub#send_last_published_item">>,
			      Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#send_last_published_item">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var =
			 <<"pubsub#send_last_published_item">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var =
			      <<"pubsub#send_last_published_item">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var =
			 <<"pubsub#send_last_published_item">>}
	| _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#send_last_published_item">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#tempsub">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{tempsub, Result} | Acc],
		 lists:delete(<<"pubsub#tempsub">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#tempsub">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#tempsub">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#tempsub">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#tempsub">>} | _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#tempsub">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#subscribe">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try dec_bool(Value) of
      Result ->
	  decode(Fs, [{subscribe, Result} | Acc],
		 lists:delete(<<"pubsub#subscribe">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#subscribe">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#subscribe">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#subscribe">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#subscribe">>} | _],
       _, _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#subscribe">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#title">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{title, Result} | Acc],
		 lists:delete(<<"pubsub#title">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#title">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#title">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#title">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#title">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#title">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = <<"pubsub#type">>,
		     values = [Value]}
	| Fs],
       Acc, Required) ->
    try Value of
      Result ->
	  decode(Fs, [{type, Result} | Acc],
		 lists:delete(<<"pubsub#type">>, Required))
    catch
      _:_ ->
	  erlang:error({?MODULE,
			{bad_var_value, <<"pubsub#type">>,
			 <<"http://jabber.org/protocol/pubsub#node_config">>}})
    end;
decode([#xdata_field{var = <<"pubsub#type">>,
		     values = []} =
	    F
	| Fs],
       Acc, Required) ->
    decode([F#xdata_field{var = <<"pubsub#type">>,
			  values = [<<>>]}
	    | Fs],
	   Acc, Required);
decode([#xdata_field{var = <<"pubsub#type">>} | _], _,
       _) ->
    erlang:error({?MODULE,
		  {too_many_values, <<"pubsub#type">>,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([#xdata_field{var = Var} | Fs], Acc, Required) ->
    if Var /= <<"FORM_TYPE">> ->
	   erlang:error({?MODULE,
			 {unknown_var, Var,
			  <<"http://jabber.org/protocol/pubsub#node_config">>}});
       true -> decode(Fs, Acc, Required)
    end;
decode([], _, [Var | _]) ->
    erlang:error({?MODULE,
		  {missing_required_var, Var,
		   <<"http://jabber.org/protocol/pubsub#node_config">>}});
decode([], Acc, []) -> Acc.

encode_access_model(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"Subscription requests must be approved "
						 "and only subscribers may retrieve items">>),
				 value = <<"authorize">>},
		   #xdata_option{label =
				     Translate(<<"Anyone may subscribe and retrieve items">>),
				 value = <<"open">>},
		   #xdata_option{label =
				     Translate(<<"Anyone with a presence subscription "
						 "of both or from may subscribe and retrieve "
						 "items">>),
				 value = <<"presence">>},
		   #xdata_option{label =
				     Translate(<<"Anyone in the specified roster group(s) "
						 "may subscribe and retrieve items">>),
				 value = <<"roster">>},
		   #xdata_option{label =
				     Translate(<<"Only those on a whitelist may subscribe "
						 "and retrieve items">>),
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

encode_body_xslt(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#body_xslt">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The URL of an XSL transformation which "
				 "can be applied to payloads in order "
				 "to generate an appropriate message body "
				 "element.">>)}.

encode_children_association_policy(Value, Options,
				   Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"Anyone may associate leaf nodes with "
						 "the collection">>),
				 value = <<"all">>},
		   #xdata_option{label =
				     Translate(<<"Only collection node owners may associate "
						 "leaf nodes with the collection">>),
				 value = <<"owners">>},
		   #xdata_option{label =
				     Translate(<<"Only those on a whitelist may associate "
						 "leaf nodes with the collection">>),
				 value = <<"whitelist">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var =
		     <<"pubsub#children_association_policy">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Who may associate leaf nodes with a "
				 "collection">>)}.

encode_children_association_whitelist(Value,
				      Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [enc_jid(V) || V <- Value]
	     end,
    Opts = [],
    #xdata_field{var =
		     <<"pubsub#children_association_whitelist">>,
		 values = Values, required = false, type = 'jid-multi',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The list of JIDs that may associate "
				 "leaf nodes with a collection">>)}.

encode_children(Value, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#children">>,
		 values = Values, required = false, type = 'text-multi',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The child nodes (leaf or collection) "
				 "associated with a collection">>)}.

encode_children_max(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#children_max">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The maximum number of child nodes that "
				 "can be associated with a collection">>)}.

encode_collection(Value, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#collection">>,
		 values = Values, required = false, type = 'text-multi',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The collections with which a node is "
				 "affiliated">>)}.

encode_contact(Value, Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [enc_jid(V) || V <- Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#contact">>,
		 values = Values, required = false, type = 'jid-multi',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The JIDs of those to contact with questions">>)}.

encode_dataform_xslt(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#dataform_xslt">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The URL of an XSL transformation which "
				 "can be applied to the payload format "
				 "in order to generate a valid Data Forms "
				 "result that the client could display "
				 "using a generic Data Forms rendering "
				 "engine">>)}.

encode_deliver_notifications(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#deliver_notifications">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Deliver event notifications">>)}.

encode_deliver_payloads(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#deliver_payloads">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Deliver payloads with event notifications">>)}.

encode_description(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#description">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"A description of the node">>)}.

encode_item_expire(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#item_expire">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Number of seconds after which to automaticall"
				 "y purge items">>)}.

encode_itemreply(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"Statically specify a replyto of the "
						 "node owner(s)">>),
				 value = <<"owner">>},
		   #xdata_option{label =
				     Translate(<<"Dynamically specify a replyto of the "
						 "item publisher">>),
				 value = <<"publisher">>},
		   #xdata_option{value = <<"none">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#itemreply">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Whether owners or publisher should receive "
				 "replies to items">>)}.

encode_language(Value, Options, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = if Options == default -> [];
	      true ->
		  [#xdata_option{label = Translate(L), value = V}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#language">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"The default language of the node">>)}.

encode_max_items(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_int(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#max_items">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Max # of items to persist">>)}.

encode_max_payload_size(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_int(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#max_payload_size">>,
		 values = Values, required = false, type = 'text-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Max payload size in bytes">>)}.

encode_node_type(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"The node is a leaf node (default)">>),
				 value = <<"leaf">>},
		   #xdata_option{label =
				     Translate(<<"The node is a collection node">>),
				 value = <<"collection">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#node_type">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Whether the node is a leaf (default) "
				 "or a collection">>)}.

encode_notification_type(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"Messages of type normal">>),
				 value = <<"normal">>},
		   #xdata_option{label =
				     Translate(<<"Messages of type headline">>),
				 value = <<"headline">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#notification_type">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Specify the event message type">>)}.

encode_notify_config(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#notify_config">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Notify subscribers when the node configuratio"
				 "n changes">>)}.

encode_notify_delete(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#notify_delete">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Notify subscribers when the node is "
				 "deleted">>)}.

encode_notify_retract(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#notify_retract">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Notify subscribers when items are removed "
				 "from the node">>)}.

encode_notify_sub(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#notify_sub">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Whether to notify owners about new subscriber"
				 "s and unsubscribes">>)}.

encode_persist_items(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#persist_items">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Persist items to storage">>)}.

encode_presence_based_delivery(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#presence_based_delivery">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Only deliver notifications to available "
				 "users">>)}.

encode_publish_model(Value, Options, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label =
				     Translate(<<"Only publishers may publish">>),
				 value = <<"publishers">>},
		   #xdata_option{label =
				     Translate(<<"Subscribers may publish">>),
				 value = <<"subscribers">>},
		   #xdata_option{label =
				     Translate(<<"Anyone may publish">>),
				 value = <<"open">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#publish_model">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label = Translate(<<"Specify the publisher model">>)}.

encode_purge_offline(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#purge_offline">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Purge all items when the relevant publisher "
				 "goes offline">>)}.

encode_roster_groups_allowed(Value, Options,
			     Translate) ->
    Values = case Value of
	       [] -> [];
	       Value -> [Value]
	     end,
    Opts = if Options == default -> [];
	      true ->
		  [#xdata_option{label = Translate(L), value = V}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var = <<"pubsub#roster_groups_allowed">>,
		 values = Values, required = false, type = 'list-multi',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Roster groups allowed to subscribe">>)}.

encode_send_last_published_item(Value, Options,
				Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_enum(Value)]
	     end,
    Opts = if Options == default ->
		  [#xdata_option{label = Translate(<<"Never">>),
				 value = <<"never">>},
		   #xdata_option{label =
				     Translate(<<"When a new subscription is processed">>),
				 value = <<"on_sub">>},
		   #xdata_option{label =
				     Translate(<<"When a new subscription is processed "
						 "and whenever a subscriber comes online">>),
				 value = <<"on_sub_and_presence">>}];
	      true ->
		  [#xdata_option{label = Translate(L),
				 value = enc_enum(V)}
		   || {L, V} <- Options]
	   end,
    #xdata_field{var =
		     <<"pubsub#send_last_published_item">>,
		 values = Values, required = false, type = 'list-single',
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"When to send the last published item">>)}.

encode_tempsub(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#tempsub">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Whether to make all subscriptions temporary, "
				 "based on subscriber presence">>)}.

encode_subscribe(Value, Translate) ->
    Values = case Value of
	       undefined -> [];
	       Value -> [enc_bool(Value)]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#subscribe">>,
		 values = Values, required = false, type = boolean,
		 options = Opts, desc = <<>>,
		 label =
		     Translate(<<"Whether to allow subscriptions">>)}.

encode_title(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#title">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>,
		 label = Translate(<<"A friendly name for the node">>)}.

encode_type(Value, Translate) ->
    Values = case Value of
	       <<>> -> [];
	       Value -> [Value]
	     end,
    Opts = [],
    #xdata_field{var = <<"pubsub#type">>, values = Values,
		 required = false, type = 'text-single', options = Opts,
		 desc = <<>>,
		 label =
		     Translate(<<"The type of node data, usually specified "
				 "by the namespace of the payload (if "
				 "any)">>)}.
