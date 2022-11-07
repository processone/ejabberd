%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Sep 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------
-module(mod_avatar).
-behaviour(gen_mod).
-protocol({xep, 398, '0.2.0', '17.09', "", ""}).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1, mod_options/1]).
-export([mod_doc/0]).
%% Hooks
-export([pubsub_publish_item/6, vcard_iq_convert/1, vcard_iq_publish/1,
	 get_sm_features/5]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("pubsub.hrl").
-include("translate.hrl").

-type avatar_id_meta() :: #{avatar_meta => {binary(), avatar_meta()}}.
-opaque convert_rule() :: {default | eimp:img_type(), eimp:img_type()}.
-export_type([convert_rule/0]).

%%%===================================================================
%%% API
%%%===================================================================
start(Host, _Opts) ->
    ejabberd_hooks:add(pubsub_publish_item, Host, ?MODULE,
		       pubsub_publish_item, 50),
    ejabberd_hooks:add(vcard_iq_set, Host, ?MODULE,
		       vcard_iq_convert, 30),
    ejabberd_hooks:add(vcard_iq_set, Host, ?MODULE,
		       vcard_iq_publish, 100),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       get_sm_features, 50).

stop(Host) ->
    ejabberd_hooks:delete(pubsub_publish_item, Host, ?MODULE,
			  pubsub_publish_item, 50),
    ejabberd_hooks:delete(vcard_iq_set, Host, ?MODULE, vcard_iq_convert, 30),
    ejabberd_hooks:delete(vcard_iq_set, Host, ?MODULE, vcard_iq_publish, 100),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  get_sm_features, 50).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [{mod_vcard, hard}, {mod_vcard_xupdate, hard}, {mod_pubsub, hard}].

%%%===================================================================
%%% Hooks
%%%===================================================================
-spec pubsub_publish_item(binary(), binary(), jid(), jid(), binary(), [xmlel()]) -> ok.
pubsub_publish_item(LServer, ?NS_AVATAR_METADATA,
		    #jid{luser = LUser, lserver = LServer} = From,
		    #jid{luser = LUser, lserver = LServer} = Host,
		    ItemId, [Payload|_]) ->
    try xmpp:decode(Payload) of
	#avatar_meta{info = []} ->
	    delete_vcard_avatar(From);
	#avatar_meta{info = Info} ->
	    Rules = mod_avatar_opt:convert(LServer),
	    case get_meta_info(Info, Rules) of
		#avatar_info{type = MimeType, id = ID, url = <<"">>} = I ->
		    case get_avatar_data(Host, ID) of
			{ok, Data} ->
			    Meta = #avatar_meta{info = [I]},
			    Photo = #vcard_photo{type = MimeType,
						 binval = Data},
			    set_vcard_avatar(From, Photo,
					     #{avatar_meta => {ID, Meta}});
			{error, _} ->
			    ok
		    end;
		#avatar_info{type = MimeType, url = URL} ->
		    Photo = #vcard_photo{type = MimeType,
					 extval = URL},
		    set_vcard_avatar(From, Photo, #{})
	    end;
	_ ->
	    ?WARNING_MSG("Invalid avatar metadata of ~ts@~ts published "
			 "with item id ~ts",
			 [LUser, LServer, ItemId])
    catch _:{xmpp_codec, Why} ->
	    ?WARNING_MSG("Failed to decode avatar metadata of ~ts@~ts: ~ts",
			 [LUser, LServer, xmpp:format_error(Why)])
    end;
pubsub_publish_item(_, _, _, _, _, _) ->
    ok.

-spec vcard_iq_convert(iq()) -> iq() | {stop, stanza_error()}.
vcard_iq_convert(#iq{from = From, lang = Lang, sub_els = [VCard]} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case convert_avatar(LUser, LServer, VCard) of
	{ok, MimeType, Data} ->
	    VCard1 = VCard#vcard_temp{
		       photo = #vcard_photo{type = MimeType,
					    binval = Data}},
	    IQ#iq{sub_els = [VCard1]};
	pass ->
	    IQ;
	{error, Reason} ->
	    stop_with_error(Lang, Reason)
    end;
vcard_iq_convert(Acc) ->
    Acc.

-spec vcard_iq_publish(iq()) -> iq() | {stop, stanza_error()}.
vcard_iq_publish(#iq{sub_els = [#vcard_temp{photo = undefined}]} = IQ) ->
    publish_avatar(IQ, #avatar_meta{}, <<>>, <<>>, <<>>);
vcard_iq_publish(#iq{sub_els = [#vcard_temp{
				   photo = #vcard_photo{
					      type = MimeType,
					      binval = Data}}]} = IQ)
  when is_binary(Data), Data /= <<>> ->
    SHA1 = str:sha(Data),
    M = get_avatar_meta(IQ),
    case M of
	{ok, SHA1, _} ->
	    IQ;
	{ok, _ItemID, #avatar_meta{info = Info} = Meta} ->
	    case lists:keyfind(SHA1, #avatar_info.id, Info) of
		#avatar_info{} ->
		    IQ;
		false ->
		    Info1 = lists:filter(
			      fun(#avatar_info{url = URL}) -> URL /= <<"">> end,
			      Info),
		    Meta1 = Meta#avatar_meta{info = Info1},
		    publish_avatar(IQ, Meta1, MimeType, Data, SHA1)
	    end;
	{error, _} ->
	    publish_avatar(IQ, #avatar_meta{}, MimeType, Data, SHA1)
    end;
vcard_iq_publish(Acc) ->
    Acc.

-spec get_sm_features({error, stanza_error()} | empty | {result, [binary()]},
		      jid(), jid(), binary(), binary()) ->
			     {error, stanza_error()} | empty | {result, [binary()]}.
get_sm_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_sm_features(Acc, _From, _To, <<"">>, _Lang) ->
    {result, [?NS_PEP_VCARD_CONVERSION_0 |
	      case Acc of
		  {result, Features} -> Features;
		  empty -> []
	      end]};
get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_meta_info([avatar_info()], [convert_rule()]) -> avatar_info().
get_meta_info(Info, Rules) ->
    case lists:foldl(
	   fun(_, #avatar_info{} = Acc) ->
		   Acc;
	      (#avatar_info{url = URL}, Acc) when URL /= <<"">> ->
		   Acc;
	      (#avatar_info{} = I, _) when Rules == [] ->
		   I;
	      (#avatar_info{type = MimeType} = I, Acc) ->
		   T = decode_mime_type(MimeType),
		   case lists:keymember(T, 2, Rules) of
		       true ->
			   I;
		       false ->
			   case convert_to_type(T, Rules) of
			       undefined ->
				   Acc;
			       _ ->
				   [I|Acc]
			   end
		   end
	   end, [], Info) of
	#avatar_info{} = I -> I;
	[] -> hd(Info);
	Is -> hd(lists:reverse(Is))
    end.

-spec get_avatar_data(jid(), binary()) -> {ok, binary()} |
					  {error,
					   notfound | invalid_data | internal_error}.
get_avatar_data(JID, ItemID) ->
    {LUser, LServer, _} = LBJID = jid:remove_resource(jid:tolower(JID)),
    case mod_pubsub:get_item(LBJID, ?NS_AVATAR_DATA, ItemID) of
	#pubsub_item{payload = [Payload|_]} ->
	    try xmpp:decode(Payload) of
		#avatar_data{data = Data} ->
		    {ok, Data};
		_ ->
		    ?WARNING_MSG("Invalid avatar data detected "
				 "for ~ts@~ts with item id ~ts",
				 [LUser, LServer, ItemID]),
		    {error, invalid_data}
	    catch _:{xmpp_codec, Why} ->
		    ?WARNING_MSG("Failed to decode avatar data for "
				 "~ts@~ts with item id ~ts: ~ts",
				 [LUser, LServer, ItemID,
				  xmpp:format_error(Why)]),
		    {error, invalid_data}
	    end;
	#pubsub_item{payload = []} ->
	    ?WARNING_MSG("Empty avatar data detected "
			 "for ~ts@~ts with item id ~ts",
			 [LUser, LServer, ItemID]),
	    {error, invalid_data};
	{error, #stanza_error{reason = 'item-not-found'}} ->
	    {error, notfound};
	{error, Reason} ->
	    ?WARNING_MSG("Failed to get item for ~ts@~ts at node ~ts "
			 "with item id ~ts: ~p",
			 [LUser, LServer, ?NS_AVATAR_METADATA, ItemID, Reason]),
	    {error, internal_error}
    end.

-spec get_avatar_meta(iq()) -> {ok, binary(), avatar_meta()} |
			       {error,
				notfound | invalid_metadata | internal_error}.
get_avatar_meta(#iq{meta = #{avatar_meta := {ItemID, Meta}}}) ->
    {ok, ItemID, Meta};
get_avatar_meta(#iq{from = JID}) ->
    {LUser, LServer, _} = LBJID = jid:remove_resource(jid:tolower(JID)),
    case mod_pubsub:get_items(LBJID, ?NS_AVATAR_METADATA) of
	[#pubsub_item{itemid = {ItemID, _}, payload = [Payload|_]}|_] ->
	    try xmpp:decode(Payload) of
		#avatar_meta{} = Meta ->
		    {ok, ItemID, Meta};
		_ ->
		    ?WARNING_MSG("Invalid metadata payload detected "
				 "for ~ts@~ts with item id ~ts",
				 [LUser, LServer, ItemID]),
		    {error, invalid_metadata}
	    catch _:{xmpp_codec, Why} ->
		    ?WARNING_MSG("Failed to decode metadata for "
				 "~ts@~ts with item id ~ts: ~ts",
				 [LUser, LServer, ItemID,
				  xmpp:format_error(Why)]),
		    {error, invalid_metadata}
	    end;
	{error, #stanza_error{reason = 'item-not-found'}} ->
	    {error, notfound};
	{error, Reason} ->
	    ?WARNING_MSG("Failed to get items for ~ts@~ts at node ~ts: ~p",
			 [LUser, LServer, ?NS_AVATAR_METADATA, Reason]),
	    {error, internal_error}
    end.

-spec publish_avatar(iq(), avatar_meta(), binary(), binary(), binary()) ->
			    iq() | {stop, stanza_error()}.
publish_avatar(#iq{from = JID} = IQ, Meta, <<>>, <<>>, <<>>) ->
    {_, LServer, _} = LBJID = jid:remove_resource(jid:tolower(JID)),
    case mod_pubsub:publish_item(
	   LBJID, LServer, ?NS_AVATAR_METADATA,
	   JID, <<>>, [xmpp:encode(Meta)]) of
	{result, _} ->
	    IQ;
	{error, StanzaErr} ->
	    {stop, StanzaErr}
    end;
publish_avatar(#iq{from = JID} = IQ, Meta, MimeType, Data, ItemID) ->
    #avatar_meta{info = Info} = Meta,
    {_, LServer, _} = LBJID = jid:remove_resource(jid:tolower(JID)),
    Payload = xmpp:encode(#avatar_data{data = Data}),
    case mod_pubsub:publish_item(
	   LBJID, LServer, ?NS_AVATAR_DATA,
	   JID, ItemID, [Payload]) of
	{result, _} ->
	    {W, H} = case eimp:identify(Data) of
			 {ok, ImgInfo} ->
			     {proplists:get_value(width, ImgInfo),
			      proplists:get_value(height, ImgInfo)};
			 _ ->
			     {undefined, undefined}
		     end,
	    I = #avatar_info{id = ItemID,
			     width = W,
			     height = H,
			     type = MimeType,
			     bytes = size(Data)},
	    Meta1 = Meta#avatar_meta{info = [I|Info]},
	    case mod_pubsub:publish_item(
		   LBJID, LServer, ?NS_AVATAR_METADATA,
		   JID, ItemID, [xmpp:encode(Meta1)]) of
		{result, _} ->
		    IQ;
		{error, StanzaErr} ->
		    ?ERROR_MSG("Failed to publish avatar metadata for ~ts: ~p",
			       [jid:encode(JID), StanzaErr]),
		    {stop, StanzaErr}
	    end;
	{error, #stanza_error{reason = 'not-acceptable'} = StanzaErr} ->
	    ?WARNING_MSG("Failed to publish avatar data for ~ts: ~p",
			 [jid:encode(JID), StanzaErr]),
	    {stop, StanzaErr};
	{error, StanzaErr} ->
	    ?ERROR_MSG("Failed to publish avatar data for ~ts: ~p",
		       [jid:encode(JID), StanzaErr]),
	    {stop, StanzaErr}
    end.

-spec convert_avatar(binary(), binary(), vcard_temp()) ->
			    {ok, binary(), binary()} |
			    {error, eimp:error_reason() | base64_error} |
			    pass.
convert_avatar(LUser, LServer, VCard) ->
    case mod_avatar_opt:convert(LServer) of
	[] ->
	    pass;
	Rules ->
	    case VCard#vcard_temp.photo of
		#vcard_photo{binval = Data} when is_binary(Data) ->
		    convert_avatar(LUser, LServer, Data, Rules);
		_ ->
		    pass
	    end
    end.

-spec convert_avatar(binary(), binary(), binary(), [convert_rule()]) ->
			    {ok, binary(), binary()} |
			    {error, eimp:error_reason()} |
			    pass.
convert_avatar(LUser, LServer, Data, Rules) ->
    Type = get_type(Data),
    NewType = convert_to_type(Type, Rules),
    if NewType == undefined ->
	    pass;
       true ->
	    ?DEBUG("Converting avatar of ~ts@~ts: ~ts -> ~ts",
		   [LUser, LServer, Type, NewType]),
	    RateLimit = mod_avatar_opt:rate_limit(LServer),
	    Opts = [{limit_by, {LUser, LServer}},
		    {rate_limit, RateLimit}],
	    case eimp:convert(Data, NewType, Opts) of
		{ok, NewData} ->
		    {ok, encode_mime_type(NewType), NewData};
		{error, Reason} = Err ->
		    ?ERROR_MSG("Failed to convert avatar of "
			       "~ts@~ts (~ts -> ~ts): ~ts",
			       [LUser, LServer, Type, NewType,
				eimp:format_error(Reason)]),
		    Err
	    end
    end.

-spec set_vcard_avatar(jid(), vcard_photo() | undefined, avatar_id_meta()) -> ok.
set_vcard_avatar(JID, VCardPhoto, Meta) ->
    case get_vcard(JID) of
	{ok, #vcard_temp{photo = VCardPhoto}} ->
	    ok;
	{ok, VCard} ->
	    VCard1 = VCard#vcard_temp{photo = VCardPhoto},
	    IQ = #iq{from = JID, to = JID, id = p1_rand:get_string(),
		     type = set, sub_els = [VCard1], meta = Meta},
	    LServer = JID#jid.lserver,
	    ejabberd_hooks:run_fold(vcard_iq_set, LServer, IQ, []),
	    ok;
	{error, _} ->
	    ok
    end.

-spec delete_vcard_avatar(jid()) -> ok.
delete_vcard_avatar(JID) ->
    set_vcard_avatar(JID, undefined, #{}).

-spec get_vcard(jid()) -> {ok, vcard_temp()} | {error, invalid_vcard}.
get_vcard(#jid{luser = LUser, lserver = LServer}) ->
    VCardEl = case mod_vcard:get_vcard(LUser, LServer) of
		  [El] -> El;
		  _ -> #vcard_temp{}
	      end,
    try xmpp:decode(VCardEl, ?NS_VCARD, []) of
	#vcard_temp{} = VCard ->
	    {ok, VCard};
	_ ->
	    ?ERROR_MSG("Invalid vCard of ~ts@~ts in the database",
		       [LUser, LServer]),
	    {error, invalid_vcard}
    catch _:{xmpp_codec, Why} ->
	    ?ERROR_MSG("Failed to decode vCard of ~ts@~ts: ~ts",
		       [LUser, LServer, xmpp:format_error(Why)]),
	    {error, invalid_vcard}
    end.

-spec stop_with_error(binary(), eimp:error_reason()) ->
			     {stop, stanza_error()}.
stop_with_error(Lang, Reason) ->
    Txt = eimp:format_error(Reason),
    {stop, xmpp:err_internal_server_error(Txt, Lang)}.

-spec get_type(binary()) -> eimp:img_type() | unknown.
get_type(Data) ->
    eimp:get_type(Data).

-spec convert_to_type(eimp:img_type() | unknown, [convert_rule()]) ->
			     eimp:img_type() | undefined.
convert_to_type(unknown, _Rules) ->
    undefined;
convert_to_type(Type, Rules) ->
    case proplists:get_value(Type, Rules) of
	undefined ->
	    proplists:get_value(default, Rules);
	Type ->
	    undefined;
	T ->
	    T
    end.

-spec decode_mime_type(binary()) -> eimp:img_type() | unknown.
decode_mime_type(MimeType) ->
    case str:to_lower(MimeType) of
	<<"image/jpeg">> -> jpeg;
	<<"image/png">> -> png;
	<<"image/webp">> -> webp;
	<<"image/gif">> -> gif;
	_ -> unknown
    end.

-spec encode_mime_type(eimp:img_type()) -> binary().
encode_mime_type(Type) ->
    <<"image/", (atom_to_binary(Type, latin1))/binary>>.

mod_opt_type(convert) ->
    case eimp:supported_formats() of
	[] ->
	    fun(_) -> econf:fail(eimp_error) end;
	Formats ->
	    econf:options(
	      maps:from_list(
		[{Type, econf:enum(Formats)}
		 || Type <- [default|Formats]]))
    end;
mod_opt_type(rate_limit) ->
    econf:pos_int().

-spec mod_options(binary()) -> [{convert, [?MODULE:convert_rule()]} |
				{atom(), any()}].
mod_options(_) ->
    [{rate_limit, 10},
     {convert, []}].

mod_doc() ->
    #{desc =>
          [?T("The purpose of the module is to cope with legacy and modern "
              "XMPP clients posting avatars. The process is described in "
              "https://xmpp.org/extensions/xep-0398.html"
              "[XEP-0398: User Avatar to vCard-Based Avatars Conversion]."), "",
           ?T("Also, the module supports conversion between avatar "
              "image formats on the fly."), "",
           ?T("The module depends on _`mod_vcard`_, _`mod_vcard_xupdate`_ and "
              "_`mod_pubsub`_.")],
      opts =>
          [{convert,
            #{value => "{From: To}",
              desc =>
                  ?T("Defines image conversion rules: the format in 'From' "
                     "will be converted to format in 'To'. The value of 'From' "
                     "can also be 'default', which is match-all rule. NOTE: "
                     "the list of supported formats is detected at compile time "
                     "depending on the image libraries installed in the system."),
              example =>
                    ["convert:",
                     "  webp: jpg",
                     "  default: png"]}},
           {rate_limit,
            #{value => ?T("Number"),
              desc =>
                  ?T("Limit any given JID by the number of avatars it is able "
                     "to convert per minute. This is to protect the server from "
                     "image conversion DoS. The default value is '10'.")}}]}.
