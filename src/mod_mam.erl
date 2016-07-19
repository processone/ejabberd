%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @doc
%%%      Message Archive Management (XEP-0313)
%%% @end
%%% Created :  4 Jul 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2013-2016   ProcessOne
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
-module(mod_mam).

-protocol({xep, 313, '0.5.1'}).
-protocol({xep, 334, '0.2'}).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, depends/2]).

-export([user_send_packet/4, user_send_packet_strip_tag/4, user_receive_packet/5,
	 process_iq_v0_2/3, process_iq_v0_3/3, disco_sm_features/5,
	 remove_user/2, remove_room/3, mod_opt_type/1, muc_process_iq/4,
	 muc_filter_message/5, message_is_archived/5, delete_old_messages/2,
	 get_commands_spec/0, msg_to_el/4, get_room_config/4, set_room_option/4]).

-include("jlib.hrl").
-include("logger.hrl").
-include("mod_muc_room.hrl").
-include("ejabberd_commands.hrl").
-include("mod_mam.hrl").

-define(DEF_PAGE_SIZE, 50).
-define(MAX_PAGE_SIZE, 250).

-callback init(binary(), gen_mod:opts()) -> any().
-callback remove_user(binary(), binary()) -> any().
-callback remove_room(binary(), binary(), binary()) -> any().
-callback delete_old_messages(binary() | global,
			      erlang:timestamp(),
			      all | chat | groupchat) -> any().
-callback extended_fields() -> [xmlel()].
-callback store(xmlel(), binary(), {binary(), binary()}, chat | groupchat,
		jid(), binary(), recv | send) -> {ok, binary()} | any().
-callback write_prefs(binary(), binary(), #archive_prefs{}, binary()) -> ok | any().
-callback get_prefs(binary(), binary()) -> {ok, #archive_prefs{}} | error.
-callback select(binary(), jid(), jid(),
		 none | erlang:timestamp(),
		 none | erlang:timestamp(),
		 none | ljid() | {text, binary()},
		 none | #rsm_in{},
		 chat | groupchat) ->
    {[{binary(), non_neg_integer(), xmlel()}], boolean(), non_neg_integer()}.

%%%===================================================================
%%% API
%%%===================================================================
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
			     one_queue),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    init_cache(Opts),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_MAM_TMP, ?MODULE, process_iq_v0_2, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_MAM_TMP, ?MODULE, process_iq_v0_2, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_MAM_0, ?MODULE, process_iq_v0_3, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_MAM_0, ?MODULE, process_iq_v0_3, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_MAM_1, ?MODULE, process_iq_v0_3, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_MAM_1, ?MODULE, process_iq_v0_3, IQDisc),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       user_receive_packet, 88),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 88),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
               user_send_packet_strip_tag, 500),
    ejabberd_hooks:add(muc_filter_message, Host, ?MODULE,
		       muc_filter_message, 50),
    ejabberd_hooks:add(muc_process_iq, Host, ?MODULE,
		       muc_process_iq, 50),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       disco_sm_features, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(remove_room, Host, ?MODULE,
		       remove_room, 50),
    ejabberd_hooks:add(get_room_config, Host, ?MODULE,
		       get_room_config, 50),
    ejabberd_hooks:add(set_room_option, Host, ?MODULE,
		       set_room_option, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE,
		       remove_user, 50),
    case gen_mod:get_opt(assume_mam_usage, Opts,
			 fun(if_enabled) -> if_enabled;
			    (on_request) -> on_request;
			    (never) -> never
			 end, never) of
	never ->
	    ok;
	_ ->
	    ejabberd_hooks:add(message_is_archived, Host, ?MODULE,
			       message_is_archived, 50)
    end,
    ejabberd_commands:register_commands(get_commands_spec()),
    ok.

init_cache(Opts) ->
    MaxSize = gen_mod:get_opt(cache_size, Opts,
			      fun(I) when is_integer(I), I>0 -> I end,
			      1000),
    LifeTime = gen_mod:get_opt(cache_life_time, Opts,
			       fun(I) when is_integer(I), I>0 -> I end,
			       timer:hours(1) div 1000),
    cache_tab:new(archive_prefs, [{max_size, MaxSize},
				  {life_time, LifeTime}]).

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet, 88),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  user_receive_packet, 88),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
              user_send_packet_strip_tag, 500),
    ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE,
			  muc_filter_message, 50),
    ejabberd_hooks:delete(muc_process_iq, Host, ?MODULE,
			  muc_process_iq, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MAM_TMP),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_TMP),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MAM_0),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_0),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MAM_1),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_1),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  disco_sm_features, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(remove_room, Host, ?MODULE,
			  remove_room, 50),
    ejabberd_hooks:delete(get_room_config, Host, ?MODULE,
			  get_room_config, 50),
    ejabberd_hooks:delete(set_room_option, Host, ?MODULE,
			  set_room_option, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    case gen_mod:get_module_opt(Host, ?MODULE, assume_mam_usage,
				fun(if_enabled) -> if_enabled;
				   (on_request) -> on_request;
				   (never) -> never
				end, never) of
	never ->
	    ok;
	_ ->
	    ejabberd_hooks:delete(message_is_archived, Host, ?MODULE,
				  message_is_archived, 50)
    end,
    ejabberd_commands:unregister_commands(get_commands_spec()),
    ok.

depends(_Host, _Opts) ->
    [].

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer).

remove_room(LServer, Name, Host) ->
    LName = jid:nodeprep(Name),
    LHost = jid:nameprep(Host),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_room(LServer, LName, LHost).

get_room_config(X, RoomState, _From, Lang) ->
    Config = RoomState#state.config,
    Label = <<"Enable message archiving">>,
    Var = <<"muc#roomconfig_mam">>,
    Val = case Config#config.mam of
	      true -> <<"1">>;
	      _ -> <<"0">>
	  end,
    XField = #xmlel{name = <<"field">>,
		    attrs =
			[{<<"type">>, <<"boolean">>},
			 {<<"label">>, translate:translate(Lang, Label)},
			 {<<"var">>, Var}],
		    children =
			[#xmlel{name = <<"value">>, attrs = [],
				children = [{xmlcdata, Val}]}]},
    X ++ [XField].

set_room_option(_Acc, <<"muc#roomconfig_mam">> = Opt, Vals, Lang) ->
    try
	Val = case Vals of
		  [<<"0">>|_] -> false;
		  [<<"false">>|_] -> false;
		  [<<"1">>|_] -> true;
		  [<<"true">>|_] -> true
	      end,
	{#config.mam, Val}
    catch _:{case_clause, _} ->
	    Txt = <<"Value of '~s' should be boolean">>,
	    ErrTxt = iolist_to_binary(io_lib:format(Txt, [Opt])),
	    {error, ?ERRT_BAD_REQUEST(Lang, ErrTxt)}
    end;
set_room_option(Acc, _Opt, _Vals, _Lang) ->
    Acc.

user_receive_packet(Pkt, C2SState, JID, Peer, To) ->
    LUser = JID#jid.luser,
    LServer = JID#jid.lserver,
    IsBareCopy = is_bare_copy(JID, To),
    case should_archive(Pkt, LServer) of
	true when not IsBareCopy ->
	    NewPkt = strip_my_archived_tag(Pkt, LServer),
	    case store_msg(C2SState, NewPkt, LUser, LServer, Peer, recv) of
		{ok, ID} ->
		    Archived = #xmlel{name = <<"archived">>,
				      attrs = [{<<"by">>, LServer},
					       {<<"xmlns">>, ?NS_MAM_TMP},
					       {<<"id">>, ID}]},
		    StanzaID = #xmlel{name = <<"stanza-id">>,
				      attrs = [{<<"by">>, LServer},
					       {<<"xmlns">>, ?NS_SID_0},
					       {<<"id">>, ID}]},
                    NewEls = [Archived, StanzaID|NewPkt#xmlel.children],
		    NewPkt#xmlel{children = NewEls};
		_ ->
		    NewPkt
	    end;
	_ ->
	    Pkt
    end.

user_send_packet(Pkt, C2SState, JID, Peer) ->
    LUser = JID#jid.luser,
    LServer = JID#jid.lserver,
    case should_archive(Pkt, LServer) of
	true ->
	    NewPkt = strip_my_archived_tag(Pkt, LServer),
	    case store_msg(C2SState, jlib:replace_from_to(JID, Peer, NewPkt),
		      LUser, LServer, Peer, send) of
              {ok, ID} ->
      		    Archived = #xmlel{name = <<"archived">>,
      				      attrs = [{<<"by">>, LServer},
      					       {<<"xmlns">>, ?NS_MAM_TMP},
      					       {<<"id">>, ID}]},
      		    StanzaID = #xmlel{name = <<"stanza-id">>,
      				      attrs = [{<<"by">>, LServer},
      					       {<<"xmlns">>, ?NS_SID_0},
      					       {<<"id">>, ID}]},
                          NewEls = [Archived, StanzaID|NewPkt#xmlel.children],
      		    NewPkt#xmlel{children = NewEls};
            _ ->
                NewPkt
        end;
	false ->
	    Pkt
    end.

user_send_packet_strip_tag(Pkt, _C2SState, JID, _Peer) ->
    LServer = JID#jid.lserver,
    strip_my_archived_tag(Pkt, LServer).

muc_filter_message(Pkt, #state{config = Config} = MUCState,
		   RoomJID, From, FromNick) ->
    if Config#config.mam ->
	    LServer = RoomJID#jid.lserver,
	    NewPkt = strip_my_archived_tag(Pkt, LServer),
	    StorePkt = strip_x_jid_tags(NewPkt),
	    case store_muc(MUCState, StorePkt, RoomJID, From, FromNick) of
		{ok, ID} ->
		    Archived = #xmlel{name = <<"archived">>,
				      attrs = [{<<"by">>, LServer},
					       {<<"xmlns">>, ?NS_MAM_TMP},
					       {<<"id">>, ID}]},
		    StanzaID = #xmlel{name = <<"stanza-id">>,
				      attrs = [{<<"by">>, LServer},
                                               {<<"xmlns">>, ?NS_SID_0},
                                               {<<"id">>, ID}]},
                    NewEls = [Archived, StanzaID|NewPkt#xmlel.children],
                    NewPkt#xmlel{children = NewEls};
		_ ->
		    NewPkt
	    end;
	true ->
	    Pkt
    end.

% Query archive v0.2
process_iq_v0_2(#jid{lserver = LServer} = From,
	       #jid{lserver = LServer} = To,
	       #iq{type = get, sub_el = #xmlel{name = <<"query">>} = SubEl} = IQ) ->
    Fs = parse_query_v0_2(SubEl),
    process_iq(LServer, From, To, IQ, SubEl, Fs, chat);
process_iq_v0_2(From, To, IQ) ->
    process_iq(From, To, IQ).

% Query archive v0.3
process_iq_v0_3(#jid{lserver = LServer} = From,
		#jid{lserver = LServer} = To,
		#iq{type = set, sub_el = #xmlel{name = <<"query">>} = SubEl} = IQ) ->
    process_iq(LServer, From, To, IQ, SubEl, get_xdata_fields(SubEl), chat);
process_iq_v0_3(#jid{lserver = LServer},
		#jid{lserver = LServer},
		#iq{type = get, sub_el = #xmlel{name = <<"query">>}} = IQ) ->
    process_iq(LServer, IQ);
process_iq_v0_3(From, To, IQ) ->
    process_iq(From, To, IQ).

muc_process_iq(#iq{type = set,
		   sub_el = #xmlel{name = <<"query">>,
				   attrs = Attrs} = SubEl} = IQ,
	       MUCState, From, To) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
	NS when NS == ?NS_MAM_0; NS == ?NS_MAM_1 ->
	    muc_process_iq(IQ, MUCState, From, To, get_xdata_fields(SubEl));
	_ ->
	    IQ
    end;
muc_process_iq(#iq{type = get,
		   sub_el = #xmlel{name = <<"query">>,
				   attrs = Attrs} = SubEl} = IQ,
	       MUCState, From, To) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
	?NS_MAM_TMP ->
	    muc_process_iq(IQ, MUCState, From, To, parse_query_v0_2(SubEl));
	NS when NS == ?NS_MAM_0; NS == ?NS_MAM_1 ->
	    LServer = MUCState#state.server_host,
	    process_iq(LServer, IQ);
	_ ->
	    IQ
    end;
muc_process_iq(IQ, _MUCState, _From, _To) ->
    IQ.

get_xdata_fields(SubEl) ->
    case {fxml:get_subtag_with_xmlns(SubEl, <<"x">>, ?NS_XDATA),
	  fxml:get_subtag_with_xmlns(SubEl, <<"set">>, ?NS_RSM)} of
	{#xmlel{} = XData, false} ->
	    jlib:parse_xdata_submit(XData);
	{#xmlel{} = XData, #xmlel{}} ->
	    [{<<"set">>, SubEl} | jlib:parse_xdata_submit(XData)];
	{false, #xmlel{}} ->
	    [{<<"set">>, SubEl}];
	{false, false} ->
	    []
    end.

disco_sm_features(empty, From, To, Node, Lang) ->
    disco_sm_features({result, []}, From, To, Node, Lang);
disco_sm_features({result, OtherFeatures},
		  #jid{luser = U, lserver = S},
		  #jid{luser = U, lserver = S}, <<>>, _Lang) ->
    {result, [?NS_MAM_TMP, ?NS_MAM_0, ?NS_MAM_1 | OtherFeatures]};
disco_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

message_is_archived(true, _C2SState, _Peer, _JID, _Pkt) ->
    true;
message_is_archived(false, C2SState, Peer,
		    #jid{luser = LUser, lserver = LServer}, Pkt) ->
    Res = case gen_mod:get_module_opt(LServer, ?MODULE, assume_mam_usage,
				      fun(if_enabled) -> if_enabled;
					 (on_request) -> on_request;
					 (never) -> never
				      end, never) of
	      if_enabled ->
		  case get_prefs(LUser, LServer) of
		      #archive_prefs{} = P ->
			  {ok, P};
		      error ->
			  error
		  end;
	      on_request ->
		  Mod = gen_mod:db_mod(LServer, ?MODULE),
		  cache_tab:lookup(archive_prefs, {LUser, LServer},
				   fun() ->
					   Mod:get_prefs(LUser, LServer)
				   end);
	      never ->
		  error
	  end,
    case Res of
	{ok, Prefs} ->
	    should_archive(strip_my_archived_tag(Pkt, LServer), LServer)
		andalso should_archive_peer(C2SState, Prefs, Peer);
	error ->
	    false
    end.

delete_old_messages(TypeBin, Days) when TypeBin == <<"chat">>;
					TypeBin == <<"groupchat">>;
					TypeBin == <<"all">> ->
    Diff = Days * 24 * 60 * 60 * 1000000,
    TimeStamp = usec_to_now(p1_time_compat:system_time(micro_seconds) - Diff),
    Type = jlib:binary_to_atom(TypeBin),
    DBTypes = lists:usort(
		lists:map(
		  fun(Host) ->
			  case gen_mod:db_type(Host, ?MODULE) of
			      sql -> {sql, Host};
			      Other -> {Other, global}
			  end
		  end, ?MYHOSTS)),
    Results = lists:map(
		fun({DBType, ServerHost}) ->
			Mod = gen_mod:db_mod(DBType, ?MODULE),
			Mod:delete_old_messages(ServerHost, TimeStamp, Type)
		end, DBTypes),
    case lists:filter(fun(Res) -> Res /= ok end, Results) of
	[] -> ok;
	[NotOk|_] -> NotOk
    end;
delete_old_messages(_TypeBin, _Days) ->
    unsupported_type.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_iq(LServer, #iq{sub_el = #xmlel{attrs = Attrs}} = IQ) ->
    NS = case fxml:get_attr_s(<<"xmlns">>, Attrs) of
	     ?NS_MAM_0 ->
		 ?NS_MAM_0;
	     _ ->
		 ?NS_MAM_1
	 end,
    CommonFields = [#xmlel{name = <<"field">>,
			   attrs = [{<<"type">>, <<"hidden">>},
				    {<<"var">>, <<"FORM_TYPE">>}],
			   children = [#xmlel{name = <<"value">>,
					      children = [{xmlcdata, NS}]}]},
		    #xmlel{name = <<"field">>,
			   attrs = [{<<"type">>, <<"jid-single">>},
				    {<<"var">>, <<"with">>}]},
		    #xmlel{name = <<"field">>,
			   attrs = [{<<"type">>, <<"text-single">>},
				    {<<"var">>, <<"start">>}]},
		    #xmlel{name = <<"field">>,
			   attrs = [{<<"type">>, <<"text-single">>},
				    {<<"var">>, <<"end">>}]}],
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    ExtendedFields = Mod:extended_fields(),
    Fields = ExtendedFields ++ CommonFields,
    Form = #xmlel{name = <<"x">>,
		  attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
		  children = Fields},
    IQ#iq{type = result,
	  sub_el = [#xmlel{name = <<"query">>,
			   attrs = [{<<"xmlns">>, NS}],
			   children = [Form]}]}.

% Preference setting (both v0.2 & v0.3)
process_iq(#jid{luser = LUser, lserver = LServer},
	   #jid{lserver = LServer},
	   #iq{type = set, lang = Lang, sub_el = #xmlel{name = <<"prefs">>} = SubEl} = IQ) ->
    try {case fxml:get_tag_attr_s(<<"default">>, SubEl) of
	    <<"always">> -> always;
	    <<"never">> -> never;
	    <<"roster">> -> roster
	    end,
	    lists:foldl(
		fun(#xmlel{name = <<"always">>, children = Els}, {A, N}) ->
			{get_jids(Els) ++ A, N};
		    (#xmlel{name = <<"never">>, children = Els}, {A, N}) ->
			{A, get_jids(Els) ++ N};
		    (_, {A, N}) ->
			{A, N}
		end, {[], []}, SubEl#xmlel.children)} of
	{Default, {Always0, Never0}} ->
	    Always = lists:usort(Always0),
	    Never = lists:usort(Never0),
	    case write_prefs(LUser, LServer, LServer, Default, Always, Never) of
		ok ->
		    NewPrefs = prefs_el(Default, Always, Never, IQ#iq.xmlns),
		    IQ#iq{type = result, sub_el = [NewPrefs]};
		_Err ->
		    Txt = <<"Database failure">>,
		    IQ#iq{type = error,
			sub_el = [SubEl, ?ERRT_INTERNAL_SERVER_ERROR(Lang, Txt)]}
	    end
    catch _:_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end;
process_iq(#jid{luser = LUser, lserver = LServer},
	   #jid{lserver = LServer},
	   #iq{type = get, sub_el = #xmlel{name = <<"prefs">>}} = IQ) ->
    Prefs = get_prefs(LUser, LServer),
    PrefsEl = prefs_el(Prefs#archive_prefs.default,
		       Prefs#archive_prefs.always,
		       Prefs#archive_prefs.never,
		       IQ#iq.xmlns),
    IQ#iq{type = result, sub_el = [PrefsEl]};
process_iq(_, _, #iq{sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

process_iq(LServer, #jid{luser = LUser} = From, To, IQ, SubEl, Fs, MsgType) ->
    case MsgType of
	chat ->
	    maybe_activate_mam(LUser, LServer);
	{groupchat, _Role, _MUCState} ->
	    ok
    end,
    case catch lists:foldl(
		 fun({<<"start">>, [Data|_]}, {_, End, With, RSM}) ->
			 {{_, _, _} = jlib:datetime_string_to_timestamp(Data),
			  End, With, RSM};
		    ({<<"end">>, [Data|_]}, {Start, _, With, RSM}) ->
			 {Start,
			  {_, _, _} = jlib:datetime_string_to_timestamp(Data),
			  With, RSM};
		    ({<<"with">>, [Data|_]}, {Start, End, _, RSM}) ->
			 {Start, End, jid:tolower(jid:from_string(Data)), RSM};
		    ({<<"withtext">>, [Data|_]}, {Start, End, _, RSM}) ->
			 {Start, End, {text, Data}, RSM};
		    ({<<"set">>, El}, {Start, End, With, _}) ->
			 {Start, End, With, jlib:rsm_decode(El)};
		    (_, Acc) ->
			 Acc
		 end, {none, [], none, none}, Fs) of
	{'EXIT', _} ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]};
	{_Start, _End, _With, #rsm_in{index = Index}} when is_integer(Index) ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]};
	{Start, End, With, RSM} ->
	    NS = fxml:get_tag_attr_s(<<"xmlns">>, SubEl),
	    select_and_send(LServer, From, To, Start, End,
			    With, limit_max(RSM, NS), IQ, MsgType)
    end.

muc_process_iq(#iq{lang = Lang, sub_el = SubEl} = IQ, MUCState, From, To, Fs) ->
    case may_enter_room(From, MUCState) of
	true ->
	    LServer = MUCState#state.server_host,
	    Role = mod_muc_room:get_role(From, MUCState),
	    process_iq(LServer, From, To, IQ, SubEl, Fs,
		       {groupchat, Role, MUCState});
	false ->
	    Text = <<"Only members may query archives of this room">>,
	    Error = ?ERRT_FORBIDDEN(Lang, Text),
	    IQ#iq{type = error, sub_el = [SubEl, Error]}
    end.

parse_query_v0_2(Query) ->
    lists:flatmap(
      fun (#xmlel{name = <<"start">>} = El) ->
	      [{<<"start">>, [fxml:get_tag_cdata(El)]}];
	  (#xmlel{name = <<"end">>} = El) ->
	      [{<<"end">>, [fxml:get_tag_cdata(El)]}];
	  (#xmlel{name = <<"with">>} = El) ->
	      [{<<"with">>, [fxml:get_tag_cdata(El)]}];
	  (#xmlel{name = <<"withtext">>} = El) ->
	      [{<<"withtext">>, [fxml:get_tag_cdata(El)]}];
	  (#xmlel{name = <<"set">>}) ->
	      [{<<"set">>, Query}];
	  (_) ->
	     []
      end, Query#xmlel.children).

should_archive(#xmlel{name = <<"message">>} = Pkt, LServer) ->
    case fxml:get_attr_s(<<"type">>, Pkt#xmlel.attrs) of
	<<"error">> ->
	    false;
	<<"groupchat">> ->
	    false;
	_ ->
	    case is_resent(Pkt, LServer) of
		true ->
		    false;
		false ->
		    case check_store_hint(Pkt) of
			store ->
			    true;
			no_store ->
			    false;
			none ->
			    case fxml:get_subtag_cdata(Pkt, <<"body">>) of
				<<>> ->
				    %% Empty body
				    false;
				_ ->
				    true
			    end
		    end
	    end
    end;
should_archive(#xmlel{}, _LServer) ->
    false.

strip_my_archived_tag(Pkt, LServer) ->
    NewEls = lists:filter(
	    fun(#xmlel{name = Tag, attrs = Attrs})
			when Tag == <<"archived">>; Tag == <<"stanza-id">> ->
		    case catch jid:nameprep(
			    fxml:get_attr_s(
				<<"by">>, Attrs)) of
			LServer ->
			    false;
			_ ->
			    true
		    end;
		(_) ->
		    true
	    end, Pkt#xmlel.children),
    Pkt#xmlel{children = NewEls}.

strip_x_jid_tags(Pkt) ->
    NewEls = lists:filter(
	      fun(#xmlel{name = <<"x">>} = XEl) ->
		      not lists:any(fun(ItemEl) ->
					    fxml:get_tag_attr(<<"jid">>, ItemEl)
					      /= false
				    end, fxml:get_subtags(XEl, <<"item">>));
		 (_) ->
		      true
	      end, Pkt#xmlel.children),
    Pkt#xmlel{children = NewEls}.

should_archive_peer(C2SState,
		    #archive_prefs{default = Default,
				   always = Always,
				   never = Never},
		    Peer) ->
    LPeer = jid:tolower(Peer),
    case lists:member(LPeer, Always) of
	true ->
	    true;
	false ->
	    case lists:member(LPeer, Never) of
		true ->
		    false;
		false ->
		    case Default of
			always -> true;
			never -> false;
			roster ->
			    case ejabberd_c2s:get_subscription(
				   LPeer, C2SState) of
				both -> true;
				from -> true;
				to -> true;
				_ -> false
			    end
		    end
	    end
    end.

should_archive_muc(Pkt) ->
    case fxml:get_attr_s(<<"type">>, Pkt#xmlel.attrs) of
	<<"groupchat">> ->
	    case check_store_hint(Pkt) of
		store ->
		    true;
		no_store ->
		    false;
		none ->
		    case fxml:get_subtag_cdata(Pkt, <<"body">>) of
			<<>> ->
			    case fxml:get_subtag_cdata(Pkt, <<"subject">>) of
				<<>> ->
				    false;
				_ ->
				    true
			    end;
			_ ->
			    true
		    end
	    end;
	_ ->
	    false
    end.

check_store_hint(Pkt) ->
    case has_store_hint(Pkt) of
	true ->
	    store;
	false ->
	    case has_no_store_hint(Pkt) of
		true ->
		    no_store;
		false ->
		    none
	    end
    end.

has_store_hint(Message) ->
    fxml:get_subtag_with_xmlns(Message, <<"store">>, ?NS_HINTS)
      /= false.

has_no_store_hint(Message) ->
    fxml:get_subtag_with_xmlns(Message, <<"no-store">>, ?NS_HINTS)
      /= false orelse
    fxml:get_subtag_with_xmlns(Message, <<"no-storage">>, ?NS_HINTS)
      /= false orelse
    fxml:get_subtag_with_xmlns(Message, <<"no-permanent-store">>, ?NS_HINTS)
      /= false orelse
    fxml:get_subtag_with_xmlns(Message, <<"no-permanent-storage">>, ?NS_HINTS)
      /= false.

is_resent(Pkt, LServer) ->
    case fxml:get_subtag_with_xmlns(Pkt, <<"stanza-id">>, ?NS_SID_0) of
	#xmlel{attrs = Attrs} ->
	    case fxml:get_attr(<<"by">>, Attrs) of
		{value, LServer} ->
		    true;
		_ ->
		    false
	    end;
	false ->
	    false
    end.

may_enter_room(From,
	       #state{config = #config{members_only = false}} = MUCState) ->
    mod_muc_room:get_affiliation(From, MUCState) /= outcast;
may_enter_room(From, MUCState) ->
    mod_muc_room:is_occupant_or_admin(From, MUCState).

store_msg(C2SState, Pkt, LUser, LServer, Peer, Dir) ->
    Prefs = get_prefs(LUser, LServer),
    case should_archive_peer(C2SState, Prefs, Peer) of
	true ->
	    US = {LUser, LServer},
	    Mod = gen_mod:db_mod(LServer, ?MODULE),
	    Mod:store(Pkt, LServer, US, chat, Peer, <<"">>, Dir);
	false ->
	    pass
    end.

store_muc(MUCState, Pkt, RoomJID, Peer, Nick) ->
    case should_archive_muc(Pkt) of
	true ->
	    LServer = MUCState#state.server_host,
	    {U, S, _} = jid:tolower(RoomJID),
	    Mod = gen_mod:db_mod(LServer, ?MODULE),
	    Mod:store(Pkt, LServer, {U, S}, groupchat, Peer, Nick, recv);
	false ->
	    pass
    end.

write_prefs(LUser, LServer, Host, Default, Always, Never) ->
    Prefs = #archive_prefs{us = {LUser, LServer},
			   default = Default,
			   always = Always,
			   never = Never},
    Mod = gen_mod:db_mod(Host, ?MODULE),
    cache_tab:dirty_insert(
      archive_prefs, {LUser, LServer}, Prefs,
      fun() ->  Mod:write_prefs(LUser, LServer, Prefs, Host) end).

get_prefs(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Res = cache_tab:lookup(archive_prefs, {LUser, LServer},
			   fun() -> Mod:get_prefs(LUser, LServer) end),
    case Res of
	{ok, Prefs} ->
	    Prefs;
	error ->
	    ActivateOpt = gen_mod:get_module_opt(
			    LServer, ?MODULE, request_activates_archiving,
			    fun(B) when is_boolean(B) -> B end, false),
	    case ActivateOpt of
		true ->
		    #archive_prefs{us = {LUser, LServer}, default = never};
		false ->
		    Default = gen_mod:get_module_opt(
				LServer, ?MODULE, default,
				fun(always) -> always;
				   (never) -> never;
				   (roster) -> roster
				end, never),
		    #archive_prefs{us = {LUser, LServer}, default = Default}
	    end
    end.

prefs_el(Default, Always, Never, NS) ->
    Default1 = jlib:atom_to_binary(Default),
    JFun = fun(L) ->
		   [#xmlel{name = <<"jid">>,
			   children = [{xmlcdata, jid:to_string(J)}]}
		    || J <- L]
	   end,
    Always1 = #xmlel{name = <<"always">>,
		     children = JFun(Always)},
    Never1 = #xmlel{name = <<"never">>,
		    children = JFun(Never)},
    #xmlel{name = <<"prefs">>,
	   attrs = [{<<"xmlns">>, NS},
		    {<<"default">>, Default1}],
	   children = [Always1, Never1]}.

maybe_activate_mam(LUser, LServer) ->
    ActivateOpt = gen_mod:get_module_opt(LServer, ?MODULE,
					 request_activates_archiving,
					 fun(B) when is_boolean(B) -> B end,
					 false),
    case ActivateOpt of
	true ->
	    Mod = gen_mod:db_mod(LServer, ?MODULE),
	    Res = cache_tab:lookup(archive_prefs, {LUser, LServer},
				   fun() ->
					   Mod:get_prefs(LUser, LServer)
				   end),
	    case Res of
		{ok, _Prefs} ->
		    ok;
		error ->
		    Default = gen_mod:get_module_opt(LServer, ?MODULE, default,
						     fun(always) -> always;
							(never) -> never;
							(roster) -> roster
						     end, never),
		    write_prefs(LUser, LServer, LServer, Default, [], [])
	    end;
	false ->
	    ok
    end.

select_and_send(LServer, From, To, Start, End, With, RSM, IQ, MsgType) ->
    {Msgs, IsComplete, Count} = select_and_start(LServer, From, To, Start, End,
						 With, RSM, MsgType),
    SortedMsgs = lists:keysort(2, Msgs),
    send(From, To, SortedMsgs, RSM, Count, IsComplete, IQ).

select_and_start(LServer, From, To, Start, End, With, RSM, MsgType) ->
    case MsgType of
	chat ->
	    select(LServer, From, From, Start, End, With, RSM, MsgType);
	{groupchat, _Role, _MUCState} ->
	    select(LServer, From, To, Start, End, With, RSM, MsgType)
    end.

select(_LServer, JidRequestor, JidArchive, Start, End, _With, RSM,
       {groupchat, _Role, #state{config = #config{mam = false},
				 history = History}} = MsgType) ->
    #lqueue{len = L, queue = Q} = History,
    {Msgs0, _} =
	lists:mapfoldl(
	  fun({Nick, Pkt, _HaveSubject, UTCDateTime, _Size}, I) ->
		  Now = datetime_to_now(UTCDateTime, I),
		  TS = now_to_usec(Now),
		  case match_interval(Now, Start, End) and
		      match_rsm(Now, RSM) of
		      true ->
			  {[{jlib:integer_to_binary(TS), TS,
			     msg_to_el(#archive_msg{
					  type = groupchat,
					  timestamp = Now,
					  peer = undefined,
					  nick = Nick,
					  packet = Pkt},
				       MsgType, JidRequestor, JidArchive)}],
			   I+1};
		      false ->
			  {[], I+1}
		  end
	  end, 0, queue:to_list(Q)),
    Msgs = lists:flatten(Msgs0),
    case RSM of
	#rsm_in{max = Max, direction = before} ->
	    {NewMsgs, IsComplete} = filter_by_max(lists:reverse(Msgs), Max),
	    {NewMsgs, IsComplete, L};
	#rsm_in{max = Max} ->
	    {NewMsgs, IsComplete} = filter_by_max(Msgs, Max),
	    {NewMsgs, IsComplete, L};
	_ ->
	    {Msgs, true, L}
    end;
select(LServer, JidRequestor, JidArchive, Start, End, With, RSM, MsgType) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:select(LServer, JidRequestor, JidArchive, Start, End, With, RSM,
	       MsgType).

msg_to_el(#archive_msg{timestamp = TS, packet = Pkt1, nick = Nick, peer = Peer},
	  MsgType, JidRequestor, #jid{lserver = LServer} = JidArchive) ->
    Pkt2 = maybe_update_from_to(Pkt1, JidRequestor, JidArchive, Peer, MsgType,
				Nick),
    Pkt3 = #xmlel{name = <<"forwarded">>,
		  attrs = [{<<"xmlns">>, ?NS_FORWARD}],
		  children = [fxml:replace_tag_attr(
				<<"xmlns">>, <<"jabber:client">>, Pkt2)]},
    jlib:add_delay_info(Pkt3, LServer, TS).

maybe_update_from_to(#xmlel{children = Els} = Pkt, JidRequestor, JidArchive,
		     Peer, {groupchat, Role,
			    #state{config = #config{anonymous = Anon}}},
		     Nick) ->
    ExposeJID = case {Peer, JidRequestor} of
		    {undefined, _JidRequestor} ->
			false;
		    {{U, S, _R}, #jid{luser = U, lserver = S}} ->
			true;
		    {_Peer, _JidRequestor} when not Anon; Role == moderator ->
			true;
		    {_Peer, _JidRequestor} ->
			false
		end,
    Items = case ExposeJID of
		true ->
		    [#xmlel{name = <<"x">>,
			    attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
			    children =
				[#xmlel{name = <<"item">>,
					attrs = [{<<"jid">>,
						  jid:to_string(Peer)}]}]}];
		false ->
		    []
	    end,
    Pkt1 = Pkt#xmlel{children = Items ++ Els},
    Pkt2 = jlib:replace_from(jid:replace_resource(JidArchive, Nick), Pkt1),
    jlib:remove_attr(<<"to">>, Pkt2);
maybe_update_from_to(Pkt, _JidRequestor, _JidArchive, _Peer, chat, _Nick) ->
    Pkt.

is_bare_copy(#jid{luser = U, lserver = S, lresource = R}, To) ->
    PrioRes = ejabberd_sm:get_user_present_resources(U, S),
    MaxRes = case catch lists:max(PrioRes) of
		 {_Prio, Res} when is_binary(Res) ->
		     Res;
		 _ ->
		     undefined
	     end,
    IsBareTo = case To of
		   #jid{lresource = <<"">>} ->
		       true;
		   #jid{lresource = LRes} ->
		       %% Unavailable resources are handled like bare JIDs.
		       lists:keyfind(LRes, 2, PrioRes) =:= false
	       end,
    case {IsBareTo, R} of
	{true, MaxRes} ->
	    ?DEBUG("Recipient of message to bare JID has top priority: ~s@~s/~s",
		   [U, S, R]),
	    false;
	{true, _R} ->
	    %% The message was sent to our bare JID, and we currently have
	    %% multiple resources with the same highest priority, so the session
	    %% manager routes the message to each of them. We store the message
	    %% only from the resource where R equals MaxRes.
	    ?DEBUG("Additional recipient of message to bare JID: ~s@~s/~s",
		   [U, S, R]),
	    true;
	{false, _R} ->
	    false
    end.

send(From, To, Msgs, RSM, Count, IsComplete, #iq{sub_el = SubEl} = IQ) ->
    QID = fxml:get_tag_attr_s(<<"queryid">>, SubEl),
    NS = fxml:get_tag_attr_s(<<"xmlns">>, SubEl),
    QIDAttr = if QID /= <<>> ->
		      [{<<"queryid">>, QID}];
		 true ->
		    []
	      end,
    CompleteAttr = if NS == ?NS_MAM_TMP ->
			   [];
		      NS == ?NS_MAM_0; NS == ?NS_MAM_1 ->
			   [{<<"complete">>, jlib:atom_to_binary(IsComplete)}]
		   end,
    Els = lists:map(
	    fun({ID, _IDInt, El}) ->
		    #xmlel{name = <<"message">>,
			   children = [#xmlel{name = <<"result">>,
					      attrs = [{<<"xmlns">>, NS},
						       {<<"id">>, ID}|QIDAttr],
					      children = [El]}]}
	    end, Msgs),
    RSMOut = make_rsm_out(Msgs, RSM, Count, QIDAttr ++ CompleteAttr, NS),
    if NS == ?NS_MAM_TMP; NS == ?NS_MAM_1 ->
	    lists:foreach(
	      fun(El) ->
		      ejabberd_router:route(To, From, El)
	      end, Els),
	    IQ#iq{type = result, sub_el = RSMOut};
       NS == ?NS_MAM_0 ->
	    ejabberd_router:route(
	      To, From, jlib:iq_to_xml(IQ#iq{type = result, sub_el = []})),
	    lists:foreach(
	      fun(El) ->
		      ejabberd_router:route(To, From, El)
	      end, Els),
	    ejabberd_router:route(
	      To, From, #xmlel{name = <<"message">>,
			       children = RSMOut}),
	    ignore
    end.

make_rsm_out([], _, Count, Attrs, NS) ->
    Tag = if NS == ?NS_MAM_TMP -> <<"query">>;
	     true -> <<"fin">>
	  end,
    [#xmlel{name = Tag, attrs = [{<<"xmlns">>, NS}|Attrs],
	    children = jlib:rsm_encode(#rsm_out{count = Count})}];
make_rsm_out([{FirstID, _, _}|_] = Msgs, _, Count, Attrs, NS) ->
    {LastID, _, _} = lists:last(Msgs),
    Tag = if NS == ?NS_MAM_TMP -> <<"query">>;
	     true -> <<"fin">>
	  end,
    [#xmlel{name = Tag, attrs = [{<<"xmlns">>, NS}|Attrs],
	    children = jlib:rsm_encode(
			 #rsm_out{first = FirstID, count = Count,
				  last = LastID})}].

filter_by_max(Msgs, undefined) ->
    {Msgs, true};
filter_by_max(Msgs, Len) when is_integer(Len), Len >= 0 ->
    {lists:sublist(Msgs, Len), length(Msgs) =< Len};
filter_by_max(_Msgs, _Junk) ->
    {[], true}.

limit_max(RSM, ?NS_MAM_TMP) ->
    RSM; % XEP-0313 v0.2 doesn't require clients to support RSM.
limit_max(none, _NS) ->
    #rsm_in{max = ?DEF_PAGE_SIZE};
limit_max(#rsm_in{max = Max} = RSM, _NS) when not is_integer(Max) ->
    RSM#rsm_in{max = ?DEF_PAGE_SIZE};
limit_max(#rsm_in{max = Max} = RSM, _NS) when Max > ?MAX_PAGE_SIZE ->
    RSM#rsm_in{max = ?MAX_PAGE_SIZE};
limit_max(RSM, _NS) ->
    RSM.

match_interval(Now, Start, End) ->
    (Now >= Start) and (Now =< End).

match_rsm(Now, #rsm_in{id = ID, direction = aft}) when ID /= <<"">> ->
    Now1 = (catch usec_to_now(jlib:binary_to_integer(ID))),
    Now > Now1;
match_rsm(Now, #rsm_in{id = ID, direction = before}) when ID /= <<"">> ->
    Now1 = (catch usec_to_now(jlib:binary_to_integer(ID))),
    Now < Now1;
match_rsm(_Now, _) ->
    true.

now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.

usec_to_now(Int) ->
    Secs = Int div 1000000,
    USec = Int rem 1000000,
    MSec = Secs div 1000000,
    Sec = Secs rem 1000000,
    {MSec, Sec, USec}.

datetime_to_now(DateTime, USecs) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime) -
	calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    {Seconds div 1000000, Seconds rem 1000000, USecs}.

get_jids(Els) ->
    lists:flatmap(
      fun(#xmlel{name = <<"jid">>} = El) ->
	      J = jid:from_string(fxml:get_tag_cdata(El)),
	      [jid:tolower(jid:remove_resource(J)),
	       jid:tolower(J)];
	 (_) ->
	      []
      end, Els).

get_commands_spec() ->
    [#ejabberd_commands{name = delete_old_mam_messages, tags = [purge],
			desc = "Delete MAM messages older than DAYS",
			longdesc = "Valid message TYPEs: "
				   "\"chat\", \"groupchat\", \"all\".",
			module = ?MODULE, function = delete_old_messages,
			args = [{type, binary}, {days, integer}],
			result = {res, rescode}}].

mod_opt_type(assume_mam_usage) ->
    fun(if_enabled) -> if_enabled;
       (on_request) -> on_request;
       (never) -> never
    end;
mod_opt_type(cache_life_time) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(cache_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(default) ->
    fun (always) -> always;
	(never) -> never;
	(roster) -> roster
    end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(request_activates_archiving) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) ->
    [assume_mam_usage, cache_life_time, cache_size, db_type, default, iqdisc,
     request_activates_archiving].
