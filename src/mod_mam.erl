%%%-------------------------------------------------------------------
%%% File    : mod_mam.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : Message Archive Management (XEP-0313)
%%% Created :  4 Jul 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2013-2017   ProcessOne
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
-export([start/2, stop/1, reload/3, depends/2]).

-export([user_send_packet/1, user_send_packet_strip_tag/1, user_receive_packet/1,
	 process_iq_v0_2/1, process_iq_v0_3/1, disco_sm_features/5,
	 remove_user/2, remove_room/3, mod_opt_type/1, muc_process_iq/2,
	 muc_filter_message/3, message_is_archived/3, delete_old_messages/2,
	 get_commands_spec/0, msg_to_el/4, get_room_config/4, set_room_option/3,
	 offline_message/1]).

-include("xmpp.hrl").
-include("logger.hrl").
-include("mod_muc_room.hrl").
-include("ejabberd_commands.hrl").
-include("mod_mam.hrl").

-define(DEF_PAGE_SIZE, 50).
-define(MAX_PAGE_SIZE, 250).

-type c2s_state() :: ejabberd_c2s:state().

-callback init(binary(), gen_mod:opts()) -> any().
-callback remove_user(binary(), binary()) -> any().
-callback remove_room(binary(), binary(), binary()) -> any().
-callback delete_old_messages(binary() | global,
			      erlang:timestamp(),
			      all | chat | groupchat) -> any().
-callback extended_fields() -> [mam_query:property() | #xdata_field{}].
-callback store(xmlel(), binary(), {binary(), binary()}, chat | groupchat,
		jid(), binary(), recv | send) -> {ok, binary()} | any().
-callback write_prefs(binary(), binary(), #archive_prefs{}, binary()) -> ok | any().
-callback get_prefs(binary(), binary()) -> {ok, #archive_prefs{}} | error.
-callback select(binary(), jid(), jid(), mam_query:result(),
		 #rsm_set{} | undefined, chat | groupchat) ->
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
    register_iq_handlers(Host, IQDisc),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       user_receive_packet, 88),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 88),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
               user_send_packet_strip_tag, 500),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       offline_message, 40),
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
    case gen_mod:get_opt(assume_mam_usage, Opts,
			 fun(B) when is_boolean(B) -> B end, false) of
	true ->
	    ejabberd_hooks:add(message_is_archived, Host, ?MODULE,
			       message_is_archived, 50);
	false ->
	    ok
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
    unregister_iq_handlers(Host),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet, 88),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  user_receive_packet, 88),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet_strip_tag, 500),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE,
			  offline_message, 40),
    ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE,
			  muc_filter_message, 50),
    ejabberd_hooks:delete(muc_process_iq, Host, ?MODULE,
			  muc_process_iq, 50),
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
    case gen_mod:get_module_opt(Host, ?MODULE, assume_mam_usage,
				fun(B) when is_boolean(B) -> B end, false) of
	true ->
	    ejabberd_hooks:delete(message_is_archived, Host, ?MODULE,
				  message_is_archived, 50);
	false ->
	    ok
    end,
    ejabberd_commands:unregister_commands(get_commands_spec()),
    ok.

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    case gen_mod:is_equal_opt(cache_size, NewOpts, OldOpts,
			      fun(I) when is_integer(I), I>0 -> I end,
                              1000) of
	{false, MaxSize, _} ->
	    cache_tab:setopts(archive_prefs, [{max_size, MaxSize}]);
	true ->
	    ok
    end,
    case gen_mod:is_equal_opt(cache_life_time, NewOpts, OldOpts,
			      fun(I) when is_integer(I), I>0 -> I end,
			      timer:hours(1) div 1000) of
	{false, LifeTime, _} ->
	    cache_tab:setopts(archive_prefs, [{life_time, LifeTime}]);
	true ->
	    ok
    end,
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
	    register_iq_handlers(Host, IQDisc);
	true ->
	    ok
    end,
    case gen_mod:is_equal_opt(assume_mam_usage, NewOpts, OldOpts,
			      fun(B) when is_boolean(B) -> B end, false) of
	{false, true, _} ->
	    ejabberd_hooks:add(message_is_archived, Host, ?MODULE,
			       message_is_archived, 50);
	{false, false, _} ->
	    ejabberd_hooks:delete(message_is_archived, Host, ?MODULE,
				  message_is_archived, 50);
	true ->
	    ok
    end.

depends(_Host, _Opts) ->
    [].

-spec register_iq_handlers(binary(), gen_iq_handler:type()) -> ok.
register_iq_handlers(Host, IQDisc) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MAM_TMP,
				  ?MODULE, process_iq_v0_2, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_TMP,
				  ?MODULE, process_iq_v0_2, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MAM_0,
				  ?MODULE, process_iq_v0_3, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_0, ?MODULE,
				  process_iq_v0_3, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MAM_1,
				  ?MODULE, process_iq_v0_3, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_1,
				  ?MODULE, process_iq_v0_3, IQDisc).

-spec unregister_iq_handlers(binary()) -> ok.
unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MAM_TMP),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_TMP),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MAM_0),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_0),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MAM_1),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_1).

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer),
    cache_tab:dirty_delete(archive_prefs, {LUser, LServer}, fun() -> ok end),
    ok.

-spec remove_room(binary(), binary(), binary()) -> ok.
remove_room(LServer, Name, Host) ->
    LName = jid:nodeprep(Name),
    LHost = jid:nameprep(Host),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_room(LServer, LName, LHost),
    ok.

-spec get_room_config([muc_roomconfig:property()], mod_muc_room:state(),
		      jid(), binary()) -> [muc_roomconfig:property()].
get_room_config(Fields, RoomState, _From, _Lang) ->
    Config = RoomState#state.config,
    Fields ++ [{mam, Config#config.mam}].

-spec set_room_option({pos_integer(), _}, muc_roomconfig:property(), binary())
      -> {pos_integer(), _}.
set_room_option(_Acc, {mam, Val}, _Lang) ->
    {#config.mam, Val};
set_room_option(Acc, _Property, _Lang) ->
    Acc.

-spec user_receive_packet({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_receive_packet({Pkt, #{jid := JID} = C2SState}) ->
    Peer = xmpp:get_from(Pkt),
    LUser = JID#jid.luser,
    LServer = JID#jid.lserver,
    Pkt2 = case should_archive(Pkt, LServer) of
	true ->
		   Pkt1 = strip_my_archived_tag(Pkt, LServer),
		   case store_msg(C2SState, Pkt1, LUser, LServer, Peer, recv) of
		{ok, ID} ->
			   set_stanza_id(Pkt1, JID, ID);
		_ ->
			   Pkt1
	    end;
	_ ->
	    Pkt
	   end,
    {Pkt2, C2SState}.

-spec user_send_packet({stanza(), ejabberd_c2s:state()}) -> {stanza(), ejabberd_c2s:state()}.
user_send_packet({Pkt, #{jid := JID} = C2SState}) ->
    Peer = xmpp:get_to(Pkt),
    LUser = JID#jid.luser,
    LServer = JID#jid.lserver,
    Pkt2 = case should_archive(Pkt, LServer) of
	true ->
		   Pkt1 = strip_my_archived_tag(Pkt, LServer),
		   case store_msg(C2SState, xmpp:set_from_to(Pkt1, JID, Peer),
		      LUser, LServer, Peer, send) of
              {ok, ID} ->
			   set_stanza_id(Pkt1, JID, ID);
            _ ->
			   Pkt1
        end;
	false ->
	    Pkt
	   end,
    {Pkt2, C2SState}.

-spec offline_message({any(), message()}) -> {any(), message()}.
offline_message({_Action, #message{from = Peer, to = To} = Pkt} = Acc) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    case should_archive(Pkt, LServer) of
	true ->
	    Pkt1 = strip_my_archived_tag(Pkt, LServer),
	    case store_msg(undefined, Pkt1, LUser, LServer, Peer, recv) of
		{ok, ID} ->
		    {archived, set_stanza_id(Pkt1, To, ID)};
		_ ->
		    Acc
	    end;
	false ->
	    Acc
    end.

-spec user_send_packet_strip_tag({stanza(), ejabberd_c2s:state()}) ->
					{stanza(), ejabberd_c2s:state()}.
user_send_packet_strip_tag({Pkt, #{jid := JID} = C2SState}) ->
    LServer = JID#jid.lserver,
    {strip_my_archived_tag(Pkt, LServer), C2SState}.

-spec muc_filter_message(message(), mod_muc_room:state(),
			 binary()) -> message().
muc_filter_message(Pkt, #state{config = Config, jid = RoomJID} = MUCState,
		   FromNick) ->
    From = xmpp:get_from(Pkt),
    if Config#config.mam ->
	    LServer = RoomJID#jid.lserver,
	    NewPkt = strip_my_archived_tag(Pkt, LServer),
	    StorePkt = strip_x_jid_tags(NewPkt),
	    case store_muc(MUCState, StorePkt, RoomJID, From, FromNick) of
		{ok, ID} ->
		    set_stanza_id(NewPkt, RoomJID, ID);
		_ ->
		    NewPkt
	    end;
	true ->
	    Pkt
    end.

set_stanza_id(Pkt, JID, ID) ->
    BareJID = jid:remove_resource(JID),
    Archived = #mam_archived{by = BareJID, id = ID},
    StanzaID = #stanza_id{by = BareJID, id = ID},
    NewEls = [Archived, StanzaID|xmpp:get_els(Pkt)],
    xmpp:set_els(Pkt, NewEls).

% Query archive v0.2
process_iq_v0_2(#iq{from = #jid{lserver = LServer},
		    to = #jid{lserver = LServer},
		    type = get, sub_els = [#mam_query{}]} = IQ) ->
    process_iq(LServer, IQ, chat);
process_iq_v0_2(IQ) ->
    process_iq(IQ).

% Query archive v0.3
process_iq_v0_3(#iq{from = #jid{lserver = LServer},
		    to = #jid{lserver = LServer},
		    type = set, sub_els = [#mam_query{}]} = IQ) ->
    process_iq(LServer, IQ, chat);
process_iq_v0_3(#iq{from = #jid{lserver = LServer},
		    to = #jid{lserver = LServer},
		    type = get, sub_els = [#mam_query{}]} = IQ) ->
    process_iq(LServer, IQ);
process_iq_v0_3(IQ) ->
    process_iq(IQ).

-spec muc_process_iq(ignore | iq(), mod_muc_room:state()) -> ignore | iq().
muc_process_iq(#iq{type = T, lang = Lang,
		   from = From,
		   sub_els = [#mam_query{xmlns = NS}]} = IQ,
	       MUCState)
  when (T == set andalso (NS == ?NS_MAM_0 orelse NS == ?NS_MAM_1)) orelse
       (T == get andalso NS == ?NS_MAM_TMP) ->
    case may_enter_room(From, MUCState) of
	true ->
	    LServer = MUCState#state.server_host,
	    Role = mod_muc_room:get_role(From, MUCState),
	    process_iq(LServer, IQ, {groupchat, Role, MUCState});
	false ->
	    Text = <<"Only members may query archives of this room">>,
	    xmpp:make_error(IQ, xmpp:err_forbidden(Text, Lang))
    end;
muc_process_iq(#iq{type = get,
		   sub_els = [#mam_query{xmlns = NS}]} = IQ,
	       MUCState) when NS == ?NS_MAM_0; NS == ?NS_MAM_1 ->
    LServer = MUCState#state.server_host,
    process_iq(LServer, IQ);
muc_process_iq(IQ, _MUCState) ->
    IQ.

parse_query(#mam_query{xmlns = ?NS_MAM_TMP,
		       start = Start, 'end' = End,
		       with = With, withtext = Text}, _Lang) ->
    {ok, [{start, Start}, {'end', End},
	  {with, With}, {withtext, Text}]};
parse_query(#mam_query{xdata = #xdata{}} = Query, Lang) ->
    X = xmpp_util:set_xdata_field(
	  #xdata_field{var = <<"FORM_TYPE">>,
		       type = hidden, values = [?NS_MAM_1]},
	  Query#mam_query.xdata),
    try	mam_query:decode(X#xdata.fields) of
	Form -> {ok, Form}
    catch _:{mam_query, Why} ->
	    Txt = mam_query:format_error(Why),
	    {error, xmpp:err_bad_request(Txt, Lang)}
    end;
parse_query(#mam_query{}, _Lang) ->
    {ok, []}.

disco_sm_features(empty, From, To, Node, Lang) ->
    disco_sm_features({result, []}, From, To, Node, Lang);
disco_sm_features({result, OtherFeatures},
		  #jid{luser = U, lserver = S},
		  #jid{luser = U, lserver = S}, <<"">>, _Lang) ->
    {result, [?NS_MAM_TMP, ?NS_MAM_0, ?NS_MAM_1 | OtherFeatures]};
disco_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec message_is_archived(boolean(), ejabberd_c2s:state(), message()) -> boolean().
message_is_archived(true, _C2SState, _Pkt) ->
    true;
message_is_archived(false, #{jid := JID} = C2SState, Pkt) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    Peer = xmpp:get_from(Pkt),
    case gen_mod:get_module_opt(LServer, ?MODULE, assume_mam_usage,
				fun(B) when is_boolean(B) -> B end, false) of
	true ->
	    should_archive(strip_my_archived_tag(Pkt, LServer), LServer)
		andalso should_archive_peer(C2SState, LUser, LServer,
					    get_prefs(LUser, LServer),
					    Peer);
	false ->
	    false
    end.

delete_old_messages(TypeBin, Days) when TypeBin == <<"chat">>;
					TypeBin == <<"groupchat">>;
					TypeBin == <<"all">> ->
    Diff = Days * 24 * 60 * 60 * 1000000,
    TimeStamp = usec_to_now(p1_time_compat:system_time(micro_seconds) - Diff),
    Type = misc:binary_to_atom(TypeBin),
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

process_iq(LServer, #iq{sub_els = [#mam_query{xmlns = NS}]} = IQ) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    CommonFields = [{with, undefined},
		    {start, undefined},
		    {'end', undefined}],
    ExtendedFields = Mod:extended_fields(),
    Fields = mam_query:encode(CommonFields ++ ExtendedFields),
    X = xmpp_util:set_xdata_field(
	  #xdata_field{var = <<"FORM_TYPE">>, type = hidden, values = [NS]},
	  #xdata{type = form, fields = Fields}),
    xmpp:make_iq_result(IQ, #mam_query{xmlns = NS, xdata = X}).

% Preference setting (both v0.2 & v0.3)
process_iq(#iq{type = set, lang = Lang,
	       sub_els = [#mam_prefs{default = undefined, xmlns = NS}]} = IQ) ->
    Why = {missing_attr, <<"default">>, <<"prefs">>, NS},
    ErrTxt = xmpp:format_error(Why),
    xmpp:make_error(IQ, xmpp:err_bad_request(ErrTxt, Lang));
process_iq(#iq{from = #jid{luser = LUser, lserver = LServer},
	       to = #jid{lserver = LServer},
	       type = set, lang = Lang,
	       sub_els = [#mam_prefs{xmlns = NS,
				     default = Default,
				     always = Always0,
				     never = Never0}]} = IQ) ->
    Always = lists:usort(get_jids(Always0)),
    Never = lists:usort(get_jids(Never0)),
    case write_prefs(LUser, LServer, LServer, Default, Always, Never) of
	ok ->
	    NewPrefs = prefs_el(Default, Always, Never, NS),
	    xmpp:make_iq_result(IQ, NewPrefs);
	_Err ->
	    Txt = <<"Database failure">>,
	    xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang))
    end;
process_iq(#iq{from = #jid{luser = LUser, lserver = LServer},
	       to = #jid{lserver = LServer},
	       type = get, sub_els = [#mam_prefs{xmlns = NS}]} = IQ) ->
    Prefs = get_prefs(LUser, LServer),
    PrefsEl = prefs_el(Prefs#archive_prefs.default,
		       Prefs#archive_prefs.always,
		       Prefs#archive_prefs.never,
		       NS),
    xmpp:make_iq_result(IQ, PrefsEl);
process_iq(IQ) ->
    xmpp:make_error(IQ, xmpp:err_not_allowed()).

process_iq(LServer, #iq{from = #jid{luser = LUser}, lang = Lang,
			sub_els = [SubEl]} = IQ, MsgType) ->
    case MsgType of
	chat ->
	    maybe_activate_mam(LUser, LServer);
	{groupchat, _Role, _MUCState} ->
	    ok
    end,
    case SubEl of
	#mam_query{rsm = #rsm_set{index = I}} when is_integer(I) ->
	    Txt = <<"Unsupported <index/> element">>,
	    xmpp:make_error(IQ, xmpp:err_feature_not_implemented(Txt, Lang));
	#mam_query{rsm = RSM, xmlns = NS} ->
	    case parse_query(SubEl, Lang) of
		{ok, Query} ->
		    NewRSM = limit_max(RSM, NS),
		    select_and_send(LServer, Query, NewRSM, IQ, MsgType);
		{error, Err} ->
		    xmpp:make_error(IQ, Err)
	    end
    end.

should_archive(#message{type = error}, _LServer) ->
    false;
should_archive(#message{meta = #{sm_copy := true}}, _LServer) ->
    false;
should_archive(#message{meta = #{from_offline := true}}, _LServer) ->
    false;
should_archive(#message{body = Body, subject = Subject,
			type = Type} = Pkt, LServer) ->
    case is_resent(Pkt, LServer) of
	true ->
	    false;
	false ->
	    case check_store_hint(Pkt) of
		store ->
		    true;
		no_store ->
		    false;
		none when Type == groupchat; Type == headline ->
		    false;
		none ->
		    xmpp:get_text(Body) /= <<>> orelse
			xmpp:get_text(Subject) /= <<>>
	    end
    end;
should_archive(_, _LServer) ->
    false.

-spec strip_my_archived_tag(stanza(), binary()) -> stanza().
strip_my_archived_tag(Pkt, LServer) ->
    Els = xmpp:get_els(Pkt),
    NewEls = lists:filter(
	       fun(El) ->
		       Name = xmpp:get_name(El),
		       NS = xmpp:get_ns(El),
		       if (Name == <<"archived">> andalso NS == ?NS_MAM_TMP);
			  (Name == <<"stanza-id">> andalso NS == ?NS_SID_0) ->
			       try xmpp:decode(El) of
				   #mam_archived{by = By} ->
				       By#jid.lserver /= LServer;
				   #stanza_id{by = By} ->
				       By#jid.lserver /= LServer
			       catch _:{xmpp_codec, _} ->
				       false
			       end;
			  true ->
			       true
		       end
	       end, Els),
    xmpp:set_els(Pkt, NewEls).

-spec strip_x_jid_tags(stanza()) -> stanza().
strip_x_jid_tags(Pkt) ->
    Els = xmpp:get_els(Pkt),
    NewEls = lists:filter(
	       fun(El) ->
		       case xmpp:get_name(El) of
			   <<"x">> ->
			       NS = xmpp:get_ns(El),
			       Items = if NS == ?NS_MUC_USER;
					  NS == ?NS_MUC_ADMIN;
					  NS == ?NS_MUC_OWNER ->
					       try xmpp:decode(El) of
						   #muc_user{items = Is} -> Is;
						   #muc_admin{items = Is} -> Is;
						   #muc_owner{items = Is} -> Is
					       catch _:{xmpp_codec, _} ->
						       []
					       end;
					  true ->
					       []
				       end,
			       not lists:any(
				     fun(#muc_item{jid = JID}) ->
					     JID /= undefined
				     end, Items);
			   _ ->
			       true
		       end
	       end, Els),
    xmpp:set_els(Pkt, NewEls).

-spec should_archive_peer(c2s_state() | undefined, binary(), binary(),
			  #archive_prefs{}, jid()) -> boolean().
should_archive_peer(C2SState, LUser, LServer,
		    #archive_prefs{default = Default,
				   always = Always,
				   never = Never},
		    Peer) ->
    LPeer = jid:remove_resource(jid:tolower(Peer)),
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
			    Sub = case C2SState of
				      undefined ->
					  {S, _} = ejabberd_hooks:run_fold(
						     roster_get_jid_info,
						     LServer, {none, []},
						     [LUser, LServer, Peer]),
					  S;
				      _ ->
					  ejabberd_c2s:get_subscription(
					    LPeer, C2SState)
				  end,
			    case Sub of
				both -> true;
				from -> true;
				to -> true;
				_ -> false
			    end
		    end
	    end
    end.

should_archive_muc(#message{type = groupchat,
			    body = Body, subject = Subj} = Pkt) ->
    case check_store_hint(Pkt) of
	store ->
	    true;
	no_store ->
	    false;
	none ->
	    case xmpp:get_text(Body) of
		<<"">> ->
		    case xmpp:get_text(Subj) of
			<<"">> ->
			    false;
			_ ->
			    true
		    end;
		_ ->
		    true
	    end
    end;
should_archive_muc(_) ->
    false.

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


-spec has_store_hint(message()) -> boolean().
has_store_hint(Message) ->
    xmpp:has_subtag(Message, #hint{type = 'store'}).

-spec has_no_store_hint(message()) -> boolean().
has_no_store_hint(Message) ->
    xmpp:has_subtag(Message, #hint{type = 'no-store'}) orelse
    xmpp:has_subtag(Message, #hint{type = 'no-storage'}) orelse
    xmpp:has_subtag(Message, #hint{type = 'no-permanent-store'}) orelse
    xmpp:has_subtag(Message, #hint{type = 'no-permanent-storage'}).

-spec is_resent(message(), binary()) -> boolean().
is_resent(Pkt, LServer) ->
    case xmpp:get_subtag(Pkt, #stanza_id{by = #jid{}}) of
	#stanza_id{by = #jid{lserver = LServer}} ->
	    true;
	_ ->
	    false
    end.

may_enter_room(From,
	       #state{config = #config{members_only = false}} = MUCState) ->
    mod_muc_room:get_affiliation(From, MUCState) /= outcast;
may_enter_room(From, MUCState) ->
    mod_muc_room:is_occupant_or_admin(From, MUCState).

-spec store_msg(c2s_state() | undefined, stanza(),
		binary(), binary(), jid(), send | recv) ->
		       {ok, binary()} | pass.
store_msg(C2SState, Pkt, LUser, LServer, Peer, Dir) ->
    Prefs = get_prefs(LUser, LServer),
    case should_archive_peer(C2SState, LUser, LServer, Prefs, Peer) of
	true ->
	    US = {LUser, LServer},
	    case ejabberd_hooks:run_fold(store_mam_message, LServer, Pkt,
					 [LUser, LServer, Peer, chat, Dir]) of
		drop ->
		    pass;
		NewPkt ->
		    Mod = gen_mod:db_mod(LServer, ?MODULE),
		    El = xmpp:encode(NewPkt),
		    Mod:store(El, LServer, US, chat, Peer, <<"">>, Dir)
	    end;
	false ->
	    pass
    end.

store_muc(MUCState, Pkt, RoomJID, Peer, Nick) ->
    case should_archive_muc(Pkt) of
	true ->
	    {U, S, _} = jid:tolower(RoomJID),
	    LServer = MUCState#state.server_host,
	    case ejabberd_hooks:run_fold(store_mam_message, LServer, Pkt,
					 [U, S, Peer, groupchat, recv]) of
		drop ->
		    pass;
		NewPkt ->
		    Mod = gen_mod:db_mod(LServer, ?MODULE),
		    El = xmpp:encode(NewPkt),
		    Mod:store(El, LServer, {U, S}, groupchat, Peer, Nick, recv)
	    end;
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
    #mam_prefs{default = Default,
	       always = [jid:make(LJ) || LJ <- Always],
	       never = [jid:make(LJ) || LJ <- Never],
	       xmlns = NS}.

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

select_and_send(LServer, Query, RSM, #iq{from = From, to = To} = IQ, MsgType) ->
    {Msgs, IsComplete, Count} =
	case MsgType of
	    chat ->
		select(LServer, From, From, Query, RSM, MsgType);
	    {groupchat, _Role, _MUCState} ->
		select(LServer, From, To, Query, RSM, MsgType)
	end,
    SortedMsgs = lists:keysort(2, Msgs),
    send(SortedMsgs, Count, IsComplete, IQ).

select(_LServer, JidRequestor, JidArchive, Query, RSM,
       {groupchat, _Role, #state{config = #config{mam = false},
				 history = History}} = MsgType) ->
    Start = proplists:get_value(start, Query),
    End = proplists:get_value('end', Query),
    #lqueue{queue = Q} = History,
    L = p1_queue:len(Q),
    Msgs =
	lists:flatmap(
	  fun({Nick, Pkt, _HaveSubject, Now, _Size}) ->
		  TS = now_to_usec(Now),
		  case match_interval(Now, Start, End) and
		      match_rsm(Now, RSM) of
		      true ->
			  case msg_to_el(#archive_msg{
					    id = integer_to_binary(TS),
					    type = groupchat,
					    timestamp = Now,
					    peer = undefined,
					    nick = Nick,
					    packet = Pkt},
					 MsgType, JidRequestor, JidArchive) of
			      {ok, Msg} ->
				  [{integer_to_binary(TS), TS, Msg}];
			      {error, _} ->
				  []
			  end;
		      false ->
			  []
		  end
	  end, p1_queue:to_list(Q)),
    case RSM of
	#rsm_set{max = Max, before = Before} when is_binary(Before) ->
	    {NewMsgs, IsComplete} = filter_by_max(lists:reverse(Msgs), Max),
	    {NewMsgs, IsComplete, L};
	#rsm_set{max = Max} ->
	    {NewMsgs, IsComplete} = filter_by_max(Msgs, Max),
	    {NewMsgs, IsComplete, L};
	_ ->
	    {Msgs, true, L}
    end;
select(LServer, JidRequestor, JidArchive, Query, RSM, MsgType) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:select(LServer, JidRequestor, JidArchive, Query, RSM, MsgType).

msg_to_el(#archive_msg{timestamp = TS, packet = El, nick = Nick,
		       peer = Peer, id = ID},
	  MsgType, JidRequestor, #jid{lserver = LServer} = JidArchive) ->
    try xmpp:decode(El, ?NS_CLIENT, [ignore_els]) of
	Pkt1 ->
	    Pkt2 = set_stanza_id(Pkt1, JidArchive, ID),
	    Pkt3 = maybe_update_from_to(
		     Pkt2, JidRequestor, JidArchive, Peer, MsgType, Nick),
	    Delay = #delay{stamp = TS, from = jid:make(LServer)},
	    {ok, #forwarded{xml_els = [xmpp:encode(Pkt3)], delay = Delay}}
    catch _:{xmpp_codec, Why} ->
	    ?ERROR_MSG("Failed to decode raw element ~p from message "
		       "archive of user ~s: ~s",
		       [El, jid:encode(JidArchive), xmpp:format_error(Why)]),
	    {error, invalid_xml}
    end.

maybe_update_from_to(#message{sub_els = Els} = Pkt, JidRequestor, JidArchive,
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
		    [#muc_user{items = [#muc_item{jid = Peer}]}];
		false ->
		    []
	    end,
    Pkt#message{from = jid:replace_resource(JidArchive, Nick),
		to = undefined,
		sub_els = Items ++ Els};
maybe_update_from_to(Pkt, _JidRequestor, _JidArchive, _Peer, chat, _Nick) ->
    Pkt.

-spec send([{binary(), integer(), xmlel()}],
	   non_neg_integer(), boolean(), iq()) -> iq() | ignore.
send(Msgs, Count, IsComplete,
     #iq{from = From, to = To,
	 sub_els = [#mam_query{id = QID, xmlns = NS}]} = IQ) ->
    Hint = #hint{type = 'no-store'},
    Els = lists:map(
	    fun({ID, _IDInt, El}) ->
		    #message{from = To,
			     to = From,
			     sub_els = [#mam_result{xmlns = NS,
						    id = ID,
						    queryid = QID,
						    sub_els = [El]}]}
	    end, Msgs),
    RSMOut = make_rsm_out(Msgs, Count),
    Result = if NS == ?NS_MAM_TMP ->
		     #mam_query{xmlns = NS, id = QID, rsm = RSMOut};
		true ->
		     #mam_fin{xmlns = NS, id = QID, rsm = RSMOut,
			      complete = IsComplete}
	     end,
    if NS == ?NS_MAM_TMP; NS == ?NS_MAM_1 ->
	    lists:foreach(
	      fun(El) ->
		      ejabberd_router:route(El)
	      end, Els),
	    xmpp:make_iq_result(IQ, Result);
       NS == ?NS_MAM_0 ->
	    ejabberd_router:route(xmpp:make_iq_result(IQ)),
	    lists:foreach(
	      fun(El) ->
		      ejabberd_router:route(El)
	      end, Els),
	    ejabberd_router:route(
	      #message{from = To, to = From, sub_els = [Result, Hint]}),
	    ignore
    end.

-spec make_rsm_out([{binary(), integer(), xmlel()}], non_neg_integer()) -> rsm_set().
make_rsm_out([], Count) ->
    #rsm_set{count = Count};
make_rsm_out([{FirstID, _, _}|_] = Msgs, Count) ->
    {LastID, _, _} = lists:last(Msgs),
    #rsm_set{first = #rsm_first{data = FirstID}, last = LastID, count = Count}.

filter_by_max(Msgs, undefined) ->
    {Msgs, true};
filter_by_max(Msgs, Len) when is_integer(Len), Len >= 0 ->
    {lists:sublist(Msgs, Len), length(Msgs) =< Len};
filter_by_max(_Msgs, _Junk) ->
    {[], true}.

-spec limit_max(rsm_set(), binary()) -> rsm_set() | undefined.
limit_max(RSM, ?NS_MAM_TMP) ->
    RSM; % XEP-0313 v0.2 doesn't require clients to support RSM.
limit_max(undefined, _NS) ->
    #rsm_set{max = ?DEF_PAGE_SIZE};
limit_max(#rsm_set{max = Max} = RSM, _NS) when not is_integer(Max) ->
    RSM#rsm_set{max = ?DEF_PAGE_SIZE};
limit_max(#rsm_set{max = Max} = RSM, _NS) when Max > ?MAX_PAGE_SIZE ->
    RSM#rsm_set{max = ?MAX_PAGE_SIZE};
limit_max(RSM, _NS) ->
    RSM.

match_interval(Now, Start, undefined) ->
    Now >= Start;
match_interval(Now, Start, End) ->
    (Now >= Start) and (Now =< End).

match_rsm(Now, #rsm_set{'after' = ID}) when is_binary(ID), ID /= <<"">> ->
    Now1 = (catch usec_to_now(binary_to_integer(ID))),
    Now > Now1;
match_rsm(Now, #rsm_set{before = ID}) when is_binary(ID), ID /= <<"">> ->
    Now1 = (catch usec_to_now(binary_to_integer(ID))),
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

get_jids(undefined) ->
    [];
get_jids(Js) ->
    [jid:tolower(jid:remove_resource(J)) || J <- Js].

get_commands_spec() ->
    [#ejabberd_commands{name = delete_old_mam_messages, tags = [purge],
			desc = "Delete MAM messages older than DAYS",
			longdesc = "Valid message TYPEs: "
				   "\"chat\", \"groupchat\", \"all\".",
			module = ?MODULE, function = delete_old_messages,
			args = [{type, binary}, {days, integer}],
			result = {res, rescode}}].

mod_opt_type(assume_mam_usage) ->
    fun (B) when is_boolean(B) -> B end;
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
