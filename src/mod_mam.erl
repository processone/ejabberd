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
	 process_iq_v0_2/1, process_iq_v0_3/1, disco_sm_features/5,
	 remove_user/2, remove_room/3, mod_opt_type/1, muc_process_iq/2,
	 muc_filter_message/5, message_is_archived/5, delete_old_messages/2,
	 get_commands_spec/0, msg_to_el/4, get_room_config/4, set_room_option/4]).

-include("xmpp.hrl").
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
-callback extended_fields() -> [xdata_field()].
-callback store(xmlel(), binary(), {binary(), binary()}, chat | groupchat,
		jid(), binary(), recv | send) -> {ok, binary()} | any().
-callback write_prefs(binary(), binary(), #archive_prefs{}, binary()) -> ok | any().
-callback get_prefs(binary(), binary()) -> {ok, #archive_prefs{}} | error.
-callback select(binary(), jid(), jid(), mam_query(), chat | groupchat) ->
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
    XField = #xdata_field{type = boolean,
			  label = translate:translate(Lang, Label),
			  var = Var,
			  values = [Val]},
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
	    {error, xmpp:err_bad_request(ErrTxt, Lang)}
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
		    set_stanza_id(NewPkt, LServer, ID);
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
	    case store_msg(C2SState, xmpp:set_from_to(NewPkt, JID, Peer),
		      LUser, LServer, Peer, send) of
              {ok, ID} ->
		    set_stanza_id(NewPkt, LServer, ID);
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
		    set_stanza_id(NewPkt, LServer, ID);
		_ ->
		    NewPkt
	    end;
	true ->
	    Pkt
    end.

set_stanza_id(Pkt, LServer, ID) ->
    Archived = #mam_archived{by = jid:make(LServer), id = ID},
    StanzaID = #stanza_id{by = jid:make(LServer), id = ID},
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

parse_query(#mam_query{xdata = #xdata{fields = Fs}} = Query, Lang) ->
    try
	lists:foldl(
	  fun(#xdata_field{var = <<"start">>, values = [Data|_]}, Q) ->
		  case jlib:datetime_string_to_timestamp(Data) of
		      undefined -> throw({error, <<"start">>});
		      {_, _, _} = TS -> Q#mam_query{start = TS}
		  end;
	     (#xdata_field{var = <<"end">>, values = [Data|_]}, Q) ->
		  case jlib:datetime_string_to_timestamp(Data) of
		      undefined -> throw({error, <<"end">>});
		      {_, _, _} = TS -> Q#mam_query{'end' = TS}
		  end;
	     (#xdata_field{var = <<"with">>, values = [Data|_]}, Q) ->
		  case jid:from_string(Data) of
		      error -> throw({error, <<"with">>});
		      J -> Q#mam_query{with = J}
		  end;
	     (#xdata_field{var = <<"withtext">>, values = [Data|_]}, Q) ->
		  case Data of
		      <<"">> -> throw({error, <<"withtext">>});
		      _ -> Q#mam_query{withtext = Data}
	      end;
	     (#xdata_field{var = <<"FORM_TYPE">>, values = [NS|_]}, Q) ->
		  case Query#mam_query.xmlns of
		      NS -> Q;
		      _ -> throw({error, <<"FORM_TYPE">>})
		  end;
	     (#xdata_field{}, Acc) ->
		  Acc
	  end, Query, Fs)
    catch throw:{error, Var} ->
	    Txt = io_lib:format("Incorrect value of field '~s'", [Var]),
	    {error, xmpp:err_bad_request(iolist_to_binary(Txt), Lang)}
    end;
parse_query(Query, _Lang) ->
    Query.

disco_sm_features(empty, From, To, Node, Lang) ->
    disco_sm_features({result, []}, From, To, Node, Lang);
disco_sm_features({result, OtherFeatures},
		  #jid{luser = U, lserver = S},
		  #jid{luser = U, lserver = S}, undefined, _Lang) ->
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

process_iq(LServer, #iq{sub_els = [#mam_query{xmlns = NS}]} = IQ) ->
    CommonFields = [#xdata_field{type = hidden,
				 var = <<"FORM_TYPE">>,
				 values = [NS]},
		    #xdata_field{type = 'jid-single', var = <<"with">>},
		    #xdata_field{type = 'text-single', var = <<"start">>},
		    #xdata_field{type = 'text-single', var = <<"end">>}],
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    ExtendedFields = Mod:extended_fields(),
    Fields = CommonFields ++ ExtendedFields,
    Form = #xdata{type = form, fields = Fields},
    xmpp:make_iq_result(IQ, #mam_query{xmlns = NS, xdata = Form}).

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
    case parse_query(SubEl, Lang) of
	#mam_query{rsm = #rsm_set{index = I}} when is_integer(I) ->
	    xmpp:make_error(IQ, xmpp:err_feature_not_implemented());
	#mam_query{rsm = RSM, xmlns = NS} = Query ->
	    NewRSM = limit_max(RSM, NS),
	    NewQuery = Query#mam_query{rsm = NewRSM},
	    select_and_send(LServer, NewQuery, IQ, MsgType);
	{error, Err} ->
	    xmpp:make_error(IQ, Err)
    end.

should_archive(#message{type = T}, _LServer) when T == error; T == result ->
    false;
should_archive(#message{body = Body} = Pkt, LServer) ->
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
		    case xmpp:get_text(Body) of
			<<>> ->
			    %% Empty body
			    false;
			_ ->
			    true
		    end
	    end
    end;
should_archive(_, _LServer) ->
    false.

strip_my_archived_tag(Pkt, LServer) ->
    NewPkt = xmpp:decode_els(
	       Pkt, fun(El) ->
			    case xmpp:get_name(El) of
				<<"archived">> ->
				    xmpp:get_ns(El) == ?NS_MAM_TMP;
				<<"stanza-id">> ->
				    xmpp:get_ns(El) == ?NS_SID_0;
				_ ->
				    false
			    end
		    end),
    NewEls = lists:filter(
	       fun(#mam_archived{by = #jid{luser = <<>>} = By}) ->
		       By#jid.lserver /= LServer;
		  (#stanza_id{by = #jid{luser = <<>>} = By}) ->
		       By#jid.lserver /= LServer;
		  (_) ->
		       true
	       end, xmpp:get_els(NewPkt)),
    xmpp:set_els(NewPkt, NewEls).

strip_x_jid_tags(Pkt) ->
    NewPkt = xmpp:decode_els(
	       Pkt, fun(El) ->
			    case xmpp:get_name(El) of
				<<"x">> ->
				    case xmpp:get_ns(El) of
					?NS_MUC_USER -> true;
					?NS_MUC_ADMIN -> true;
					?NS_MUC_OWNER -> true;
					_ -> false
				    end;
				_ ->
				    false
			    end
		    end),
    NewEls = lists:filter(
	       fun(El) ->
		       Items = case El of
				   #muc_user{items = Is} -> Is;
				   #muc_admin{items = Is} -> Is;
				   #muc_owner{items = Is} -> Is;
				   _ -> []
			       end,
		       not lists:any(fun(#muc_item{jid = JID}) ->
					     JID /= undefined
				     end, Items)
	       end, xmpp:get_els(NewPkt)),
    xmpp:set_els(NewPkt, NewEls).

should_archive_peer(C2SState,
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
	    end;
	_ ->
	    false
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
    case xmpp:get_subtag(Pkt, #stanza_id{}) of
	#stanza_id{by = #jid{luser = <<>>, lserver = LServer}} ->
	    true;
	_ ->
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
	    El = xmpp:encode(Pkt),
	    Mod:store(El, LServer, US, chat, Peer, <<"">>, Dir);
	false ->
	    pass
    end.

store_muc(MUCState, Pkt, RoomJID, Peer, Nick) ->
    case should_archive_muc(Pkt) of
	true ->
	    LServer = MUCState#state.server_host,
	    {U, S, _} = jid:tolower(RoomJID),
	    Mod = gen_mod:db_mod(LServer, ?MODULE),
	    El = xmpp:encode(Pkt),
	    Mod:store(El, LServer, {U, S}, groupchat, Peer, Nick, recv);
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

select_and_send(LServer, Query, #iq{from = From, to = To} = IQ, MsgType) ->
    {Msgs, IsComplete, Count} =
	case MsgType of
	    chat ->
		select(LServer, From, From, Query, MsgType);
	    {groupchat, _Role, _MUCState} ->
		select(LServer, From, To, Query, MsgType)
	end,
    SortedMsgs = lists:keysort(2, Msgs),
    send(SortedMsgs, Count, IsComplete, IQ).

select(_LServer, JidRequestor, JidArchive,
       #mam_query{start = Start, 'end' = End, rsm = RSM},
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
			  {[{integer_to_binary(TS), TS,
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
	#rsm_set{max = Max, before = Before} when is_binary(Before) ->
	    {NewMsgs, IsComplete} = filter_by_max(lists:reverse(Msgs), Max),
	    {NewMsgs, IsComplete, L};
	#rsm_set{max = Max} ->
	    {NewMsgs, IsComplete} = filter_by_max(Msgs, Max),
	    {NewMsgs, IsComplete, L};
	_ ->
	    {Msgs, true, L}
    end;
select(LServer, JidRequestor, JidArchive, Query, MsgType) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:select(LServer, JidRequestor, JidArchive, Query, MsgType).

msg_to_el(#archive_msg{timestamp = TS, packet = Pkt1, nick = Nick, peer = Peer},
	  MsgType, JidRequestor, #jid{lserver = LServer} = JidArchive) ->
    Pkt2 = maybe_update_from_to(Pkt1, JidRequestor, JidArchive, Peer, MsgType,
				Nick),
    #forwarded{sub_els = [Pkt2],
	       delay = #delay{stamp = TS, from = jid:make(LServer)}}.

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

-spec send([{binary(), integer(), xmlel()}],
	   non_neg_integer(), boolean(), iq()) -> iq() | ignore.
send(Msgs, Count, IsComplete,
     #iq{from = From, to = To,
	 sub_els = [#mam_query{id = QID, xmlns = NS}]} = IQ) ->
    Els = lists:map(
	    fun({ID, _IDInt, El}) ->
		    #message{sub_els = [#mam_result{xmlns = NS,
						    id = ID,
						    queryid = QID,
						    sub_els = [El]}]}
	    end, Msgs),
    RSMOut = make_rsm_out(Msgs, Count),
    Result = if NS == ?NS_MAM_TMP ->
		     #mam_query{xmlns = NS, id = QID, rsm = RSMOut};
		true ->
		     #mam_fin{id = QID, rsm = RSMOut, complete = IsComplete}
	     end,
    if NS == ?NS_MAM_TMP; NS == ?NS_MAM_1 ->
	    lists:foreach(
	      fun(El) ->
		      ejabberd_router:route(To, From, El)
	      end, Els),
	    xmpp:make_iq_result(IQ, Result);
       NS == ?NS_MAM_0 ->
	    ejabberd_router:route(To, From, xmpp:make_iq_result(IQ)),
	    lists:foreach(
	      fun(El) ->
		      ejabberd_router:route(To, From, El)
	      end, Els),
	    ejabberd_router:route(To, From, #message{sub_els = [Result]}),
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

-spec limit_max(rsm_set(), binary()) -> rsm_set().
limit_max(RSM, ?NS_MAM_TMP) ->
    RSM; % XEP-0313 v0.2 doesn't require clients to support RSM.
limit_max(#rsm_set{max = Max} = RSM, _NS) when not is_integer(Max) ->
    RSM#rsm_set{max = ?DEF_PAGE_SIZE};
limit_max(#rsm_set{max = Max} = RSM, _NS) when Max > ?MAX_PAGE_SIZE ->
    RSM#rsm_set{max = ?MAX_PAGE_SIZE};
limit_max(RSM, _NS) ->
    RSM.

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

datetime_to_now(DateTime, USecs) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime) -
	calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    {Seconds div 1000000, Seconds rem 1000000, USecs}.

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
