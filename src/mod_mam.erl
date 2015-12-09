%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @doc
%%%      Message Archive Management (XEP-0313)
%%% @end
%%% Created :  4 Jul 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2013-2015   ProcessOne
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

-protocol({xep, 313, '0.4'}).
-protocol({xep, 334, '0.2'}).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1]).

-export([user_send_packet/4, user_receive_packet/5,
	 process_iq_v0_2/3, process_iq_v0_3/3, remove_user/2,
	 remove_user/3, mod_opt_type/1, muc_process_iq/4,
	 muc_filter_message/5]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("mod_muc_room.hrl").

-record(archive_msg,
	{us = {<<"">>, <<"">>}                :: {binary(), binary()} | '$2',
	 id = <<>>                            :: binary() | '_',
	 timestamp = p1_time_compat:timestamp() :: erlang:timestamp() | '_' | '$1',
	 peer = {<<"">>, <<"">>, <<"">>}      :: ljid() | '_' | '$3' | undefined,
	 bare_peer = {<<"">>, <<"">>, <<"">>} :: ljid() | '_' | '$3',
	 packet = #xmlel{}                    :: xmlel() | '_',
	 nick = <<"">>                        :: binary(),
	 type = chat                          :: chat | groupchat}).

-record(archive_prefs,
	{us = {<<"">>, <<"">>} :: {binary(), binary()},
	 default = never       :: never | always | roster,
	 always = []           :: [ljid()],
	 never = []            :: [ljid()]}).

%%%===================================================================
%%% API
%%%===================================================================
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
			     one_queue),
    DBType = gen_mod:db_type(Host, Opts),
    init_db(DBType, Host),
    init_cache(DBType, Opts),
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
		       user_receive_packet, 500),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 500),
    ejabberd_hooks:add(muc_filter_message, Host, ?MODULE,
		       muc_filter_message, 50),
    ejabberd_hooks:add(muc_process_iq, Host, ?MODULE,
		       muc_process_iq, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host, ?MODULE,
		       remove_user, 50),
    ok.

init_db(mnesia, _Host) ->
    mnesia:create_table(archive_msg,
			[{disc_only_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, archive_msg)}]),
    mnesia:create_table(archive_prefs,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, archive_prefs)}]);
init_db(_, _) ->
    ok.

init_cache(_DBType, Opts) ->
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
			  user_send_packet, 500),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  user_receive_packet, 500),
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
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    ok.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    remove_user(LUser, LServer,
		gen_mod:db_type(LServer, ?MODULE)).

remove_user(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:delete({archive_msg, US}),
		mnesia:delete({archive_prefs, US})
	end,
    mnesia:transaction(F);
remove_user(LUser, LServer, odbc) ->
    SUser = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from archive where username='">>, SUser, <<"';">>]),
    ejabberd_odbc:sql_query(
      LServer,
      [<<"delete from archive_prefs where username='">>, SUser, <<"';">>]).

user_receive_packet(Pkt, C2SState, JID, Peer, To) ->
    LUser = JID#jid.luser,
    LServer = JID#jid.lserver,
    IsBareCopy = is_bare_copy(JID, To),
    case should_archive(Pkt) of
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
    case should_archive(Pkt) of
	true ->
	    NewPkt = strip_my_archived_tag(Pkt, LServer),
	    store_msg(C2SState, jlib:replace_from_to(JID, Peer, NewPkt),
		      LUser, LServer, Peer, send),
	    NewPkt;
	false ->
	    Pkt
    end.

muc_filter_message(Pkt, #state{config = Config} = MUCState,
		   RoomJID, From, FromNick) ->
    if Config#config.mam ->
	    By = jid:to_string(RoomJID),
	    NewPkt = strip_my_archived_tag(Pkt, By),
	    case store_muc(MUCState, NewPkt, RoomJID, From, FromNick) of
		{ok, ID} ->
		    StanzaID = #xmlel{name = <<"stanza-id">>,
				      attrs = [{<<"by">>, By},
                                               {<<"xmlns">>, ?NS_SID_0},
                                               {<<"id">>, ID}]},
                    NewEls = [StanzaID|NewPkt#xmlel.children],
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
    Fs = lists:flatmap(
	    fun (#xmlel{name = <<"start">>} = El) ->
		    [{<<"start">>, [xml:get_tag_cdata(El)]}];
		(#xmlel{name = <<"end">>} = El) ->
		    [{<<"end">>, [xml:get_tag_cdata(El)]}];
		(#xmlel{name = <<"with">>} = El) ->
		    [{<<"with">>, [xml:get_tag_cdata(El)]}];
		(#xmlel{name = <<"withtext">>} = El) ->
		    [{<<"withtext">>, [xml:get_tag_cdata(El)]}];
		(#xmlel{name = <<"set">>}) ->
		    [{<<"set">>, SubEl}];
		(_) ->
		   []
	   end, SubEl#xmlel.children),
    process_iq(LServer, From, To, IQ, SubEl, Fs, chat);
process_iq_v0_2(From, To, IQ) ->
    process_iq(From, To, IQ).

% Query archive v0.3
process_iq_v0_3(#jid{lserver = LServer} = From,
		#jid{lserver = LServer} = To,
		#iq{type = set, sub_el = #xmlel{name = <<"query">>} = SubEl} = IQ) ->
    process_iq(LServer, From, To, IQ, SubEl, get_xdata_fields(SubEl), chat);
process_iq_v0_3(From, To, IQ) ->
    process_iq(From, To, IQ).

muc_process_iq(#iq{type = set,
		   sub_el = #xmlel{name = <<"query">>,
				   attrs = Attrs} = SubEl} = IQ,
	       MUCState, From, To) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
	?NS_MAM_0 ->
	    LServer = MUCState#state.server_host,
	    Role = mod_muc_room:get_role(From, MUCState),
	    process_iq(LServer, From, To, IQ, SubEl,
		       get_xdata_fields(SubEl), {groupchat, Role, MUCState});
	_ ->
	    IQ
    end;
muc_process_iq(IQ, _MUCState, _From, _To) ->
    IQ.

get_xdata_fields(SubEl) ->
    case {xml:get_subtag_with_xmlns(SubEl, <<"x">>, ?NS_XDATA),
	  xml:get_subtag_with_xmlns(SubEl, <<"set">>, ?NS_RSM)} of
	{#xmlel{} = XData, false} ->
	    jlib:parse_xdata_submit(XData);
	{#xmlel{} = XData, #xmlel{}} ->
	    [{<<"set">>, SubEl} | jlib:parse_xdata_submit(XData)];
	{false, #xmlel{}} ->
	    [{<<"set">>, SubEl}];
	{false, false} ->
	    []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Preference setting (both v0.2 & v0.3)
process_iq(#jid{luser = LUser, lserver = LServer},
	   #jid{lserver = LServer},
	   #iq{type = set, sub_el = #xmlel{name = <<"prefs">>} = SubEl} = IQ) ->
    try {case xml:get_tag_attr_s(<<"default">>, SubEl) of
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
	{Default, {Always, Never}} ->
	    case write_prefs(LUser, LServer, LServer, Default,
		    lists:usort(Always), lists:usort(Never)) of
		ok ->
		    IQ#iq{type = result, sub_el = []};
		_Err ->
		    IQ#iq{type = error,
			sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
	    end
    catch _:_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end;
process_iq(#jid{luser = LUser, lserver = LServer},
	   #jid{lserver = LServer},
	   #iq{type = get, sub_el = #xmlel{name = <<"prefs">>}} = IQ) ->
    Prefs = get_prefs(LUser, LServer),
    Default = jlib:atom_to_binary(Prefs#archive_prefs.default),
    JFun = fun(L) ->
		   [#xmlel{name = <<"jid">>,
			   children = [{xmlcdata, jid:to_string(J)}]}
		    || J <- L]
	   end,
    Always = #xmlel{name = <<"always">>,
		    children = JFun(Prefs#archive_prefs.always)},
    Never = #xmlel{name = <<"never">>,
		   children = JFun(Prefs#archive_prefs.never)},
    IQ#iq{type = result,
	  sub_el = [#xmlel{name = <<"prefs">>,
			   attrs = [{<<"xmlns">>, IQ#iq.xmlns},
				    {<<"default">>, Default}],
			   children = [Always, Never]}]};
process_iq(_, _, #iq{sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

process_iq(LServer, From, To, IQ, SubEl, Fs, MsgType) ->
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
	{Start, End, With, RSM} ->
	    select_and_send(LServer, From, To, Start, End,
			    With, RSM, IQ, MsgType)
    end.

should_archive(#xmlel{name = <<"message">>} = Pkt) ->
    case xml:get_attr_s(<<"type">>, Pkt#xmlel.attrs) of
	<<"error">> ->
	    false;
	<<"groupchat">> ->
	    false;
	_ ->
	    case is_resent(Pkt) of
		true ->
		    false;
		false ->
		    case check_store_hint(Pkt) of
			store ->
			    true;
			no_store ->
			    false;
			none ->
			    case xml:get_subtag_cdata(Pkt, <<"body">>) of
				<<>> ->
				    %% Empty body
				    false;
				_ ->
				    true
			    end
		    end
	    end
    end;
should_archive(#xmlel{}) ->
    false.

strip_my_archived_tag(Pkt, LServer) ->
    NewEls = lists:filter(
	    fun(#xmlel{name = Tag, attrs = Attrs})
			when Tag == <<"archived">>; Tag == <<"stanza-id">> ->
		    case catch jid:nameprep(
			    xml:get_attr_s(
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

should_archive_muc(_MUCState, _Peer) ->
    %% TODO
    true.

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
    xml:get_subtag_with_xmlns(Message, <<"store">>, ?NS_HINTS)
      /= false.

has_no_store_hint(Message) ->
    xml:get_subtag_with_xmlns(Message, <<"no-store">>, ?NS_HINTS)
      /= false orelse
    xml:get_subtag_with_xmlns(Message, <<"no-storage">>, ?NS_HINTS)
      /= false orelse
    xml:get_subtag_with_xmlns(Message, <<"no-permanent-store">>, ?NS_HINTS)
      /= false orelse
    xml:get_subtag_with_xmlns(Message, <<"no-permanent-storage">>, ?NS_HINTS)
      /= false.

is_resent(Pkt) ->
    case xml:get_subtag_cdata(Pkt, <<"delay">>) of
	<<>> ->
	    false;
	Desc ->
	    binary:match(Desc, <<"Resent">>) =/= nomatch
    end.

store_msg(C2SState, Pkt, LUser, LServer, Peer, Dir) ->
    Prefs = get_prefs(LUser, LServer),
    case should_archive_peer(C2SState, Prefs, Peer) of
	true ->
	    US = {LUser, LServer},
	    store(Pkt, LServer, US, chat, Peer, <<"">>, Dir,
		  gen_mod:db_type(LServer, ?MODULE));
	false ->
	    pass
    end.

store_muc(MUCState, Pkt, RoomJID, Peer, Nick) ->
    case should_archive_muc(MUCState, Peer) of
	true ->
	    LServer = MUCState#state.server_host,
	    {U, S, _} = jid:tolower(RoomJID),
	    store(Pkt, LServer, {U, S}, groupchat, Peer, Nick, recv,
		  gen_mod:db_type(LServer, ?MODULE));
	false ->
	    pass
    end.

store(Pkt, _, {LUser, LServer}, Type, Peer, Nick, _Dir, mnesia) ->
    LPeer = {PUser, PServer, _} = jid:tolower(Peer),
    TS = p1_time_compat:timestamp(),
    ID = jlib:integer_to_binary(now_to_usec(TS)),
    case mnesia:dirty_write(
	   #archive_msg{us = {LUser, LServer},
			id = ID,
			timestamp = TS,
			peer = LPeer,
			bare_peer = {PUser, PServer, <<>>},
			type = Type,
			nick = Nick,
			packet = Pkt}) of
	ok ->
	    {ok, ID};
	Err ->
	    Err
    end;
store(Pkt, LServer, {LUser, LHost}, Type, Peer, Nick, _Dir, odbc) ->
    TSinteger = p1_time_compat:system_time(micro_seconds),
    ID = TS = jlib:integer_to_binary(TSinteger),
    SUser = case Type of
		chat -> LUser;
		groupchat -> jid:to_string({LUser, LHost, <<>>})
	    end,
    BarePeer = jid:to_string(
		 jid:tolower(
		   jid:remove_resource(Peer))),
    LPeer = jid:to_string(
	      jid:tolower(Peer)),
    XML = xml:element_to_binary(Pkt),
    Body = xml:get_subtag_cdata(Pkt, <<"body">>),
    case ejabberd_odbc:sql_query(
	    LServer,
	    [<<"insert into archive (username, timestamp, "
		    "peer, bare_peer, xml, txt, kind, nick) values (">>,
		<<"'">>, ejabberd_odbc:escape(SUser), <<"', ">>,
		<<"'">>, TS, <<"', ">>,
		<<"'">>, ejabberd_odbc:escape(LPeer), <<"', ">>,
		<<"'">>, ejabberd_odbc:escape(BarePeer), <<"', ">>,
		<<"'">>, ejabberd_odbc:escape(XML), <<"', ">>,
		<<"'">>, ejabberd_odbc:escape(Body), <<"', ">>,
		<<"'">>, jlib:atom_to_binary(Type), <<"', ">>,
		<<"'">>, ejabberd_odbc:escape(Nick), <<"');">>]) of
	{updated, _} ->
	    {ok, ID};
	Err ->
	    Err
    end.

write_prefs(LUser, LServer, Host, Default, Always, Never) ->
    DBType = case gen_mod:db_type(Host, ?MODULE) of
		 odbc -> {odbc, Host};
		 DB -> DB
	     end,
    Prefs = #archive_prefs{us = {LUser, LServer},
			   default = Default,
			   always = Always,
			   never = Never},
    cache_tab:dirty_insert(
      archive_prefs, {LUser, LServer}, Prefs,
      fun() ->  write_prefs(LUser, LServer, Prefs, DBType) end).

write_prefs(_LUser, _LServer, Prefs, mnesia) ->
    mnesia:dirty_write(Prefs);
write_prefs(LUser, _LServer, #archive_prefs{default = Default,
					   never = Never,
					   always = Always},
	    {odbc, Host}) ->
    SUser = ejabberd_odbc:escape(LUser),
    SDefault = erlang:atom_to_binary(Default, utf8),
    SAlways = ejabberd_odbc:encode_term(Always),
    SNever = ejabberd_odbc:encode_term(Never),
    case update(Host, <<"archive_prefs">>,
		[<<"username">>, <<"def">>, <<"always">>, <<"never">>],
		[SUser, SDefault, SAlways, SNever],
		[<<"username='">>, SUser, <<"'">>]) of
	{updated, _} ->
	    ok;
	Err ->
	    Err
    end.

get_prefs(LUser, LServer) ->
    DBType = gen_mod:db_type(LServer, ?MODULE),
    Res = cache_tab:lookup(archive_prefs, {LUser, LServer},
			   fun() -> get_prefs(LUser, LServer,
					      DBType)
			   end),
    case Res of
	{ok, Prefs} ->
	    Prefs;
	error ->
	    Default = gen_mod:get_module_opt(
		    LServer, ?MODULE, default,
		    fun(always) -> always;
			(never) -> never;
			(roster) -> roster
		    end, never),
	    #archive_prefs{us = {LUser, LServer}, default = Default}
    end.

get_prefs(LUser, LServer, mnesia) ->
    case mnesia:dirty_read(archive_prefs, {LUser, LServer}) of
	[Prefs] ->
	    {ok, Prefs};
	_ ->
	    error
    end;
get_prefs(LUser, LServer, odbc) ->
    case ejabberd_odbc:sql_query(
	   LServer,
	   [<<"select def, always, never from archive_prefs ">>,
	    <<"where username='">>,
	    ejabberd_odbc:escape(LUser), <<"';">>]) of
	{selected, _, [[SDefault, SAlways, SNever]]} ->
	    Default = erlang:binary_to_existing_atom(SDefault, utf8),
	    Always = ejabberd_odbc:decode_term(SAlways),
	    Never = ejabberd_odbc:decode_term(SNever),
	    {ok, #archive_prefs{us = {LUser, LServer},
		    default = Default,
		    always = Always,
		    never = Never}};
	_ ->
	    error
    end.

select_and_send(LServer, From, To, Start, End, With, RSM, IQ, MsgType) ->
    DBType = case gen_mod:db_type(LServer, ?MODULE) of
		 odbc -> {odbc, LServer};
		 DB -> DB
	     end,
    select_and_send(LServer, From, To, Start, End, With, RSM, IQ,
		    MsgType, DBType).

select_and_send(LServer, From, To, Start, End, With, RSM, IQ, MsgType, DBType) ->
    {Msgs, IsComplete, Count} = select_and_start(LServer, From, To, Start, End,
						 With, RSM, MsgType, DBType),
    SortedMsgs = lists:keysort(2, Msgs),
    send(From, To, SortedMsgs, RSM, Count, IsComplete, IQ).

select_and_start(LServer, From, To, Start, End, With, RSM, MsgType, DBType) ->
    case MsgType of
	chat ->
	    select(LServer, From, Start, End, With, RSM, MsgType, DBType);
	{groupchat, _Role, _MUCState} ->
	    select(LServer, To, Start, End, With, RSM, MsgType, DBType)
    end.

select(_LServer, JidRequestor, Start, End, _With, RSM,
       {groupchat, _Role, #state{config = #config{mam = false},
				 history = History}} = MsgType,
       _DBType) ->
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
				       MsgType,
				       JidRequestor)}], I+1};
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
select(_LServer, #jid{luser = LUser, lserver = LServer} = JidRequestor,
       Start, End, With, RSM, MsgType, mnesia) ->
    MS = make_matchspec(LUser, LServer, Start, End, With),
    Msgs = mnesia:dirty_select(archive_msg, MS),
    {FilteredMsgs, IsComplete} = filter_by_rsm(Msgs, RSM),
    Count = length(Msgs),
    {lists:map(
       fun(Msg) ->
	       {Msg#archive_msg.id,
		jlib:binary_to_integer(Msg#archive_msg.id),
		msg_to_el(Msg, MsgType, JidRequestor)}
       end, FilteredMsgs), IsComplete, Count};
select(LServer, #jid{luser = LUser} = JidRequestor,
       Start, End, With, RSM, MsgType, {odbc, Host}) ->
    User = case MsgType of
	       chat -> LUser;
	       {groupchat, _Role, _MUCState} -> jid:to_string(JidRequestor)
	   end,
    {Query, CountQuery} = make_sql_query(User, LServer,
					 Start, End, With, RSM),
    % XXX TODO from XEP-0313:
    % To conserve resources, a server MAY place a reasonable limit on
    % how many stanzas may be pushed to a client in one request. If a
    % query returns a number of stanzas greater than this limit and
    % the client did not specify a limit using RSM then the server
    % should return a policy-violation error to the client.
    case {ejabberd_odbc:sql_query(Host, Query),
	  ejabberd_odbc:sql_query(Host, CountQuery)} of
	{{selected, _, Res}, {selected, _, [[Count]]}} ->
	    {Max, Direction} = case RSM of
				   #rsm_in{max = M, direction = D} -> {M, D};
				   _ -> {undefined, undefined}
			       end,
	    {Res1, IsComplete} =
		if Max >= 0 andalso Max /= undefined andalso length(Res) > Max ->
			if Direction == before ->
				{lists:nthtail(1, Res), false};
			   true ->
				{lists:sublist(Res, Max), false}
			end;
		   true ->
			{Res, true}
		end,
	    {lists:map(
	       fun([TS, XML, PeerBin, Kind, Nick]) ->
		       #xmlel{} = El = xml_stream:parse_element(XML),
		       Now = usec_to_now(jlib:binary_to_integer(TS)),
		       PeerJid = jid:tolower(jid:from_string(PeerBin)),
		       T = case Kind of
                               <<"">> -> chat;
                               null -> chat;
                               _ -> jlib:binary_to_atom(Kind)
			   end,
		       {TS, jlib:binary_to_integer(TS),
			msg_to_el(#archive_msg{timestamp = Now,
					       packet = El,
					       type = T,
					       nick = Nick,
					       peer = PeerJid},
				  MsgType,
				  JidRequestor)}
		    end, Res1), IsComplete, jlib:binary_to_integer(Count)};
	_ ->
	    {[], false, 0}
    end.

msg_to_el(#archive_msg{timestamp = TS, packet = Pkt1, nick = Nick, peer = Peer},
	  MsgType, JidRequestor) ->
    Delay = jlib:now_to_utc_string(TS),
    Pkt = maybe_update_from_to(Pkt1, JidRequestor, Peer, MsgType, Nick),
    #xmlel{name = <<"forwarded">>,
	   attrs = [{<<"xmlns">>, ?NS_FORWARD}],
	   children = [#xmlel{name = <<"delay">>,
			      attrs = [{<<"xmlns">>, ?NS_DELAY},
				       {<<"stamp">>, Delay}]},
		       xml:replace_tag_attr(
			 <<"xmlns">>, <<"jabber:client">>, Pkt)]}.

maybe_update_from_to(Pkt, JidRequestor, Peer, chat, _Nick) ->
    case xml:get_attr_s(<<"type">>, Pkt#xmlel.attrs) of
	<<"groupchat">> when Peer /= undefined ->
	    Pkt2 = xml:replace_tag_attr(<<"to">>,
					jid:to_string(JidRequestor),
					Pkt),
	    xml:replace_tag_attr(<<"from">>, jid:to_string(Peer),
				 Pkt2);
	_ -> Pkt
    end;
maybe_update_from_to(#xmlel{children = Els} = Pkt, JidRequestor,
		     Peer, {groupchat, Role, _MUCState}, Nick) ->
    Items = case Role of
		moderator when Peer /= undefined ->
		    [#xmlel{name = <<"x">>,
			    attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
			    children =
				[#xmlel{name = <<"item">>,
					attrs = [{<<"jid">>,
						  jid:to_string(Peer)}]}]}];
		_ ->
		    []
	    end,
    Pkt1 = Pkt#xmlel{children = Items ++ Els},
    Pkt2 = jlib:replace_from(jid:replace_resource(JidRequestor, Nick), Pkt1),
    jlib:remove_attr(<<"to">>, Pkt2).

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
    QID = xml:get_tag_attr_s(<<"queryid">>, SubEl),
    NS = xml:get_tag_attr_s(<<"xmlns">>, SubEl),
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

filter_by_rsm(Msgs, none) ->
    {Msgs, true};
filter_by_rsm(_Msgs, #rsm_in{max = Max}) when Max < 0 ->
    {[], true};
filter_by_rsm(Msgs, #rsm_in{max = Max, direction = Direction, id = ID}) ->
    NewMsgs = case Direction of
		  aft when ID /= <<"">> ->
		      lists:filter(
			fun(#archive_msg{id = I}) ->
				I > ID
			end, Msgs);
		  before when ID /= <<"">> ->
		      lists:foldl(
			fun(#archive_msg{id = I} = Msg, Acc) when I < ID ->
				[Msg|Acc];
			   (_, Acc) ->
				Acc
			end, [], Msgs);
		  before when ID == <<"">> ->
		      lists:reverse(Msgs);
		  _ ->
		      Msgs
	      end,
    filter_by_max(NewMsgs, Max).

filter_by_max(Msgs, undefined) ->
    {Msgs, true};
filter_by_max(Msgs, Len) when is_integer(Len), Len >= 0 ->
    {lists:sublist(Msgs, Len), length(Msgs) =< Len};
filter_by_max(_Msgs, _Junk) ->
    {[], true}.

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

make_matchspec(LUser, LServer, Start, End, {_, _, <<>>} = With) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
		       us = US,
		       bare_peer = BPeer} = Msg)
	    when Start =< TS, End >= TS,
		 US == {LUser, LServer},
		 BPeer == With ->
	      Msg
      end);
make_matchspec(LUser, LServer, Start, End, {_, _, _} = With) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
		       us = US,
		       peer = Peer} = Msg)
	    when Start =< TS, End >= TS,
		 US == {LUser, LServer},
		 Peer == With ->
	      Msg
      end);
make_matchspec(LUser, LServer, Start, End, none) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
		       us = US,
		       peer = Peer} = Msg)
	    when Start =< TS, End >= TS,
		 US == {LUser, LServer} ->
	      Msg
      end).

make_sql_query(User, _LServer, Start, End, With, RSM) ->
    {Max, Direction, ID} = case RSM of
	#rsm_in{} ->
	    {RSM#rsm_in.max,
		RSM#rsm_in.direction,
		RSM#rsm_in.id};
	none ->
	    {none, none, <<>>}
    end,
    LimitClause = if is_integer(Max), Max >= 0 ->
			  [<<" limit ">>, jlib:integer_to_binary(Max+1)];
		     true ->
			  []
		  end,
    WithClause = case With of
		     {text, <<>>} ->
			 [];
		     {text, Txt} ->
			 [<<" and match (txt) against ('">>,
			  ejabberd_odbc:escape(Txt), <<"')">>];
		     {_, _, <<>>} ->
			 [<<" and bare_peer='">>,
			  ejabberd_odbc:escape(jid:to_string(With)),
			  <<"'">>];
		     {_, _, _} ->
			 [<<" and peer='">>,
			  ejabberd_odbc:escape(jid:to_string(With)),
			  <<"'">>];
		     none ->
			 []
		 end,
    PageClause = case catch jlib:binary_to_integer(ID) of
		     I when is_integer(I), I >= 0 ->
			 case Direction of
			     before ->
				 [<<" AND timestamp < ">>, ID];
			     aft ->
				 [<<" AND timestamp > ">>, ID];
			     _ ->
				 []
			 end;
		     _ ->
			 []
		 end,
    StartClause = case Start of
		      {_, _, _} ->
			  [<<" and timestamp >= ">>,
			   jlib:integer_to_binary(now_to_usec(Start))];
		      _ ->
			  []
		  end,
    EndClause = case End of
		    {_, _, _} ->
			[<<" and timestamp <= ">>,
			 jlib:integer_to_binary(now_to_usec(End))];
		    _ ->
			[]
		end,
    SUser = ejabberd_odbc:escape(User),

    Query = [<<"SELECT timestamp, xml, peer, kind, nick"
	      " FROM archive WHERE username='">>,
	     SUser, <<"'">>, WithClause, StartClause, EndClause,
	     PageClause],

    QueryPage =
	case Direction of
	    before ->
		% ID can be empty because of
		% XEP-0059: Result Set Management
		% 2.5 Requesting the Last Page in a Result Set
		[<<"SELECT timestamp, xml, peer, kind, nick FROM (">>, Query,
		 <<" ORDER BY timestamp DESC ">>,
		 LimitClause, <<") AS t ORDER BY timestamp ASC;">>];
	    _ ->
		[Query, <<" ORDER BY timestamp ASC ">>,
		 LimitClause, <<";">>]
	end,
    {QueryPage,
     [<<"SELECT COUNT(*) FROM archive WHERE username='">>,
      SUser, <<"'">>, WithClause, StartClause, EndClause, <<";">>]}.

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
	      J = jid:from_string(xml:get_tag_cdata(El)),
	      [jid:tolower(jid:remove_resource(J)),
	       jid:tolower(J)];
	 (_) ->
	      []
      end, Els).

update(LServer, Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query(LServer,
				 [<<"update ">>, Table, <<" set ">>,
				  join(UPairs, <<", ">>), <<" where ">>, Where,
				  <<";">>])
	of
	{updated, 1} -> {updated, 1};
	_ ->
	    ejabberd_odbc:sql_query(LServer,
				    [<<"insert into ">>, Table, <<"(">>,
				     join(Fields, <<", ">>), <<") values ('">>,
				     join(Vals, <<"', '">>), <<"');">>])
    end.

%% Almost a copy of string:join/2.
join([], _Sep) -> [];
join([H | T], Sep) -> [H, [[Sep, X] || X <- T]].

mod_opt_type(cache_life_time) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(cache_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(default) ->
    fun (always) -> always;
	(never) -> never;
	(roster) -> roster
    end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(store_body_only) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) ->
    [cache_life_time, cache_size, db_type, default, iqdisc,
     store_body_only].
