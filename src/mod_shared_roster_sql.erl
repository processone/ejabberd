%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_shared_roster_sql).

-behaviour(mod_shared_roster).

%% API
-export([init/2, list_groups/1, groups_with_opts/1, create_group/3,
	 delete_group/2, get_group_opts/2, set_group_opts/3,
	 get_user_groups/2, get_group_explicit_users/2,
	 get_user_displayed_groups/3, is_user_in_group/3,
	 add_user_to_group/3, remove_user_from_group/3, import/1,
	 import/2, export/1]).

-include("jlib.hrl").
-include("mod_roster.hrl").
-include("mod_shared_roster.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

list_groups(Host) ->
    case ejabberd_sql:sql_query(
	   Host, [<<"select name from sr_group;">>]) of
	{selected, [<<"name">>], Rs} -> [G || [G] <- Rs];
	_ -> []
    end.

groups_with_opts(Host) ->
    case ejabberd_sql:sql_query(Host,
				 [<<"select name, opts from sr_group;">>])
	of
      {selected, [<<"name">>, <<"opts">>], Rs} ->
	  [{G, mod_shared_roster:opts_to_binary(ejabberd_sql:decode_term(Opts))}
	   || [G, Opts] <- Rs];
      _ -> []
    end.

create_group(Host, Group, Opts) ->
    SGroup = ejabberd_sql:escape(Group),
    SOpts = ejabberd_sql:encode_term(Opts),
    F = fun () ->
		sql_queries:update_t(<<"sr_group">>,
				      [<<"name">>, <<"opts">>], [SGroup, SOpts],
				      [<<"name='">>, SGroup, <<"'">>])
	end,
    ejabberd_sql:sql_transaction(Host, F).

delete_group(Host, Group) ->
    SGroup = ejabberd_sql:escape(Group),
    F = fun () ->
		ejabberd_sql:sql_query_t([<<"delete from sr_group where name='">>,
					   SGroup, <<"';">>]),
		ejabberd_sql:sql_query_t([<<"delete from sr_user where grp='">>,
					   SGroup, <<"';">>])
	end,
    case ejabberd_sql:sql_transaction(Host, F) of
        {atomic,{updated,_}} -> {atomic, ok};
        Res -> Res
    end.

get_group_opts(Host, Group) ->
    SGroup = ejabberd_sql:escape(Group),
    case catch ejabberd_sql:sql_query(
		 Host,
		 [<<"select opts from sr_group where name='">>,
		  SGroup, <<"';">>]) of
	{selected, [<<"opts">>], [[SOpts]]} ->
	    mod_shared_roster:opts_to_binary(ejabberd_sql:decode_term(SOpts));
	_ -> error
    end.

set_group_opts(Host, Group, Opts) ->
    SGroup = ejabberd_sql:escape(Group),
    SOpts = ejabberd_sql:encode_term(Opts),
    F = fun () ->
		sql_queries:update_t(<<"sr_group">>,
				      [<<"name">>, <<"opts">>], [SGroup, SOpts],
				      [<<"name='">>, SGroup, <<"'">>])
	end,
    ejabberd_sql:sql_transaction(Host, F).

get_user_groups(US, Host) ->
    SJID = make_jid_s(US),
    case catch ejabberd_sql:sql_query(
		 Host,
		 [<<"select grp from sr_user where jid='">>,
		  SJID, <<"';">>]) of
	{selected, [<<"grp">>], Rs} -> [G || [G] <- Rs];
	_ -> []
    end.

get_group_explicit_users(Host, Group) ->
    SGroup = ejabberd_sql:escape(Group),
    case catch ejabberd_sql:sql_query(
		 Host,
		 [<<"select jid from sr_user where grp='">>,
		  SGroup, <<"';">>]) of
	{selected, [<<"jid">>], Rs} ->
	    lists:map(
	      fun([JID]) ->
		      {U, S, _} = jid:tolower(jid:from_string(JID)),
		      {U, S}
	      end, Rs);
	_ ->
	    []
    end.

get_user_displayed_groups(LUser, LServer, GroupsOpts) ->
    SJID = make_jid_s(LUser, LServer),
    case catch ejabberd_sql:sql_query(
		 LServer,
		 [<<"select grp from sr_user where jid='">>,
		  SJID, <<"';">>]) of
	{selected, [<<"grp">>], Rs} ->
	    [{Group, proplists:get_value(Group, GroupsOpts, [])}
	     || [Group] <- Rs];
	_ -> []
    end.

is_user_in_group(US, Group, Host) ->
    SJID = make_jid_s(US),
    SGroup = ejabberd_sql:escape(Group),
    case catch ejabberd_sql:sql_query(Host,
				       [<<"select * from sr_user where jid='">>,
					SJID, <<"' and grp='">>, SGroup,
					<<"';">>]) of
	{selected, _, []} -> false;
	_ -> true
    end.

add_user_to_group(Host, US, Group) ->
    SJID = make_jid_s(US),
    SGroup = ejabberd_sql:escape(Group),
    F = fun () ->
		sql_queries:update_t(<<"sr_user">>,
				      [<<"jid">>, <<"grp">>], [SJID, SGroup],
				      [<<"jid='">>, SJID, <<"' and grp='">>,
				       SGroup, <<"'">>])
	end,
    ejabberd_sql:sql_transaction(Host, F).

remove_user_from_group(Host, US, Group) ->
    SJID = make_jid_s(US),
    SGroup = ejabberd_sql:escape(Group),
    F = fun () ->
		ejabberd_sql:sql_query_t([<<"delete from sr_user where jid='">>,
					   SJID, <<"' and grp='">>, SGroup,
					   <<"';">>]),
		ok
	end,
    ejabberd_sql:sql_transaction(Host, F).

export(_Server) ->
    [{sr_group,
      fun(Host, #sr_group{group_host = {Group, LServer}, opts = Opts})
            when LServer == Host ->
              SGroup = ejabberd_sql:escape(Group),
              SOpts = ejabberd_sql:encode_term(Opts),
              [[<<"delete from sr_group where name='">>, Group, <<"';">>],
               [<<"insert into sr_group(name, opts) values ('">>,
                SGroup, <<"', '">>, SOpts, <<"');">>]];
         (_Host, _R) ->
              []
      end},
     {sr_user,
      fun(Host, #sr_user{us = {U, S}, group_host = {Group, LServer}})
            when LServer == Host ->
              SGroup = ejabberd_sql:escape(Group),
              SJID = ejabberd_sql:escape(
                       jid:to_string(
                         jid:tolower(
                           jid:make(U, S, <<"">>)))),
              [[<<"delete from sr_user where jid='">>, SJID,
                <<"'and grp='">>, Group, <<"';">>],
               [<<"insert into sr_user(jid, grp) values ('">>,
                SJID, <<"', '">>, SGroup, <<"');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select name, opts from sr_group;">>,
      fun([Group, SOpts]) ->
              #sr_group{group_host = {Group, LServer},
                        opts = ejabberd_sql:decode_term(SOpts)}
      end},
     {<<"select jid, grp from sr_user;">>,
      fun([SJID, Group]) ->
              #jid{luser = U, lserver = S} = jid:from_string(SJID),
              #sr_user{us = {U, S}, group_host = {Group, LServer}}
      end}].

import(_, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_jid_s(U, S) ->
    ejabberd_sql:escape(jid:to_string(jid:tolower(jid:make(U, S, <<"">>)))).

make_jid_s({U, S}) -> make_jid_s(U, S).
