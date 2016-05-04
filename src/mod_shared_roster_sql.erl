%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_shared_roster_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

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
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

list_groups(Host) ->
    case ejabberd_sql:sql_query(
	   Host,
           ?SQL("select @(name)s from sr_group")) of
	{selected, Rs} -> [G || {G} <- Rs];
	_ -> []
    end.

groups_with_opts(Host) ->
    case ejabberd_sql:sql_query(
           Host,
           ?SQL("select @(name)s, @(opts)s from sr_group"))
	of
      {selected, Rs} ->
	  [{G, mod_shared_roster:opts_to_binary(ejabberd_sql:decode_term(Opts))}
	   || {G, Opts} <- Rs];
      _ -> []
    end.

create_group(Host, Group, Opts) ->
    SOpts = jlib:term_to_expr(Opts),
    F = fun () ->
		?SQL_UPSERT_T(
                   "sr_group",
                   ["!name=%(Group)s",
                    "opts=%(SOpts)s"])
	end,
    ejabberd_sql:sql_transaction(Host, F).

delete_group(Host, Group) ->
    F = fun () ->
		ejabberd_sql:sql_query_t(
                  ?SQL("delete from sr_group where name=%(Group)s")),
		ejabberd_sql:sql_query_t(
                  ?SQL("delete from sr_user where grp=%(Group)s"))
	end,
    case ejabberd_sql:sql_transaction(Host, F) of
        {atomic,{updated,_}} -> {atomic, ok};
        Res -> Res
    end.

get_group_opts(Host, Group) ->
    case catch ejabberd_sql:sql_query(
		 Host,
		 ?SQL("select @(opts)s from sr_group where name=%(Group)s")) of
	{selected, [{SOpts}]} ->
	    mod_shared_roster:opts_to_binary(ejabberd_sql:decode_term(SOpts));
	_ -> error
    end.

set_group_opts(Host, Group, Opts) ->
    SOpts = jlib:term_to_expr(Opts),
    F = fun () ->
		?SQL_UPSERT_T(
                   "sr_group",
                   ["!name=%(Group)s",
                    "opts=%(SOpts)s"])
	end,
    ejabberd_sql:sql_transaction(Host, F).

get_user_groups(US, Host) ->
    SJID = make_jid_s(US),
    case catch ejabberd_sql:sql_query(
		 Host,
		 ?SQL("select @(grp)s from sr_user where jid=%(SJID)s")) of
	{selected, Rs} -> [G || {G} <- Rs];
	_ -> []
    end.

get_group_explicit_users(Host, Group) ->
    case catch ejabberd_sql:sql_query(
		 Host,
		 ?SQL("select @(jid)s from sr_user where grp=%(Group)s")) of
	{selected, Rs} ->
	    lists:map(
	      fun({JID}) ->
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
		 ?SQL("select @(grp)s from sr_user where jid=%(SJID)s")) of
	{selected, Rs} ->
	    [{Group, proplists:get_value(Group, GroupsOpts, [])}
	     || {Group} <- Rs];
	_ -> []
    end.

is_user_in_group(US, Group, Host) ->
    SJID = make_jid_s(US),
    case catch ejabberd_sql:sql_query(
                 Host,
                 ?SQL("select @(jid)s from sr_user where jid=%(SJID)s"
                      " and grp=%(Group)s")) of
	{selected, []} -> false;
	_ -> true
    end.

add_user_to_group(Host, US, Group) ->
    SJID = make_jid_s(US),
    ejabberd_sql:sql_query(
      Host,
      ?SQL("insert into sr_user(jid, grp) values ("
           "%(SJID)s, %(Group)s)")).

remove_user_from_group(Host, US, Group) ->
    SJID = make_jid_s(US),
    F = fun () ->
		ejabberd_sql:sql_query_t(
                  ?SQL("delete from sr_user where jid=%(SJID)s and"
                       " grp=%(Group)s")),
		ok
	end,
    ejabberd_sql:sql_transaction(Host, F).

export(_Server) ->
    [{sr_group,
      fun(Host, #sr_group{group_host = {Group, LServer}, opts = Opts})
            when LServer == Host ->
              SOpts = jlib:term_to_expr(Opts),
              [?SQL("delete from sr_group where name=%(Group)s;"),
               ?SQL("insert into sr_group(name, opts) values ("
                    "%(Group)s, %(SOpts)s);")];
         (_Host, _R) ->
              []
      end},
     {sr_user,
      fun(Host, #sr_user{us = {U, S}, group_host = {Group, LServer}})
            when LServer == Host ->
              SJID = jid:to_string(
                       jid:tolower(
                         jid:make(U, S, <<"">>))),
              [?SQL("select @(jid)s from sr_user where jid=%(SJID)s"
                    " and grp=%(Group)s;"),
               ?SQL("insert into sr_user(jid, grp) values ("
                    "%(SJID)s, %(Group)s);")];
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
    jid:to_string(jid:tolower(jid:make(U, S, <<"">>))).

make_jid_s({U, S}) -> make_jid_s(U, S).
