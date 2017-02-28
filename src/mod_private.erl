%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for private storage.
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
%%%----------------------------------------------------------------------

-module(mod_private).

-author('alexey@process-one.net').

-protocol({xep, 49, '1.2'}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_sm_iq/1, import_info/0,
	 remove_user/2, get_data/2, get_data/3, export/1,
	 import/5, import_start/2, mod_opt_type/1, set_data/3, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").
-include("mod_private.hrl").

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), [binary()]) -> ok.
-callback set_data(binary(), binary(), [{binary(), xmlel()}]) -> {atomic, any()}.
-callback get_data(binary(), binary(), binary()) -> {ok, xmlel()} | error.
-callback get_all_data(binary(), binary()) -> [xmlel()].
-callback remove_user(binary(), binary()) -> any().

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_PRIVATE, ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_PRIVATE).

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
	    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE,
					  ?MODULE, process_sm_iq, IQDisc);
	true ->
	    ok
    end.

-spec process_sm_iq(iq()) -> iq().
process_sm_iq(#iq{type = Type, lang = Lang,
		  from = #jid{luser = LUser, lserver = LServer},
		  to = #jid{luser = LUser, lserver = LServer},
		  sub_els = [#private{xml_els = Els0}]} = IQ) ->
    case filter_xmlels(Els0) of
	[] ->
	    Txt = <<"No private data found in this query">>,
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	Data when Type == set ->
	    set_data(LUser, LServer, Data),
	    xmpp:make_iq_result(IQ);
	Data when Type == get ->
	    StorageEls = get_data(LUser, LServer, Data),
	    xmpp:make_iq_result(IQ, #private{xml_els = StorageEls})
    end;
process_sm_iq(#iq{lang = Lang} = IQ) ->
    Txt = <<"Query to another users is forbidden">>,
    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang)).

-spec filter_xmlels([xmlel()]) -> [{binary(), xmlel()}].
filter_xmlels(Els) ->
    lists:flatmap(
      fun(#xmlel{} = El) ->
	      case fxml:get_tag_attr_s(<<"xmlns">>, El) of
		  <<"">> -> [];
		  NS -> [{NS, El}]
	      end
      end, Els).

-spec set_data(binary(), binary(), [{binary(), xmlel()}]) -> {atomic, any()}.
set_data(LUser, LServer, Data) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_data(LUser, LServer, Data).

-spec get_data(binary(), binary(), [{binary(), xmlel()}]) -> [xmlel()].
get_data(LUser, LServer, Data) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    lists:map(
      fun({NS, El}) ->
	      case Mod:get_data(LUser, LServer, NS) of
		  {ok, StorageEl} ->
		      StorageEl;
		  error ->
		      El
	      end
      end, Data).

-spec get_data(binary(), binary()) -> [xmlel()].
get_data(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_all_data(LUser, LServer).

-spec remove_user(binary(), binary()) -> any().
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(Server, ?MODULE),
    Mod:remove_user(LUser, LServer).

import_info() ->
    [{<<"private_storage">>, 4}].

import_start(LServer, DBType) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:init(LServer, []).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer, {sql, _}, DBType, Tab, L) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Tab, L).

depends(_Host, _Opts) ->
    [].

mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [db_type, iqdisc].
