%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for private storage.
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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

-export([start/2, stop/1, process_sm_iq/3, import/3,
	 remove_user/2, get_data/2, export/1, import/1,
	 mod_opt_type/1, set_data/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").
-include("mod_private.hrl").

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #private_storage{}) -> ok | pass.
-callback set_data(binary(), binary(), [{binary(), xmlel()}]) -> {atomic, any()}.
-callback get_data(binary(), binary(), binary()) -> {ok, xmlel()} | error.
-callback get_all_data(binary(), binary()) -> [xmlel()].
    
-define(Xmlel_Query(Attrs, Children),
	#xmlel{name = <<"query">>, attrs = Attrs,
	       children = Children}).

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

process_sm_iq(#jid{luser = LUser, lserver = LServer},
	      #jid{luser = LUser, lserver = LServer}, #iq{lang = Lang} = IQ)
    when IQ#iq.type == set ->
    case IQ#iq.sub_el of
      #xmlel{name = <<"query">>, children = Xmlels} ->
	  case filter_xmlels(Xmlels) of
	    [] ->
		Txt = <<"No private data found in this query">>,
		IQ#iq{type = error,
		      sub_el = [IQ#iq.sub_el, ?ERRT_NOT_ACCEPTABLE(Lang, Txt)]};
	    Data ->
		set_data(LUser, LServer, Data),
		IQ#iq{type = result, sub_el = []}
	  end;
      _ ->
	  Txt = <<"No query found">>,
	  IQ#iq{type = error,
		sub_el = [IQ#iq.sub_el, ?ERRT_NOT_ACCEPTABLE(Lang, Txt)]}
    end;
%%
process_sm_iq(#jid{luser = LUser, lserver = LServer},
	      #jid{luser = LUser, lserver = LServer}, #iq{lang = Lang} = IQ)
    when IQ#iq.type == get ->
    case IQ#iq.sub_el of
      #xmlel{name = <<"query">>, attrs = Attrs,
	     children = Xmlels} ->
	  case filter_xmlels(Xmlels) of
	    [] ->
		Txt = <<"No private data found in this query">>,
		IQ#iq{type = error,
		      sub_el = [IQ#iq.sub_el, ?ERRT_BAD_FORMAT(Lang, Txt)]};
	    Data ->
		case catch get_data(LUser, LServer, Data) of
		  {'EXIT', _Reason} ->
		      Txt = <<"Database failure">>,
		      IQ#iq{type = error,
			    sub_el =
				[IQ#iq.sub_el, ?ERRT_INTERNAL_SERVER_ERROR(Lang, Txt)]};
		  Storage_Xmlels ->
		      IQ#iq{type = result,
			    sub_el = [?Xmlel_Query(Attrs, Storage_Xmlels)]}
		end
	  end;
      _ ->
	  Txt = <<"No query found">>,
	  IQ#iq{type = error,
		sub_el = [IQ#iq.sub_el, ?ERRT_BAD_FORMAT(Lang, Txt)]}
    end;
%%
process_sm_iq(_From, _To, #iq{lang = Lang} = IQ) ->
    Txt = <<"Query to another users is forbidden">>,
    IQ#iq{type = error,
	  sub_el = [IQ#iq.sub_el, ?ERRT_FORBIDDEN(Lang, Txt)]}.

filter_xmlels(Xmlels) -> filter_xmlels(Xmlels, []).

filter_xmlels([], Data) -> lists:reverse(Data);
filter_xmlels([#xmlel{attrs = Attrs} = Xmlel | Xmlels],
	      Data) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
      <<"">> -> [];
      XmlNS -> filter_xmlels(Xmlels, [{XmlNS, Xmlel} | Data])
    end;
filter_xmlels([_ | Xmlels], Data) ->
    filter_xmlels(Xmlels, Data).

set_data(LUser, LServer, Data) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_data(LUser, LServer, Data).

get_data(LUser, LServer, Data) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    get_data(LUser, LServer, Data, Mod, []).

get_data(_LUser, _LServer, [], _Mod, Storage_Xmlels) ->
    lists:reverse(Storage_Xmlels);
get_data(LUser, LServer, [{XmlNS, Xmlel} | Data], Mod, Storage_Xmlels) ->
    case Mod:get_data(LUser, LServer, XmlNS) of
	{ok, Storage_Xmlel} ->
	    get_data(LUser, LServer, Data, Mod, [Storage_Xmlel | Storage_Xmlels]);
	error ->
	    get_data(LUser, LServer, Data, Mod, [Xmlel | Storage_Xmlels])
    end.

get_data(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_all_data(LUser, LServer).

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(Server, ?MODULE),
    Mod:remove_user(LUser, LServer).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, PD) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, PD).

mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [db_type, iqdisc].
