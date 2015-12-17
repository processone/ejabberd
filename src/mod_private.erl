%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for private storage.
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
	 mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-record(private_storage,
        {usns = {<<"">>, <<"">>, <<"">>} :: {binary(), binary(), binary() |
                                             '$1' | '_'},
         xml = #xmlel{} :: xmlel() | '_' | '$1'}).

-define(Xmlel_Query(Attrs, Children),
	#xmlel{name = <<"query">>, attrs = Attrs,
	       children = Children}).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    case gen_mod:db_type(Host, Opts) of
      mnesia ->
	  mnesia:create_table(private_storage,
			      [{disc_only_copies, [node()]},
			       {attributes,
				record_info(fields, private_storage)}]),
	  update_table();
      _ -> ok
    end,
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
	      #jid{luser = LUser, lserver = LServer}, IQ)
    when IQ#iq.type == set ->
    case IQ#iq.sub_el of
      #xmlel{name = <<"query">>, children = Xmlels} ->
	  case filter_xmlels(Xmlels) of
	    [] ->
		IQ#iq{type = error,
		      sub_el = [IQ#iq.sub_el, ?ERR_NOT_ACCEPTABLE]};
	    Data ->
		DBType = gen_mod:db_type(LServer, ?MODULE),
		F = fun () ->
			    lists:foreach(fun (Datum) ->
						  set_data(LUser, LServer,
							   Datum, DBType)
					  end,
					  Data)
		    end,
		case DBType of
		  odbc -> ejabberd_odbc:sql_transaction(LServer, F);
		  mnesia -> mnesia:transaction(F);
		  riak -> F()
		end,
		IQ#iq{type = result, sub_el = []}
	  end;
      _ ->
	  IQ#iq{type = error,
		sub_el = [IQ#iq.sub_el, ?ERR_NOT_ACCEPTABLE]}
    end;
%%
process_sm_iq(#jid{luser = LUser, lserver = LServer},
	      #jid{luser = LUser, lserver = LServer}, IQ)
    when IQ#iq.type == get ->
    case IQ#iq.sub_el of
      #xmlel{name = <<"query">>, attrs = Attrs,
	     children = Xmlels} ->
	  case filter_xmlels(Xmlels) of
	    [] ->
		IQ#iq{type = error,
		      sub_el = [IQ#iq.sub_el, ?ERR_BAD_FORMAT]};
	    Data ->
		case catch get_data(LUser, LServer, Data) of
		  {'EXIT', _Reason} ->
		      IQ#iq{type = error,
			    sub_el =
				[IQ#iq.sub_el, ?ERR_INTERNAL_SERVER_ERROR]};
		  Storage_Xmlels ->
		      IQ#iq{type = result,
			    sub_el = [?Xmlel_Query(Attrs, Storage_Xmlels)]}
		end
	  end;
      _ ->
	  IQ#iq{type = error,
		sub_el = [IQ#iq.sub_el, ?ERR_BAD_FORMAT]}
    end;
%%
process_sm_iq(_From, _To, IQ) ->
    IQ#iq{type = error,
	  sub_el = [IQ#iq.sub_el, ?ERR_FORBIDDEN]}.

filter_xmlels(Xmlels) -> filter_xmlels(Xmlels, []).

filter_xmlels([], Data) -> lists:reverse(Data);
filter_xmlels([#xmlel{attrs = Attrs} = Xmlel | Xmlels],
	      Data) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
      <<"">> -> [];
      XmlNS -> filter_xmlels(Xmlels, [{XmlNS, Xmlel} | Data])
    end;
filter_xmlels([_ | Xmlels], Data) ->
    filter_xmlels(Xmlels, Data).

set_data(LUser, LServer, {XmlNS, Xmlel}, mnesia) ->
    mnesia:write(#private_storage{usns =
				      {LUser, LServer, XmlNS},
				  xml = Xmlel});
set_data(LUser, LServer, {XMLNS, El}, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    LXMLNS = ejabberd_odbc:escape(XMLNS),
    SData = ejabberd_odbc:escape(xml:element_to_binary(El)),
    odbc_queries:set_private_data(LServer, Username, LXMLNS,
				  SData);
set_data(LUser, LServer, {XMLNS, El}, riak) ->
    ejabberd_riak:put(#private_storage{usns = {LUser, LServer, XMLNS},
                                       xml = El},
		      private_storage_schema(),
                      [{'2i', [{<<"us">>, {LUser, LServer}}]}]).

get_data(LUser, LServer, Data) ->
    get_data(LUser, LServer,
	     gen_mod:db_type(LServer, ?MODULE), Data, []).

get_data(_LUser, _LServer, _DBType, [],
	 Storage_Xmlels) ->
    lists:reverse(Storage_Xmlels);
get_data(LUser, LServer, mnesia,
	 [{XmlNS, Xmlel} | Data], Storage_Xmlels) ->
    case mnesia:dirty_read(private_storage,
			   {LUser, LServer, XmlNS})
	of
      [#private_storage{xml = Storage_Xmlel}] ->
	  get_data(LUser, LServer, mnesia, Data,
		   [Storage_Xmlel | Storage_Xmlels]);
      _ ->
	  get_data(LUser, LServer, mnesia, Data,
		   [Xmlel | Storage_Xmlels])
    end;
get_data(LUser, LServer, odbc, [{XMLNS, El} | Els],
	 Res) ->
    Username = ejabberd_odbc:escape(LUser),
    LXMLNS = ejabberd_odbc:escape(XMLNS),
    case catch odbc_queries:get_private_data(LServer,
					     Username, LXMLNS)
	of
      {selected, [<<"data">>], [[SData]]} ->
	  case xml_stream:parse_element(SData) of
	    Data when is_record(Data, xmlel) ->
		get_data(LUser, LServer, odbc, Els, [Data | Res])
	  end;
      _ -> get_data(LUser, LServer, odbc, Els, [El | Res])
    end;
get_data(LUser, LServer, riak, [{XMLNS, El} | Els],
	 Res) ->
    case ejabberd_riak:get(private_storage, private_storage_schema(),
			   {LUser, LServer, XMLNS}) of
        {ok, #private_storage{xml = NewEl}} ->
            get_data(LUser, LServer, riak, Els, [NewEl|Res]);
        _ ->
            get_data(LUser, LServer, riak, Els, [El|Res])
    end.

get_data(LUser, LServer) ->
    get_all_data(LUser, LServer,
                 gen_mod:db_type(LServer, ?MODULE)).

get_all_data(LUser, LServer, mnesia) ->
    lists:flatten(
      mnesia:dirty_select(private_storage,
                          [{#private_storage{usns = {LUser, LServer, '_'},
                                             xml = '$1'},
                            [], ['$1']}]));
get_all_data(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_private_data(LServer, Username) of
        {selected, [<<"namespace">>, <<"data">>], Res} ->
            lists:flatmap(
              fun([_, SData]) ->
                      case xml_stream:parse_element(SData) of
                          #xmlel{} = El ->
                              [El];
                          _ ->
                              []
                      end
              end, Res);
        _ ->
            []
    end;
get_all_data(LUser, LServer, riak) ->
    case ejabberd_riak:get_by_index(
           private_storage, private_storage_schema(),
	   <<"us">>, {LUser, LServer}) of
        {ok, Res} ->
            [El || #private_storage{xml = El} <- Res];
        _ ->
            []
    end.

private_storage_schema() ->
    {record_info(fields, private_storage), #private_storage{}}.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    remove_user(LUser, LServer,
		gen_mod:db_type(Server, ?MODULE)).

remove_user(LUser, LServer, mnesia) ->
    F = fun () ->
		Namespaces = mnesia:select(private_storage,
					   [{#private_storage{usns =
								  {LUser,
								   LServer,
								   '$1'},
							      _ = '_'},
					     [], ['$$']}]),
		lists:foreach(fun ([Namespace]) ->
				      mnesia:delete({private_storage,
						     {LUser, LServer,
						      Namespace}})
			      end,
			      Namespaces)
	end,
    mnesia:transaction(F);
remove_user(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_user_private_storage(LServer,
					  Username);
remove_user(LUser, LServer, riak) ->
    {atomic, ejabberd_riak:delete_by_index(private_storage,
                                           <<"us">>, {LUser, LServer})}.

update_table() ->
    Fields = record_info(fields, private_storage),
    case mnesia:table_info(private_storage, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            private_storage, Fields, set,
            fun(#private_storage{usns = {U, _, _}}) -> U end,
            fun(#private_storage{usns = {U, S, NS}, xml = El} = R) ->
                    R#private_storage{usns = {iolist_to_binary(U),
                                              iolist_to_binary(S),
                                              iolist_to_binary(NS)},
                                      xml = xml:to_xmlel(El)}
            end);
      _ ->
	  ?INFO_MSG("Recreating private_storage table", []),
	  mnesia:transform_table(private_storage, ignore, Fields)
    end.

export(_Server) ->
    [{private_storage,
      fun(Host, #private_storage{usns = {LUser, LServer, XMLNS},
                                 xml = Data})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              LXMLNS = ejabberd_odbc:escape(XMLNS),
              SData =
                  ejabberd_odbc:escape(xml:element_to_binary(Data)),
              odbc_queries:set_private_data_sql(Username, LXMLNS,
                                                SData);
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, namespace, data from private_storage;">>,
      fun([LUser, XMLNS, XML]) ->
              El = #xmlel{} = xml_stream:parse_element(XML),
              #private_storage{usns = {LUser, LServer, XMLNS},
                               xml = El}
      end}].

import(_LServer, mnesia, #private_storage{} = PS) ->
    mnesia:dirty_write(PS);

import(_LServer, riak, #private_storage{usns = {LUser, LServer, _}} = PS) ->
    ejabberd_riak:put(PS, private_storage_schema(),
		      [{'2i', [{<<"us">>, {LUser, LServer}}]}]);
import(_, _, _) ->
    pass.

mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(_) -> [db_type, iqdisc].
