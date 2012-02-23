%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for private storage.
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_private).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_sm_iq/3,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(private_storage, {usns, xml}).

-define(Xmlel_Query(Attrs, Children),
(
    {xmlelement, "query", Attrs, Children}
)).

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(private_storage,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, private_storage)}]),
    update_table(),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE,
				  ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE).


process_sm_iq(#jid{luser = LUser, lserver = LServer},
  #jid{luser = LUser, lserver = LServer}, IQ)
  when IQ#iq.type == 'set' ->
    case IQ#iq.sub_el of
        {xmlelement, "query", _, Xmlels} ->
            case filter_xmlels(Xmlels) of
                [] ->
                    IQ#iq{
                      type   = error,
                      sub_el = [IQ#iq.sub_el, ?ERR_NOT_ACCEPTABLE]
                    };
                Data ->
                    mnesia:transaction(fun() ->
                        lists:foreach(fun
                            (Datum) ->
                                set_data(LUser, LServer, Datum)
                        end, Data)
                    end),
                    IQ#iq{type = result, sub_el = []}
            end;
        _ ->
            IQ#iq{type = error, sub_el = [IQ#iq.sub_el, ?ERR_NOT_ACCEPTABLE]}
    end;
%%
process_sm_iq(#jid{luser = LUser, lserver = LServer},
  #jid{luser = LUser, lserver = LServer}, IQ)
  when IQ#iq.type == 'get' ->
    case IQ#iq.sub_el of
        {xmlelement, "query", Attrs, Xmlels} ->
            case filter_xmlels(Xmlels) of
                [] ->
                    IQ#iq{
                      type   = error,
                      sub_el = [IQ#iq.sub_el, ?ERR_BAD_FORMAT]
                    };
                Data ->
                    case catch get_data(LUser, LServer, Data) of
                        {'EXIT', _Reason} ->
                            IQ#iq{
                                type   = error,
                                sub_el = [IQ#iq.sub_el, ?ERR_INTERNAL_SERVER_ERROR]
                            };
                        Storage_Xmlels ->
                            IQ#iq{
                                type   = result,
                                sub_el = [?Xmlel_Query(Attrs, Storage_Xmlels)]
                            }
                    end
            end;
        _ ->
            IQ#iq{type = error, sub_el = [IQ#iq.sub_el, ?ERR_BAD_FORMAT]}
    end;
%%
process_sm_iq(_From, _To, IQ) ->
    IQ#iq{type = error, sub_el = [IQ#iq.sub_el, ?ERR_FORBIDDEN]}.


filter_xmlels(Xmlels) ->
    filter_xmlels(Xmlels, []).

filter_xmlels([], Data) ->
    lists:reverse(Data);
filter_xmlels([{xmlelement, _, Attrs, _} = Xmlel | Xmlels], Data) ->
    case xml:get_attr_s("xmlns", Attrs) of
        ""    -> [];
        XmlNS -> filter_xmlels(Xmlels, [{XmlNS, Xmlel} | Data])
    end;
filter_xmlels([_ | Xmlels], Data) ->
    filter_xmlels(Xmlels, Data).


set_data(LUser, LServer, {XmlNS, Xmlel}) ->
    mnesia:write(#private_storage{
        usns = {LUser, LServer, XmlNS},
        xml  = Xmlel
    }).


get_data(LUser, LServer, Data) ->
    get_data(LUser, LServer, Data, []).

get_data(_LUser, _LServer, [], Storage_Xmlels) ->
    lists:reverse(Storage_Xmlels);
get_data(LUser, LServer, [{XmlNS, Xmlel} | Data], Storage_Xmlels) ->
    case mnesia:dirty_read(private_storage, {LUser, LServer, XmlNS}) of
        [#private_storage{xml = Storage_Xmlel}] ->
            get_data(LUser, LServer, Data, [Storage_Xmlel | Storage_Xmlels]);
        _ ->
            get_data(LUser, LServer, Data, [Xmlel | Storage_Xmlels])
    end.


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    F = fun() ->
		Namespaces = mnesia:select(
			    private_storage,
			    [{#private_storage{usns={LUser, LServer, '$1'},
					       _ = '_'},
			     [],
			     ['$$']}]),
		lists:foreach(
		  fun([Namespace]) ->
			  mnesia:delete({private_storage,
					 {LUser, LServer, Namespace}})
		     end, Namespaces)
        end,
    mnesia:transaction(F).


update_table() ->
    Fields = record_info(fields, private_storage),
    case mnesia:table_info(private_storage, attributes) of
	Fields ->
	    ok;
	[userns, xml] ->
	    ?INFO_MSG("Converting private_storage table from "
		      "{user, default, lists} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_private_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, private_storage},
			      {attributes, record_info(fields, private_storage)}]),
	    mnesia:transform_table(private_storage, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_private_tmp_table),
			 mnesia:foldl(
			   fun(#private_storage{usns = {U, NS}} = R, _) ->
				   mnesia:dirty_write(
				     mod_private_tmp_table,
				     R#private_storage{usns = {U, Host, NS}})
			   end, ok, private_storage)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(private_storage),
	    F2 = fun() ->
			 mnesia:write_lock_table(private_storage),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_private_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_private_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating private_storage table", []),
	    mnesia:transform_table(private_storage, ignore, Fields)
    end.


