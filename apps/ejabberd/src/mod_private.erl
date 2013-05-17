%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for private storage.
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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


process_sm_iq(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
	true ->
            if
                From#jid.luser == To#jid.luser ->
                        #xmlel{name = Name, attrs = Attrs,
                               children = Els} = SubEl,
	                case Type of
		                set ->
		                        F = fun() ->
				                lists:foreach(
				                fun(El) ->
					        set_data(LUser, LServer, El)
				                end, Els)
			                end,
		                        mnesia:transaction(F),
		                        IQ#iq{type = result,
			                        sub_el = [SubEl]};
		                get ->
		                        case catch get_data(LUser, LServer, Els) of
			                        {'EXIT', _Reason} ->
			                                IQ#iq{type = error,
				                                sub_el = [SubEl,
					                        ?ERR_INTERNAL_SERVER_ERROR]};
			                        Res ->
			                                IQ#iq{type = result,
				                        sub_el = [SubEl#xmlel{children = Res}]}
		                        end
	                end;
                true ->
                        IQ#iq{type = error,
                              sub_el = [SubEl,
                                        ?ERR_FORBIDDEN]}
            end;
	false ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.

set_data(LUser, LServer, El) ->
    case El of
	#xmlel{attrs = Attrs} ->
            XMLNS = xml:get_attr_s(<<"xmlns">>, Attrs),
	    case XMLNS of
                <<>> ->
		    ignore;
		_ ->
		    mnesia:write(
		      #private_storage{usns = {LUser, LServer, XMLNS},
				       xml = El})
	    end;
	_ ->
	    ignore
    end.

get_data(LUser, LServer, Els) ->
    get_data(LUser, LServer, Els, []).

get_data(_LUser, _LServer, [], Res) ->
    lists:reverse(Res);
get_data(LUser, LServer, [El | Els], Res) ->
    case El of
	#xmlel{attrs = Attrs} ->
            XMLNS = xml:get_attr_s(<<"xmlns">>, Attrs),
	    case mnesia:dirty_read(private_storage, {LUser, LServer, XMLNS}) of
		[R] ->
		    get_data(LUser, LServer, Els,
			     [R#private_storage.xml | Res]);
		[] ->
		    get_data(LUser, LServer, Els,
			     [El | Res])
	    end;
	_ ->
	    get_data(LUser, LServer, Els, Res)
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


