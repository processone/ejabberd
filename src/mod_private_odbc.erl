%%%----------------------------------------------------------------------
%%% File    : mod_private_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Private storage support
%%% Created :  5 Oct 2006 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_private_odbc).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_sm_iq/3,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE,
				  ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE).


process_sm_iq(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    {xmlelement, Name, Attrs, Els} = SubEl,
	    case Type of
		set ->
		    F = fun() ->
				lists:foreach(
				  fun(El) ->
					  set_data(LUser, LServer, El)
				  end, Els)
			end,
		    odbc_queries:sql_transaction(LServer, F),
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, Name, Attrs, []}]};
		get ->
		    case catch get_data(LUser, LServer, Els) of
			{'EXIT', _Reason} ->
			    IQ#iq{type = error,
				  sub_el = [SubEl,
					    ?ERR_INTERNAL_SERVER_ERROR]};
			Res ->
			    IQ#iq{type = result,
				  sub_el = [{xmlelement, Name, Attrs, Res}]}
		    end
	    end;
	false ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.

set_data(LUser, LServer, El) ->
    case El of
	{xmlelement, _Name, Attrs, _Els} ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs),
	    case XMLNS of
		"" ->
		    ignore;
		_ ->
		    Username = ejabberd_odbc:escape(LUser),
		    LXMLNS = ejabberd_odbc:escape(XMLNS),
		    SData = ejabberd_odbc:escape(
			      xml:element_to_binary(El)),
			odbc_queries:set_private_data(LServer, Username, LXMLNS, SData)
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
	{xmlelement, _Name, Attrs, _} ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs),
	    Username = ejabberd_odbc:escape(LUser),
	    LXMLNS = ejabberd_odbc:escape(XMLNS),
	    case catch odbc_queries:get_private_data(LServer, Username, LXMLNS) of
		{selected, ["data"], [{SData}]} ->
		    case xml_stream:parse_element(SData) of
			Data when element(1, Data) == xmlelement ->
			    get_data(LUser, LServer, Els,
				     [Data | Res])
		    end;
		%% MREMOND: I wonder when the query could return a vcard ?
		{selected, ["vcard"], []} ->
		    get_data(LUser, LServer, Els,
			     [El | Res]);
	    _ -> 
	        get_data(LUser, LServer, Els,[El | Res])
	    end;
	_ ->
	    get_data(LUser, LServer, Els, Res)
    end.


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    odbc_queries:del_user_private_storage(LServer, Username).
