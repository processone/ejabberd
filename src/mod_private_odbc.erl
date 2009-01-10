%%%----------------------------------------------------------------------
%%% File    : mod_private_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Private storage support
%%% Created :  5 Oct 2006 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

start(Host, Opts) ->
    HostB = list_to_binary(Host),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_PRIVATE,
				  ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_PRIVATE).


process_sm_iq(From, To, #iq{type = Type} = IQ_Rec) ->
    case check_packet(From, To, IQ_Rec) of
	ok ->
	    case Type of
		set ->
		    process_iq_set(From, To, IQ_Rec);
		get ->
		    process_iq_get(From, To, IQ_Rec)
	    end;
	{error, Error} ->
	    exmpp_iq:error(IQ_Rec, Error)
    end.

process_iq_get(From, _To, #iq{payload = SubEl} = IQ_Rec) ->
    LUser = exmpp_jid:lnode_as_list(From),
    LServer = exmpp_jid:ldomain_as_list(From),
    case catch get_data(LUser,
			LServer,
			exmpp_xml:get_child_elements(SubEl)) of
	{'EXIT', _Reason} ->
	    {error, 'internal-server-error'};
	Res ->
	    exmpp_iq:result(IQ_Rec, #xmlel{ns = ?NS_PRIVATE,
					   name = 'query',
					   children = Res})
    end.


process_iq_set(From, _To, #iq{payload = SubEl} = IQ_Rec) ->
    LUser = exmpp_jid:lnode_as_list(From),
    LServer = exmpp_jid:ldomain_as_list(From),
    F = fun() ->
        lists:foreach(
          fun(El) ->
              set_data(LUser, LServer, El)
          end, exmpp_xml:get_child_elements(SubEl))
    end,
    odbc_queries:sql_transaction(LServer, F),
    exmpp_iq:result(IQ_Rec).


check_packet(From, To, IQ_Rec) ->
    check_packet(From, To, IQ_Rec, [ fun check_domain/3,
				     fun check_user/3,
				     fun check_ns/3]).
check_packet(_From, _To, _IQ_Rec, []) ->
    ok;
check_packet(From, To, IQ_Rec, [F | R]) ->
    case F(From, To, IQ_Rec) of
	{error, _} = Error -> Error;
	ok -> check_packet(From, To, IQ_Rec, R)
    end.

check_domain(From, _To, _IQ_Rec) ->
    LServer = exmpp_jid:ldomain_as_list(From),
    case lists:member(LServer, ?MYHOSTS) of
	true -> ok;
	false -> {error, 'not-allowed'}
    end.

% the iq can't be directed to another jid
check_user(From, To, _IQ_Rec) ->
    case exmpp_jid:compare_bare_jids(From, To) of
	true -> ok;
	false -> {error, 'forbidden'}
    end.

%there must be at least one child, and every child should have
%a namespace specified (reject if the namespace is jabber:iq:private,
%the same than the parent element).
check_ns(_From, _To, #iq{payload = SubEl}) ->
    case exmpp_xml:get_child_elements(SubEl) of
	[] ->
	    {error, 'not-acceptable'};
	Children ->
	    case lists:any(fun(Child) ->
			       exmpp_xml:get_ns_as_atom(Child) =:= ?NS_PRIVATE
			   end, Children) of
		true -> {error, 'not-acceptable'};
		false -> ok
	    end
    end.



set_data(LUser, LServer, El) ->
    XMLNS = exmpp_xml:get_ns_as_list(El),
    Username = ejabberd_odbc:escape(LUser),
	LXMLNS = ejabberd_odbc:escape(XMLNS),
	SData = ejabberd_odbc:escape(exmpp_xml:document_to_list(El)),
	odbc_queries:set_private_data(LServer, Username, LXMLNS, SData).

get_data(LUser, LServer, Els) ->
    get_data(LUser, LServer, Els, []).

get_data(_LUser, _LServer, [], Res) ->
    lists:reverse(Res);
get_data(LUser, LServer, [El | Els], Res) ->
    XMLNS = exmpp_xml:get_ns_as_list(El),
    Username = ejabberd_odbc:escape(LUser),
    LXMLNS = ejabberd_odbc:escape(XMLNS),
    case catch odbc_queries:get_private_data(LServer, Username, LXMLNS) of
    {selected, ["data"], [{SData}]} ->
	[Data] = exmpp_xml:parse_document(SData,
                         [names_as_atom, {check_elems, xmpp}, 
                          {check_nss,xmpp}, {check_attrs,xmpp}]),
	get_data(LUser, LServer, Els, [Data | Res]);
    %% MREMOND: I wonder when the query could return a vcard ?
    {selected, ["vcard"], []} ->
	get_data(LUser, LServer, Els,
	  [El | Res]);
    _ ->
	get_data(LUser, LServer, Els, [El | Res])
end.


remove_user(User, Server) when is_binary(User), is_binary(Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	Username = ejabberd_odbc:escape(LUser),
	odbc_queries:del_user_private_storage(LServer, Username)
    catch
	_ ->
	    ok
    end.
