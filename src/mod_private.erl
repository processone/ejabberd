%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for private storage.
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_private).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_sm_iq/3,
	 remove_user/2]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

-record(private_storage, {usns, xml}).

start(Host, Opts) ->
    HostB = list_to_binary(Host),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(private_storage,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, private_storage)}]),
    update_table(),
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
    mnesia:transaction(F),
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
    XMLNS = exmpp_xml:get_ns_as_atom(El),
    mnesia:write(#private_storage{usns = {LUser, LServer, XMLNS},
				  xml = El}).

get_data(LUser, LServer, Els) ->
    get_data(LUser, LServer, Els, []).

get_data(_LUser, _LServer, [], Res) ->
    lists:reverse(Res);
get_data(LUser, LServer, [El | Els], Res) ->
    XMLNS = exmpp_xml:get_ns_as_atom(El),
    case mnesia:dirty_read(private_storage, {LUser, LServer, XMLNS}) of
	[R] ->
	    get_data(LUser, LServer, Els,
	      [R#private_storage.xml | Res]);
	[] ->
	    get_data(LUser, LServer, Els,
	      [El | Res])
    end.

remove_user(User, Server) 
        when is_binary(User), is_binary(Server) ->
    try
	LUser = exmpp_stringprep:nodeprep(User),
	LServer = exmpp_stringprep:nameprep(Server),
	F = fun() ->
		    Namespaces = mnesia:select(
				private_storage,
				[{#private_storage{usns = {LUser, LServer, '$1'},
						   _ = '_'},
				 [],
				 ['$$']}]),
		    lists:foreach(
		      fun([Namespace]) ->
			      mnesia:delete({private_storage,
					     {LUser, LServer, Namespace}})
			 end, Namespaces)
	    end,
	mnesia:transaction(F)
    catch
	_ ->
	    ok
    end.


update_table() ->
    Fields = record_info(fields, private_storage),
    case mnesia:table_info(private_storage, attributes) of
	Fields ->
            convert_to_exmpp();
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
			   fun(#private_storage{usns = {U, NS}, xml = El} = R, _) ->
				   NS1 = list_to_atom(NS),
				   El0 = exmpp_xml:xmlelement_to_xmlel(El,
				     [?NS_PRIVATE], [{?NS_XMPP, ?NS_XMPP_pfx}]),
				   El1 = exmpp_xml:remove_whitespaces_deeply(El0),
				   mnesia:dirty_write(
				     mod_private_tmp_table,
				     R#private_storage{usns = {U, Host, NS1}, xml = El1})
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


convert_to_exmpp() ->
    Fun = fun() ->
	    case mnesia:first(private_storage) of
		'$end_of_table' ->
		    none;
		Key ->
		    case mnesia:read({private_storage, Key}) of
			[#private_storage{xml = #xmlel{}}] ->
			    none;
			[#private_storage{xml = #xmlelement{}}] ->
			    mnesia:foldl(fun convert_to_exmpp2/2,
			      done, private_storage, write)
		    end
	    end
    end,
    mnesia:transaction(Fun).

convert_to_exmpp2(#private_storage{usns = {U, S, NS} = Key, xml = El} = R,
  Acc) ->
    mnesia:delete({private_storage, Key}),
    NS1 = list_to_atom(NS),
    El0 = exmpp_xml:xmlelement_to_xmlel(El,
      [?NS_PRIVATE], [{?NS_XMPP, ?NS_XMPP_pfx}]),
    El1 = exmpp_xml:remove_whitespaces_deeply(El0),
    New_R = R#private_storage{
      usns = {U, S, NS1},
      xml = El1},
    mnesia:write(New_R),
    Acc.
