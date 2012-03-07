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

%%% Database schema (version / storage / table)
%%%
%%% 2.1.x / mnesia / private_storage
%%%  usns = {Username::string(), Host::string(), Namespace::string()}
%%%  xml = xmlelement()
%%%
%%% 2.1.x / odbc / private_storage
%%%  username = varchar250
%%%  namespace = varchar250
%%%  data = text
%%%
%%% 3.0.0-prealpha / mnesia / private_storage
%%%  usns = {Username::binary(), Host::binary(), Namespace::atom()}
%%%  xml = xmlel()
%%%
%%% 3.0.0-prealpha / odbc / private_storage
%%%  Same as 2.1.x
%%%
%%% 3.0.0-alpha / mnesia / private_storage
%%%  user_host_ns = {Username::binary(), Host::binary(), Namespace::atom()}
%%%  xml = xmlel()
%%%
%%% 3.0.0-alpha / odbc / private_storage
%%%  user = varchar
%%%  host = varchar
%%%  ns = varchar250
%%%  xml = text

-module(mod_private).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_sm_iq/3,
	 remove_user/2]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

%% TODO: usns instead of user_host_ns requires no migration
-record(private_storage, {user_host_ns, xml}).

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    gen_storage:create_table(Backend, HostB, private_storage,
			     [{disc_only_copies, [node()]},
			      {odbc_host, HostB},
			      {attributes, record_info(fields, private_storage)},
			      {types, [{user_host_ns, {binary, binary, atom}}, {xml, xmlel}]}]),
    update_table(HostB, Backend),
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
    LUser = exmpp_jid:prep_node(From),
    LServer = exmpp_jid:prep_domain(From),
    case catch get_data(LUser,
			LServer,
			exmpp_xml:get_child_elements(SubEl)) of
	{'EXIT', _Reason} ->
            exmpp_iq:error(IQ_Rec, 'internal-server-error');
	Res ->
	    exmpp_iq:result(IQ_Rec, #xmlel{ns = ?NS_PRIVATE,
					   name = 'query',
					   children = Res})
    end.


process_iq_set(From, _To, #iq{payload = SubEl} = IQ_Rec) ->
    LUser = exmpp_jid:prep_node(From),
    LServer = exmpp_jid:prep_domain(From),
    F = fun() ->
        lists:foreach(
          fun(El) ->
              set_data(LUser, LServer, El)
          end, exmpp_xml:get_child_elements(SubEl))
    end,
    gen_storage:transaction(LServer, private_storage, F),
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
    LServer = exmpp_jid:prep_domain_as_list(From),
    case ?IS_MY_HOST(LServer) of
	true -> ok;
	false -> {error, 'not-allowed'}
    end.

% the iq can't be directed to another jid
check_user(From, To, _IQ_Rec) ->
    case exmpp_jid:bare_compare(From, To) of
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

%% The xml is stored as xmlel() in mnesia, but as text in odbc
set_data(LUser, LServer, El) ->
    XMLNS = exmpp_xml:get_ns_as_atom(El),
    gen_storage:write(LServer,
		      #private_storage{user_host_ns = {LUser, LServer, XMLNS},
				       xml = El}).

get_data(LUser, LServer, Els) ->
    get_data(LUser, LServer, Els, []).

get_data(_LUser, _LServer, [], Res) ->
    lists:reverse(Res);
get_data(LUser, LServer, [El | Els], Res) ->
    XMLNS = exmpp_xml:get_ns_as_atom(El),
    case gen_storage:dirty_read(LServer, private_storage, {LUser, LServer, XMLNS}) of
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
		Records = gen_storage:select(LServer, private_storage,
					     [{'=', user_host_ns, {LUser, LServer, '_'}}]),
		lists:foreach(
		  fun(#private_storage{user_host_ns = USNS}) ->
			  gen_storage:delete(LServer, {private_storage, USNS})
		     end, Records)
	    end,
	gen_storage:transaction(LServer, private_storage, F)
    catch
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_table(global, Storage) ->
    [update_table(HostB, Storage) || HostB <- ejabberd_hosts:get_hosts(ejabberd)];

update_table(Host, mnesia) ->
    gen_storage_migration:migrate_mnesia(
      Host, private_storage,
      [{private_storage, [usns, xml],
	fun({private_storage, {User, Server, NS}, Xml}) ->
		U1 = list_to_binary(User),
		S1 = list_to_binary(Server),
		NS1 = list_to_atom(NS),
		El1 = exmpp_xml:xmlelement_to_xmlel(Xml, [?NS_PRIVATE],
						    [{?NS_XMPP, ?NS_XMPP_pfx}]),
		#private_storage{user_host_ns = {U1, S1, NS1},
				 xml = El1}
	end}]);

update_table(Host, odbc) ->
    gen_storage_migration:migrate_odbc(
      Host, [private_storage],
      [{"private_storage", ["username", "namespace", "data"],
	fun(_, Username, Namespace, Data) ->
		[#private_storage{user_host_ns = {Username, Host, Namespace},
				  xml = Data}]
	end}]).
