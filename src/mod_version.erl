%%%----------------------------------------------------------------------
%%% File    : mod_version.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XEP-0092: Software Version
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_version).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_local_iq/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").


start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_SOFT_VERSION,
				  ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, list_to_binary(Host), ?NS_SOFT_VERSION).


process_local_iq(_From, To, #iq{type = Type} = IQ_Rec) ->
    case Type of
	set ->
	    exmpp_iq:error(IQ_Rec, 'not-allowed');
	get ->
	    Host = exmpp_jid:domain_as_list(To),
	    OS = case gen_mod:get_module_opt(Host, ?MODULE, show_os, true) of
		     true -> [get_os()];
		     false -> []
		 end,
	    R = #xmlel{ns = ?NS_SOFT_VERSION, name = 'query',
		       children = [ exmpp_xml:set_cdata(#xmlel{ns = ?NS_SOFT_VERSION,
							       name = 'name'},
							<<"ejabberd">>),
				    exmpp_xml:set_cdata(#xmlel{ns = ?NS_SOFT_VERSION,
							       name = 'version'},
							?VERSION) | OS]},
	    exmpp_iq:result(IQ_Rec, R)
    end.


get_os() ->
    OSType = case os:type() of
		 {Osfamily, Osname} ->
		     atom_to_list(Osfamily) ++ "/" ++
			 atom_to_list(Osname);
		 Osfamily ->
		     atom_to_list(Osfamily)
	     end,
    OSVersion = case os:version() of
		    {Major, Minor, Release} ->
			lists:flatten(
			  io_lib:format("~w.~w.~w",
					[Major, Minor, Release]));
		    VersionString ->
			VersionString
		end,
    OS = OSType ++ " " ++ OSVersion,
    exmpp_xml:set_cdata(#xmlel{ns = ?NS_SOFT_VERSION, name = 'os'}, OS).
