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

-include("ejabberd.hrl").
-include("jlib.hrl").



start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VERSION,
				  ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VERSION).


process_local_iq(_From, To, #iq{id = _ID, type = Type,
			       xmlns = _XMLNS, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    Host = To#jid.lserver,
	    OS = case gen_mod:get_module_opt(Host, ?MODULE, show_os, true) of
		     true -> [get_os()];
		     false -> []
		 end,
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "query",
			     [{"xmlns", ?NS_VERSION}],
			     [{xmlelement, "name", [],
			       [{xmlcdata, "ejabberd"}]},
			      {xmlelement, "version", [],
			       [{xmlcdata, ?VERSION}]}
			     ] ++ OS
			    }]}
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
    {xmlelement, "os", [], [{xmlcdata, OS}]}.
