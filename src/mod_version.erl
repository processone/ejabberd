%%%----------------------------------------------------------------------
%%% File    : mod_version.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : XEP-0092: Software Version
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%%----------------------------------------------------------------------

-module(mod_version).
-author('alexey@sevcom.net').

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


process_local_iq(From, To, #iq{id = ID, type = Type,
			       xmlns = XMLNS, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    Host = To#jid.server,
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
