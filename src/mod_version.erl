%%%----------------------------------------------------------------------
%%% File    : mod_version.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XEP-0092: Software Version
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(mod_version).

-author('alexey@process-one.net').

-protocol({xep, 92, '1.1'}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_local_iq/1,
	 mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_VERSION, ?MODULE, process_local_iq,
				  IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_VERSION).

reload(Host, NewOpts, OldOpts) ->
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
	    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VERSION,
					  ?MODULE, process_local_iq, IQDisc);
	true ->
	    ok
    end.

process_local_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_local_iq(#iq{type = get, to = To} = IQ) ->
    Host = To#jid.lserver,
    OS = case gen_mod:get_module_opt(Host, ?MODULE, show_os,
				     fun(B) when is_boolean(B) -> B end,
				     true) of
	     true -> get_os();
	     false -> undefined
	 end,
    xmpp:make_iq_result(IQ, #version{name = <<"ejabberd">>,
				     ver = ?VERSION,
				     os = OS}).

get_os() ->
    {Osfamily, Osname} = os:type(),
    OSType = list_to_binary([atom_to_list(Osfamily), $/, atom_to_list(Osname)]),
    OSVersion = case os:version() of
		  {Major, Minor, Release} ->
		      (str:format("~w.~w.~w",
						     [Major, Minor, Release]));
		  VersionString -> VersionString
		end,
    <<OSType/binary, " ", OSVersion/binary>>.

depends(_Host, _Opts) ->
    [].

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(show_os) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [iqdisc, show_os].
