%%%----------------------------------------------------------------------
%%% File    : mod_version.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XEP-0092: Software Version
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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
	 mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_VERSION, ?MODULE, process_local_iq).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_VERSION).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

process_local_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_local_iq(#iq{type = get, to = To} = IQ) ->
    Host = To#jid.lserver,
    OS = case mod_version_opt:show_os(Host) of
	     true -> get_os();
	     false -> undefined
	 end,
    xmpp:make_iq_result(IQ, #version{name = <<"ejabberd">>,
				     ver = ejabberd_option:version(),
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

mod_opt_type(show_os) ->
    econf:bool().

mod_options(_Host) ->
    [{show_os, true}].

mod_doc() ->
    #{desc =>
          ?T("This module implements "
             "https://xmpp.org/extensions/xep-0092.html"
             "[XEP-0092: Software Version]. Consequently, "
             "it answers ejabberd's version when queried."),
      opts =>
          [{show_os,
            #{value => "true | false",
              desc =>
                  ?T("Should the operating system be revealed or not. "
                     "The default value is 'true'.")}}]}.
