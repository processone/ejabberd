%%%----------------------------------------------------------------------
%%% File    : mod_time.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Purpose :
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-module(mod_time).

-author('alexey@process-one.net').

-protocol({xep, 202, '2.0', '2.1.0', "complete", ""}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_local_iq/1,
	 mod_options/1, depends/2, mod_doc/0]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

start(_Host, _Opts) ->
    {ok, [{iq_handler, ejabberd_local, ?NS_TIME, process_local_iq}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec process_local_iq(iq()) -> iq().
process_local_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_local_iq(#iq{type = get} = IQ) ->
    Now = os:timestamp(),
    Now_universal = calendar:now_to_universal_time(Now),
    Now_local = calendar:universal_time_to_local_time(Now_universal),
    Seconds_diff =
	calendar:datetime_to_gregorian_seconds(Now_local) -
	calendar:datetime_to_gregorian_seconds(Now_universal),
    {Hd, Md, _} = calendar:seconds_to_time(abs(Seconds_diff)),
    xmpp:make_iq_result(IQ, #time{tzo = {Hd, Md}, utc = Now}).

depends(_Host, _Opts) ->
    [].

mod_options(_Host) ->
    [].

mod_doc() ->
    #{desc =>
          ?T("This module adds support for "
             "https://xmpp.org/extensions/xep-0202.html"
             "[XEP-0202: Entity Time]. In other words, "
             "the module reports server's system time.")}.
