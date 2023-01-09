%%%----------------------------------------------------------------------
%%% File    : gen_iq_handler.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : IQ handler support
%%% Created : 22 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(gen_iq_handler).

-author('alexey@process-one.net').

%% API
-export([add_iq_handler/5, remove_iq_handler/3, handle/1, handle/2,
	 start/1, get_features/2]).
%% Deprecated functions
-export([add_iq_handler/6, handle/5, iqdisc/1]).
-deprecated([{add_iq_handler, 6}, {handle, 5}, {iqdisc, 1}]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").
-include("ejabberd_stacktrace.hrl").

-type component() :: ejabberd_sm | ejabberd_local.

%%====================================================================
%% API
%%====================================================================
-spec start(component()) -> ok.
start(Component) ->
    catch ets:new(Component, [named_table, public, ordered_set,
			      {read_concurrency, true},
			      {heir, erlang:group_leader(), none}]),
    ok.

-spec add_iq_handler(component(), binary(), binary(), module(), atom()) -> ok.
add_iq_handler(Component, Host, NS, Module, Function) ->
    ets:insert(Component, {{Host, NS}, Module, Function}),
    ok.

-spec remove_iq_handler(component(), binary(), binary()) -> ok.
remove_iq_handler(Component, Host, NS) ->
    ets:delete(Component, {Host, NS}),
    ok.

-spec handle(iq()) -> ok.
handle(#iq{to = To} = IQ) ->
    Component = case To#jid.luser of
		    <<"">> -> ejabberd_local;
		    _ -> ejabberd_sm
		end,
    handle(Component, IQ).

-spec handle(component(), iq()) -> ok.
handle(Component,
       #iq{to = To, type = T, lang = Lang, sub_els = [El]} = Packet)
  when T == get; T == set ->
    XMLNS = xmpp:get_ns(El),
    Host = To#jid.lserver,
    case ets:lookup(Component, {Host, XMLNS}) of
	[{_, Module, Function}] ->
	    process_iq(Host, Module, Function, Packet);
	[] ->
	    Txt = ?T("No module is handling this query"),
	    Err = xmpp:err_service_unavailable(Txt, Lang),
	    ejabberd_router:route_error(Packet, Err)
    end;
handle(_, #iq{type = T, lang = Lang, sub_els = SubEls} = Packet)
  when T == get; T == set ->
    Txt = case SubEls of
	      [] -> ?T("No child elements found");
	      _ -> ?T("Too many child elements")
	  end,
    Err = xmpp:err_bad_request(Txt, Lang),
    ejabberd_router:route_error(Packet, Err);
handle(_, #iq{type = T}) when T == result; T == error ->
    ok.

-spec get_features(component(), binary()) -> [binary()].
get_features(Component, Host) ->
    get_features(Component, ets:next(Component, {Host, <<"">>}), Host, []).

get_features(Component, {Host, XMLNS}, Host, XMLNSs) ->
    get_features(Component,
		 ets:next(Component, {Host, XMLNS}), Host, [XMLNS|XMLNSs]);
get_features(_, _, _, XMLNSs) ->
    XMLNSs.

-spec process_iq(binary(), atom(), atom(), iq()) -> ok.
process_iq(_Host, Module, Function, IQ) ->
    try process_iq(Module, Function, IQ) of
	#iq{} = ResIQ ->
	    ejabberd_router:route(ResIQ);
	ignore ->
	    ok
    catch ?EX_RULE(Class, Reason, St) ->
	    StackTrace = ?EX_STACK(St),
	    ?ERROR_MSG("Failed to process iq:~n~ts~n** ~ts",
		       [xmpp:pp(IQ),
			misc:format_exception(2, Class, Reason, StackTrace)]),
	    Txt = ?T("Module failed to handle the query"),
	    Err = xmpp:err_internal_server_error(Txt, IQ#iq.lang),
	    ejabberd_router:route_error(IQ, Err)
    end.

-spec process_iq(module(), atom(), iq()) -> ignore | iq().
process_iq(Module, Function, #iq{lang = Lang, sub_els = [El]} = IQ) ->
    try
	Pkt = case erlang:function_exported(Module, decode_iq_subel, 1) of
		  true -> Module:decode_iq_subel(El);
		  false -> xmpp:decode(El)
	      end,
	Module:Function(IQ#iq{sub_els = [Pkt]})
    catch error:{xmpp_codec, Why} ->
	    Txt = xmpp:io_format_error(Why),
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang))
    end.

-spec iqdisc(binary() | global) -> no_queue.
iqdisc(_Host) ->
    no_queue.

%%====================================================================
%% Deprecated API
%%====================================================================
-spec add_iq_handler(module(), binary(), binary(), module(), atom(), any()) -> ok.
add_iq_handler(Component, Host, NS, Module, Function, _Type) ->
    add_iq_handler(Component, Host, NS, Module, Function).

-spec handle(binary(), atom(), atom(), any(), iq()) -> any().
handle(Host, Module, Function, _Opts, IQ) ->
    process_iq(Host, Module, Function, IQ).
