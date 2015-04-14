%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_sm.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : Bytestreams manager.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(mod_proxy65_sm).

-author('xram@jabber.ru').

-behaviour(gen_server).

%% gen_server callbacks.
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

%% API.
-export([start_link/2, register_stream/1,
	 unregister_stream/1, activate_stream/4]).

-record(state, {max_connections = infinity :: non_neg_integer() | infinity}).

-include("jlib.hrl").

-record(bytestream,
	{sha1 = <<"">> :: binary() | '$1',
         target :: pid() | '_',
         initiator :: pid() | '_',
         active = false :: boolean() | '_',
         jid_i = {<<"">>, <<"">>, <<"">>} :: ljid() | '_'}).

-define(PROCNAME, ejabberd_mod_proxy65_sm).

%% Unused callbacks.
handle_cast(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> {noreply, State}.

%%----------------

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Opts],
			  []).

init([Opts]) ->
    mnesia:create_table(bytestream, [{ram_copies, [node()]},
				     {attributes, record_info(fields, bytestream)}]),
    mnesia:add_table_copy(bytestream, node(), ram_copies),
    MaxConnections = gen_mod:get_opt(max_connections, Opts,
                                     fun(I) when is_integer(I), I>0 ->
                                             I;
                                        (infinity) ->
                                             infinity
                                     end, infinity),
    {ok, #state{max_connections = MaxConnections}}.

terminate(_Reason, _State) -> ok.

handle_call({activate, SHA1, IJid}, _From, State) ->
    MaxConns = State#state.max_connections,
    F = fun () ->
		case mnesia:read(bytestream, SHA1, write) of
		  [#bytestream{target = TPid, initiator = IPid} =
		       ByteStream]
		      when is_pid(TPid), is_pid(IPid) ->
		      ActiveFlag = ByteStream#bytestream.active,
		      if ActiveFlag == false ->
			     ConnsPerJID = mnesia:select(bytestream,
							 [{#bytestream{sha1 =
									   '$1',
								       jid_i =
									   IJid,
								       _ = '_'},
							   [], ['$1']}]),
			     if length(ConnsPerJID) < MaxConns ->
				    mnesia:write(ByteStream#bytestream{active =
									   true,
								       jid_i =
									   IJid}),
				    {ok, IPid, TPid};
				true -> {limit, IPid, TPid}
			     end;
			 true -> conflict
		      end;
		  _ -> false
		end
	end,
    Reply = mnesia:transaction(F),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%%----------------------
%%% API.
%%%----------------------
%%%---------------------------------------------------
%%% register_stream(SHA1) -> {atomic, ok}      |
%%%                          {atomic, error}   |
%%%                          transaction abort
%%% SHA1 = string()
%%%---------------------------------------------------
register_stream(SHA1) when is_binary(SHA1) ->
    StreamPid = self(),
    F = fun () ->
		case mnesia:read(bytestream, SHA1, write) of
		  [] ->
		      mnesia:write(#bytestream{sha1 = SHA1,
					       target = StreamPid});
		  [#bytestream{target = Pid, initiator = undefined} =
		       ByteStream]
		      when is_pid(Pid), Pid /= StreamPid ->
		      mnesia:write(ByteStream#bytestream{initiator =
							     StreamPid});
		  _ -> error
		end
	end,
    mnesia:transaction(F).

%%%----------------------------------------------------
%%% unregister_stream(SHA1) -> ok | transaction abort
%%% SHA1 = string()
%%%----------------------------------------------------
unregister_stream(SHA1) when is_binary(SHA1) ->
    F = fun () -> mnesia:delete({bytestream, SHA1}) end,
    mnesia:transaction(F).

%%%--------------------------------------------------------
%%% activate_stream(SHA1, IJid, TJid, Host) -> ok       |
%%%                                            false    |
%%%                                            limit    |
%%%                                            conflict |
%%%                                            error
%%% SHA1 = string()
%%% IJid = TJid = jid()
%%% Host = string()
%%%--------------------------------------------------------
activate_stream(SHA1, IJid, TJid, Host)
    when is_binary(SHA1) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    case catch gen_server:call(Proc, {activate, SHA1, IJid})
	of
      {atomic, {ok, IPid, TPid}} ->
	  mod_proxy65_stream:activate({IPid, IJid}, {TPid, TJid});
      {atomic, {limit, IPid, TPid}} ->
	  mod_proxy65_stream:stop(IPid),
	  mod_proxy65_stream:stop(TPid),
	  limit;
      {atomic, conflict} -> conflict;
      {atomic, false} -> false;
      _ -> error
    end.
