%%%-------------------------------------------------------------------
%%% Created : 16 Jan 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------
-module(mod_proxy65_mnesia).
-behaviour(gen_server).
-behaviour(mod_proxy65).

%% API
-export([init/0, register_stream/2, unregister_stream/1, activate_stream/4]).
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").

-record(bytestream,
	{sha1 = <<"">> :: binary() | '$1',
         target :: pid() | '_',
         initiator :: pid() | '_' | undefined,
         active = false :: boolean() | '_',
         jid_i :: undefined | binary() | '_'}).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init() ->
    Spec = {?MODULE, {?MODULE, start_link, []}, transient,
	    5000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_backend_sup, Spec).

register_stream(SHA1, StreamPid) ->
    F = fun () ->
		case mnesia:read(bytestream, SHA1, write) of
		    [] ->
			mnesia:write(#bytestream{sha1 = SHA1,
						 target = StreamPid});
		    [#bytestream{target = Pid, initiator = undefined} =
			 ByteStream] when is_pid(Pid), Pid /= StreamPid ->
			mnesia:write(ByteStream#bytestream{
				       initiator = StreamPid})
		end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, Reason}
    end.

unregister_stream(SHA1) ->
    F = fun () -> mnesia:delete({bytestream, SHA1}) end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, Reason}
    end.

activate_stream(SHA1, Initiator, MaxConnections, _Node) ->
    case gen_server:call(?MODULE,
			 {activate_stream, SHA1, Initiator, MaxConnections}) of
	{atomic, {ok, IPid, TPid}} ->
	    {ok, IPid, TPid};
	{atomic, {limit, IPid, TPid}} ->
	    {error, {limit, IPid, TPid}};
	{atomic, conflict} ->
	    {error, conflict};
	{atomic, notfound} ->
	    {error, notfound};
	Err ->
	    {error, Err}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ejabberd_mnesia:create(?MODULE, bytestream,
			   [{ram_copies, [node()]},
			    {attributes, record_info(fields, bytestream)}]),
    mnesia:add_table_copy(bytestream, node(), ram_copies),
    {ok, #state{}}.

handle_call({activate_stream, SHA1, Initiator, MaxConnections}, _From, State) ->
    F = fun () ->
		case mnesia:read(bytestream, SHA1, write) of
		    [#bytestream{target = TPid, initiator = IPid} =
			 ByteStream] when is_pid(TPid), is_pid(IPid) ->
			ActiveFlag = ByteStream#bytestream.active,
			if ActiveFlag == false ->
				ConnsPerJID = mnesia:select(
						bytestream,
						[{#bytestream{sha1 = '$1',
							      jid_i = Initiator,
							      _ = '_'},
						  [], ['$1']}]),
				if length(ConnsPerJID) < MaxConnections ->
					mnesia:write(
					  ByteStream#bytestream{active = true,
								jid_i = Initiator}),
					{ok, IPid, TPid};
				   true ->
					{limit, IPid, TPid}
				end;
			   true ->
				conflict
			end;
		    _ ->
			notfound
		end
	end,
    Reply = mnesia:transaction(F),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
