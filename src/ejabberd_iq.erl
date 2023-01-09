%%%-------------------------------------------------------------------
%%% File    : ejabberd_iq.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose :
%%% Created : 10 Nov 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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
%%%-------------------------------------------------------------------

-module(ejabberd_iq).

-behaviour(gen_server).

%% API
-export([start_link/0, route/4, dispatch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_stacktrace.hrl").

-record(state, {expire = infinity :: timeout()}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec route(iq(), atom() | pid(), term(), non_neg_integer()) -> ok.
route(#iq{type = T} = IQ, Proc, Ctx, Timeout) when T == set; T == get ->
    Expire = current_time() + Timeout,
    Rnd = p1_rand:get_string(),
    ID = encode_id(Expire, Rnd),
    ets:insert(?MODULE, {{Expire, Rnd}, Proc, Ctx}),
    gen_server:cast(?MODULE, {restart_timer, Expire}),
    ejabberd_router:route(IQ#iq{id = ID}).

-spec dispatch(iq()) -> boolean().
dispatch(#iq{type = T, id = ID} = IQ) when T == error; T == result ->
    case decode_id(ID) of
	{ok, Expire, Rnd, Node} ->
	    ejabberd_cluster:send({?MODULE, Node}, {route, IQ, {Expire, Rnd}});
	error ->
	    false
    end;
dispatch(_) ->
    false.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    _ = ets:new(?MODULE, [named_table, ordered_set, public]),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    noreply(State).

handle_cast({restart_timer, Expire}, State) ->
    State1 = State#state{expire = min(Expire, State#state.expire)},
    noreply(State1);
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    noreply(State).

handle_info({route, IQ, Key}, State) ->
    case ets:lookup(?MODULE, Key) of
	[{_, Proc, Ctx}] ->
	    callback(Proc, IQ, Ctx),
	    ets:delete(?MODULE, Key);
	[] ->
	    ok
    end,
    noreply(State);
handle_info(timeout, State) ->
    Expire = clean(ets:first(?MODULE)),
    noreply(State#state{expire = Expire});
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    noreply(State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec current_time() -> non_neg_integer().
current_time() ->
    erlang:system_time(millisecond).

-spec clean({non_neg_integer(), binary()} | '$end_of_table')
	   -> non_neg_integer() | infinity.
clean({Expire, _} = Key) ->
    case current_time() of
	Time when Time >= Expire ->
	    case ets:lookup(?MODULE, Key) of
		[{_, Proc, Ctx}] ->
		    callback(Proc, timeout, Ctx),
		    ets:delete(?MODULE, Key);
		[] ->
		    ok
	    end,
	    clean(ets:next(?MODULE, Key));
	_ ->
	    Expire
    end;
clean('$end_of_table') ->
    infinity.

-spec noreply(state()) -> {noreply, state()} | {noreply, state(), non_neg_integer()}.
noreply(#state{expire = Expire} = State) ->
    case Expire of
	infinity ->
	    {noreply, State};
	_ ->
	    Timeout = max(0, Expire - current_time()),
	    {noreply, State, Timeout}
    end.

-spec encode_id(non_neg_integer(), binary()) -> binary().
encode_id(Expire, Rnd) ->
    ExpireBin = integer_to_binary(Expire),
    Node = ejabberd_cluster:node_id(),
    CheckSum = calc_checksum(<<ExpireBin/binary, Rnd/binary, Node/binary>>),
    <<"rr-", ExpireBin/binary, $-, Rnd/binary, $-, CheckSum/binary, $-, Node/binary>>.

-spec decode_id(binary()) -> {ok, non_neg_integer(), binary(), atom()} | error.
decode_id(<<"rr-", ID/binary>>) ->
    try
	[ExpireBin, Tail] = binary:split(ID, <<"-">>),
	[Rnd, Rest] = binary:split(Tail, <<"-">>),
	[CheckSum, NodeBin] = binary:split(Rest, <<"-">>),
	CheckSum = calc_checksum(<<ExpireBin/binary, Rnd/binary, NodeBin/binary>>),
	Node = ejabberd_cluster:get_node_by_id(NodeBin),
	Expire = binary_to_integer(ExpireBin),
	{ok, Expire, Rnd, Node}
    catch _:{badmatch, _} ->
	    error
    end;
decode_id(_) ->
    error.

-spec calc_checksum(binary()) -> binary().
calc_checksum(Data) ->
    Key = ejabberd_config:get_shared_key(),
    base64:encode(crypto:hash(sha, <<Data/binary, Key/binary>>)).

-spec callback(atom() | pid(), #iq{} | timeout, term()) -> any().
callback(undefined, IQRes, Fun) ->
    try Fun(IQRes)
    catch ?EX_RULE(Class, Reason, St) ->
	    StackTrace = ?EX_STACK(St),
	    ?ERROR_MSG("Failed to process iq response:~n~ts~n** ~ts",
		       [xmpp:pp(IQRes),
			misc:format_exception(2, Class, Reason, StackTrace)])
    end;
callback(Proc, IQRes, Ctx) ->
    try
        Proc ! {iq_reply, IQRes, Ctx}
    catch _:badarg ->
        ok
    end.
