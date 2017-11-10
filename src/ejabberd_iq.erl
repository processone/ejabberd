%%%-------------------------------------------------------------------
%%% @author xram <xram@debian.zinid.ru>
%%% @copyright (C) 2017, xram
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2017 by xram <xram@debian.zinid.ru>
%%%-------------------------------------------------------------------
-module(ejabberd_iq).

-behaviour(gen_server).

%% API
-export([start_link/0, route/4, dispatch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("xmpp.hrl").
-include("logger.hrl").

-record(state, {expire = infinity :: timeout()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

route(#iq{type = T} = IQ, Proc, Ctx, Timeout) when T == set; T == get ->
    Expire = current_time() + Timeout,
    Rnd = randoms:get_string(),
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
    ets:new(?MODULE, [named_table, ordered_set, public]),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    {stop, {unexpected_call, Request, From}, State}.

handle_cast({restart_timer, Expire}, State) ->
    State1 = State#state{expire = min(Expire, State#state.expire)},
    noreply(State1);
handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
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
    ?WARNING_MSG("unexpected info: ~p", [Info]),
    noreply(State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
current_time() ->
    p1_time_compat:system_time(milli_seconds).

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

noreply(#state{expire = Expire} = State) ->
    case Expire of
	infinity ->
	    {noreply, State};
	_ ->
	    Timeout = max(0, Expire - current_time()),
	    {noreply, State, Timeout}
    end.

encode_id(Expire, Rnd) ->
    ExpireBin = integer_to_binary(Expire),
    Node = atom_to_binary(node(), utf8),
    CheckSum = calc_checksum(<<ExpireBin/binary, Rnd/binary, Node/binary>>),
    <<"rr-", ExpireBin/binary, $-, Rnd/binary, $-, CheckSum/binary, $-, Node/binary>>.

decode_id(<<"rr-", ID/binary>>) ->
    try
	[ExpireBin, Tail] = binary:split(ID, <<"-">>),
	[Rnd, Rest] = binary:split(Tail, <<"-">>),
	[CheckSum, NodeBin] = binary:split(Rest, <<"-">>),
	CheckSum = calc_checksum(<<ExpireBin/binary, Rnd/binary, NodeBin/binary>>),
	Node = erlang:binary_to_existing_atom(NodeBin, utf8),
	Expire = binary_to_integer(ExpireBin),
	{ok, Expire, Rnd, Node}
    catch _:{badmatch, _} ->
	    error
    end;
decode_id(_) ->
    error.

calc_checksum(Data) ->
    Key = ejabberd_config:get_option(shared_key),
    base64:encode(crypto:hash(sha, <<Data/binary, Key/binary>>)).

callback(undefined, IQRes, Fun) ->
    Fun(IQRes);
callback(Proc, IQRes, Ctx) ->
    Proc ! {iq_reply, IQRes, Ctx}.
