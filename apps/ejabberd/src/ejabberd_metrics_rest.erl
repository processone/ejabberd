%%%%===================================================================
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Module providing REST interface for folsom metrics
%%% @end
%%%===================================================================

-module(ejabberd_metrics_rest).

-type command() :: available_metrics | sum_metrics |
    sum_metric | host_metrics | host_metric.
-record(state, {cmd :: command()}).

%% cowboy_rest callbacks
-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2]).

%% response callbacks
-export([response/2]).

%%--------------------------------------------------------------------
%% cowboy_rest callbacks
%%--------------------------------------------------------------------
-spec init({atom(), http}, cowboy_req:req(), any()) ->
    {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy_req:req(), [command()]) ->
    {ok, cowboy_req:req(), #state{}}.
rest_init(Req, [Command]) ->
    {ok, Req, #state{cmd = Command}}.

-spec allowed_methods(cowboy_req:req(), #state{}) ->
    {[binary()], cowboy_req:req(), #state{}}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), #state{}) ->
    {[{{binary(), binary(), []}, atom()}], cowboy_req:req(), #state{}}.
content_types_provided(Req, State) ->
    TypesProvided = [{{<<"application">>, <<"json">>, []}, response}],
    {TypesProvided, Req, State}.

%%--------------------------------------------------------------------
%% response callbacks
%%--------------------------------------------------------------------
-spec response(cowboy_req:req(), #state{}) ->
    {binary(), cowboy_req:req(), #state{}} | {halt, cowboy_req:req(), #state{}}.
response(Req, #state{cmd=available_metrics}=State) ->
    {Hosts, Metrics} = get_available_hosts_metrics(),
    Response = response_json([{hosts, Hosts}, {metrics, Metrics}]),
    {Response, Req, State};
response(Req, #state{cmd=sum_metrics}=State) ->
    Metrics = get_sum_metrics(),
    Response = response_json([{metrics, Metrics}]),
    {Response, Req, State};
response(Req, #state{cmd=sum_metric}=State) ->
    {Metric, NewReq} = cowboy_req:binding(metric, Req),
    try
        MetricAtom = binary_to_existing_atom(Metric, utf8),
        Value = get_sum_metric(MetricAtom),
        Response = response_json([{metric, Value}]),
        {Response, NewReq, State}
    catch _:_ ->
        {ok, NewReq2} = cowboy_req:reply(404, NewReq),
        {halt, NewReq2, State}
    end;
response(Req, #state{cmd=host_metrics}=State) ->
    {Host, NewReq} = cowboy_req:binding(host, Req),
    case get_host_metrics(Host) of
        [] ->
            {ok, NewReq2} = cowboy_req:reply(404, NewReq),
            {halt, NewReq2, State};
        Metrics ->
            Response = response_json([{metrics, Metrics}]),
            {Response, NewReq, State}
    end;
response(Req, #state{cmd=host_metric}=State) ->
    {Host, Req2} = cowboy_req:binding(host, Req),
    {Metric, NewReq} = cowboy_req:binding(metric, Req2),
    try
        MetricAtom = binary_to_existing_atom(Metric, utf8),
        Value = folsom_metrics:get_metric_value({Host, MetricAtom}),
        Response = response_json([{metric, Value}]),
        {Response, NewReq, State}
    catch _:_ ->
        {ok, NewReq2} = cowboy_req:reply(404, NewReq),
        {halt, NewReq2, State}
    end;
response(Req, State) ->
    {ok, NewReq} = cowboy_req:reply(404, Req),
    {halt, NewReq, State}.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
get_available_hosts() ->
    HostsSet = lists:foldl(fun({Host, _Metric}, Hosts) ->
                    ordsets:add_element(Host, Hosts)
            end, ordsets:new(), folsom_metrics:get_metrics()),
    ordsets:to_list(HostsSet).

get_available_metrics(Host) ->
    Metrics = lists:foldl(fun({CurrentHost, Metric}, Acc) ->
                    case CurrentHost of
                        Host ->
                            [Metric | Acc];
                        _ ->
                            Acc
                    end
            end, [], folsom_metrics:get_metrics()),
    lists:reverse(Metrics).

get_available_hosts_metrics() ->
    Hosts = get_available_hosts(),
    Metrics = get_available_metrics(hd(Hosts)),
    {Hosts, Metrics}.

get_sum_metrics() ->
    {Hosts, Metrics} = get_available_hosts_metrics(),
    Sum = lists:foldl(fun({_Host, Metric}=Name, Dict) ->
                    Value = folsom_metrics:get_metric_value(Name),
                    case orddict:is_key(Metric, Dict) of
                        false ->
                            orddict:store(Metric, Value, Dict);
                        true ->
                            OldValue = orddict:fetch(Metric, Dict),
                            NewMetric = update_sum_metric(OldValue, Value),
                            orddict:store(Metric, NewMetric, Dict)
                    end
            end, orddict:new(), [{H, M} || H <- Hosts, M <- Metrics]),
    orddict:to_list(Sum).

get_sum_metric(Metric) ->
    {Hosts, _Metrics} = get_available_hosts_metrics(),
    lists:foldl(fun(Host, Acc) ->
                Value = folsom_metrics:get_metric_value({Host, Metric}),
                update_sum_metric(Acc, Value)
        end, nil, Hosts).

update_sum_metric(nil, Value) ->
    Value;
update_sum_metric(OldValue, Value) when is_integer(Value) ->
    OldValue+Value;
update_sum_metric([{count,OldCount},{one,OldOne}],[{count,Count},{one,One}]) ->
    [{count, OldCount+Count}, {one, OldOne+One}];
update_sum_metric(OldValue, _Value) ->
    OldValue.

get_host_metrics(Host) ->
    Metrics = folsom_metrics:get_metrics_value(Host),
    [{Name, Value} || {{_Host, Name}, Value} <- Metrics].

response_json(Element) ->
    mochijson2:encode(fix_element(Element)).

fix_element({ElementName, [{_Key, _Val}|_Rest] = Proplist}) ->
    {ElementName, fix_element(Proplist)};
fix_element(Proplist) when is_list(Proplist) ->
    {struct, [fix_element(Element) || Element <- Proplist]};
fix_element(Other) ->
    Other.
