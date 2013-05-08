%%%%===================================================================
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Module providing REST interface for folsom metrics
%%% @end
%%%===================================================================

-module(ejabberd_metrics_rest).
-behaviour(cowboy_rest).

-record(state, {cmd :: atom()}).

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
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, [Command]) ->
    {ok, Req, #state{cmd = Command}}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    TypesProvided = [{{<<"application">>, <<"json">>, []}, response}],
    {TypesProvided, Req, State}.

%%--------------------------------------------------------------------
%% response callbacks
%%--------------------------------------------------------------------
response(Req, #state{cmd=available_metrics}=State) ->
    {Hosts, Metrics} = get_available_hosts_metrics(),
    Response = response_json([{hosts, Hosts}, {metrics, Metrics}]),
    {Response, Req, State};
response(Req, #state{cmd=host_metrics}=State) ->
    {Host, NewReq} = cowboy_req:binding(host, Req),
    case get_host_metrics(Host) of
        [] ->
            {ok, NewReq} = cowboy_req:reply(404, Req),
            {halt, NewReq, State};
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
        {ok, NewReq2} = cowboy_req:reply(404, Req),
        {halt, NewReq2, State}
    end;
response(Req, State) ->
    {ok, NewReq} = cowboy_req:reply(404, Req),
    {halt, NewReq, State}.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
get_available_hosts_metrics() ->
    {HostsSet, MetricsSet} = lists:foldl(fun({Host, Metric}, {Hosts, Metrics}) ->
                    NewHosts = ordsets:add_element(Host, Hosts),
                    NewMetrics = ordsets:add_element(Metric, Metrics),
                    {NewHosts, NewMetrics};
                (Metric, {Hosts, Metrics}) ->
                    NewMetrics = ordsets:add_element(Metric, Metrics),
                    {Hosts, NewMetrics}
            end, {ordsets:new(), ordsets:new()}, folsom_metrics:get_metrics()),
    {ordsets:to_list(HostsSet), ordsets:to_list(MetricsSet)}.

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
