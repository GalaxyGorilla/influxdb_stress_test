%%%-------------------------------------------------------------------
%% @doc metrics_emitter public API
%% @end
%%%-------------------------------------------------------------------

-module(influxdb_stress_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1
        ]).

-define(SUBSCRIBER_INTERVAL, 10000).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    {ok, MetricsOptions} = application:get_env(influxdb_stress_test, metrics_options),
    Reporters = exometer_report:list_reporters(),
    io:format("Using reporters: ~p~n", [Reporters]),
    setup_metrics(MetricsOptions),
    [setup_subscriber(Reporter, MetricsOptions) || {Reporter, _ } <- Reporters],
    {ok, self()}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
setup_metrics(MetricsOptions) ->
    FunctionN = proplists:get_value(function, MetricsOptions),
    HistogramN = proplists:get_value(histogram, MetricsOptions),
    CounterN = proplists:get_value(counter, MetricsOptions),
    [setup_metric_function(N) || N <- lists:seq(1, FunctionN)],
    [setup_metric_histogram(N) || N <- lists:seq(1, HistogramN)],
    [setup_metric_counter(N) || N <- lists:seq(1, CounterN)].

setup_subscriber(Reporter, MetricsOptions) ->
    FunctionN = proplists:get_value(function, MetricsOptions),
    HistogramN = proplists:get_value(histogram, MetricsOptions),
    CounterN = proplists:get_value(counter, MetricsOptions),
    [setup_subscribe_function(Reporter, N) || N <- lists:seq(1, FunctionN)],
    [setup_subscribe_histogram(Reporter, N) || N <- lists:seq(1, HistogramN)],
    [setup_subscribe_counter(Reporter, N) || N <- lists:seq(1, CounterN)].

setup_metric_function(Arg) ->
    ok = exometer:new([stress_test, function, Arg],
                      {function, erlang, memory, ['$dp'], value,
                       [total]}).

setup_metric_histogram(Arg) ->
    ok = exometer:new([stress_test, histogram, Arg],
                       histogram,
                       [{time_span, timer:seconds(60)}]).

setup_metric_counter(Arg) ->
    ok = exometer:new([stress_test, counter, Arg], counter, []).

setup_subscribe_function(Reporter, Arg) ->
    ok = exometer_report:subscribe(Reporter,
                                   [stress_test, function, Arg],
                                   [total], 
                                   ?SUBSCRIBER_INTERVAL,
                                   [{id, {from_name, 3}}]).

setup_subscribe_histogram(Reporter, Arg) ->
    ok = exometer_report:subscribe(Reporter,
                                   [stress_test, histogram, Arg],
                                   [mean], 
                                   ?SUBSCRIBER_INTERVAL,
                                   [{id, {from_name, 3}}]).

setup_subscribe_counter(Reporter, Arg) ->
    ok = exometer_report:subscribe(Reporter,
                                   [stress_test, counter, Arg],
                                   [value], 
                                   ?SUBSCRIBER_INTERVAL,
                                   [{id, {from_name, 3}}]).

