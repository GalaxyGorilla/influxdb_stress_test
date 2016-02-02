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

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    {ok, MetricsOptions} = application:get_env(influxdb_stress_test, metrics_options),
    ok = exometer:new([stress_test, counter], counter, []).
    ok = exometer_report:subscribe(exometer_influxdb, [stress_test, counter], [value], 1000, []).
    {ok, self()}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

