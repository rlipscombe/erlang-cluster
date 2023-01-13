-module(erlclu_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
    ?LOG_INFO("Starting up"),

    start_ssh_daemon(),
    start_http_listener(),
    start_prometheus_listener(),

    ok = telemetry:attach(
        <<"cluster-telemetry">>,
        [cluster, connected_nodes],
        fun handle_telemetry_event/4,
        []
    ),

    erlclu_sup:start_link().

handle_telemetry_event(Event, Measurements, Metadata, Config) ->
    ?LOG_INFO(#{
        event => Event, measurements => Measurements, metadata => Metadata, config => Config
    }).

start_ssh_daemon() ->
    SystemDir = os:getenv("SSH_SYSTEM_DIR"),
    UserDir = os:getenv("SSH_USER_DIR"),
    start_ssh_daemon(SystemDir, UserDir).

start_ssh_daemon(SystemDir, UserDir) when is_list(SystemDir), is_list(UserDir) ->
    {ok, _} = ssh:daemon(10022, [
        {system_dir, SystemDir},
        {user_dir, UserDir},
        {auth_methods, "publickey"}
    ]),
    ?LOG_INFO("SSH daemon listening on port 10022");
start_ssh_daemon(_SystemDir, _UserDir) ->
    ?LOG_WARNING("Not starting SSH daemon").

start_http_listener() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", home_handler, []},
            {"/readyz", readyz_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        erlclu_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    ?LOG_INFO("Cowboy listening on port 8080"),
    ok.

start_prometheus_listener() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/metrics/[:registry]", erlclu_metrics_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        prometheus_listener,
        [{port, 9153}],
        #{env => #{dispatch => Dispatch}}
    ),
    ?LOG_INFO("Metrics on port 9153"),
    ok.

stop(_State) ->
    ok.
