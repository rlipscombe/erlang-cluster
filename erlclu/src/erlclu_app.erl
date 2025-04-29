-module(erlclu_app).
-behaviour(application).
-export([
    start/2,
    prep_stop/1,
    stop/1
]).

-include_lib("kernel/include/logger.hrl").
-define(APPLICATION, erlclu).

start(_StartType, _StartArgs) ->
    ?LOG_INFO("Starting up"),

    start_ssh_daemon(),
    start_http_listener(),
    start_prometheus_listener(),

    erlclu_sup:start_link().

start_ssh_daemon() ->
    Port = list_to_integer(os:getenv("SSH_PORT", "22")),
    SystemDir = os:getenv("SSH_SYSTEM_DIR"),
    UserDir = os:getenv("SSH_USER_DIR"),
    start_ssh_daemon(Port, SystemDir, UserDir).

start_ssh_daemon(Port, SystemDir, UserDir) when
    is_integer(Port), is_list(SystemDir), is_list(UserDir)
->
    {ok, _} = ssh:daemon(Port, [
        {system_dir, SystemDir},
        {user_dir, UserDir},
        {auth_methods, "publickey"}
    ]),
    ?LOG_INFO("SSH daemon listening on port ~B", [Port]);
start_ssh_daemon(_Port, _SystemDir, _UserDir) ->
    ?LOG_WARNING("Not starting SSH daemon").

start_http_listener() ->
    Port = list_to_integer(os:getenv("HTTP_PORT", "8080")),
    start_http_listener(Port).

start_http_listener(Port) when is_integer(Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", index_handler, []},
            {"/cert/client", cert_handler, [client]},
            {"/cert/server", cert_handler, [server]},
            {"/readyz", readyz_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        erlclu_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ?LOG_INFO("Cowboy listening on port ~B", [Port]),
    ok.

start_prometheus_listener() ->
    Port = list_to_integer(os:getenv("METRICS_PORT", "9153")),
    start_prometheus_listener(Port).

start_prometheus_listener(Port) when is_integer(Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/metrics/[:registry]", erlclu_metrics_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        prometheus_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ?LOG_INFO("Metrics on port ~B", [Port]),
    ok.

prep_stop(State) ->
    % Because we're using the same listener for /readyz as everything else, we can't simply stop the listener. If we do
    % that, then we've literally caused the problem we're trying to avoid.

    % Instead, we have /readyz return an error, so that the readinessProbe takes us out of the ingress/service.
    % To signal this state, we set an application env var:
    application:set_env(?APPLICATION, erlclu_listener_ready, false),

    % We need to continue responding until we're removed from the ingress. Because the probe is configured for every
    % 10 seconds (see the deployment yaml), we need to wait for around 20s before stopping.

    % K8s gives us 30s between SIGTERM and SIGKILL, so 20s is fine.

    % If we were dealing with relatively long-lived connections (we're not), we would also wait until they'd drained.
    % Because we're sharing the listener, this would need to be done in several steps:
    % 1. disable /readyz
    % 2. wait until the probe fails
    % 3. stop the listener
    % 4. wait until existing connections are drained
    % 5. stop
    ShutdownDelayMs = list_to_integer(os:getenv("SHUTDOWN_DELAY_MS", "20000")),
    ?LOG_INFO("Delaying shutdown for ~B ms", [ShutdownDelayMs]),
    timer:sleep(ShutdownDelayMs),
    State.

stop(_State) ->
    ?LOG_INFO("Stopped"),
    ok.
