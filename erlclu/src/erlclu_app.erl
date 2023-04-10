-module(erlclu_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

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
            {"/", home_handler, []},
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

stop(_State) ->
    ok.
