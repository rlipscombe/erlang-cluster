-module(erlclu_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
    ?LOG_INFO("Starting up"),

    SystemDir = os:getenv("SSH_SYSTEM_DIR"),
    UserDir = os:getenv("SSH_USER_DIR"),
    {ok, _} = ssh:daemon(10022, [
        {system_dir, SystemDir},
        {user_dir, UserDir},
        {auth_methods, "publickey"}
    ]),
    ?LOG_INFO("SSH listening on port 10022"),

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
    erlclu_sup:start_link().

stop(_State) ->
    ok.
