-module(erlclu_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
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
    erlclu_sup:start_link().

stop(_State) ->
    ok.
