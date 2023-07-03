-module(readyz_handler).

-export([init/2]).
-define(APPLICATION, erlclu).

init(Req0, Opts) ->
    Req =
        case application:get_env(?APPLICATION, erlclu_listener_ready, true) of
            true -> cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Req0);
            false -> cowboy_req:reply(503, #{<<"content-type">> => <<"text/plain">>}, <<"Service Unavailable">>, Req0)
        end,
    {ok, Req, Opts}.
