-module(home_handler).

-export([init/2]).

init(Req0, Opts) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = io_lib:format("~p~n", [node()]),
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, Opts}.
