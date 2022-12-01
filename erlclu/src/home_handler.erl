-module(home_handler).

-export([init/2]).

-define(APPLICATION, erlclu).

init(Req0, Opts) ->
    Priv = code:priv_dir(?APPLICATION),
    {ok, Template} =
        file:read_file(
            filename:join([Priv, "index.html"])
        ),

    Headers = #{<<"content-type">> => <<"text/html">>},
    Body =
        bbmustache:render(
            Template,
            #{
                node => node(),
                nodes => lists:sort([node() | nodes()]),
                cookie => erlang:get_cookie()
            },
            [{key_type, atom}]
        ),
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, Opts}.
