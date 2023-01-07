-module(home_handler).

-export([init/2]).

-define(APPLICATION, erlclu).

init(Req0, Opts) ->
    Nodes = lists:sort([node() | nodes()]),
    {ok, Vsn} = application:get_key(?APPLICATION, vsn),

    Priv = code:priv_dir(?APPLICATION),
    {ok, Template} =
        file:read_file(
            filename:join([Priv, "web", "index.html"])
        ),

    Headers = #{<<"content-type">> => <<"text/html">>},
    Body =
        bbmustache:render(
            Template,
            #{
                node => node(),
                nodes => Nodes,
                node_count => length(Nodes),
                application_vsn => Vsn,
                cookie => erlang:get_cookie(),
                uptime => uptime()
            },
            [{key_type, atom}]
        ),
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, Opts}.

uptime() ->
    UptimeNative = erlang:monotonic_time() - erlang:system_info(start_time),
    UptimeSecs = erlang:convert_time_unit(UptimeNative, native, seconds),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UptimeSecs),
    list_to_binary(
        io_lib:format("~Bd, ~Bh~2.10.0Bm~2.10.0Bs", [D, H, M, S])).
