-module(home_handler).

-export([init/2]).

-define(APPLICATION, erlclu).

init(Req0, Opts) ->
    Nodes = lists:sort([node() | nodes()]),
    {ok, Vsn} = application:get_key(?APPLICATION, vsn),

    {ok, [[DistOptFile]]} = init:get_argument(ssl_dist_optfile),
    {ok, [DistOpts]} = file:consult(DistOptFile),

    % TODO: Lotta duplication.
    ClientOpts = proplists:get_value(client, DistOpts),
 
    ClientCertFile = proplists:get_value(certfile, ClientOpts),
    [ClientCert] = erlclu_cert:read_certificate(ClientCertFile),
    ClientCertDetails = erlclu_cert:convert_certificate(ClientCert),

    ClientCaFile = proplists:get_value(cacertfile, ClientOpts),
    ClientCas = erlclu_cert:read_certificate(ClientCaFile),
    ClientCaDetails = [erlclu_cert:convert_certificate(Ca) || Ca <- ClientCas],

    ServerOpts = proplists:get_value(server, DistOpts),

    ServerCertFile = proplists:get_value(certfile, ServerOpts),
    [ServerCert] = erlclu_cert:read_certificate(ServerCertFile),
    ServerCertDetails = erlclu_cert:convert_certificate(ServerCert),

    ServerCaFile = proplists:get_value(cacertfile, ServerOpts),
    ServerCas = erlclu_cert:read_certificate(ServerCaFile),
    ServerCaDetails = [erlclu_cert:convert_certificate(Ca) || Ca <- ServerCas],

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
                uptime => uptime(),
                cookie => erlang:get_cookie(),
                client_cert => io_lib:format("~p", [ClientCertDetails]),
                client_ca => io_lib:format("~p", [ClientCaDetails]),
                server_cert => io_lib:format("~p", [ServerCertDetails]),
                server_ca => io_lib:format("~p", [ServerCaDetails])
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
        io_lib:format("~Bd, ~Bh~Bm~Bs", [D, H, M, S])
    ).
