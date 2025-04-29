-module(index_handler).

-export([init/2]).

-define(APPLICATION, erlclu).

init(Req0, Opts) ->
    Nodes = lists:sort([node() | nodes()]),
    {ok, Vsn} = application:get_key(?APPLICATION, vsn),

    {ok, [[DistOptFile]]} = init:get_argument(ssl_dist_optfile),
    {ok, [DistOpts]} = file:consult(DistOptFile),
    {ClientCertDetails, ClientCaDetails} = get_certificate_details(client, DistOpts),
    {ServerCertDetails, ServerCaDetails} = get_certificate_details(server, DistOpts),

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
                otp_release => list_to_binary(erlang:system_info(otp_release)),
                application_vsn => Vsn,
                uptime => uptime(),
                system_architecture => list_to_binary(erlang:system_info(system_architecture)),
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

get_certificate_details(Which, DistOpts) when Which =:= client; Which =:= server ->
    Opts = proplists:get_value(Which, DistOpts),

    CertFile = proplists:get_value(certfile, Opts),
    [Cert] = erlclu_cert:read_certificate(CertFile),
    CertDetails = erlclu_cert:convert_certificate(Cert),

    CaFile = proplists:get_value(cacertfile, Opts),
    Cas = erlclu_cert:read_certificate(CaFile),
    CaDetails = [erlclu_cert:convert_certificate(Ca) || Ca <- Cas],

    {CertDetails, CaDetails}.

uptime() ->
    UptimeNative = erlang:monotonic_time() - erlang:system_info(start_time),
    UptimeSecs = erlang:convert_time_unit(UptimeNative, native, seconds),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UptimeSecs),
    list_to_binary(
        io_lib:format("~Bd, ~Bh~Bm~Bs", [D, H, M, S])
    ).
