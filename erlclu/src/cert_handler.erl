-module(cert_handler).

-export([init/2]).

-define(APPLICATION, erlclu).

init(Req0, [Which] = Opts) ->
    {ok, [[DistOptFile]]} = init:get_argument(ssl_dist_optfile),
    {ok, [DistOpts]} = file:consult(DistOptFile),
    Cert = get_certificate(Which, DistOpts),

    Priv = code:priv_dir(?APPLICATION),
    {ok, Template} =
        file:read_file(
            filename:join([Priv, "web", "cert.html"])
        ),

    Headers = #{<<"content-type">> => <<"text/html">>},
    Body =
        bbmustache:render(
            Template,
            #{
                cert => io_lib:format("~p", [Cert])
            },
            [{key_type, atom}]
        ),
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, Opts}.

get_certificate(Which, DistOpts) when Which =:= client; Which =:= server ->
    Opts = proplists:get_value(Which, DistOpts),

    CertFile = proplists:get_value(certfile, Opts),
    [Cert] = erlclu_cert:read_certificate(CertFile),
    Der = public_key:der_encode('Certificate', Cert),
    OTPCert = public_key:pkix_decode_cert(Der, otp),
    OTPCert.
