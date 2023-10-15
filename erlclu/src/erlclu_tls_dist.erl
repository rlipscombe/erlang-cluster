-module(erlclu_tls_dist).
-export([get_certificate_files/0]).

get_certificate_files() ->
    {ok, [[OptFile]]} = init:get_argument(ssl_dist_optfile),
    {ok, [Opts]} = file:consult(OptFile),
    Client = proplists:get_value(client, Opts),
    MyCertFile = proplists:get_value(certfile, Client),
    CACertFile = proplists:get_value(cacertfile, Client),
    {MyCertFile, CACertFile}.
