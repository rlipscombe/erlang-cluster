-module(erlclu_metrics_handler).
-export([
    init/2,
    content_types_provided/2,
    generate_response/2
]).

init(Req, _State) ->
    {cowboy_rest, Req, #{}}.

content_types_provided(Req, Context) ->
    {[{{<<"text">>, <<"plain">>, '*'}, generate_response}], Req, Context}.

generate_response(Req, Context) ->
    Method = cowboy_req:method(Req),
    Response = gen_response(Method, Req),
    {stop, Response, Context}.

gen_response(<<"GET">>, Req) ->
    Registry0 = cowboy_req:binding(registry, Req, <<"default">>),
    case prometheus_registry:exists(Registry0) of
        false ->
            cowboy_req:reply(404, #{}, <<"Unknown Registry">>, Req);
        Registry ->
            Body = prometheus_text_format:format(Registry),
            cowboy_req:reply(200, #{}, Body, Req)
    end;
gen_response(_, Req) ->
    Req.
