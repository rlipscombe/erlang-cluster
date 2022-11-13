%%%-------------------------------------------------------------------
%% @doc erlclu public API
%% @end
%%%-------------------------------------------------------------------

-module(erlclu_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlclu_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
