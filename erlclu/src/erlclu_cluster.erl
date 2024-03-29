-module(erlclu_cluster).
-export([start_link/0]).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
    init/1,
    handle_call/3,
    handle_info/2,
    handle_cast/2,
    handle_continue/2,
    terminate/2,
    code_change/3
]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    prometheus_gauge:new([
        {name, connected_node_count}, {labels, [node]}, {help, "Connected node count"}
    ]),
    {ok, undefined, {continue, undefined}}.

handle_continue(_, State) ->
    refresh(),
    timer:send_interval(5_000, refresh),
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info(refresh, State) ->
    refresh(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_OldVsn, State, _) ->
    {ok, State}.

refresh() ->
    ?LOG_DEBUG("Refreshing node list"),
    IPAddresses = inet_res:lookup("erlclu-headless.erlclu.svc.cluster.local", in, a),
    ?LOG_DEBUG("Found ~p", [IPAddresses]),
    Nodes =
        [
            erlang:list_to_atom(
                lists:flatten(["erlclu@", inet:ntoa(A)])
            )
         || A <- IPAddresses
        ],
    Status = [{Node, net_kernel:connect_node(Node)} || Node <- Nodes, Node =/= node()],
    ?LOG_DEBUG("Connection status: ~p", [Status]),
    prometheus_gauge:set(connected_node_count, [node()], length(nodes()) + 1),
    ok.
