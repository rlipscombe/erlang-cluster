-module(erlclu_dist).
-export([
    childspecs/0,
    listen/2,
    accept/1,
    accept_connection/5,
    setup/5,
    close/1,
    select/1,
    is_node_name/1
]).

-include_lib("kernel/include/logger.hrl").

-define(MOD, inet_tls_dist).

childspecs() ->
    {ok, [
        {ssl_dist_sup, {ssl_dist_sup, start_link, []}, permanent, infinity, supervisor, [
            ssl_dist_sup
        ]}
    ]}.

listen(Name, Host) ->
    ?LOG_INFO(#{func => ?FUNCTION_NAME, name => Name, host => Name}),
    ?MOD:?FUNCTION_NAME(Name, Host).

accept(Listen) ->
    ?LOG_INFO(#{func => ?FUNCTION_NAME, listen => Listen}),
    ?MOD:?FUNCTION_NAME(Listen).

accept_connection(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    ?LOG_INFO(#{
        func => ?FUNCTION_NAME,
        accept_pid => AcceptPid,
        dist_ctrl => DistCtrl,
        my_node => MyNode,
        allowed => Allowed,
        setup_time => SetupTime
    }),
    ?MOD:?FUNCTION_NAME(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime).

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ?LOG_INFO(#{
        func => ?FUNCTION_NAME,
        node => Node,
        type => Type,
        my_node => MyNode,
        long_or_short_names => LongOrShortNames,
        setup_time => SetupTime
    }),
    ?MOD:?FUNCTION_NAME(Node, Type, MyNode, LongOrShortNames, SetupTime).

close(Socket) ->
    ?LOG_INFO(#{func => ?FUNCTION_NAME, socket => Socket}),
    ?MOD:?FUNCTION_NAME(Socket).

is_node_name(Node) ->
    ?LOG_INFO(#{
        func => ?FUNCTION_NAME,
        node => Node
    }),
    ?MOD:?FUNCTION_NAME(Node).

select(Node) ->
    ?LOG_INFO(#{
        func => ?FUNCTION_NAME,
        node => Node
    }),
    ?MOD:?FUNCTION_NAME(Node).
