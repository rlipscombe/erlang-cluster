-module(erlclu_inspect).
-export([sslsocket_from_pid/1]).

% You've got a TLS socket in inet:i(), and you want to use
% ssl:connection_information with it.
% DO NOT DO THIS!
sslsocket_from_pid(Pid) ->
    {connection, State} = sys:get_state(Pid),
    StaticEnv = element(2, State),
    Port = element(11, StaticEnv),
    ProtocolSpecific = element(9, State),
    Pid2 = maps:get(sender, ProtocolSpecific),

    Transport = gen_tcp,
    ConnectionCb = tls_connection,
    {sslsocket, {Transport, Port, ConnectionCb, undefined}, [Pid, Pid2]}.
