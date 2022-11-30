# erlclu

An experiment in Erlang clustering. This is the node that's going to be clustered.

## Remote Console

```
kubectl --namespace erlclu exec -it deploy/erlclu -- /erlclu/bin/erlclu remote_console
```

## Connecting them

```
IPAddresses = inet_res:lookup("erlclu-headless.erlclu.svc.cluster.local", in, a).
Nodes = [erlang:list_to_atom(lists:flatten(["erlclu@", inet:ntoa(A)])) || A <- IPAddresses].
[net_kernel:connect_node(Node) || Node <- Nodes].
```
