# erlclu

An experiment in Erlang clustering. This is the node that's going to be clustered.

## Remote Console

Because we're using TLS distribution, the default behaviour doesn't work; we'll need to use 'nodetool'.

```
kubectl --namespace erlclu exec -it deploy/erlclu -- env "USE_NODETOOL=1" /erlclu/bin/erlclu remote_console
```
