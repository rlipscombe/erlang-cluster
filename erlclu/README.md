# erlclu

An experiment in Erlang clustering. This is the node that's going to be clustered.

## Remote Console

Because we're using TLS distribution, the default behaviour doesn't work; we'll need to use 'nodetool'.

```
kubectl --namespace erlclu exec -it deploy/erlclu -- env "USE_NODETOOL=1" /erlclu/bin/erlclu remote_console
```

## Connecting to SSH daemon

...to use SSH, rather than remote console?

```
kubectl --namespace erlclu port-forward pods/erlclu-7d86f49786-qq79w 10022:10022
kubectl --namespace erlclu port-forward deployment/erlclu 10022:10022
```

```
ssh -p 10022 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost
```
