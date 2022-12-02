# erlclu

An experiment in Erlang clustering. This is the node that's going to be clustered.

## Remote Console

Because we're using TLS distribution, the default behaviour doesn't work; we'll need to use 'nodetool'.

```
kubectl --namespace erlclu exec -it deploy/erlclu -- env "USE_NODETOOL=1" /erlclu/bin/erlclu remote_console
```

...but note that it breaks if you've not set `verify_peer`, because it attempts
to parse stdout. That makes it _really_ hard to debug.

## Connecting to SSH daemon

...to use SSH, rather than remote console?

```
kubectl --namespace erlclu port-forward pods/erlclu-7d86f49786-qq79w 10022:10022
kubectl --namespace erlclu port-forward deployment/erlclu 10022:10022
```

```
ssh -p 10022 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost
```

## Debug container

```
kubectl --namespace erlclu debug -it erlclu-7d86f49786-trrx8 --image=busybox -- /bin/sh

kubectl --namespace erlclu debug --quiet -i erlclu-7d86f49786-trrx8 \
    --target=erlclu --image=nicolaka/netshoot -- \
        tcpdump -i eth0 -s 65535 -w - > dump.pcap
```

Do NOT include `-t` in `-it`, and DO include `--quiet`, otherwise various human-readable stuff gets written, which confuses wireshark.
