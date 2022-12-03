# erlclu

An experiment in Erlang clustering. This is the node that's going to be clustered.

## Remote Console

Because we're using TLS distribution, the default behaviour doesn't work; we'll need to use 'nodetool'.

```
kubectl --namespace erlclu exec -it deploy/erlclu -- env "USE_NODETOOL=1" /erlclu/bin/erlclu remote_console
```

...but note that it breaks if you've not set `verify_peer`, because it attempts
to parse stdout. That makes it _really_ hard to debug.

It takes multiple seconds to connect, so maybe you want to use SSH instead... 

## Connecting to the SSH daemon

```
kubectl --namespace erlclu port-forward pods/erlclu-7d86f49786-qq79w 10022:10022
kubectl --namespace erlclu port-forward deployment/erlclu 10022:10022
```

```
ssh -p 10022 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost
```

## Debugging `inet_tls_dist`

With a remote console (e.g. via SSH, above), you can use the following to help diagnose what `inet_tls_dist` is doing.
It works because `inet_tls_dist` provides a handle do-nothing `trace` function that it calls with useful information.
You can hook it with `dbg`.

```
{ok, _} = dbg:start().
{ok, _} = dbg:tracer(process, {fun(Msg, _) -> io:format("~p\n", [Msg]) end, []}).
{ok, [{matched, _, 1}]} = dbg:tpl(inet_tls_dist, trace, '_', []).
{ok, [{matched, _, _}]} = dbg:p(all, c).
```

## Debug container

```
kubectl --namespace erlclu debug -it erlclu-7d86f49786-trrx8 --image=busybox -- /bin/sh

kubectl --namespace erlclu debug --quiet -i erlclu-7d86f49786-trrx8 \
    --target=erlclu --image=nicolaka/netshoot -- \
        tcpdump -i eth0 -s 65535 -w - > dump.pcap
```

Do NOT include `-t` in `-it`, and DO include `--quiet`, otherwise various human-readable stuff gets written, which
confuses Wireshark.

## Caveats

- The certificates expire. When this happens, it may break TLS distribution. The best way to fix this (other than
  restarting pods) is probably to implement a sidecar that issues a new certificate request before expiry.
- The SSH host key is stored in a secret and is the same for all pods. We might prefer to generate it in a/the init
  container. The `ssh` command above ignores the host key, which makes this currently moot.
- SSH daemon authentication accepts any password; we should use public key auth. This would require synchronising
  authorized_users somehow.
- The Erlang runtime doesn't support running as pid 1. Consider shared process namespace or using a lightweight init,
  such as `tini`.
- The containers run as 'root'; this needs addressing.
- Rather than use `imagePullPolicy: Always` (and delete the deployment every time), we should version the created images
  appropriately.
- Erlang distribution should probably be restricted with K8s NetworkPolicy.
