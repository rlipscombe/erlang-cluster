## Erlang Cookie

```bash
ERLANG_COOKIE="$(env LC_CTYPE=C tr -dc 'A-Z' < /dev/random | head -c 20)"
kubectl --namespace erlclu create secret generic erlang-cookie --from-literal=cookie="$ERLANG_COOKIE"
```

## SSH host key

```
ssh-keygen -q -N "" -t rsa -f ssh_host_rsa_key
kubectl --namespace erlclu create secret generic ssh-host-key \
        --from-file=ssh_host_rsa_key=ssh_host_rsa_key \
        --from-file=ssh_host_rsa_key.pub=ssh_host_rsa_key.pub
```
