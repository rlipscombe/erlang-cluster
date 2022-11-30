# Erlang Cookie

```bash
ERLANG_COOKIE="$(env LC_CTYPE=C tr -dc 'A-Z' < /dev/random | head -c 20)"
kubectl --namespace erlclu create secret generic erlang-cookie --from-literal=cookie="$ERLANG_COOKIE"
```
