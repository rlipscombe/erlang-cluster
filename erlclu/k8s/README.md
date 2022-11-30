# Erlang Cookie

```bash
ERLANG_COOKIE="$(env LC_CTYPE=C tr -dc 'A-Z' < /dev/random | head -c 20)"
kubectl --namespace erlclu create secret generic erlang-cookie --from-literal=cookie="$ERLANG_COOKIE"
```

# TLS Distribution Certificates (for now)

We need a self-signed server certificate (not going to bother with a CA at this point):

```bash
./certs self-signed \
    --out-cert erlclu-dist-tls.crt --out-key erlclu-dist-tls.key \
    --template server \
    --subject "/CN=inet_tls_dist"
```
