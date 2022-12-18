## Erlang Cookie

```bash
ERLANG_COOKIE="$(env LC_CTYPE=C tr -dc 'A-Z' < /dev/random | head -c 20)"
kubectl --namespace erlclu create secret generic erlang-cookie --from-literal=cookie="$ERLANG_COOKIE"
```

## CA Key

Note that this is just enough to get it working; I've not considered expiry, usages, whatever. Depending on your
security policies, you might want to keep a root CA in an HSM and use an intermediate CA.

```
openssl ecparam -name prime256v1 -genkey -noout -out erlclu-ca.key
openssl req -new -x509 -key erlclu-ca.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA" -out erlclu-ca.crt
```

## CA Secret

```
kubectl --namespace erlclu create secret tls erlclu-ca-key-pair \
    --cert=erlclu-ca.crt \
    --key=erlclu-ca.key
```

## SSH host key

```
ssh-keygen -q -N "" -t rsa -f ssh_host_rsa_key
kubectl --namespace erlclu create secret generic ssh-host-key \
        --from-file=ssh_host_rsa_key=ssh_host_rsa_key \
        --from-file=ssh_host_rsa_key.pub=ssh_host_rsa_key.pub
```
