# Certificates

## CA Key

We'll use an EC keypair. They're smaller than RSA keys for the same strength.

```
openssl ecparam -name prime256v1 -genkey -noout -out erlclu-ca.key
```

## CA Certificate

Note that this is just enough to get it working; I've not considered expiry, usages, whatever. Depending on your
security policies, you might want to keep a root CA in an HSM and use an intermediate CA.

```
openssl req -new -x509 -key erlclu-ca.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA" -out erlclu-ca.crt
```

## CA Secret

This is the secret used by _cert-manager_ for signing TLS distribution certificates. See `erlclu-init.sh`.

```
kubectl --namespace erlclu create secret tls erlclu-ca-key-pair \
    --cert=erlclu-ca.crt \
    --key=erlclu-ca.key
```

## Trusted CA Secret

To allow for rotating CA certificates in future, we need to trust multiple CA certificates, rather than only the one
used by _cert-manager_. We use a separate secret for that. For now, though, it's just a single certificate:

```
kubectl --namespace erlclu create secret generic erlclu-ca-certificates \
    --from-file=ca.crt=erlclu-ca.crt
```

## Rotating the CA certificate

Assuming that the key hasn't changed, create a new certificate as follows:

```sh
cert_timestamp="$(date +%FT%H-%M-%S)"
openssl req -new -x509 -key erlclu-ca.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA" -out "erlclu-ca-$cert_timestamp.crt"
```

If we update the _cert-manager_ keypair at this point, new or restarted pods will fail to join the cluster, because the
existing nodes don't trust the newly-issued certificates. So we have to update the list of trusted certificates first:

```sh
kubectl --namespace erlclu get secret erlclu-ca-certificates -o json | \
    jq -r '.data."ca.crt"' | base64 -d > erlclu-ca-existing.crt
cat erlclu-ca-*.crt > ca-certificates.crt
kubectl --namespace erlclu delete secret erlclu-ca-certificates
kubectl --namespace erlclu create secret generic erlclu-ca-certificates \
    --from-file=ca.crt=ca-certificates.crt
```

Note that the above results in unbounded growth of the CA list, because we don't remove expired certificates or
duplicates. I'll fix that later.

Now we can update the _cert-manager_ keypair as follows:

```sh
kubectl --namespace erlclu delete secret erlclu-ca-key-pair
kubectl --namespace erlclu create secret tls erlclu-ca-key-pair \
    --cert=erlclu-ca-$cert_timestamp.crt \
    --key=erlclu-ca.key
```
