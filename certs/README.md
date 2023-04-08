# Certificates

## CA Key

We'll use an EC keypair. They're smaller than RSA keys for the same strength.

```sh
cert_timestamp="$(date +%FT%H-%M-%S)"
openssl ecparam -name prime256v1 -genkey -noout -out erlclu-ca-$cert_timestamp.key
```

## CA Certificate

Note that this is just enough to get it working; I've not considered expiry, usages, whatever. Depending on your
security policies, you might want to keep a root CA in an HSM and use an intermediate CA.

```sh
openssl req -new -x509 -key erlclu-ca.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA $cert_timestamp" -out "erlclu-ca-$cert_timestamp.crt"
```

## CA Secret

This is the secret used by _cert-manager_ for signing TLS distribution certificates. See `erlclu-init.sh`.

```sh
kubectl --namespace erlclu create secret tls erlclu-ca-key-pair \
    --cert=erlclu-ca-$cert_timestamp.crt \
    --key=erlclu-ca-$cert_timestamp.key
```

## Trusted CA Secret

To allow for rotating CA certificates in future, we need to trust multiple CA certificates, rather than only the one
used by _cert-manager_. We use a separate secret for that. Initially, though, it's just a single certificate:

```sh
kubectl --namespace erlclu create secret generic erlclu-ca-certificates \
    --from-file=ca.crt=erlclu-ca-$cert_timestamp.crt
```

## Rotating the CA certificate

Optionally create a new key as follows:

```sh
cert_timestamp="$(date +%FT%H-%M-%S)"
openssl ecparam -name prime256v1 -genkey -noout -out erlclu-ca-$cert_timestamp.key
```

Create a new certificate as follows:

```sh
cert_timestamp="$(date +%FT%H-%M-%S)"
openssl req -new -x509 -key erlclu-ca-$cert_timestamp.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA $cert_timestamp" -out "erlclu-ca-$cert_timestamp.crt"
```

If we update the _cert-manager_ keypair at this point, new or restarted pods will fail to join the cluster, because the
existing nodes don't trust the newly-issued certificates. So we have to update the list of trusted certificates first:

```sh
kubectl --namespace erlclu get secret erlclu-ca-certificates -o json | \
    jq -r '.data."ca.crt"' | base64 -d > erlclu-ca-existing.crt

./filter-ca-certs.escript <(cat erlclu-ca-*.crt) > ca-certificates.crt

kubectl --namespace erlclu delete secret erlclu-ca-certificates
kubectl --namespace erlclu create secret generic erlclu-ca-certificates \
    --from-file=ca.crt=ca-certificates.crt
```

Now we can update the _cert-manager_ keypair as follows:

```sh
kubectl --namespace erlclu delete secret erlclu-ca-key-pair
kubectl --namespace erlclu create secret tls erlclu-ca-key-pair \
    --cert=erlclu-ca-$cert_timestamp.crt \
    --key=erlclu-ca-$cert_timestamp.key
```
