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
