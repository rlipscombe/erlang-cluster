#!/bin/sh

set -x

# Create a private key
openssl ecparam -name prime256v1 -genkey -noout -out "$CERTS_DIR/$CERT_FILENAME.key"
openssl ec -in "$CERTS_DIR/$CERT_FILENAME.key" -text -noout 2>/dev/null

# Create a certificate signing request.
cat <<EOF > "$CERTS_DIR/$CERT_FILENAME.cnf"
[req]
req_extensions = req_extensions
distinguished_name = req_distinguished_name

[req_distinguished_name]

[req_extensions]
subjectAltName = @alt_names
extendedKeyUsage = serverAuth,clientAuth

[alt_names]
DNS = ${MY_POD_IP}
EOF

# FUCK: cert-manager appears to strip the dirName out.
# TODO: Let's try having the init container generate the certificates w/o cert-manager.
# We'll need to put the CA key in the container, or in a secret.
# Balls.
# Or, going back to basics: let's try some mTLS between hosts, rather than k8s pods.
# That'll let us rule out some crap.
# BUT: I _did_. What's going on? Can I get more logging?

# TODO: Don't bother writing it to a file.
openssl req -new \
            -key "$CERTS_DIR/$CERT_FILENAME.key" \
            -subj "/CN=${MY_POD_IP}" \
            -out "$CERTS_DIR/$CERT_FILENAME.csr" \
            -config "$CERTS_DIR/$CERT_FILENAME.cnf"

openssl req -in "$CERTS_DIR/$CERT_FILENAME.csr" -text -noout

encoded_csr=$(base64 -w0 < "$CERTS_DIR/$CERT_FILENAME.csr")

AUTH_TOKEN="$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)"
NAMESPACE="$(cat /var/run/secrets/kubernetes.io/serviceaccount/namespace)"

certificate_requests_base_url="https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/apis/cert-manager.io/v1/namespaces/${NAMESPACE}/certificaterequests"
request_name="$(hostname -s)"

cat << EOF | \
    curl -q -X POST \
        --header "Content-Type: application/json" \
        --header "Authorization: Bearer ${AUTH_TOKEN}" \
        --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
        --data-binary @- \
        "${certificate_requests_base_url}"
{
    "apiVersion": "cert-manager.io/v1",
    "kind": "CertificateRequest",
    "metadata": {
        "name": "$request_name",
        "namespace": "$NAMESPACE"
    },
    "spec": {
        "request": "$encoded_csr",
        "issuerRef": {
            "kind": "$ISSUER_KIND",
            "name": "$ISSUER_NAME"
        }
    }
}
EOF

# TODO: Actually *poll* the CertificateRequest.
sleep 5s

res=$(curl -q \
    --header "Accept: application/json" \
    --header "Authorization: Bearer ${AUTH_TOKEN}" \
    --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
    "${certificate_requests_base_url}/$request_name")
echo "$res"

# Write the cert and the CA to files.
echo "$res" | jq -r '.status.ca' | base64 -d > "$CERTS_DIR/ca.crt"
echo "$res" | jq -r ".status.certificate" | base64 -d > "$CERTS_DIR/$CERT_FILENAME.crt"

openssl x509 -in "$CERTS_DIR/$CERT_FILENAME.crt" -text -noout
openssl x509 -in "$CERTS_DIR/ca.crt" -text -noout

# TODO: Delete the CertificateRequest object if it completed successfully.
