#!/bin/sh

#set -x

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
keyUsage = digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth,clientAuth

[alt_names]
DNS = ${MY_POD_IP}
IP = ${MY_POD_IP}
EOF

# TODO: Don't bother writing it to a file?
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

# TODO: Eat the output unless the request fails.

cat << EOF | \
    curl -s -X POST \
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
        "namespace": "$NAMESPACE",
        "labels": {
            "app": "$APPLICATION_LABEL"
        }
    },
    "spec": {
        "request": "$encoded_csr",
        "usages": ["digital signature", "key encipherment", "server auth", "client auth"],
        "issuerRef": {
            "kind": "$ISSUER_KIND",
            "name": "$ISSUER_NAME"
        }
    }
}
EOF

# Give it a chance to complete before we poll it the first time:
sleep 1s

for _ in 1 2 3 4 5; do
    res=$(curl -s \
        --header "Accept: application/json" \
        --header "Authorization: Bearer ${AUTH_TOKEN}" \
        --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
        "${certificate_requests_base_url}/$request_name")
    ready_status=$(echo "$res" | jq -r '.status.conditions[] | select(.type == "Ready") | .status')
    if [ "$ready_status" = "True" ]; then break; fi

    sleep 5s
done

if [ "$ready_status" != "True" ]; then exit 1; fi

# Write the generated cert to a file. The trusted CA certs are managed elsewhere.
echo "$res" | jq -r '.status.certificate' | base64 -d > "$CERTS_DIR/$CERT_FILENAME.crt"

openssl x509 -in "$CERTS_DIR/$CERT_FILENAME.crt" -text -noout
