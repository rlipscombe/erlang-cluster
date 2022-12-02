#!/bin/sh

#set -x

expiry=$(date --date="${MAX_AGE_MINS} minutes ago" --utc +"%Y-%m-%dT%H:%M:%SZ")

AUTH_TOKEN="$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)"
NAMESPACE="$(cat /var/run/secrets/kubernetes.io/serviceaccount/namespace)"

certificate_requests_base_url="https://${KUBERNETES_SERVICE_HOST}:${KUBERNETES_SERVICE_PORT}/apis/cert-manager.io/v1/namespaces/${NAMESPACE}/certificaterequests"

certificate_requests=$(curl -s -X GET \
        --header "Accept: application/json" \
        --header "Authorization: Bearer ${AUTH_TOKEN}" \
        --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
        "${certificate_requests_base_url}")

#echo "$certificate_requests"

expired_requests=$(echo "$certificate_requests" | \
    jq --arg expiry "$expiry" -r '.items[] | select(.metadata.creationTimestamp < $expiry) | .metadata.name')

for x in $expired_requests; do
    curl -s -X DELETE \
        --header "Authorization: Bearer ${AUTH_TOKEN}" \
        --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
        "${certificate_requests_base_url}/$x" --output /dev/null
done
