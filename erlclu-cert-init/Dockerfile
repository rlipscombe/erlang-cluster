FROM docker.io/alpine:3.17.3

RUN apk add --no-cache openssl && \
    apk add --no-cache curl && \
    apk add --no-cache jq
WORKDIR /erlclu-cert-init
COPY erlclu-cert-init.sh erlclu-cert-init.sh

ENTRYPOINT ["/erlclu-cert-init/erlclu-cert-init.sh"]
