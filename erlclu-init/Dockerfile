FROM docker.io/alpine:3.17.3

RUN apk add --no-cache openssl && \
    apk add --no-cache curl && \
    apk add --no-cache jq
WORKDIR /erlclu-init
COPY erlclu-init.sh erlclu-init.sh

ENTRYPOINT ["/erlclu-init/erlclu-init.sh"]
