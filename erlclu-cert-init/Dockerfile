FROM --platform=$TARGETPLATFORM docker.io/alpine:3.21.3
ARG TARGETPLATFORM

# unused; suppress "not consumed" warning
ARG RELEASE_VSN

RUN apk add --no-cache openssl && \
    apk add --no-cache curl && \
    apk add --no-cache jq
WORKDIR /erlclu-cert-init
COPY erlclu-cert-init.sh erlclu-cert-init.sh

ENTRYPOINT ["/erlclu-cert-init/erlclu-cert-init.sh"]
