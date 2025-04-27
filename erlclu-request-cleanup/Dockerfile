FROM --platform=$TARGETPLATFORM docker.io/alpine:3.21.3
ARG TARGETPLATFORM

# unused; suppress "not consumed" warning
ARG RELEASE_VSN

# We need coreutils for a version of 'date' that can do --date '15 minutes ago'.
# We need curl and jq to access the Kubernetes API and to parse the responses.
RUN apk add --no-cache coreutils && \
    apk add --no-cache curl && \
    apk add --no-cache jq

WORKDIR /erlclu-request-cleanup
COPY erlclu-request-cleanup.sh erlclu-request-cleanup.sh

ENTRYPOINT ["/erlclu-request-cleanup/erlclu-request-cleanup.sh"]
