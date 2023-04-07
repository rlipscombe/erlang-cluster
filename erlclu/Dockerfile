FROM docker.io/erlang:25.3-alpine AS build

# We need git for rebar3 get-deps.
RUN apk add --no-cache git

WORKDIR /build

# Fetch deps into a separate layer; should improve caching.
COPY rebar.config rebar.config
COPY rebar.lock rebar.lock

RUN rebar3 get-deps
RUN rebar3 compile --deps_only

# Copy the rest and compile it
COPY . .

# We need RELEASE_VSN for rebar.config.script, etc.
ARG RELEASE_VSN
RUN rebar3 as prod release

####
FROM docker.io/alpine:3.17.3

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Use 'tini' to avoid Erlang running as PID 0.
RUN apk add --no-cache tini

COPY --from=build /build/_build/prod/rel/erlclu /erlclu

RUN addgroup -g 10000 erlclu && adduser -u 10000 -D -G erlclu -h /erlclu erlclu
RUN chown -R erlclu.erlclu /erlclu/releases/${RELEASE_VSN}
USER erlclu
ENV HOME /erlclu

EXPOSE 8080 9153

ENTRYPOINT ["/sbin/tini", "--"]
CMD ["/erlclu/bin/erlclu", "foreground"]
