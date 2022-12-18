FROM docker.io/erlang:25.1.2.0-alpine AS build

WORKDIR /build

# Fetch deps into a separate layer; should improve caching.
COPY rebar.config rebar.config
COPY rebar.lock rebar.lock
RUN rebar3 get-deps
RUN rebar3 compile --deps_only

# Copy the rest and compile it
COPY . .
RUN rebar3 as prod release

#
FROM docker.io/alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# BUG: Stop running as root

COPY --from=build /build/_build/prod/rel/erlclu /erlclu

ENV HOME /erlclu

EXPOSE 8080 9153

ENTRYPOINT ["/erlclu/bin/erlclu", "foreground"]
