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

COPY --from=build /build/_build/prod/rel/erlclu /erlclu

RUN addgroup -g 10000 erlclu && adduser -u 10000 -D -G erlclu -h /erlclu erlclu
USER erlclu

EXPOSE 8080 9153

ENV HOME /erlclu
ENTRYPOINT ["/erlclu/bin/erlclu", "foreground"]
