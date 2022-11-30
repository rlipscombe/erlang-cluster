FROM docker.io/erlang:25.1.2.0-alpine AS build

COPY / /build
WORKDIR /build

RUN rebar3 as prod release

#
FROM docker.io/alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

COPY --from=build /build/_build/prod/rel/erlclu /erlclu

EXPOSE 8080

ENTRYPOINT ["/erlclu/bin/erlclu", "foreground"]
