#!/bin/sh

# This is taken from https://www.camptocamp.com/en/actualite/flexible-docker-entrypoints-scripts/, but I took the
# "docker" naming out and made it work with alpine (BusyBox) sh.
DIR=/entrypoint.d

if [ -d "$DIR" ]
then
    /bin/run-parts --exit-on-error "$DIR"
fi

exec "$@"
