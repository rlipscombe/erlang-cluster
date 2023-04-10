RELEASE_VSN ?= dev
BRANCH_NAME ?= $(shell git branch --show-current)

export RELEASE_VSN

all: build-image push-image

shell:
	rebar3 shell --sname erlclu

release:
	rebar3 as prod release

console:
	MY_POD_IP=$(shell hostname) RELEASE_COOKIE=cookie ./_build/prod/rel/erlclu/bin/erlclu console

build-image:
	podman build -f Dockerfile --build-arg RELEASE_VSN=$(RELEASE_VSN) -t erlclu

push-image:
	podman push erlclu $(DOCKER_REGISTRY)/erlclu:$(RELEASE_VSN)
	podman push erlclu $(DOCKER_REGISTRY)/erlclu:$(BRANCH_NAME)
