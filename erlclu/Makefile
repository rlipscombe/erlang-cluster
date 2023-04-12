RELEASE_VSN ?= dev
BRANCH_NAME ?= $(shell git branch --show-current)
IMAGE_NAME := erlclu

export RELEASE_VSN

all: build-image push-image

shell:
	rebar3 shell --sname erlclu

get-deps:
	rebar3 get-deps

release:
	rebar3 as prod release

console:
	MY_POD_IP=$(shell hostname) RELEASE_COOKIE=cookie ./_build/prod/rel/erlclu/bin/erlclu console

build-image:
	podman build -f Dockerfile --build-arg RELEASE_VSN=$(RELEASE_VSN) -t $(IMAGE_NAME)

push-image:
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(RELEASE_VSN)
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(BRANCH_NAME)
