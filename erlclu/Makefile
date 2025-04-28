IMAGE_NAME := erlclu
RELEASE_VSN ?= dev
BRANCH_NAME ?= $(subst /,_,$(shell git branch --show-current))
IMAGE_TAG := $(subst +,_,$(RELEASE_VSN))
PLATFORMS ?= linux/amd64,linux/arm64/v8

export RELEASE_VSN

all: build-image push-image

compile:
	rebar3 compile

shell:
	rebar3 shell --sname erlclu

get-deps:
	rebar3 get-deps

release:
	rebar3 as prod release

console:
	MY_POD_IP=$(shell hostname) RELEASE_COOKIE=cookie ./_build/prod/rel/erlclu/bin/erlclu console

build-image:
	podman manifest exists $(IMAGE_NAME) && podman manifest rm $(IMAGE_NAME) || true
	podman manifest create $(IMAGE_NAME)
	podman build -f Dockerfile --build-arg RELEASE_VSN=$(RELEASE_VSN) --platform $(PLATFORMS) --manifest $(IMAGE_NAME)

push-image:
	podman manifest push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(IMAGE_TAG)
	podman manifest push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(BRANCH_NAME)
