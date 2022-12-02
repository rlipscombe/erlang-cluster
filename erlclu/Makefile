RELEASE_VSN ?= 0.1.0
DOCKER_REGISTRY ?= docker.k3s.differentpla.net

export RELEASE_VSN DOCKER_REGISTRY

all: build-image push-image
	make -C erlclu-init
	make -C erlclu-request-cleanup

release:
	rebar3 as prod release

build-image:
	podman build -f Dockerfile -t erlclu

push-image:
	podman push erlclu $(DOCKER_REGISTRY)/erlclu:$(RELEASE_VSN)
