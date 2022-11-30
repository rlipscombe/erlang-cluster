RELEASE_VERSION ?= 0.1.0
DOCKER_REGISTRY ?= docker.k3s.differentpla.net

all: build-image push-image

release:
	rebar3 as prod release

build-image:
	podman build -f Dockerfile -t erlclu

push-image:
	podman push erlclu $(DOCKER_REGISTRY)/erlclu:$(RELEASE_VERSION)
