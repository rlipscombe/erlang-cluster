RELEASE_VSN ?= 0.10.3
DOCKER_REGISTRY ?= docker.k3s.differentpla.net

export RELEASE_VSN DOCKER_REGISTRY

all:
	make -C erlclu
	make -C erlclu-init
	make -C erlclu-request-cleanup

release:
	make -C erlclu release
