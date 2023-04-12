RELEASE_VSN ?= 0.10.7
BRANCH_NAME ?= $(shell git branch --show-current)
DOCKER_REGISTRY ?= docker.k3s.differentpla.net

export RELEASE_VSN BRANCH_NAME DOCKER_REGISTRY

all:
	make -C erlclu
	make -C erlclu-cert-init
	make -C erlclu-request-cleanup

release:
	make -C erlclu release
