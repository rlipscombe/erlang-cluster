RELEASE_VSN ?= 0.11.0
BRANCH_NAME ?= $(shell git branch --show-current)
DOCKER_REGISTRY ?= docker.k3s.differentpla.net

export RELEASE_VSN BRANCH_NAME DOCKER_REGISTRY

all:
	make -C erlclu
	make -C erlclu-init
	make -C erlclu-request-cleanup

release:
	make -C erlclu release
