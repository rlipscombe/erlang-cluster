RELEASE_VSN ?= $(shell scripts/git-vsn)
BRANCH_NAME ?= $(subst /,_,$(shell git branch --show-current))
DOCKER_REGISTRY ?= docker.k3s.differentpla.net

export RELEASE_VSN BRANCH_NAME DOCKER_REGISTRY

all:
	make -C erlclu
	make -C erlclu-cert-init
	make -C erlclu-request-cleanup

release:
	make -C erlclu release

deploy:
	kubectl apply -k k8s/dev
	kubectl --namespace erlclu rollout restart deployment erlclu
