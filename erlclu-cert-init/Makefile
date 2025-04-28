IMAGE_NAME := erlclu-cert-init
RELEASE_VSN ?= dev
BRANCH_NAME ?= $(shell git branch --show-current)
IMAGE_TAG := $(subst +,_,$(RELEASE_VSN))

all: build-image push-image

build-image:
	podman build -f Dockerfile --build-arg RELEASE_VSN=$(RELEASE_VSN) --platform linux/amd64 -t $(IMAGE_NAME)

push-image:
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(IMAGE_TAG)
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(BRANCH_NAME)
