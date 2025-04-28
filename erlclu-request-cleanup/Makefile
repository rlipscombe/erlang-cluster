IMAGE_NAME := erlclu-request-cleanup
RELEASE_VSN ?= dev
BRANCH_NAME ?= $(subst /,_,$(shell git branch --show-current))
IMAGE_TAG := $(subst +,_,$(RELEASE_VSN))
PLATFORMS ?= linux/amd64,linux/arm64/v8

all: build-image push-image

build-image:
	podman manifest exists $(IMAGE_NAME) && podman manifest rm $(IMAGE_NAME) || true
	podman manifest create $(IMAGE_NAME)
	podman build -f Dockerfile --build-arg RELEASE_VSN=$(RELEASE_VSN) --platform $(PLATFORMS) --manifest $(IMAGE_NAME)

push-image:
	podman manifest push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(IMAGE_TAG)
	podman manifest push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(BRANCH_NAME)
