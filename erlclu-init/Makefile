IMAGE_NAME := erlclu-init

all: build-image push-image

build-image:
	podman build -f Dockerfile --build-arg RELEASE_VSN=$(RELEASE_VSN) -t $(IMAGE_NAME)

push-image:
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(RELEASE_VSN)
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(BRANCH_NAME)
