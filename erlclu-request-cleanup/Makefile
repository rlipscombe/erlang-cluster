all: build-image push-image

build-image:
	podman build -f Dockerfile -t erlclu-request-cleanup

push-image:
	podman push erlclu-request-cleanup $(DOCKER_REGISTRY)/erlclu-request-cleanup:$(RELEASE_VSN)
	podman push erlclu-request-cleanup $(DOCKER_REGISTRY)/erlclu-request-cleanup:$(BRANCH_NAME)
