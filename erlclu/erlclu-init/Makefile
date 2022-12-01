all: build-init-image push-init-image

build-init-image:
	podman build -f Dockerfile.init -t erlclu-init

push-init-image:
	podman push erlclu-init $(DOCKER_REGISTRY)/erlclu-init:$(RELEASE_VSN)
