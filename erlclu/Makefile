all: build-image push-image

shell:
	rebar3 shell --sname erlclu

release:
	rebar3 as prod release

build-image:
	podman build -f Dockerfile --build-arg RELEASE_VSN=$(RELEASE_VSN) -t erlclu

push-image:
	podman push erlclu $(DOCKER_REGISTRY)/erlclu:$(RELEASE_VSN)
