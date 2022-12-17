all: build-image push-image

release:
	rebar3 as prod release

build-image: ssh-host-key
	podman build -f Dockerfile -t erlclu

ssh-host-key: priv/ssh/system/ssh_host_rsa_key

priv/ssh/system/ssh_host_rsa_key:
	ssh-keygen -q -N "" -t rsa -f priv/ssh/system/ssh_host_rsa_key

push-image:
	podman push erlclu $(DOCKER_REGISTRY)/erlclu:$(RELEASE_VSN)
