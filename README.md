# erlang-cluster

Erlang nodes running in a cluster on the nodes in a Kubernetes cluster :)

This project demonstrates the following:

- Running Erlang nodes in a Kubernetes cluster.
- Automatic clustering between the Erlang nodes, using DNS discovery.
- Using TLS for Erlang distribution between the nodes.
- Automatic certificate provisioning using cert-manager.
- Using Erlang/OTP's SSH daemon for the remote console.

## Contents

- `erlclu`: the Erlang node.
- `erlclu-cert-init`: init container that provisions the certificates.
- `erlclu-request-cleanup`: cron job that cleans up completed/stale certificate
  requests.
- `k8s`: Kubernetes manifests.

## Deploying it

1. Update `RELEASE_VSN` in the top-level Makefile. Do this first, otherwise `make` will overwrite the current container
   image, which might break things if a pod gets deployed.
2. If you want ArgoCD to track this branch: `argocd app set erlang-cluster --revision BRANCH`
3. Hack hack hack.
4. Run `make` at the top-level. This will build and push the container images.
5. Update the image tags in the `k8s/` directory.
6. Either:
   - `kubectl apply -k k8s/`
   - `argocd app sync erlang-cluster`
