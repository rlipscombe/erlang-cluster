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

1. If you want ArgoCD to track this branch: `argocd app set erlang-cluster --revision BRANCH`
2. Hack hack hack.
3. Run `make` at the top-level. This will build and push the container images.
4. Update the image tags in the `k8s/` directory. Note that `+` in the semver is changed to `_` in the image tag. If
   you're working on a branch, you can use the branch name. Use `make RELEASE_VSN=0.15.1` (for example) if you don't
   want the suffix.
5. Either:
   - `kubectl apply -k k8s/` (note: `-k`, not `-f`; we use kustomize)
   - `argocd app sync erlang-cluster`
6. If you've made no changes to the deployment, but you want to roll out a new container image:
   - `kubectl --namespace erlclu rollout restart deployment erlclu`

## Or, in 'dev' mode

1. Hack hack hack.
2. `make RELEASE_VSN=dev all deploy`
