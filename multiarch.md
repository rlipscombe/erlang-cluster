
Enable multiarch building in podman with the following:

```sh
sudo apt install qemu-user-static
sudo podman run --rm --privileged docker.io/multiarch/qemu-user-static --reset -p yes
```
