# Container image for eturnal STUN/TURN Server

[eturnal](https://eturnal.net/) container images are available for multiple 
architectures as `ghcr.io/processone/eturnal` from [GitHub Packages](https://github.com/processone/eturnal/pkgs/container/eturnal). 
The images are based on [Alpine Linux](https://alpinelinux.org).

## Tags

`XX.YY.ZZ` represents the official eturnal release, a `-AA` suffix the image 
version of a particular release in case of any bug fix etc. of the image.

| Tags  | Description  | Additional notes  |
| ------------ | ------------ | ------------ |
| `edge`  | Built from `master` branch, see [changelog](https://github.com/processone/eturnal/blob/master/CHANGELOG.md)  | For testing purposes. Only built for x64 and arm64. |
| `1.10.1`, `latest`  | [Release changelog](https://github.com/processone/eturnal/releases/tag/1.10.1)  |   |

Images are scanned daily by [Trivy](https://www.aquasec.com/products/trivy) and,
if necessary, the `latest` release will be rebuilt and updated.

## Usage with [Docker](https://www.docker.com) or [Podman](https://podman.io/)

> _Note:_ the below commands can be run with podman as well, just use podman as 
> an equivalent: **alias docker=podman**

To pull the image:

    docker pull ghcr.io/processone/eturnal:latest

This will run a container named `eturnal` with the default, unprivileged user 
`eturnal` (`uid=9000`) in `foreground` mode with default ports published, if 
started this way:

```shell
docker run -d --rm \
    --name eturnal \
    -p 3478:3478 \
    -p 3478:3478/udp \
    -p 49152-65535:49152-65535/udp \
  ghcr.io/processone/eturnal:latest
```

**Recommended:** The container can also run in a less privileged mode:

```shell
docker run -d --rm \
    --name eturnal \
    --read-only \
    --cap-drop=ALL \
    --cap-add=NET_BIND_SERVICE \
    --security-opt no-new-privileges \
    -p 3478:3478 \
    -p 3478:3478/udp \
    -p 49152-65535:49152-65535/udp \
  ghcr.io/processone/eturnal:latest
```

**Only relevant for Docker, not Podman**: Since Docker 
[does not perform well with large port ranges](https://github.com/instrumentisto/coturn-docker-image/issues/3), 
consider decreasing the TURN default port range, e.g. through [environment variables](https://eturnal.net/documentation/#Environment_Variables):

```shell
docker run -d --rm \
    --name eturnal \
    --read-only \
    --cap-drop=ALL \
    --cap-add=NET_BIND_SERVICE \
    --security-opt no-new-privileges \
    -p 3478:3478 \
    -p 3478:3478/udp \
    -p 50000-50500:50000-50500/udp \
    -e ETURNAL_RELAY_MIN_PORT=50000 \
    -e ETURNAL_RELAY_MAX_PORT=50500 \
  ghcr.io/processone/eturnal:latest
```

Or use the [host network](https://docs.docker.com/network/host/) by adding 
`--network=host` to the command line:

```shell
docker run -d --rm \
    --name eturnal \
    --read-only \
    --cap-drop=ALL \
    --cap-add=NET_BIND_SERVICE \
    --security-opt no-new-privileges \
    --network=host \
  ghcr.io/processone/eturnal:latest
```

**Note:** The container is no longer isolated from the [host network](https://docs.docker.com/network/host/) 
when using this option.

Inspect the running container with:

    docker logs eturnal

To use the `eturnalctl` [command](https://eturnal.net/documentation/#Operation),
 e.g. just run:

    docker exec eturnal eturnalctl info

Stop the running container with:

    docker stop eturnal

## Configuration

Configuration is mainly done by a mounted `eturnal.yml` file (recommended), see 
the [example configuration file](https://github.com/processone/eturnal/blob/master/config/eturnal.yml). 
The file must be readable by the eturnal user (e.g. `chown 9000:9000` and 
`chmod 640`). **Mountpath**, e.g. with `docker run` add:

    -v /path/to/eturnal.yml:/etc/eturnal.yml:ro

eturnal may also be configured by specifying certain environment variables, see 
the [documentation](https://eturnal.net/documentation/#Environment_Variables). 
Here are some more hints [how to configure eturnal](https://eturnal.net/documentation/#Global_Configuration).

### General hints:

* For logs to be printed with the `docker logs` command, `log_dir:` should be 
set to `stdout` in `eturnal.yml`.
* The container attempts to autodetect the `relay_ipv4_address` and 
`relay_ipv6_address` using an external STUN service. 
  * This STUN service may be exchanged by defining a different external STUN 
  service with the `STUN_SERVICE` environment variable, which defaults to: 
  `STUN_SERVICE="stun.conversations.im 3478"`. Note: the stun client 
  **only supports UDP** queries. 
  * If that fails, consider defining the `relay_ipv4_address` (and 
  `relay_ipv6_address`) either within a mounted `eturnal.yml` file or with the 
  `ETURNAL_RELAY_IPV4_ADDR` (and `ETURNAL_RELAY_IPV6_ADDR`) environment variable 
  to enable the TURN service. Note: the **IPv6 address is optional**.
  * If the external STUN lookup is not desired, define the environment variable 
  `STUN_SERVICE=false` in the `docker run` command.
* If eturnal shall bind directly to privileged ports (<1024), then the 
`docker run` option `--security-opt no-new-privileges` must not be used, since 
the unprivileged eturnal user needs to escalate `CAP_NET_BIND_SERVICE`.

### Custom TLS certificates and dh-parameter file

To use eturnal's TLS listener with cutsom TLS certificates/dh-parameter files 
they must be mounted into the container and [referenced](https://eturnal.net/documentation/#tls_crt_file) 
in the `eturnal.yml` file. TLS certificates and the dh-parameter file shall be 
`.pem` files. They must be readable by the eturnal user/group and should not 
have world-readable access rights (e.g. `chown 9000:9000` and `chmod 440`). 
**Mountpath**, e.g. with `docker run` add:

    -v /path/to/tls-files:/opt/eturnal/tls:ro

### Rootless environments:

* If eturnal runs in rootless environments, e.g. `podman rootless`, then file
  permissions and editing files must be performed within the same user namespace
  as the container runs in. More information [here](https://www.tutorialworks.com/podman-rootless-volumes/).
* The "magic" command in e.g. `podman rootless` is called [podman unshare](https://docs.podman.io/en/latest/markdown/podman-unshare.1.html).
* To change e.g. the file permissions for `eturnal.yml` use:

```
podman unshare chown 9000:9000 /path/to/eturnal.yml
podman unshare chmod 640 /path/to/eturnal.yml
```

## Examples for Docker Compose and Kubernetes

This repository also contains configuration examples for:

* [Docker Compose](https://github.com/processone/eturnal/tree/master/docker-k8s/examples/docker-compose)
* [Kubernetes (Kustomize)](https://github.com/processone/eturnal/tree/master/docker-k8s/examples/kubernetes-kustomize)

