# Container image for eturnal STUN/TURN Server

[eturnal](https://eturnal.net/) container images are available for multiple
architectures as `ghcr.io/processone/eturnal` from [GitHub Packages](https://github.com/processone/eturnal/pkgs/container/eturnal)
or [DockerHub](https://hub.docker.com/r/eturnal/eturnal). The images are based
on [Alpine Linux](https://alpinelinux.org).

## Tags and variants

`XX.YY.ZZ` represents the official eturnal release, a `-AA` suffix the image
version of a particular release in case of any bug fix etc. of the image.

| Tags  | Description  | Additional notes  |
| ------------ | ------------ | ------------ |
| `edge`  | Built from `master` branch, see [changelog](https://github.com/processone/eturnal/blob/master/CHANGELOG.md)  | For testing purposes. |
| `1.12.0`, `latest`  | [Release changelog](https://github.com/processone/eturnal/releases/tag/1.12.0)  |   |
| `1.12.0-acme`, `acme`  | As the standalone image, but including the [acme.sh](https://github.com/acmesh-official/acme.sh) | [Variant specific documentation](https://github.com/processone/eturnal/blob/master/doc/CONTAINER-ACME.md)  |

Images are scanned daily by [Trivy](https://www.aquasec.com/products/trivy) and,
if necessary, the `latest` release will be rebuilt and updated.

## Getting started

> _Note:_ the below commands can be run with podman as well, just use podman as
> an equivalent: **alias docker=podman**

To pull the image:

```shell
docker pull ghcr.io/processone/eturnal:latest
```

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
    --security-opt no-new-privileges \
    -p 3478:3478 \
    -p 3478:3478/udp \
    -p 49152-65535:49152-65535/udp \
  ghcr.io/processone/eturnal:latest
```

> _Note:_ Stating `--cap-add=NET_BIND_SERVICE` may be needed here depending
> on the container runtime, see e.g. [Docker](https://github.com/moby/moby/pull/41030)

**Only relevant for Docker, not Podman**: Since Docker
[does not perform well with large port ranges](https://github.com/instrumentisto/coturn-docker-image/issues/3),
consider decreasing the TURN default port range, e.g. through [environment variables](https://eturnal.net/doc/#Environment_Variables):

```shell
docker run -d --rm \
    --name eturnal \
    --read-only \
    --cap-drop=ALL \
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
    --security-opt no-new-privileges \
    --network=host \
  ghcr.io/processone/eturnal:latest
```

**Note:** The container is no longer isolated from the [host network](https://docs.docker.com/network/host/)
when using this option.

Inspect the running container with:

```shell
docker logs eturnal
```

To use the `eturnalctl` [command](https://eturnal.net/doc/#Operation), e.g. just
run:

```shell
docker exec eturnal eturnalctl info
```

Stop the running container with:

```shell
docker stop eturnal
```

## Configuration

Configuration is mainly done by a mounted `eturnal.yml` file (recommended), see
the [example configuration file](https://github.com/processone/eturnal/blob/master/config/eturnal.yml).
The file must be readable by the eturnal user (e.g. `chown 9000:9000` and
`chmod 640`). **Mountpath**, e.g. with `docker run` add:

```shell
-v /path/to/eturnal.yml:/etc/eturnal.yml:ro
```

eturnal may also be configured by specifying certain environment variables, see
the [documentation](https://eturnal.net/doc/#Environment_Variables). Here are
some more hints [how to configure eturnal](https://eturnal.net/doc/#Global_Configuration).

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
* If eturnal shall bind to privileged ports (<1024) directly, there are two ways
  to accomplish that:
  * The eturnal container has the capability `NET_BIND_SERVICE` included and the
  option `--security-opt no-new-privileges` is not set, since the unprivileged
  container user `eturnal` needs to escalate `NET_BIND_SERVICE`.
  * You enable binding to privileged ports [system-wide](https://github.com/containers/podman/blob/main/rootless.md#shortcomings-of-rootless-podman) through defining the lowest port:

        sysctl net.ipv4.ip_unprivileged_port_start=80

    This also works in [kubernetes](https://kubernetes.io/docs/tasks/administer-cluster/sysctl-cluster/#setting-sysctls-for-a-pod).

  Hint: Newer [Docker](https://github.com/moby/moby/pull/41030) versions set
  this option during install already.

### Custom TLS certificates and dh-parameter file

To use eturnal's TLS listener with cutsom TLS certificates/dh-parameter files
they must be mounted into the container and [referenced](https://eturnal.net/doc/#tls_crt_file)
in the `eturnal.yml` file. TLS certificates and the dh-parameter file shall be
`.pem` files. They must be readable by the eturnal user/group and should not
have world-readable access rights (e.g. `chown 9000:9000` and `chmod 440`).
**Mountpath**, e.g. with `docker run` add:

```shell
-v /path/to/tls-files:/opt/eturnal/tls:ro
```

### Rootless environments:

* If eturnal runs in rootless environments, e.g. `podman rootless`, then file
  permissions and editing files must be performed within the same user namespace
  as the container runs in. More information [here](https://www.tutorialworks.com/podman-rootless-volumes/).
* The "magic" command in e.g. `podman rootless` is called [podman unshare](https://docs.podman.io/en/latest/markdown/podman-unshare.1.html).
* To change e.g. the file permissions for `eturnal.yml` use:

```shell
podman unshare chown 9000:9000 /path/to/eturnal.yml
podman unshare chmod 640 /path/to/eturnal.yml
```

## Deployment examples

This repository also contains configuration examples for:

* [Docker Compose](https://github.com/processone/eturnal/tree/master/examples/docker-compose)
* [Kubernetes (Kustomize)](https://github.com/processone/eturnal/tree/master/examples/kubernetes-kustomize)

## Building yourself

Instructions can be found [here](https://eturnal.net/doc/container-build.html).
