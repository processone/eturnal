# Docker Image for eturnal STUN/TURN Server

This is a multi-arch [eturnal](https://eturnal.net/) Docker image based on Alpine Linux and currently built for:

* linux/amd64
* linux/386
* linux/s390x
* linux/ppc64le
* linux/arm64
* linux/arm/v7
* linux/arm/v6

The image is available as `ghcr.io/processone/eturnal` from [GitHub Packages](https://github.com/processone/eturnal/pkgs/container/eturnal).

## Usage

To pull the image:

    docker pull ghcr.io/processone/eturnal

The image will run `eturnal` in `foreground mode`, if started this way:

    docker run -d ghcr.io/processone/eturnal

The image can also run in a less "privileged" mode:

```
docker run -d \
  --name eturnal \
  --user 9000:9000 \
  -v /path/to/eturnal.yml:/opt/eturnal/etc/eturnal.yml \
  -p 3478:3478/udp \
  -p 49152-65535:49152-65535/udp \
  --read-only \
  --security-opt no-new-privileges \
  --cap-drop=ALL \
  ghcr.io/processone/eturnal
```

As an alternative, since Docker [performs badly with large port ranges](https://github.com/instrumentisto/coturn-docker-image/issues/3), use the host network by adding `--network=host` to the command line:

```
docker run -d \
  --name eturnal \
  --user 9000:9000 \
  -v /path/to/eturnal.yml:/opt/eturnal/etc/eturnal.yml \
  --network=host \
  --read-only \
  --security-opt no-new-privileges \
  --cap-drop=ALL \
  ghcr.io/processone/eturnal
```

**Note:** The Docker container is no longer isolated from the host network when using this option.

Inspect the running container with:

    docker logs <container_name>

To use the `eturnalctl` [command](https://eturnal.net/documentation/#Operation), e.g. just run:

    docker exec <container_name> eturnalctl info

## Tags

`XX.YY.ZZ` represents the official eturnal release. `-AA` suffix for image version of the particular release in case of any bug fix etc. of the image.

Images are scanned daily by Trivy. The newest release (tag `latest` and respective release version) and images from `master` branch (tag `edge`) are continuously re-built every Sunday and pushed to the registries.

| Tags  | Description  | Additional notes  |
| ------------ | ------------ | ------------ |
| `edge`  | Built from `master` branch, see [Changelog](https://github.com/processone/eturnal/blob/master/CHANGELOG.md)  | May be unstable  |
| `1.8.4`, `latest`  | [Release changelog](https://github.com/processone/eturnal/releases/tag/1.8.4)  |   |


## Configuration

Configuration is mainly done by the mounted `eturnal.yml` file (recommended), see the [example configuration file](https://github.com/processone/eturnal/blob/master/config/eturnal.yml). However, eturnal may also be configured by specifying certain environment variables, see the [documentation](https://eturnal.net/documentation/#Environment_Variables).

The configuration file is best mounted directly into the container:

**Mountpath:**
` -v /path/to/eturnal.yml:/opt/eturnal/etc/eturnal.yml`

**Note:** For logs to be printed with the `docker logs` command, `log_dir:` should be set to `stdout` in `eturnal.yml`.

Here are some more hints [how to configure eturnal](https://eturnal.net/documentation/#Global_Configuration).

## Volume Mounts

Volumes may be mounted for the configuration file and TLS certificates/dh-parameter files. TLS certificates and the dh-parameter file shall be `.pem` files.

```yaml
volumes:
  - /path/to/eturnal.yml:/opt/eturnal/etc/eturnal.yml  # For (custom) configuration file.
  - /path/to/cert-files:/opt/eturnal/tls               # For TLS certificates.
```

TLS certificates must be readable by the eturnal user/group `9000:9000` and should not have world-readable access rights (`chmod 400`).

## Examples for Docker Compose and Kubernetes

This repository also contains configuration examples for:

* [Docker Compose](https://github.com/processone/eturnal/tree/master/docker-k8s/examples/docker-compose)
* [Kubernetes (Kustomize)](https://github.com/processone/eturnal/tree/master/docker-k8s/examples/kubernetes-kustomize)
