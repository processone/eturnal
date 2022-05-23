# Docker image for eturnal STUN/TURN server

This is a multi-arch [eturnal](https://eturnal.net/) docker image, currently built for

* linux/amd64
* linux/386
* linux/arm64
* linux/arm/v7
* linux/arm/v6

and based on Alpine Linux.

[Docker Hub link](https://hub.docker.com/r/sando38/docker-eturnal)

## Usage

To pull the image, just refer to the Docker Hub repository:

`docker pull sando38/docker-eturnal`

The image will run in eturnal foreground mode, if started this way:

`docker run -d sando38/docker-eturnal:latest --name eturnal`

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
  sando38/docker-eturnal:latest
```

As an alternative since [docker performs badly with large port ranges](https://github.com/instrumentisto/coturn-docker-image/issues/3) with using the host network `--network=host`. Please note, that the docker container is not isolated from the host network anymore when using this option.

```
docker run -d \
  --name eturnal \
  --user 9000:9000 \
  -v /path/to/eturnal.yml:/opt/eturnal/etc/eturnal.yml \
  --network=host \
  --read-only \
  --security-opt no-new-privileges \
  --cap-drop=ALL \
  sando38/docker-eturnal:latest
```

Inspect the running container with

`docker logs < container name >`

**Note:** for logs to be printed with `docker logs` command, `log_dir:` in `eturnal.yml` should be set to `stdout`.

To use the `eturnalctl` [command](https://eturnal.net/documentation/#Operation), e.g. just run:

`docker exec < container name > eturnalctl info`

## Tags

`vXX.YY.ZZ` represents the official eturnal release.

| TAGS  | Description  | Architectures  |
| ------------ | ------------ | ------------ |
| latest  | Built from master branch, may be unstable  | linux/amd64,linux/386,linux/arm64,linux/arm/v7,linux/arm/v6  |
| v1.8.3-3  | [Changelog](https://github.com/processone/eturnal/releases/tag/1.8.3), fix an issue with readonly fs and tls certificates | linux/amd64,linux/386,linux/arm64,linux/arm/v7,linux/arm/v6  |
| v1.8.3-2  | [Changelog](https://github.com/processone/eturnal/releases/tag/1.8.3), image adjusted, so that not erlang.cookie must be mounted with readonly file systems | linux/amd64,linux/386,linux/arm64,linux/arm/v7,linux/arm/v6  |
| v1.8.3  | [Changelog](https://github.com/processone/eturnal/releases/tag/1.8.3) | linux/amd64,linux/386,linux/arm64,linux/arm/v7,linux/arm/v6  |

## Configuration

Configuration is mainly done by the mounted `eturnal.yml` file (recommended). Here is an example [eturnal.yml](https://github.com/processone/eturnal/blob/master/config/eturnal.yml) file. However, eturnal may also be configured with some environment variables (see [eturnal documentation](https://eturnal.net/documentation/#Environment_Variables)).

The configuration file is best mounted directly into the container:

**Mountpath:**
` -v /path/to/eturnal.yml:/opt/eturnal/etc/eturnal.yml`

Here are some more hints [how to configure eturnal](https://eturnal.net/documentation/#Global_Configuration).

## Volume mounts

Volumes may be mounted for the configuration file and tls certificates/ dh-parameter file. TLS certificates and dh-parameter file shall be `.pem` files.

```
volumes:
  - /path/to/eturnal.yml:/opt/eturnal/etc/eturnal.yml  # for (custom) configuration file
  - /path/to/cert-files:/opt/eturnal/tls               # for tls certicates
```

TLS certificates must be readable by eturnal user/ group `9000:9000` and should not have world readable access rights (`chmod 400`).