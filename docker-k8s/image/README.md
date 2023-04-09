# Dockerfile and build instructions

Please have a look through the following information to build eturnal images.

## Dockerfile multi-stage diagram

The `Dockerfile` is a multi-stage Dockerfile. The nodes in the flowchart below
illustrate the different stages. Thick links between the nodes describe the
default build process defined by the build arguments in the `Dockerfile`.

```mermaid
 flowchart LR
    subgraph build & install from source
    A(local) == SOURCE='local' ==> D;
    B(git) -. SOURCE='git'<br/>VERSION='x.x.x' .-> D;
    C(web) -. SOURCE='web'<br/>VERSION='x.x.x' .-> D;
    end
    subgraph install from binary tarball
    K[(output from:<br/>scripts/make-binaries)] -. eturnal-*musl*.tar.gz .-> E;
    end
    D(build) == METHOD='build' ==> F;
    E(package) -. METHOD='package' .-> F;
    subgraph prepare runtime image
    G(runtime-package) == METHOD='build' ==> H(runtime-build) ==> I;
    G(runtime-package) -. METHOD='package' .-> I;
    end
    F(eturnal) ==> J(prod);
    I(runtime) ==> J(prod);
```

## Build container images

From the root of the repository, to build with `local` source files the
`docker buildx build` or `podman build` command is:

```shell
docker buildx build --load \
    -f docker-k8s/image/Dockerfile \
    -t myname/eturnal:mytag \
    --build-arg METHOD='build' \
    --build-arg SOURCE='local' \
    .
```

Omitting `--build-arg METHOD='build'` and `--build-arg SOURCE='local'` in the
example above would lead to the same result, because they are the default.

Instead, if the source files should be downloaded from github and a specific
version should be built (`VERSION=master` is default), the build command would
be:

```shell
docker buildx build --load \
    -f docker-k8s/image/Dockerfile \
    -t myname/eturnal:mytag \
    --build-arg METHOD='build' \
    --build-arg SOURCE='git' \
    --build-arg REPOSITORY='https://github.com/processone/eturnal' \
    --build-arg VERSION='1.10.1' \
    .
```

When building with `SOURCE='web'` you need to define an eturnal release version,
e.g. `VERSION='1.10.1'`. Otherwise the build will fail. Please have a look at
the [official archive](https://eturnal.net/download/).

```shell
docker buildx build --load \
    -f docker-k8s/image/Dockerfile \
    -t myname/eturnal:mytag \
    --build-arg METHOD='build' \
    --build-arg SOURCE='web' \
    --build-arg VERSION='1.10.1' \
    .
```

Building with `METHOD='package'` requires eturnal binary tarballs built with the
[make-binaries](../../scripts/make-binaries) script from this repository. The
respective targets must be `x86_64-linux-musl` or `aarch64-linux-musl`. This
depends of course on the image variant you want to build. The tarballs must be
located in the root of the repository.

```shell
docker buildx build --load \
    -f docker-k8s/image/Dockerfile \
    -t myname/eturnal:mytag \
    --build-arg METHOD='package' \
    .
```

## Building eturnal tarballs for musl-libc with Docker or Podman

You can use the `Dockerfile.ctng` in this directory to build eturnal tarballs
with the `make-binaries` script.

From the root of the eturnal repository, do:

```shell
docker build \
    -f docker-k8s/image/Dockerfile.ctng \
    -t localhost/myname/ctng:eturnal \
    .
```

Building tarballs for `musl-libc` with the Debian-based `Dockerfile.ctng` image
requires a bootstrapped erlang based on the docker host machine's architecture,
hence, `$(uname -m)-linux-gnu` in the targets below.

```shell
targets="$(uname -m)-linux-gnu $(uname -m)-linux-musl"
sed -i "s|targets='.*'|targets='$targets'|" $PWD/scripts/make-binaries
docker run --rm -d  \
    --name ctng-eturnal \
    --user $(id -u $(whoami)) \
    -v ctng:/ctng \
    -v $PWD:/eturnal \
    localhost/myname/ctng:eturnal
docker logs -f ctng-eturnal
```

Be aware that building the toolchains takes a long time in the first run. The
toolchain is stored persistently in the docker volume `ctng`. New runs will be
significantly faster.

Building e.g. with `Podman rootless` requires to have the correct permissions of
the eturnal repository:

    podman unshare chown -R $(id -u $(whoami)) $PWD
