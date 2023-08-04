# Dockerfile and build instructions

Please have a look through the following information to build eturnal images.

## Dockerfile multi-stage diagram

The [Dockerfile](https://github.com/processone/eturnal/blob/master/Dockerfile)
is a multi-stage Dockerfile. The left side of the flowchart below shows the
build arguments. The nodes illustrate the different stages. Thick links between
the nodes describe the default build process defined by the build arguments in
the `Dockerfile`.

```mermaid
 flowchart LR
    L[METHOD='build'] == SOURCE='local' ==> A;
    L[METHOD='build'] -. SOURCE='git'<br/>VERSION='x.x.x' .-> B;
    L[METHOD='build'] -. SOURCE='web'<br/>VERSION='x.x.x' .-> C;
    L[METHOD='build'] ==> G;
    M[METHOD='package'] .-> K;
    M[METHOD='package'] .-> H;
    subgraph prepare eturnal
    subgraph build & install eturnal
    A(local) ==> D;
    B(git) .-> D;
    C(web) .-> D;
    end
    subgraph install eturnal package
    K[(output from:<br/>tools/make-binaries)] .-> E;
    end
    D(build) ==> F;
    E(package) ==> F;
    end
    subgraph prepare runtime base image
    G(base-build) ==> I;
    H(base-package) .-> I;
    end
    F(eturnal) ==> J(release);
    I(runtime) ==> J(release);
```

## Build container images

From the root of the repository, to build with `local` source files the
`docker buildx build` or `podman build` command is:

    docker buildx build --load -t myname/eturnal:mytag .

No need to set `--build-arg METHOD='build'` and `--build-arg SOURCE='local'` in
the example above as those are the default values (see diagram above).

Instead, if the source files should be downloaded from github and a specific
version should be built (`VERSION=master` is default), the build command would
be:

```shell
docker buildx build --load \
    -t myname/eturnal:mytag \
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
    -t myname/eturnal:mytag \
    --build-arg SOURCE='web' \
    --build-arg VERSION='1.10.1' \
    .
```

Building with `METHOD='package'` requires eturnal binary tarballs built with the
[make-binaries](https://github.com/processone/eturnal/blob/master/tools/make-binaries)
script from this repository. The respective targets must be `x86_64-linux-musl`
or `aarch64-linux-musl`. This depends of course on the image variant you want to
build. The tarballs must be located in the root of the repository.

```shell
docker buildx build --load \
    -t myname/eturnal:mytag \
    --build-arg METHOD='package' \
    .
```

## Building eturnal tarballs for musl-libc

You can use the provided [Dockerfile.ctng](https://github.com/processone/eturnal/blob/master/tools/Dockerfile.ctng)
to build eturnal tarballs with our `make-binaries` script.

From the root of the eturnal repository, do:

    docker build -f tools/Dockerfile.ctng -t localhost/myname/ctng:eturnal .

Building tarballs for `musl-libc` with the Debian-based `Dockerfile.ctng` image
requires a bootstrapped erlang based on the docker host machine's architecture,
hence, `$(uname -m)-linux-gnu` in the targets below.

```shell
targets="$(uname -m)-linux-gnu $(uname -m)-linux-musl"
docker run --rm -d \
    --name ctng-eturnal \
    --user $(id -u $(whoami)) \
    -v ctng:/ctng \
    -v $PWD:/eturnal \
    localhost/myname/ctng:eturnal \
    $targets
docker logs -f ctng-eturnal
```

Be aware that building the toolchains takes a long time in the first run. The
toolchain is stored persistently in the docker volume `ctng`. New runs will be
significantly faster.

Building e.g. with `Podman rootless` requires to have the correct permissions of
the eturnal repository:

    podman unshare chown -R $(id -u $(whoami)) $PWD
