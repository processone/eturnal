# Dockerfile multi-stage diagram

The Dockerfile is a multi-stage Dockerfile. The nodes in the flowchart below
illustrate the different stages. Thick links between the nodes describe the
default build process defined by the build arguments in the Dockerfile.

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
    E(binary) -. METHOD='binary' .-> F;
    subgraph prepare runtime image
    G(runtime-binary) == METHOD='build' ==> H(runtime-build) ==> I;
    G(runtime-binary) -. METHOD='binary' .-> I;
    end
    F(eturnal) ==> J(prod);
    I(runtime) ==> J(prod);
```

From the root of the repository, a `docker buildx build` command simulating
the default settings would be:

```console
docker buildx build --load \
    -f docker-k8s/image/Dockerfile \
    -t myname/eturnal:mytag \
    --build-arg METHOD='build' \
    --build-arg SOURCE='local' \
    .
```

Instead, if the source files should be derived from github and a specific
version should be built (`VERSION=master` is default), the build command would
be:

```console
docker buildx build --load \
    -f docker-k8s/image/Dockerfile \
    -t myname/eturnal:mytag \
    --build-arg METHOD='build' \
    --build-arg SOURCE='git' \
    --build-arg VERSION='1.10.1' \
    .
```

When building with `SOURCE='web'` you need to define a release version of
eturnal. Otherwise the build will fail. Please have a look at the
[official archive](https://eturnal.net/download/).

Building with `METHOD='binary'` requires eturnal binary tarballs built with the
[make-binaries](../../scripts/make-binaries) script from this repository. The
respective targets must be `x86_64-linux-musl` or `aarch64-linux-musl`. This
depends of course on the image variant you want to build.
