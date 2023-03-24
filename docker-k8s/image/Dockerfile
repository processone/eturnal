#' Define default build variables
ARG ALPINE_VSN='3.17'
ARG UID='9000'
ARG USER='eturnal'
ARG HOME="opt/$USER"
ARG METHOD='build'
ARG SOURCE='local'
ARG BUILD_DIR="/$USER"
ARG REPOSITORY='https://github.com/processone/eturnal'
ARG VERSION='master'
ARG WEB_URL='https://eturnal.net'
ARG CI_MUSL_VSN='1.2.3'

################################################################################
#' METHOD='build' - install build dependencies
FROM alpine:${ALPINE_VSN} AS build-base
RUN apk -U add --no-cache \
        build-base \
        erlang-dev \
        git \
        openssl-dev \
        yaml-dev

################################################################################
#' METHOD='build', SOURCE='local' - source files from 'local' machine
FROM build-base AS local
ARG BUILD_DIR
COPY / $BUILD_DIR/

################################################################################
#' METHOD='build', SOURCE='git' - source files from 'git' repository
FROM build-base AS git
ARG REPOSITORY
ARG VERSION
ARG BUILD_DIR
WORKDIR $BUILD_DIR
RUN git clone $REPOSITORY . \
    && git checkout $VERSION

################################################################################
#' METHOD='build', SOURCE='web' - source files from 'web': https://eturnal.net/
FROM build-base AS web
ARG WEB_URL
ARG VERSION
ARG BUILD_DIR
RUN wget -O - $WEB_URL/download/eturnal-$VERSION.tar.gz | tar -xzf - \
    && mv eturnal-$VERSION $BUILD_DIR

################################################################################
#' METHOD='build' - build and install eturnal from source
FROM ${SOURCE} AS build
ARG BUILD_DIR
WORKDIR $BUILD_DIR
RUN if ! [ -f 'rebar3' ]; then wget https://s3.amazonaws.com/rebar3/rebar3; fi \
    && chmod +x rebar3

ARG REBAR_PROFILE=prod
RUN ./rebar3 as $REBAR_PROFILE tar
# run rebar3 test suites
ARG REBAR_CT=true
RUN ./rebar3 xref
RUN ./rebar3 eunit
RUN if [ "$REBAR_CT" = 'true' ]; then ./rebar3 ct; fi

ARG HOME
WORKDIR /rootfs/$HOME
RUN tar -xzf $BUILD_DIR/_build/$REBAR_PROFILE/rel/eturnal/eturnal-*.tar.gz

################################################################################
#' METHOD='binary' - install eturnal from binary tarball
FROM alpine:${ALPINE_VSN} AS binary
COPY eturnal-*-linux-musl-*.tar.gz /tmp/
WORKDIR /rootfs
ARG HOME
RUN home_root_dir=$(echo $HOME | sed 's|\(.*\)/.*|\1 |') \
    && mkdir -p $home_root_dir \
    && ARCH=$(uname -m | sed -e 's/x86_64/x64/;s/aarch64/arm64/') \
    && tar -xzf /tmp/eturnal-*-linux-musl-$ARCH.tar.gz -C $home_root_dir

################################################################################
#' Prepare eturnal for runtime
FROM ${METHOD} AS eturnal
RUN apk -U add --no-cache \
        libcap-utils

WORKDIR /rootfs
ARG HOME
RUN mkdir -p $HOME/log $HOME/run $HOME/tls etc usr/local/bin

ARG USER
ARG UID
RUN echo "$USER:x:$UID:$UID:$USER,,,:/$HOME:/sbin/nologin" >> etc/passwd \
    && echo "$USER:x:$UID:$USER" >> etc/group

RUN rm -rf $HOME/etc/* \
    && echo -e \
        "# A more detailed, commented example configuration can be found here: \
        \n# https://github.com/processone/eturnal/blob/master/config/eturnal.yml \
        \neturnal: \
        \n  log_dir: stdout \
        \n  modules: \
        \n    mod_log_stun: {}" > $HOME/etc/eturnal.yml

RUN home_root_dir=$(echo $HOME | sed 's|\(.*\)/.*|\1 |') \
    && echo "-setcookie eturnal" >> $(find $home_root_dir -name vm.args) \
    && setcap 'cap_net_bind_service=+ep' $(find $home_root_dir -name beam.smp) \
    && echo -e \
        "#!/bin/sh \
        \n \
        \nexec eturnalctl foreground" > usr/local/bin/run.sh \
    && echo -e \
        "#!/bin/sh \
        \nif [ \"\$STUN_SERVICE\" != 'false' ] \
        \nthen \
        \n  case \"\$@\" in foreground|daemon*|console*|reload|restart|reboot) \
        \n    export ETURNAL_RELAY_IPV4_ADDR=\${ETURNAL_RELAY_IPV4_ADDR-\$(stun -4 \$STUN_SERVICE)} \
        \n    export ETURNAL_RELAY_IPV6_ADDR=\${ETURNAL_RELAY_IPV6_ADDR-\$(stun -6 \$STUN_SERVICE)} \
        \n  esac \
        \nfi \
        \nexec /$(find $home_root_dir -name eturnalctl) \"\$@\"" > usr/local/bin/eturnalctl \
    && chmod +x usr/local/bin/run.sh usr/local/bin/eturnalctl \
    && ln -s /$(find $home_root_dir -name stun) usr/local/bin/stun

RUN chown -R $UID:$UID $HOME

################################################################################
#' METHOD='binary' - install runtime dependencies
FROM alpine:${ALPINE_VSN} AS runtime-binary
RUN apk -U upgrade --available --no-cache \
    && apk add --no-cache \
        busybox-binsh \
        libcap2 \
        tini

################################################################################
#' METHOD='build' - install runtime dependencies
FROM runtime-binary AS runtime-build
RUN apk add --no-cache \
        libssl3 \
        libstdc++ \
        ncurses-libs \
        yaml \
        zlib

################################################################################
#' Strip Alpine to only include necessary runtime packages
FROM runtime-${METHOD} AS runtime
RUN apk del --repositories-file /dev/null \
        alpine-baselayout \
        alpine-keys \
        apk-tools \
        libc-utils \
    && rm -rf /var/cache/apk /etc/apk /tmp \
    && find /lib/apk/db -type f -not -name 'installed' -delete

################################################################################
#' Build together musl-libc CI image
# source: https://github.com/sando38/musl-ctr/blob/main/Dockerfile
FROM docker.io/sando38/musl-ctr:${CI_MUSL_VSN} AS musl-ci
COPY --from=eturnal /rootfs /

################################################################################
#' Build together production image
FROM scratch AS prod
ARG REPOSITORY
ARG VERSION
ARG WEB_URL
ARG USER
ARG HOME
ENV ERL_DIST_PORT='3470' \
    PIPE_DIR="/$HOME/run/pipe/" \
    STUN_SERVICE='stun.conversations.im 3478'

COPY --from=runtime / /
COPY --from=eturnal /rootfs /

WORKDIR /$HOME
USER $USER
VOLUME ["/$HOME"]
EXPOSE 3478 3478/udp

HEALTHCHECK \
    --interval=1m \
    --timeout=5s \
    --start-period=5s \
    --retries=3 \
    CMD eturnalctl status

LABEL   org.opencontainers.image.title='eturnal' \
        org.opencontainers.image.description='STUN / TURN standalone server' \
        org.opencontainers.image.url="$WEB_URL" \
        org.opencontainers.image.source="$REPOSITORY" \
        org.opencontainers.image.version="$VERSION" \
        org.opencontainers.image.licenses='Apache-2.0'

ENTRYPOINT ["/sbin/tini","--"]
CMD run.sh
