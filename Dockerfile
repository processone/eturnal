#' Define default build variables
## specific ARGs for METHOD='build'
ARG OTP_VSN='27'
ARG BUILD_IMAGE="cgr.dev/chainguard/wolfi-base"
## specific ARGs for METHOD='package'
ARG PACKAGE_IMAGE="cgr.dev/chainguard/wolfi-base"
## specific ARGs for VARIANT='acme'
ARG ACMESH_VSN='3.0.7'
ARG ACMESH_SHA512='83d080b461662bf2c5cfa9cb51aaf41d7f873f54908e2e5f94d7e3fe8e3f6953d73aafb66adc97455aa958f37c72ef77ba475c7d7cbb3ca3c5bbffb4937c4bae'
## general ARGs
ARG UID='9000'
ARG USER='eturnal'
ARG HOME="opt/$USER"
ARG METHOD='build'
ARG SOURCE='local'
ARG VARIANT='standalone'
ARG BUILD_DIR="/$USER"
ARG REPOSITORY='https://github.com/processone/eturnal'
ARG VERSION='master'
ARG WEB_URL='https://eturnal.net'

################################################################################
#' METHOD='build' - install build dependencies
FROM ${BUILD_IMAGE} AS base
ARG OTP_VSN
RUN apk -U add --no-cache \
        build-base \
        erlang-$OTP_VSN-dev \
        git \
        openssl-dev \
        wget \
        yaml-dev

################################################################################
#' METHOD='build', SOURCE='local' - source files from 'local' machine
FROM base AS local
ARG BUILD_DIR
COPY / $BUILD_DIR/

################################################################################
#' METHOD='build', SOURCE='git' - source files from 'git' repository
FROM base AS git
ARG REPOSITORY
ARG VERSION
ARG BUILD_DIR
WORKDIR $BUILD_DIR
RUN git clone $REPOSITORY . \
    && git checkout $VERSION

################################################################################
#' METHOD='build', SOURCE='web' - source files from 'web': https://eturnal.net/
FROM base AS web
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

RUN wget -O /usr/bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3 \
    && chmod +x /usr/bin/rebar3

ARG REBAR_PROFILE=prod
RUN rebar3 as $REBAR_PROFILE tar
# run rebar3 test suites
RUN rebar3 xref
RUN rebar3 eunit -v
# On slow architectures (e.g. QEMU builds), Common Tests (CT) may fail due to
# timeouts, therefore, retry twice on failure. On versions <= '1.10.1' CTs have
# even shorter timeouts, hence consider to set `--build-arg REBAR_CT=false`.
ARG REBAR_CT=true
RUN if [ "$REBAR_CT" = 'true' ]; then rebar3 ct || rebar3 ct || rebar3 ct; fi

ARG HOME
WORKDIR /rootfs/$HOME
RUN tar -xzf $BUILD_DIR/_build/$REBAR_PROFILE/rel/eturnal/eturnal-*.tar.gz

################################################################################
#' METHOD='package', SOURCE='local' - copy eturnal tarball from local repository
FROM ${PACKAGE_IMAGE} AS package-local
COPY eturnal-*-linux-glibc-*.tar.gz /tmp/

################################################################################
#' METHOD='package', SOURCE='web' - download eturnal binary tarballs from web
FROM ${PACKAGE_IMAGE} AS package-web
ARG WEB_URL
ARG VERSION
RUN apk -U add --no-cache \
        wget \
    && arch=$(uname -m | sed -e 's/x86_64/x64/;s/aarch64/arm64/') \
    && wget -P /tmp $WEB_URL/download/linux/eturnal-$VERSION-linux-glibc-$arch.tar.gz

################################################################################
#' METHOD='package' - install eturnal from binary tarball
FROM package-${SOURCE} AS package
WORKDIR /rootfs
ARG HOME
RUN home_root_dir=$(echo $HOME | sed 's|\(.*\)/.*|\1 |') \
    && mkdir -p $home_root_dir \
    && arch=$(uname -m | sed -e 's/x86_64/x64/;s/aarch64/arm64/') \
    && tar -xzf /tmp/eturnal-*-linux-glibc-$arch.tar.gz -C $home_root_dir

################################################################################
#' Prepare eturnal for runtime
FROM ${METHOD} AS eturnal-standalone
RUN apk -U add --no-cache \
        libcap-utils \
        pax-utils

WORKDIR /rootfs
ARG HOME
RUN mkdir -p $HOME/log $HOME/run $HOME/tls bin etc usr/local/bin

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
        \nif [ \"\$STUN_SERVICE\" != 'false' ] \
        \nthen \
        \n  case \"\$@\" in foreground|daemon*|console*|reload|restart|reboot) \
        \n    export ETURNAL_RELAY_IPV4_ADDR=\${ETURNAL_RELAY_IPV4_ADDR-\$(stun -4 \$STUN_SERVICE)} \
        \n    export ETURNAL_RELAY_IPV6_ADDR=\${ETURNAL_RELAY_IPV6_ADDR-\$(stun -6 \$STUN_SERVICE)} \
        \n  esac \
        \nfi \
        \nexec /$(find $home_root_dir -name eturnalctl) \"\$@\"" > usr/local/bin/eturnalctl \
    && chmod +x usr/local/bin/* \
    && ln -s /$(find $home_root_dir -name stun) usr/local/bin/stun \
    && scanelf --needed --nobanner --format '%n#p' --recursive $home_root_dir \
        | tr ',' '\n' \
        | sort -u \
        | awk 'system("[ -e $home_root_dir" $1 " ]") == 0 { next } { print "so:" $1 }' \
            > /tmp/runDeps

ARG UID
RUN chown -R $UID:$UID $HOME
COPY --chmod=555 overlay/container/standalone/usr/local/bin/run.sh /rootfs/usr/local/bin/run.sh

################################################################################
#' VARIANT='acme' - copy s6 entrypoint/ runtime scripts
FROM eturnal-standalone AS eturnal-acme
RUN apk add --no-cache \
        openssl

ARG UID
ARG HOME
COPY --chown=$UID:$UID overlay/container/acmesh/etc /rootfs/$HOME/etc
COPY overlay/container/acmesh/usr /rootfs/usr

WORKDIR /rootfs
RUN mkdir -p etc \
    && ln -s /$HOME/etc/s6.d etc/s6.d

RUN export PEM=$HOME/tls/key.pem \
    && wget -O /etc/ssl/openssl.cnf \
        https://raw.githubusercontent.com/openssl/openssl/openssl-3.3/apps/openssl.cnf \
    && openssl req -x509 \
            -batch \
            -nodes \
            -newkey rsa:4096 \
            -keyout $PEM \
            -out $PEM \
            -days 3650 \
            -subj "/CN=localhost" \
    && touch $HOME/tls/fullchain.pem \
    && chmod 644 $HOME/tls/fullchain.pem \
    && chown -R $UID:$UID $HOME/tls

################################################################################
#' define the final eturnal VARIANT
FROM eturnal-${VARIANT} AS eturnal

################################################################################
#' Update, finalize & strip Alpine to only include necessary runtime packages
FROM ${BUILD_IMAGE} AS runtime
RUN apk -U upgrade --available --no-cache

ARG USER
ARG UID
ARG HOME
RUN addgroup $USER -g $UID \
    && adduser -s /sbin/nologin -D -u $UID -h /$HOME -G $USER $USER

ARG VARIANT
RUN if [ "$VARIANT" = 'acme' ]; \
    then \
        apk add --no-cache -t .acmesh-deps \
            ca-certificates-bundle \
            openssl \
            s6 \
            socat \
            wget ; \
    fi

ARG ACMESH_VSN
ARG ACMESH_SHA512
USER $USER
ENV HOME="/$HOME"
WORKDIR /$HOME
RUN if [ "$VARIANT" = 'acme' ]; \
    then \
        wget -O acmesh.tar.gz \
            https://github.com/acmesh-official/acme.sh/archive/"${ACMESH_VSN}".tar.gz \
        && echo "$ACMESH_SHA512 *acmesh.tar.gz" | sha512sum -c - \
        && tar -xzf acmesh.tar.gz -C /tmp \
        && cd /tmp/acme.sh-"${ACMESH_VSN}" \
        && ./acme.sh --install --no-cron \
        && cd $OLDPWD \
        && rm -rf /tmp/acme.sh-"${ACMESH_VSN}" acmesh.tar.gz ; \
    fi

USER root
COPY --from=eturnal-standalone /tmp/runDeps /tmp/runDeps
RUN apk add --no-cache -t .runtime-deps \
        $(cat /tmp/runDeps) \
        busybox \
        so:libcap.so.2 \
        tini

RUN apk del --repositories-file /dev/null \
        apk-tools \
        wolfi-base \
        wolfi-keys \
    && rm -rf /var/cache/apk /etc/apk /tmp/* \
    && find /lib/apk/db -type f -not -name 'installed' -delete

################################################################################
#' Build together production image
FROM scratch AS release
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
