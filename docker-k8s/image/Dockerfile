FROM alpine:3.16.1 AS eturnal
# BUILD: define image build arguments
ARG BUILD_DIR=/eturnal
ARG REPOSITORY=https://github.com/processone/eturnal.git
ARG VERSION=master
ARG REBAR_PROFILE=stripped
# RUNTIME: define container runtime environment variables
ENV HOME=/opt/eturnal \
    ERL_DIST_PORT=3470 \
    STUN_SERVICE="stun.conversations.im 3478"

WORKDIR $BUILD_DIR
RUN apk -U upgrade --latest \
# BUILD: add build dependencies
    && apk add -t build-deps \
      build-base \
      erlang-dev \
      git \
      openssl-dev \
      yaml-dev \
# BUILD: prepare and build
    && git clone $REPOSITORY --branch $VERSION --single-branch . \
    && echo "-setcookie eturnal" >> config/vm.args \
    && wget https://s3.amazonaws.com/rebar3/rebar3 \
    && chmod +x rebar3 \
    && ./rebar3 as $REBAR_PROFILE tar \
# RUNTIME: add/ define runtime packages
    && apk add \
      libssl1.1 \
      libstdc++ \
      ncurses-libs \
      tini \
      yaml \
      zlib \
# RUNTIME: add runtime user and directories
    && install -o 9000 -g 9000 -d $HOME/log $HOME/run $HOME/tls \
    && addgroup eturnal -g 9000 \
    && adduser -s /bin/sh -D -u 9000 -H -G eturnal eturnal \
 # RUNTIME: extract release tarball to eturnal's runtime directory
    && tar -xzf $BUILD_DIR/_build/$REBAR_PROFILE/rel/eturnal/eturnal-*.tar.gz -C $HOME \
    && rm -rf $HOME/etc/* \
# RUNTIME: create eturnalctl link, entrypoint script and configuration file
    && ln -s $HOME/bin/eturnalctl /usr/local/bin/eturnalctl \
    && echo -e \
        '#!/bin/sh \
        \nexport ETURNAL_RELAY_IPV4_ADDR=${ETURNAL_RELAY_IPV4_ADDR-$($HOME/bin/stun $STUN_SERVICE)} \
        \nexec "$@"' > /usr/local/bin/run.sh \
    && chmod +x /usr/local/bin/run.sh \
    && echo -e \
        "# A more detailed, commented example configuration can be found here: \
        \n# https://github.com/processone/eturnal/blob/master/config/eturnal.yml \
        \neturnal: \
        \n  log_dir: stdout \
        \n  modules: \
        \n    mod_log_stun: {}" > $HOME/etc/eturnal.yml \
# CLEANUP: remove build dependencies, apk package manager and source files
    && apk del --repositories-file /dev/null \
      alpine-baselayout \
      alpine-keys \
      apk-tools \
      build-deps \
      libc-utils \
    && rm -rf $BUILD_DIR /var/cache/apk /etc/apk \
    && find /lib/apk/db -type f -not -name 'installed' -delete

# RUNTIME: define container runtime parameters
WORKDIR $HOME
USER eturnal
VOLUME ["$HOME/log","$HOME/run"]
EXPOSE 3478 3478/udp
ENTRYPOINT ["/sbin/tini","--","run.sh"]
CMD ["eturnalctl", "foreground"]
