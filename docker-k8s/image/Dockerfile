FROM alpine:3.16.1 AS eturnal
# BUILD: define image build arguments
ARG BUILD_DIR=/eturnal
ARG REPOSITORY=https://github.com/processone/eturnal.git
ARG VERSION=master
ARG REBAR_PROFILE=prod
ARG HOME=/opt/eturnal
# RUNTIME: define container runtime environment variables
ENV ERL_DIST_PORT=3470 \
    PIPE_DIR=$HOME/run/pipe/ \
    STUN_SERVICE="stun.conversations.im 3478"

WORKDIR $BUILD_DIR
RUN set -x && apk -U upgrade --latest \
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
# BUILD: run rebar3 test suites
    && ./rebar3 xref \
    && ./rebar3 eunit \
    && ./rebar3 ct \
# RUNTIME: add/ define runtime packages
    && apk add \
      libcap \
      libssl1.1 \
      libstdc++ \
      ncurses-libs \
      tini \
      yaml \
      zlib \
# RUNTIME: add runtime user and directories
    && install -o 9000 -g 9000 -d $HOME/log $HOME/run $HOME/tls \
    && addgroup eturnal -g 9000 \
    && adduser -s /sbin/nologin -D -u 9000 -H -G eturnal eturnal \
# RUNTIME: extract release tarball to eturnal's runtime directory
    && tar -xzf $BUILD_DIR/_build/$REBAR_PROFILE/rel/eturnal/eturnal-*.tar.gz -C $HOME \
    && rm -rf $HOME/etc/* \
# RUNTIME: create symbolic links, entrypoint script and minimal configuration file
    && ln -s $HOME/bin/eturnalctl /usr/local/bin/eturnalctl \
    && ln -s $HOME/bin/stun /usr/local/bin/stun \
    && echo -e \
        '#!/bin/sh \
        \nif [ "$STUN_SERVICE" != "false" ] \
        \nthen \
        \n  export ETURNAL_RELAY_IPV4_ADDR=${ETURNAL_RELAY_IPV4_ADDR-$(stun -4 $STUN_SERVICE)} \
        \n  export ETURNAL_RELAY_IPV6_ADDR=${ETURNAL_RELAY_IPV6_ADDR-$(stun -6 $STUN_SERVICE)} \
        \nfi \
        \nexec "$@"' > /usr/local/bin/run.sh \
    && chmod +x /usr/local/bin/run.sh \
    && echo -e \
        "# A more detailed, commented example configuration can be found here: \
        \n# https://github.com/processone/eturnal/blob/master/config/eturnal.yml \
        \neturnal: \
        \n  log_dir: stdout \
        \n  modules: \
        \n    mod_log_stun: {}" > $HOME/etc/eturnal.yml \
# RUNTIME: set CAPS for binding to privileged ports and remove unnecessary binaries
    && setcap 'cap_net_bind_service=+ep' $(find $HOME -name beam.smp) \
    && find /usr/sbin -type f -not -name 'capsh,getcap,getpcaps,setcap' -delete \
# CLEANUP: remove build dependencies, apk package manager and source files
    && apk del --repositories-file /dev/null \
      alpine-baselayout \
      alpine-keys \
      apk-tools \
      build-deps \
      libc-utils \
    && rm -rf $BUILD_DIR /var/cache/apk /etc/apk /tmp \
    && find /lib/apk/db -type f -not -name 'installed' -delete

# RUNTIME: define container runtime parameters
WORKDIR $HOME
USER eturnal
VOLUME ["$HOME/log","$HOME/run"]
EXPOSE 3478 3478/udp
ENTRYPOINT ["/sbin/tini","--","run.sh"]
CMD ["eturnalctl", "foreground"]
