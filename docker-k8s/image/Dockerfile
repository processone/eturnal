FROM alpine:3.16.0 AS eturnal
# build arguments
ARG REPOSITORY=https://github.com/processone/eturnal.git
ARG VERSION=master
ARG BUILD_DIR=/eturnal
# runtime environment variables
ENV HOME=/opt/eturnal \
    ERL_DIST_PORT=3470

RUN apk update --no-cache \
    && apk upgrade --available --no-cache \
# add build dependencies
    && apk add --no-cache -t build-deps \
      build-base \
      erlang-dev \
      git \
      openssl-dev \
      yaml-dev \
# build eturnal from source
    && mkdir -p $BUILD_DIR \
    && cd $BUILD_DIR \
    && git clone $REPOSITORY . \
    && git checkout $VERSION \
    && echo "-setcookie eturnal" >> config/vm.args \
    && wget https://s3.amazonaws.com/rebar3/rebar3 \
    && chmod +x rebar3 \
    && ./rebar3 as prod tar \
# user and directories
    && mkdir -p $HOME/log $HOME/run $HOME/tls \
    && cd $HOME \
    && addgroup eturnal -g 9000 \
    && adduser -s /bin/sh -D -u 9000 -h $HOME -G eturnal eturnal \
# extract eturnal
    && tar -xzf $BUILD_DIR/_build/prod/rel/eturnal/eturnal-*.tar.gz -C $HOME \
    && chown -R 9000:9000 $HOME \
# add runtime packages
    && apk add --no-cache \
      libstdc++ \
      ncurses-libs \
      tini \
      yaml \
# remove build dependencies and source files
    && apk del build-deps \
    && rm -rf $BUILD_DIR \
# eturnalctl link, scripts, and configuration files
    && ln -s $HOME/bin/eturnalctl /usr/local/bin/eturnalctl \
    && echo -e \
        '#!/bin/sh \
        \nexport ETURNAL_RELAY_IPV4_ADDR=${ETURNAL_RELAY_IPV4_ADDR:-$(wget -qO - https://v4.ident.me | xargs echo)} \
        \nexec eturnalctl $@' > /usr/local/bin/run.sh \
    && chmod +x /usr/local/bin/* \
    && sed -i -e "s/#log_dir: stdout/log_dir: stdout/g" $HOME/etc/eturnal.yml

WORKDIR $HOME
USER eturnal
VOLUME ["$HOME/log", "$HOME/run", "$HOME/tls"]
EXPOSE 3478 3478/udp 5349

ENTRYPOINT ["/sbin/tini","--","run.sh"]
CMD ["foreground"]
