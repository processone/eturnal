FROM alpine:3.15.4 AS build
ARG VERSION=master

RUN apk upgrade --update musl \
    && apk add \
    build-base \
    erlang \
    erlang-dev \
    git \
    openssl \
    openssl-dev \
    yaml-dev

WORKDIR eturnal

RUN git clone https://github.com/processone/eturnal.git . \
    && git checkout $VERSION \
    && echo "-setcookie eturnal" >> config/vm.args \
    && wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 \
    && ./rebar3 as prod tar

RUN mkdir -p /opt/eturnal \
    && cp -r _build/prod/rel/eturnal/eturnal-*.tar.gz /opt/eturnal

FROM alpine:3.15.4
ENV HOME=/opt/eturnal

COPY --from=build /opt /opt

# add packages and remove cache
RUN apk upgrade --update musl \
    && apk add \
    libstdc++ \
    ncurses-libs \
    openssl \
    yaml \
    && rm -rf /var/cache/apk/* \
# user and directories
    && tar -xzf $HOME/eturnal-*.tar.gz -C $HOME \
    && rm -rf $HOME/eturnal-*.tar.gz \
    && addgroup eturnal -g 9000 \
    && adduser -s /bin/sh -D -u 9000 -h $HOME -G eturnal eturnal \
    && mkdir -p $HOME/log $HOME/run $HOME/tls \
    && chown -R 9000:9000 $HOME \
# eturnalctl link, scripts and configuration files
    && ln -s $HOME/bin/eturnalctl /usr/local/bin/eturnalctl

WORKDIR $HOME
USER eturnal
VOLUME ["$HOME/tls", "$HOME/log"]
EXPOSE 3478/udp 3478 5349

CMD ["/usr/local/bin/eturnalctl", "foreground"]
