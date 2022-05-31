FROM alpine:3.16.0 AS build
ARG VERSION=master

RUN apk upgrade --update musl \
    && apk add \
      build-base \
      erlang-dev \
      git \
      openssl-dev \
      yaml-dev

WORKDIR eturnal

RUN git clone https://github.com/processone/eturnal.git . \
    && git checkout $VERSION \
    && echo "-setcookie eturnal" >> config/vm.args \
    && wget https://s3.amazonaws.com/rebar3/rebar3 \
    && chmod +x rebar3 \
    && ./rebar3 as prod tar

RUN mkdir -p /opt/eturnal \
    && cp _build/prod/rel/eturnal/eturnal-*.tar.gz /opt/eturnal

FROM alpine:3.16.0 AS eturnal
ENV HOME=/opt/eturnal \
    ERL_DIST_PORT=3470

COPY --from=build /opt /opt

# add packages and remove cache
RUN apk upgrade --update --no-cache musl \
    && apk --no-cache add \
    libstdc++ \
    ncurses-libs \
    openssl \
    yaml \
# user and directories
    && tar -xzf $HOME/eturnal-*.tar.gz -C $HOME \
    && rm -f $HOME/eturnal-*.tar.gz \
    && addgroup eturnal -g 9000 \
    && adduser -s /bin/sh -D -u 9000 -h $HOME -G eturnal eturnal \
    && mkdir -p $HOME/log $HOME/run $HOME/tls \
    && chown -R 9000:9000 $HOME \
# eturnalctl link, scripts, and configuration files
    && ln -s $HOME/bin/eturnalctl /usr/local/bin/eturnalctl \
    && sed -i -e "s/#log_dir: stdout/log_dir: stdout/g" $HOME/etc/eturnal.yml

WORKDIR $HOME
USER eturnal
VOLUME ["$HOME/log", "$HOME/run", "$HOME/tls"]
EXPOSE 3478/udp 3478 5349

CMD ["/usr/local/bin/eturnalctl", "foreground"]
