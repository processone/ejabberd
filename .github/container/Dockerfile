#' Define default build variables
## source ARGs
ARG OTP_VSN='27'
ARG ELIXIR_VSN='1.17.2'
## general ARGs
ARG UID='9000'
ARG USER='ejabberd'
ARG HOME="opt/$USER"
ARG METHOD='direct'
ARG BUILD_DIR="/$USER"
ARG VERSION='master'

################################################################################
#' Build and base image
FROM cgr.dev/chainguard/wolfi-base AS erlang
ARG OTP_VSN
ENV LC_ALL='C.UTF-8' \
    LANG='C.UTF-8'

RUN apk -U upgrade --available && apk add --no-cache  \
        autoconf \
        automake \
        bash \
        build-base \
        ca-certificates-bundle \
        curl \
        expat-dev \
        erlang-$OTP_VSN \
        erlang-$OTP_VSN-dev \
        erlang-$OTP_VSN-odbc \
        file \
        freetds freetds-dev \
        freetype-dev \
        gd-dev \
        git \
        libjpeg-dev \
        libpng-dev \
        libwebp-dev \
        linux-pam-dev \
        ncurses-dev \
        pax-utils \
        perl-dev \
        openssl \
        openssl-dev \
        sqlite-dev \
        unixodbc unixodbc-dev \
        wget \
        yaml-dev \
        zlib-dev

WORKDIR /
ARG ELIXIR_VSN
RUN wget -O - https://github.com/elixir-lang/elixir/archive/v"$ELIXIR_VSN".tar.gz \
        | tar -xzf -

WORKDIR /elixir-"$ELIXIR_VSN"
RUN make install clean

RUN mix local.hex --force \
    && mix local.rebar --force

################################################################################
#' Build and prepare ejabberd
FROM erlang AS direct
ENV LC_ALL='C.UTF-8' \
    LANG='C.UTF-8'

ARG BUILD_DIR
COPY / $BUILD_DIR/

WORKDIR $BUILD_DIR
RUN mv .github/container/ejabberdctl.template . \
    && ./autogen.sh \
    && ./configure --with-rebar=mix --enable-all \
    && make deps \
    && make rel

WORKDIR /rootfs
ARG VERSION
ARG HOME
RUN mkdir -p $HOME $HOME-$VERSION \
&& cp -r $BUILD_DIR/_build/prod/rel/ejabberd/* $HOME-$VERSION \
    && mv $HOME-$VERSION/conf $HOME/conf

RUN cp -p $BUILD_DIR/tools/captcha*.sh $HOME-$VERSION/lib

RUN find "$HOME-$VERSION/bin" -name 'ejabberd' -delete \
    && find "$HOME-$VERSION/releases" -name 'COOKIE' -delete

RUN sed -i '/^loglevel:/a \ \
        \ncertfiles: \
        \n  - /opt/ejabberd/conf/server.pem' "$HOME/conf/ejabberd.yml"

################################################################################
#' METHOD='package' - install ejabberd from binary tarball package
FROM cgr.dev/chainguard/wolfi-base AS package
COPY tarballs/ejabberd-*-linux-gnu-*.tar.gz /tmp/
WORKDIR /rootfs
ARG HOME
RUN home_root_dir=$(echo $HOME | sed 's|\(.*\)/.*|\1 |') \
    && mkdir -p $home_root_dir \
    && ARCH=$(uname -m | sed -e 's/x86_64/x64/;s/aarch64/arm64/') \
    && tar -xzf /tmp/ejabberd-*-linux-gnu-$ARCH.tar.gz -C $home_root_dir

################################################################################
#' OpenSSL - Create server certificate for localhost,
#            because wolfi misses openssl config file
FROM docker.io/library/alpine AS servercert
RUN apk -U add --no-cache openssl
RUN export PEM=/tmp/server.pem \
    && openssl req -x509 \
            -batch \
            -nodes \
            -newkey rsa:4096 \
            -keyout $PEM \
            -out $PEM \
            -days 3650 \
            -subj "/CN=localhost"

################################################################################
#' Prepare ejabberd for runtime
FROM ${METHOD} AS ejabberd
RUN apk -U add --no-cache \
        git \
        libcap-utils \
        pax-utils

WORKDIR /rootfs
ARG HOME
RUN mkdir -p usr/local/bin $HOME/conf $HOME/database $HOME/logs $HOME/upload

ARG BUILD_DIR
RUN if [ ! -d $HOME/.ejabberd-modules ]; \
    then \
        if [ -d $BUILD_DIR/.ejabberd-modules ]; \
        then cp -r $BUILD_DIR/.ejabberd-modules $HOME; \
        else git clone https://github.com/processone/ejabberd-contrib --depth 1 \
                $HOME/.ejabberd-modules/sources/ejabberd-contrib; \
        fi \
    fi

COPY --from=servercert /tmp/server.pem $HOME/conf/server.pem

RUN home_root_dir=$(echo $HOME | sed 's|\(.*\)/.*|\1 |') \
    && setcap 'cap_net_bind_service=+ep' $(find $home_root_dir -name beam.smp) \
    && echo -e \
        "#!/bin/sh \
        \n[ -z \$ERLANG_NODE_ARG ] && export ERLANG_NODE_ARG=ejabberd@localhost \
        \nexport CONFIG_DIR=/$HOME/conf \
        \nexport LOGS_DIR=/$HOME/logs \
        \nexport SPOOL_DIR=/$HOME/database \
        \nexec /$(find $home_root_dir -name ejabberdctl) \"\$@\"" \
            > usr/local/bin/ejabberdctl \
    && chmod +x usr/local/bin/* \
    && scanelf --needed --nobanner --format '%n#p' --recursive "$PWD" \
        | tr ',' '\n' \
        | sort -u \
        | awk 'system("[ -e $PWD" $1 " ]") == 0 { next } { print "so:" $1 }' \
            > /tmp/runDeps

ARG UID
RUN chown -R $UID:$UID $HOME

################################################################################
#' Build release image
FROM cgr.dev/chainguard/wolfi-base AS release
ARG USER
ARG UID
ARG HOME
RUN addgroup $USER -g $UID \
    && adduser -s /sbin/nologin -D -u $UID -h /$HOME -G $USER $USER

COPY --from=ejabberd /tmp/runDeps /tmp/runDeps
RUN apk -U upgrade --available --no-cache \
    && apk add --no-cache -t .ejabberd-rundeps \
        $(cat /tmp/runDeps) \
        freetds \
        unixodbc \
        libcap \
        busybox \
        ca-certificates-bundle \
        tini

COPY --from=ejabberd /rootfs /

ENV ERL_DIST_PORT='5210' \
    LC_ALL='C.UTF-8' \
    LANG='C.UTF-8'

HEALTHCHECK \
    --interval=1m \
    --timeout=5s \
    --start-period=5s \
    --retries=10 \
    CMD ejabberdctl status

WORKDIR /$HOME
USER $USER
VOLUME ["/$HOME"]
EXPOSE 1883 4369-4399 5210 5222 5269 5280 5443

ENTRYPOINT ["/sbin/tini","--","ejabberdctl"]
CMD ["foreground"]
