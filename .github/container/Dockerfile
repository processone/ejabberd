#' Define default build variables
ARG ALPINE_VSN='3.17'
ARG UID='9000'
ARG USER='ejabberd'
ARG HOME="opt/$USER"
ARG METHOD='direct'
ARG BUILD_DIR="/$USER"
ARG VERSION='master'

################################################################################
#' METHOD='direct' - build and install ejabberd directly from source
FROM alpine:${ALPINE_VSN} AS direct

RUN apk -U add --no-cache \
        autoconf \
        automake \
        bash \
        build-base \
        curl \
        elixir \
        erlang-odbc \
        erlang-reltool \
        expat-dev \
        file \
        gd-dev \
        git \
        jpeg-dev \
        libpng-dev \
        libwebp-dev \
        linux-pam-dev \
        openssl-dev \
        sqlite-dev \
        yaml-dev \
        zlib-dev

RUN mix local.hex --force \
    && mix local.rebar --force

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

RUN wget -O "$HOME/conf/cacert.pem" 'https://curl.se/ca/cacert.pem' \
    && sed -i '/^loglevel:/a \ \
        \nca_file: /opt/ejabberd/conf/cacert.pem \
        \ncertfiles: \
        \n  - /opt/ejabberd/conf/server.pem' "$HOME/conf/ejabberd.yml"

################################################################################
#' METHOD='package' - install ejabberd from binary tarball package
FROM alpine:${ALPINE_VSN} AS package
COPY tarballs/ejabberd-*-linux-musl-*.tar.gz /tmp/
WORKDIR /rootfs
ARG HOME
RUN home_root_dir=$(echo $HOME | sed 's|\(.*\)/.*|\1 |') \
    && mkdir -p $home_root_dir \
    && ARCH=$(uname -m | sed -e 's/x86_64/x64/;s/aarch64/arm64/') \
    && tar -xzf /tmp/ejabberd-*-linux-musl-$ARCH.tar.gz -C $home_root_dir

################################################################################
#' Prepare ejabberd for runtime
FROM ${METHOD} AS ejabberd
RUN apk -U add --no-cache \
        git \
        libcap-utils \
        openssl

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

RUN export PEM=$HOME/conf/server.pem \
    && openssl req -x509 \
            -batch \
            -nodes \
            -newkey rsa:4096 \
            -keyout $PEM \
            -out $PEM \
            -days 3650 \
            -subj "/CN=localhost"

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
    && chmod +x usr/local/bin/*

ARG UID
RUN chown -R $UID:$UID $HOME

################################################################################
#' METHOD='package' - install runtime dependencies
FROM alpine:${ALPINE_VSN} AS runtime-package
RUN apk -U upgrade --available --no-cache \
    && apk add --no-cache \
        libcap2 \
        tini

################################################################################
#' METHOD='direct' - install runtime dependencies
FROM runtime-package AS runtime-direct
RUN apk add --no-cache \
        expat \
        freetds \
        gd \
        jpeg \
        libgd \
        libpng \
        libstdc++ \
        libwebp \
        linux-pam \
        ncurses-libs \
        sqlite \
        sqlite-libs \
        unixodbc \
        yaml \
        zlib \
    && ln -fs /usr/lib/libtdsodbc.so.0 /usr/lib/libtdsodbc.so

################################################################################
#' Finalize runtime environment
FROM runtime-${METHOD} AS runtime
ARG USER
ARG UID
ARG HOME
RUN addgroup $USER -g $UID \
    && adduser -s /sbin/nologin -D -u $UID -h /$HOME -G $USER $USER

################################################################################
#' Build together production image
FROM scratch AS prod
ARG USER
ARG HOME

COPY --from=runtime / /
COPY --from=ejabberd /rootfs /

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

ENTRYPOINT ["/sbin/tini","--"]
CMD ["ejabberdctl", "foreground"]
