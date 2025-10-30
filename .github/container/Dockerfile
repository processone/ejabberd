#' Define default build variables
ARG OTP_VSN='28.1.0.0'
ARG ELIXIR_VSN='1.19.1'
ARG UID='9000'
ARG USER='ejabberd'
ARG HOME="opt/$USER"
ARG BUILD_DIR="/$USER"
ARG VERSION='master'

################################################################################
#' Compile ejabberdapi
FROM docker.io/golang:1.25-alpine AS api
RUN go install -v \
    github.com/processone/ejabberd-api/cmd/ejabberd@master \
    && mv bin/ejabberd bin/ejabberdapi

################################################################################
#' build and install ejabberd directly from source
FROM docker.io/erlang:${OTP_VSN}-alpine AS ejabberd

RUN apk -U add --no-cache \
        autoconf \
        automake \
        bash \
        build-base \
        curl \
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

ARG ELIXIR_VSN
RUN wget -O - https://github.com/elixir-lang/elixir/archive/v$ELIXIR_VSN.tar.gz \
        | tar -xzf -

WORKDIR /elixir-$ELIXIR_VSN
ENV ERL_FLAGS="+JPperf true"
RUN make install clean

RUN mix local.hex --force \
    && mix local.rebar --force

ARG BUILD_DIR
COPY / $BUILD_DIR/

WORKDIR $BUILD_DIR
RUN mv .github/container/ejabberdctl.template . \
    && mv .github/container/ejabberd.yml.example . \
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

RUN wget -O "$HOME/conf/cacert.pem" 'https://curl.se/ca/cacert.pem'

#' Prepare ejabberd for runtime
RUN apk -U add --no-cache \
        git \
        libcap \
        openssl

RUN mkdir -p usr/local/bin $HOME/conf $HOME/database $HOME/logs $HOME/upload

COPY --from=api /go/bin/ejabberdapi usr/local/bin/

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

RUN sed -i 's|^#CTL_OVER_HTTP=|CTL_OVER_HTTP=../|' "$HOME/conf/ejabberdctl.cfg"

RUN home_root_dir=$(echo $HOME | sed 's|\(.*\)/.*|\1 |') \
    && setcap 'cap_net_bind_service=+ep' $(find $home_root_dir -name beam.smp) \
    && echo -e \
        "#!/bin/sh \
        \n[ -z \$ERLANG_NODE_ARG ] && export ERLANG_NODE_ARG=ejabberd@localhost \
        \nexport EMA=\"\$EJABBERD_MACRO_ADMIN\" \
        \nexport HOST=\"\${EJABBERD_MACRO_HOST:-localhost}\" \
        \nif [ -n \"\$EMA\" ] \
        \nthen \
        \n    if [ \"\$EMA\" != \"\${EMA%%@*}\" ] \
        \n    then \
        \n        export USERNAME=\"\${EMA%%@*}\" \
        \n        export HOST=\"\${EMA##*@}\" \
        \n    else \
        \n        export USERNAME=\"\$EMA\" \
        \n        export SHOW_WARNING=\"true\" \
        \n    fi \
        \nelif [ -n \"\$REGISTER_ADMIN_PASSWORD\" ] \
        \nthen \
        \n    export USERNAME=\"admin\" \
        \nelse \
        \n    export USERNAME=\"\$(od -A n -N 8 -t x8 /dev/urandom)\" \
        \nfi \
        \nexport EJABBERD_MACRO_ADMIN=\"\$USERNAME@\$HOST\" \
        \n[ -n \"\$SHOW_WARNING\" ] && echo \"WARNING: The EJABBERD_MACRO_ADMIN environment variable was set to '\$EMA', but it should include the host... I'll overwrite it to become '\$EJABBERD_MACRO_ADMIN'.\" \
        \n[ -n \"\$CTL_ON_CREATE\" ] && export SEPARATOR=\";\" \
        \n[ -n \"\$REGISTER_ADMIN_PASSWORD\" ] && export CTL_ON_CREATE=\"register \${EJABBERD_MACRO_ADMIN%%@*} \${EJABBERD_MACRO_ADMIN##*@} \$REGISTER_ADMIN_PASSWORD \$SEPARATOR \$CTL_ON_CREATE\" \
        \nexport CONFIG_DIR=/$HOME/conf \
        \nexport LOGS_DIR=/$HOME/logs \
        \nexport SPOOL_DIR=/$HOME/database \
        \nexec /$(find $home_root_dir -name ejabberdctl) \"\$@\"" \
            > usr/local/bin/ejabberdctl \
    && chmod +x usr/local/bin/* \
    && scanelf --needed --nobanner --format '%n#p' --recursive $home_root_dir \
        | tr ',' '\n' \
        | sort -u \
        | awk 'system("[ -e $home_root_dir" $1 " ]") == 0 { next } { print "so:" $1 }' \
        | sed -e "s|so:libc.so|so:libc.musl-$(uname -m).so.1|" \
            > /tmp/runDeps

ARG UID
RUN chown -R $UID:$UID $HOME

RUN cp /rootfs/$HOME-$VERSION/lib/captcha*.sh usr/local/bin/
RUN mkdir $HOME/sql \
    && find /rootfs/$HOME-$VERSION/lib/ -name *.sql -exec cp {} $HOME/sql \; -exec cp {} $HOME/database \;

################################################################################
#' Remove erlang/OTP & rebar3
FROM docker.io/erlang:${OTP_VSN}-alpine AS runtime
RUN apk del .erlang-rundeps \
    && rm -f $(which rebar3) \
    && find /usr -type d -name 'erlang' -exec rm -rf {} + \
    && find /usr -type l -exec test ! -e {} \; -delete

#' Update alpine, finalize runtime environment
COPY --from=ejabberd /tmp/runDeps /tmp/runDeps
RUN apk -U upgrade --available --no-cache \
    && apk add --no-cache \
        $(cat /tmp/runDeps) \
        so:libcap.so.2 \
        so:libtdsodbc.so.0 \
        curl \
        tini \
    && rm /tmp/runDeps \
    && ln -fs /usr/lib/libtdsodbc.so.0 /usr/lib/libtdsodbc.so

ARG USER
ARG UID
ARG HOME
RUN addgroup $USER -g $UID \
    && adduser -s /sbin/nologin -D -u $UID -h /$HOME -G $USER $USER

RUN ln -fs /usr/local/bin/ /opt/ejabberd/bin
RUN rm -rf /home \
    && ln -fs /opt /home

################################################################################
#' Build together production image
FROM scratch
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
EXPOSE 1880 1883 4369-4399 5210 5222 5269 5280 5443

ENTRYPOINT ["/sbin/tini","--","ejabberdctl"]
CMD ["foreground"]
