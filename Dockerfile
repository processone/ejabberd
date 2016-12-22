FROM debian:jessie
MAINTAINER Rafael Römhild <rafael@roemhild.de>

ENV XMPP_DOMAIN=localhost \
    EJABBERD_HOME=/opt/ejabberd \
    PATH=/opt/ejabberd/bin:/usr/sbin:/usr/bin:/sbin:/bin \
    LC_ALL=C.UTF-8 \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8

# bootstrap
COPY . /tmp/ejabberd
RUN /tmp/ejabberd/docker/bootstrap.sh

# Continue as user
USER ejabberd

# Set workdir to ejabberd root
WORKDIR /opt/ejabberd

VOLUME ["/opt/ejabberd/conf", "/opt/ejabberd/database", "/opt/ejabberd/ssl", "/opt/ejabberd/backup", "/opt/ejabberd/upload", "/opt/ejabberd/modules"]

EXPOSE 4560 5222 5269 5280 5443

ENTRYPOINT ["/opt/ejabberd/docker/start.sh"]
