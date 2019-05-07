# docker build -t ejabberd/riak .
# docker run --rm -it -p 8087:8087 -p 8098:8098 -v "`pwd`/ebin:/usr/lib/ejabberd/ebin" --name ejabberd-riak ejabberd/riak
# docker exec -it ejabberd-riak /bin/bash
# docker push ejabberd/riak:latest
FROM ubuntu:16.04

ENV DEBIAN_FRONTEND noninteractive
# ENV RIAK_VERSION 2.0.9-1
# install riak=${RIAK_VERSION} to fallback to an older version

RUN \
    apt-get update -qq && \
    apt-get install -y curl apt-utils iputils-ping && \
    curl -s https://packagecloud.io/install/repositories/basho/riak/script.deb.sh | bash && \
    apt-get install -y riak && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Expose default ports
EXPOSE 8087
EXPOSE 8098

# Expose volumes for data and logs
VOLUME /var/log/riak
VOLUME /var/lib/riak

# Expose volumes for ejabberd code for map/reduce in Riak.
# It can be overloaded to provide a new ejabberd_riak.beam file.
# Note that ejabberd_riak.beam needs to be compiled with Erlang/OTP R16
# as this is the version of Erlang Basho has packaged with Riak.
COPY ejabberd_riak.beam /usr/lib/ejabberd/ebin/ejabberd_riak.beam
VOLUME /usr/lib/ejabberd/ebin

# Install custom start script
COPY riak-cluster.sh /sbin/riak-cluster.sh
RUN chmod a+x /sbin/riak-cluster.sh

# Change configuration to make it work with ejabberd
COPY advanced.config /etc/riak/advanced.config
COPY riak.conf /etc/riak/riak.conf

# Install custom hooks
COPY prestart.d /etc/riak/prestart.d
# COPY poststart.d /etc/riak/poststart.d

# Prepare for bootstrapping schemas
RUN mkdir -p /etc/riak/schemas

# Clean up APT cache
RUN rm -rf /var/lib/apt/lists/* /tmp/*

WORKDIR /var/lib/riak

CMD ["/sbin/riak-cluster.sh"]
