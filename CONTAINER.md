
[![GitHub tag (latest SemVer)](https://img.shields.io/github/v/tag/processone/ejabberd?sort=semver&logo=embarcadero&label=&color=49c0c4)](https://github.com/processone/ejabberd/tags)
[![GitHub Container](https://img.shields.io/github/v/tag/processone/ejabberd?label=container&sort=semver)](https://github.com/processone/ejabberd/pkgs/container/ejabberd)
[![Docker Image Version (latest semver)](https://img.shields.io/docker/v/ejabberd/ecs?label=docker)](https://hub.docker.com/r/ejabberd/ecs/)


ejabberd Container
==================

[ejabberd][home] is an open-source,
robust, scalable and extensible realtime platform built using [Erlang/OTP][erlang],
that includes [XMPP][xmpp] Server, [MQTT][mqtt] Broker and [SIP][sip] Service.

[home]: https://ejabberd.im/
[erlang]: https://www.erlang.org/
[xmpp]: https://xmpp.org/
[mqtt]: https://mqtt.org/
[sip]: https://en.wikipedia.org/wiki/Session_Initiation_Protocol

This document explains how to use the
[ejabberd container images](https://github.com/processone/ejabberd/pkgs/container/ejabberd)
available in the GitHub Container Registry,
built using the files in `.github/container/`.

Alternatively, there are also
[ejabberd-ecs Docker images](https://hub.docker.com/r/ejabberd/ecs/)
available in Docker Hub,
built using the
[docker-ejabberd/ecs](https://github.com/processone/docker-ejabberd/tree/master/ecs)
repository.

If you are using a Windows operating system, check the tutorials mentioned in
[ejabberd Docs > Docker Image](https://docs.ejabberd.im/admin/installation/#docker-image).


Start ejabberd
--------------

### With default configuration

Start ejabberd in a new container:

```bash
docker run --name ejabberd -d -p 5222:5222 ghcr.io/processone/ejabberd
```

That runs the container as a daemon,
using ejabberd default configuration file and XMPP domain "localhost".

Stop the running container:

```bash
docker stop ejabberd
```

Restart the stopped ejabberd container:

```bash
docker restart ejabberd
```


### Start with Erlang console attached

Start ejabberd with an Erlang console attached using the `live` command:

```bash
docker run --name ejabberd -it -p 5222:5222 ghcr.io/processone/ejabberd live
```

That uses the default configuration file and XMPP domain "localhost".


### Start with your configuration and database

Pass a configuration file as a volume
and share the local directory to store database:

```bash
mkdir database
chown ejabberd database

cp ejabberd.yml.example ejabberd.yml

docker run --name ejabberd -it \
  -v $(pwd)/ejabberd.yml:/opt/ejabberd/conf/ejabberd.yml \
  -v $(pwd)/database:/opt/ejabberd/database \
  -p 5222:5222 ghcr.io/processone/ejabberd live
```

Notice that ejabberd runs in the container with an account named `ejabberd`,
and the volumes you mount must grant proper rights to that account.


Next steps
----------

### Register the administrator account

The default ejabberd configuration does not grant admin privileges
to any account,
you may want to register a new account in ejabberd
and grant it admin rights.

Register an account using the `ejabberdctl` script:

```bash
docker exec -it ejabberd ejabberdctl register admin localhost passw0rd
```

Then edit conf/ejabberd.yml and add the ACL as explained in
[ejabberd Docs: Administration Account](https://docs.ejabberd.im/admin/installation/#administration-account)


### Check ejabberd log files

Check the content of the log files inside the container,
even if you do not put it on a shared persistent drive:

```bash
docker exec -it ejabberd tail -f logs/ejabberd.log
```


### Inspect the container files

The container uses Alpine Linux. Start a shell inside the container:

```bash
docker exec -it ejabberd sh
```


### Open ejabberd debug console

Open an interactive debug Erlang console attached to a running ejabberd in a running container:

```bash
docker exec -it ejabberd ejabberdctl debug
```


### CAPTCHA

ejabberd includes two example CAPTCHA scripts.
If you want to use any of them, first install some additional required libraries:

```bash
docker exec --user root ejabberd apk add imagemagick ghostscript-fonts bash
```

Now update your ejabberd configuration file, for example:
```bash
docker exec -it ejabberd vi conf/ejabberd.yml
```

and add the required options:
```
captcha_cmd: /opt/ejabberd-22.04/lib/ejabberd-22.04/priv/bin/captcha.sh
captcha_url: https://localhost:5443/captcha
```

Finally, reload the configuration file or restart the container:
```bash
docker exec ejabberd ejabberdctl reload_config
```


Advanced Container Configuration
--------------------------------

### Ports

This container image exposes the ports:

- `5222`: The default port for XMPP clients.
- `5269`: For XMPP federation. Only needed if you want to communicate with users on other servers.
- `5280`: For admin interface.
- `5443`: With encryption, used for admin interface, API, CAPTCHA, OAuth, Websockets and XMPP BOSH.
- `1883`: Used for MQTT
- `4369-4399`: EPMD and Erlang connectivity, used for `ejabberdctl` and clustering


### Volumes

ejabberd produces two types of data: log files and database spool files (Mnesia).
This is the kind of data you probably want to store on a persistent or local drive (at least the database).

The volumes you may want to map:

- `/opt/ejabberd/conf/`: Directory containing configuration and certificates
- `/opt/ejabberd/database/`: Directory containing Mnesia database.
You should back up or export the content of the directory to persistent storage
(host storage, local storage, any storage plugin)
- `/opt/ejabberd/logs/`: Directory containing log files
- `/opt/ejabberd/upload/`: Directory containing uploaded files. This should also be backed up.

All these files are owned by `ejabberd` user inside the container.

It's possible to install additional ejabberd modules using volumes,
[this comment](https://github.com/processone/docker-ejabberd/issues/81#issuecomment-1036115146)
explains how to install an additional module using docker-compose.


### Commands on start

The ejabberdctl script reads the `CTL_ON_CREATE` environment variable
the first time the docker container is started,
and reads `CTL_ON_START` every time the container is started.
Those variables can contain one ejabberdctl command,
or several commands separated with the blankspace and `;` characters.

Example usage (see full example [docker-compose.yml](https://github.com/processone/docker-ejabberd/issues/64#issuecomment-887741332)):
```yaml
    environment:
      - CTL_ON_CREATE=register admin localhost asd
      - CTL_ON_START=stats registeredusers ;
                     check_password admin localhost asd ;
                     status
```


### Clustering

When setting several containers to form a
[cluster of ejabberd nodes](https://docs.ejabberd.im/admin/guide/clustering/),
each one must have a different
[Erlang Node Name](https://docs.ejabberd.im/admin/guide/security/#erlang-node-name)
and the same
[Erlang Cookie](https://docs.ejabberd.im/admin/guide/security/#erlang-cookie).

For this you can either:
- edit `conf/ejabberdctl.cfg` and set variables `ERLANG_NODE` and `ERLANG_COOKIE`
- set the environment variables `ERLANG_NODE_ARG` and `ERLANG_COOKIE`

Example using environment variables (see full example [docker-compose.yml](https://github.com/processone/docker-ejabberd/issues/64#issuecomment-887741332)):
```yaml
    environment:
      - ERLANG_NODE_ARG=ejabberd@node7
      - ERLANG_COOKIE=dummycookie123
```


Generating a Container Image
----------------------------

This container image includes ejabberd as a standalone OTP release built using Elixir.

That OTP release is configured with:

- `mix.exs`: Customize ejabberd release
- `vars.config`: ejabberd compilation configuration options
- `config/runtime.exs`: Customize ejabberd paths
- `ejabberd.yml.template`: ejabberd default config file

Build ejabberd Community Server base image from ejabberd master on GitHub:

```bash
VERSION=master
docker build \
    --build-arg VERSION=$VERSION \
    -t personal/ejabberd:$VERSION \
    .github/container
```

Build ejabberd Community Server base image for a given ejabberd version,
both for amd64 and arm64 architectures:

```bash
VERSION=22.05
docker buildx build \
    --platform=linux/amd64,linux/arm64
    --build-arg VERSION=$VERSION \
    -t personal/ejabberd:$VERSION \
    .github/container
```

It's also possible to use podman instead of docker, just notice:
- `EXPOSE 4369-4399` port range is not supported, remove that in Dockerfile
- It mentions that `healthcheck` is not supported by the Open Container Initiative image format
- If you want to start with command `live`, add environment variable `EJABBERD_BYPASS_WARNINGS=true`
```bash
VERSION=master
podman build \
    --build-arg VERSION=$VERSION \
    -t ja:$(version) \
    .github/container
```
