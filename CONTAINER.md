
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

This document explains how to use the `ejabberd` container image available in
[ghcr.io/processone/ejabberd](https://github.com/processone/ejabberd/pkgs/container/ejabberd),
built using the files in `.github/container/`.

Alternatively, there is also the `ecs` container image available in
[docker.io/ejabberd/ecs](https://hub.docker.com/r/ejabberd/ecs/),
built using the
[docker-ejabberd/ecs](https://github.com/processone/docker-ejabberd/tree/master/ecs)
repository.
Check the [differences between `ejabberd` and `ecs` images](https://github.com/processone/docker-ejabberd/blob/master/ecs/HUB-README.md#alternative-image-in-github).

If you are using a Windows operating system, check the tutorials mentioned in
[ejabberd Docs > Docker Image](https://docs.ejabberd.im/admin/installation/#docker-image).


Start ejabberd
==============

## With default configuration

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


## Start with Erlang console attached

Start ejabberd with an Erlang console attached using the `live` command:

```bash
docker run --name ejabberd -it -p 5222:5222 ghcr.io/processone/ejabberd live
```

That uses the default configuration file and XMPP domain "localhost".


## Start with your configuration and database

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
==========

## Register the administrator account

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


## Check ejabberd log files

Check the content of the log files inside the container,
even if you do not put it on a shared persistent drive:

```bash
docker exec -it ejabberd tail -f logs/ejabberd.log
```


## Inspect the container files

The container uses Alpine Linux. Start a shell inside the container:

```bash
docker exec -it ejabberd sh
```


## Open ejabberd debug console

Open an interactive debug Erlang console attached to a running ejabberd in a running container:

```bash
docker exec -it ejabberd ejabberdctl debug
```


## CAPTCHA

ejabberd includes two example CAPTCHA scripts.
If you want to use any of them, first install some additional required libraries:

```bash
docker exec --user root ejabberd apk add imagemagick ghostscript-fonts bash
```

Now update your ejabberd configuration file, for example:
```bash
docker exec -it ejabberd vi conf/ejabberd.yml
```

and add this option:
```yaml
captcha_cmd: /opt/ejabberd-22.04/lib/captcha.sh
```

Finally, reload the configuration file or restart the container:
```bash
docker exec ejabberd ejabberdctl reload_config
```

If the CAPTCHA image is not visible, there may be a problem generating it
(the ejabberd log file may show some error message);
or the image URL may not be correctly detected by ejabberd,
in that case you can set the correct URL manually, for example:
```yaml
captcha_url: https://localhost:5443/captcha
```

For more details about CAPTCHA options, please check the
[CAPTCHA](https://docs.ejabberd.im/admin/configuration/basic/#captcha)
documentation section.


Advanced Container Configuration
================================

## Ports

This container image exposes the ports:

- `5222`: The default port for XMPP clients.
- `5269`: For XMPP federation. Only needed if you want to communicate with users on other servers.
- `5280`: For admin interface.
- `5443`: With encryption, used for admin interface, API, CAPTCHA, OAuth, Websockets and XMPP BOSH.
- `1883`: Used for MQTT
- `4369-4399`: EPMD and Erlang connectivity, used for `ejabberdctl` and clustering
- `5210`: Erlang connectivity when `ERL_DIST_PORT` is set, alternative to EPMD


## Volumes

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


## Commands on start

The ejabberdctl script reads the `CTL_ON_CREATE` environment variable
the first time the container is started,
and reads `CTL_ON_START` every time the container is started.
Those variables can contain one ejabberdctl command,
or several commands separated with the blankspace and `;` characters.

Example usage (or check the [full example](#customized-example)):
```yaml
    environment:
      - CTL_ON_CREATE=register admin localhost asd
      - CTL_ON_START=stats registeredusers ;
                     check_password admin localhost asd ;
                     status
```


## Clustering

When setting several containers to form a
[cluster of ejabberd nodes](https://docs.ejabberd.im/admin/guide/clustering/),
each one must have a different
[Erlang Node Name](https://docs.ejabberd.im/admin/guide/security/#erlang-node-name)
and the same
[Erlang Cookie](https://docs.ejabberd.im/admin/guide/security/#erlang-cookie).

For this you can either:
- edit `conf/ejabberdctl.cfg` and set variables `ERLANG_NODE` and `ERLANG_COOKIE`
- set the environment variables `ERLANG_NODE_ARG` and `ERLANG_COOKIE`

Example to connect a local `ejabberdctl` to a containerized ejabberd:
1. When creating the container, export port 5210, and set `ERLANG_COOKIE`:
```
docker run --name ejabberd -it \
  -e ERLANG_COOKIE=`cat $HOME/.erlang.cookie` \
  -p 5210:5210 -p 5222:5222 \
  ghcr.io/processone/ejabberd
```
2. Set `ERL_DIST_PORT=5210` in ejabberdctl.cfg of container and local ejabberd
3. Restart the container
4. Now use `ejabberdctl` in your local ejabberd deployment

To connect using a local `ejabberd` script:
```
ERL_DIST_PORT=5210 _build/dev/rel/ejabberd/bin/ejabberd ping
```

Example using environment variables (see full example [docker-compose.yml](https://github.com/processone/docker-ejabberd/issues/64#issuecomment-887741332)):
```yaml
    environment:
      - ERLANG_NODE_ARG=ejabberd@node7
      - ERLANG_COOKIE=dummycookie123
```


Build a Container Image
=======================

This container image includes ejabberd as a standalone OTP release built using Elixir.
That OTP release is configured with:

- `mix.exs`: Customize ejabberd release
- `vars.config`: ejabberd compilation configuration options
- `config/runtime.exs`: Customize ejabberd paths
- `ejabberd.yml.template`: ejabberd default config file

## Direct build

Build ejabberd Community Server container image from ejabberd master git repository:

```bash
docker buildx build \
    -t personal/ejabberd \
    -f .github/container/Dockerfile \
    .
```

## Podman build

It's also possible to use podman instead of docker, just notice:
- `EXPOSE 4369-4399` port range is not supported, remove that in Dockerfile
- It mentions that `healthcheck` is not supported by the Open Container Initiative image format
- If you want to start with command `live`, add environment variable `EJABBERD_BYPASS_WARNINGS=true`
```bash
podman build \
    -t ejabberd \
    -f .github/container/Dockerfile \
    .

podman run --name eja1 -d -p 5222:5222 localhost/ejabberd

podman exec eja1 ejabberdctl status

podman exec -it eja1 sh

podman stop eja1
```

## Package build for `arm64`

By default, `.github/container/Dockerfile` builds this container by directly compiling ejabberd,
it is a fast and direct method.
However, a problem with QEMU prevents building the container in QEMU using Erlang/OTP 25
for the `arm64` architecture.

Providing `--build-arg METHOD=package` is an alternate method to build the container
used by the Github Actions workflow that provides `amd64` and `arm64` container images.
It first builds an ejabberd binary package, and later installs it in the image.
That method avoids using QEMU, so it can build `arm64` container images, but is extremely
slow the first time it's used, and consequently not recommended for general use.

In this case, to build the ejabberd container image for arm64 architecture:

```bash
docker buildx build \
    --build-arg METHOD=package \
    --platform linux/arm64 \
    -t personal/ejabberd:$VERSION \
    -f .github/container/Dockerfile \
    .
```


Composer Examples
=================

## Minimal Example

This is the barely minimal file to get a usable ejabberd.
Store it as `docker-compose.yml`:

```yaml
services:
  main:
    image: ghcr.io/processone/ejabberd
    container_name: ejabberd
    ports:
      - "5222:5222"
      - "5269:5269"
      - "5280:5280"
      - "5443:5443"
```

Create and start the container with the command:
```bash
docker-compose up
```

## Customized Example

This example shows the usage of several customizations:
it uses a local configuration file,
stores the mnesia database in a local path,
registers an account when it's created,
and checks the number of registered accounts every time it's started.

Download or copy the ejabberd configuration file:
```bash
wget https://raw.githubusercontent.com/processone/ejabberd/master/ejabberd.yml.example
mv ejabberd.yml.example ejabberd.yml
```

Create the database directory and allow the container access to it:
```bash
mkdir database
sudo chown 9000:9000 database
```

Now write this `docker-compose.yml` file:
```yaml
version: '3.7'

services:

  main:
    image: ghcr.io/processone/ejabberd
    container_name: ejabberd
    environment:
      - CTL_ON_CREATE=register admin localhost asd
      - CTL_ON_START=registered_users localhost ;
                     status
    ports:
      - "5222:5222"
      - "5269:5269"
      - "5280:5280"
      - "5443:5443"
    volumes:
      - ./ejabberd.yml:/opt/ejabberd/conf/ejabberd.yml:ro
      - ./database:/opt/ejabberd/database
```

## Clustering Example

In this example, the main container is created first.
Once it is fully started and healthy, a second container is created,
and once ejabberd is started in it, it joins the first one.

An account is registered in the first node when created,
and it should exist in the second node after join.

Notice that in this example the main container does not have access
to the exterior; the replica exports the ports and can be accessed.

```yaml
version: '3.7'

services:

  main:
    image: ghcr.io/processone/ejabberd
    container_name: ejabberd
    environment:
      - ERLANG_NODE_ARG=ejabberd@main
      - ERLANG_COOKIE=dummycookie123
      - CTL_ON_CREATE=register admin localhost asd

  replica:
    image: ghcr.io/processone/ejabberd
    container_name: replica
    depends_on:
      main:
        condition: service_healthy
    ports:
      - "5222:5222"
      - "5269:5269"
      - "5280:5280"
      - "5443:5443"
    environment:
      - ERLANG_NODE_ARG=ejabberd@replica
      - ERLANG_COOKIE=dummycookie123
      - CTL_ON_CREATE=join_cluster ejabberd@main
      - CTL_ON_START=registered_users localhost ;
                     status
```
