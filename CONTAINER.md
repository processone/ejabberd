
[![GitHub tag (latest SemVer)](https://img.shields.io/github/v/tag/processone/ejabberd?sort=semver&logo=embarcadero&label=&color=49c0c4)](https://github.com/processone/ejabberd/tags)
[![ejabberd Container on GitHub](https://img.shields.io/github/v/tag/processone/ejabberd?label=ejabberd&sort=semver&logo=docker)](https://github.com/processone/ejabberd/pkgs/container/ejabberd)
[![ecs Container on Docker](https://img.shields.io/docker/v/ejabberd/ecs?label=ecs&sort=semver&logo=docker)](https://hub.docker.com/r/ejabberd/ecs/)

`ejabberd` Container Image
==========================

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
This image is based in Alpine 3.19, includes Erlang/OTP 27.2 and Elixir 1.18.1.

Alternatively, there is also the `ecs` container image available in
[docker.io/ejabberd/ecs](https://hub.docker.com/r/ejabberd/ecs/),
built using the
[docker-ejabberd/ecs](https://github.com/processone/docker-ejabberd/tree/master/ecs)
repository.
Check the [differences between `ejabberd` and `ecs` images](https://github.com/processone/docker-ejabberd/blob/master/ecs/HUB-README.md#alternative-image-in-github).

If you are using a Windows operating system, check the tutorials mentioned in
[ejabberd Docs > Docker Image](https://docs.ejabberd.im/admin/install/container/#ejabberd-container-image).


Start ejabberd
--------------

### With default configuration

Start ejabberd in a new container:

```bash
docker run --name ejabberd -d -p 5222:5222 ghcr.io/processone/ejabberd
```

That runs the container as a daemon,
using ejabberd default configuration file and XMPP domain `localhost`.

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

That uses the default configuration file and XMPP domain `localhost`.


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
[ejabberd Docs: Administration Account](https://docs.ejabberd.im/admin/install/next-steps/#administration-account)


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
--------------------------------

### Ports

This container image exposes the ports:

- `5222`: The default port for XMPP clients.
- `5269`: For XMPP federation. Only needed if you want to communicate with users on other servers.
- `5280`: For admin interface.
- `5443`: With encryption, used for admin interface, API, CAPTCHA, OAuth, Websockets and XMPP BOSH.
- `1883`: Used for MQTT
- `4369-4399`: EPMD and Erlang connectivity, used for `ejabberdctl` and clustering
- `5210`: Erlang connectivity when `ERL_DIST_PORT` is set, alternative to EPMD


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
the first time the container is started,
and reads `CTL_ON_START` every time the container is started.
Those variables can contain one ejabberdctl command,
or several commands separated with the blankspace and `;` characters.

By default failure of any of commands executed that way would
abort start, this can be disabled by prefixing commands with `!`

Example usage (or check the [full example](#customized-example)):
```yaml
    environment:
      - CTL_ON_CREATE=! register admin localhost asd
      - CTL_ON_START=stats registeredusers ;
                     check_password admin localhost asd ;
                     status
```


### Macros in environment

ejabberd reads `EJABBERD_MACRO_*` environment variables
and uses them to define the `*`
[macros](https://docs.ejabberd.im/admin/configuration/file-format/#macros-in-configuration-file),
overwriting the corresponding macro definition if it was set in the configuration file.
This is supported since ejabberd 24.12.

For example, if you configure this in `ejabberd.yml`:

```yaml
acl:
  admin:
    user: ADMINJID
```

now you can define the admin account JID using an environment variable:
```yaml
    environment:
      - EJABBERD_MACRO_ADMINJID=admin@localhost
```

Check the [full example](#customized-example) for other example.


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

Example to connect a local `ejabberdctl` to a containerized ejabberd:

1. When creating the container, export port 5210, and set `ERLANG_COOKIE`:
    ```sh
    docker run --name ejabberd -it \
      -e ERLANG_COOKIE=`cat $HOME/.erlang.cookie` \
      -p 5210:5210 -p 5222:5222 \
      ghcr.io/processone/ejabberd
    ```
2. Set `ERL_DIST_PORT=5210` in ejabberdctl.cfg of container and local ejabberd
3. Restart the container
4. Now use `ejabberdctl` in your local ejabberd deployment

To connect using a local `ejabberd` script:
```sh
ERL_DIST_PORT=5210 _build/dev/rel/ejabberd/bin/ejabberd ping
```

Example using environment variables (see full example [docker-compose.yml](https://github.com/processone/docker-ejabberd/issues/64#issuecomment-887741332)):
```yaml
    environment:
      - ERLANG_NODE_ARG=ejabberd@node7
      - ERLANG_COOKIE=dummycookie123
```


Build a Container Image
-----------------------

This container image includes ejabberd as a standalone OTP release built using Elixir.
That OTP release is configured with:

- `mix.exs`: Customize ejabberd release
- `vars.config`: ejabberd compilation configuration options
- `config/runtime.exs`: Customize ejabberd paths
- `ejabberd.yml.template`: ejabberd default config file

### Direct build

Build ejabberd Community Server container image from ejabberd master git repository:

```bash
docker buildx build \
    -t personal/ejabberd \
    -f .github/container/Dockerfile \
    .
```

### Podman build

To build the image using Podman, please notice:

- `EXPOSE 4369-4399` port range is not supported, remove that in Dockerfile
- It mentions that `healthcheck` is not supported by the Open Container Initiative image format
- to start with command `live`, you may want to add environment variable `EJABBERD_BYPASS_WARNINGS=true`

```bash
podman build \
    -t ejabberd \
    -f .github/container/Dockerfile \
    .

podman run --name eja1 -d -p 5222:5222 localhost/ejabberd

podman exec eja1 ejabberdctl status

podman exec -it eja1 sh

podman stop eja1

podman run --name eja1 -it -e EJABBERD_BYPASS_WARNINGS=true -p 5222:5222 localhost/ejabberd live
```

### Package build for `arm64`

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
-----------------

### Minimal Example

This is the barely minimal file to get a usable ejabberd.

If using Docker, write this `docker-compose.yml` file
and start it with `docker-compose up`:

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

If using Podman, write this `minimal.yml` file
and start it with `podman kube play minimal.yml`:

```yaml
apiVersion: v1

kind: Pod

metadata:
  name: ejabberd

spec:
  containers:

  - name: ejabberd
    image: ghcr.io/processone/ejabberd
    ports:
    - containerPort: 5222
      hostPort: 5222
    - containerPort: 5269
      hostPort: 5269
    - containerPort: 5280
      hostPort: 5280
    - containerPort: 5443
      hostPort: 5443
```


### Customized Example

This example shows the usage of several customizations:
it uses a local configuration file,
defines a configuration macro using an environment variable,
stores the mnesia database in a local path,
registers an account when it's created,
and checks the number of registered accounts every time it's started.

Download or copy the ejabberd configuration file:
```bash
wget https://raw.githubusercontent.com/processone/ejabberd/master/ejabberd.yml.example
mv ejabberd.yml.example ejabberd.yml
```

Use a macro in `ejabberd.yml` to set the served vhost, with `localhost` as default value:
```yaml
define_macro:
  XMPPHOST: localhost

hosts:
  - XMPPHOST
```

Create the database directory and allow the container access to it:
```bash
mkdir database
sudo chown 9000:9000 database
```

If using Docker, write this `docker-compose.yml` file
and start it with `docker-compose up`:

```yaml
version: '3.7'

services:

  main:
    image: ghcr.io/processone/ejabberd
    container_name: ejabberd
    environment:
      - EJABBERD_MACRO_XMPPHOST=example.com
      - CTL_ON_CREATE=register admin example.com asd
      - CTL_ON_START=registered_users example.com ;
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

If using Podman, write this `custom.yml` file
and start it with `podman kube play custom.yml`:

```yaml
apiVersion: v1

kind: Pod

metadata:
  name: ejabberd

spec:
  containers:

  - name: ejabberd
    image: ghcr.io/processone/ejabberd
    env:
    - name: CTL_ON_CREATE
      value: register admin example.com asd
    - name: CTL_ON_START
      value: registered_users example.com ;
             status
    ports:
    - containerPort: 5222
      hostPort: 5222
    - containerPort: 5269
      hostPort: 5269
    - containerPort: 5280
      hostPort: 5280
    - containerPort: 5443
      hostPort: 5443
    volumeMounts:
    - mountPath: /opt/ejabberd/conf/ejabberd.yml
      name: config
      readOnly: true
    - mountPath: /opt/ejabberd/database
      name: db

  volumes:
  - name: config
    hostPath:
      path: ./ejabberd.yml
      type: File
  - name: db
    hostPath:
      path: ./database
      type: DirectoryOrCreate
```


### Clustering Example

In this example, the main container is created first.
Once it is fully started and healthy, a second container is created,
and once ejabberd is started in it, it joins the first one.

An account is registered in the first node when created (and
we ignore errors that can happen when doing that - for example
when account already exists),
and it should exist in the second node after join.

Notice that in this example the main container does not have access
to the exterior; the replica exports the ports and can be accessed.

If using Docker, write this `docker-compose.yml` file
and start it with `docker-compose up`:

```yaml
version: '3.7'

services:

  main:
    image: ghcr.io/processone/ejabberd
    container_name: ejabberd
    environment:
      - ERLANG_NODE_ARG=ejabberd@main
      - ERLANG_COOKIE=dummycookie123
      - CTL_ON_CREATE=! register admin localhost asd

  replica:
    image: ghcr.io/processone/ejabberd
    container_name: replica
    depends_on:
      main:
        condition: service_healthy
    environment:
      - ERLANG_NODE_ARG=ejabberd@replica
      - ERLANG_COOKIE=dummycookie123
      - CTL_ON_CREATE=join_cluster ejabberd@main
      - CTL_ON_START=registered_users localhost ;
                     status
    ports:
      - "5222:5222"
      - "5269:5269"
      - "5280:5280"
      - "5443:5443"
```

If using Podman, write this `cluster.yml` file
and start it with `podman kube play cluster.yml`:

```yaml
apiVersion: v1

kind: Pod

metadata:
  name: cluster

spec:
  containers:

  - name: first
    image: ghcr.io/processone/ejabberd
    env:
    - name: ERLANG_NODE_ARG
      value: main@cluster
    - name: ERLANG_COOKIE
      value: dummycookie123
    - name: CTL_ON_CREATE
      value: register admin localhost asd
    - name: CTL_ON_START
      value: stats registeredusers ;
             status
    - name: EJABBERD_MACRO_PORT_C2S
      value: 6222
    - name: EJABBERD_MACRO_PORT_C2S_TLS
      value: 6223
    - name: EJABBERD_MACRO_PORT_S2S
      value: 6269
    - name: EJABBERD_MACRO_PORT_HTTP_TLS
      value: 6443
    - name: EJABBERD_MACRO_PORT_HTTP
      value: 6280
    - name: EJABBERD_MACRO_PORT_MQTT
      value: 6883
    - name: EJABBERD_MACRO_PORT_PROXY65
      value: 6777
    volumeMounts:
    - mountPath: /opt/ejabberd/conf/ejabberd.yml
      name: config
      readOnly: true

  - name: second
    image: ghcr.io/processone/ejabberd
    env:
    - name: ERLANG_NODE_ARG
      value: replica@cluster
    - name: ERLANG_COOKIE
      value: dummycookie123
    - name: CTL_ON_CREATE
      value: join_cluster main@cluster ;
             started ;
             list_cluster
    - name: CTL_ON_START
      value: stats registeredusers ;
             check_password admin localhost asd ;
             status
    ports:
    - containerPort: 5222
      hostPort: 5222
    - containerPort: 5280
      hostPort: 5280
    volumeMounts:
    - mountPath: /opt/ejabberd/conf/ejabberd.yml
      name: config
      readOnly: true

  volumes:
  - name: config
    hostPath:
      path: ./conf/ejabberd.yml
      type: File

```

Your configuration file should use those macros to allow each ejabberd node
use different listening port numbers:

```diff
diff --git a/ejabberd.yml.example b/ejabberd.yml.example
index 39e423a64..6e875b48f 100644
--- a/ejabberd.yml.example
+++ b/ejabberd.yml.example
@@ -24,9 +24,19 @@ loglevel: info
 #  - /etc/letsencrypt/live/domain.tld/fullchain.pem
 #  - /etc/letsencrypt/live/domain.tld/privkey.pem
 
+define_macro:
+  PORT_C2S: 5222
+  PORT_C2S_TLS: 5223
+  PORT_S2S: 5269
+  PORT_HTTP_TLS: 5443
+  PORT_HTTP: 5280
+  PORT_STUN: 5478
+  PORT_MQTT: 1883
+  PORT_PROXY65: 7777
+
 listen:
   -
-    port: 5222
+    port: PORT_C2S
     ip: "::"
     module: ejabberd_c2s
     max_stanza_size: 262144
@@ -34,7 +44,7 @@ listen:
     access: c2s
     starttls_required: true
   -
-    port: 5223
+    port: PORT_C2S_TLS
     ip: "::"
     module: ejabberd_c2s
     max_stanza_size: 262144
@@ -42,13 +52,13 @@ listen:
     access: c2s
     tls: true
   -
-    port: 5269
+    port: PORT_S2S
     ip: "::"
     module: ejabberd_s2s_in
     max_stanza_size: 524288
     shaper: s2s_shaper
   -
-    port: 5443
+    port: PORT_HTTP_TLS
     ip: "::"
     module: ejabberd_http
     tls: true
@@ -60,14 +70,14 @@ listen:
       /upload: mod_http_upload
       /ws: ejabberd_http_ws
   -
-    port: 5280
+    port: PORT_HTTP
     ip: "::"
     module: ejabberd_http
     request_handlers:
       /admin: ejabberd_web_admin
       /.well-known/acme-challenge: ejabberd_acme
   -
-    port: 5478
+    port: PORT_STUN
     ip: "::"
     transport: udp
     module: ejabberd_stun
@@ -77,7 +87,7 @@ listen:
     ## The server's public IPv6 address:
     # turn_ipv6_address: "2001:db8::3"
   -
-    port: 1883
+    port: PORT_MQTT
     ip: "::"
     module: mod_mqtt
     backlog: 1000
@@ -207,6 +217,7 @@ modules:
   mod_proxy65:
     access: local
     max_connections: 5
+    port: PORT_PROXY65
   mod_pubsub:
     access_createnode: pubsub_createnode
     plugins:
```
