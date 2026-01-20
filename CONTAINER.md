
[![GitHub tag (latest SemVer)](https://img.shields.io/github/v/tag/processone/ejabberd?sort=semver&logo=embarcadero&label=&color=49c0c4)](https://github.com/processone/ejabberd/tags)
[![ejabberd Container on GitHub](https://img.shields.io/github/v/tag/processone/ejabberd?label=ejabberd&sort=semver&logo=opencontainersinitiative&logoColor=2094f3)](https://github.com/processone/ejabberd/pkgs/container/ejabberd)
[![ecs Container on Docker](https://img.shields.io/docker/v/ejabberd/ecs?label=ecs&sort=semver&logo=docker)](https://hub.docker.com/r/ejabberd/ecs/)

ejabberd Container Images
=========================

[ejabberd][home] is an open-source,
robust, scalable and extensible realtime platform built using [Erlang/OTP][erlang],
that includes [XMPP][xmpp] Server, [MQTT][mqtt] Broker and [SIP][sip] Service.

[home]: https://www.ejabberd.im/
[erlang]: https://www.erlang.org/
[xmpp]: https://xmpp.org/
[mqtt]: https://mqtt.org/
[sip]: https://en.wikipedia.org/wiki/Session_Initiation_Protocol

This page documents those container images ([images comparison](#images-comparison)):

- [![ejabberd Container](https://img.shields.io/badge/ejabberd-grey?logo=opencontainersinitiative&logoColor=2094f3)](https://github.com/processone/ejabberd/pkgs/container/ejabberd)  
  published in [ghcr.io/processone/ejabberd](https://github.com/processone/ejabberd/pkgs/container/ejabberd),
  built using [ejabberd](https://github.com/processone/ejabberd/tree/master/.github/container) repository,
  both for stable ejabberd releases and the `master` branch, in x64 and arm64 architectures.

- [![ecs Container](https://img.shields.io/badge/ecs-grey?logo=docker&logoColor=2094f3)](https://hub.docker.com/r/ejabberd/ecs/)  
  published in [docker.io/ejabberd/ecs](https://hub.docker.com/r/ejabberd/ecs/),
  built using [docker-ejabberd/ecs](https://github.com/processone/docker-ejabberd/tree/master/ecs) repository
  for ejabberd stable releases in x64 architectures.

For Microsoft Windows, see
[Docker Desktop for Windows 10](https://www.process-one.net/blog/install-ejabberd-on-windows-10-using-docker-desktop/),
and [Docker Toolbox for Windows 7](https://www.process-one.net/blog/install-ejabberd-on-windows-7-using-docker-toolbox/).

For Kubernetes Helm, see [help-ejabberd](https://github.com/sando38/helm-ejabberd).


Start ejabberd
--------------

### daemon

Start ejabberd in a new container:

```bash
docker run --name ejabberd -d -p 5222:5222 ghcr.io/processone/ejabberd
```

That runs the container as a daemon,
using ejabberd default configuration file and XMPP domain `localhost`.

Restart the stopped ejabberd container:

```bash
docker restart ejabberd
```

Stop the running container:

```bash
docker stop ejabberd
```

Remove the ejabberd container:

```bash
docker rm ejabberd
```


### with Erlang console

Start ejabberd with an interactive Erlang console attached using the `live` command:

```bash
docker run --name ejabberd -it -p 5222:5222 ghcr.io/processone/ejabberd live
```

That uses the default configuration file and XMPP domain `localhost`.


### with your data

Pass a configuration file as a volume
and share the local directory to store database:

```bash
mkdir conf && cp ejabberd.yml.example conf/ejabberd.yml

mkdir database && chown ejabberd database

docker run --name ejabberd -it \
  -v $(pwd)/conf/ejabberd.yml:/opt/ejabberd/conf/ejabberd.yml \
  -v $(pwd)/database:/opt/ejabberd/database \
  -p 5222:5222 ghcr.io/processone/ejabberd live
```

Notice that ejabberd runs in the container with an account named `ejabberd`
with UID 9000 and group `ejabberd` with GID 9000,
and the volumes you mount must grant proper rights to that account.


Next steps
----------

### Register admin account

#### [![ejabberd Container](https://img.shields.io/badge/ejabberd-grey?logo=opencontainersinitiative&logoColor=2094f3)](https://github.com/processone/ejabberd/pkgs/container/ejabberd) [🔅](#images-comparison)

If you set the `REGISTER_ADMIN_PASSWORD` environment variable,
an account is automatically registered with that password,
and admin privileges are granted to it.
The account created depends on what variables you have set:

- `EJABBERD_MACRO_ADMIN=juliet@example.org` -> `juliet@example.org`
- `EJABBERD_MACRO_HOST=example.org` -> `admin@example.org`
- None of those variables are set -> `admin@localhost`

The account registration is shown in the container log:

```bash
$ podman run -it \
  --env EJABBERD_MACRO_HOST=example.org \
  --env EJABBERD_MACRO_ADMIN=juliet@example.org \
  --env REGISTER_ADMIN_PASSWORD=somePassw0rd \
  ghcr.io/processone/ejabberd

:> ejabberdctl register juliet example.org somePassw0rd
User juliet@example.org successfully registered
```

This is implemented internally by using
[Commands on start](#commands-on-start).

Alternatively, you can register the account manually yourself
and edit `conf/ejabberd.yml` and add the ACL as explained in
[ejabberd Docs: Administration Account](https://docs.ejabberd.im/admin/install/next-steps/#administration-account).

---

#### [![ecs Container](https://img.shields.io/badge/ecs-grey?logo=docker&logoColor=2094f3)](https://hub.docker.com/r/ejabberd/ecs/)

The default ejabberd configuration has already granted admin privilege
to an account that would be called `admin@localhost`,
so you just need to register it, for example:

```bash
docker exec -it ejabberd ejabberdctl register admin localhost passw0rd
```

### Check ejabberd log

Check the content of the log files inside the container,
even if you do not put it on a shared persistent drive:

```bash
docker exec -it ejabberd tail -f logs/ejabberd.log
```


### Inspect container files

The container uses Alpine Linux. Start a shell inside the container:

```bash
docker exec -it ejabberd sh
```


### Open debug console

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
captcha_cmd: "$HOME/bin/captcha.sh"
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


Advanced
--------

### Ports 🟠
The container image exposes several ports
(check also [Docs: Firewall Settings](https://docs.ejabberd.im/admin/guide/security/#firewall-settings)):

- `5222`: The default port for XMPP clients.
- `5269`: For XMPP federation. Only needed if you want to communicate with users on other servers.
- `5280`: For admin interface (URL is `admin/`).
- `5443`: With encryption, used for admin interface, API, CAPTCHA, OAuth, Websockets and XMPP BOSH.
- `1880`: For admin interface (URL is `/`, useful for [podman-desktop](https://podman-desktop.io/) and [docker-desktop](https://www.docker.com/products/docker-desktop/)) [🔅](#images-comparison)
- `1883`: Used for MQTT
- `5478` UDP: STUN service 🟠
- `50000-50099` UDP: TURN service 🟠
- `7777`: SOCKS5 file transfer proxy 🟠
- `5210`: Erlang connectivity when `ERL_DIST_PORT` is set, alternative to EPMD [🔅](#images-comparison)
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

All these files are owned by an account named `ejabberd` with group `ejabberd` in the container.
Its corresponding `UID:GID` is `9000:9000`.
If you prefer bind mounts instead of volumes, then
you need to map this to valid `UID:GID` on your host to get read/write access on
mounted directories.

If using Docker, try:
```bash
mkdir database
sudo chown 9000:9000 database
```

If using Podman, try:
```bash
mkdir database
podman unshare chown 9000:9000 database
```

It's possible to install additional ejabberd modules using volumes, check
[this Docs tutorial](https://docs.ejabberd.im/developer/extending-ejabberd/modules/#your-module-in-ejabberd-modules-with-ejabberd-container).


### Commands on start

The ejabberdctl script reads the `CTL_ON_CREATE` environment variable
the first time the container is started,
and reads `CTL_ON_START` every time the container is started.
Those variables can contain one ejabberdctl command,
or several commands separated with the blankspace and `;` characters.

If any of those commands returns a failure, the container starting gets aborted.
If there is a command with a result that can be ignored,
prefix that command with `!`

All this works when starting ejabberd with the default method `foreground`,
not when using `live`, `iexlive`, ...

This example registers an `admin@localhost` account when the container is first created.
Everytime the container starts, it shows the list of registered accounts,
checks that the admin account exists and password is valid,
changes the password of an account if it exists (ignoring any failure),
and shows the ejabberd starts (check also the [full example](#customized-example)):
```yaml
    environment:
      - CTL_ON_CREATE=register admin localhost asd
      - CTL_ON_START=stats registeredusers ;
                     check_password admin localhost asd ;
                     ! change_password bot123 localhost qqq ;
                     status
```

Same example using Podman:
```bash
$ podman run -it \
  --env CTL_ON_CREATE="register admin localhost asd" \
  --env CTL_ON_START="stats registeredusers ; \
                      check_password admin localhost asd ; \
                      ! change_password bot123 localhost qqq ; \
                      status" \
  ghcr.io/processone/ejabberd

...

:> ejabberdctl register admin localhost asd
User admin@localhost successfully registered

:> ejabberdctl stats registeredusers
1

:> ejabberdctl check_password admin localhost asd

:> ejabberdctl change_password bot123 localhost qqq
{not_found,"unknown_user"}
:> FAILURE in command 'change_password bot123 localhost qqq' !!! Ignoring result

:> ejabberdctl status
The node ejabberd@localhost is started. Status: started
ejabberd 25.10.0 is running in that node
```


### Macros in environment

ejabberd reads `EJABBERD_MACRO_*` environment variables
and uses them to define the corresponding
[macros](https://docs.ejabberd.im/admin/configuration/file-format/#macros-in-configuration-file),
overwriting the corresponding macro definition if it was set in the configuration file.
This is supported since ejabberd 24.12.

For example, if you configure this in `ejabberd.yml`:

```yaml
acl:
  admin:
    user: ADMIN
```

now you can define the admin account JID using an environment variable:
```yaml
    environment:
      - EJABBERD_MACRO_ADMIN=admin@localhost
```

Check the [full example](#customized-example) for other example.


### ejabberd-contrib

This section addresses those topics related to
[ejabberd-contrib](https://docs.ejabberd.im/admin/guide/modules/#ejabberd-contrib):

- [Download source code](#download-source-code)
- [Install a module](#install-a-module)
- [Install git for dependencies](#install-git-for-dependencies)
- [Install your module](#install-your-module)

---

#### Download source code

The `ejabberd` container image includes the ejabberd-contrib git repository source code,
but `ecs` does not, so first download it:
```bash
$ docker exec ejabberd ejabberdctl modules_update_specs
```

#### Install a module

Compile and install any of the contributed modules, for example:
```bash
docker exec ejabberd ejabberdctl module_install mod_statsdx

Module mod_statsdx has been installed and started.
It's configured in the file:
  /opt/ejabberd/.ejabberd-modules/mod_statsdx/conf/mod_statsdx.yml
Configure the module in that file, or remove it
and configure in your main ejabberd.yml
```

#### Install git for dependencies

Some modules depend on erlang libraries,
but the container images do not include `git` or `mix` to download them.
Consequently, when you attempt to install such a module,
there will be error messages like:

```bash
docker exec ejabberd ejabberdctl module_install ejabberd_observer_cli

I'll download "recon" using git because I can't use Mix to fetch from hex.pm:
  /bin/sh: mix: not found
Fetching dependency observer_cli:
  /bin/sh: git: not found
...
```

the solution is to install `git` in the container image:

```bash
docker exec --user root ejabberd apk add git

fetch https://dl-cdn.alpinelinux.org/alpine/v3.21/main/x86_64/APKINDEX.tar.gz
fetch https://dl-cdn.alpinelinux.org/alpine/v3.21/community/x86_64/APKINDEX.tar.gz
(1/3) Installing pcre2 (10.43-r0)
(2/3) Installing git (2.47.2-r0)
(3/3) Installing git-init-template (2.47.2-r0)
Executing busybox-1.37.0-r12.trigger
OK: 27 MiB in 42 packages
```

and now you can upgrade the module:

```bash
docker exec ejabberd ejabberdctl module_upgrade ejabberd_observer_cli

I'll download "recon" using git because I can't use Mix to fetch from hex.pm:
/bin/sh: mix: not found
Fetching dependency observer_cli: Cloning into 'observer_cli'...
Fetching dependency os_stats: Cloning into 'os_stats'...
Fetching dependency recon: Cloning into 'recon'...
Inlining: inline_size=24 inline_effort=150
Old inliner: threshold=0 functions=[{insert,2},{merge,2}]
Module ejabberd_observer_cli has been installed.
Now you can configure it in your ejabberd.yml
I'll download "recon" using git because I can't use Mix to fetch from hex.pm:
/bin/sh: mix: not found
```

#### Install your module

If you [developed an ejabberd module](https://docs.ejabberd.im/developer/extending-ejabberd/modules/),
you can install it in your container image:

1. Create a local directory for `ejabberd-modules`:

    ``` sh
    mkdir docker-modules
    ```

2. Then create the directory structure for your custom module:

    ``` sh
    cd docker-modules

    mkdir -p sources/mod_hello_world/
    touch sources/mod_hello_world/mod_hello_world.spec

    mkdir sources/mod_hello_world/src/
    mv mod_hello_world.erl sources/mod_hello_world/src/

    mkdir sources/mod_hello_world/conf/
    echo -e "modules:\n  mod_hello_world: {}" > sources/mod_hello_world/conf/mod_hello_world.yml

    cd ..
    ```

3. Grant ownership of that directory to the UID that ejabberd will use inside the Docker image:

    ``` sh
    sudo chown 9000 -R docker-modules/
    ```

4. Start ejabberd in the container:

    ``` sh
    sudo docker run \
      --name hellotest \
      -d \
      --volume "$(pwd)/docker-modules:/home/ejabberd/.ejabberd-modules/" \
      -p 5222:5222 \
      -p 5280:5280 \
      ejabberd/ecs
    ```

5. Check the module is available for installing, and then install it:

    ``` sh
    sudo docker exec -it hellotest ejabberdctl modules_available
    mod_hello_world []

    sudo docker exec -it hellotest ejabberdctl module_install mod_hello_world
    ```

6. If the module works correctly, you will see `Hello` in the ejabberd logs when it starts:

    ``` sh
    sudo docker exec -it hellotest grep Hello logs/ejabberd.log
    2020-10-06 13:40:13.154335+00:00 [info]
      <0.492.0>@mod_hello_world:start/2:15 Hello, ejabberd world!
    ```


### ejabberdapi

When the container is running (and thus ejabberd), you can exec commands inside the container
using `ejabberdctl` or any other of the available interfaces, see
[Understanding ejabberd "commands"](https://docs.ejabberd.im/developer/ejabberd-api/#understanding-ejabberd-commands)

Additionally, the container image includes the `ejabberdapi` executable.
Please check the [ejabberd-api homepage](https://github.com/processone/ejabberd-api)
for configuration and usage details.

For example, if you configure ejabberd like this:
```yaml
listen:
  -
    port: 5282
    module: ejabberd_http
    request_handlers:
      "/api": mod_http_api

acl:
  loopback:
    ip:
      - 127.0.0.0/8
      - ::1/128
      - ::FFFF:127.0.0.1/128

api_permissions:
  "admin access":
    who:
      access:
        allow:
          acl: loopback
    what:
      - "register"
```

Then you could register new accounts with this query:

```bash
docker exec -it ejabberd ejabberdapi register --endpoint=http://127.0.0.1:5282/ --jid=admin@localhost --password=passw0rd
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

---

Example to connect a local `ejabberdctl` to a containerized ejabberd:

1. When creating the container, export port 5210, and set `ERLANG_COOKIE`:
    ```sh
    docker run --name ejabberd -it \
      -e ERLANG_COOKIE=`cat $HOME/.erlang.cookie` \
      -p 5210:5210 -p 5222:5222 \
      ghcr.io/processone/ejabberd
    ```
2. Set `ERL_DIST_PORT=5210` in `ejabberdctl.cfg` of container and local ejabberd
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

---

Once you have the ejabberd nodes properly set and running,
you can tell the secondary nodes to join the master node using the
[`join_cluster`](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#join-cluster)
API call.

Example using environment variables (see the full
[`docker-compose.yml` clustering example](#clustering-example)):
```yaml
environment:
  - ERLANG_NODE_ARG=ejabberd@replica
  - ERLANG_COOKIE=dummycookie123
  - CTL_ON_CREATE=join_cluster ejabberd@main
```

### Change Mnesia Node Name

To use the same Mnesia database in a container with a different hostname,
it is necessary to change the old hostname stored in Mnesia.

This section is equivalent to the ejabberd Documentation
[Change Computer Hostname](https://docs.ejabberd.im/admin/guide/managing/#change-computer-hostname),
but particularized to containers that use this
`ecs` container image from ejabberd 23.01 or older.

#### Setup Old Container

Let's assume a container running ejabberd 23.01 (or older) from
this `ecs` container image, with the database directory binded
and one registered account.
This can be produced with:
```bash
OLDCONTAINER=ejaold
NEWCONTAINER=ejanew

mkdir database
sudo chown 9000:9000 database
docker run -d --name $OLDCONTAINER -p 5222:5222 \
       -v $(pwd)/database:/opt/ejabberd/database \
       ghcr.io/processone/ejabberd:23.01
docker exec -it $OLDCONTAINER ejabberdctl started
docker exec -it $OLDCONTAINER ejabberdctl register user1 localhost somepass
docker exec -it $OLDCONTAINER ejabberdctl registered_users localhost
```

Methods to know the Erlang node name:
```bash
ls database/ | grep ejabberd@
docker exec -it $OLDCONTAINER ejabberdctl status
docker exec -it $OLDCONTAINER grep "started in the node" logs/ejabberd.log
```

#### Change Mnesia Node

First of all let's store the Erlang node names and paths in variables.
In this example they would be:
```bash
OLDCONTAINER=ejaold
NEWCONTAINER=ejanew
OLDNODE=ejabberd@95145ddee27c
NEWNODE=ejabberd@localhost
OLDFILE=/opt/ejabberd/database/old.backup
NEWFILE=/opt/ejabberd/database/new.backup
```

1. Start your old container that can still read the Mnesia database correctly.
If you have the Mnesia spool files,
but don't have access to the old container anymore, go to
[Create Temporary Container](#create-temporary-container)
and later come back here.

2. Generate a backup file and check it was created:
```bash
docker exec -it $OLDCONTAINER ejabberdctl backup $OLDFILE
ls -l database/*.backup
```

3. Stop ejabberd:
```bash
docker stop $OLDCONTAINER
```

4. Create the new container. For example:
```bash
docker run \
       --name $NEWCONTAINER \
       -d \
       -p 5222:5222 \
       -v $(pwd)/database:/opt/ejabberd/database \
       ghcr.io/processone/ejabberd:latest
```

5. Convert the backup file to new node name:
```bash
docker exec -it $NEWCONTAINER ejabberdctl mnesia_change_nodename $OLDNODE $NEWNODE $OLDFILE $NEWFILE
```

6. Install the backup file as a fallback:
```bash
docker exec -it $NEWCONTAINER ejabberdctl install_fallback $NEWFILE
```

7. Restart the container:
```bash
docker restart $NEWCONTAINER
```

8. Check that the information of the old database is available.
In this example, it should show that the account `user1` is registered:
```bash
docker exec -it $NEWCONTAINER ejabberdctl registered_users localhost
```

9. When the new container is working perfectly with the converted Mnesia database,
you may want to remove the unneeded files:
the old container, the old Mnesia spool files, and the backup files.

#### Create Temporary Container

In case the old container that used the Mnesia database is not available anymore,
a temporary container can be created just to read the Mnesia database
and make a backup of it, as explained in the previous section.

This method uses `--hostname` command line argument for docker,
and `ERLANG_NODE_ARG` environment variable for ejabberd.
Their values must be the hostname of your old container
and the Erlang node name of your old ejabberd node.
To know the Erlang node name please check
[Setup Old Container](#setup-old-container).

Command line example:
```bash
OLDHOST=${OLDNODE#*@}
docker run \
       -d \
       --name $OLDCONTAINER \
       --hostname $OLDHOST \
       -p 5222:5222 \
       -v $(pwd)/database:/opt/ejabberd/database \
       -e ERLANG_NODE_ARG=$OLDNODE \
       ghcr.io/processone/ejabberd:latest
```

Check the old database content is available:
```bash
docker exec -it $OLDCONTAINER ejabberdctl registered_users localhost
```

Now that you have ejabberd running with access to the Mnesia database,
you can continue with step 2 of previous section
[Change Mnesia Node](#change-mnesia-node).


Build Container Image
----------------

The container image includes ejabberd as a standalone OTP release built using Elixir.

### Build `ejabberd` [![ejabberd Container](https://img.shields.io/badge/ejabberd-grey?logo=opencontainersinitiative&logoColor=2094f3)](https://github.com/processone/ejabberd/pkgs/container/ejabberd)

The ejabberd Erlang/OTP release is configured with:

- `mix.exs`: Customize ejabberd release
- `vars.config`: ejabberd compilation configuration options
- `config/runtime.exs`: Customize ejabberd paths
- `ejabberd.yml.template`: ejabberd default config file

#### Direct build

Build ejabberd Community Server container image from ejabberd master git repository:

```bash
docker buildx build \
    -t personal/ejabberd \
    -f .github/container/Dockerfile \
    .
```

#### Podman build

Some minor remarks:

- When building, it mentions that `healthcheck` is not supported by the Open Container Initiative image format
- To start with command `live`, you may want to add environment variable `EJABBERD_BYPASS_WARNINGS=true`

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

### Build `ecs` [![ecs Container](https://img.shields.io/badge/ecs-grey?logo=docker&logoColor=2094f3)](https://hub.docker.com/r/ejabberd/ecs/)

The ejabberd Erlang/OTP release is configured with:

- `rel/config.exs`: Customize ejabberd release
- `rel/dev.exs`: ejabberd environment configuration for development release
- `rel/prod.exs`: ejabberd environment configuration for production release
- `vars.config`: ejabberd compilation configuration options
- `conf/ejabberd.yml`: ejabberd default config file

Build ejabberd Community Server base image from ejabberd master on Github:

```bash
docker build -t personal/ejabberd .
```

Build ejabberd Community Server base image for a given ejabberd version:

```bash
./build.sh 18.03
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

Prepare an ejabberd configuration file:
```bash
mkdir conf && cp ejabberd.yml.example conf/ejabberd.yml
```

Create the database directory and allow the container access to it:

- Docker:
    ```bash
    mkdir database && sudo chown 9000:9000 database
    ```
- Podman:
    ```bash
    mkdir database && podman unshare chown 9000:9000 database
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
      - EJABBERD_MACRO_HOST=example.com
      - EJABBERD_MACRO_ADMIN=admin@example.com
      - REGISTER_ADMIN_PASSWORD=somePassw0rd
      - CTL_ON_START=registered_users example.com ;
                     status
    ports:
      - "5222:5222"
      - "5269:5269"
      - "5280:5280"
      - "5443:5443"
    volumes:
      - ./conf/ejabberd.yml:/opt/ejabberd/conf/ejabberd.yml:ro
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
    - name: EJABBERD_MACRO_HOST
      value: example.com
    - name: EJABBERD_MACRO_ADMIN
      value: admin@example.com
    - name: REGISTER_ADMIN_PASSWORD
      value: somePassw0rd
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
      path: ./conf/ejabberd.yml
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
    container_name: main
    environment:
      - ERLANG_NODE_ARG=ejabberd@main
      - ERLANG_COOKIE=dummycookie123
      - CTL_ON_CREATE=! register admin localhost asd
    healthcheck:
      test: netstat -nl | grep -q 5222
      start_period: 5s
      interval: 5s
      timeout: 5s
      retries: 120

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


Images Comparison
-----------------

Let's summarize the differences between both container images. Legend:

- ❇️:  is the recommended alternative
- 🟠: changed in ejabberd 26.01
- 🔆: changed in ...
- 🔅: changed in ejabberd 25.03

|                       | [![ejabberd Container](https://img.shields.io/badge/ejabberd-grey?logo=opencontainersinitiative&logoColor=2094f3)](https://github.com/processone/ejabberd/pkgs/container/ejabberd) | [![ecs Container](https://img.shields.io/badge/ecs-grey?logo=docker&logoColor=2094f3)](https://hub.docker.com/r/ejabberd/ecs/) |
|:----------------------|:------------------|:-----------------------|
| Source code           | [ejabberd/.github/container](https://github.com/processone/ejabberd/tree/master/.github/container) | [docker-ejabberd/ecs](https://github.com/processone/docker-ejabberd/tree/master/ecs) |
| Generated by          | [container.yml](https://github.com/processone/ejabberd/blob/master/.github/workflows/container.yml) | [tests.yml](https://github.com/processone/docker-ejabberd/blob/master/.github/workflows/tests.yml) |
| Built for             | stable releases <br /> `master` branch | stable releases <br /> [`master` branch zip](https://github.com/processone/docker-ejabberd/actions/workflows/tests.yml) |
| Architectures         | `linux/amd64` <br /> `linux/arm64` | `linux/amd64` |
| Software              | Erlang/OTP 28.3.1.0-alpine 🟠 <br /> Elixir 1.19.5 🟠 | Alpine 3.22 <br /> Erlang/OTP 26.2 <br /> Elixir 1.18.3 |
| Published in          | [ghcr.io/processone/ejabberd](https://github.com/processone/ejabberd/pkgs/container/ejabberd) | [docker.io/ejabberd/ecs](https://hub.docker.com/r/ejabberd/ecs/) <br /> [ghcr.io/processone/ecs](https://github.com/processone/docker-ejabberd/pkgs/container/ecs) |
| :black_square_button: **Additional content** |
| [ejabberd-contrib](#ejabberd-contrib) | included | not included |
| [ejabberdapi](#ejabberdapi) | included 🔅 | included |
| :black_square_button: **Ports** |
| [1880](#ports) for WebAdmin     | yes 🔅 | yes 🔅 |
| [5210](#ports) for `ERL_DIST_PORT` | supported | supported 🔅 |
| :black_square_button: **Paths** |
| `$HOME`               | `/opt/ejabberd/` | `/home/ejabberd/` |
| User data             | `$HOME` ❇️  <br /> `/home/ejabberd/` 🔅 | `$HOME` <br /> `/opt/ejabberd/` ❇️  |
| `ejabberdctl`         | `ejabberdctl` ❇️  <br /> `bin/ejabberdctl` 🔅  | `bin/ejabberdctl` <br /> `ejabberdctl` ❇️  |
| [`captcha.sh`](#captcha)         | `$HOME/bin/captcha.sh` 🔅  | `$HOME/bin/captcha.sh` 🔅 |
| `*.sql` files | `$HOME/sql/*.sql` ❇️  🔅 <br />  `$HOME/database/*.sql` 🔅 | `$HOME/database/*.sql` <br /> `$HOME/sql/*.sql` ❇️  🔅 |
| Mnesia spool files | `$HOME/database/` ❇️  <br /> `$HOME/database/NODENAME/` 🔅  | `$HOME/database/NODENAME/` <br /> `$HOME/database/` ❇️  🔅 |
| :black_square_button: **Variables** |
| [`EJABBERD_MACRO_*`](#macros-in-environment)      | supported | supported |
| Macros used in `ejabberd.yml` | yes 🔅 | yes 🔅 |
| [`EJABBERD_MACRO_ADMIN`](#register-admin-account) | Grant admin rights 🔅 <br /> (default `admin@localhost`) <br /> | Hardcoded `admin@localhost` |
| [`REGISTER_ADMIN_PASSWORD`](#register-admin-account) | Register admin account 🔅 | unsupported |
| `CTL_OVER_HTTP`       | enabled 🔅 | unsupported |
