ejabberd container

- [Introduction](#introduction)
    - [Version](#version)
- [Quick Start](#quick-start)
- [Usage](#usage)
    - [Persistence](#persistence)
    - [SSL Certificates](#ssl-certificates)
    - [Base Image](#base-image)
    - [Cluster Example](#cluster-example)
- [Runtime Configuration](#runtime-configuration)
    - [Served Hostnames](#served-hostnames)
    - [Authentication](#authentication)
    - [Admins](#admins)
    - [Users](#users)
    - [SSL](#ssl)
    - [Erlang](#erlang)
    - [Modules](#modules)
    - [Logging](#logging)
    - [Mount Configurations](#mount-configurations)
- [Maintenance](#maintenance)
    - [Register Users](#register-users)
    - [Creating Backups](#creating-backups)
    - [Restoring Backups](#restoring-backups)
- [Debug](#debug)
    - [Erlang Shell](#erlang-shell)
    - [System Shell](#system-shell)
    - [System Commands](#system-commands)
- [Exposed Ports](#exposed-ports)

# Introduction

This [ejabberd][] docker container is based on the work done by [rroemhild][]. See more [in this blogpost][].
This container includes the necessary files to build your own containerized ejabberd,
but *IT IS NOT* used to generate official images on the docker [hub][].
This container is not maintained by [ProcessOne][].

[ProcessOne][] provides and maintain official containers on the docker [hub][], which targets developers for now and will becomes production ready in a near future.
These [new containers] allow to build and run ejabberd in a simple and lightweight environment.

[ejabberd]: https://www.ejabberd.im/
[rroemhild]: https://github.com/rroemhild/docker-ejabberd/
[in this blogpost]: https://blog.process-one.net/ejabberd-16-12/
[hub]: https://hub.docker.com/r/ejabberd/ecs/
[new containers]: https://github.com/processone/docker-ejabberd/
[ProcessOne]: https://www.process-one.net/

## Version

Current Version: `17.08`

Docker Tag Names are based on ejabberd versions in git [branches][] and [tags][]. The image tag `:latest` is based on the master branch.

[tags]: https://github.com/rroemhild/docker-ejabberd/tags
[branches]: https://github.com/rroemhild/docker-ejabberd/branches

# Quick Start

You can start of with the following container:

```bash
docker run -d \
    --name "ejabberd" \
    -p 5222:5222 \
    -p 5269:5269 \
    -p 5280:5280 \
    -h 'xmpp.example.de' \
    -e "XMPP_DOMAIN=example.de" \
    -e "ERLANG_NODE=ejabberd" \
    -e "EJABBERD_ADMINS=admin@example.de admin2@example.de" \
    -e "EJABBERD_USERS=admin@example.de:password1234 admin2@example.de" \
    -e "TZ=Europe/Berlin" \
    rroemhild/ejabberd
```

or with the [docker-compose](examples/docker-compose/docker-compose.yml) example

```bash
wget https://raw.githubusercontent.com/rroemhild/docker-ejabberd/master/examples/docker-compose/docker-compose.yml
docker-compose up
```

# Usage

## Persistence

For storage of the application data, you can mount volumes at

* `/opt/ejabberd/ssl`
* `/opt/ejabberd/backup`
* `/opt/ejabberd/upload`
* `/opt/ejabberd/database`

or use a data container

```bash
docker create --name ejabberd-data rroemhild/ejabberd-data
docker run -d --name ejabberd --volumes-from ejabberd-data rroemhild/ejabberd
```

## SSL Certificates

TLS is enabled by default and the run script will auto-generate two snake-oil certificates during boot if you don't provide your SSL certificates.

To use your own certificates, there are two options.

1. Mount the volume `/opt/ejabberd/ssl` to a local directory with the `.pem` files:

    * /tmp/ssl/host.pem (SERVER_HOSTNAME)
    * /tmp/ssl/xmpp_domain.pem (XMPP_DOMAIN)

    Make sure that the certificate and private key are in one `.pem` file. If one file is missing it will be auto-generated. I.e. you can provide your certificate for your **XMMP_DOMAIN** and use a snake-oil certificate for the `SERVER_HOSTNAME`.

2. Specify the certificates via environment variables: **EJABBERD_SSLCERT_HOST** and **EJABBERD_SSLCERT_EXAMPLE_COM**. For the
domain certificates, make sure you match the domain names given in **XMPP_DOMAIN** and replace dots and dashes with underscore.

## Base Image

Build your own ejabberd container image and add your config templates, certificates or [extend](#cluster-example) it for your needs.

```
FROM rroemhild/ejabberd
ADD ./ejabberd.yml.tpl /opt/ejabberd/conf/ejabberd.yml.tpl
ADD ./ejabberdctl.cfg.tpl /opt/ejabberd/conf/ejabberdctl.cfg.tpl
ADD ./example.com.pem /opt/ejabberd/ssl/example.com.pem
```

If you need root privileges switch to `USER root` and go back to `USER ejabberd` when you're done.

## Cluster Example

The [docker-compose-cluster](examples/docker-compose-cluster) example demonstrates how to extend this container image to setup a multi-master cluster.

# Runtime Configuration

You can additionally provide extra runtime configuration in a downstream image by replacing the config template `ejabberd.yml.tpl` with one based on this image's template and include extra interpolation of environment variables. The template is parsed by Jinja2 with the runtime environment (equivalent to Python's `os.environ` available as `env`).

## Served Hostnames

By default the container will serve the XMPP domain `localhost`. In order to serve a different domain at runtime, provide the **XMPP_DOMAIN** variable with a domain name. You can add more domains separated with whitespace.

```
XMPP_DOMAIN=example.ninja xyz.io test.com
```

## Authentication

Authentication methods can be set with the **EJABBERD_AUTH_METHOD** environment variable. The default authentication mode is `internal`.

Supported authentication methods:

* anonymous
* internal
* external
* ldap

Internal and anonymous authentication example:

```
EJABBERD_AUTH_METHOD=internal anonymous
```

[External authentication](http://docs.ejabberd.im/admin/guide/configuration/#external-script) example:
```
EJABBERD_AUTH_METHOD=external
EJABBERD_EXTAUTH_PROGRAM="/opt/ejabberd/scripts/authenticate-user.sh"
EJABBERD_EXTAUTH_INSTANCES=3
EJABBERD_EXTAUTH_CACHE=600
```
**EJABBERD_EXTAUTH_INSTANCES** must be an integer with a minimum value of 1. **EJABBERD_EXTAUTH_CACHE** can be set to "false" or an integer value representing cache time in seconds. Note that caching should not be enabled if internal auth is also enabled.

### Password format

The variable `EJABBERD_AUTH_PASSWORD_FORMAT` controls in which format user passwords are
stored. Possible values are `plain` and `scram`. The default is to store
[SCRAM](https://en.wikipedia.org/wiki/Salted_Challenge_Response_Authentication_Mechanism)bled
passwords, meaning that it is impossible to obtain the original plain password from the
stored information.

NOTE: SCRAM does not work with SIP/TURN foreign authentication methods. In this case, you
may have to disable the option. More details can be found here:
https://docs.ejabberd.im/admin/configuration/#internal

If using SCRAM with an SQL database that has plaintext passwords stored, use the command

```
ejabberdctl convert_to_scram example.org
```

to convert all your existing plaintext passwords to scrambled format.

### MySQL Authentication

Set `EJABBERD_AUTH_METHOD=external` and `EJABBERD_EXTAUTH_PROGRAM=/opt/ejabberd/scripts/lib/auth_mysql.py` to enable MySQL authentication. Use the following environment variables to configure the database connection and the layout of the database. Password changing, registration, and unregistration are optional features and are enabled only if the respective queries are provided.

- **AUTH_MYSQL_HOST**: The MySQL host
- **AUTH_MYSQL_USER**: Username to connect to the MySQL host
- **AUTH_MYSQL_PASSWORD**: Password to connect to the MySQL host
- **AUTH_MYSQL_DATABASE**: Database name where to find the user information
- **AUTH_MYSQL_HASHALG**: Format of the password in the database. Default is cleartext. Options are `crypt`, `md5`, `sha1`, `sha224`, `sha256`, `sha384`, `sha512`. `crypt` is recommended, as it is salted. When setting the password, `crypt` uses SHA-512 (prefix `$6$`).
- **AUTH_MYSQL_QUERY_GETPASS**: Get the password for a user. Use the placeholders `%(user)s`, `%(host)s`. Example: `SELECT password FROM users WHERE username = CONCAT(%(user)s, '@', %(host)s)`
- **AUTH_MYSQL_QUERY_SETPASS**: Update the password for a user. Leave empty to disable. Placeholder `%(password)s` contains the hashed password. Example: `UPDATE users SET password = %(password)s WHERE username = CONCAT(%(user)s, '@', %(host)s)`
- **AUTH_MYSQL_QUERY_REGISTER**: Register a new user. Leave empty to disable. Example: `INSERT INTO users ( username, password ) VALUES ( CONCAT(%(user)s, '@', %(host)s), %(password)s )`
- **AUTH_MYSQL_QUERY_UNREGISTER**: Removes a user. Leave empty to disable. Example: `DELETE FROM users WHERE username = CONCAT(%(user)s, '@', %(host)s)`

Note that the MySQL authentication script writes a debug log into the file `/var/log/ejabberd/extauth.log`. To get its content, execute the following command:

```bash
docker exec -ti ejabberd tail -n50 -f /var/log/ejabberd/extauth.log
```

To find out more about the mysql authentication script, check out the [ejabberd-auth-mysql](https://github.com/rankenstein/ejabberd-auth-mysql) repository.

### LDAP Auth

Full documentation http://docs.ejabberd.im/admin/guide/configuration/#ldap.

Connection

- **EJABBERD_LDAP_SERVERS**: List of IP addresses or DNS names of your LDAP servers. This option is required.
- **EJABBERD_LDAP_ENCRYPT**: The value `tls` enables encryption by using LDAP over SSL. The default value is: `none`.
- **EJABBERD_LDAP_TLS_VERIFY**: `false|soft|hard` This option specifies whether to verify LDAP server certificate or not when TLS is enabled. The default is `false` which means no checks are performed.
- **EJABBERD_LDAP_TLS_CACERTFILE**: Path to file containing PEM encoded CA certificates.
- **EJABBERD_LDAP_TLS_DEPTH**: Specifies the maximum verification depth when TLS verification is enabled. The default value is 1.
- **EJABBERD_LDAP_PORT**: The default port is `389` if encryption is disabled; and `636` if encryption is enabled.
- **EJABBERD_LDAP_ROOTDN**: Bind DN. The default value is "" which means ‘anonymous connection’.
- **EJABBERD_LDAP_PASSWORD**: Bind password. The default value is "".
- **EJABBERD_LDAP_DEREF_ALIASES**: `never|always|finding|searching`
   Whether or not to dereference aliases. The default is `never`.

Authentication

- **EJABBERD_LDAP_BASE**: LDAP base directory which stores users accounts. This option is required.
- **EJABBERD_LDAP_UIDS**: `ldap_uidattr:ldap_uidattr_format` The default attributes are `uid:%u`.
- **EJABBERD_LDAP_FILTER**: RFC 4515 LDAP filter. The default Filter value is undefined.
- **EJABBERD_LDAP_DN_FILTER**: `{ Filter: FilterAttrs }` This filter is applied on the results returned by the main filter. By default ldap_dn_filter is undefined.

## Admins

Set one or more admin user (seperated by whitespace) with the **EJABBERD_ADMINS** environment variable. You can register admin users with the **EJABBERD_USERS** environment variable during container startup, use you favorite XMPP client or the `ejabberdctl` command line utility.

```
EJABBERD_ADMINS=admin@example.ninja
```

## Users

Automatically register users during container startup. Uses random password if you don't provide a password for the user. Format is `JID:PASSWORD`. Register more users separated with whitespace.

Register the admin user from **EJABBERD_ADMINS** with a give password:

```
EJABBERD_USERS=admin@example.ninja:password1234
```

Or without a random password printed to stdout (check container logs):

```
EJABBERD_USERS=admin@example.ninja
```

Register more than one user:

```
EJABBERD_USERS=admin@example.ninja:password1234 user1@test.com user1@xyz.io
```

## SSL
- **EJABBERD_SKIP_MAKE_SSLCERT**: Skip generating ssl certificates. Default: false
- **EJABBERD_SSLCERT_HOST**: SSL Certificate for the hostname.
- **EJABBERD_SSLCERT_EXAMPLE_COM**: SSL Certificates for XMPP domains.
- **EJABBERD_STARTTLS**: Set to `false` to disable StartTLS for client to server connections. Defaults
 to `true`.
- **EJABBERD_S2S_SSL**: Set to `false` to disable SSL in server 2 server connections. Defaults to `true`.
- **EJABBERD_HTTPS**: If your proxy terminates SSL you may want to disable HTTPS on port 5280 and 5443. Defaults to `true`.
- **EJABBERD_PROTOCOL_OPTIONS_TLSV1**: Allow TLSv1 protocol. Defaults to `false`.
- **EJABBERD_PROTOCOL_OPTIONS_TLSV1_1**: Allow TLSv1.1 protocol. Defaults to `true`.
- **EJABBERD_CIPHERS**: Cipher suite. Defaults to `HIGH:!aNULL:!3DES`.
- **EJABBERD_DHPARAM**: Set to `true` to use or generate custom DH parameters. Defaults to `false`.
- **EJABBERD_SKIP_MAKE_DHPARAM**: Skip generating DH params. Default: false

## Erlang
- **ERLANG_NODE**: Allows to explicitly specify erlang node for ejabberd. Set to `ejabberd` lets erlang add the hostname. Defaults to `ejabberd@localhost`.
- **ERLANG_COOKIE**: Set erlang cookie. Defaults to auto-generated cookie.
- **ERLANG_OPTIONS**: Overwrite additional options passed to erlang while starting ejabberd.

## Modules
- **EJABBERD_SKIP_MODULES_UPDATE**: If you do not need to update ejabberd modules specs, skip the update task and speedup start. Defaults to `false`.
- **EJABBERD_MOD_MUC_ADMIN**: Activate the mod_muc_admin module. Defaults to `false`.
- **EJABBERD_MOD_ADMIN_EXTRA**: Activate the mod_muc_admin module. Defaults to `true`.
- **EJABBERD_REGISTER_TRUSTED_NETWORK_ONLY**: Only allow user registration from the trusted_network access rule. Defaults to `true`.
- **EJABBERD_MOD_VERSION**: Activate the mod_version module. Defaults to `true`.
- **EJABBERD_SOURCE_MODULES**: List of modules, which will be installed from sources localized in ${EJABBERD_HOME}/module_source.
- **EJABBERD_CONTRIB_MODULES**: List of modules, which will be installed from contrib repository.
- **EJABBERD_RESTART_AFTER_MODULE_INSTALL**: If any modules were installed, restart the server, if the option is enabled.
- **EJABBERD_CUSTOM_AUTH_MODULE_OVERRIDE**: If a custom module was defined for handling auth, we need to override the pre-defined auth methods in the config.

## Logging

Use the **EJABBERD_LOGLEVEL** environment variable to set verbosity. Defaults to `4` (Info).

```
loglevel: Verbosity of log files generated by ejabberd.
0: No ejabberd log at all (not recommended)
1: Critical
2: Error
3: Warning
4: Info
5: Debug
```

## Mount Configurations

If you prefer to use your own configuration files and avoid passing docker environment variables (```-e```), you can do so by mounting a host directory.
Pass in an additional ```-v``` to the ```docker run``` command, like so:
```
docker run -d \
    --name "ejabberd" \
    -p 5222:5222 \
    -p 5269:5269 \
    -p 5280:5280 \
    -h 'xmpp.example.de' \
    -v /<host_path>/conf:/opt/ejabberd/conf \
    rroemhild/ejabberd
```

Your ```/<host_path>/conf``` folder should look like so:

```
/<host_path>/conf/
├── ejabberdctl.cfg
├── ejabberd.yml
└── inetrc
```

Example configuration files can be downloaded from the ejabberd [github](https://github.com/rroemhild/ejabberd) page.

When these files exist in ```/opt/ejabberd/conf```, the run script will ignore the configuration templates.

# Maintenance

The `ejabberdctl` command is in the search path and can be run by:

```bash
docker exec CONTAINER ejabberdctl help
```

## Register Users

```bash
docker exec CONTAINER ejabberdctl register user XMPP_DOMAIN PASSWORD
```

## Creating Backups

Create a backupfile with ejabberdctl and copy the file from the container to localhost

```bash
docker exec CONTAINER ejabberdctl backup /opt/ejabberd/backup/ejabberd.backup
docker cp CONTAINER:/opt/ejabberd/backup/ejabberd.backup /tmp/ejabberd.backup
```

## Restoring Backups

Copy the backupfile from localhost to the running container and restore with ejabberdctl

```bash
docker cp /tmp/ejabberd.backup CONTAINER:/opt/ejabberd/backup/ejabberd.backup
docker exec CONTAINER ejabberdctl restore /opt/ejabberd/backup/ejabberd.backup
```

# Debug

## Erlang Shell

Set `-i` and `-t` option and append `live` to get an interactive erlang shell:

```bash
docker run -i -t -P rroemhild/ejabberd live
```

You can terminate the erlang shell with `q().`.

## System Shell

```bash
docker run -i -t rroemhild/ejabberd shell
```

## System Commands

```bash
docker run -i -t rroemhild/ejabberd env
```

# Exposed Ports

* 4560 (XMLRPC)
* 5222 (Client 2 Server)
* 5269 (Server 2 Server)
* 5280 (HTTP admin/websocket/http-bind)
* 5443 (HTTP Upload)
