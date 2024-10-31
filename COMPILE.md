Compile and Install ejabberd
============================

This document explains how to compile and install ejabberd
from source code.

For a more detailed explanation, please check the
ejabberd Docs: [Source Code Installation][docs-source].

[docs-source]: https://docs.ejabberd.im/admin/install/source/


Requirements
------------

To compile ejabberd you need:

- GNU Make
- GCC
- Libexpat ≥ 1.95
- Libyaml ≥ 0.1.4
- Erlang/OTP ≥ 20.0
- OpenSSL ≥ 1.0.0

Other optional libraries are:

- Zlib ≥ 1.2.3, for Stream Compression support (XEP-0138)
- PAM library, for Pluggable Authentication Modules (PAM)
- ImageMagick's Convert program and Ghostscript fonts, for CAPTCHA
  challenges
- Elixir ≥ 1.10.3, for Elixir support. It is recommended Elixir 1.13.4 or higher
  and Erlang/OTP 23.0 or higher.

If your system splits packages in libraries and development headers,
install the development packages too.


Download Source Code
--------------------

There are several ways to obtain the ejabberd source code:

- Source code archive from [ProcessOne Downloads][p1dl]
- Source code package from [ejabberd GitHub Releases][ghr]
- Latest development code from [ejabberd Git repository][gitrepo]

[p1dl]: https://www.process-one.net/download/ejabberd/
[ghr]: https://github.com/processone/ejabberd/releases
[gitrepo]: https://github.com/processone/ejabberd


Compile
-------

The general instructions to compile ejabberd are:

    ./configure
    make

If the source code doesn't contain a `configure` script,
first of all install `autoconf` and run this to generate it:

    ./autogen.sh

To configure the compilation, features, install paths...

    ./configure --help


Install in the System
---------------------

To install ejabberd in the system, run this with system administrator rights (root user):

    sudo make install

This will:

- Install the configuration files in `/etc/ejabberd/`
- Install ejabberd binary, header and runtime files in `/lib/ejabberd/`
- Install the administration script: `/sbin/ejabberdctl`
- Install ejabberd documentation in `/share/doc/ejabberd/`
- Create a spool directory: `/var/lib/ejabberd/`
- Create a directory for log files: `/var/log/ejabberd/`


Build an OTP Release
--------------------

Instead of installing ejabberd in the system, you can build an OTP release
that includes all necessary to run ejabberd in a subdirectory:

    ./configure
    make prod

Check the full list of targets:

    make help


Start ejabberd
--------------

You can use the `ejabberdctl` command line administration script to
start and stop ejabberd. Some examples, depending on your installation method:

- When installed in the system:
  ```
  ejabberdctl start
  /sbin/ejabberdctl start
  ```

- When built an OTP production release:
  ```
  _build/prod/rel/ejabberd/bin/ejabberdctl start
  _build/prod/rel/ejabberd/bin/ejabberdctl live
  ```

- Start interactively without installing or building OTP release:
  ```
  make relive
  ```
