
<p align="center">
    <img src="https://www.process-one.net/wp-content/uploads/2022/05/ejabberd-logo-rounded-index.png">
</p>
<p align="center">
    <a href="https://github.com/processone/ejabberd/tags" alt="GitHub tag (latest SemVer)">
       <img src="https://img.shields.io/github/v/tag/processone/ejabberd?sort=semver&logo=embarcadero&label=&color=3fb0d2&logoWidth=20" /></a>
    <a href="https://hex.pm/packages/ejabberd" alt="Hex version">
       <img src="https://img.shields.io/hexpm/v/ejabberd.svg" /></a>
    <a href="https://formulae.brew.sh/formula/ejabberd" alt="homebrew version">
       <img src="https://img.shields.io/homebrew/v/ejabberd" /></a>
    <a href="https://hub.docker.com/r/ejabberd/ecs/" alt="Docker Image Version (latest semver)">
       <img src="https://img.shields.io/docker/v/ejabberd/ecs?label=ecs&logo=docker" /></a>
    <a href="https://github.com/processone/ejabberd/pkgs/container/ejabberd" alt="GitHub Container">
       <img src="https://img.shields.io/github/v/tag/processone/ejabberd?label=ejabberd&sort=semver&logo=docker" /></a>
    <br />
    <a href="https://github.com/processone/ejabberd/actions/workflows/ci.yml" alt="CI">
       <img src="https://github.com/processone/ejabberd/actions/workflows/ci.yml/badge.svg" /></a>
    <a href="https://coveralls.io/github/processone/ejabberd?branch=master" alt="Coverage Status">
       <img src="https://coveralls.io/repos/github/processone/ejabberd/badge.svg?branch=master "Coverage in coveralls.io"" /></a>
    <a href="https://hosted.weblate.org/projects/ejabberd/ejabberd-po/" alt="Translation status">
       <img src="https://hosted.weblate.org/widgets/ejabberd/-/ejabberd-po/svg-badge.svg"" /></a>
</p>


[ejabberd][im] is an open-source,
robust, scalable and extensible realtime platform built using [Erlang/OTP][erlang],
that includes [XMPP][xmpp] Server, [MQTT][mqtt] Broker and [SIP][sip] Service.

Check the features in [ejabberd.im][im], [ejabberd Docs][features],
[ejabberd at ProcessOne][p1home], and the list of [supported protocols in ProcessOne][xeps]
and [XMPP.org][xmppej].

Installation
------------

There are several ways to install ejabberd:

- Source code: compile yourself, see [COMPILE](COMPILE.md)
- Installers from [ProcessOne Download][p1download] or [ejabberd GitHub Releases][releases] (run/deb/rpm for x64 and arm64)
- `ecs` container image available in [Docker Hub][hubecs] and [Github Packages][packagesecs], see [ecs README][docker-ecs-readme] (for x64)
- `ejabberd` container image available in [Github Packages][packages], see [CONTAINER](CONTAINER.md) (for x64 and arm64)
- Using your [Operating System package][osp]
- Using the [Homebrew][homebrew] package manager


Documentation
-------------

Please check the [ejabberd Docs][docs] website.

When compiling from source code, you can get some help with:

    ./configure --help
    make help

Once ejabberd is installed, try:

    ejabberdctl help
    man ejabberd.yml


Development
-----------

Bug reports and features are tracked using [GitHub Issues][issues],
please check [CONTRIBUTING](CONTRIBUTING.md) for details.

Translations can be improved online [using Weblate][weblate]
or in your local machine as explained in [Localization][localization].

Documentation for developers is available in [ejabberd docs: Developers][docs-dev].

Security reports or concerns should preferably be reported privately,
please send an email to the address: contact at process-one dot net
or some other method from [ProcessOne Contact][p1contact].

For commercial offering and support, including [ejabberd Business Edition][p1home]
and [Fluux (ejabberd in the Cloud)][fluux], please check [ProcessOne ejabberd page][p1home].


Community
---------

There are several places to get in touch with other ejabberd developers and administrators:

- ejabberd XMPP chatroom: [ejabberd@conference.process-one.net][muc]
- [GitHub Discussions][discussions]
- [Stack Overflow][stackoverflow]


License
-------

ejabberd is released under the GNU General Public License v2 (see [COPYING](COPYING.md)),
and [ejabberd translations](https://github.com/processone/ejabberd-po/) under MIT License.


[discussions]: https://github.com/processone/ejabberd/discussions
[docker-ecs-readme]: https://github.com/processone/docker-ejabberd/tree/master/ecs#readme
[docs-dev]: https://docs.ejabberd.im/developer/
[docs]: https://docs.ejabberd.im
[erlang]: https://www.erlang.org/
[features]: https://docs.ejabberd.im/admin/introduction/
[fluux]: https://fluux.io/
[homebrew]: https://docs.ejabberd.im/admin/installation/#homebrew
[hubecs]: https://hub.docker.com/r/ejabberd/ecs/
[im]: https://ejabberd.im/
[issues]: https://github.com/processone/ejabberd/issues
[localization]: https://docs.ejabberd.im/developer/extending-ejabberd/localization/
[mqtt]: https://mqtt.org/
[muc]: xmpp:ejabberd@conference.process-one.net
[osp]: https://docs.ejabberd.im/admin/installation/#operating-system-packages
[p1contact]: https://www.process-one.net/en/company/contact/
[p1download]: https://www.process-one.net/en/ejabberd/downloads/
[p1home]: https://www.process-one.net/en/ejabberd/
[packages]: https://github.com/processone/ejabberd/pkgs/container/ejabberd
[packagesecs]: https://github.com/processone/docker-ejabberd/pkgs/container/ecs
[releases]: https://github.com/processone/ejabberd/releases
[sip]: https://en.wikipedia.org/wiki/Session_Initiation_Protocol
[stackoverflow]: https://stackoverflow.com/questions/tagged/ejabberd?sort=newest
[weblate]: https://hosted.weblate.org/projects/ejabberd/ejabberd-po/
[xeps]: https://www.process-one.net/en/ejabberd/protocols/
[xmpp]: https://xmpp.org/
[xmppej]: https://xmpp.org/software/servers/ejabberd/
