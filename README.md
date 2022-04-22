ejabberd Community Edition
==========================

[![CI](https://github.com/processone/ejabberd/actions/workflows/ci.yml/badge.svg)](https://github.com/processone/ejabberd/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/processone/ejabberd/badge.svg?branch=master "Coverage in coveralls.io")](https://coveralls.io/github/processone/ejabberd?branch=master)
[![Translation status](https://hosted.weblate.org/widgets/ejabberd/-/ejabberd-po/svg-badge.svg "Translation status in Weblate")](https://hosted.weblate.org/projects/ejabberd/ejabberd-po/)
[![Hex version](https://img.shields.io/hexpm/v/ejabberd.svg "Hex version")](https://hex.pm/packages/ejabberd)


[ejabberd][im] is an open-source,
robust, scalable and extensible realtime platform built using [Erlang/OTP][erlang],
that includes [XMPP][xmpp] Server, [MQTT][mqtt] Broker and [SIP][sip] Service.

Check the features in [ejabberd.im][im], [ejabberd Docs][features],
[ejabberd at ProcessOne][p1home], and a list of [supported protocols and XEPs][xeps].


Installation
------------

There are several ways to install ejabberd:

- Source code: compile yourself, see [COMPILE](COMPILE.md)
- Installers from [ProcessOne Downloads][p1dl] (run/deb/rpm for x64)
- Installers from [ejabberd GitHub Releases][releases] (run/deb/rpm for x64 and arm64)
- Container image from [ejabberd Docker Hub][hubecs], see [ecs README][docker-ecs-readme] (for x64)
- Container image from [ejabberd Github Packages][packages], see [CONTAINER](CONTAINER.md) (for x64 and arm64)
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
please send an email to the address: contact [at] process-one [dot] net
or some other method from [ProcessOne Contact][p1contact].

For commercial offering and support, including _ejabberd Business Edition_
and _Fluux (ejabberd in the Cloud)_, please check [ProcessOne ejabberd page][p1home].


Community
---------

There are several places to get in touch with other ejabberd developers and administrators:

- [ejabberd XMPP chatroom][muc]: ejabberd@conference.process-one.net
- [Mailing list][list]
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
[github]: https://github.com/processone/ejabberd
[homebrew]: https://docs.ejabberd.im/admin/installation/#homebrew
[hubecs]: https://hub.docker.com/r/ejabberd/ecs/
[im]: https://ejabberd.im/
[issues]: https://github.com/processone/ejabberd/issues
[list]: https://lists.jabber.ru/mailman/listinfo/ejabberd
[localization]: https://docs.ejabberd.im/developer/extending-ejabberd/localization/
[mqtt]: https://mqtt.org/
[muc]: xmpp:ejabberd@conference.process-one.net
[osp]: https://docs.ejabberd.im/admin/installation/#operating-system-packages
[p1contact]: https://www.process-one.net/en/company/contact/
[p1dl]: https://www.process-one.net/en/ejabberd/downloads/
[p1home]: https://www.process-one.net/en/ejabberd/
[packages]: https://github.com/processone/ejabberd/pkgs/container/ejabberd
[releases]: https://github.com/processone/ejabberd/releases
[sip]: https://en.wikipedia.org/wiki/Session_Initiation_Protocol
[stackoverflow]: https://stackoverflow.com/questions/tagged/ejabberd?sort=newest
[weblate]: https://hosted.weblate.org/projects/ejabberd/ejabberd-po/
[xeps]: https://www.process-one.net/en/ejabberd/protocols/
[xmpp]: https://xmpp.org/
