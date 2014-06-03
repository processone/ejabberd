ejabberd - High-Performance Enterprise Instant Messaging Server 
---------------------------------------------------------------

Quickstart guide
================


0. Requirements
---------------

To compile ejabberd you need:

 - GNU Make
 - GCC
 - Libexpat 1.95 or higher
 - Libyaml 0.1.4 or higher
 - Erlang/OTP R15B or higher.
 - OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption.
 - Zlib 1.2.3 or higher, for Stream Compression support
   (XEP-0138). Optional.
 - PAM library. Optional. For Pluggable Authentication Modules (PAM).
 - GNU Iconv 1.8 or higher, for the IRC Transport
   (mod_irc). Optional. Not needed on systems with GNU Libc.
 - ImageMagick's Convert program. Optional. For CAPTCHA challenges.


1. Compile and install on *nix systems
--------------------------------------

To compile ejabberd execute the commands:

    ./configure
    make

To install ejabberd, run this command with system administrator rights
(root user):

    sudo make install

These commands will:

 - Install the configuration files in `/etc/ejabberd/`
 - Install ejabberd binary, header and runtime files in `/lib/ejabberd/`
 - Install the administration script: `/sbin/ejabberdctl`
 - Install ejabberd documentation in `/share/doc/ejabberd/`
 - Create a spool directory: `/var/lib/ejabberd/`
 - Create a directory for log files: `/var/log/ejabberd/`


2. Start ejabberd
-----------------

You can use the `ejabberdctl` command line administration script to
start and stop ejabberd. For example:

    ejabberdctl start


For detailed information please refer to the [ejabberd Installation and
Operation Guide][1].

[1]: http://www.process-one.net/docs/ejabberd/guide_en.html
