%%%----------------------------------------------------------------------
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module(ejabberd_options_doc).

%% API
-export([doc/0]).

-include("translate.hrl").

%%%===================================================================
%%% API
%%%===================================================================
doc() ->
    [{hosts,
      #{value => ?T("[Domain1, Domain2, ...]"),
        desc =>
            ?T("List of one or more "
               "_`../configuration/basic.md#host-names|host names`_ "
               "(or domains) that 'ejabberd' will serve. This is a "
               "**mandatory** option.")}},
     {listen,
      #{value => "[Options, ...]",
        desc =>
            ?T("The option for listeners configuration. See the "
               "_`listen.md|Listen Modules`_ section "
               "for details.")}},
     {modules,
      #{value => "{Module: Options}",
        desc =>
            ?T("Set all the "
               "_`modules.md|modules`_ configuration options.")}},
     {loglevel,
      #{value =>
            "none | emergency | alert | critical | "
            "error | warning | notice | info | debug",
        desc =>
            ?T("Verbosity of ejabberd "
               "_`../configuration/basic.md#logging|logging`_. "
               "The default value is 'info'. "
               "NOTE: previous versions of ejabberd had log levels "
               "defined in numeric format ('0..5'). The numeric values "
               "are still accepted for backward compatibility, but "
               "are not recommended.")}},
     {cache_life_time,
      #{value => "timeout()",
        desc =>
            ?T("The time of a cached item to keep in cache. "
	       "Once it's expired, the corresponding item is "
	       "erased from cache. The default value is '1 hour'. "
	       "Several modules have a similar option; and some core "
	       "ejabberd parts support similar options too, see "
	       "_`auth_cache_life_time`_, _`oauth_cache_life_time`_, "
	       "_`router_cache_life_time`_, and _`sm_cache_life_time`_.")}},
     {cache_missed,
      #{value => "true | false",
        desc =>
            ?T("Whether or not to cache missed lookups. When there is "
	       "an attempt to lookup for a value in a database and "
	       "this value is not found and the option is set to 'true', "
	       "this attempt will be cached and no attempts will be "
	       "performed until the cache expires (see _`cache_life_time`_). "
	       "Usually you don't want to change it. Default is 'true'. "
	       "Several modules have a similar option; and some core "
	       "ejabberd parts support similar options too, see "
	       "_`auth_cache_missed`_, _`oauth_cache_missed`_, "
	       "_`router_cache_missed`_, and _`sm_cache_missed`_.")}},
     {cache_size,
      #{value => "pos_integer() | infinity",
        desc =>
	    ?T("A maximum number of items (not memory!) in cache. "
	       "The rule of thumb, for all tables except rosters, "
	       "you should set it to the number of maximum online "
	       "users you expect. For roster multiply this number "
	       "by 20 or so. If the cache size reaches this threshold, "
	       "it's fully cleared, i.e. all items are deleted, and "
	       "the corresponding warning is logged. You should avoid "
	       "frequent cache clearance, because this degrades "
	       "performance. The default value is '1000'. "
	       "Several modules have a similar option; and some core "
	       "ejabberd parts support similar options too, see "
	       "_`auth_cache_size`_, _`oauth_cache_size`_, "
	       "_`router_cache_size`_, and _`sm_cache_size`_.")}},
     {use_cache,
      #{value => "true | false",
        desc =>
	    ?T("Enable or disable cache. The default is 'true'. "
	       "Several modules have a similar option; and some core "
	       "ejabberd parts support similar options too, see "
	       "_`auth_use_cache`_, _`oauth_use_cache`_, _`router_use_cache`_, "
	       "and _`sm_use_cache`_.")}},
     {default_db,
      #{value => "mnesia | sql",
        desc =>
            ?T("_`database.md#default-database|Default database`_ "
               "to store persistent data in ejabberd. "
               "Modules and other components (e.g. authentication) "
               "may have its own value. The default value is 'mnesia'.")}},
     {default_ram_db,
      #{value => "mnesia | redis | sql",
        desc =>
            ?T("Default volatile (in-memory) storage for ejabberd. "
               "Modules and other components (e.g. session management) "
               "may have its own value. The default value is 'mnesia'.")}},
     {queue_type,
      #{value => "ram | file",
        desc =>
            ?T("Default type of queues in ejabberd. "
               "Modules may have its own value of the option. "
               "The value of 'ram' means that queues will be kept in memory. "
               "If value 'file' is set, you may also specify directory "
               "in _`queue_dir`_ option where file queues will be placed. "
               "The default value is 'ram'.")}},
     {version,
      #{value => "string()",
        desc =>
            ?T("The option can be used to set custom ejabberd version, "
               "that will be used by different parts of ejabberd, for "
               "example by _`mod_version`_ module. The default value is "
               "obtained at compile time from the underlying version "
               "control system.")}},
     {acl,
      #{value => "{ACLName: {ACLType: ACLValue}}",
        desc =>
            ?T("This option defines "
               "_`../configuration/basic.md#acl|access control lists`_: "
               "named sets "
               "of rules which are used to match against different targets "
               "(such as a JID or an IP address). Every set of rules "
               "has name 'ACLName': it can be any string except 'all' or 'none' "
               "(those are predefined names for the rules that match all or nothing "
               "respectively). The name 'ACLName' can be referenced from other "
               "parts of the configuration file, for example in _`access_rules`_ "
               "option. The rules of 'ACLName' are represented by mapping "
               "'pass:[{ACLType: ACLValue}]'. These can be one of the following:")},
      [{user,
        #{value => ?T("Username"),
          desc =>
              ?T("If 'Username' is in the form of \"user@server\", "
                 "the rule matches a JID against this value. "
                 "Otherwise, if 'Username' is in the form of \"user\", "
                 "the rule matches any JID that has 'Username' in the node part "
                 "as long as the server part of this JID is any virtual "
                 "host served by ejabberd.")}},
       {server,
        #{value => ?T("Server"),
          desc =>
              ?T("The rule matches any JID from server 'Server'. "
                 "The value of 'Server' must be a valid "
                 "hostname or an IP address.")}},
       {resource,
        #{value => ?T("Resource"),
          desc =>
              ?T("The rule matches any JID with a resource 'Resource'.")}},
       {ip,
        #{value => ?T("Network"),
          desc =>
              ?T("The rule matches any IP address from the 'Network'.")}},
       {user_regexp,
        #{value => ?T("Regexp"),
          desc =>
              ?T("If 'Regexp' is in the form of \"regexp@server\", the rule "
                 "matches any JID with node part matching regular expression "
                 "\"regexp\" as long as the server part of this JID is equal "
                 "to \"server\". If 'Regexp' is in the form of \"regexp\", the rule "
                 "matches any JID with node part matching regular expression "
                 "\"regexp\" as long as the server part of this JID is any virtual "
                 "host served by ejabberd.")}},
       {server_regexp,
        #{value => ?T("Regexp"),
          desc =>
              ?T("The rule matches any JID from the server that "
                 "matches regular expression 'Regexp'.")}},
       {resource_regexp,
        #{value => ?T("Regexp"),
          desc =>
              ?T("The rule matches any JID with a resource that "
                 "matches regular expression 'Regexp'.")}},
       {node_regexp,
        #{value => ?T("user_regexp@server_regexp"),
          desc =>
              ?T("The rule matches any JID with node part matching regular "
                 "expression 'user_regexp' and server part matching regular "
                 "expression 'server_regexp'.")}},
       {user_glob,
        #{value => ?T("Pattern"),
          desc =>
              ?T("Same as 'user_regexp', but matching is performed on a "
                 "specified 'Pattern' according to the rules used by the "
                 "Unix shell.")}},
       {server_glob,
        #{value => ?T("Pattern"),
          desc =>
              ?T("Same as 'server_regexp', but matching is performed on a "
                 "specified 'Pattern' according to the rules used by the "
                 "Unix shell.")}},
       {resource_glob,
        #{value => ?T("Pattern"),
          desc =>
              ?T("Same as 'resource_regexp', but matching is performed on a "
                 "specified 'Pattern' according to the rules used by the "
                 "Unix shell.")}},
       {node_glob,
        #{value => ?T("Pattern"),
          desc =>
              ?T("Same as 'node_regexp', but matching is performed on a "
                 "specified 'Pattern' according to the rules used by the "
                 "Unix shell.")}}]},
     {access_rules,
      #{value => "{AccessName: {allow|deny: ACLRules|ACLName}}",
        desc =>
            ?T("This option defines "
               "_`basic.md#access-rules|Access Rules`_. "
               "Each access rule is "
               "assigned a name that can be referenced from other parts "
               "of the configuration file (mostly from 'access' options of "
               "ejabberd modules). Each rule definition may contain "
               "arbitrary number of 'allow' or 'deny' sections, and each "
               "section may contain any number of ACL rules (see _`acl`_ option). "
               "There are no access rules defined by default."),
        example =>
            ["access_rules:",
             "  configure:",
             "    allow: admin",
             "  something:",
             "    deny: someone",
             "    allow: all",
             "  s2s_banned:",
             "    deny: problematic_hosts",
             "    deny: banned_forever",
             "    deny:",
             "      ip: 222.111.222.111/32",
             "    deny:",
             "      ip: 111.222.111.222/32",
             "    allow: all",
             "  xmlrpc_access:",
             "    allow:",
             "      user: peter@example.com",
             "    allow:",
             "      user: ivone@example.com",
             "    allow:",
             "      user: bot@example.com",
             "      ip: 10.0.0.0/24"]}},
     {acme,
      #{value => ?T("Options"),
        desc =>
            ?T("_`basic.md#acme|ACME`_ configuration, to automatically "
               "obtain SSL certificates for the domains served by ejabberd, "
               "which means that certificate requests and renewals are "
               "performed to some CA server (aka \"ACME server\") in a fully "
               "automated mode. The 'Options' are:"),
        example =>
            ["acme:",
             "  ca_url: https://acme-v02.api.letsencrypt.org/directory",
             "  contact:",
             "    - mailto:admin@domain.tld",
             "    - mailto:bot@domain.tld",
             "  auto: true",
             "  cert_type: rsa"]},
      [{ca_url,
        #{value => ?T("URL"),
          desc =>
              ?T("The ACME directory URL used as an entry point "
                 "for the ACME server. The default value is "
                 "<https://acme-v02.api.letsencrypt.org/directory> - "
                 "the directory URL of Let's Encrypt authority.")}},
       {contact,
        #{value => ?T("[Contact, ...]"),
          desc =>
              ?T("A list of contact addresses (typically emails) "
                 "where an ACME server will send notifications "
                 "when problems occur. The value of 'Contact' must "
                 "be in the form of \"scheme:address\" (e.g. "
                 "\"mailto:user@domain.tld\"). The default "
                 "is an empty list which means an ACME server "
                 "will send no notices.")}},
       {auto,
        #{value => "true | false",
          desc =>
              ?T("Whether to automatically request certificates for "
                 "all configured domains (that yet have no a certificate) "
                 "on server start or configuration reload. The default is 'true'.")}},
       {cert_type,
        #{value => "rsa | ec",
          desc =>
              ?T("A type of a certificate key. Available values are "
                 "'ec' and 'rsa' for EC and RSA certificates respectively. "
                 "It's better to have RSA certificates for the purpose "
                 "of backward compatibility with legacy clients and servers, "
                 "thus the default is 'rsa'.")}}]},
     {allow_contrib_modules,
      #{value => "true | false",
        desc =>
            ?T("Whether to allow installation of third-party modules or not. "
               "See _`../../developer/extending-ejabberd/modules.md#ejabberd-contrib|ejabberd-contrib`_ "
               "documentation section. "
               "The default value is 'true'.")}},
     {allow_multiple_connections,
      #{value => "true | false",
        desc =>
            ?T("This option is only used when the anonymous mode is enabled. "
               "Setting it to 'true' means that the same username can be "
               "taken multiple times in anonymous login mode if different "
               "resource are used to connect. This option is only useful "
               "in very special occasions. The default value is 'false'.")}},
     {anonymous_protocol,
      #{value => "login_anon | sasl_anon | both",
        desc =>
            [?T("Define what "
                "_`authentication.md#anonymous-login-and-sasl-anonymous|anonymous`_ "
                "protocol will be used: "), "",
            ?T("* 'login_anon' means that the anonymous login method will be used. "), "",
            ?T("* 'sasl_anon' means that the SASL Anonymous method will be used. "), "",
            ?T("* 'both' means that SASL Anonymous and login anonymous are both "
               "enabled."), "",
            ?T("The default value is 'sasl_anon'."), ""]}},
     {api_permissions,
      #{value => "[Permission, ...]",
        desc =>
            ?T("Define the permissions for API access. Please consult the "
               "ejabberd Docs web -> For Developers -> ejabberd ReST API -> "
               "_`../../developer/ejabberd-api/permissions.md|API Permissions`_.")}},
     {append_host_config,
      #{value => "{Host: Options}",
        desc =>
            ?T("Add a few specific options to a certain "
               "_`../configuration/basic.md#virtual-hosting|virtual host`_.")}},
     {auth_cache_life_time,
      #{value => "timeout()",
        desc =>
            ?T("Same as _`cache_life_time`_, but applied to authentication cache "
               "only. If not set, the value from _`cache_life_time`_ will be used.")}},
     {auth_cache_missed,
      #{value => "true | false",
        desc =>
            ?T("Same as _`cache_missed`_, but applied to authentication cache "
               "only. If not set, the value from _`cache_missed`_ will be used.")}},
     {auth_cache_size,
      #{value => "pos_integer() | infinity",
        desc =>
            ?T("Same as _`cache_size`_, but applied to authentication cache "
               "only. If not set, the value from _`cache_size`_ will be used.")}},
     {auth_method,
      #{value => "[mnesia | sql | anonymous | external | jwt | ldap | pam, ...]",
        desc =>
            ?T("A list of _`authentication.md|authentication`_ methods to use. "
               "If several methods are defined, authentication is "
               "considered successful as long as authentication of "
               "at least one of the methods succeeds. "
               "The default value is '[mnesia]'.")}},
     {auth_opts,
      #{value => "[Option, ...]",
        desc =>
            ?T("This is used by the contributed module "
	       "'ejabberd_auth_http' that can be installed from the "
	       "https://github.com/processone/ejabberd-contrib[ejabberd-contrib] "
               "Git repository. Please refer to that "
	       "module's README file for details.")}},
     {auth_password_format,
      #{value => "plain | scram",
        note => "improved in 20.01",
        desc =>
            [?T("The option defines in what format the users passwords "
               "are stored, plain text or in _`authentication.md#scram|SCRAM`_ format:"), "",
            ?T("* 'plain': The password is stored as plain text "
               "in the database. This is risky because the passwords "
               "can be read if your database gets compromised. "
               "This is the default value. This format allows clients to "
               "authenticate using: the old Jabber Non-SASL (XEP-0078), "
               "SASL PLAIN, SASL DIGEST-MD5, and SASL SCRAM-SHA-1/256/512(-PLUS). "), "",
            ?T("* 'scram': The password is not stored, only some information "
               "required to verify the hash provided by the client. "
               "It is impossible to obtain the original plain password "
               "from the stored information; for this reason, when this "
               "value is configured it cannot be changed to plain anymore. "
               "This format allows clients to authenticate using: "
               "SASL PLAIN and SASL SCRAM-SHA-1/256/512(-PLUS). The SCRAM variant "
               "depends on the _`auth_scram_hash`_ option."), "",
            ?T("The default value is 'plain'."), ""]}},
     {auth_scram_hash,
      #{value => "sha | sha256 | sha512",
        desc =>
        ?T("Hash algorithm that should be used to store password in _`authentication.md#scram|SCRAM`_ format. "
           "You shouldn't change this if you already have passwords generated with "
           "a different algorithm - users that have such passwords will not be able "
           "to authenticate. The default value is 'sha'.")}},
     {auth_external_user_exists_check,
      #{value => "true | false",
        note => "added in 23.10",
        desc =>
        ?T("Supplement check for user existence based on 'mod_last' data, for authentication "
           "methods that don't have a way to reliably tell if a user exists (like is the case for "
           "'jwt' and certificate based authentication). This helps with processing offline message "
           "for those users. The default value is 'true'.")}},
     {auth_use_cache,
      #{value => "true | false",
        desc =>
            ?T("Same as _`use_cache`_, but applied to authentication cache "
               "only. If not set, the value from _`use_cache`_ will be used.")}},
     {c2s_cafile,
      #{value => ?T("Path"),
        desc =>
            [?T("Full path to a file containing one or more CA certificates "
               "in PEM format. All client certificates should be signed by "
               "one of these root CA certificates and should contain the "
               "corresponding JID(s) in 'subjectAltName' field. "
               "There is no default value."), "",
             ?T("You can use _`host_config`_ to specify this option per-vhost."), "",
             ?T("To set a specific file per listener, use the listener's _`listen-options.md#cafile|cafile`_ option. Please notice that 'c2s_cafile' overrides the listener's 'cafile' option."), ""
            ]}},
     {c2s_ciphers,
      #{value => "[Cipher, ...]",
        desc =>
            ?T("A list of OpenSSL ciphers to use for c2s connections. "
               "The default value is shown in the example below:"),
        example =>
            ["c2s_ciphers:",
             "  - HIGH",
             "  - \"!aNULL\"",
             "  - \"!eNULL\"",
             "  - \"!3DES\"",
             "  - \"@STRENGTH\""]}},
     {c2s_dhfile,
      #{value => ?T("Path"),
        desc =>
            ?T("Full path to a file containing custom DH parameters "
               "to use for c2s connections. "
               "Such a file could be created with the command \"openssl "
               "dhparam -out dh.pem 2048\". If this option is not specified, "
               "2048-bit MODP Group with 256-bit Prime Order Subgroup will be "
               "used as defined in RFC5114 Section 2.3.")}},
     {c2s_protocol_options,
      #{value => "[Option, ...]",
        desc =>
            ?T("List of general SSL options to use for c2s connections. "
               "These map to OpenSSL's 'set_options()'. The default value is "
               "shown in the example below:"),
        example =>
            ["c2s_protocol_options:",
             "  - no_sslv3",
             "  - cipher_server_preference",
             "  - no_compression"]}},
     {c2s_tls_compression,
      #{value => "true | false",
        desc =>
            ?T("Whether to enable or disable TLS compression for c2s connections. "
               "The default value is 'false'.")}},
     {ca_file,
      #{value => ?T("Path"),
        desc =>
            [?T("Path to a file of CA root certificates. "
               "The default is to use system defined file if possible."), "",
             ?T("For server connections, this 'ca_file' option is overridden by the _`s2s_cafile`_ option."), ""
            ]}},
     {captcha_cmd,
      #{value => ?T("Path | ModuleName"),
        note => "improved in 23.01",
        desc =>
            ?T("Full path to a script that generates _`basic.md#captcha|CAPTCHA`_ images. "
               "'@VERSION@' is replaced with ejabberd version number in 'XX.YY' format. "
               "'@SEMVER@' is replaced with ejabberd version number in semver format "
               "when compiled with Elixir's mix, or XX.YY format otherwise. "
               "Alternatively, it can be the name of a module that implements ejabberd CAPTCHA support. "
               "There is no default value: when this option is not "
               "set, CAPTCHA functionality is completely disabled."),
        example =>
            [{?T("When using the ejabberd installers or container image, the example captcha scripts can be used like this:"),
              ["captcha_cmd: /opt/ejabberd-@VERSION@/lib/ejabberd-@SEMVER@/priv/bin/captcha.sh"]}]}},
     {captcha_limit,
      #{value => "pos_integer() | infinity",
        desc =>
            ?T("Maximum number of _`basic.md#captcha|CAPTCHA`_ generated images per minute for "
               "any given JID. The option is intended to protect the server "
               "from CAPTCHA DoS. The default value is 'infinity'.")}},
     {captcha_host,
      #{value => "String",
        desc => ?T("Deprecated. Use _`captcha_url`_ instead.")}},
     {captcha_url,
      #{value => ?T("URL | auto | undefined"),
        note => "improved in 23.04",
        desc =>
            ?T("An URL where _`basic.md#captcha|CAPTCHA`_ requests should be sent. NOTE: you need "
               "to configure 'request_handlers' for 'ejabberd_http' listener "
               "as well. "
               "If set to 'auto', it builds the URL using a 'request_handler' "
               "already enabled, with encryption if available. "
               "If set to 'undefined', it builds the URL using "
               "the deprecated _`captcha_host`_ + /captcha. "
               "The default value is 'auto'.")}},
     {certfiles,
      #{value => "[Path, ...]",
        desc =>
            ?T("The option accepts a list of file paths (optionally with "
               "wildcards) containing either PEM certificates or PEM private "
               "keys. At startup or configuration reload, ejabberd reads all "
               "certificates from these files, sorts them, removes duplicates, "
               "finds matching private keys and then rebuilds full certificate "
               "chains for the use in TLS connections. "
               "Use this option when TLS is enabled in either of "
               "ejabberd listeners: 'ejabberd_c2s', 'ejabberd_http' and so on. "
               "NOTE: if you modify the certificate files or change the value "
               "of the option, run 'ejabberdctl reload-config' in order to "
               "rebuild and reload the certificate chains."),
        example =>
            [{?T("If you use https://letsencrypt.org[Let's Encrypt] certificates "
                 "for your domain \"domain.tld\", the configuration will look "
                 "like this:"),
              ["certfiles:",
               "  - /etc/letsencrypt/live/domain.tld/fullchain.pem",
               "  - /etc/letsencrypt/live/domain.tld/privkey.pem"]}]}},
     {cluster_backend,
      #{value => ?T("Backend"),
        desc =>
            ?T("A database backend to use for storing information about "
               "cluster. The only available value so far is 'mnesia'.")}},
     {cluster_nodes,
      #{value => "[Node, ...]",
        desc =>
            ?T("A list of Erlang nodes to connect on ejabberd startup. "
               "This option is mostly intended for ejabberd customization "
               "and sophisticated setups. The default value is an empty list.")}},
     {define_macro,
      #{value => "{MacroName: MacroValue}",
        desc =>
            ?T("Defines a "
               "_`../configuration/file-format.md#macros-in-configuration-file|macro`_. "
               "The value can be any valid arbitrary "
               "YAML value. For convenience, it's recommended to define "
               "a 'MacroName' in capital letters. Duplicated macros are not allowed. "
               "Macros are processed after additional configuration files have "
               "been included, so it is possible to use macros that are defined "
               "in configuration files included before the usage. "
               "It is possible to use a 'MacroValue' in the definition of another macro."),
        example =>
            ["define_macro:",
             "  DEBUG: debug",
             "  LOG_LEVEL: DEBUG",
             "  USERBOB:",
             "    user: bob@localhost",
             "",
             "loglevel: LOG_LEVEL",
             "",
             "acl:",
             "  admin: USERBOB"]}},
      {disable_sasl_scram_downgrade_protection,
          #{value => "true | false",
              desc =>
                ?T("Allows to disable sending data required by "
                "'XEP-0474: SASL SCRAM Downgrade Protection'. "
                "There are known buggy clients (like those that use strophejs 1.6.2) "
                "which will not be able to authenticatate when servers sends data from "
                "that specification. This options allows server to disable it to allow "
                "even buggy clients connects, but in exchange decrease MITM protection. "
                "The default value of this option is 'false' which enables this extension.")}},
     {disable_sasl_mechanisms,
      #{value => "[Mechanism, ...]",
        desc =>
            ?T("Specify a list of SASL mechanisms (such as 'DIGEST-MD5' or "
               "'SCRAM-SHA1') that should not be offered to the client. "
               "For convenience, the value of 'Mechanism' is case-insensitive. "
               "The default value is an empty list, i.e. no mechanisms "
               "are disabled by default.")}},
     {domain_balancing,
      #{value => "{Domain: Options}",
        desc =>
            ?T("An algorithm to "
               "_`../guide/clustering.md#service-load-balancing|load-balance`_ "
               "the components that are plugged "
               "on an ejabberd cluster. It means that you can plug one or several "
               "instances of the same component on each ejabberd node and that "
               "the traffic will be automatically distributed. The algorithm "
               "to deliver messages to the component(s) can be specified by "
               "this option. For any component connected as 'Domain', available "
               "'Options' are:"),
        example =>
            ["domain_balancing:",
             "  component.domain.tld:",
             "    type: destination",
             "    component_number: 5",
             "  transport.example.org:",
             "    type: bare_source"]},
      [{type,
        #{value => "random | source | destination | bare_source | bare_destination",
          desc =>
              ?T("How to deliver stanzas to connected components: "
                 "'random' - an instance is chosen at random; "
                 "'destination' - an instance is chosen by the full JID of "
                 "the packet's 'to' attribute; "
                 "'source' - by the full JID of the packet's 'from' attribute; "
                 "'bare_destination' - by the bare JID (without resource) "
                 "of the packet's 'to' attribute; "
                 "'bare_source' - by the bare JID (without resource) of the "
                 "packet's 'from' attribute is used. The default value is 'random'.")}},
       {component_number,
        #{value => "2..1000",
          desc =>
              ?T("The number of components to balance.")}}]},
     {extauth_pool_name,
      #{value => ?T("Name"),
        desc =>
            ?T("Define the pool name appendix in "
               "_`authentication.md#external-script|external auth`_, "
               "so the full pool name will be "
               "'extauth_pool_Name'. The default value is the hostname.")}},
     {extauth_pool_size,
      #{value => ?T("Size"),
        desc =>
            ?T("The option defines the number of instances of the same "
               "_`authentication.md#external-script|external auth`_ "
               "program to start for better load balancing. "
               "The default is the number of available CPU cores.")}},
     {extauth_program,
      #{value => ?T("Path"),
        desc =>
            ?T("Indicate in this option the full path to the "
               "_`authentication.md#external-script|external authentication script`_. "
               "The script must be executable by ejabberd.")}},
     {ext_api_headers,
      #{value => "Headers",
        desc =>
            ?T("String of headers (separated with commas ',') that will be "
               "provided by ejabberd when sending ReST requests. "
               "The default value is an empty string of headers: '\"\"'.")}},
     {ext_api_http_pool_size,
      #{value => "pos_integer()",
        desc =>
            ?T("Define the size of the HTTP pool, that is, the maximum number "
               "of sessions that the ejabberd ReST service will handle "
               "simultaneously. The default value is: '100'.")}},
     {ext_api_path_oauth,
      #{value => "Path",
        desc =>
            ?T("Define the base URI path when performing OAUTH ReST requests. "
               "The default value is: '\"/oauth\"'.")}},
     {ext_api_url,
      #{value => "URL",
        desc =>
            ?T("Define the base URI when performing ReST requests. "
               "The default value is: '\"http://localhost/api\"'.")}},
     {fqdn,
      #{value => ?T("Domain"),
        desc =>
            ?T("A fully qualified domain name that will be used in "
               "SASL DIGEST-MD5 authentication. The default is detected "
               "automatically.")}},
     {hide_sensitive_log_data,
      #{value => "true | false",
        desc =>
            ?T("A privacy option to not log sensitive data "
               "(mostly IP addresses). The default value "
               "is 'false' for backward compatibility.")}},
     {host_config,
      #{value => "{Host: Options}",
        desc =>
            ?T("The option is used to redefine 'Options' for "
               "_`../configuration/basic.md#virtual-hosting|virtual host`_ "
               "'Host'. "
               "In the example below LDAP authentication method "
               "will be used on virtual host 'domain.tld' and SQL method "
               "will be used on virtual host 'example.org'."),
        example =>
            ["hosts:",
             "  - domain.tld",
             "  - example.org",
             "",
             "auth_method:",
             "  - sql",
             "",
             "host_config:",
             "  domain.tld:",
             "    auth_method:",
             "      - ldap"]}},
     {include_config_file,
      #{value => "[Filename, ...\\] | {Filename: Options}",
        desc =>
            ?T("Read and "
               "_`../configuration/file-format.md#include-additional-files|include additional file`_ "
               "from 'Filename'. If the "
               "value is provided in 'pass:[{Filename: Options}]' format, the "
               "'Options' must be one of the following:")},
      [{disallow,
        #{value => "[OptionName, ...]",
          desc =>
              ?T("Disallows the usage of those options in the included "
                 "file 'Filename'. The options that match this criteria "
                 "are not accepted. The default value is an empty list.")}},
       {allow_only,
        #{value => "[OptionName, ...]",
          desc =>
              ?T("Allows only the usage of those options in the included "
                 "file 'Filename'. The options that do not match this "
                 "criteria are not accepted. The default value is to include "
                 "all options.")}}]},
     {install_contrib_modules,
      #{value => "[Module, ...]",
        note => "added in 23.10",
        desc =>
            ?T("Modules to install from "
               "_`../../developer/extending-ejabberd/modules.md#ejabberd-contrib|ejabberd-contrib`_ "
               "at start time. "
               "The default value is an empty list of modules: '[]'.")}},
     {jwt_auth_only_rule,
      #{value => ?T("AccessName"),
        desc =>
            ?T("This ACL rule defines accounts that can use only the "
               "_`authentication.md#jwt-authentication|JWT`_ auth "
               "method, even if others are also defined in the ejabberd "
               "configuration file. In other words: if there are several auth "
               "methods enabled for this host (JWT, SQL, ...), users that "
               "match this rule can only use JWT. "
               "The default value is 'none'.")}},
     {jwt_jid_field,
      #{value => ?T("FieldName"),
        desc =>
            ?T("By default, the JID is defined in the '\"jid\"' JWT field. "
               "In this option you can specify other "
               "_`authentication.md#jwt-authentication|JWT`_ "
               "field name "
               "where the JID is defined.")}},
     {jwt_key,
      #{value => ?T("FilePath"),
        desc =>
            ?T("Path to the file that contains the "
               "_`authentication.md#jwt-authentication|JWT`_ key. "
               "The default value is 'undefined'.")}},
     {language,
      #{value => ?T("Language"),
        desc =>
            ?T("Define the "
               "_`../configuration/basic.md#default-language|default language`_ "
               "of server strings "
               "that can be seen by XMPP clients. If an XMPP client does not "
               "possess 'xml:lang' attribute, the specified language is used. "
               "The default value is '\"en\"'. ")}},
     {ldap_servers,
      #{value => "[Host, ...]",
        desc =>
            ?T("A list of IP addresses or DNS names of your LDAP servers. "
               "The default value is '[localhost]'.")}},
     {ldap_backups,
      #{value => "[Host, ...]",
        desc =>
            ?T("A list of IP addresses or DNS names of LDAP backup servers. "
               "When no servers listed in _`ldap_servers`_ option are reachable, "
               "ejabberd will try to connect to these backup servers. "
               "The default is an empty list, i.e. no backup servers specified. "
               "WARNING: ejabberd doesn't try to reconnect back to the main "
               "servers when they become operational again, so the only way "
               "to restore these connections is to restart ejabberd. This "
               "limitation might be fixed in future releases.")}},
     {ldap_encrypt,
      #{value => "tls | none",
        desc =>
            ?T("Whether to encrypt LDAP connection using TLS or not. "
               "The default value is 'none'. NOTE: STARTTLS encryption "
               "is not supported.")}},
     {ldap_tls_certfile,
      #{value => ?T("Path"),
        desc =>
            ?T("A path to a file containing PEM encoded certificate "
               "along with PEM encoded private key. This certificate "
               "will be provided by ejabberd when TLS enabled for "
               "LDAP connections. There is no default value, which means "
               "no client certificate will be sent.")}},
     {ldap_tls_verify,
      #{value => "false | soft | hard",
        desc =>
            ?T("This option specifies whether to verify LDAP server "
               "certificate or not when TLS is enabled. When 'hard' is set, "
               "ejabberd doesn't proceed if the certificate is invalid. "
               "When 'soft' is set, ejabberd proceeds even if the check has failed. "
               "The default is 'false', which means no checks are performed.")}},
     {ldap_tls_cacertfile,
      #{value => ?T("Path"),
        desc =>
            ?T("A path to a file containing PEM encoded CA certificates. "
               "This option is required when TLS verification is enabled.")}},
     {ldap_tls_depth,
      #{value => ?T("Number"),
        desc =>
            ?T("Specifies the maximum verification depth when TLS verification "
               "is enabled, i.e. how far in a chain of certificates the "
               "verification process can proceed before the verification "
               "is considered to be failed. Peer certificate = 0, "
               "CA certificate = 1, higher level CA certificate = 2, etc. "
               "The value '2' thus means that a chain can at most contain "
               "peer cert, CA cert, next CA cert, and an additional CA cert. "
               "The default value is '1'.")}},
     {ldap_port,
      #{value => "1..65535",
        desc =>
            ?T("Port to connect to your LDAP server. The default port is "
               "'389' if encryption is disabled and '636' if encryption is "
               "enabled.")}},
     {ldap_rootdn,
      #{value => "RootDN",
        desc =>
            ?T("Bind Distinguished Name. The default value is an empty "
               "string, which means \"anonymous connection\".")}},
     {ldap_password,
      #{value => ?T("Password"),
        desc =>
            ?T("Bind password. The default value is an empty string.")}},
     {ldap_deref_aliases,
      #{value => "never | always | finding | searching",
        desc =>
            ?T("Whether to dereference aliases or not. "
               "The default value is 'never'.")}},
     {ldap_base,
      #{value => "Base",
        desc =>
            ?T("LDAP base directory which stores users accounts. "
               "There is no default value: you must set the option "
               "in order for LDAP connections to work properly.")}},
     {ldap_uids,
      #{value => "[Attr\\] | {Attr: AttrFormat}",
        desc =>
            ?T("LDAP attributes which hold a list of attributes to use "
               "as alternatives for getting the JID, where 'Attr' is "
               "an LDAP attribute which holds the user's part of the JID and "
               "'AttrFormat' must contain one and only one pattern variable "
               "\"%u\" which will be replaced by the user's part of the JID. "
               "For example, \"%u@example.org\". If the value is in the form "
               "of '[Attr]' then 'AttrFormat' is assumed to be \"%u\".")}},
     {ldap_filter,
      #{value => ?T("Filter"),
        desc =>
            ?T("An LDAP filter as defined in "
               "https://tools.ietf.org/html/rfc4515[RFC4515]. "
               "There is no default value. Example: "
               "\"(&(objectClass=shadowAccount)(memberOf=XMPP Users))\". "
               "NOTE: don't forget to close brackets and don't use superfluous "
               "whitespaces. Also you must not use \"uid\" attribute in the "
               "filter because this attribute will be appended to the filter "
               "automatically.")}},
     {ldap_dn_filter,
      #{value => "{Filter: FilterAttrs}",
        desc =>
            ?T("This filter is applied on the results returned by the main "
               "filter. The filter performs an additional LDAP lookup to make "
               "the complete result. This is useful when you are unable to "
               "define all filter rules in 'ldap_filter'. You can define "
               "\"%u\", \"%d\", \"%s\" and \"%D\" pattern variables in 'Filter': "
               "\"%u\" is replaced by a user's part of the JID, \"%d\" is "
               "replaced by the corresponding domain (virtual host), all \"%s\" "
               "variables are consecutively replaced by values from the attributes "
               "in 'FilterAttrs' and \"%D\" is replaced by Distinguished Name from "
               "the result set. There is no default value, which means the "
               "result is not filtered. WARNING: Since this filter makes "
               "additional LDAP lookups, use it only as the last resort: "
               "try to define all filter rules in _`ldap_filter`_ option if possible."),
        example =>
            ["ldap_dn_filter:",
             "  \"(&(name=%s)(owner=%D)(user=%u@%d))\": [sn]"]}},
     {log_rotate_count,
      #{value => ?T("Number"),
        desc =>
            ?T("The number of rotated log files to keep. "
               "The default value is '1', which means that only keeps "
               "`ejabberd.log.0`, `error.log.0` and `crash.log.0`.")}},
     {log_rotate_size,
      #{value => "pos_integer() | infinity",
        desc =>
            ?T("The size (in bytes) of a log file to trigger rotation. "
               "If set to 'infinity', log rotation is disabled. "
               "The default value is '10485760' (that is, 10 Mb).")}},
     {log_burst_limit_count,
      #{value => ?T("Number"),
        note => "added in 22.10",
        desc =>
            ?T("The number of messages to accept in "
               "`log_burst_limit_window_time` period before starting to "
               "drop them. Default 500")}},
     {log_burst_limit_window_time,
      #{value => ?T("Number"),
        note => "added in 22.10",
        desc =>
            ?T("The time period to rate-limit log messages "
               "by. Defaults to 1 second.")}},
     {log_modules_fully,
      #{value => "[Module, ...]",
        note => "added in 23.01",
        desc =>
            ?T("List of modules that will log everything "
               "independently from the general loglevel option.")}},
     {max_fsm_queue,
      #{value => ?T("Size"),
        desc =>
            ?T("This option specifies the maximum number of elements "
               "in the queue of the FSM (Finite State Machine). Roughly "
               "speaking, each message in such queues represents one "
               "XML stanza queued to be sent into its relevant outgoing "
               "stream. If queue size reaches the limit (because, for "
               "example, the receiver of stanzas is too slow), the FSM "
               "and the corresponding connection (if any) will be terminated "
               "and error message will be logged. The reasonable value for "
               "this option depends on your hardware configuration. "
               "The allowed values are positive integers. "
               "The default value is '10000'.")}},
     {negotiation_timeout,
      #{value => "timeout()",
        desc =>
            ?T("Time to wait for an XMPP stream negotiation to complete. "
               "When timeout occurs, the corresponding XMPP stream is closed. "
               "The default value is '120' seconds.")}},
     {net_ticktime,
      #{value => "timeout()",
        desc =>
            ?T("This option can be used to tune tick time parameter of "
               "'net_kernel'. It tells Erlang VM how often nodes should check "
               "if intra-node communication was not interrupted. This option "
               "must have identical value on all nodes, or it will lead to subtle "
               "bugs. Usually leaving default value of this is option is best, "
               "tweak it only if you know what you are doing. "
               "The default value is '1 minute'.")}},
     {new_sql_schema,
      #{value => "true | false",
        desc =>
            {?T("Whether to use the "
                "_`database.md#default-and-new-schemas|new SQL schema`_. "
                "All schemas are located "
                "at <https://github.com/processone/ejabberd/tree/~s/sql>. "
                "There are two schemas available. The default legacy schema "
                "stores one XMPP domain into one ejabberd database. "
                "The 'new' schema can handle several XMPP domains in a "
                "single ejabberd database. Using this 'new' schema is best when "
                "serving several XMPP domains and/or changing domains from "
                "time to time. This avoid need to manage several databases and "
                "handle complex configuration changes. The default depends on "
                "configuration flag '--enable-new-sql-schema' which is set "
                "at compile time."),
             [binary:part(ejabberd_config:version(), {0,5})]}}},
     {update_sql_schema,
      #{value => "true | false",
        note => "updated in 24.06",
        desc =>
            ?T("Allow ejabberd to update SQL schema. "
               "This option was added in ejabberd 23.10, "
               "and enabled by default since 24.06. "
               "The default value is 'true'.")}},
     {update_sql_schema_timeout,
      #{value => "timeout()",
        note => "added in 24.07",
        desc =>
        ?T("Time allocated to SQL schema update queries. "
           "The default value is set to 5 minutes.")}},
     {oauth_access,
      #{value => ?T("AccessName"),
        desc => ?T("By default creating OAuth tokens is not allowed. "
                   "To define which users can create OAuth tokens, "
                   "you can refer to an ejabberd access rule in the "
                   "'oauth_access' option. Use 'all' to allow everyone "
                   "to create tokens.")}},
     {oauth_cache_life_time,
      #{value => "timeout()",
        desc =>
            ?T("Same as _`cache_life_time`_, but applied to OAuth cache "
               "only. If not set, the value from _`cache_life_time`_ will be used.")}},
     {oauth_cache_missed,
      #{value => "true | false",
        desc =>
            ?T("Same as _`cache_missed`_, but applied to OAuth cache "
               "only. If not set, the value from _`cache_missed`_ will be used.")}},
     {oauth_cache_rest_failure_life_time,
      #{value => "timeout()",
        note => "added in 21.01",
        desc =>
            ?T("The time that a failure in OAuth ReST is cached. "
               "The default value is 'infinity'.")}},
     {oauth_cache_size,
      #{value => "pos_integer() | infinity",
        desc =>
            ?T("Same as _`cache_size`_, but applied to OAuth cache "
               "only. If not set, the value from _`cache_size`_ will be used.")}},
     {oauth_client_id_check,
      #{value => "allow | db | deny",
        desc =>
            ?T("Define whether the client authentication is always allowed, "
               "denied, or it will depend if the client ID is present in the "
               "database. The default value is 'allow'.")}},
     {oauth_use_cache,
      #{value => "true | false",
        desc =>
            ?T("Same as _`use_cache`_, but applied to OAuth cache "
               "only. If not set, the value from _`use_cache`_ will be used.")}},
     {oauth_db_type,
      #{value => "mnesia | sql",
        desc =>
            ?T("Database backend to use for OAuth authentication. "
               "The default value is picked from _`default_db`_ option, or "
               "if it's not set, 'mnesia' will be used.")}},
     {oauth_expire,
      #{value => "timeout()",
        desc =>
            ?T("Time during which the OAuth token is valid, in seconds. "
               "After that amount of time, the token expires and the delegated "
               "credential cannot be used and is removed from the database. "
               "The default is '4294967' seconds.")}},
     {oom_killer,
      #{value => "true | false",
        desc =>
            ?T("Enable or disable OOM (out-of-memory) killer. "
               "When system memory raises above the limit defined in "
               "_`oom_watermark`_ option, ejabberd triggers OOM killer "
               "to terminate most memory consuming Erlang processes. "
               "Note that in order to maintain functionality, ejabberd only "
               "attempts to kill transient processes, such as those managing "
               "client sessions, s2s or database connections. "
               "The default value is 'true'.")}},
     {oom_queue,
      #{value => ?T("Size"),
        desc =>
            ?T("Trigger OOM killer when some of the running Erlang processes "
               "have messages queue above this 'Size'. Note that "
               "such processes won't be killed if _`oom_killer`_ option is set "
               "to 'false' or if 'oom_watermark' is not reached yet.")}},
     {oom_watermark,
      #{value => ?T("Percent"),
        desc =>
            ?T("A percent of total system memory consumed at which "
               "OOM killer should be activated with some of the processes "
               "possibly be killed (see _`oom_killer`_ option). Later, when "
               "memory drops below this 'Percent', OOM killer is deactivated. "
               "The default value is '80' percents.")}},
     {outgoing_s2s_families,
      #{value => "[ipv6 | ipv4, ...]",
        note => "changed in 23.01",
        desc =>
            ?T("Specify which address families to try, in what order. "
               "The default is '[ipv6, ipv4]' which means it first tries "
               "connecting with IPv6, if that fails it tries using IPv4. "
               "This option is obsolete and irrelevant when using ejabberd 23.01 "
               "and Erlang/OTP 22, or newer versions of them.")}},
     {outgoing_s2s_ipv4_address,
      #{value => "Address",
        note => "added in 20.12",
        desc =>
            ?T("Specify the IPv4 address that will be used when establishing "
               "an outgoing S2S IPv4 connection, for example \"127.0.0.1\". "
               "The default value is 'undefined'.")}},
     {outgoing_s2s_ipv6_address,
      #{value => "Address",
        note => "added in 20.12",
        desc =>
            ?T("Specify the IPv6 address that will be used when establishing "
               "an outgoing S2S IPv6 connection, for example "
               "\"::FFFF:127.0.0.1\". The default value is 'undefined'.")}},
     {outgoing_s2s_port,
      #{value => "1..65535",
        desc =>
            ?T("A port number to use for outgoing s2s connections when the target "
               "server doesn't have an SRV record. The default value is '5269'.")}},
     {outgoing_s2s_timeout,
      #{value => "timeout()",
        desc =>
            ?T("The timeout in seconds for outgoing S2S connection attempts. "
               "The default value is '10' seconds.")}},
     {pam_service,
      #{value => ?T("Name"),
        desc =>
            ?T("This option defines the "
                "_`authentication.md#pam-authentication|PAM`_ "
               "service name. Refer to the PAM "
               "documentation of your operation system for more information. "
               "The default value is 'ejabberd'.")}},
     {pam_userinfotype,
      #{value => "username | jid",
        desc =>
            ?T("This option defines what type of information about the "
               "user ejabberd provides to the "
               "_`authentication.md#pam-authentication|PAM`_ "
               "service: only the username, "
               "or the user's JID. Default is 'username'.")}},
     {pgsql_users_number_estimate,
      #{value => "true | false",
        desc =>
            ?T("Whether to use PostgreSQL estimation when counting registered "
               "users. The default value is 'false'.")}},
     {queue_dir,
      #{value => ?T("Directory"),
        desc =>
            ?T("If _`queue_type`_ option is set to 'file', use this 'Directory' "
               "to store file queues. The default is to keep queues inside "
               "Mnesia directory.")}},
     {redis_connect_timeout,
      #{value => "timeout()",
        desc =>
            ?T("A timeout to wait for the connection to be re-established "
               "to the _`database.md#redis|Redis`_ "
               "server. The default is '1 second'.")}},
     {redis_db,
      #{value => ?T("Number"),
        desc => ?T("_`database.md#redis|Redis`_ "
                   "database number. The default is '0'.")}},
     {redis_password,
      #{value => ?T("Password"),
        desc =>
            ?T("The password to the _`database.md#redis|Redis`_ server. "
               "The default is an empty string, i.e. no password.")}},
     {redis_pool_size,
      #{value => ?T("Number"),
        desc =>
            ?T("The number of simultaneous connections to the "
                "_`database.md#redis|Redis`_ server. "
               "The default value is '10'.")}},
     {redis_port,
      #{value => "1..65535",
        desc =>
            ?T("The port where the _`database.md#redis|Redis`_ "
               " server is accepting connections. "
               "The default is '6379'.")}},
     {redis_queue_type,
      #{value => "ram | file",
        desc =>
            ?T("The type of request queue for the "
               "_`database.md#redis|Redis`_ server. "
               "See description of _`queue_type`_ option for the explanation. "
               "The default value is the value defined in _`queue_type`_ "
               "or 'ram' if the latter is not set.")}},
     {redis_server,
      #{value => ?T("Hostname"),
        desc =>
            ?T("A hostname or an IP address of the "
               "_`database.md#redis|Redis`_ server."
               "The default is 'localhost'.")}},
     {registration_timeout,
      #{value => "timeout()",
        desc =>
            ?T("This is a global option for module _`mod_register`_. "
               "It limits the frequency of registrations from a given "
               "IP or username. So, a user that tries to register a "
               "new account from the same IP address or JID during "
               "this time after their previous registration "
               "will receive an error with the corresponding explanation. "
               "To disable this limitation, set the value to 'infinity'. "
               "The default value is '600 seconds'.")}},
     {resource_conflict,
      #{value => "setresource | closeold | closenew",
        desc =>
            ?T("NOTE: this option is deprecated and may be removed "
               "anytime in the future versions. The possible values "
               "match exactly the three possibilities described in "
               "https://tools.ietf.org/html/rfc6120#section-7.7.2.2"
               "[XMPP Core: section 7.7.2.2]. "
               "The default value is 'closeold'. If the client "
               "uses old Jabber Non-SASL authentication (XEP-0078), "
               "then this option is not respected, and the action performed "
               "is 'closeold'.")}},
     {router_cache_life_time,
      #{value => "timeout()",
        desc =>
            ?T("Same as _`cache_life_time`_, but applied to routing table cache "
               "only. If not set, the value from _`cache_life_time`_ will be used.")}},
     {router_cache_missed,
      #{value => "true | false",
        desc =>
            ?T("Same as _`cache_missed`_, but applied to routing table cache "
               "only. If not set, the value from _`cache_missed`_ will be used.")}},
     {router_cache_size,
      #{value => "pos_integer() | infinity",
        desc =>
            ?T("Same as _`cache_size`_, but applied to routing table cache "
               "only. If not set, the value from _`cache_size`_ will be used.")}},
     {router_db_type,
      #{value => "mnesia | redis | sql",
        desc =>
            ?T("Database backend to use for routing information. "
               "The default value is picked from _`default_ram_db`_ option, or "
               "if it's not set, 'mnesia' will be used.")}},
     {router_use_cache,
      #{value => "true | false",
        desc =>
            ?T("Same as _`use_cache`_, but applied to routing table cache "
               "only. If not set, the value from _`use_cache`_ will be used.")}},
     {rpc_timeout,
      #{value => "timeout()",
        desc =>
            ?T("A timeout for remote function calls between nodes "
               "in an ejabberd cluster. You should probably never change "
               "this value since those calls are used for internal needs "
               "only. The default value is '5' seconds.")}},
     {s2s_access,
      #{value => ?T("Access"),
        desc =>
            ?T("This _`basic.md#access-rules|Access Rule`_ defines to "
               "what remote servers can s2s connections be established. "
               "The default value is 'all'; no restrictions are applied, it is"
               " allowed to connect s2s to/from all remote servers.")}},
     {s2s_cafile,
      #{value => ?T("Path"),
        desc =>
            [?T("A path to a file with CA root certificates that will "
               "be used to authenticate s2s connections. If not set, "
               "the value of _`ca_file`_ will be used."), "",
             ?T("You can use _`host_config`_ to specify this option per-vhost."), ""
            ]}},
     {s2s_ciphers,
      #{value => "[Cipher, ...]",
        desc =>
            ?T("A list of OpenSSL ciphers to use for s2s connections. "
               "The default value is shown in the example below:"),
        example =>
            ["s2s_ciphers:",
             "  - HIGH",
             "  - \"!aNULL\"",
             "  - \"!eNULL\"",
             "  - \"!3DES\"",
             "  - \"@STRENGTH\""]}},
     {s2s_dhfile,
      #{value => ?T("Path"),
        desc =>
            ?T("Full path to a file containing custom DH parameters "
               "to use for s2s connections. "
               "Such a file could be created with the command \"openssl "
               "dhparam -out dh.pem 2048\". If this option is not specified, "
               "2048-bit MODP Group with 256-bit Prime Order Subgroup will be "
               "used as defined in RFC5114 Section 2.3.")}},
     {s2s_protocol_options,
      #{value => "[Option, ...]",
        desc =>
            ?T("List of general SSL options to use for s2s connections. "
               "These map to OpenSSL's 'set_options()'. The default value is "
               "shown in the example below:"),
        example =>
            ["s2s_protocol_options:",
             "  - no_sslv3",
             "  - cipher_server_preference",
             "  - no_compression"]}},
     {s2s_tls_compression,
      #{value => "true | false",
        desc =>
            ?T("Whether to enable or disable TLS compression for s2s connections. "
               "The default value is 'false'.")}},
     {s2s_dns_retries,
      #{value => ?T("Number"),
        desc =>
            ?T("DNS resolving retries. The default value is '2'.")}},
     {s2s_dns_timeout,
      #{value => "timeout()",
        desc =>
            ?T("The timeout for DNS resolving. The default value is '10' seconds.")}},
     {s2s_max_retry_delay,
      #{value => "timeout()",
        desc =>
            ?T("The maximum allowed delay for s2s connection retry to connect after a "
               "failed connection attempt. The default value is '300' seconds "
               "(5 minutes).")}},
     {s2s_queue_type,
      #{value => "ram | file",
        desc =>
            ?T("The type of a queue for s2s packets. "
               "See description of _`queue_type`_ option for the explanation. "
               "The default value is the value defined in _`queue_type`_ "
               "or 'ram' if the latter is not set.")}},
     {s2s_timeout,
      #{value => "timeout()",
        desc =>
            ?T("A time to wait before closing an idle s2s connection. "
               "The default value is '1' hour.")}},
     {s2s_use_starttls,
      #{value => "true | false | optional | required",
        desc =>
            ?T("Whether to use STARTTLS for s2s connections. "
               "The value of 'false' means STARTTLS is prohibited. "
               "The value of 'true' or 'optional' means STARTTLS is enabled "
               "but plain connections are still allowed. And the value of "
               "'required' means that only STARTTLS connections are allowed. "
               "The default value is 'false' (for historical reasons).")}},
     {s2s_zlib,
      #{value => "true | false",
        desc =>
            ?T("Whether to use 'zlib' compression (as defined in "
               "https://xmpp.org/extensions/xep-0138.html[XEP-0138]) or not. "
               "The default value is 'false'. WARNING: this type "
               "of compression is nowadays considered insecure.")}},
     {shaper,
      #{value => "{ShaperName: Rate}",
        desc =>
            ?T("The option defines a set of "
               "_`../configuration/basic.md#shapers|shapers`_. "
               "Every shaper is assigned "
               "a name 'ShaperName' that can be used in other parts of the "
               "configuration file, such as _`shaper_rules`_ option. The shaper "
               "itself is defined by its 'Rate', where 'Rate' stands for the "
               "maximum allowed incoming rate in **bytes** per second. "
               "When a connection exceeds this limit, ejabberd stops reading "
               "from the socket until the average rate is again below the "
               "allowed maximum. In the example below shaper 'normal' limits "
               "the traffic speed to 1,000 bytes/sec and shaper 'fast' limits "
               "the traffic speed to 50,000 bytes/sec:"),
        example =>
            ["shaper:",
             "  normal: 1000",
             "  fast: 50000"]}},
     {shaper_rules,
      #{value => "{ShaperRuleName: {Number|ShaperName: ACLRule|ACLName}}",
        desc =>
            ?T("This option defines "
               "_`../configuration/basic.md#shaper-rules|shaper rules`_ "
               "to use for matching user/hosts. "
               "Semantics is similar to _`access_rules`_ option, the only difference is "
               "that instead using 'allow' or 'deny', a name of a shaper (defined in "
               "_`shaper`_ option) or a positive number should be used."),
        example =>
            ["shaper_rules:",
             "  connections_limit:",
             "    10:",
             "      user: peter@example.com",
             "    100: admin",
             "    5: all",
             "  download_speed:",
             "    fast: admin",
             "    slow: anonymous_users",
             "    normal: all",
             "  log_days: 30"]}},
     {sm_cache_life_time,
      #{value => "timeout()",
        desc =>
            ?T("Same as _`cache_life_time`_, but applied to client sessions table cache "
               "only. If not set, the value from _`cache_life_time`_ will be used.")}},
     {sm_cache_missed,
      #{value => "true | false",
        desc =>
            ?T("Same as _`cache_missed`_, but applied to client sessions table cache "
               "only. If not set, the value from _`cache_missed`_ will be used.")}},
     {sm_cache_size,
      #{value => "pos_integer() | infinity",
        desc =>
            ?T("Same as _`cache_size`_, but applied to client sessions table cache "
               "only. If not set, the value from _`cache_size`_ will be used.")}},
     {sm_db_type,
      #{value => "mnesia | redis | sql",
        desc =>
            ?T("Database backend to use for client sessions information. "
               "The default value is picked from _`default_ram_db`_ option, or "
               "if it's not set, 'mnesia' will be used.")}},
     {sm_use_cache,
      #{value => "true | false",
        desc =>
            ?T("Same as _`use_cache`_, but applied to client sessions table cache "
               "only. If not set, the value from _`use_cache`_ will be used.")}},
     {sql_type,
      #{value => "mssql | mysql | odbc | pgsql | sqlite",
        desc =>
            ?T("The type of an SQL connection. The default is 'odbc'.")}},
     {sql_connect_timeout,
      #{value => "timeout()",
        desc =>
            ?T("A time to wait for connection to an SQL server to be "
               "established. The default value is '5' seconds.")}},
     {sql_database,
      #{value => ?T("Database"),
        desc =>
            ?T("An SQL database name. For SQLite this must be a full "
               "path to a database file. The default value is 'ejabberd'.")}},
     {sql_keepalive_interval,
      #{value => "timeout()",
        desc =>
            ?T("An interval to make a dummy SQL request to keep alive the "
               "connections to the database. There is no default value, so no "
               "keepalive requests are made.")}},
     {sql_odbc_driver,
      #{value => "Path",
        note => "added in 20.12",
        desc =>
            ?T("Path to the ODBC driver to use to connect to a Microsoft SQL "
               "Server database. This option only applies if the _`sql_type`_ "
               "option is set to 'mssql' and _`sql_server`_  is not an ODBC "
               "connection string. The default value is: 'libtdsodbc.so'")}},
     {sql_password,
      #{value => ?T("Password"),
        desc =>
            ?T("The password for SQL authentication. The default is empty string.")}},
     {sql_pool_size,
      #{value => ?T("Size"),
        desc =>
            ?T("Number of connections to the SQL server that ejabberd will "
               "open for each virtual host. The default value is 10. WARNING: "
               "for SQLite this value is '1' by default and it's not recommended "
               "to change it due to potential race conditions.")}},
     {sql_port,
      #{value => "1..65535",
        desc =>
            ?T("The port where the SQL server is accepting connections. "
               "The default is '3306' for MySQL, '5432' for PostgreSQL and "
               "'1433' for MS SQL. The option has no effect for SQLite.")}},
     {sql_prepared_statements,
      #{value => "true | false",
        note => "added in 20.01",
        desc =>
	    ?T("This option is 'true' by default, and is useful to disable "
	       "prepared statements. The option is valid for PostgreSQL and MySQL.")}},
     {sql_flags,
      #{value => "[mysql_alternative_upsert]",
        note => "added in 24.02",
        desc =>
	    ?T("This option accepts a list of SQL flags, and is empty by default. "
               "'mysql_alternative_upsert' forces the alternative upsert implementation in MySQL.")}},
     {sql_query_timeout,
      #{value => "timeout()",
        desc =>
            ?T("A time to wait for an SQL query response. "
               "The default value is '60' seconds.")}},
     {sql_queue_type,
      #{value => "ram | file",
        desc =>
            ?T("The type of a request queue for the SQL server. "
               "See description of _`queue_type`_ option for the explanation. "
               "The default value is the value defined in _`queue_type`_ "
               "or 'ram' if the latter is not set.")}},
     {sql_server,
      #{value => "Host | IP Address | ODBC Connection String | Unix Socket Path",
        note => "improved in 24.06",
        desc =>
            ?T("The hostname or IP address of the SQL server. For _`sql_type`_ "
               "'mssql' or 'odbc' this can also be an ODBC connection string. "
               "When _`sql_type`_ is 'mysql' or 'pgsql', this can be the path to "
               "a unix domain socket expressed like: \"unix:/path/to/socket\"."
               "The default value is 'localhost'.")}},
     {sql_ssl,
      #{value => "true | false",
        note => "improved in 20.03",
        desc =>
            ?T("Whether to use SSL encrypted connections to the "
               "SQL server. The option is only available for MySQL, MS SQL and "
               "PostgreSQL. The default value is 'false'.")}},
     {sql_ssl_cafile,
      #{value => ?T("Path"),
        desc =>
            ?T("A path to a file with CA root certificates that will "
               "be used to verify SQL connections. Implies _`sql_ssl`_ "
               "and _`sql_ssl_verify`_ options are set to 'true'. "
               "There is no default which means "
               "certificate verification is disabled. "
               "This option has no effect for MS SQL.")}},
     {sql_ssl_certfile,
      #{value => ?T("Path"),
        desc =>
            ?T("A path to a certificate file that will be used "
               "for SSL connections to the SQL server. Implies _`sql_ssl`_ "
               "option is set to 'true'. There is no default which means "
               "ejabberd won't provide a client certificate to the SQL "
               "server. "
               "This option has no effect for MS SQL.")}},
     {sql_ssl_verify,
      #{value => "true | false",
        desc =>
            ?T("Whether to verify SSL connection to the SQL server against "
               "CA root certificates defined in _`sql_ssl_cafile`_ option. "
               "Implies _`sql_ssl`_ option is set to 'true'. "
               "This option has no effect for MS SQL. "
               "The default value is 'false'.")}},
     {sql_start_interval,
      #{value => "timeout()",
        desc =>
            ?T("A time to wait before retrying to restore failed SQL connection. "
               "The default value is '30' seconds.")}},
     {sql_username,
      #{value => ?T("Username"),
        desc =>
            ?T("A user name for SQL authentication. "
               "The default value is 'ejabberd'.")}},
     {trusted_proxies,
      #{value => "all | [Network1, Network2, ...]",
        desc =>
            ?T("Specify what proxies are trusted when an HTTP request "
               "contains the header 'X-Forwarded-For'. You can specify "
               "'all' to allow all proxies, or specify a list of IPs, "
               "possibly with masks. The default value is an empty list. "
               "Using this option you can know the real IP "
               "of the request, for admin purpose, or security configuration "
               "(for example using 'mod_fail2ban'). IMPORTANT: The proxy MUST "
               "be configured to set the 'X-Forwarded-For' header if you "
               "enable this option as, otherwise, the client can set it "
               "itself and as a result the IP value cannot be trusted for "
               "security rules in ejabberd.")}},
     {validate_stream,
      #{value => "true | false",
        desc =>
            ?T("Whether to validate any incoming XML packet according "
               "to the schemas of "
               "https://github.com/processone/xmpp#supported-xmpp-elements"
               "[supported XMPP extensions]. WARNING: the validation is only "
               "intended for the use by client developers - don't enable "
               "it in production environment. The default value is 'false'.")}},
     {websocket_origin,
      #{value => "ignore | URL",
        desc =>
            ?T("This option enables validation for 'Origin' header to "
               "protect against connections from other domains than given "
               "in the configuration file. In this way, the lower layer load "
               "balancer can be chosen for a specific ejabberd implementation "
               "while still providing a secure WebSocket connection. "
               "The default value is 'ignore'. An example value of the 'URL' is "
               "\"https://test.example.org:8081\".")}},
     {websocket_ping_interval,
      #{value => "timeout()",
        desc =>
            ?T("Defines time between pings sent by the server to a client "
               "(WebSocket level protocol pings are used for this) to keep "
               "a connection active. If the client doesn't respond to two "
               "consecutive pings, the connection will be assumed as closed. "
               "The value of '0' can be used to disable the feature. This option "
               "makes the server sending pings only for connections using the RFC "
               "compliant protocol. For older style connections the server "
               "expects that whitespace pings would be used for this purpose. "
               "The default value is '60' seconds.")}},
     {websocket_timeout,
      #{value => "timeout()",
        desc =>
            ?T("Amount of time without any communication after which the "
               "connection would be closed. The default value is '300' seconds.")}}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
