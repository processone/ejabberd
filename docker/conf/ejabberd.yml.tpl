###
###               ejabberd configuration file
###
###

### The parameters used in this configuration file are explained in more detail
### in the ejabberd Installation and Operation Guide.
### Please consult the Guide in case of doubts, it is included with
### your copy of ejabberd, and is also available online at
### http://www.process-one.net/en/ejabberd/docs/

###   =======
###   LOGGING

loglevel: {{ env['EJABBERD_LOGLEVEL'] or 4 }}
log_rotate_size: 10485760
log_rotate_count: 0
log_rate_limit: 100

## watchdog_admins:
##   - "bob@example.com"

###   ================
###   SERVED HOSTNAMES

hosts:
{%- for xmpp_domain in env['XMPP_DOMAIN'].split() %}
  - "{{ xmpp_domain }}"
{%- endfor %}

##
## route_subdomains: Delegate subdomains to other XMPP servers.
## For example, if this ejabberd serves example.org and you want
## to allow communication with an XMPP server called im.example.org.
##
## route_subdomains: s2s

###   ===============
###   LISTENING PORTS

listen:
  -
    port: 5222
    module: ejabberd_c2s
    {%- if env['EJABBERD_STARTTLS'] == "true" %}
    starttls_required: true
    {%- endif %}
    protocol_options:
      - "no_sslv3"
    {%- if env.get('EJABBERD_PROTOCOL_OPTIONS_TLSV1', "false") == "false" %}
      - "no_tlsv1"
    {%- endif %}
    {%- if env.get('EJABBERD_PROTOCOL_OPTIONS_TLSV1_1', "true") == "false" %}
      - "no_tlsv1_1"
    {%- endif %}
    max_stanza_size: 65536
    shaper: c2s_shaper
    access: c2s
    ciphers: "{{ env.get('EJABBERD_CIPHERS', 'HIGH:!aNULL:!3DES') }}"
    {%- if env.get('EJABBERD_DHPARAM', false) == "true" %}
    dhfile: "/opt/ejabberd/ssl/dh.pem"
    {%- endif %}
  -
    port: 5269
    module: ejabberd_s2s_in
  -
    port: 4560
    module: ejabberd_xmlrpc
    access_commands:
      configure:
        all: []

  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      "/websocket": ejabberd_http_ws
    ##  "/pub/archive": mod_http_fileserver
    web_admin: true
    http_bind: true
    ## register: true
    captcha: true
    {%- if env['EJABBERD_HTTPS'] == "true" %}
    tls: true
    certfile: "/opt/ejabberd/ssl/host.pem"
    {% endif %}
  -
    port: 5443
    module: ejabberd_http
    request_handlers:
      "": mod_http_upload
    {%- if env['EJABBERD_HTTPS'] == "true" %}
    tls: true
    certfile: "/opt/ejabberd/ssl/host.pem"
    {% endif %}


###   SERVER TO SERVER
###   ================

{%- if env['EJABBERD_S2S_SSL'] == "true" %}
s2s_use_starttls: required
s2s_certfile: "/opt/ejabberd/ssl/host.pem"
s2s_protocol_options:
  - "no_sslv3"
  {%- if env.get('EJABBERD_PROTOCOL_OPTIONS_TLSV1', "false") == "false" %}
  - "no_tlsv1"
  {%- endif %}
  {%- if env.get('EJABBERD_PROTOCOL_OPTIONS_TLSV1_1', "true") == "false" %}
  - "no_tlsv1_1"
  {%- endif %}
s2s_ciphers: "{{ env.get('EJABBERD_CIPHERS', 'HIGH:!aNULL:!3DES') }}"
{%- if env.get('EJABBERD_DHPARAM', false) == "true" %}
s2s_dhfile: "/opt/ejabberd/ssl/dh.pem"
{%- endif %}
{% endif %}

###   ==============
###   AUTHENTICATION

auth_method:
{%- for auth_method in env.get('EJABBERD_AUTH_METHOD', 'internal').split() %}
  - {{ auth_method }}
{%- endfor %}

{%- if 'anonymous' in env.get('EJABBERD_AUTH_METHOD', 'internal').split() %}
anonymous_protocol: login_anon
allow_multiple_connections: true
{%- endif %}


## LDAP authentication

{%- if 'ldap' in env.get('EJABBERD_AUTH_METHOD', 'internal').split() %}

ldap_servers:
{%- for ldap_server in env.get('EJABBERD_LDAP_SERVERS', 'internal').split() %}
  - "{{ ldap_server }}"
{%- endfor %}

ldap_encrypt: {{ env.get('EJABBERD_LDAP_ENCRYPT', 'none') }}
ldap_tls_verify: {{ env.get('EJABBERD_LDAP_TLS_VERIFY', 'false') }}

{%- if env['EJABBERD_LDAP_TLS_CACERTFILE'] %}
ldap_tls_cacertfile: "{{ env['EJABBERD_LDAP_TLS_CACERTFILE'] }}"
{%- endif %}

ldap_tls_depth: {{ env.get('EJABBERD_LDAP_TLS_DEPTH', 1) }}

{%- if env['EJABBERD_LDAP_PORT'] %}
ldap_port: {{ env['EJABBERD_LDAP_PORT'] }}
{%- endif %}

{%- if env['EJABBERD_LDAP_ROOTDN'] %}
ldap_rootdn: "{{ env['EJABBERD_LDAP_ROOTDN'] }}"
{%- endif %}

{%- if env['EJABBERD_LDAP_PASSWORD'] %}
ldap_password: "{{ env['EJABBERD_LDAP_PASSWORD'] }}"
{%- endif %}

ldap_deref_aliases: {{ env.get('EJABBERD_LDAP_DEREF_ALIASES', 'never') }}
ldap_base: "{{ env['EJABBERD_LDAP_BASE'] }}"

{%- if env['EJABBERD_LDAP_UIDS'] %}
ldap_uids:
{%- for ldap_uid in env['EJABBERD_LDAP_UIDS'].split() %}
  "{{ ldap_uid.split(':')[0] }}": "{{ ldap_uid.split(':')[1] }}"
{%- endfor %}
{%- endif %}

{%- if env['EJABBERD_LDAP_FILTER'] %}
ldap_filter: "{{ env['EJABBERD_LDAP_FILTER'] }}"
{%- endif %}

{%- if env['EJABBERD_LDAP_DN_FILTER'] %}
ldap_dn_filter:
{%- for dn_filter in env['EJABBERD_LDAP_DN_FILTER'].split() %}
  "{{ dn_filter.split(':')[0] }}": ["{{ dn_filter.split(':')[1] }}"]
{%- endfor %}
{%- endif %}

{%- endif %}

{%- if 'external' in env.get('EJABBERD_AUTH_METHOD', 'internal').split() %}
  {%- if env['EJABBERD_EXTAUTH_PROGRAM'] %}
extauth_program: "{{ env['EJABBERD_EXTAUTH_PROGRAM'] }}"
  {%- endif %}
  {%- if env['EJABBERD_EXTAUTH_INSTANCES'] %}
extauth_instances: {{ env['EJABBERD_EXTAUTH_INSTANCES'] }}
  {%- endif %}
  {%- if 'internal' in env.get('EJABBERD_AUTH_METHOD').split() %}
extauth_cache: false
  {%- elif env['EJABBERD_EXTAUTH_CACHE'] %}
extauth_cache: {{ env['EJABBERD_EXTAUTH_CACHE'] }}
  {%- endif %}
{% endif %}

###   ===============
###   TRAFFIC SHAPERS

shaper:
  normal: 1000
  fast: 50000
max_fsm_queue: 1000

###   ====================
###   ACCESS CONTROL LISTS

acl:
  admin:
    user:
    {%- if env['EJABBERD_ADMINS'] %}
      {%- for admin in env['EJABBERD_ADMINS'].split() %}
      - "{{ admin.split('@')[0] }}": "{{ admin.split('@')[1] }}"
      {%- endfor %}
    {%- else %}
      - "admin": "{{ env['XMPP_DOMAIN'].split()[0] }}"
    {%- endif %}
  local:
    user_regexp: ""

###   ============
###   ACCESS RULES

access:
  ## Maximum number of simultaneous sessions allowed for a single user:
  max_user_sessions:
    all: 10
  ## Maximum number of offline messages that users can have:
  max_user_offline_messages:
    admin: 5000
    all: 100
  ## This rule allows access only for local users:
  local:
    local: allow
  ## Only non-blocked users can use c2s connections:
  c2s:
    blocked: deny
    all: allow
  ## For C2S connections, all users except admins use the "normal" shaper
  c2s_shaper:
    admin: none
    all: normal
  ## All S2S connections use the "fast" shaper
  s2s_shaper:
    all: fast
  ## Only admins can send announcement messages:
  announce:
    admin: allow
  ## Only admins can use the configuration interface:
  configure:
    admin: allow
  ## Admins of this server are also admins of the MUC service:
  muc_admin:
    admin: allow
  ## Only accounts of the local ejabberd server, or only admins can create rooms, depending on environment variable:
  muc_create:
    {%- if env['EJABBERD_MUC_CREATE_ADMIN_ONLY'] == "true" %}
    admin: allow
    {% else %}
    local: allow
    {% endif %}
  ## All users are allowed to use the MUC service:
  muc:
    all: allow
  ## Only accounts on the local ejabberd server can create Pubsub nodes:
  pubsub_createnode:
    local: allow
  ## In-band registration allows registration of any possible username.
  register:
    {%- if env['EJABBERD_REGISTER_ADMIN_ONLY'] == "true" %}
    all: deny
    admin: allow
    {% else %}
    all: allow
    {% endif %}
  ## Only allow to register from localhost
  trusted_network:
    loopback: allow
  soft_upload_quota:
    all: 400 # MiB
  hard_upload_quota:
    all: 500 # MiB


language: "en"

###   =======
###   MODULES

modules:
  mod_adhoc: {}
  {%- if env['EJABBERD_MOD_ADMIN_EXTRA'] == "true" %}
  mod_admin_extra: {}
  {% endif %}
  mod_announce: # recommends mod_adhoc
    access: announce
  mod_blocking: {} # requires mod_privacy
  mod_caps: {}
  mod_carboncopy: {}
  mod_client_state:
    queue_chat_states: true
    queue_presence: false
  mod_configure: {} # requires mod_adhoc
  mod_disco: {}
  ## mod_echo: {}
  mod_irc: {}
  mod_http_bind: {}
  ## mod_http_fileserver:
  ##   docroot: "/var/www"
  ##   accesslog: "/var/log/ejabberd/access.log"
  mod_last: {}
  mod_mam:
    default: always
  mod_muc:
    host: "conference.@HOST@"
    access: muc
    access_create: muc_create
    access_persistent: muc_create
    access_admin: muc_admin
    history_size: 50
    default_room_options:
      persistent: true
  {%- if env['EJABBERD_MOD_MUC_ADMIN'] == "true" %}
  mod_muc_admin: {}
  {% endif %}
  ## mod_muc_log: {}
  ## mod_multicast: {}
  mod_offline:
    access_max_user_messages: max_user_offline_messages
  mod_ping: {}
  ## mod_pres_counter:
  ##   count: 5
  ##   interval: 60
  mod_privacy: {}
  mod_private: {}
  ## mod_proxy65: {}
  mod_pubsub:
    access_createnode: pubsub_createnode
    ## reduces resource comsumption, but XEP incompliant
    ignore_pep_from_offline: true
    ## XEP compliant, but increases resource comsumption
    ## ignore_pep_from_offline: false
    last_item_cache: false
    plugins:
      - "flat"
      - "hometree"
      - "pep" # pep requires mod_caps
  mod_register:
    ##
    ## Protect In-Band account registrations with CAPTCHA.
    ##
    ## captcha_protected: true

    ##
    ## Set the minimum informational entropy for passwords.
    ##
    ## password_strength: 32

    ##
    ## After successful registration, the user receives
    ## a message with this subject and body.
    ##
    welcome_message:
      subject: "Welcome!"
      body: |-
        Hi.
        Welcome to this XMPP server.

    ##
    ## Only clients in the server machine can register accounts
    ##
    {%- if env['EJABBERD_REGISTER_TRUSTED_NETWORK_ONLY'] == "true" %}
    ip_access: trusted_network
    {% endif %}

    access: register
  mod_roster: {}
  mod_shared_roster: {}
  mod_stats: {}
  mod_time: {}
  mod_vcard: {}
  {% if env.get('EJABBERD_MOD_VERSION', true) == "true" %}
  mod_version: {}
  {% endif %}
  mod_http_upload:
    docroot: "/opt/ejabberd/upload"
    {%- if env['EJABBERD_HTTPS'] == "true" %}
    put_url: "https://@HOST@:5443"
    {%- else %}
    put_url: "http://@HOST@:5443"
    {% endif %}
  mod_http_upload_quota:
    max_days: 10

###   ============
###   HOST CONFIG

host_config:
{%- for xmpp_domain in env['XMPP_DOMAIN'].split() %}
  "{{ xmpp_domain }}":
    domain_certfile: "/opt/ejabberd/ssl/{{ xmpp_domain }}.pem"
{%- endfor %}

{%- if env['EJABBERD_CONFIGURE_ODBC'] == "true" %}
###   ====================
###   ODBC DATABASE CONFIG
sql_type: {{ env['EJABBERD_ODBC_TYPE'] }}
sql_server: "{{ env['EJABBERD_ODBC_SERVER'] }}"
sql_database: "{{ env['EJABBERD_ODBC_DATABASE'] }}"
sql_username: "{{ env['EJABBERD_ODBC_USERNAME'] }}"
sql_password: "{{ env['EJABBERD_ODBC_PASSWORD'] }}"

default_db: sql
{% endif %}

{%- if env['EJABBERD_DEFAULT_DB'] is defined %}
default_db: {{ env['EJABBERD_DEFAULT_DB'] }}
{% endif %}

###   =====================
###   SESSION MANAGEMENT DB
sm_db_type: {{ env['EJABBERD_SESSION_DB'] or "mnesia" }}

{%- if env['EJABBERD_CONFIGURE_REDIS'] == "true" %}
###   ====================
###   REDIS DATABASE CONFIG
redis_server: {{ env['EJABBERD_REDIS_SERVER'] or "localhost" }}
redis_port: {{ env['EJABBERD_REDIS_PORT'] or 6379 }}
{%- if env['EJABBERD_REDIS_PASSWORD'] is defined %}
redis_password: {{ env['EJABBERD_REDIS_PASSWORD'] }}
{% endif %}
redis_db: {{ env['EJABBERD_REDIS_DB'] or 0}}
redis_reconnect_timeout: {{ env['EJABBERD_REDIS_RECONNECT_TIMEOUT'] or 1 }}
redis_connect_timeout: {{ env['EJABBERD_REDIS_CONNECT_TIMEOUT'] or 1 }}
{% endif %}
