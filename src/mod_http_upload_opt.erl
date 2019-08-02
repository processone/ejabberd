%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_http_upload_opt).

-export([access/1]).
-export([custom_headers/1]).
-export([dir_mode/1]).
-export([docroot/1]).
-export([external_secret/1]).
-export([file_mode/1]).
-export([get_url/1]).
-export([host/1]).
-export([hosts/1]).
-export([jid_in_url/1]).
-export([max_size/1]).
-export([name/1]).
-export([put_url/1]).
-export([rm_on_unregister/1]).
-export([secret_length/1]).
-export([service_url/1]).
-export([thumbnail/1]).
-export([vcard/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'local' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, access).

-spec custom_headers(gen_mod:opts() | global | binary()) -> [{binary(),binary()}].
custom_headers(Opts) when is_map(Opts) ->
    gen_mod:get_opt(custom_headers, Opts);
custom_headers(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, custom_headers).

-spec dir_mode(gen_mod:opts() | global | binary()) -> 'undefined' | non_neg_integer().
dir_mode(Opts) when is_map(Opts) ->
    gen_mod:get_opt(dir_mode, Opts);
dir_mode(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, dir_mode).

-spec docroot(gen_mod:opts() | global | binary()) -> binary().
docroot(Opts) when is_map(Opts) ->
    gen_mod:get_opt(docroot, Opts);
docroot(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, docroot).

-spec external_secret(gen_mod:opts() | global | binary()) -> binary().
external_secret(Opts) when is_map(Opts) ->
    gen_mod:get_opt(external_secret, Opts);
external_secret(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, external_secret).

-spec file_mode(gen_mod:opts() | global | binary()) -> 'undefined' | non_neg_integer().
file_mode(Opts) when is_map(Opts) ->
    gen_mod:get_opt(file_mode, Opts);
file_mode(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, file_mode).

-spec get_url(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
get_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(get_url, Opts);
get_url(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, get_url).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, host).

-spec hosts(gen_mod:opts() | global | binary()) -> [binary()].
hosts(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hosts, Opts);
hosts(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, hosts).

-spec jid_in_url(gen_mod:opts() | global | binary()) -> 'node' | 'sha1'.
jid_in_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(jid_in_url, Opts);
jid_in_url(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, jid_in_url).

-spec max_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_size, Opts);
max_size(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, max_size).

-spec name(gen_mod:opts() | global | binary()) -> binary().
name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(name, Opts);
name(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, name).

-spec put_url(gen_mod:opts() | global | binary()) -> binary().
put_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(put_url, Opts);
put_url(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, put_url).

-spec rm_on_unregister(gen_mod:opts() | global | binary()) -> boolean().
rm_on_unregister(Opts) when is_map(Opts) ->
    gen_mod:get_opt(rm_on_unregister, Opts);
rm_on_unregister(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, rm_on_unregister).

-spec secret_length(gen_mod:opts() | global | binary()) -> 1..1114111.
secret_length(Opts) when is_map(Opts) ->
    gen_mod:get_opt(secret_length, Opts);
secret_length(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, secret_length).

-spec service_url(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
service_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(service_url, Opts);
service_url(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, service_url).

-spec thumbnail(gen_mod:opts() | global | binary()) -> boolean().
thumbnail(Opts) when is_map(Opts) ->
    gen_mod:get_opt(thumbnail, Opts);
thumbnail(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, thumbnail).

-spec vcard(gen_mod:opts() | global | binary()) -> 'undefined' | tuple().
vcard(Opts) when is_map(Opts) ->
    gen_mod:get_opt(vcard, Opts);
vcard(Host) ->
    gen_mod:get_module_opt(Host, mod_http_upload, vcard).

