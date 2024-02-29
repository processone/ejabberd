%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_muc_opt).

-export([access/1]).
-export([access_admin/1]).
-export([access_create/1]).
-export([access_mam/1]).
-export([access_persistent/1]).
-export([access_register/1]).
-export([cleanup_affiliations_on_start/1]).
-export([db_type/1]).
-export([default_room_options/1]).
-export([hibernation_timeout/1]).
-export([history_size/1]).
-export([host/1]).
-export([hosts/1]).
-export([max_captcha_whitelist/1]).
-export([max_password/1]).
-export([max_room_desc/1]).
-export([max_room_id/1]).
-export([max_room_name/1]).
-export([max_rooms_discoitems/1]).
-export([max_user_conferences/1]).
-export([max_users/1]).
-export([max_users_admin_threshold/1]).
-export([max_users_presence/1]).
-export([min_message_interval/1]).
-export([min_presence_interval/1]).
-export([name/1]).
-export([preload_rooms/1]).
-export([queue_type/1]).
-export([ram_db_type/1]).
-export([regexp_room_id/1]).
-export([room_shaper/1]).
-export([user_message_shaper/1]).
-export([user_presence_shaper/1]).
-export([vcard/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, access).

-spec access_admin(gen_mod:opts() | global | binary()) -> 'none' | acl:acl().
access_admin(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_admin, Opts);
access_admin(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, access_admin).

-spec access_create(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access_create(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_create, Opts);
access_create(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, access_create).

-spec access_mam(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access_mam(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_mam, Opts);
access_mam(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, access_mam).

-spec access_persistent(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access_persistent(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_persistent, Opts);
access_persistent(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, access_persistent).

-spec access_register(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access_register(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_register, Opts);
access_register(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, access_register).

-spec cleanup_affiliations_on_start(gen_mod:opts() | global | binary()) -> boolean().
cleanup_affiliations_on_start(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cleanup_affiliations_on_start, Opts);
cleanup_affiliations_on_start(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, cleanup_affiliations_on_start).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, db_type).

-spec default_room_options(gen_mod:opts() | global | binary()) -> [{atom(),'anyone' | 'false' | 'moderators' | 'nobody' | 'none' | 'participants' | 'true' | 'undefined' | binary() | ['moderator' | 'participant' | 'visitor'] | pos_integer() | tuple()}].
default_room_options(Opts) when is_map(Opts) ->
    gen_mod:get_opt(default_room_options, Opts);
default_room_options(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, default_room_options).

-spec hibernation_timeout(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
hibernation_timeout(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hibernation_timeout, Opts);
hibernation_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, hibernation_timeout).

-spec history_size(gen_mod:opts() | global | binary()) -> non_neg_integer().
history_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(history_size, Opts);
history_size(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, history_size).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, host).

-spec hosts(gen_mod:opts() | global | binary()) -> [binary()].
hosts(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hosts, Opts);
hosts(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, hosts).

-spec max_captcha_whitelist(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_captcha_whitelist(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_captcha_whitelist, Opts);
max_captcha_whitelist(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_captcha_whitelist).

-spec max_password(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_password(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_password, Opts);
max_password(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_password).

-spec max_room_desc(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_room_desc(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_room_desc, Opts);
max_room_desc(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_room_desc).

-spec max_room_id(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_room_id(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_room_id, Opts);
max_room_id(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_room_id).

-spec max_room_name(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
max_room_name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_room_name, Opts);
max_room_name(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_room_name).

-spec max_rooms_discoitems(gen_mod:opts() | global | binary()) -> non_neg_integer().
max_rooms_discoitems(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_rooms_discoitems, Opts);
max_rooms_discoitems(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_rooms_discoitems).

-spec max_user_conferences(gen_mod:opts() | global | binary()) -> pos_integer().
max_user_conferences(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_user_conferences, Opts);
max_user_conferences(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_user_conferences).

-spec max_users(gen_mod:opts() | global | binary()) -> pos_integer().
max_users(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_users, Opts);
max_users(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_users).

-spec max_users_admin_threshold(gen_mod:opts() | global | binary()) -> pos_integer().
max_users_admin_threshold(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_users_admin_threshold, Opts);
max_users_admin_threshold(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_users_admin_threshold).

-spec max_users_presence(gen_mod:opts() | global | binary()) -> integer().
max_users_presence(Opts) when is_map(Opts) ->
    gen_mod:get_opt(max_users_presence, Opts);
max_users_presence(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, max_users_presence).

-spec min_message_interval(gen_mod:opts() | global | binary()) -> number().
min_message_interval(Opts) when is_map(Opts) ->
    gen_mod:get_opt(min_message_interval, Opts);
min_message_interval(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, min_message_interval).

-spec min_presence_interval(gen_mod:opts() | global | binary()) -> number().
min_presence_interval(Opts) when is_map(Opts) ->
    gen_mod:get_opt(min_presence_interval, Opts);
min_presence_interval(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, min_presence_interval).

-spec name(gen_mod:opts() | global | binary()) -> binary().
name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(name, Opts);
name(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, name).

-spec preload_rooms(gen_mod:opts() | global | binary()) -> boolean().
preload_rooms(Opts) when is_map(Opts) ->
    gen_mod:get_opt(preload_rooms, Opts);
preload_rooms(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, preload_rooms).

-spec queue_type(gen_mod:opts() | global | binary()) -> 'file' | 'ram'.
queue_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(queue_type, Opts);
queue_type(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, queue_type).

-spec ram_db_type(gen_mod:opts() | global | binary()) -> atom().
ram_db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ram_db_type, Opts);
ram_db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, ram_db_type).

-spec regexp_room_id(gen_mod:opts() | global | binary()) -> <<>> | misc:re_mp().
regexp_room_id(Opts) when is_map(Opts) ->
    gen_mod:get_opt(regexp_room_id, Opts);
regexp_room_id(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, regexp_room_id).

-spec room_shaper(gen_mod:opts() | global | binary()) -> atom().
room_shaper(Opts) when is_map(Opts) ->
    gen_mod:get_opt(room_shaper, Opts);
room_shaper(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, room_shaper).

-spec user_message_shaper(gen_mod:opts() | global | binary()) -> atom().
user_message_shaper(Opts) when is_map(Opts) ->
    gen_mod:get_opt(user_message_shaper, Opts);
user_message_shaper(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, user_message_shaper).

-spec user_presence_shaper(gen_mod:opts() | global | binary()) -> atom().
user_presence_shaper(Opts) when is_map(Opts) ->
    gen_mod:get_opt(user_presence_shaper, Opts);
user_presence_shaper(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, user_presence_shaper).

-spec vcard(gen_mod:opts() | global | binary()) -> 'undefined' | tuple().
vcard(Opts) when is_map(Opts) ->
    gen_mod:get_opt(vcard, Opts);
vcard(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, vcard).

