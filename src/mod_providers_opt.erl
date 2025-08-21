%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_providers_opt).

-export([alternativeJids/1]).
-export([busFactor/1]).
-export([freeOfCharge/1]).
-export([languages/1]).
-export([legalNotice/1]).
-export([maximumHttpFileUploadStorageTime/1]).
-export([maximumHttpFileUploadTotalSize/1]).
-export([maximumMessageArchiveManagementStorageTime/1]).
-export([organization/1]).
-export([passwordReset/1]).
-export([professionalHosting/1]).
-export([serverLocations/1]).
-export([serverTesting/1]).
-export([since/1]).
-export([website/1]).

-spec alternativeJids(gen_mod:opts() | global | binary()) -> [binary()].
alternativeJids(Opts) when is_map(Opts) ->
    gen_mod:get_opt(alternativeJids, Opts);
alternativeJids(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, alternativeJids).

-spec busFactor(gen_mod:opts() | global | binary()) -> integer().
busFactor(Opts) when is_map(Opts) ->
    gen_mod:get_opt(busFactor, Opts);
busFactor(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, busFactor).

-spec freeOfCharge(gen_mod:opts() | global | binary()) -> boolean().
freeOfCharge(Opts) when is_map(Opts) ->
    gen_mod:get_opt(freeOfCharge, Opts);
freeOfCharge(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, freeOfCharge).

-spec languages(gen_mod:opts() | global | binary()) -> [binary()].
languages(Opts) when is_map(Opts) ->
    gen_mod:get_opt(languages, Opts);
languages(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, languages).

-spec legalNotice(gen_mod:opts() | global | binary()) -> binary().
legalNotice(Opts) when is_map(Opts) ->
    gen_mod:get_opt(legalNotice, Opts);
legalNotice(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, legalNotice).

-spec maximumHttpFileUploadStorageTime(gen_mod:opts() | global | binary()) -> 'default_value' | integer().
maximumHttpFileUploadStorageTime(Opts) when is_map(Opts) ->
    gen_mod:get_opt(maximumHttpFileUploadStorageTime, Opts);
maximumHttpFileUploadStorageTime(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, maximumHttpFileUploadStorageTime).

-spec maximumHttpFileUploadTotalSize(gen_mod:opts() | global | binary()) -> 'default_value' | integer().
maximumHttpFileUploadTotalSize(Opts) when is_map(Opts) ->
    gen_mod:get_opt(maximumHttpFileUploadTotalSize, Opts);
maximumHttpFileUploadTotalSize(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, maximumHttpFileUploadTotalSize).

-spec maximumMessageArchiveManagementStorageTime(gen_mod:opts() | global | binary()) -> integer().
maximumMessageArchiveManagementStorageTime(Opts) when is_map(Opts) ->
    gen_mod:get_opt(maximumMessageArchiveManagementStorageTime, Opts);
maximumMessageArchiveManagementStorageTime(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, maximumMessageArchiveManagementStorageTime).

-spec organization(gen_mod:opts() | global | binary()) -> '' | 'commercial person' | 'company' | 'governmental' | 'non-governmental' | 'private person'.
organization(Opts) when is_map(Opts) ->
    gen_mod:get_opt(organization, Opts);
organization(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, organization).

-spec passwordReset(gen_mod:opts() | global | binary()) -> 'default_value' | binary().
passwordReset(Opts) when is_map(Opts) ->
    gen_mod:get_opt(passwordReset, Opts);
passwordReset(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, passwordReset).

-spec professionalHosting(gen_mod:opts() | global | binary()) -> boolean().
professionalHosting(Opts) when is_map(Opts) ->
    gen_mod:get_opt(professionalHosting, Opts);
professionalHosting(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, professionalHosting).

-spec serverLocations(gen_mod:opts() | global | binary()) -> [binary()].
serverLocations(Opts) when is_map(Opts) ->
    gen_mod:get_opt(serverLocations, Opts);
serverLocations(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, serverLocations).

-spec serverTesting(gen_mod:opts() | global | binary()) -> boolean().
serverTesting(Opts) when is_map(Opts) ->
    gen_mod:get_opt(serverTesting, Opts);
serverTesting(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, serverTesting).

-spec since(gen_mod:opts() | global | binary()) -> binary().
since(Opts) when is_map(Opts) ->
    gen_mod:get_opt(since, Opts);
since(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, since).

-spec website(gen_mod:opts() | global | binary()) -> binary().
website(Opts) when is_map(Opts) ->
    gen_mod:get_opt(website, Opts);
website(Host) ->
    gen_mod:get_module_opt(Host, mod_providers, website).

