%%%-------------------------------------------------------------------
%%% Author  : Badlop <badlop@process-one.net>
%%% Created : 5 Feb 2025 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
%%%-------------------------------------------------------------------

-module(configtest_tests).

-compile(export_all).

-include("suite.hrl").

%%%==================================

single_cases() ->
    {configtest_single,
     [sequence],
     [single_test(macro_over_keyword),
      single_test(keyword_inside_macro),
      single_test(macro_and_keyword),
      single_test(macro_double),
      single_test(keyword_double),

      single_test(macro_toplevel_global_atom),
      single_test(macro_toplevel_global_string),
      single_test(macro_toplevel_global_string_inside),
      single_test(macro_toplevel_local_atom),
      single_test(macro_toplevel_local_string),
      single_test(macro_toplevel_local_string_inside),

      single_test(keyword_toplevel_global_atom),
      single_test(keyword_toplevel_global_string),
      single_test(keyword_toplevel_global_string_inside),
      single_test(keyword_toplevel_local_atom),
      single_test(keyword_toplevel_local_string),
      single_test(keyword_toplevel_local_string_inside),

      single_test(macro_module_atom),
      single_test(macro_module_string),
      single_test(macro_module_string_inside),

      single_test(keyword_module_atom),
      single_test(keyword_module_string),
      single_test(keyword_module_string_inside),

      single_test(toplevel_global_predefined),
      single_test(toplevel_local_predefined),
      single_test(module_predefined)]}.

%% Interactions

macro_over_keyword(_) ->
    toplevel_global(macro, macro_over_keyword).

keyword_inside_macro(_) ->
    toplevel_global(<<"+macro+/-keyword-">>, keyword_inside_macro).

macro_and_keyword(_) ->
    toplevel_global(<<"+macro+&-keyword-">>, macro_and_keyword).

macro_double(_) ->
    toplevel_global(<<"macro--macro">>, macro_double).

keyword_double(_) ->
    toplevel_global(<<"keyword--keyword">>, keyword_double).

%% Macro Toplevel

macro_toplevel_global_atom(_) ->
    toplevel_global(mtga, mtga).

macro_toplevel_global_string(_) ->
    toplevel_global(<<"Mtgs">>, mtgs).

macro_toplevel_global_string_inside(_) ->
    toplevel_global(<<"Mtgsi">>, mtgsi).

macro_toplevel_local_atom(_) ->
    toplevel_local(mtla, mtla).

macro_toplevel_local_string(_) ->
    toplevel_local(<<"Mtls">>, mtls).

macro_toplevel_local_string_inside(_) ->
    toplevel_local(<<"Mtlsi">>, mtlsi).

%% Keyword Toplevel

keyword_toplevel_global_atom(_) ->
    toplevel_global(ktga, ktga).

keyword_toplevel_global_string(_) ->
    toplevel_global(<<"Ktgs">>, ktgs).

keyword_toplevel_global_string_inside(_) ->
    toplevel_global(<<"Ktgsi">>, ktgsi).

keyword_toplevel_local_atom(_) ->
    toplevel_local(ktla, ktla).

keyword_toplevel_local_string(_) ->
    toplevel_local(<<"Ktls">>, ktls).

keyword_toplevel_local_string_inside(_) ->
    toplevel_local(<<"Ktlsi">>, ktlsi).

%% Macro Module

macro_module_atom(_) ->
    module(mma, mma).

macro_module_string(_) ->
    module(<<"Mms">>, mms).

macro_module_string_inside(_) ->
    module(<<"Mmsi">>, mmsi).

%% Keyword Module

keyword_module_atom(_) ->
    module(kma, kma).

keyword_module_string(_) ->
    module(<<"Kms">>, kms).

keyword_module_string_inside(_) ->
    module(<<"Kmsi">>, kmsi).

%% Predefined

toplevel_global_predefined(_) ->
    Semver = ejabberd_option:version(),
    Version = misc:semver_to_xxyy(Semver),
    String = <<"tgp - semver: ", Semver/binary, ", version: ", Version/binary>>,
    toplevel_global(String, tgp).

toplevel_local_predefined(_) ->
    Semver = ejabberd_option:version(),
    Version = misc:semver_to_xxyy(Semver),
    String = <<"tlp - semver: ", Semver/binary, ", version: ", Version/binary>>,
    toplevel_local(String, tlp).

module_predefined(_) ->
    Host = <<"configtest.localhost">>,
    Semver = ejabberd_option:version(),
    Version = misc:semver_to_xxyy(Semver),
    String = <<"mp - host: ", Host/binary, ", semver: ", Semver/binary, ", version: ", Version/binary>>,
    module(String, predefined_keywords).

%%%==================================
%%%% internal functions

single_test(T) ->
    list_to_atom("configtest_" ++ atom_to_list(T)).

toplevel_global(Result, Option) ->
    ?match(Result, ejabberd_config:get_option(Option)).

toplevel_local(Result, Option) ->
    Host = <<"configtest.localhost">>,
    ?match(Result, ejabberd_config:get_option({Option, Host})).

module(Result, Option) ->
    Host = <<"configtest.localhost">>,
    Module = mod_configtest,
    ?match(Result, gen_mod:get_module_opt(Host, Module, Option)).

%%%==================================

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
