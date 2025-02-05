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

%%%% definitions

%% @format-begin

-module(config_tests).

-compile(export_all).

-include("suite.hrl").

%%%==================================
%%%% setup

single_cases() ->
    Tests = case lists:member(configtest, suite:get_config_backends()) of
        true -> single_cases2();
        false -> []
    end,
    {config_single,
     [sequence],
     Tests}.

single_cases2() ->
     [single_test(macro_over_keyword),
      single_test(keyword_inside_macro),
      single_test(macro_toplevel_global_string),
      single_test(macro_toplevel_global_integer),
      single_test(macro_toplevel_local_string),
      single_test(macro_toplevel_local_integer),
      single_test(keyword_toplevel_global_string),
      single_test(keyword_toplevel_global_string_inside),
      single_test(keyword_toplevel_local_string),
      single_test(keyword_toplevel_local_string_inside),
      single_test(macro_module_local_string),
      single_test(macro_module_local_integer),
      single_test(keyword_module_local_string),
      single_test(keyword_module_local_string_inside),
      single_test(keyword_module_local_integer),
      single_test(default_keywords)].

%% Interactions

macro_over_keyword(_Config) ->
    toplevel(12345, captcha_limit).

keyword_inside_macro(_Config) ->
    toplevel(<<"http://example.org/captcha">>, captcha_url).

%% Macro Toplevel

macro_toplevel_global_string(_Config) ->
    toplevel(<<"macro-global-server">>, sql_server).

macro_toplevel_global_integer(_Config) ->
    toplevel(1111, sql_port).

macro_toplevel_local_string(_Config) ->
    toplevel2(<<"macro-local-server">>, sql_server).

macro_toplevel_local_integer(_Config) ->
    toplevel2(2222, sql_port).

%% Keyword Toplevel

keyword_toplevel_global_string(_Config) ->
    toplevel(<<"global-keyword-username">>, sql_username).

keyword_toplevel_global_string_inside(_Config) ->
    toplevel(<<"password--global-keyword-username">>, sql_password).

keyword_toplevel_local_string(_Config) ->
    toplevel2(<<"local-keyword-username">>, sql_username).

keyword_toplevel_local_string_inside(_Config) ->
    toplevel2(<<"password--local-keyword-username">>, sql_password).

%% Macro Module

macro_module_local_string(_Config) ->
    module(<<"macro-local-name">>, mod_proxy65, name).

macro_module_local_integer(_Config) ->
    module(3333, mod_vcard, matches).

%% Keyword Module

keyword_module_local_string(_Config) ->
    module(<<"keyword-local-name">>, mod_disco, name).

keyword_module_local_string_inside(_Config) ->
    module(<<"mix.keyword-local-name">>, mod_mix, name).

keyword_module_local_integer(_Config) ->
    module(4444, mod_muc_admin, subscribe_room_many_max_users).

%% Predefined

default_keywords(_Config) ->
    Host = <<"configtest.localhost">>,
    Version =
        misc:semver_to_xxyy(
            ejabberd_option:version()),
    String = <<"host: ", Host/binary, ", version: ", Version/binary>>,
    module(String, mod_multicast, name).

%%%==================================
%%%% internal functions

single_test(T) ->
    list_to_atom("config_" ++ atom_to_list(T)).

toplevel(Result, Option) ->
    ?match(Result, ejabberd_config:get_option(Option)).

toplevel2(Result, Option) ->
    ?match(Result, ejabberd_config:get_option({Option, <<"configtest.localhost">>})).

module(Result, Module, Option) ->
    ?match(Result, gen_mod:get_module_opt(<<"configtest.localhost">>, Module, Option)).

%%%==================================

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
