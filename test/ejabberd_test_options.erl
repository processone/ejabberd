%%%----------------------------------------------------------------------
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
%%%----------------------------------------------------------------------
-module(ejabberd_test_options).
-behaviour(ejabberd_config).

-export([opt_type/1, options/0, globals/0, doc/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec opt_type(atom()) -> econf:validator().

opt_type(macro_over_keyword) ->
    econf:atom();

opt_type(keyword_inside_macro) ->
    econf:binary();

opt_type(macro_and_keyword) ->
    econf:binary();

opt_type(macro_double) ->
    econf:binary();

opt_type(keyword_double) ->
    econf:binary();

opt_type(mtga) ->
    econf:atom();

opt_type(mtgs) ->
    econf:binary();

opt_type(mtgsi) ->
    econf:binary();

opt_type(mtla) ->
    econf:atom();

opt_type(mtls) ->
    econf:binary();

opt_type(mtlsi) ->
    econf:binary();

opt_type(ktga) ->
    econf:atom();

opt_type(ktgs) ->
    econf:binary();

opt_type(ktgsi) ->
    econf:binary();

opt_type(ktla) ->
    econf:atom();

opt_type(ktls) ->
    econf:binary();

opt_type(ktlsi) ->
    econf:binary();

opt_type(tgp) ->
    econf:binary();

opt_type(tlp) ->
    econf:binary().

options() ->
    [{macro_over_keyword, undefined},
     {keyword_inside_macro, undefined},
     {macro_and_keyword, undefined},
     {macro_double, undefined},
     {keyword_double, undefined},
     {mtga, undefined},
     {mtgs, undefined},
     {mtgsi, undefined},
     {mtla, undefined},
     {mtls, undefined},
     {mtlsi, undefined},
     {ktga, undefined},
     {ktgs, undefined},
     {ktgsi, undefined},
     {ktla, undefined},
     {ktls, undefined},
     {ktlsi, undefined},
     {tgp, undefined},
     {tlp, undefined}
    ].

-spec globals() -> [atom()].
globals() ->
    [].

doc() ->
    ejabberd_options_doc:doc().

