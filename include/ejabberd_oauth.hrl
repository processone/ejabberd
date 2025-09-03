%%%----------------------------------------------------------------------
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
%%%----------------------------------------------------------------------
%% @efmt:off
%% @indent-begin

-record(oauth_token, {
          token = <<"">>           :: binary() | '_',
          us = {<<"">>, <<"">>}    :: {binary(), binary()} | '_',
          scope = []               :: [binary()] | '_',
          expire                   :: integer() | '$1' | '_'
         }).

-record(oauth_client, {
          client_id = <<"">>       :: binary() | '_',
          client_name = <<"">>     :: binary() | '_',
          grant_type               :: password | implicit | '_',
          options                  :: [any()] | '_'
         }).
