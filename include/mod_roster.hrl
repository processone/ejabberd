%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-record(roster,
{
    usj = {<<>>, <<>>, {<<>>, <<>>, <<>>}} :: {binary(), binary(), jid:ljid()} | '_',
    us = {<<>>, <<>>}                      :: {binary(), binary()} | '_',
    jid = {<<>>, <<>>, <<>>}               :: jid:ljid(),
    name = <<>>                            :: binary() | '_',
    subscription = none                    :: subscription() | '_',
    ask = none                             :: ask() | '_',
    groups = []                            :: [binary()] | '_',
    askmessage = <<"">>                    :: binary() | '_',
    xs = []                                :: [fxml:xmlel()] | '_',
    mix_participant_id = <<>>              :: binary() | '_'
}).

-record(roster_version,
{
    us = {<<>>, <<>>} :: {binary(), binary()},
    version = <<>>    :: binary()
}).

-type ask() :: none | in | out | both | subscribe | unsubscribe.
-type subscription() :: none | both | from | to | remove.
