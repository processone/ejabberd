%%%
%%%  Copyright 2009 © INRIA
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 20 août 2009 by Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

-module(ts_erlang).
-vc('$Id: ts_erlang.erl,v 0.0 2009/08/20 16:31:58 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-define(TIMEOUT,36000000). % 1 hour

-include("ts_profile.hrl").

-export([client/4]).

client(MasterPid,Server,Port,Opts)->
    receive
        {Module, Fun, Args, Size} ->
            Res=apply(Module,Fun,Args),
            MasterPid ! {erlang,self(),{Module,Fun,Args,Res}},
            client(MasterPid,Server,Port,Opts)
    after ?TIMEOUT ->
            MasterPid ! timeout
    end.


