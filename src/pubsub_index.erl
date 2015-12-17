%%%----------------------------------------------------------------------
%%% File    : pubsub_index.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Provide uniq integer index for pubsub node
%%% Created : 30 Apr 2009 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

%% important note:
%% new/1 and free/2 MUST be called inside a transaction bloc

-module(pubsub_index).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-export([init/3, new/1, free/2]).

init(_Host, _ServerHost, _Opts) ->
    mnesia:create_table(pubsub_index,
	[{disc_copies, [node()]},
	    {attributes, record_info(fields, pubsub_index)}]).

new(Index) ->
    case mnesia:read({pubsub_index, Index}) of
	[I] ->
	    case I#pubsub_index.free of
		[] ->
		    Id = I#pubsub_index.last + 1,
		    mnesia:write(I#pubsub_index{last = Id}),
		    Id;
		[Id | Free] ->
		    mnesia:write(I#pubsub_index{free = Free}), Id
	    end;
	_ ->
	    mnesia:write(#pubsub_index{index = Index, last = 1, free = []}),
	    1
    end.

free(Index, Id) ->
    case mnesia:read({pubsub_index, Index}) of
	[I] ->
	    Free = I#pubsub_index.free,
	    mnesia:write(I#pubsub_index{free = [Id | Free]});
	_ ->
	    ok
    end.
