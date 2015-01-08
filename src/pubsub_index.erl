%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

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
	  mnesia:write(#pubsub_index{index = Index, last = 1,
				     free = []}),
	  1
    end.

free(Index, Id) ->
    case mnesia:read({pubsub_index, Index}) of
      [I] ->
	  Free = I#pubsub_index.free,
	  mnesia:write(I#pubsub_index{free = [Id | Free]});
      _ -> ok
    end.
