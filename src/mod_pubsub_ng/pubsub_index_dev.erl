%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2012, ProcessOne.
%%%
%%%
%%% @copyright 2006-2012 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%% important note:
%% new/1 and free/2 MUST be called inside a transaction bloc

-module(pubsub_index_dev).
-author('christophe.romain@process-one.net').

-include("pubsub_dev.hrl").

-export([
  init/1,
  new/2,
  free/3
]).

-import(pubsub_db_mnesia,
[
  table/2
]).


%%
-spec(init/1 ::
(
  Suffix::atom()) -> 'ok'
).

init(Suffix) ->
    mnesia:create_table(table('pubsub_index_dev', Suffix),
        [{disc_copies, [node()]},
         {record_name, pubsub_index_dev},
         {attributes, record_info(fields, pubsub_index_dev)}]).

%%
-spec(new/2 ::
(
  Suffix :: atom(),
  Index  :: exmpp_pubsub:index())
    -> Idx::exmpp_pubsub:nodeIdx()
).

new(Suffix, Index) ->
    case
        mnesia:read(Table_Pubsub_Index = table('pubsub_index_dev', Suffix),
            Index, write)
    of
        [#pubsub_index_dev{free = []} = Pubsub_Index] ->
            mnesia:write(Table_Pubsub_Index,
                Pubsub_Index#pubsub_index_dev{
                    last = Idx = Pubsub_Index#pubsub_index_dev.last + 1
                },
                write),
            Idx;
        [#pubsub_index_dev{free = [Idx|Free]} = Pubsub_Index] ->
            mnesia:write(Table_Pubsub_Index,
                Pubsub_Index#pubsub_index_dev{
                    free = Free
                },
                write),
            Idx;
        _ ->
            mnesia:write(Table_Pubsub_Index,
                #pubsub_index_dev{index = Index, last = 1, free = []}, write),
            1
    end.

%%
-spec(free/3 ::
(
  Suffix :: atom(),
  Index  :: exmpp_pubsub:index(),
  Idx    :: exmpp_pubsub:nodeIdx())
    -> 'ok'
).

free(Suffix, Index, Idx) ->
    case
        mnesia:read(Table_Pubsub_Index = table('pubsub_index_dev', Suffix),
            Index, write)
    of
        [Pubsub_Index] ->
            mnesia:write(Table_Pubsub_Index,
                Pubsub_Index#pubsub_index_dev{
                    free = [Idx | Pubsub_Index#pubsub_index_dev.free]
                },
                write);
        _ ->
            ok
    end.
