%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is ProcessOne.
%% Portions created by ProcessOne are Copyright 2006-2012, ProcessOne
%% All Rights Reserved.''

-module(p1_mnesia).
-author('mickael.remond@process-one.net').

-export([count_records/2]).

%% Return the number of records matching a given match expression.
%% This function is intended to be used inside a Mnesia transaction.
%% The count has been written to use the fewest possible memory by
%% getting the record by small increment and by using continuation.
-define(BATCHSIZE, 100).
count_records(Tab, MatchExpression) ->
    %% the result contains list of [] for each match: We do not need
    %% actual values as we only count the data.
    case mnesia:select(Tab, [{MatchExpression, [], [[]]}], ?BATCHSIZE, read) of
	{Result,Cont} ->
	    Count = length(Result),
	    count_records_cont(Cont, Count);
	'$end_of_table' ->
	    0
    end.
count_records_cont(Cont, Count) ->
    case mnesia:select(Cont) of
	{Result,Cont} ->
	    NewCount = Count + length(Result),
	    count_records_cont(Cont, NewCount);
	'$end_of_table' ->
	    Count
    end.
