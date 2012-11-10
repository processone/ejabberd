-module(ejabberd_sets).

%% Sets with adaptable storage,  use less memory if the set size isn't big. 
%% 
%%
%% -Use a simple lists,  change to gb_sets only if size grows above thereshold
%% -Function to build two sets at once, from two lists,  trying to share as much as
%%  possible.  This avoid the intersection to be repeated on both sets structure.

%%SET interface, same than gb_sets
-export([
		new/0,
		from_list/1,
		to_list/1,
		is_element/2,
		add_element/2,
		del_element/2,
		foldl/3,
		size/1]).

%% new method
-export([ 
		pack_sets/2
	]).

-define(THRESHOLD, 100).


new() ->
	[].
from_list(L) when length(L) < ?THRESHOLD ->
	L;
from_list(L) ->
	gb_sets:from_list(L).

pack_sets(L1, L2) ->
	Shared = lists:filter(fun(I) -> lists:member(I, L2) end, L1),
	SharedSet = from_list(Shared),
	%% add_element already verify that the elements doesn't exists, so no need to
	%% filter L1 and L2 before.
	R1 = lists:foldl( fun add_element/2, SharedSet, L1),
	R2 = lists:foldl( fun add_element/2, SharedSet, L2),
	{R1, R2}.



to_list(S) when is_list(S) -> S;
to_list(S) -> gb_sets:to_list(S).

is_element(El, S) when is_list(S) -> lists:member(El, S);
is_element(El, S) -> gb_sets:is_element(El, S).

add_element(El, S) when is_list(S) ->
	case lists:member(El, S) of
		true ->
			S;
		false ->
			fix_storage([El | S])
	end;
add_element(El, S) ->
	gb_sets:add_element(El, S).



size(S) when is_list(S) -> length(S);
size(S) -> gb_sets:size(S).

foldl(Fun, Init, S) when is_list(S)-> lists:foldl(Fun, Init, S);
foldl(Fun, Init, S) -> gb_sets:foldl(Fun, Init, S).

del_element(El, S) when is_list(S) ->
	lists:remove(El, S);
del_element(El, S) ->   
	fix_storage(gb_sets:del_element(El,S)).  



fix_storage(S) when is_list(S), length(S) > ?THRESHOLD ->
	gb_sets:from_list(S);
fix_storage(S) -> S.
	%%For now we don't shrink to lists again, not neccesarly
