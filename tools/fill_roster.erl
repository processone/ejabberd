%%
%% Adding subscriptions to rosters
%%

-module(fill_roster).

-export([fill/3]).

-include_lib("mod_roster.hrl").

fill(UserNum, PerRosterNum, Type) when UserNum < PerRosterNum ->
    fill_bucket(1, UserNum, Type);
fill(UserNum, PerRosterNum, Type) ->
    UserNum2 = UserNum - PerRosterNum,
    fill_bucket(UserNum2 + 1, UserNum, Type),
    fill(UserNum2, PerRosterNum, Type).

%% [From, To]
fill_bucket(From, To, Type) ->
    lists:foreach(fun(UserN) ->
                          add_contacts(UserN, From, To, Type)
                  end, lists:seq(From, To)).

add_contacts(UserN, From, To, Type) ->
    lists:foreach(fun(ContactN) ->
                          User = prep_el("user" ++ integer_to_list(UserN), Type),
                          Contact = prep_el("user" ++ integer_to_list(ContactN), Type),
                          Host = prep_el("localhost", Type),
                          Item = #roster{
                               usj = {User, Host, {Contact, Host, prep_el("", Type)}},
                               us = {User, Host},
                               jid = {Contact, Host, prep_el("", Type)},
                               subscription = both },
                          mnesia:dirty_write(Item)
                  end, lists:seq(From, To)).

prep_el(El, bin) ->
    list_to_binary(El);
prep_el(El, string) ->
    El.


