%% ts_test_dynvars_api.erl
%% @author Pablo Polvorin
%% @doc Test for the ts_dynvars module
%% created on 2008-08-22
-module(ts_test_dynvars_api).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


from_keyval_list(KeyValues) ->
   lists:foldl(fun({K,V},DynVars) ->
                    ts_dynvars:set(K,V,DynVars)
               end,
               ts_dynvars:new(),
               KeyValues
               ).
test() ->
    ok.

dynvars_new_ok_test() ->
   Keys = [one,two,three,four],
   Values = [1,2,"three",4],
   ?assertEqual([{one,1},{two,2},{three,"three"},{four,4}],
                ts_dynvars:new(Keys, Values)).

dynvars_new_more_test() ->
   Keys = [one,two,three],
   Values = [1,2,"three",[]],
   ?assertEqual([{one,1},{two,2},{three,"three"}],
                ts_dynvars:new(Keys, Values)).

dynvars_new_less_test() ->
   Keys = [one,two,three,four],
   Values = [1,2,"three"],
   ?assertEqual([{one,1},{two,2},{three,"three"},{four,""}],
                ts_dynvars:new(Keys, Values)).


dynvars_set_test() ->
   KeyValues = [one,two,three,four],
   DynVars = from_keyval_list([{K,K} || K <- KeyValues]),
   ?assertEqual([{ok,K} || K <- KeyValues],
                [ts_dynvars:lookup(Key, DynVars) || Key <- KeyValues]).

dynvars_set2_test() ->
    D = ts_dynvars:set(one,two, ts_dynvars:set(one,one, ts_dynvars:new())),
    ?assertEqual({ok,two},ts_dynvars:lookup(one,D)).

dynvars_undefined_test() ->
   ?assertEqual(false,ts_dynvars:lookup(one, ts_dynvars:new())).

dynvars_default_test() ->
   ?assertEqual({ok,default},ts_dynvars:lookup(one,ts_dynvars:new(), default)).

dynvars_entries_test() ->
   KeyValues = [{K,K} || K <- [one,two,three,four]],
   ?assertEqual(lists:reverse(KeyValues),
                ts_dynvars:entries(from_keyval_list(KeyValues))).

dynvars_map_test() ->
   KeyValues = [{K,K} || K <- [one,two,three,four]],
   ?assertEqual({ok,[two,two]},ts_dynvars:lookup(two,ts_dynvars:map(fun(X) -> [X,X] end,
                                                         two,
                                                         default,
                                                         from_keyval_list(KeyValues))
                                                )).

dynvars_map_default_test() ->
   KeyValues = [{K,K} || K <- [one,two,three,four]],
   ?assertEqual({ok,[one]},ts_dynvars:lookup(five, ts_dynvars:map(fun(X) -> [one|X] end,
                                                          five,
                                                          [],
                                                          from_keyval_list(KeyValues))
                                            )).
