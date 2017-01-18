%%%----------------------------------------------------------------------
%%% File    : mnesia_mnesia.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Handle configurable mnesia schema
%%% Created : 17 Nov 2016 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

%%% This module should be used everywhere ejabberd creates a mnesia table
%%% to make the schema customizable without code change
%%% Just apply this change in ejabberd modules
%%% s/ejabberd_mnesia:create(?MODULE, /ejabberd_mnesia:create(?MODULE, /

-module(ejabberd_mnesia).
-author('christophe.romain@process-one.net').
-export([create/3, reset/2, update/2]).

-define(STORAGE_TYPES, [disc_copies, disc_only_copies, ram_copies]).
-define(NEED_RESET, [local_content, type]).

-include("logger.hrl").

create(Module, Name, TabDef)
  when is_atom(Module), is_atom(Name), is_list(TabDef) ->
    Path = os:getenv("EJABBERD_SCHEMA_PATH"),
    Schema = schema(Path, Module, Name, TabDef),
    {attributes, Attrs} = lists:keyfind(attributes, 1, Schema),
    case catch mnesia:table_info(Name, attributes) of
	{'EXIT', _} ->
	    mnesia_op(create_table, [Name, TabDef]);
	Attrs ->
	    case need_reset(Name, Schema) of
		true -> reset(Name, Schema);
		false -> update(Name, Attrs, Schema)
	    end;
	OldAttrs ->
	    Fun = case lists:member({transform,1}, Module:module_info(exports)) of
		true -> fun(Old) -> Module:transform(Old) end;
		false -> fun(Old) -> transform(OldAttrs, Attrs, Old) end
	    end,
	    mnesia_op(transform_table, [Name, Fun, Attrs])
    end.

reset(Name, TabDef)
  when is_atom(Name), is_list(TabDef) ->
    mnesia_op(delete_table, [Name]),
    mnesia_op(create_table, [Name, TabDef]).

update(Name, TabDef)
  when is_atom(Name), is_list(TabDef) ->
    {attributes, Attrs} = lists:keyfind(attributes, 1, TabDef),
    update(Name, Attrs, TabDef).
update(Name, Attrs, TabDef) ->
    Storage = case catch mnesia:table_info(Name, storage_type) of
	{'EXIT', _} -> unknown;
	Type -> Type
	end,
    NewStorage = lists:foldl(
		   fun({Key, _}, Acc) ->
			   case lists:member(Key, ?STORAGE_TYPES) of
			       true -> Key;
			       false -> Acc
			   end
		   end, Storage, TabDef),
    R1 = [mnesia_op(change_table_copy_type, [Name, node(), NewStorage])
	  || Storage=/=NewStorage],
    CurIndexes = [lists:nth(N-1, Attrs) || N<-mnesia:table_info(Name, index)],
    NewIndexes = proplists:get_value(index, TabDef, []),
    R2 = [mnesia_op(del_table_index, [Name, Attr])
	  || Attr <- CurIndexes--NewIndexes],
    R3 = [mnesia_op(add_table_index, [Name, Attr])
	  || Attr <- NewIndexes--CurIndexes],
    lists:foldl(
      fun({atomic, ok}, Acc) -> Acc;
	 (Error, _Acc) -> Error
      end, {atomic, ok}, R1++R2++R3).

%
% utilities
%

schema(false, Module, _Name, TabDef) ->
    ?DEBUG("No custom ~s schema path", [Module]),
    TabDef;
schema(Path, Module, Name, TabDef) ->
    File = filename:join(Path, atom_to_list(Module)++".mnesia"),
    case parse(File) of
	{ok, CustomDefs} ->
	    case lists:keyfind(Name, 1, CustomDefs) of
		{Name, CustomDef} ->
		    ?INFO_MSG("Using custom ~s schema for table ~s",
			      [Module, Name]),
		    merge(TabDef, CustomDef);
		_ ->
		    TabDef
	    end;
	{error, Error} ->
	    ?ERROR_MSG("Can not use custom ~s schema for table ~s: ~p",
		       [Module, Name, Error]),
	    TabDef
    end.

merge(TabDef, CustomDef) ->
    {CustomKeys, _} = lists:unzip(CustomDef),
    CleanDef = lists:foldl(
		fun(Elem, Acc) ->
		    case lists:member(Elem, ?STORAGE_TYPES) of
			true ->
			    lists:foldl(
			      fun(Key, CleanAcc) ->
				      lists:keydelete(Key, 1, CleanAcc)
			      end, Acc, ?STORAGE_TYPES);
			false ->
			    Acc
		    end
		end, TabDef, CustomKeys),
    lists:ukeymerge(1,
		   lists:ukeysort(1, CustomDef),
		   lists:ukeysort(1, CleanDef)).

parse(File) ->
    case file:consult(File) of
	{ok, Terms} -> parse(Terms, []);
	Error -> Error
    end.
parse([], Acc) ->
    {ok, lists:reverse(Acc)};
parse([{Name, Storage, TabDef}|Tail], Acc)
  when is_atom(Name), is_atom(Storage), is_list(TabDef) ->
    NewDef = case lists:member(Storage, ?STORAGE_TYPES) of
	true -> [{Storage, [node()]} | TabDef];
	false -> TabDef
    end,
    parse(Tail, [{Name, NewDef} | Acc]);
parse([Other|_], _) ->
    {error, {invalid, Other}}.

need_reset(Table, TabDef) ->
    ValuesF = [mnesia:table_info(Table, Key) || Key <- ?NEED_RESET],
    ValuesT = [proplists:get_value(Key, TabDef) || Key <- ?NEED_RESET],
    lists:foldl(
      fun({Val, Val}, Acc) -> Acc;
	 ({_, undefined}, Acc) -> Acc;
	 ({_, _}, _) -> true
      end, false, lists:zip(ValuesF, ValuesT)).

transform(OldAttrs, Attrs, Old) ->
    [Name|OldValues] = tuple_to_list(Old),
    Before = lists:zip(OldAttrs, OldValues),
    After = lists:foldl(
	      fun(Attr, Acc) ->
		      case lists:keyfind(Attr, 1, Before) of
			  false -> [{Attr, undefined}|Acc];
			  Value -> [Value|Acc]
		      end
	      end, [], lists:reverse(Attrs)),
    {Attrs, NewRecord} = lists:unzip(After),
    list_to_tuple([Name|NewRecord]).

mnesia_op(Fun, Args) ->
    case apply(mnesia, Fun, Args) of
	{atomic, ok} ->
	    {atomic, ok};
	Other ->
	    ?ERROR_MSG("failure on mnesia ~s ~p: ~p",
		      [Fun, Args, Other]),
	    Other
    end.
