%%%----------------------------------------------------------------------
%%% File    : mnesia_mnesia.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Handle configurable mnesia schema
%%% Created : 17 Nov 2016 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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

create(Module, Name, TabDef) ->
    Schema = schema(Module, Name, TabDef),
    {attributes, Attrs} = lists:keyfind(attributes, 1, Schema),
    case catch mnesia:table_info(Name, attributes) of
	{'EXIT', _} ->
	    mnesia:create_table(Name, Schema);
	Attrs ->
	    case need_reset(TabDef, Schema) of
		true -> reset(Name, Schema);
		false -> update(Name, Schema)
	    end;
	OldAttrs ->
	    Fun = case lists:member({transform,1}, Module:module_info(exports)) of
		true -> fun(Old) -> Module:transform(Old) end;
		false -> fun(Old) -> transform(OldAttrs, Attrs, Old) end
	    end,
	    mnesia:transform_table(Name, Fun, Attrs)
    end.

reset(Name, TabDef) ->
    mnesia:delete_table(Name),
    ejabberd_mnesia:create(?MODULE, Name, TabDef).

update(Name, TabDef) ->
    Storage = mnesia:table_info(Name, storage_type),
    NewStorage = lists:foldl(
		   fun({Key, _}, Acc) ->
			   case lists:member(Key, ?STORAGE_TYPES) of
			       true -> Key;
			       false -> Acc
			   end
		   end, Storage, TabDef),
    R1 = if Storage=/=NewStorage ->
	    mnesia:change_table_copy_type(Name, node(), NewStorage);
       true ->
	    {atomic, ok}
    end,
    Indexes = mnesia:table_info(Name, index),
    NewIndexes = proplists:get_value(index, TabDef, []),
    [mnesia:del_table_index(Name, Attr)
     || Attr <- Indexes--NewIndexes],
    R2 = [mnesia:add_table_index(Name, Attr)
	  || Attr <- NewIndexes--Indexes],
    lists:foldl(
      fun({atomic, ok}, Acc) -> Acc;
	 (Error, _Acc) -> Error
      end, {atomic, ok}, [R1|R2]).

%
% utilities
%

schema(Module, Name, TabDef) ->
    case parse(Module) of
	{ok, CustomDefs} ->
	    case lists:keyfind(Name, 1, CustomDefs) of
		{Name, CustomDef} -> merge(TabDef, CustomDef);
		_ -> TabDef
	    end;
	_ ->
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

parse(Module) ->
    Path = case os:getenv("EJABBERD_SCHEMA_PATH") of
		false ->
		    case code:priv_dir(ejabberd) of
			{error, _} -> "schema";  % $SPOOL_DIR/schema
			Priv -> filename:join(Priv, "schema")
		    end;
		CustomDir ->
		    CustomDir
	    end,
    File = filename:join(Path, atom_to_list(Module)++".mnesia"),
    case file:consult(File) of
	{ok, Terms} -> parse(Terms, []);
	Error -> Error
    end.

parse([], Acc) ->
    {ok, lists:reverse(Acc)};
parse([{Name, Storage, TabDef}|Tail], Acc)
  when is_atom(Name),
       is_atom(Storage),
       is_list(TabDef) ->
    NewDef = case lists:member(Storage, ?STORAGE_TYPES) of
	true -> [{Storage, [node()]} | TabDef];
	false -> TabDef
    end,
    parse(Tail, [{Name, NewDef} | Acc]);
parse([Other|_], _) ->
    {error, {invalid, Other}}.

need_reset(FromDef, ToDef) ->
    ValuesF = [lists:keyfind(Key, 1, FromDef) || Key <- ?NEED_RESET],
    ValuesT = [lists:keyfind(Key, 1, ToDef) || Key <- ?NEED_RESET],
    lists:foldl(
      fun({Val, Val}, Acc) -> Acc;
	 ({_, false}, Acc) -> Acc;
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
