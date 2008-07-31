%%%-------------------------------------------------------------------
%%% File    : ejabberd_update.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd code updater
%%% Created : 27 Jan 2006 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(ejabberd_update).
-author('alexey@process-one.net').

%% API
-export([update/0, update_info/0]).

-include("ejabberd.hrl").

%%====================================================================
%% API
%%====================================================================
update() ->
    case update_info() of
	{ok, Dir, _UpdatedBeams, _Script, LowLevelScript, _Check} ->
	    Eval =
		release_handler_1:eval_script(
		  LowLevelScript, [],
		  [{ejabberd, "", filename:join(Dir, "..")}]),
	    ?INFO_MSG("eval: ~p~n", [Eval]),
	    Eval;
	{error, Reason} ->
	    {error, Reason}
    end.

update_info() ->
    Dir = filename:dirname(code:which(ejabberd)),
    case file:list_dir(Dir) of
	{ok, Files} ->
	    Beams = [list_to_atom(filename:rootname(FN)) ||
			FN <- Files, lists:suffix(".beam", FN)],
	    UpdatedBeams =
		lists:filter(
		  fun(Module) ->
			  {ok, {Module, NewVsn}} =
			      beam_lib:version(code:which(Module)),
			  case code:is_loaded(Module) of
			      {file, _} ->
				  Attrs = Module:module_info(attributes),
				  {value, {vsn, CurVsn}} =
				      lists:keysearch(vsn, 1, Attrs),
				  NewVsn /= CurVsn;
			      false ->
				  false
			  end
		  end, Beams),
	    ?INFO_MSG("beam files: ~p~n", [UpdatedBeams]),
	    Script = make_script(UpdatedBeams),
	    ?INFO_MSG("script: ~p~n", [Script]),
	    LowLevelScript = make_low_level_script(UpdatedBeams, Script),
	    ?INFO_MSG("low level script: ~p~n", [LowLevelScript]),
	    Check =
		release_handler_1:check_script(
		  LowLevelScript,
		  [{ejabberd, "", filename:join(Dir, "..")}]),
	    ?INFO_MSG("check: ~p~n", [Check]),
	    {ok, Dir, UpdatedBeams, Script, LowLevelScript, Check};
	{error, Reason} ->
	    {error, Reason}
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% From systools.hrl
-record(application, 
	{name,			%% Name of the application, atom().
         type = permanent,	%% Application start type, atom().
	 vsn = "",         	%% Version of the application, string().
	 id = "",		%% Id of the application, string().
	 description = "",	%% Description of application, string().
	 modules = [],		%% [Module | {Module,Vsn}] of modules 
				%% incorporated in the application, 
				%% Module = atom(), Vsn = string().
	 uses = [],		%% [Application] list of applications required
	 			%% by the application, Application = atom().
	 includes = [],		%% [Application] list of applications included
	 			%% by the application, Application = atom().
	 regs = [],		%% [RegNames] a list of registered process 
				%% names used by the application, RegNames =
				%% atom().
	 env = [],		%% [{Key,Value}] environment variable of 
	 			%% application, Key = Value = term().
	 maxT = infinity,	%% Max time an application may exist, 
				%% integer() | infinity.
	 maxP = infinity,  	%% Max number of processes in an application,
	 			%% integer() | infinity.
	 mod = [],		%% [] | {Mod, StartArgs}, Mod= atom(), 
				%% StartArgs = list().
	 start_phases = [],	%% [] | {Phase, PhaseArgs}, Phase = atom(),
				%% PhaseArgs = list().
         dir = ""		%% The directory where the .app file was 
				%% found (internal use).
	}).


make_script(UpdatedBeams) ->
    lists:map(
      fun(Module) ->
	      {ok, {Module, [{attributes, NewAttrs}]}} =
		  beam_lib:chunks(code:which(Module), [attributes]),
	      CurAttrs = Module:module_info(attributes),
	      case lists:keysearch(update_info, 1, NewAttrs) of
		  {value, {_, [{update, _}]}} ->
		      case lists:keysearch(update_info, 1, CurAttrs) of
			  {value, {_, [{update, Extra}]}} ->
			      {update, Module, {advanced, Extra}};
			  false ->
			      {update, Module, {advanced, 0}}
		      end;
		  false ->
		      {load_module, Module}
	      end
      end, UpdatedBeams).

make_low_level_script(UpdatedBeams, Script) ->
    EJDApp = #application{name = ejabberd,
			  modules = UpdatedBeams},
    {ok, LowLevelScript} =
	systools_rc:translate_scripts([Script], [EJDApp], [EJDApp]),
    LowLevelScript.
