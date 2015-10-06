%%%-------------------------------------------------------------------
%%% File    : ejabberd_update.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd code updater
%%% Created : 27 Jan 2006 by Alexey Shchepin <alexey@process-one.net>
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
%%%-------------------------------------------------------------------

-module(ejabberd_update).
-author('alexey@process-one.net').

%% API
-export([update/0, update/1, update_info/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%====================================================================
%% API
%%====================================================================

%% Update all the modified modules
update() ->
    case update_info() of
	{ok, Dir, _UpdatedBeams, _Script, LowLevelScript, _Check} ->
	    Eval =
		eval_script(
		  LowLevelScript, [],
		  [{ejabberd, "", filename:join(Dir, "..")}]),
	    ?DEBUG("eval: ~p~n", [Eval]),
	    Eval;
	{error, Reason} ->
	    {error, Reason}
    end.

%% Update only the specified modules
update(ModulesToUpdate) ->
    case update_info() of
	{ok, Dir, UpdatedBeamsAll, _Script, _LowLevelScript, _Check} ->
	    UpdatedBeamsNow =
		[A || A <- UpdatedBeamsAll, B <- ModulesToUpdate, A == B],
	    {_, LowLevelScript, _} = build_script(Dir, UpdatedBeamsNow),
	    Eval =
		eval_script(
		  LowLevelScript, [],
		  [{ejabberd, "", filename:join(Dir, "..")}]),
	    ?DEBUG("eval: ~p~n", [Eval]),
	    Eval;
	{error, Reason} ->
	    {error, Reason}
    end.

eval_script(Script, Apps, LibDirs) ->
    release_handler_1:eval_script(Script, Apps, LibDirs, [], []).

%% Get information about the modified modules
update_info() ->
    Dir = filename:dirname(code:which(ejabberd)),
    case file:list_dir(Dir) of
	{ok, Files} ->
	    update_info(Dir, Files);
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

update_info(Dir, Files) ->
    Beams = lists:sort(get_beams(Files)),
    UpdatedBeams = get_updated_beams(Beams),
    ?DEBUG("beam files: ~p~n", [UpdatedBeams]),
    {Script, LowLevelScript, Check} = build_script(Dir, UpdatedBeams),
    {ok, Dir, UpdatedBeams, Script, LowLevelScript, Check}.

get_beams(Files) ->
    [list_to_atom(filename:rootname(FN))
     || FN <- Files, lists:suffix(".beam", FN)].

%% Return only the beams that have different version
get_updated_beams(Beams) ->
    lists:filter(
      fun(Module) ->
	      NewVsn = get_new_version(Module),
	      case code:is_loaded(Module) of
		  {file, _} ->
		      CurVsn = get_current_version(Module),
		      (NewVsn /= CurVsn
		       andalso NewVsn /= unknown_version);
		  false ->
		      false
	      end
      end, Beams).

get_new_version(Module) ->
    Path = code:which(Module),
    VersionRes = beam_lib:version(Path),
    case VersionRes of
	{ok, {Module, NewVsn}} -> NewVsn;
	%% If a m1.erl has -module("m2"):
	_ -> unknown_version
    end.

get_current_version(Module) ->
    Attrs = Module:module_info(attributes),
    case lists:keysearch(vsn, 1, Attrs) of
        {value, {vsn, CurVsn}} -> CurVsn;
        _ -> unknown_version
    end.

%% @spec(Dir::string(), UpdatedBeams::[atom()]) -> {Script,LowLevelScript,Check}
build_script(Dir, UpdatedBeams) ->
    Script = make_script(UpdatedBeams),
    LowLevelScript = make_low_level_script(UpdatedBeams, Script),
    Check =
	release_handler_1:check_script(
	  LowLevelScript,
	  [{ejabberd, "", filename:join(Dir, "..")}]),
    Check1 = case Check of
	{ok, []} ->
	    ?DEBUG("script: ~p~n", [Script]),
	    ?DEBUG("low level script: ~p~n", [LowLevelScript]),
	    ?DEBUG("check: ~p~n", [Check]),
	    ok;
	_ ->
	    ?ERROR_MSG("script: ~p~n", [Script]),
	    ?ERROR_MSG("low level script: ~p~n", [LowLevelScript]),
	    ?ERROR_MSG("check: ~p~n", [Check]),
	    error
    end,
    {Script, LowLevelScript, Check1}.

%% Copied from Erlang/OTP file: lib/sasl/src/systools.hrl
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
