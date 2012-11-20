%% Copyright (C) 2009 Romuald du Song <rdusong _AT_ gmail _DOT_ com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(beam_util).

-export([module_export_list/1, filter_arity/3]).


%% Module = string()
%% Function = atom()
module_export_list( Module ) ->
	{_Module, _Binary, Filename} = code:get_object_code(Module),
	case beam_lib:info( Filename ) of
		{error, beam_lib, _} ->
			false;
		[ _ , _ , _ ] ->
			case beam_lib:chunks( Filename, [exports]) of
				{ok, {_, [{exports, Exports}]}} ->
					Exports;
				{error, beam_lib, Er} ->
					false
			end
	end. 

%% Module = string()
%% Arity = integer()
%% Exports = list()
filter_arity( Function, Arity, Exports) ->
	case lists:filter(
		   fun( EFName ) -> {Function, Arity} == EFName end,
		   Exports ) of
		[{_, _}] -> true;
		[] -> false
	end. 
