%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  4 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
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
-module(ejabberd_pkix).
-behaviour(gen_server).
-behaviour(ejabberd_config).

%% API
-export([start_link/0, opt_type/1]).
-export([certs_dir/0, ca_file/0]).
-export([add_certfile/1, try_certfile/1, get_certfile/0, get_certfile/1]).
%% Hooks
-export([ejabberd_started/0, config_reloaded/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-include("logger.hrl").
-define(CALL_TIMEOUT, timer:minutes(1)).

-record(state, {files = sets:new() :: sets:set(filename())}).

-type state() :: #state{}.
-type filename() :: binary().

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()} | {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_certfile(file:filename_all()) -> {ok, filename()} | {error, pkix:error_reason()}.
add_certfile(Path0) ->
    Path = prep_path(Path0),
    try gen_server:call(?MODULE, {add_certfile, Path}, ?CALL_TIMEOUT)
    catch exit:{noproc, _} ->
	    case add_file(Path) of
		ok -> {ok, Path};
		Err -> Err
	    end
    end.

-spec try_certfile(file:filename_all()) -> filename().
try_certfile(Path0) ->
    Path = prep_path(Path0),
    case pkix:is_pem_file(Path) of
	true -> Path;
	{false, Reason} ->
	    ?ERROR_MSG("Failed to read PEM file ~s: ~s",
		       [Path, pkix:format_error(Reason)]),
	    erlang:error(badarg)
    end.

-spec get_certfile(binary()) -> {ok, filename()} | error.
get_certfile(Domain) ->
    case get_certfile_no_default(Domain) of
	{ok, Path} ->
	    {ok, Path};
	error ->
	    get_certfile()
    end.

-spec get_certfile_no_default(binary()) -> {ok, filename()} | error.
get_certfile_no_default(Domain) ->
    case xmpp_idna:domain_utf8_to_ascii(Domain) of
	false ->
	    error;
	ASCIIDomain ->
	    case pkix:get_certfile(ASCIIDomain) of
		error -> error;
		Ret -> {ok, select_certfile(Ret)}
	    end
    end.

-spec get_certfile() -> {ok, filename()} | error.
get_certfile() ->
    case pkix:get_certfile() of
	error -> error;
	Ret -> {ok, select_certfile(Ret)}
    end.

-spec ca_file() -> filename() | undefined.
ca_file() ->
    ejabberd_config:get_option(ca_file).

-spec certs_dir() -> file:dirname_all().
certs_dir() ->
    MnesiaDir = mnesia:system_info(directory),
    filename:join(MnesiaDir, "certs").

-spec ejabberd_started() -> ok.
ejabberd_started() ->
    gen_server:call(?MODULE, ejabberd_started, ?CALL_TIMEOUT).

-spec config_reloaded() -> ok.
config_reloaded() ->
    gen_server:call(?MODULE, config_reloaded, ?CALL_TIMEOUT).

opt_type(ca_path) ->
    fun(_) ->
	    ?WARNING_MSG("Option 'ca_path' has no effect anymore, "
			 "use 'ca_file' instead", []),
	    undefined
    end;
opt_type(ca_file) ->
    fun try_certfile/1;
opt_type(certfiles) ->
    fun(Paths) -> [iolist_to_binary(Path) || Path <- Paths] end;
opt_type(O) when O == c2s_certfile; O == s2s_certfile; O == domain_certfile ->
    fun(Path) ->
	    ?WARNING_MSG("Option '~s' is deprecated, use 'certfiles' instead", [O]),
	    prep_path(Path)
    end;
opt_type(_) ->
    [ca_path, ca_file, certfiles, c2s_certfile, s2s_certfile, domain_certfile].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 100),
    ejabberd_hooks:add(ejabberd_started, ?MODULE, ejabberd_started, 30),
    case add_files() of
	{Files, []} ->
	    {ok, #state{files = Files}};
	{Files, [_|_]} ->
	    case ejabberd:is_loaded() of
		true ->
		    {ok, #state{files = Files}};
		false ->
		    del_files(Files),
		    stop_ejabberd()
	    end
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
		  {reply, ok, state()} | {noreply, state()}.
handle_call({add_certfile, Path}, _From, State) ->
    case add_file(Path) of
	ok ->
	    Files = sets:add_element(Path, State#state.files),
	    {reply, {ok, Path}, State#state{files = Files}};
	{error, _} = Err ->
	    {reply, Err, State}
    end;
handle_call(ejabberd_started, _From, State) ->
    case commit() of
	{ok, []} ->
	    check_domain_certfiles(),
	    {reply, ok, State};
	_ ->
	    stop_ejabberd()
    end;
handle_call(config_reloaded, _From, State) ->
    Old = State#state.files,
    New = get_certfiles_from_config_options(),
    del_files(sets:subtract(Old, New)),
    add_files(New),
    case commit() of
	{ok, _} ->
	    check_domain_certfiles(),
	    {reply, ok, State#state{files = New}};
	error ->
	    {reply, ok, State}
    end;
handle_call(Request, _From, State) ->
    ?WARNING_MSG("Unexpected call: ~p", [Request]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Request, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Request]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(),
		state()) -> any().
terminate(_Reason, State) ->
    ejabberd_hooks:delete(ejabberd_started, ?MODULE, ejabberd_started, 30),
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 100),
    del_files(State#state.files).

-spec code_change(term() | {down, term()}, state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec format_status(normal | terminate, list()) -> term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec add_files() -> {sets:set(filename()), [{filename(), pkix:error_reason()}]}.
add_files() ->
    Files = get_certfiles_from_config_options(),
    add_files(sets:to_list(Files), sets:new(), []).

-spec add_files(sets:set(filename())) ->
		{sets:set(filename()), [{filename(), pkix:error_reason()}]}.
add_files(Files) ->
    add_files(sets:to_list(Files), sets:new(), []).

-spec add_files([filename()], sets:set(filename()),
		[{filename(), pkix:error_reason()}]) ->
		       {sets:set(filename()), [{filename(), pkix:error_reason()}]}.
add_files([File|Files], Set, Errs) ->
    case add_file(File) of
	ok ->
	    Set1 = sets:add_element(File, Set),
	    add_files(Files, Set1, Errs);
	{error, Reason} ->
	    Errs1 = [{File, Reason}|Errs],
	    add_files(Files, Set, Errs1)
    end;
add_files([], Set, Errs) ->
    {Set, Errs}.

-spec add_file(filename()) -> ok | {error, pkix:error_reason()}.
add_file(File) ->
    case pkix:add_file(File) of
	ok -> ok;
	{error, Reason} = Err ->
	    ?ERROR_MSG("Failed to read PEM file ~s: ~s",
		       [File, pkix:format_error(Reason)]),
	    Err
    end.

-spec del_files(sets:set(filename())) -> ok.
del_files(Files) ->
    lists:foreach(fun pkix:del_file/1, sets:to_list(Files)).

-spec commit() -> {ok, [{filename(), pkix:error_reason()}]} | error.
commit() ->
    Opts = case ca_file() of
	       undefined -> [];
	       CAFile -> [{cafile, CAFile}]
	   end,
    case pkix:commit(certs_dir(), Opts) of
	{ok, Errors, Warnings, CAError} ->
	    log_errors(Errors),
	    log_cafile_error(CAError),
	    log_warnings(Warnings),
	    fast_tls_add_certfiles(),
	    {ok, Errors};
	{error, File, Reason} ->
	    ?CRITICAL_MSG("Failed to write to ~s: ~s",
			  [File, file:format_error(Reason)]),
	    error
    end.

-spec check_domain_certfiles() -> ok.
check_domain_certfiles() ->
    Hosts = ejabberd_config:get_myhosts(),
    Routes = ejabberd_router:get_all_routes(),
    check_domain_certfiles(Hosts ++ Routes).

-spec check_domain_certfiles([binary()]) -> ok.
check_domain_certfiles(Hosts) ->
    lists:foreach(
      fun(Host) ->
	      case get_certfile_no_default(Host) of
		  error ->
		      ?WARNING_MSG("No certificate found matching '~s': strictly "
				   "configured clients or servers will reject "
				   "connections with this host; obtain "
				   "a certificate for this (sub)domain from any "
				   "trusted CA such as Let's Encrypt "
				   "(www.letsencrypt.org)",
				   [Host]);
		  _ ->
		      ok
	      end
      end, Hosts).

-spec deprecated_options() -> [atom()].
deprecated_options() ->
    [c2s_certfile, s2s_certfile, domain_certfile].

-spec global_certfiles() -> sets:set(filename()).
global_certfiles() ->
    case ejabberd_config:get_option(certfiles) of
	undefined ->
	    sets:new();
	Paths ->
	    lists:foldl(
	      fun(Path, Acc) ->
		      Files = wildcard(Path),
		      lists:foldl(fun sets:add_element/2, Acc, Files)
	      end, sets:new(), Paths)
    end.

-spec local_certfiles() -> sets:set(filename()).
local_certfiles() ->
    Opts = [{Opt, Host} || Opt <- deprecated_options(),
			   Host <- ejabberd_config:get_myhosts()],
    lists:foldl(
      fun(OptHost, Acc) ->
	      case ejabberd_config:get_option(OptHost) of
		  undefined -> Acc;
		  Path -> sets:add_element(Path, Acc)
	      end
      end, sets:new(), Opts).

-spec get_certfiles_from_config_options() -> sets:set(filename()).
get_certfiles_from_config_options() ->
    Global = global_certfiles(),
    Local = local_certfiles(),
    Listen = sets:from_list(ejabberd_listener:get_certfiles()),
    sets:union([Global, Local, Listen]).

-spec prep_path(file:filename_all()) -> filename().
prep_path(Path0) ->
    case filename:pathtype(Path0) of
	relative ->
	    case file:get_cwd() of
		{ok, CWD} ->
		    unicode:characters_to_binary(filename:join(CWD, Path0));
		{error, Reason} ->
		    ?WARNING_MSG("Failed to get current directory name: ~s",
				 [file:format_error(Reason)]),
		    unicode:characters_to_binary(Path0)
	    end;
	_ ->
	    unicode:characters_to_binary(Path0)
    end.

-spec stop_ejabberd() -> no_return().
stop_ejabberd() ->
    ?CRITICAL_MSG("ejabberd initialization was aborted due to "
		  "invalid certificates configuration", []),
    ejabberd:halt().

-spec wildcard(file:filename_all()) -> [filename()].
wildcard(Path) when is_binary(Path) ->
    wildcard(binary_to_list(Path));
wildcard(Path) ->
    case filelib:wildcard(Path) of
	[] ->
	    ?WARNING_MSG("Path ~s is empty, please make sure ejabberd has "
			 "sufficient rights to read it", [Path]),
	    [];
	Files ->
	    [prep_path(File) || File <- Files]
    end.

-spec select_certfile({filename() | undefined,
		       filename() | undefined,
		       filename() | undefined}) -> filename().
select_certfile({EC, _, _}) when EC /= undefined -> EC;
select_certfile({_, RSA, _}) when RSA /= undefined -> RSA;
select_certfile({_, _, DSA}) when DSA /= undefined -> DSA.

-spec fast_tls_add_certfiles() -> ok.
fast_tls_add_certfiles() ->
    lists:foreach(
      fun({Domain, Files}) ->
	      fast_tls:add_certfile(Domain, select_certfile(Files))
      end, pkix:get_certfiles()),
    fast_tls:clear_cache().

reason_to_fmt({invalid_cert, _, _}) ->
    "Invalid certificate in ~s: ~s";
reason_to_fmt(_) ->
    "Failed to read PEM file ~s: ~s".

-spec log_warnings([{filename(), pkix:error_reason()}]) -> ok.
log_warnings(Warnings) ->
    lists:foreach(
      fun({File, Reason}) ->
	      ?WARNING_MSG(reason_to_fmt(Reason),
			   [File, pkix:format_error(Reason)])
      end, Warnings).

-spec log_errors([{filename(), pkix:error_reason()}]) -> ok.
log_errors(Errors) ->
    lists:foreach(
      fun({File, Reason}) ->
	      ?ERROR_MSG(reason_to_fmt(Reason),
			 [File, pkix:format_error(Reason)])
      end, Errors).

-spec log_cafile_error({filename(), pkix:error_reason()} | undefined) -> ok.
log_cafile_error({File, Reason}) ->
    ?CRITICAL_MSG("Failed to read CA certitificates from ~s: ~s. "
		  "Try to change/set option 'ca_file'",
		  [File, pkix:format_error(Reason)]);
log_cafile_error(_) ->
    ok.
