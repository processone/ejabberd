%%%----------------------------------------------------------------------
%%% File    : mod_http_upload_quota.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Quota management for HTTP File Upload (XEP-0363)
%%% Created : 15 Oct 2015 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2015-2024   ProcessOne
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

-module(mod_http_upload_quota).
-author('holger@zedat.fu-berlin.de').

-define(TIMEOUT, timer:hours(24)).
-define(FORMAT(Error), file:format_error(Error)).

-behaviour(gen_server).
-behaviour(gen_mod).

%% gen_mod/supervisor callbacks.
-export([start/2,
	 stop/1,
	 depends/2,
         mod_doc/0,
	 mod_opt_type/1,
	 mod_options/1]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% ejabberd_hooks callback.
-export([handle_slot_request/6]).

-include_lib("xmpp/include/jid.hrl").
-include("logger.hrl").
-include("translate.hrl").
-include_lib("kernel/include/file.hrl").

-record(state,
	{server_host                    :: binary(),
	 access_soft_quota              :: atom(),
	 access_hard_quota              :: atom(),
	 max_days                       :: pos_integer() | infinity,
	 docroot                        :: binary(),
	 disk_usage = #{}               :: disk_usage(),
	 timer                          :: reference() | undefined}).

-type disk_usage() :: #{{binary(), binary()} => non_neg_integer()}.
-type state() :: #state{}.

%%--------------------------------------------------------------------
%% gen_mod/supervisor callbacks.
%%--------------------------------------------------------------------
start(ServerHost, Opts) ->
    Proc = mod_http_upload:get_proc_name(ServerHost, ?MODULE),
    gen_mod:start_child(?MODULE, ServerHost, Opts, Proc).

stop(ServerHost) ->
    Proc = mod_http_upload:get_proc_name(ServerHost, ?MODULE),
    gen_mod:stop_child(Proc).

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(access_soft_quota) ->
    econf:shaper();
mod_opt_type(access_hard_quota) ->
    econf:shaper();
mod_opt_type(max_days) ->
    econf:pos_int(infinity).

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_) ->
    [{access_soft_quota, soft_upload_quota},
     {access_hard_quota, hard_upload_quota},
     {max_days, infinity}].

mod_doc() ->
    #{desc =>
          [?T("This module adds quota support for mod_http_upload."), "",
           ?T("This module depends on _`mod_http_upload`_.")],
      opts =>
          [{max_days,
            #{value => ?T("Days"),
              desc =>
                  ?T("If a number larger than zero is specified, "
                     "any files (and directories) older than this "
                     "number of days are removed from the subdirectories "
                     "of the 'docroot' directory, once per day. "
                     "The default value is 'infinity'.")}},
           {access_soft_quota,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("This option defines which access rule is used "
                     "to specify the \"soft quota\" for the matching JIDs. "
                     "That rule must yield a positive number of megabytes "
                     "for any JID that is supposed to have a quota limit. "
                     "See the description of the 'access_hard_quota' option "
                     "for details. The default value is 'soft_upload_quota'.")}},
           {access_hard_quota,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("This option defines which access rule is used to "
                     "specify the \"hard quota\" for the matching JIDs. "
                     "That rule must yield a positive number for any "
                     "JID that is supposed to have a quota limit. "
                     "This is the number of megabytes a corresponding "
                     "user may upload. When this threshold is exceeded, "
                     "ejabberd deletes the oldest files uploaded by that "
                     "user until their disk usage equals or falls below "
                     "the specified soft quota (see also option 'access_soft_quota'). "
                     "The default value is 'hard_upload_quota'.")}}],
      example =>
	  [{?T("Notice it's not necessary to specify the "
	       "'access_hard_quota' and 'access_soft_quota' options in order "
	       "to use the quota feature. You can stick to the default names "
	       "and just specify access rules such as those in this example:"),
          ["shaper_rules:",
           "  soft_upload_quota:",
           "    1000: all # MiB",
           "  hard_upload_quota:",
           "    1100: all # MiB",
           "",
           "modules:",
           "  mod_http_upload: {}",
           "  mod_http_upload_quota:",
           "    max_days: 100"]}]}.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [{mod_http_upload, hard}].

%%--------------------------------------------------------------------
%% gen_server callbacks.
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, state()}.
init([ServerHost|_]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(ServerHost, ?MODULE),
    AccessSoftQuota = mod_http_upload_quota_opt:access_soft_quota(Opts),
    AccessHardQuota = mod_http_upload_quota_opt:access_hard_quota(Opts),
    MaxDays = mod_http_upload_quota_opt:max_days(Opts),
    DocRoot1 = mod_http_upload_opt:docroot(ServerHost),
    DocRoot2 = mod_http_upload:expand_home(str:strip(DocRoot1, right, $/)),
    DocRoot3 = mod_http_upload:expand_host(DocRoot2, ServerHost),
    Timer = if MaxDays == infinity -> undefined;
	       true ->
		   Timeout = p1_rand:uniform(?TIMEOUT div 2),
		   erlang:send_after(Timeout, self(), sweep)
	    end,
    ejabberd_hooks:add(http_upload_slot_request, ServerHost, ?MODULE,
		       handle_slot_request, 50),
    {ok, #state{server_host = ServerHost,
		access_soft_quota = AccessSoftQuota,
		access_hard_quota = AccessHardQuota,
		max_days = MaxDays,
		docroot = DocRoot3,
		timer = Timer}}.

-spec handle_call(_, {pid(), _}, state()) -> {noreply, state()}.
handle_call(Request, From, State) ->
    ?ERROR_MSG("Unexpected request from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast({handle_slot_request, #jid{user = U, server = S} = JID, Path, Size},
	    #state{server_host = ServerHost,
		   access_soft_quota = AccessSoftQuota,
		   access_hard_quota = AccessHardQuota,
		   disk_usage = DiskUsage} = State) ->
    HardQuota = case ejabberd_shaper:match(ServerHost, AccessHardQuota, JID) of
		    Hard when is_integer(Hard), Hard > 0 ->
			Hard * 1024 * 1024;
		    _ ->
			0
		end,
    SoftQuota = case ejabberd_shaper:match(ServerHost, AccessSoftQuota, JID) of
		    Soft when is_integer(Soft), Soft > 0 ->
			Soft * 1024 * 1024;
		    _ ->
			0
		end,
    OldSize = case maps:find({U, S}, DiskUsage) of
		  {ok, Value} ->
		      Value;
		  error ->
		      undefined
	      end,
    NewSize = case {HardQuota, SoftQuota} of
		  {0, 0} ->
		      ?DEBUG("No quota specified for ~ts",
			     [jid:encode(JID)]),
		      undefined;
		  {0, _} ->
		      ?WARNING_MSG("No hard quota specified for ~ts",
				   [jid:encode(JID)]),
		      enforce_quota(Path, Size, OldSize, SoftQuota, SoftQuota);
		  {_, 0} ->
		      ?WARNING_MSG("No soft quota specified for ~ts",
				   [jid:encode(JID)]),
		      enforce_quota(Path, Size, OldSize, HardQuota, HardQuota);
		  _ when SoftQuota > HardQuota ->
		      ?WARNING_MSG("Bad quota for ~ts (soft: ~p, hard: ~p)",
				   [jid:encode(JID),
				    SoftQuota, HardQuota]),
		      enforce_quota(Path, Size, OldSize, SoftQuota, SoftQuota);
		  _ ->
		      ?DEBUG("Enforcing quota for ~ts",
			     [jid:encode(JID)]),
		      enforce_quota(Path, Size, OldSize, SoftQuota, HardQuota)
	      end,
    NewDiskUsage = if is_integer(NewSize) ->
			   maps:put({U, S}, NewSize, DiskUsage);
		      true ->
			   DiskUsage
		   end,
    {noreply, State#state{disk_usage = NewDiskUsage}};
handle_cast(Request, State) ->
    ?ERROR_MSG("Unexpected request: ~p", [Request]),
    {noreply, State}.

-spec handle_info(_, state()) -> {noreply, state()}.
handle_info(sweep, #state{server_host = ServerHost,
			  docroot = DocRoot,
			  max_days = MaxDays} = State)
    when is_integer(MaxDays), MaxDays > 0 ->
    ?DEBUG("Got 'sweep' message for ~ts", [ServerHost]),
    Timer = erlang:send_after(?TIMEOUT, self(), sweep),
    case file:list_dir(DocRoot) of
	{ok, Entries} ->
	    BackThen = secs_since_epoch() - (MaxDays * 86400),
	    DocRootS = binary_to_list(DocRoot),
	    PathNames = lists:map(fun(Entry) ->
					  DocRootS ++ "/" ++ Entry
				  end, Entries),
	    UserDirs = lists:filter(fun filelib:is_dir/1, PathNames),
	    lists:foreach(fun(UserDir) ->
				  delete_old_files(UserDir, BackThen)
			  end, UserDirs);
	{error, Error} ->
	    ?ERROR_MSG("Cannot open document root ~ts: ~ts",
		       [DocRoot, ?FORMAT(Error)])
    end,
    {noreply, State#state{timer = Timer}};
handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, _} | _, state()) -> ok.
terminate(Reason, #state{server_host = ServerHost, timer = Timer}) ->
    ?DEBUG("Stopping upload quota process for ~ts: ~p", [ServerHost, Reason]),
    ejabberd_hooks:delete(http_upload_slot_request, ServerHost, ?MODULE,
			  handle_slot_request, 50),
    misc:cancel_timer(Timer).

-spec code_change({down, _} | _, state(), _) -> {ok, state()}.
code_change(_OldVsn, #state{server_host = ServerHost} = State, _Extra) ->
    ?DEBUG("Updating upload quota process for ~ts", [ServerHost]),
    {ok, State}.

%%--------------------------------------------------------------------
%% ejabberd_hooks callback.
%%--------------------------------------------------------------------
-spec handle_slot_request(allow | deny, binary(), jid(), binary(),
			  non_neg_integer(), binary()) -> allow | deny.
handle_slot_request(allow, ServerHost, JID, Path, Size, _Lang) ->
    Proc = mod_http_upload:get_proc_name(ServerHost, ?MODULE),
    gen_server:cast(Proc, {handle_slot_request, JID, Path, Size}),
    allow;
handle_slot_request(Acc, _ServerHost, _JID, _Path, _Size, _Lang) -> Acc.

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec enforce_quota(file:filename_all(), non_neg_integer(),
		    non_neg_integer() | undefined, non_neg_integer(),
		    non_neg_integer())
      -> non_neg_integer().
enforce_quota(_UserDir, SlotSize, OldSize, _MinSize, MaxSize)
    when is_integer(OldSize), OldSize + SlotSize =< MaxSize ->
    OldSize + SlotSize;
enforce_quota(UserDir, SlotSize, _OldSize, MinSize, MaxSize) ->
    Files = lists:sort(fun({_PathA, _SizeA, TimeA}, {_PathB, _SizeB, TimeB}) ->
			       TimeA > TimeB
		       end, gather_file_info(UserDir)),
    {DelFiles, OldSize, NewSize} =
	lists:foldl(fun({_Path, Size, _Time}, {[], AccSize, AccSize})
			    when AccSize + Size + SlotSize =< MinSize ->
			    {[], AccSize + Size, AccSize + Size};
		       ({Path, Size, _Time}, {[], AccSize, AccSize}) ->
			    {[Path], AccSize + Size, AccSize};
		       ({Path, Size, _Time}, {AccFiles, AccSize, NewSize}) ->
			    {[Path | AccFiles], AccSize + Size, NewSize}
		    end, {[], 0, 0}, Files),
    if OldSize + SlotSize > MaxSize ->
	    lists:foreach(fun del_file_and_dir/1, DelFiles),
	    file:del_dir(UserDir), % In case it's empty, now.
	    NewSize + SlotSize;
       true ->
	    OldSize + SlotSize
    end.

-spec delete_old_files(file:filename_all(), integer()) -> ok.
delete_old_files(UserDir, CutOff) ->
    FileInfo = gather_file_info(UserDir),
    case [Path || {Path, _Size, Time} <- FileInfo, Time < CutOff] of
	[] ->
	    ok;
	OldFiles ->
	    lists:foreach(fun del_file_and_dir/1, OldFiles),
	    file:del_dir(UserDir) % In case it's empty, now.
    end.

-spec gather_file_info(file:filename_all())
      -> [{binary(), non_neg_integer(), non_neg_integer()}].
gather_file_info(Dir) when is_binary(Dir) ->
    gather_file_info(binary_to_list(Dir));
gather_file_info(Dir) ->
    case file:list_dir(Dir) of
	{ok, Entries} ->
	    lists:foldl(fun(Entry, Acc) ->
				Path = Dir ++ "/" ++ Entry,
				case file:read_file_info(Path,
							 [{time, posix}]) of
				    {ok, #file_info{type = directory}} ->
					gather_file_info(Path) ++ Acc;
				    {ok, #file_info{type = regular,
						    mtime = Time,
						    size = Size}} ->
					[{Path, Size, Time} | Acc];
				    {ok, _Info} ->
					?DEBUG("Won't stat(2) non-regular file ~ts",
					       [Path]),
					Acc;
				    {error, Error} ->
					?ERROR_MSG("Cannot stat(2) ~ts: ~ts",
						   [Path, ?FORMAT(Error)]),
					Acc
				end
			end, [], Entries);
	{error, enoent} ->
	    ?DEBUG("Directory ~ts doesn't exist", [Dir]),
	    [];
	{error, Error} ->
	    ?ERROR_MSG("Cannot open directory ~ts: ~ts", [Dir, ?FORMAT(Error)]),
	    []
    end.

-spec del_file_and_dir(file:name_all()) -> ok.
del_file_and_dir(File) ->
    case file:delete(File) of
	ok ->
	    ?INFO_MSG("Removed ~ts", [File]),
	    Dir = filename:dirname(File),
	    case file:del_dir(Dir) of
		ok ->
		    ?DEBUG("Removed ~ts", [Dir]);
		{error, Error} ->
		    ?DEBUG("Cannot remove ~ts: ~ts", [Dir, ?FORMAT(Error)])
	    end;
	{error, Error} ->
	    ?WARNING_MSG("Cannot remove ~ts: ~ts", [File, ?FORMAT(Error)])
    end.

-spec secs_since_epoch() -> non_neg_integer().
secs_since_epoch() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
