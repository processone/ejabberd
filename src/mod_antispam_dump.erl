%%%----------------------------------------------------------------------
%%% File    : mod_antispam_dump.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Manage dump file for filtered spam messages
%%% Created : 31 Mar 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2019-2026 ProcessOne
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

%%| Definitions
%% @format-begin

-module(mod_antispam_dump).

-author('holger@zedat.fu-berlin.de').
-author('stefan@strigler.de').

-export([init_dumping/1, terminate_dumping/2, reload_dumping/4, reopen_dump_file/2,
         write_stanza_dump/2]).
%% ejabberd_hooks callbacks
-export([dump_spam_stanza/1, reopen_log/0]).

-include("logger.hrl").
-include("mod_antispam.hrl").
-include("translate.hrl").

-include_lib("xmpp/include/xmpp.hrl").

%%--------------------------------------------------------------------
%%| Exported

init_dumping(Host) ->
    case get_path_option(Host) of
        false ->
            undefined;
        DumpFile when is_binary(DumpFile) ->
            case filelib:ensure_dir(DumpFile) of
                ok ->
                    ejabberd_hooks:add(spam_stanza_rejected, Host, ?MODULE, dump_spam_stanza, 50),
                    ejabberd_hooks:add(reopen_log_hook, ?MODULE, reopen_log, 50),
                    open_dump_file(DumpFile);
                {error, Reason} ->
                    Dirname = filename:dirname(DumpFile),
                    throw({open, Dirname, Reason})
            end
    end.

terminate_dumping(_Host, false) ->
    ok;
terminate_dumping(Host, Fd) ->
    DumpFile1 = get_path_option(Host),
    close_dump_file(Fd, DumpFile1),
    ejabberd_hooks:delete(spam_stanza_rejected, Host, ?MODULE, dump_spam_stanza, 50),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
        false ->
            ejabberd_hooks:delete(reopen_log_hook, ?MODULE, reopen_log, 50);
        true ->
            ok
    end.

reload_dumping(Host, Fd, OldOpts, NewOpts) ->
    case {get_path_option(Host, OldOpts), get_path_option(Host, NewOpts)} of
        {Old, Old} ->
            Fd;
        {Old, New} ->
            reopen_dump_file(Fd, Old, New)
    end.

-spec reopen_dump_file(binary(), file:io_device()) -> file:io_device().
reopen_dump_file(Host, Fd) ->
    DumpFile1 = get_path_option(Host),
    reopen_dump_file(Fd, DumpFile1, DumpFile1).

%%--------------------------------------------------------------------
%%| Hook callbacks

-spec dump_spam_stanza(message()) -> ok.
dump_spam_stanza(#message{to = #jid{lserver = LServer}} = Msg) ->
    By = jid:make(<<>>, LServer),
    Proc = get_proc_name(LServer),
    Time = erlang:timestamp(),
    Msg1 = misc:add_delay_info(Msg, By, Time),
    XML = fxml:element_to_binary(
              xmpp:encode(Msg1)),
    gen_server:cast(Proc, {dump_stanza, XML}).

-spec reopen_log() -> ok.
reopen_log() ->
    lists:foreach(fun(Host) ->
                     Proc = get_proc_name(Host),
                     gen_server:cast(Proc, reopen_log)
                  end,
                  get_spam_filter_hosts()).

%%--------------------------------------------------------------------
%%| File management

-spec open_dump_file(filename()) -> undefined | file:io_device().
open_dump_file(false) ->
    undefined;
open_dump_file(Name) ->
    Modes = [append, raw, binary, delayed_write],
    case file:open(Name, Modes) of
        {ok, Fd} ->
            ?DEBUG("Opened ~s", [Name]),
            Fd;
        {error, Reason} ->
            ?ERROR_MSG("Cannot open dump file ~s: ~s", [Name, file:format_error(Reason)]),
            undefined
    end.

-spec close_dump_file(undefined | file:io_device(), filename()) -> ok.
close_dump_file(undefined, false) ->
    ok;
close_dump_file(Fd, Name) ->
    case file:close(Fd) of
        ok ->
            ?DEBUG("Closed ~s", [Name]);
        {error, Reason} ->
            ?ERROR_MSG("Cannot close ~s: ~s", [Name, file:format_error(Reason)])
    end.

-spec reopen_dump_file(file:io_device(), binary(), binary()) -> file:io_device().
reopen_dump_file(Fd, OldDumpFile, NewDumpFile) ->
    close_dump_file(Fd, OldDumpFile),
    open_dump_file(NewDumpFile).

write_stanza_dump(Fd, XML) ->
    case file:write(Fd, [XML, <<$\n>>]) of
        ok ->
            ok;
        {error, Reason} ->
            ?ERROR_MSG("Cannot write spam to dump file: ~s", [file:format_error(Reason)])
    end.

%%--------------------------------------------------------------------
%%| Auxiliary

get_path_option(Host) ->
    Opts = gen_mod:get_module_opts(Host, ?MODULE_ANTISPAM),
    get_path_option(Host, Opts).

get_path_option(Host, Opts) ->
    case gen_mod:get_opt(spam_dump_file, Opts) of
        false ->
            false;
        true ->
            LogDirPath =
                iolist_to_binary(filename:dirname(
                                     ejabberd_logger:get_log_path())),
            filename:join([LogDirPath, <<"spam_dump_", Host/binary, ".log">>]);
        B when is_binary(B) ->
            B
    end.

%%--------------------------------------------------------------------
%%| Copied from mod_antispam.erl

-spec get_proc_name(binary()) -> atom().
get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE_ANTISPAM).

-spec get_spam_filter_hosts() -> [binary()].
get_spam_filter_hosts() ->
    [H || H <- ejabberd_option:hosts(), gen_mod:is_loaded(H, ?MODULE_ANTISPAM)].

%%--------------------------------------------------------------------

%%| vim: set foldmethod=marker foldmarker=%%|,%%-:
