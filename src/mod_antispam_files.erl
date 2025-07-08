%%%----------------------------------------------------------------------
%%% File    : mod_antispam_files.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Filter spam messages based on sender JID and content
%%% Created : 31 Mar 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2019-2025 ProcessOne
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

%%| definitions
%% @format-begin

-module(mod_antispam_files).

-author('holger@zedat.fu-berlin.de').
-author('stefan@strigler.de').

%% Exported
-export([init_files/1, terminate_files/1]).
% Hooks
-export([get_files_lists/2]).

-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include("mod_antispam.hrl").
-include("translate.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-type files_map() :: #{atom() => filename()}.
-type lists_map() ::
    #{jid => jid_set(),
      url => url_set(),
      atom() => sets:set(binary())}.

-define(COMMAND_TIMEOUT, timer:seconds(30)).
-define(DEFAULT_CACHE_SIZE, 10000).
-define(HTTPC_TIMEOUT, timer:seconds(3)).

%%--------------------------------------------------------------------
%%| Exported

init_files(Host) ->
    ejabberd_hooks:add(antispam_get_lists, Host, ?MODULE, get_files_lists, 50).

terminate_files(Host) ->
    ejabberd_hooks:delete(antispam_get_lists, Host, ?MODULE, get_files_lists, 50).

%%--------------------------------------------------------------------
%%| Hooks

-spec get_files_lists(lists_map(), files_map()) -> lists_map().
get_files_lists(#{jid := AccJids,
                  url := AccUrls,
                  domains := AccDomains,
                  whitelist_domains := AccWhitelist} =
                    Acc,
                Files) ->
    try read_files(Files) of
        #{jid := JIDsSet,
          url := URLsSet,
          domains := SpamDomainsSet,
          whitelist_domains := WhitelistDomains} ->
            Acc#{jid => sets:union(AccJids, JIDsSet),
                 url => sets:union(AccUrls, URLsSet),
                 domains => sets:union(AccDomains, SpamDomainsSet),
                 whitelist_domains => sets:union(AccWhitelist, WhitelistDomains)}
    catch
        {Op, File, Reason} when Op == open; Op == read ->
            ErrorText = format("Error trying to ~s file ~s: ~s", [Op, File, format_error(Reason)]),
            ?CRITICAL_MSG(ErrorText, []),
            {stop, {config_error, ErrorText}}
    end.

%%--------------------------------------------------------------------
%%| read_files

-spec read_files(files_map()) -> lists_map().
read_files(Files) ->
    maps:map(fun(Type, Filename) -> read_file(Filename, line_parser(Type)) end, Files).

-spec line_parser(Type :: atom()) -> fun((binary()) -> binary()).
line_parser(jid) ->
    fun parse_jid/1;
line_parser(url) ->
    fun parse_url/1;
line_parser(_) ->
    fun trim/1.

-spec read_file(filename(), fun((binary()) -> ljid() | url())) -> jid_set() | url_set().
read_file(none, _ParseLine) ->
    sets:new();
read_file(File, ParseLine) ->
    case file:open(File, [read, binary, raw, {read_ahead, 65536}]) of
        {ok, Fd} ->
            try
                read_line(Fd, ParseLine, sets:new())
            catch
                E ->
                    throw({read, File, E})
            after
                ok = file:close(Fd)
            end;
        {error, Reason} ->
            throw({open, File, Reason})
    end.

-spec read_line(file:io_device(),
                fun((binary()) -> ljid() | url()),
                jid_set() | url_set()) ->
                   jid_set() | url_set().
read_line(Fd, ParseLine, Set) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            read_line(Fd, ParseLine, sets:add_element(ParseLine(Line), Set));
        {error, Reason} ->
            throw(Reason);
        eof ->
            Set
    end.

-spec parse_jid(binary()) -> ljid().
parse_jid(S) ->
    try jid:decode(trim(S)) of
        #jid{} = JID ->
            jid:remove_resource(
                jid:tolower(JID))
    catch
        _:{bad_jid, _} ->
            throw({bad_jid, S})
    end.

-spec parse_url(binary()) -> url().
parse_url(S) ->
    URL = trim(S),
    RE = <<"https?://\\S+$">>,
    Options = [anchored, caseless, {capture, none}],
    case re:run(URL, RE, Options) of
        match ->
            URL;
        nomatch ->
            throw({bad_url, S})
    end.

-spec trim(binary()) -> binary().
trim(S) ->
    re:replace(S, <<"\\s+$">>, <<>>, [{return, binary}]).

%% Function copied from mod_antispam.erl
-spec format(io:format(), [term()]) -> binary().
format(Format, Data) ->
    iolist_to_binary(io_lib:format(Format, Data)).

-spec format_error(atom() | tuple()) -> binary().
format_error({bad_jid, JID}) ->
    <<"Not a valid JID: ", JID/binary>>;
format_error({bad_url, URL}) ->
    <<"Not an HTTP(S) URL: ", URL/binary>>;
format_error(Reason) ->
    list_to_binary(file:format_error(Reason)).

%%--------------------------------------------------------------------

%%| vim: set foldmethod=marker foldmarker=%%|,%%-:
