%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-record(dyndata,
        {dynvars = [], % dynamic variables
         proto         % dynamic data specific to protocol (#http_dyndata for HTTP)
       }).

-record(match,
        { regexp,
          subst = false,
         'when' = false,
          do    = continue, %(continue | loop | abort | log )
          sleep_loop,       % in seconds
          apply_to_content,
          skip_headers = no,
          max_loop,
          loop_back,
          max_restart
       }).

-record(ts_request,
        {
         ack,
         subst=false,
         match=[],
         dynvar_specs=[], % [] | [{VarName, Regexp} |...]
         param,
         endpage=false,
         host,       % override global server hostname
         port,       % override global server port
         scheme      % override global server type (ssl or gen_tcp)
        }).

% protocol options
-record(proto_opts,
        {ssl_ciphers   = negociate, % for ssl only
         retry_timeout = 10,        % retry sending in microsec
         idle_timeout  = 600000,
         tcp_rcv_size  = 32768,     % tcp buffers size
         tcp_snd_size  = 32768,
         udp_rcv_size,              % udp buffers size
         udp_snd_size}).

-define(size_mon_thresh, 524288).   % 512KB

% state of ts_client gen_server
-record(state_rcv,
        {socket=none, %
         ip,          % local ip to bind to
         timeout,     % ?
         retries=0,   % number of connect retries
         hibernate = 10000, % hibernate if thinktime is >= to this (10sec by default)
         host,        % hostname (or IP) of remote server
         port,        % server port
         protocol,    % gen_udp, gen_tcp or ssl
         proto_opts = #proto_opts{},  %
         bidi = false,% true if bidirectional protocol

         session_id,
         request,     % current request specs
         persistent,  % if true, don't exit when connexion is closed
         timestamp,   % previous message date
         starttime,   % date of the beginning of the session
         count,       % number of requests waiting to be sent
         maxcount,        % number of requests waiting to be sent
         ack_done=false,  % 'true' if the ack was sent, else 'false' (unused if ack=no_ack)
         send_timestamp,  % date when the 'request' was sent
         page_timestamp=0,% date when the first 'request' of a page was sent
         acc=[],     % Accumulator to store temporary unparsable data
                     % (Waiting for more data)
         buffer = <<>>, % buffer when we have to keep the response (we need
                     % all the response to do pattern matching)
         session,    % record of session status; depends on 'clienttype'
         datasize=0,
         id,         % user id
         size_mon_thresh=?size_mon_thresh, % if rcv data is > to this, update stats
         dyndata=[], % persistent data dynamically added during the
                     % session (Cookies for examples)
         clienttype, % module name (ts_jabber, etc.)
         transactions=[], % current transactions
         dump        % type of dump (full, light, none)
        }).

-define(restart_sleep, 2000).
-define(infinity_timeout, 15000).
-define(short_timeout, 1).
-define(config_timeout, 60000).
-define(check_noclient_timeout, 60000).
-define(retries, 4).

-define(TIMEOUT_PARALLEL_SPAWN, 60000).
-define(MAX_PHASE_EXCEED_PERCENT, 20).
-define(MAX_PHASE_EXCEED_NUSERS, 10).

-define(CRLF, "\r\n").
-define(CR,13).
-define(LF,10).

%% retry sending message after this timeout (in microsec.)
-define(config(Var), ts_utils:get_val(Var)).

-define(messages_intensity, 1/(ts_utils:get_val(messages_interarrival)*1000)).
-define(clients_intensity, 1/(ts_utils:get_val(interarrival)*1000)).


%% errors messages

-define(LOGF(Msg, Args, Level),
        ts_utils:debug(?MODULE, Msg, Args, Level)).
-define(LOG(Msg, Level),
        ts_utils:debug(?MODULE, Msg, Level)).

%% Debug messages can be completely disabled if DEBUG is not defined
-ifdef(DEBUG).
    -define(TRACE, [{debug, [trace]}]).
    -define(DebugF(Msg, Args),
            ts_utils:debug(?MODULE, Msg, Args, ?DEB)).
    -define(Debug(Msg),
            ts_utils:debug(?MODULE, Msg, ?DEB)).
-else.
    -define(TRACE, []).
    -define(DebugF(Msg, Args), ok).
    -define(Debug(Msg), ok).
-endif.

-define(EMERG, 0). % The system is unusable.
-define(ALERT, 1). % Action should be taken immediately to address the problem.
-define(CRIT, 2).  % A critical condition has occurred.
-define(ERR, 3).   % An error has occurred.
-define(WARN, 4).  % A significant event that may require attention has occurred.
-define(NOTICE, 5).% An event that does not affect system operation has occurred.
-define(INFO, 6).  % An normal operation has occurred.
-define(DEB, 7).   % Debugging info
