%%%
%%%  Copyright © IDEALX S.A.S. 2003
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 03 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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

-define(TSUNGPATH, "TSUNGPATH").
-define(SESSION_POP_ERROR_MSG, "Total sum of session popularity is not equal to 100").

-define(DEF_REGEXP_DYNVAR_BEGIN, "name=(\"|')").%'
-define(DEF_REGEXP_DYNVAR_END, "(\"|') ([^>]* )?value=(\"|')\\([^(\"|')]*\\)(\"|')").%'

-record(config, {
          name,
          duration,           % max duration of test (by default: end when all clients are done)
          loglevel = ?WARN,
          dump = none,
          stats_backend,
          controller,         % controller machine
          clients = [],       % client machines
          servers = [],       % server(s) to test
          ports_range,        % client ports range
          monitor_hosts = [], % Cluster host to monitor (for CPU, MEM usage)
          arrivalphases = [], % arrival process specs
          thinktime,          % default thinktime specs
          subst    = false,   % Substitution should be applied on the request
          match,              % Match regexp in response
          dynvar   = [],
          main_sess_type , % main type of session
          sessions = [],
          static_users=[],
          session_tab,
          use_controller_vm = false, % if true, start the first launcher in the
                                     % same vm as the controller if possible
          curthink,  % temporary var (current request think)
          curid = 0, % temporary var (current request id (can be transaction))
          cur_req_id  = 0,   % temporary var (current real request id)
          file_server = [],  % filenames for file_server
          load_loop,         % loop phases if > 0
          hibernate = 10000, % hibernate timeout (millisec) 10sec by default
          proto_opts,        % tcp/udp buffer sizes
          seed = now,        % random seed: (default= current time)
          vhost_file = none, % file server user for virtual host jabber testing
          user_server_maxuid = none % user_id max
         }).


-record(client,
        {host,
         weight   = 1,
         maxusers,
         ip       = []
        }).

-record(server,
        {host,
         port,
         type
        }).
-record(session,
        { id,
          popularity,
          type,
          name,
          persistent   = false,
          bidi         = false,
          hibernate,
          proto_opts,
          size
        }).

-record(arrivalphase,
        {phase,
         duration,
         unit,
         number, %% ?
         intensity,
         maxnumber
        }).
