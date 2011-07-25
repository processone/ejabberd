

-define(tcp_buffer, 65536).
-define(lifetime, 120000).

-record(state_rec, {log_file,    % logfile name
                    logfd,       % logfile IODevice
                    prev_port,   % previous port
                    prev_scheme, % previous scheme
                    prev_host,   % previous hostname
                    timestamp=0, % last request date
                    ext_file_id, % counter of external files (use for ex. in HTTP POST req)
                    plugin,
                    thinktime_low = 1000 % dot not record thinktime less than this
                                                % value (msec)
               }).


-record(proxy, {
          clientsock,
          http_version,
          close, % must close client socket (connection:close header was send by server)
          parse_status   = new, %% http status = body|new
          body_size      = 0,
          content_length = 0,
          parent_proxy   = false,
          buffer = [],
          plugin,
          serversock
          }).

