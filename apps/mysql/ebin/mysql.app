{application, mysql,
 [{description, "MySQL Library"},
  {vsn, "1.0"},
  {modules, [mysql,
             mysql_auth,
             mysql_conn,
             mysql_recv]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
