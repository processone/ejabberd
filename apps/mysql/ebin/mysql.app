{application, mysql,
 [{description, "MySQL Library"},
  {vsn, "666"},
  {modules, [mysql,
             mysql_auth,
             mysql_conn,
             mysql_recv]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
