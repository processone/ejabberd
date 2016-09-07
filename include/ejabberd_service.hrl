-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-type filter_attr() :: {binary(), [binary()]}.

-record(state,
        {socket                    :: ejabberd_socket:socket_state(),
         sockmod = ejabberd_socket :: ejabberd_socket | ejabberd_frontend_socket,
         streamid = <<"">>         :: binary(),
         host_opts = dict:new()    :: ?TDICT,
         host = <<"">>             :: binary(),
         access                    :: atom(),
         check_from = true         :: boolean(),
         server_hosts = ?MYHOSTS   :: [binary()],
         privilege_access          :: [attr()],
         delegations               :: [filter_attr()],
         last_pres = dict:new()    :: ?TDICT}).

-type(state() :: #state{} ).
