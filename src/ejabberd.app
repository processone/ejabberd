% $Id$

{application, ejabberd,
 [{description, "ejabberd"},
  {vsn, "0.0.1-alpha"},
  {modules, [acl,
	     ejabberd,
	     ejabberd_auth,
	     ejabberd_c2s,
	     ejabberd_config,
	     ejabberd_listener,
	     ejabberd_local,
	     ejabberd_router,
	     ejabberd_s2s,
	     ejabberd_s2s_in,
	     ejabberd_s2s_out,
	     ejabberd_service,
	     ejabberd_sm,
	     jlib,
	     mod_configure,
	     mod_disco,
	     mod_echo,
	     mod_offline,
	     mod_private,
	     mod_register,
	     mod_roster,
	     mod_stats,
	     mod_time,
	     mod_vcard,
	     mod_version,
	     randoms,
	     sha,
	     translate,
	     xml,
	     xml_stream
	    ]},
  {registered, [ejabberd,
		ejabberd_auth,
		ejabberd_router,
		ejabberd_sm,
		ejabberd_s2s,
		ejabberd_local,
		ejabberd_mod_roster,
		ejabberd_listeners
	       ]},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {ejabberd_app, []}}]}.



% Local Variables:
% mode: erlang
% End:
