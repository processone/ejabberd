%%%-------------------------------------------------------------------
%%% File    : ejabberd_admin.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Description : This module gathers admin functions used by different
%%%               access method:
%%%               - ejabberdctl command-line tool
%%%               - web admin interface
%%%               - adhoc mode
%%%
%%% Created :  7 May 2006 by Mickael Remond <mremond@process-one.net>
%%% Id      : $Id: $
%%%----------------------------------------------------------------------
%%% Copyright (c) 2006 Process One
%%%-------------------------------------------------------------------

-module(ejabberd_admin).
-author('mickael.remond@process-one.net').
-vsn('$Revision: $ ').

-export([restore/1]).

-include("ejabberd.hrl").

%% Mnesia database restore
%% This function is called from ejabberd_ctl, ejabberd_web_admin and
%% mod_configure/adhoc 
restore(Path) ->
    mnesia:restore(Path, [{keep_tables,keep_tables()},
			  {default_op, skip_tables}]).

%% This function return a list of tables that should be kept from a previous
%% version backup.
%% Obsolete tables or tables created by module who are no longer used are not
%% restored and are ignored.
keep_tables() ->
    lists:flatten([acl, passwd, config, local_config, disco_publish,
		   keep_modules_tables()]).

%% Returns the list of modules tables in use, according to the list of actually
%% loaded modules
keep_modules_tables() ->		      
    lists:map(fun(Module) -> module_tables(Module) end,
	      gen_mod:loaded_modules(?MYNAME)).

%% TODO: This mapping should probably be moved to a callback function in each
%% module.
%% Mapping between modules and their tables
module_tables(mod_announce) -> [motd, motd_users];
module_tables(mod_irc) -> [irc_custom];
module_tables(mod_last) -> [last_activity];
module_tables(mod_muc) -> [muc_room, muc_registered];
module_tables(mod_offline) -> [offline_msg];
module_tables(mod_privacy) -> [privacy];
module_tables(mod_private) -> [private_storage];
module_tables(mod_pubsub) -> [pubsub_node];
module_tables(mod_roster) -> [roster];
module_tables(mod_shared_roster) -> [sr_group, sr_user];
module_tables(mod_vcard) -> [vcard, vcard_search];
module_tables(_Other) -> [].
