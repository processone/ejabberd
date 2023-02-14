--
-- ejabberd, Copyright (C) 2002-2023   ProcessOne
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--

SET ANSI_PADDING OFF;
SET ANSI_NULLS ON;
SET QUOTED_IDENTIFIER ON;
SET ANSI_PADDING ON;

CREATE TABLE [dbo].[archive] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [timestamp] [bigint] NOT NULL,
        [peer] [varchar] (250) NOT NULL,
        [bare_peer] [varchar] (250) NOT NULL,
        [xml] [ntext] NOT NULL,
        [txt] [ntext] NULL,
        [id] [bigint] IDENTITY(1,1) NOT NULL,
        [kind] [varchar] (10) NULL,
        [nick] [varchar] (250) NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [archive_PK] PRIMARY KEY CLUSTERED
(
        [id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [archive_sh_username_timestamp] ON [archive] (server_host, username, timestamp)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [archive_sh_username_peer] ON [archive] (server_host, username, peer)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [archive_sh_username_bare_peer] ON [archive] (server_host, username, bare_peer)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [archive_sh_timestamp] ON [archive] (server_host, timestamp)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[archive_prefs] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [def] [text] NOT NULL,
        [always] [text] NOT NULL,
        [never] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [archive_prefs_PRIMARY] PRIMARY KEY CLUSTERED
(
        [server_host] ASC,
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[caps_features] (
        [node] [varchar] (250) NOT NULL,
        [subnode] [varchar] (250) NOT NULL,
        [feature] [text] NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE CLUSTERED INDEX [caps_features_node_subnode] ON [caps_features] (node, subnode)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[last] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [seconds] [text] NOT NULL,
        [state] [text] NOT NULL,
 CONSTRAINT [last_PRIMARY] PRIMARY KEY CLUSTERED
(
        [server_host] ASC,
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[motd] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [xml] [text] NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [motd_PRIMARY] PRIMARY KEY CLUSTERED
(
        [server_host] ASC,
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[muc_registered] (
        [jid] [varchar] (255) NOT NULL,
        [host] [varchar] (255) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [nick] [varchar] (255) NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
);

CREATE INDEX [muc_registered_nick] ON [muc_registered] (nick)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE UNIQUE CLUSTERED INDEX [muc_registered_jid_host] ON [muc_registered] (jid, host)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[muc_room] (
        [name] [varchar] (250) NOT NULL,
        [host] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [opts] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [muc_room_name_host] ON [muc_room] (name, host)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
CREATE INDEX [muc_room_host_created_at] ON [muc_registered] (host, nick)
    WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[muc_online_room] (
    [name] [varchar] (250) NOT NULL,
    [host] [varchar] (250) NOT NULL,
    [server_host] [varchar] (250) NOT NULL,
    [node] [varchar] (250) NOT NULL,
    [pid] [varchar] (100) NOT NULL
);

CREATE UNIQUE CLUSTERED INDEX [muc_online_room_name_host] ON [muc_online_room] (name, host)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[muc_online_users] (
    [username] [varchar] (250) NOT NULL,
    [server] [varchar] (250) NOT NULL,
    [resource] [varchar] (250) NOT NULL,
    [name] [varchar] (250) NOT NULL,
    [host] [varchar] (250) NOT NULL,
    [server_host] [varchar] (250) NOT NULL,
    [node] [varchar] (250) NOT NULL
);

CREATE UNIQUE INDEX [muc_online_users_i] ON [muc_online_users] (username, server, resource, name, host)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[muc_room_subscribers] (
        [room] [varchar] (191) NOT NULL,
        [host] [varchar] (191) NOT NULL,
        [jid] [varchar] (191) NOT NULL,
        [nick] [text] NOT NULL,
        [nodes] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
);

CREATE UNIQUE CLUSTERED INDEX [muc_room_subscribers_host_room_jid] ON [muc_room_subscribers] (host, room, jid);
CREATE INDEX [muc_room_subscribers_host_jid] ON [muc_room_subscribers] (host, jid);
CREATE INDEX [muc_room_subscribers_jid] ON [muc_room_subscribers] (jid);

CREATE TABLE [dbo].[privacy_default_list] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [name] [varchar] (250) NOT NULL,
 CONSTRAINT [privacy_default_list_PRIMARY] PRIMARY KEY CLUSTERED
(
        [server_host] ASC,
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
);

CREATE TABLE [dbo].[privacy_list] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [name] [varchar] (250) NOT NULL,
        [id] [bigint] IDENTITY(1,1) NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [privacy_list_PK] PRIMARY KEY CLUSTERED
(
        [id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
);

CREATE UNIQUE INDEX [privacy_list_sh_username_name] ON [privacy_list] (server_host, username, name)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[privacy_list_data] (
        [id] [bigint] NULL,
        [t] [char] (1) NOT NULL,
        [value] [text] NOT NULL,
        [action] [char] (1) NOT NULL,
        [ord] [smallint] NOT NULL,
        [match_all] [smallint] NOT NULL,
        [match_iq] [smallint] NOT NULL,
        [match_message] [smallint] NOT NULL,
        [match_presence_in] [smallint] NOT NULL,
        [match_presence_out] [smallint] NOT NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE CLUSTERED INDEX [privacy_list_data_id] ON [privacy_list_data] (id)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[private_storage] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [namespace] [varchar] (250) NOT NULL,
        [data] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [private_storage_sh_username_namespace] ON [private_storage] (server_host, username, namespace)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_item] (
        [nodeid] [bigint] NULL,
        [itemid] [varchar] (255) NOT NULL,
        [publisher] [varchar] (250) NOT NULL,
        [creation] [varchar] (32) NOT NULL,
        [modification] [varchar] (32) NOT NULL,
        [payload] [text] NOT NULL DEFAULT ''
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [pubsub_item_itemid] ON [pubsub_item] (itemid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE UNIQUE CLUSTERED INDEX [pubsub_item_nodeid_itemid] ON [pubsub_item] (nodeid, itemid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_node_option] (
        [nodeid] [bigint] NULL,
        [name] [varchar] (250) NOT NULL,
        [val] [varchar] (250) NOT NULL
);

CREATE CLUSTERED INDEX [pubsub_node_option_nodeid] ON [pubsub_node_option] (nodeid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_node_owner] (
        [nodeid] [bigint] NULL,
        [owner] [text] NOT NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE CLUSTERED INDEX [pubsub_node_owner_nodeid] ON [pubsub_node_owner] (nodeid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_state] (
        [nodeid] [bigint] NULL,
        [jid] [varchar] (255) NOT NULL,
        [affiliation] [char] (1) NOT NULL,
        [subscriptions] [text] NOT NULL DEFAULT '',
        [stateid] [bigint] IDENTITY(1,1) NOT NULL,
 CONSTRAINT [pubsub_state_PRIMARY] PRIMARY KEY CLUSTERED
(
        [stateid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [pubsub_state_jid] ON [pubsub_state] (jid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE UNIQUE INDEX [pubsub_state_nodeid_jid] ON [pubsub_state] (nodeid, jid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_subscription_opt] (
        [subid] [varchar] (255) NOT NULL,
        [opt_name] [varchar] (32) NOT NULL,
        [opt_value] [text] NOT NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [pubsub_subscription_opt_subid_opt_name] ON [pubsub_subscription_opt] (subid, opt_name)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_node] (
        [host] [varchar] (255) NOT NULL,
        [node] [varchar] (255) NOT NULL,
        [parent] [varchar] (255) NOT NULL DEFAULT '',
        [plugin] [varchar] (32) NOT NULL,
        [nodeid] [bigint] IDENTITY(1,1) NOT NULL,
 CONSTRAINT [pubsub_node_PRIMARY] PRIMARY KEY CLUSTERED
(
        [nodeid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
);

CREATE INDEX [pubsub_node_parent] ON [pubsub_node] (parent)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE UNIQUE INDEX [pubsub_node_host_node] ON [pubsub_node] (host, node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[roster_version] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [version] [text] NOT NULL,
 CONSTRAINT [roster_version_PRIMARY] PRIMARY KEY CLUSTERED
(
        [server_host] ASC,
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[rostergroups] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [jid] [varchar] (250) NOT NULL,
        [grp] [text] NOT NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE CLUSTERED INDEX [rostergroups_sh_username_jid] ON [rostergroups] ([server_host], [username], [jid])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[rosterusers] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [jid] [varchar] (250) NOT NULL,
        [nick] [text] NOT NULL,
        [subscription] [char] (1) NOT NULL,
        [ask] [char] (1) NOT NULL,
        [askmessage] [text] NOT NULL,
        [server] [char] (1) NOT NULL,
        [subscribe] [text] NOT NULL,
        [type] [text] NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [rosterusers_sh_username_jid] ON [rosterusers] ([server_host], [username], [jid])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [rosterusers_sh_jid] ON [rosterusers] ([server_host], [jid])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[sm] (
        [usec] [bigint] NOT NULL,
        [pid] [varchar] (100) NOT NULL,
        [node] [varchar] (255) NOT NULL,
        [username] [varchar] (255) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [resource] [varchar] (255) NOT NULL,
        [priority] [text] NOT NULL,
        [info] [text] NOT NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [sm_sid] ON [sm] (usec, pid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [sm_node] ON [sm] (node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [sm_sh_username] ON [sm] (server_host, username)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[spool] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [xml] [text] NOT NULL,
        [seq] [bigint] IDENTITY(1,1) NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [spool_PK] PRIMARY KEY CLUSTERED
(
        [seq] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [spool_sh_username] ON [spool] (server_host, username)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [spool_created_at] ON [spool] (created_at)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
;

CREATE TABLE [dbo].[sr_group] (
        [name] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [opts] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [sr_group_sh_name] ON [sr_group] ([server_host], [name])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[sr_user] (
        [jid] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [grp] [varchar] (250) NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
);

CREATE UNIQUE CLUSTERED INDEX [sr_user_sh_jid_group] ON [sr_user] ([server_host], [jid], [grp])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [sr_user_sh_grp] ON [sr_user] ([server_host], [grp])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[users] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [password] [text] NOT NULL,
        [serverkey] [text] NOT NULL DEFAULT '',
        [salt] [text] NOT NULL DEFAULT '',
        [iterationcount] [smallint] NOT NULL DEFAULT 0,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [users_PRIMARY] PRIMARY KEY CLUSTERED
(
        [server_host] ASC,
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[vcard] (
        [username] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [vcard] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [vcard_PRIMARY] PRIMARY KEY CLUSTERED
(
        [server_host] ASC,
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[vcard_search] (
        [username] [varchar] (250) NOT NULL,
        [lusername] [varchar] (250) NOT NULL,
        [server_host] [varchar] (250) NOT NULL,
        [fn] [text] NOT NULL,
        [lfn] [varchar] (250) NOT NULL,
        [family] [text] NOT NULL,
        [lfamily] [varchar] (250) NOT NULL,
        [given] [text] NOT NULL,
        [lgiven] [varchar] (250) NOT NULL,
        [middle] [text] NOT NULL,
        [lmiddle] [varchar] (250) NOT NULL,
        [nickname] [text] NOT NULL,
        [lnickname] [varchar] (250) NOT NULL,
        [bday] [text] NOT NULL,
        [lbday] [varchar] (250) NOT NULL,
        [ctry] [text] NOT NULL,
        [lctry] [varchar] (250) NOT NULL,
        [locality] [text] NOT NULL,
        [llocality] [varchar] (250) NOT NULL,
        [email] [text] NOT NULL,
        [lemail] [varchar] (250) NOT NULL,
        [orgname] [text] NOT NULL,
        [lorgname] [varchar] (250) NOT NULL,
        [orgunit] [text] NOT NULL,
        [lorgunit] [varchar] (250) NOT NULL,
 CONSTRAINT [vcard_search_PRIMARY] PRIMARY KEY CLUSTERED
(
        [server_host] ASC,
        [lusername] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [vcard_search_sh_lfn] ON [vcard_search] (server_host, lfn)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_lfamily] ON [vcard_search] (server_host, lfamily)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_lgiven] ON [vcard_search] (server_host, lgiven)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_lmiddle] ON [vcard_search] (server_host, lmiddle)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_lnickname] ON [vcard_search] (server_host, lnickname)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_lbday] ON [vcard_search] (server_host, lbday)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_lctry] ON [vcard_search] (server_host, lctry)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_llocality] ON [vcard_search] (server_host, llocality)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_lemail] ON [vcard_search] (server_host, lemail)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_lorgname] ON [vcard_search] (server_host, lorgname)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_sh_lorgunit] ON [vcard_search] (server_host, lorgunit)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

ALTER TABLE [dbo].[pubsub_item]  WITH CHECK ADD  CONSTRAINT [pubsub_item_ibfk_1] FOREIGN KEY([nodeid])
REFERENCES [dbo].[pubsub_node] ([nodeid])
ON DELETE CASCADE;

ALTER TABLE [dbo].[pubsub_item] CHECK CONSTRAINT [pubsub_item_ibfk_1];

ALTER TABLE [dbo].[pubsub_node_option]  WITH CHECK ADD  CONSTRAINT [pubsub_node_option_ibfk_1] FOREIGN KEY([nodeid])
REFERENCES [dbo].[pubsub_node] ([nodeid])
ON DELETE CASCADE;

ALTER TABLE [dbo].[pubsub_node_option] CHECK CONSTRAINT [pubsub_node_option_ibfk_1];

ALTER TABLE [dbo].[pubsub_node_owner]  WITH CHECK ADD  CONSTRAINT [pubsub_node_owner_ibfk_1] FOREIGN KEY([nodeid])
REFERENCES [dbo].[pubsub_node] ([nodeid])
ON DELETE CASCADE;

ALTER TABLE [dbo].[pubsub_node_owner] CHECK CONSTRAINT [pubsub_node_owner_ibfk_1];

ALTER TABLE [dbo].[pubsub_state]  WITH CHECK ADD  CONSTRAINT [pubsub_state_ibfk_1] FOREIGN KEY([nodeid])
REFERENCES [dbo].[pubsub_node] ([nodeid])
ON DELETE CASCADE;

ALTER TABLE [dbo].[pubsub_state] CHECK CONSTRAINT [pubsub_state_ibfk_1];

CREATE TABLE [dbo].[oauth_token] (
    [token] [varchar] (250) NOT NULL,
    [jid] [text] NOT NULL,
    [scope] [text] NOT NULL,
    [expire] [bigint] NOT NULL,
 CONSTRAINT [oauth_token_PRIMARY] PRIMARY KEY CLUSTERED
(
        [token] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[route] (
    [domain] [varchar] (255) NOT NULL,
    [server_host] [varchar] (255) NOT NULL,
    [node] [varchar] (255) NOT NULL,
    [pid] [varchar](100) NOT NULL,
    [local_hint] [text] NOT NULL
);

CREATE UNIQUE CLUSTERED INDEX [route_i] ON [route] (domain, server_host, node, pid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[bosh] (
    [sid] [varchar] (255) NOT NULL,
    [node] [varchar] (255) NOT NULL,
    [pid] [varchar](100) NOT NULL
 CONSTRAINT [bosh_PRIMARY] PRIMARY KEY CLUSTERED
(
        [sid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
);

CREATE TABLE [dbo].[push_session] (
    [username] [varchar] (255) NOT NULL,
    [server_host] [varchar] (250) NOT NULL,
    [timestamp] [bigint] NOT NULL,
    [service] [varchar] (255) NOT NULL,
    [node] [varchar] (255) NOT NULL,
    [xml] [varchar] (255) NOT NULL
);

CREATE UNIQUE NONCLUSTERED INDEX [push_session_susn] ON [push_session] (server_host, username, service, node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [push_session_sh_username_timestamp] ON [push_session] (server_host, username, timestamp)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[mix_channel] (
    [channel] [varchar] (250) NOT NULL,
    [service] [varchar] (250) NOT NULL,
    [username] [varchar] (250) NOT NULL,
    [domain] [varchar] (250) NOT NULL,
    [jid] [varchar] (250) NOT NULL,
    [hidden] [smallint] NOT NULL,
    [hmac_key] [text] NOT NULL,
    [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [mix_channel] ON [mix_channel] (channel, service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_channel_serv] ON [mix_channel] (service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[mix_participant] (
    [channel] [varchar] (250) NOT NULL,
    [service] [varchar] (250) NOT NULL,
    [username] [varchar] (250) NOT NULL,
    [domain] [varchar] (250) NOT NULL,
    [jid] [varchar] (250) NOT NULL,
    [id] [text] NOT NULL,
    [nick] [text] NOT NULL,
    [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE INDEX [mix_participant] ON [mix_participant] (channel, service, username, domain)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_participant_chan_serv] ON [mix_participant] (channel, service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[mix_subscription] (
    [channel] [varchar] (250) NOT NULL,
    [service] [varchar] (250) NOT NULL,
    [username] [varchar] (250) NOT NULL,
    [domain] [varchar] (250) NOT NULL,
    [node] [varchar] (250) NOT NULL,
    [jid] [varchar] (250) NOT NULL
);

CREATE UNIQUE INDEX [mix_subscription] ON [mix_subscription] (channel, service, username, domain, node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_subscription_chan_serv_ud] ON [mix_subscription] (channel, service, username, domain)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_subscription_chan_serv_node] ON [mix_subscription] (channel, service, node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_subscription_chan_serv] ON [mix_subscription] (channel, service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[mix_pam] (
    [username] [varchar] (250) NOT NULL,
    [server_host] [varchar] (250) NOT NULL,
    [channel] [varchar] (250) NOT NULL,
    [service] [varchar] (250) NOT NULL,
    [id] [text] NOT NULL,
    [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE NONCLUSTERED INDEX [mix_pam] ON [mix_pam] (username, server_host, channel, service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[mqtt_pub] (
    [username] [varchar] (250) NOT NULL,
    [server_host] [varchar] (250) NOT NULL,
    [resource] [varchar] (250) NOT NULL,
    [topic] [varchar] (250) NOT NULL,
    [qos] [tinyint] NOT NULL,
    [payload] [varbinary](max) NOT NULL,
    [payload_format] [tinyint] NOT NULL,
    [content_type] [text] NOT NULL,
    [response_topic] [text] NOT NULL,
    [correlation_data] [varbinary](max) NOT NULL,
    [user_properties] [varbinary](max) NOT NULL,
    [expiry] [int] NOT NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [mqtt_topic_server] ON [mqtt_pub] (topic, server_host)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
