--
-- ejabberd, Copyright (C) 2002-2015   ProcessOne
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
        [timestamp] [bigint] NOT NULL,
        [peer] [varchar] (250) NOT NULL,
        [bare_peer] [varchar] (250) NOT NULL,
        [xml] [text] NOT NULL,
        [txt] [text] NULL,
        [id] [bigint] IDENTITY(1,1) NOT NULL,
        [kind] [varchar] (10) NULL,
        [nick] [varchar] (250) NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [archive_PK] PRIMARY KEY CLUSTERED 
(
        [id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [archive_username] ON [archive] (username)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [archive_timestamp] ON [archive] (timestamp)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [archive_peer] ON [archive] (peer)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [archive_bare_peer] ON [archive] (bare_peer)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[archive_prefs] (
        [username] [varchar] (250) NOT NULL,
        [def] [text] NOT NULL,
        [always] [text] NOT NULL,
        [never] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [archive_prefs_PRIMARY] PRIMARY KEY CLUSTERED 
(
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

CREATE TABLE [dbo].[irc_custom] (
        [jid] [varchar] (255) NOT NULL,
        [host] [varchar] (255) NOT NULL,
        [data] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [irc_custom_jid_host] ON [irc_custom] (jid, host)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[last] (
        [username] [varchar] (250) NOT NULL,
        [seconds] [text] NOT NULL,
        [state] [text] NOT NULL,
 CONSTRAINT [last_PRIMARY] PRIMARY KEY CLUSTERED 
(
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[motd] (
        [username] [varchar] (250) NOT NULL,
        [xml] [text] NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [motd_PRIMARY] PRIMARY KEY CLUSTERED 
(
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[muc_registered] (
        [jid] [varchar] (255) NOT NULL,
        [host] [varchar] (255) NOT NULL,
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
        [opts] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [muc_room_name_host] ON [muc_room] (name, host)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[privacy_default_list] (
        [username] [varchar] (250) NOT NULL,
        [name] [varchar] (250) NOT NULL,
 CONSTRAINT [privacy_default_list_PRIMARY] PRIMARY KEY CLUSTERED 
(
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
);

CREATE TABLE [dbo].[privacy_list] (
        [username] [varchar] (250) NOT NULL,
        [name] [varchar] (250) NOT NULL,
        [id] [bigint] IDENTITY(1,1) NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [privacy_list_PK] PRIMARY KEY CLUSTERED 
(
        [id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
);

CREATE INDEX [privacy_list_username] ON [privacy_list] (username)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE UNIQUE INDEX [privacy_list_username_name] ON [privacy_list] (username, name)
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
        [namespace] [varchar] (250) NOT NULL,
        [data] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [private_storage_username] ON [private_storage] (username)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE UNIQUE CLUSTERED INDEX [private_storage_username_namespace] ON [private_storage] (username, namespace)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_item] (
        [nodeid] [bigint] NULL,
        [itemid] [varchar] (255) NULL,
        [publisher] [text] NULL,
        [creation] [text] NULL,
        [modification] [varchar] (255) NULL,
        [payload] [text] NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [pubsub_item_itemid] ON [pubsub_item] (itemid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE UNIQUE CLUSTERED INDEX [pubsub_item_nodeid_itemid] ON [pubsub_item] (nodeid, itemid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_node_option] (
        [nodeid] [bigint] NULL,
        [name] [text] NULL,
        [val] [text] NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE CLUSTERED INDEX [pubsub_node_option_nodeid] ON [pubsub_node_option] (nodeid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_node_owner] (
        [nodeid] [bigint] NULL,
        [owner] [text] NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE CLUSTERED INDEX [pubsub_node_owner_nodeid] ON [pubsub_node_owner] (nodeid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_state] (
        [nodeid] [bigint] NULL,
        [jid] [varchar] (255) NULL,
        [affiliation] [char] (1) NULL,
        [subscriptions] [text] NULL,
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
        [subid] [varchar] (255) NULL,
        [opt_name] [varchar] (32) NULL,
        [opt_value] [text] NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [pubsub_subscription_opt_subid_opt_name] ON [pubsub_subscription_opt] (subid, opt_name)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[pubsub_node] (
        [host] [varchar] (255) NULL,
        [node] [varchar] (255) NULL,
        [parent] [varchar] (255) NULL,
        [type] [text] NULL,
        [nodeid] [bigint] IDENTITY(1,1) NOT NULL,
 CONSTRAINT [pubsub_node_PRIMARY] PRIMARY KEY CLUSTERED 
(
        [nodeid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [pubsub_node_parent] ON [pubsub_node] (parent)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE UNIQUE INDEX [pubsub_node_host_node] ON [pubsub_node] (host, node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[roster_version] (
        [username] [varchar] (250) NOT NULL,
        [version] [text] NOT NULL,
 CONSTRAINT [roster_version_PRIMARY] PRIMARY KEY CLUSTERED 
(
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[rostergroups] (
        [username] [varchar] (250) NOT NULL,
        [jid] [varchar] (250) NOT NULL,
        [grp] [text] NOT NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE CLUSTERED INDEX [rostergroups_username_jid] ON [rostergroups] ([username], [jid])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[rosterusers] (
        [username] [varchar] (250) NOT NULL,
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

CREATE UNIQUE CLUSTERED INDEX [rosterusers_username_jid] ON [rosterusers] ([username], [jid])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [rosterusers_username] ON [rosterusers] ([username])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [rosterusers_jid] ON [rosterusers] ([jid])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[sm] (
        [usec] [bigint] NOT NULL,
        [pid] [varchar] (100) NOT NULL,
        [node] [varchar] (255) NOT NULL,
        [username] [varchar] (255) NOT NULL,
        [resource] [varchar] (255) NOT NULL,
        [priority] [text] NOT NULL,
        [info] [text] NOT NULL
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [sm_sid] ON [sm] (usec, pid)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [sm_node] ON [sm] (node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [sm_username] ON [sm] (username)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[spool] (
        [username] [varchar] (250) NOT NULL,
        [xml] [text] NOT NULL,
        [seq] [bigint] IDENTITY(1,1) NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [spool_PK] PRIMARY KEY CLUSTERED 
(
        [seq] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [spool_username] ON [spool] (username)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [spool_created_at] ON [spool] (created_at)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
;

CREATE TABLE [dbo].[sr_group] (
        [name] [varchar] (250) NOT NULL,
        [opts] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [sr_group_PRIMARY] PRIMARY KEY CLUSTERED 
(
        [name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[sr_user] (
        [jid] [varchar] (250) NOT NULL,
        [grp] [varchar] (250) NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
);

CREATE UNIQUE CLUSTERED INDEX [sr_user_jid_group] ON [sr_user] ([jid], [grp])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [sr_user_jid] ON [sr_user] ([jid])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [sr_user_grp] ON [sr_user] ([grp])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[users] (
        [username] [varchar] (250) NOT NULL,
        [password] [text] NOT NULL,
        [serverkey] [text] NOT NULL DEFAULT '',
        [salt] [text] NOT NULL DEFAULT '',
        [iterationcount] [smallint] NOT NULL DEFAULT 0,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [users_PRIMARY] PRIMARY KEY CLUSTERED 
(
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[vcard] (
        [username] [varchar] (250) NOT NULL,
        [vcard] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [vcard_PRIMARY] PRIMARY KEY CLUSTERED 
(
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE TABLE [dbo].[vcard_search] (
        [username] [varchar] (250) NOT NULL,
        [lusername] [varchar] (250) NOT NULL,
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
        [lusername] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

CREATE INDEX [vcard_search_lfn] ON [vcard_search] (lfn)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_lfamily] ON [vcard_search] (lfamily)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_lgiven] ON [vcard_search] (lgiven)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_lmiddle] ON [vcard_search] (lmiddle)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_lnickname] ON [vcard_search] (lnickname)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_lbday] ON [vcard_search] (lbday)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_lctry] ON [vcard_search] (lctry)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_llocality] ON [vcard_search] (llocality)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_lemail] ON [vcard_search] (lemail)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_lorgname] ON [vcard_search] (lorgname)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [vcard_search_lorgunit] ON [vcard_search] (lorgunit)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[vcard_xupdate] (
        [username] [varchar] (250) NOT NULL,
        [hash] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE(),
 CONSTRAINT [vcard_xupdate_PRIMARY] PRIMARY KEY CLUSTERED 
(
        [username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON)
) TEXTIMAGE_ON [PRIMARY];

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

