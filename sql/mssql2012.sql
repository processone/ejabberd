/*
 * ejabberd, Copyright (C) 2002-2015   ProcessOne
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 */

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

use [ejabber]
GO

ALTER DATABASE CURRENT SET AUTO_CLOSE OFF

ALTER DATABASE CURRENT SET RECOVERY BULK_LOGGED

ALTER DATABASE CURRENT SET RECOVERY FULL

ALTER DATABASE CURRENT SET TORN_PAGE_DETECTION ON

ALTER DATABASE CURRENT SET READ_WRITE

ALTER DATABASE CURRENT SET MULTI_USER

ALTER DATABASE CURRENT SET MULTI_USER

ALTER DATABASE CURRENT SET AUTO_SHRINK OFF

ALTER DATABASE CURRENT SET ANSI_NULL_DEFAULT OFF

ALTER DATABASE CURRENT SET RECURSIVE_TRIGGERS OFF

ALTER DATABASE CURRENT SET ANSI_NULLS OFF

ALTER DATABASE CURRENT SET CONCAT_NULL_YIELDS_NULL OFF

ALTER DATABASE CURRENT SET CURSOR_CLOSE_ON_COMMIT OFF

ALTER DATABASE CURRENT SET CURSOR_DEFAULT GLOBAL

ALTER DATABASE CURRENT SET QUOTED_IDENTIFIER OFF

ALTER DATABASE CURRENT SET ANSI_WARNINGS OFF

ALTER DATABASE CURRENT SET AUTO_CREATE_STATISTICS ON

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[last]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[last]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[rostergroups]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[rostergroups]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[rosterusers]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[rosterusers]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[spool]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[spool]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[users]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[users]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[vcard]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[vcard]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[vcard_search]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[vcard_search]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[private_storage]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[private_storage]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[privacy_default_list]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[privacy_default_list]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[privacy_list]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[privacy_list]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[privacy_list_data]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[privacy_list_data]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[roster_version]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[roster_version]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[pubsub_node_option]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[pubsub_node_option]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[pubsub_node_owner]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[pubsub_node_owner]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[pubsub_state]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[pubsub_state]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[pubsub_item]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[pubsub_item]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[pubsub_subscription_opt]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[pubsub_subscription_opt]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[pubsub_node]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[pubsub_node]
GO

CREATE TABLE [dbo].[last] (
	[username] [varchar] (250) NOT NULL ,
	[seconds] [varchar] (50) NOT NULL ,
	[state] [varchar] (100) NOT NULL ,
	[Modify_Date] [datetime] NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[rostergroups] (
	[username] [varchar] (250) NOT NULL ,
	[jid] [varchar] (250) NOT NULL ,
	[grp] [varchar] (100) NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[rosterusers] (
	[username] [varchar] (250) NOT NULL ,
	[jid] [varchar] (250) NOT NULL ,
	[nick] [varchar] (50) NOT NULL ,
	[subscription] [char] (1) NOT NULL ,
	[ask] [char] (1) NOT NULL ,
	[askmessage] [varchar] (250) NOT NULL ,
	[server] [char] (1) NOT NULL ,
	[subscribe] [varchar] (200) NULL ,
	[type] [varchar] (50) NULL ,
CONSTRAINT [PK_rosterusers] PRIMARY KEY NONCLUSTERED
(
	[username] ASC,
	[jid] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[spool] (
	[id] [numeric](19, 0) IDENTITY (1, 1) NOT NULL ,
	[username] [varchar] (250) NOT NULL ,
	[xml] [text] NOT NULL ,
	[notifyprocessed] [bit] NULL ,
	[created] [datetime] NULL ,
	[MustDelete] [bit] NOT NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[users] (
	[username] [varchar] (250) NOT NULL ,
	[password] [varchar] (50) NOT NULL ,
	[created] [datetime] NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[vcard] (
	[username] [varchar] (250) NOT NULL ,
	[vcard] [text] NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[vcard_search] (
	[username] [varchar] (250) NOT NULL ,
	[lusername] [varchar] (250) NOT NULL ,
	[fn] [text] NOT NULL ,
	[lfn] [varchar] (250) NOT NULL ,
	[family] [text] NOT NULL ,
	[lfamily] [varchar] (250) NOT NULL ,
	[given] [text] NOT NULL ,
	[lgiven] [varchar] (250) NOT NULL ,
	[middle] [text] NOT NULL ,
	[lmiddle] [varchar] (250) NOT NULL ,
	[nickname] [text] NOT NULL ,
	[lnickname] [varchar] (250) NOT NULL ,
	[bday] [text] NOT NULL ,
	[lbday] [varchar] (250) NOT NULL ,
	[ctry] [text] NOT NULL ,
	[lctry] [varchar] (250) NOT NULL ,
	[locality] [text] NOT NULL ,
	[llocality] [varchar] (250) NOT NULL ,
	[email] [text] NOT NULL ,
	[lemail] [varchar] (250) NOT NULL ,
	[orgname] [text] NOT NULL ,
	[lorgname] [varchar] (250) NOT NULL ,
	[orgunit] [text] NOT NULL ,
	[lorgunit] [varchar] (250) NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[private_storage] (
    [username] [varchar] (250) NOT NULL ,
    [namespace] [varchar] (250) NOT NULL ,
    [data] [text] NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[privacy_default_list] (
    [username] [varchar] (250) NOT NULL,
    [name] [varchar] (250) NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[privacy_list](
	[username] [varchar](250) NOT NULL,
	[name] [varchar](250) NOT NULL,
	[id] [bigint] IDENTITY(1,1) NOT NULL,
 CONSTRAINT [PK_privacy_list] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[privacy_list_data] (
    [id] [bigint] NOT NULL,
    [t] [character] (1) NOT NULL,
    [value] [text] NOT NULL,
    [action] [character] (1) NOT NULL,
    [ord] [NUMERIC] NOT NULL,
    [match_all] [bit] NOT NULL,
    [match_iq] [bit] NOT NULL,
    [match_message] [bit] NOT NULL,
    [match_presence_in] [bit] NOT NULL,
    [match_presence_out] [bit] NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[roster_version] (
    [username] [varchar](250) PRIMARY KEY,
    [version] [text] NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[pubsub_node] (
    [host] [varchar](250),
    [node] [varchar](250),
    [parent] [varchar](250),
    [type] [varchar](250),
    [nodeid] [bigint] IDENTITY(1,1) PRIMARY KEY
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[pubsub_node_option] (
    [nodeid] [bigint],
    [name] [varchar](250),
    [val] [varchar](250)
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[pubsub_node_owner] (
    [nodeid] [bigint],
    [owner] [varchar](250)
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[pubsub_state] (
    [nodeid] [bigint],
    [jid] [varchar](250),
    [affiliation] [CHAR](1),
    [subscriptions] [text],
    [stateid] [bigint] IDENTITY(1,1) PRIMARY KEY
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[pubsub_item] (
    [nodeid] [bigint],
    [itemid] [varchar](250),
    [publisher] [text],
    [creation] [text],
    [modification] [text],
    [payload] [text]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[pubsub_subscription_opt] (
    [subid] [varchar](250),
    [opt_name] [varchar](32),
    [opt_value] [text]
) ON [PRIMARY]
GO

/* Constraints to add:
- id in privacy_list is a SERIAL autogenerated number
- id in privacy_list_data must exist in the table privacy_list */

ALTER TABLE [dbo].[last] WITH NOCHECK ADD
	CONSTRAINT [PK_last] PRIMARY KEY  CLUSTERED
	(
		[username]
	) WITH  FILLFACTOR = 90  ON [PRIMARY]
GO

ALTER TABLE [dbo].[rostergroups] WITH NOCHECK ADD
	CONSTRAINT [PK_rostergroups] PRIMARY KEY  CLUSTERED
	(
		[username],
		[jid],
		[grp]
	) WITH  FILLFACTOR = 90  ON [PRIMARY]
GO

ALTER TABLE [dbo].[spool] WITH NOCHECK ADD
	CONSTRAINT [PK_spool] PRIMARY KEY  CLUSTERED
	(
		[username],
		[id]
	) WITH  FILLFACTOR = 90  ON [PRIMARY]
GO

ALTER TABLE [dbo].[users] WITH NOCHECK ADD
	CONSTRAINT [PK_users] PRIMARY KEY  CLUSTERED
	(
		[username]
	) WITH  FILLFACTOR = 90  ON [PRIMARY]
GO

ALTER TABLE [dbo].[vcard] WITH NOCHECK ADD
	CONSTRAINT [PK_vcard] PRIMARY KEY  CLUSTERED
	(
		[username]
	) WITH  FILLFACTOR = 90  ON [PRIMARY]
GO

ALTER TABLE [dbo].[pubsub_node_option] WITH NOCHECK ADD 
	CONSTRAINT [FK_pubsub_node_option] FOREIGN KEY 
	(
		[nodeid]
	) REFERENCES [dbo].[pubsub_node]
	(
		[nodeid]
	) 
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[pubsub_node_owner] WITH NOCHECK ADD 
    CONSTRAINT [FK_pubsub_node_owner] FOREIGN KEY 
    (
		[nodeid]
	) REFERENCES [pubsub_node] 
	(
		[nodeid]
	) 
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[pubsub_state] WITH NOCHECK ADD 
    CONSTRAINT [FK_pubsub_state] FOREIGN KEY 
    (
		[nodeid]
	) REFERENCES [pubsub_node] 
	(
		[nodeid]
	) 
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[pubsub_item] WITH NOCHECK ADD 
    CONSTRAINT [FK_pubsub_item] FOREIGN KEY 
    (
		[nodeid]
	) REFERENCES [pubsub_node] 
	(
		[nodeid]
	) 
ON DELETE CASCADE
GO

CREATE  INDEX [IX_vcard_search_lfn]       ON [dbo].[vcard_search]([lfn]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_lfamily]   ON [dbo].[vcard_search]([lfamily]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_lgiven]    ON [dbo].[vcard_search]([lgiven]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_lmiddle]   ON [dbo].[vcard_search]([lmiddle]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_lnickname] ON [dbo].[vcard_search]([lnickname]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_lbday]     ON [dbo].[vcard_search]([lbday]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_lctry]     ON [dbo].[vcard_search]([lctry]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_llocality] ON [dbo].[vcard_search]([llocality]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_lemail]    ON [dbo].[vcard_search]([lemail]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_lorgname]  ON [dbo].[vcard_search]([lorgname]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO
CREATE  INDEX [IX_vcard_search_lorgunit]  ON [dbo].[vcard_search]([lorgunit]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO


CREATE  CLUSTERED  INDEX [IX_rosterusers_user] ON [dbo].[rosterusers]([username]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

ALTER TABLE [dbo].[last] WITH NOCHECK ADD
	CONSTRAINT [DF_last_updated] DEFAULT (getdate()) FOR [Modify_Date]
GO

ALTER TABLE [dbo].[spool] WITH NOCHECK ADD
	CONSTRAINT [DF_spool_notifyprocessed] DEFAULT (0) FOR [notifyprocessed],
	CONSTRAINT [DF_spool_created] DEFAULT (getdate()) FOR [created],
	CONSTRAINT [DF_spool_MustDelete] DEFAULT (0) FOR [MustDelete]
GO

ALTER TABLE [dbo].[users] WITH NOCHECK ADD
	CONSTRAINT [DF_users_created] DEFAULT (getdate()) FOR [created]
GO

ALTER TABLE [dbo].[privacy_default_list] WITH NOCHECK ADD
	CONSTRAINT [PK_privacy_defaut_list] PRIMARY KEY  CLUSTERED
	(
		[username]
	) WITH  FILLFACTOR = 90  ON [PRIMARY]
GO

 CREATE  INDEX [IX_rostergroups_jid] ON [dbo].[rostergroups]([jid]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IX_rostergroups_user] ON [dbo].[rostergroups]([username]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IX_spool_user] ON [dbo].[spool]([username]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IX_spool_process] ON [dbo].[spool]([created], [notifyprocessed]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IK_Spool_Del] ON [dbo].[spool]([MustDelete]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IK_Spool_Created] ON [dbo].[spool]([created]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IX_private_user] ON [dbo].[private_storage]([username]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IX_private_user_ns] ON [dbo].[private_storage]([username], [namespace]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_privacy_list_username] ON [dbo].[privacy_list]([username]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_privacy_list_username_name] ON [dbo].[privacy_list]([username], [name]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_pubsub_node_parent]        ON [dbo].[pubsub_node]([parent]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_pubsub_node_tuple]         ON [dbo].[pubsub_node]([host], [node])  WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_pubsub_node_option_nodeid] ON [dbo].[pubsub_node_option]([nodeid])  WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_pubsub_node_owner_nodeid]  ON [dbo].[pubsub_node_owner]([nodeid])  WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_pubsub_state_jid]          ON [dbo].[pubsub_state]([jid])  WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_pubsub_state_tuple]        ON [dbo].[pubsub_state]([nodeid], [jid])  WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_pubsub_item_itemid]        ON [dbo].[pubsub_item]([itemid])  WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_pubsub_item_tuple]         ON [dbo].[pubsub_item]([nodeid], [itemid])  WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE INDEX [IX_pubsub_subscription_opt]   ON [dbo].[pubsub_subscription_opt]([subid], [opt_name])  WITH  FILLFACTOR = 90 ON [PRIMARY]
Go

/*********************************************************/
/** These store procedures are for use with ejabberd    **/
/** 1.1 and Microsoft Sql Server 2000                   **/
/**                                                     **/
/** The stored procedures reduce the need to sql        **/
/** compilation of the database and also allow for also **/
/** provide each of database integration. The stored    **/
/** procedure have been optimized to increase database  **/
/** performance and a reduction of 80% in CPU was       **/
/** achieved over the use of standard sql.              **/
/*********************************************************/

/****** Object:  StoredProcedure [dbo].[add_roster] ******/
/** Add or update user entries in the roster            **/
/*********************************************************/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[add_roster]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[add_roster]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[add_roster_group]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[add_roster_group]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[add_roster_user]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[add_roster_user]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_roster_groups]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_roster_groups]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[add_spool]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[add_spool]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[add_user]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[add_user]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[set_password]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[set_password]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[clean_spool_msg]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[clean_spool_msg]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_password]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_password]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_last]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_last]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_roster]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_roster]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_spool_msg]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_spool_msg]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_user]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_user]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_user_return_password]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_user_return_password]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_user_roster]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_user_roster]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_and_del_spool_msg]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_and_del_spool_msg]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_last]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_last]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_roster]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_roster]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_roster_by_jid]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_roster_by_jid]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_roster_jid_groups]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_roster_jid_groups]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_roster_groups]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_roster_groups]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_rostergroup_by_jid]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_rostergroup_by_jid]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_subscription]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_subscription]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[list_users]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[list_users]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[set_last]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[set_last]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[set_private_data]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[set_private_data]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_private_data]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_private_data]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_user_storage]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_user_storage]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[set_vcard]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[set_vcard]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_vcard]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_vcard]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_default_privacy_list]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_default_privacy_list]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_privacy_list_names]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_privacy_list_names]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_privacy_list_id]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_privacy_list_id]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_privacy_list_data]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_privacy_list_data]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[get_privacy_list_data_by_id]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[get_privacy_list_data_by_id]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[set_default_privacy_list]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[set_default_privacy_list]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[unset_default_privacy_list]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[unset_default_privacy_list]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[remove_privacy_list]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[remove_privacy_list]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[add_privacy_list]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[add_privacy_list]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[set_privacy_list]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[set_privacy_list]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_privacy_list_by_id]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_privacy_list_by_id]
GO
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[del_privacy_lists]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[del_privacy_lists]
GO

CREATE PROCEDURE [dbo].[add_roster]
  @Username       varchar(250),
  @JID            varchar(250),
  @Nick           varchar(50),
  @Subscription   char(1),
  @Ask            char(1),
  @AskMessage     varchar(250),
  @Server         char(1),
  @Subscribe      varchar(200),
  @Type           varchar(50),
  @Grp            varchar(100)
AS
BEGIN
  BEGIN TRANSACTION
    --- Update Roster if user exist else add roster item
    IF EXISTS (SELECT username FROM rosterusers WITH (NOLOCK) WHERE rosterusers.username=@Username AND rosterusers.jid=@JID)
      BEGIN
        UPDATE rosterusers
          SET rosterusers.username=@Username,
              rosterusers.jid=@JID,
              rosterusers.nick=@Nick,
              rosterusers.subscription=@Subscription,
              rosterusers.ask=@Ask,
              rosterusers.askmessage=@AskMessage,
              rosterusers.server=@Server,
              rosterusers.subscribe=@Subscribe,
              rosterusers.type=@Type
        WHERE (rosterusers.username=@Username) AND (rosterusers.jid=@JID);
      END
    ELSE
      BEGIN
        INSERT INTO rosterusers
          ( rosterusers.username,
            rosterusers.jid,
            rosterusers.nick,
            rosterusers.subscription,
            rosterusers.ask,
            rosterusers.askmessage,
            rosterusers.server,
            rosterusers.subscribe,
            rosterusers.type
          )
        VALUES
          ( @Username,
            @JID,
            @Nick,
            @Subscription,
            @Ask,
            @AskMessage,
            @Server,
            @Subscribe,
            @Type
          );
      END

   --- Update Roster Groups if exist else add group entry
   IF NOT EXISTS (SELECT username FROM rostergroups WITH (NOLOCK) WHERE rostergroups.username=@Username AND rostergroups.jid=@JID AND rostergroups.grp=@Grp)
     BEGIN
       INSERT INTO rostergroups
         ( rostergroups.username,
           rostergroups.jid,
           rostergroups.grp
         )
       VALUES
         ( @Username,
           @JID,
           @Grp
         );
     END

  COMMIT
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[add_roster_group] ******/
/** Add or update user group entries in the roster groups     **/
/***************************************************************/
CREATE PROCEDURE [dbo].[add_roster_group]
  @Username  varchar(250),
  @JID       varchar(250),
  @Grp       varchar(100)
AS
BEGIN
  --- Update Roster Groups if exist else add group
  IF NOT EXISTS (SELECT username FROM rostergroups WHERE rostergroups.username=@Username AND rostergroups.jid=@JID AND rostergroups.grp=@Grp)
    BEGIN
      INSERT INTO rostergroups
        ( rostergroups.username,
          rostergroups.jid,
          rostergroups.grp
        )
      VALUES
        ( @Username,
          @JID,
          @Grp
        )
    END
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[add_roster_user]  ******/
/** Add or update user entries in the roster                  **/
/***************************************************************/
CREATE PROCEDURE [dbo].[add_roster_user]
  @Username      varchar(250),
  @JID           varchar(250),
  @Nick          varchar(50),
  @Subscription  char(1),
  @Ask           char(1),
  @AskMessage    varchar(250),
  @Server        char(1),
  @Subscribe     varchar(200),
  @Type          varchar(50),
  @Grp           varchar(100) = Null
AS
BEGIN
  BEGIN TRANSACTION
    --- Update Roster Users if exist of add new user
    IF EXISTS (SELECT username FROM rosterusers WHERE rosterusers.username=@Username AND rosterusers.jid=@JID)
      BEGIN
        UPDATE rosterusers
          SET rosterusers.username=@Username,
              rosterusers.jid=@JID,
              rosterusers.nick=@Nick,
              rosterusers.subscription=@Subscription,
              rosterusers.ask=@Ask,
              rosterusers.askmessage=@AskMessage,
              rosterusers.server=@Server,
              rosterusers.subscribe=@Subscribe,
              rosterusers.type=@Type
        WHERE (rosterusers.username=@Username) AND (rosterusers.jid=@JID);
      END
    ELSE
      BEGIN
        INSERT INTO rosterusers
          ( rosterusers.username,
            rosterusers.jid,
            rosterusers.nick,
            rosterusers.subscription,
            rosterusers.ask,
            rosterusers.askmessage,
            rosterusers.server,
            rosterusers.subscribe,
            rosterusers.type
          )
        VALUES
          ( @Username,
            @JID,
            @Nick,
            @Subscription,
            @Ask,
            @AskMessage,
            @Server,
            @Subscribe,
            @Type
          );
      END

    --- Update Roster Group if exist of add new group
    IF @Grp IS NOT NULL
      EXECUTE [dbo].[add_roster_group] @Username, @JID, @Grp

  COMMIT
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[del_roster_groups] ******/
/** Remove user group entries from the roster groups table    **/
/***************************************************************/
CREATE PROCEDURE [dbo].[del_roster_groups]
  @Username  varchar(250),
  @JID       varchar(250)
AS
BEGIN
      DELETE FROM rostergroups
      WITH (ROWLOCK)
      WHERE (rostergroups.username = @Username) AND (rostergroups.jid = @JID);
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[add_spool]        ******/
/** Add a entry to the spool table                            **/
/***************************************************************/
CREATE PROCEDURE [dbo].[add_spool]
  @Username varchar(250),
  @XML varchar(8000)
AS
BEGIN
  INSERT INTO spool
    ( spool.username,
      spool.xml
    )
  VALUES
    ( @Username,
      @XML
    )
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[add_user]         ******/
/** Add or update user entries to jabber                      **/
/***************************************************************/
CREATE PROCEDURE [dbo].[add_user]
  @Username varchar(200),
  @Password varchar(50)
AS
BEGIN
  INSERT INTO users
    ( [username],
      [password]
    )
  VALUES
    ( @Username,
      @Password
    );
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[set_password]            **/
/** Update users password                                        **/
/******************************************************************/
CREATE PROCEDURE [dbo].[set_password]
  @Username varchar(200),
  @Password varchar(50)
AS
BEGIN
  IF EXISTS (SELECT username FROM users WITH (NOLOCK) WHERE username=@Username)
    BEGIN
      UPDATE users SET username=@Username, password=@Password WHERE username=@Username;
    END
  ELSE
    BEGIN
      INSERT INTO users (username, password) VALUES (@Username, @Password);
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_password]            **/
/** Retrive the user password                                    **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_password]
  @Username varchar(200)
AS
BEGIN
  SELECT users.password as password
  FROM users WITH (NOLOCK)
  WHERE username=@Username;
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[set_roster_version]      **/
/** Update users roster_version                                  **/
/******************************************************************/
CREATE PROCEDURE [dbo].[set_roster_version]
  @Username varchar(200),
  @Version varchar(8000)
AS
BEGIN
  IF EXISTS (SELECT username FROM roster_version WITH (NOLOCK) WHERE username=@Username)
    BEGIN
      UPDATE roster_version SET username=@Username, version=@Version WHERE username=@Username;
    END
  ELSE
    BEGIN
      INSERT INTO roster_version (username, version) VALUES (@Username, @Version);
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_roster_version]      **/
/** Retrive the user roster_version                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_roster_version]
  @Username varchar(200)
AS
BEGIN
  SELECT roster_version.version as version
  FROM roster_version WITH (NOLOCK)
  WHERE username=@Username;
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[clean_spool_msg]  ******/
/** Delete messages older that 3 days from spool              **/
/***************************************************************/
CREATE   PROCEDURE [dbo].[clean_spool_msg]
AS
DECLARE
  @dt         datetime,
  @myRowCount int
BEGIN
  -- Delete small amounts because if locks the database table
  SET ROWCOUNT 500
  SET @myRowCount = 1

  WHILE (@myRowCount) > 0
    BEGIN
      BEGIN TRANSACTION
        SELECT @dt = DATEADD(d, -3, GETDATE())
        DELETE FROM spool
        WITH (ROWLOCK)
        WHERE (MustDelete=1) OR (Created < @dt);

        SET @myRowCount = @@RowCount
      COMMIT
    END
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[del_last]         ******/
/** Delete an entry from the last table                       **/
/***************************************************************/
CREATE PROCEDURE [dbo].[del_last]
  @Username  varchar(250)
AS
BEGIN
  DELETE FROM [last]
  WITH (ROWLOCK)
  WHERE [last].username=@Username;
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[del_roster]       ******/
/** Delete an entry from the roster                           **/
/***************************************************************/
CREATE PROCEDURE [dbo].[del_roster]
  @Username varchar(250),
  @JID      varchar(250)
AS
BEGIN
  BEGIN TRANSACTION
    DELETE FROM rosterusers
    WITH (ROWLOCK)
    WHERE (rosterusers.username = @Username) AND (rosterusers.jid = @JID);

    DELETE FROM rostergroups
    WITH (ROWLOCK)
    WHERE (rostergroups.username = @Username) AND (rostergroups.jid = @JID);
  COMMIT
END
GO


/***************************************************************/
/****** Object:  StoredProcedure [dbo].[del_spool_msg]    ******/
/** Delete an entry from the spool table                      **/
/***************************************************************/
CREATE PROCEDURE [dbo].[del_spool_msg]
  @Username varchar(250)
AS
BEGIN
  DELETE FROM spool
  WITH (ROWLOCK)
  WHERE spool.username=@Username;
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[del_user]         ******/
/** Delete an entry from the user table                       **/
/***************************************************************/
CREATE PROCEDURE [dbo].[del_user]
  @Username varchar(200)
AS
BEGIN
  DELETE FROM users
  WITH (ROWLOCK)
  WHERE username=@Username;
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[del_user_return_password]**/
/** Delete an entry from the user table and return user password **/
/******************************************************************/
CREATE PROCEDURE [dbo].[del_user_return_password]
 @Username varchar(250)
AS
DECLARE
 @Pwd varchar(50)
BEGIN
 EXECUTE @Pwd = dbo.get_password @Username
 DELETE FROM users
 WITH (ROWLOCK)
 WHERE username=@Username

 SELECT @Pwd;
END
GO


/******************************************************************/
/****** Object:  StoredProcedure [dbo].[del_user_roster]         **/
/** Delete the users roster                                      **/
/******************************************************************/
CREATE PROCEDURE [dbo].[del_user_roster]
 @Username varchar(250)
AS
BEGIN
  BEGIN TRANSACTION
    DELETE FROM rosterusers
    WITH (ROWLOCK)
    WHERE rosterusers.username = @Username;

    DELETE FROM rostergroups
    WITH (ROWLOCK)
    WHERE rostergroups.username = @Username;
  COMMIT
END
GO


/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_and_del_spool_msg]   **/
/** Fetch and delete the users offline messages                  **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_and_del_spool_msg]
  @Username varchar(250)
AS
DECLARE
  @vSpool table( username varchar(1),
                 xml      varchar(1))
BEGIN
  IF EXISTS (SELECT username FROM spool with (nolock) WHERE spool.username=@Username)
    BEGIN
      SELECT spool.username AS username,
             spool.xml AS xml
      FROM spool WITH (NOLOCK)
      WHERE spool.username=@Username;

      DELETE spool
      WITH (ROWLOCK)
      WHERE spool.username=@Username
    END
  ELSE
    BEGIN
      SELECT * FROM @vSpool;
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_last]                **/
/** Retrive the last user login                                  **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_last]
  @Username varchar(250)
AS
BEGIN
  SELECT last.seconds AS seconds,
         last.state AS state
  FROM last WITH (NOLOCK)
  WHERE last.username=@Username;
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_roster]              **/
/** Retrive the user roster                                      **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_roster]
  @Username varchar(250)
AS
DECLARE
  @vRosterusers table( username      varchar(1),
                       jid           varchar(1),
                       nick          varchar(1),
                       subscription  varchar(1),
                       ask           varchar(1),
                       askmessage    varchar(1),
                       server        varchar(1),
                       subscribe     varchar(1),
                       type          varchar(1))
BEGIN
  IF EXISTS (SELECT username FROM rosterusers with (nolock) WHERE rosterusers.username = @Username)
    BEGIN
      SELECT  rosterusers.username AS username,
              rosterusers.jid AS jid,
              rosterusers.nick AS nick,
              rosterusers.subscription AS subscription,
              rosterusers.ask AS ask,
              rosterusers.askmessage AS askmessage,
              rosterusers.server AS server,
              rosterusers.subscribe AS subscribe,
              rosterusers.type AS type
      FROM rosterusers WITH (NOLOCK)
      WHERE rosterusers.username = @Username;
    END
  ELSE
    BEGIN
      SELECT * FROM @vRosterusers
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_roster_by_jid]       **/
/** Retrive the user roster via JID                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_roster_by_jid]
  @Username varchar(200),
  @JID      varchar(250)
AS
DECLARE
  @vRosterusers table( username      varchar(1),
                       jid           varchar(1),
                       nick          varchar(1),
                       subscription  varchar(1),
                       ask           varchar(1),
                       askmessage    varchar(1),
                       server        varchar(1),
                       subscribe     varchar(1),
                       type          varchar(1))
BEGIN
  IF EXISTS (SELECT username FROM rosterusers with (nolock) WHERE (rosterusers.username = @Username) AND (rosterusers.jid = @JID))
    BEGIN
      SELECT rosterusers.username AS username,
             rosterusers.jid AS jid,
             rosterusers.nick AS nick,
             rosterusers.subscription AS subscription,
             rosterusers.ask AS ask,
             rosterusers.askmessage AS askmessage,
             rosterusers.server AS server,
             rosterusers.subscribe AS subscribe,
             rosterusers.type AS type
      FROM rosterusers WITH (NOLOCK)
      WHERE (rosterusers.username = @Username) AND (rosterusers.jid = @JID);
    END
  ELSE
    BEGIN
      SELECT * FROM @vRosterusers
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_roster_jid_groups]   **/
/** Retrieve the user roster groups                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_roster_jid_groups]
  @Username varchar(200)
AS
DECLARE
  @vrostergroups table( jid  varchar(1),
                        grp  varchar(1))
BEGIN
  IF EXISTS (SELECT username FROM rostergroups with (nolock) WHERE rostergroups.username = @Username)
    BEGIN
      SELECT rostergroups.jid AS jid,
             rostergroups.grp AS grp
      FROM rostergroups WITH (NOLOCK)
      WHERE rostergroups.username = @Username;
    END
  ELSE
    BEGIN
      SELECT * FROM @vrostergroups
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_roster_groups]       **/
/** Retrive the user roster groups                               **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_roster_groups]
  @Username varchar(200),
  @JID      varchar(250)
AS
DECLARE
  @vrostergroups table( grp  varchar(1))
BEGIN
  IF EXISTS (SELECT username FROM rostergroups with (nolock) WHERE rostergroups.username = @Username)
    BEGIN
      SELECT rostergroups.grp AS grp
      FROM rostergroups WITH (NOLOCK)
      WHERE (rostergroups.username = @Username)  AND (rostergroups.jid = @JID);
    END
  ELSE
    BEGIN
      SELECT * FROM @vrostergroups
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_rostergroup_by_jid]  **/
/** Retrive the user roster groups via JID                       **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_rostergroup_by_jid]
  @Username varchar(250),
  @JID      varchar(250)
AS
DECLARE
  @vrostergroups table(grp varchar(1))
BEGIN
  IF EXISTS (SELECT username FROM rostergroups with (nolock) WHERE rostergroups.username=@Username AND rostergroups.jid=@JID)
    BEGIN
      SELECT rostergroups.grp AS grp
      FROM rostergroups WITH (NOLOCK)
      WHERE rostergroups.username=@Username AND rostergroups.jid=@JID;
    END
  ELSE
    BEGIN
      SELECT * FROM @vrostergroups
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_subscription]        **/
/** Retrive the user subscription requests                       **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_subscription]
  @Username varchar(250),
  @JID      varchar(250)
AS
DECLARE
  @vrosterusers table( subscription varchar(1))
BEGIN
  IF EXISTS (SELECT username FROM rosterusers with (nolock) WHERE rosterusers.username=@Username AND rosterusers.jid=@JID)
    BEGIN
      SELECT rosterusers.subscription AS subscription
      FROM rosterusers WITH (NOLOCK)
      WHERE rosterusers.username=@Username AND rosterusers.jid=@JID;
    END
  ELSE
    BEGIN
      SELECT * FROM @vrosterusers
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[list_users]              **/
/** Retrieve a list of all users                                 **/
/******************************************************************/
CREATE PROCEDURE [dbo].[list_users]
AS
BEGIN
  SELECT users.username AS username FROM users WITH (NOLOCK);
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[set_last]                **/
/** Update users last login status                               **/
/******************************************************************/
CREATE PROCEDURE [dbo].[set_last]
  @Username  varchar(250),
  @Seconds   varchar(50),
  @State     varchar(100)
AS
BEGIN
  IF EXISTS (SELECT username FROM [last] WITH (NOLOCK) WHERE username=@Username)
    BEGIN
      UPDATE [last]
      SET [last].username = @Username,
          [last].seconds = @Seconds,
          [last].state = @State
      WHERE last.username=@Username;
    END
  ELSE
    BEGIN
      INSERT INTO [last]
        (  [last].username,
           [last].seconds,
           [last].state
        )
      VALUES
        (  @Username,
           @Seconds,
           @State
        )
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[set_private_data]        **/
/** store user private data by namespace                         **/
/******************************************************************/
CREATE PROCEDURE [dbo].[set_private_data]
    @Username  varchar(250),
    @Namespace varchar(250),
    @Data varchar(8000)
AS
BEGIN
  IF EXISTS (SELECT username FROM private_storage with (nolock) WHERE private_storage.username = @Username AND private_storage.namespace = @Namespace)
    BEGIN
        UPDATE [private_storage]
        SET [private_storage].username = @Username,
            [private_storage].namespace = @Namespace,
            [private_storage].data = @Data
            WHERE private_storage.username = @Username AND private_storage.namespace = @Namespace;
    END
  ELSE
    BEGIN
        INSERT INTO [private_storage]
            ( [private_storage].username,
              [private_storage].namespace,
              [private_storage].data
            )
        VALUES
            ( @Username,
              @Namespace,
              @Data
            )
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_private_data]        **/
/** Retrieve user private data by namespace                      **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_private_data]
    @Username  varchar(250),
    @Namespace varchar(250)
AS
BEGIN
  SELECT private_storage.data AS data
  FROM private_storage WITH (NOLOCK)
  WHERE username=@Username and namespace=@Namespace;
END
GO

/***************************************************************/
/****** Object:  StoredProcedure [dbo].[del_user_storage] ******/
/** Delete private storage area for a given user              **/
/***************************************************************/
CREATE PROCEDURE [dbo].[del_user_storage]
  @Username  varchar(250)
AS
BEGIN
  DELETE FROM [private_storage]
  WITH (ROWLOCK)
  WHERE [private_storage].username=@Username;
END
GO


/******************************************************************/
/****** Object:  StoredProcedure [dbo].[set_vcard]               **/
/** Set the user's vCard                                         **/
/******************************************************************/
CREATE PROCEDURE [dbo].[set_vcard]
            @VCard varchar(8000),
            @Username  varchar(250),
            @Lusername varchar(250),
            @Fn varchar(8000),
            @Lfn varchar(250),
            @Family varchar(8000),
            @Lfamily varchar(250),
            @Given varchar(8000),
            @Lgiven varchar(250),
            @Middle varchar(8000),
            @Lmiddle varchar(250),
            @Nickname varchar(8000),
            @Lnickname varchar(250),
            @Bday varchar(8000),
            @Lbday varchar(250),
            @Ctry varchar(8000),
            @Lctry varchar(250),
            @Locality varchar(8000),
            @Llocality varchar(250),
            @Email varchar(8000),
            @Lemail varchar(250),
            @Orgname varchar(8000),
            @Lorgname varchar(250),
            @Orgunit varchar(8000),
            @Lorgunit varchar(250)
AS
BEGIN
  IF EXISTS (SELECT username FROM vcard with (nolock) WHERE vcard.username = @Username)
    BEGIN
        UPDATE [vcard]
        SET [vcard].username = @LUsername,
            [vcard].vcard = @Vcard
            WHERE vcard.username = @LUsername;

        UPDATE [vcard_search]
        SET [vcard_search].username = @Username,
            [vcard_search].lusername = @Lusername,
            [vcard_search].fn = @Fn,
            [vcard_search].lfn = @Lfn,
            [vcard_search].family = @Family,
            [vcard_search].lfamily = @Lfamily,
            [vcard_search].given = @Given,
            [vcard_search].lgiven = @Lgiven,
            [vcard_search].middle = @Middle,
            [vcard_search].lmiddle = @Lmiddle,
            [vcard_search].nickname = @Nickname,
            [vcard_search].lnickname = @Lnickname,
            [vcard_search].bday = @Bday,
            [vcard_search].lbday = @Lbday,
            [vcard_search].ctry = @Ctry,
            [vcard_search].lctry = @Lctry,
            [vcard_search].locality = @Locality,
            [vcard_search].llocality = @Llocality,
            [vcard_search].email = @Email,
            [vcard_search].lemail = @Lemail,
            [vcard_search].orgname = @Orgname,
            [vcard_search].lorgname = @Lorgname,
            [vcard_search].orgunit = @Orgunit,
            [vcard_search].lorgunit = @Lorgunit
            WHERE vcard_search.lusername = @LUsername;
    END
  ELSE
    BEGIN
        INSERT INTO [vcard]
            ( [vcard].username,
              [vcard].vcard
            )
        VALUES
            ( @lUsername,
              @Vcard
            );

        INSERT INTO [vcard_search]
	(
            [vcard_search].username ,
            [vcard_search].lusername ,
            [vcard_search].fn ,
            [vcard_search].lfn ,
            [vcard_search].family ,
            [vcard_search].lfamily ,
            [vcard_search].given ,
            [vcard_search].lgiven ,
            [vcard_search].middle ,
            [vcard_search].lmiddle ,
            [vcard_search].nickname,
            [vcard_search].lnickname,
            [vcard_search].bday,
            [vcard_search].lbday,
            [vcard_search].ctry,
            [vcard_search].lctry,
            [vcard_search].locality,
            [vcard_search].llocality,
            [vcard_search].email,
            [vcard_search].lemail,
            [vcard_search].orgname,
            [vcard_search].lorgname,
            [vcard_search].orgunit,
            [vcard_search].lorgunit
        )
	VALUES
	(
            @Username,
            @Lusername,
            @Fn,
            @Lfn,
            @Family,
            @Lfamily,
            @Given,
            @Lgiven,
            @Middle,
            @Lmiddle,
            @Nickname,
            @Lnickname,
            @Bday,
            @Lbday,
            @Ctry,
            @Lctry,
            @Locality,
            @Llocality,
            @Email,
            @Lemail,
            @Orgname,
            @Lorgname,
            @Orgunit,
            @Lorgunit
       	)
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_vcard]               **/
/** Retrive the user's vCard                                     **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_vcard]
  @Username varchar(250)
AS
BEGIN
  SELECT vcard.vcard as vcard
  FROM vcard WITH (NOLOCK)
  WHERE username=@Username;
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_default_privacy_list]**/
/** Retrive the user's default privacy list                      **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_default_privacy_list]
  @Username varchar(250)
AS
BEGIN
  SELECT list.name 
  FROM privacy_default_list list WITH (NOLOCK)
  WHERE list.username=@Username
END
GO
  
/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_privacy_list_names]  **/
/** Retrive the user's default privacy list names                **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_privacy_list_names]
  @username varchar(250)
AS
BEGIN
  SELECT list.name 
  FROM privacy_list list WITH (NOLOCK)
  WHERE list.username=@Username
END
GO
      
/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_privacy_list_id]     **/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_privacy_list_id]
  @username varchar(250),
  @SName varchar(250)
AS
BEGIN
  SELECT id FROM privacy_list
  WHERE username=@Username
    AND name=@SName
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_privacy_list_data]   **/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_privacy_list_data]
  @username varchar(250),
  @SName varchar(250)
AS
BEGIN
  SELECT l_data.t, 
		 l_data.value, 
		 l_data.action, 
		 l_data.ord, 
		 l_data.match_all, 
		 l_data.match_iq,
         l_data.match_message, 
         l_data.match_presence_in, 
         l_data.match_presence_out
  FROM privacy_list_data l_data (NOLOCK)
  WHERE l_data.id = (SELECT list.id 
			         FROM privacy_list list
			         WHERE list.username=@username
			           AND list.name=@SName)
  ORDER BY l_data.ord
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[get_privacy_list_data_by_id]**/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[get_privacy_list_data_by_id]
  @Id bigint
AS
BEGIN
  SELECT l_data.t, 
		 l_data.value, 
		 l_data.action, 
		 l_data.ord, 
		 l_data.match_all, 
		 l_data.match_iq,
         l_data.match_message, 
         l_data.match_presence_in, 
         l_data.match_presence_out
  FROM privacy_list_data l_data (NOLOCK)
  WHERE l_data.id=@ID
  ORDER BY l_data.ord
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[set_default_privacy_list]**/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[set_default_privacy_list]
  @username varchar(250),
  @Sname varchar(250)
AS
BEGIN
  IF EXISTS (SELECT username FROM privacy_default_list with (nolock) WHERE privacy_default_list.username = @Username AND privacy_default_list.name = @Sname)
    BEGIN
        UPDATE [privacy_default_list]
        SET [privacy_default_list].username = @Username,
            [privacy_default_list].name = @Sname
            WHERE privacy_default_list.username = @Username
    END
  ELSE
    BEGIN
        INSERT INTO [privacy_default_list]
            ( [privacy_default_list].username,
              [privacy_default_list].name
            )
        VALUES
            ( @Username,
              @SName
            )
    END
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[unset_default_privacy_list]**/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[unset_default_privacy_list]
  @username varchar(250)
AS
BEGIN
  DELETE 
  FROM privacy_default_list
  WHERE privacy_default_list.username=@username
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[remove_privacy_list]     **/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[remove_privacy_list]
  @username varchar(250),
  @SName varchar(250)
AS
BEGIN
  DELETE 
  FROM privacy_list
  WHERE privacy_list.username=@username
  AND privacy_list.name=@SName
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[add_privacy_list]        **/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[add_privacy_list]
  @username varchar(250),
  @SName varchar(250)
AS
BEGIN
  INSERT INTO privacy_list(username, name)
  VALUES (@username, @SName)
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[set_privacy_list]        **/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[set_privacy_list]
  @Id bigint,
  @t char(1), 
  @value text, 
  @action char(1), 
  @ord numeric, 
  @match_all bit, 
  @match_iq bit, 
  @match_message bit, 
  @match_presence_in bit, 
  @match_presence_out bit
AS
BEGIN
	insert into privacy_list_data (
		id, 
		t, 
		value, 
		action, 
		ord, 
		match_all, 
		match_iq, 
		match_message, 
		match_presence_in, 
		match_presence_out
	)
	values (@Id,
            @t, 
            @value, 
            @action, 
            @ord, 
            @match_all, 
            @match_iq, 
            @match_message, 
            @match_presence_in, 
            @match_presence_out
           )

END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[del_privacy_list_by_id]  **/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[del_privacy_list_by_id]
  @Id bigint
AS
BEGIN
  DELETE FROM privacy_list_data 
  WHERE privacy_list_data.id=@Id
END
GO

/******************************************************************/
/****** Object:  StoredProcedure [dbo].[del_privacy_lists]       **/
/**                                                              **/
/******************************************************************/
CREATE PROCEDURE [dbo].[del_privacy_lists]
  @Server varchar(250),
  @username varchar(250)
AS
BEGIN
  DELETE FROM privacy_list WHERE username=@username
  DELETE FROM privacy_list_data WHERE convert(varchar,value)=@username+'@'+@Server
  DELETE FROM privacy_default_list WHERE username=@username
END
GO
