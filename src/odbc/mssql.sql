SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

exec sp_dboption N'ejabberd', N'autoclose', N'false'
GO

exec sp_dboption N'ejabberd', N'bulkcopy', N'true'
GO

exec sp_dboption N'ejabberd', N'trunc. log', N'false'
GO

exec sp_dboption N'ejabberd', N'torn page detection', N'true'
GO

exec sp_dboption N'ejabberd', N'read only', N'false'
GO

exec sp_dboption N'ejabberd', N'dbo use', N'false'
GO

exec sp_dboption N'ejabberd', N'single', N'false'
GO

exec sp_dboption N'ejabberd', N'autoshrink', N'false'
GO

exec sp_dboption N'ejabberd', N'ANSI null default', N'false'
GO

exec sp_dboption N'ejabberd', N'recursive triggers', N'false'
GO

exec sp_dboption N'ejabberd', N'ANSI nulls', N'false'
GO

exec sp_dboption N'ejabberd', N'concat null yields null', N'false'
GO

exec sp_dboption N'ejabberd', N'cursor close on commit', N'false'
GO

exec sp_dboption N'ejabberd', N'default to local cursor', N'false'
GO

exec sp_dboption N'ejabberd', N'quoted identifier', N'false'
GO

exec sp_dboption N'ejabberd', N'ANSI warnings', N'false'
GO

exec sp_dboption N'ejabberd', N'auto create statistics', N'true'
GO

exec sp_dboption N'ejabberd', N'auto update statistics', N'true'
GO

use [ejabberd]
GO

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

CREATE TABLE [dbo].[last] (
	[username] [varchar] (250) NOT NULL ,
	[seconds] [varchar] (50) NOT NULL ,
	[state] [varchar] (100) NULL ,
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
	[nick] [varchar] (50) NULL ,
	[subscription] [char] (1) NOT NULL ,
	[ask] [char] (1) NOT NULL ,
	[askmessage] [varchar] (250) NULL ,
	[server] [char] (1) NOT NULL ,
	[subscribe] [varchar] (200) NULL ,
	[type] [varchar] (50) NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[spool] (
	[id] [numeric](19, 0) IDENTITY (1, 1) NOT NULL ,
	[username] [varchar] (250) NOT NULL ,
	[xml] [text] NULL ,
	[notifyprocessed] [bit] NULL ,
	[created] [datetime] NULL ,
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
	[full_name] [varchar] (250) NULL ,
	[first_name] [varchar] (50) NULL ,
	[last_name] [varchar] (50) NULL ,
	[nick_name] [varchar] (50) NULL ,
	[url] [varchar] (1024) NULL ,
	[address1] [varchar] (50) NULL ,
	[address2] [varchar] (50) NULL ,
	[locality] [varchar] (50) NULL ,
	[region] [varchar] (50) NULL ,
	[pcode] [varchar] (50) NULL ,
	[country] [varchar] (50) NULL ,
	[telephone] [varchar] (50) NULL ,
	[email] [varchar] (250) NULL ,
	[orgname] [varchar] (50) NULL ,
	[orgunit] [varchar] (50) NULL ,
	[title] [varchar] (50) NULL ,
	[role] [varchar] (50) NULL ,
	[b_day] [datetime] NULL ,
	[descr] [varchar] (500) NULL 
) ON [PRIMARY]
GO

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

 CREATE  INDEX [IX_rostergroups_jid] ON [dbo].[rostergroups]([jid]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IX_rostergroups_user] ON [dbo].[rostergroups]([username]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IX_rosterusers_jid] ON [dbo].[rosterusers]([username], [jid]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IX_spool_user] ON [dbo].[spool]([username]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IX_spool_process] ON [dbo].[spool]([created], [notifyprocessed]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IK_Spool_Del] ON [dbo].[spool]([MustDelete]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

 CREATE  INDEX [IK_Spool_Created] ON [dbo].[spool]([created]) WITH  FILLFACTOR = 90 ON [PRIMARY]
GO

/*********************************************************/
/** These store procedures are for use with ejabberd    **/
/** 1.1 and Microsoft Sql Server 2000                   **/
/**                                                     **/
/** The stored procedures reduce the need to sql        **/
/** compilation of the database and also allow for also **/
/** provide each of database integration. The store     **/
/** procedure have been optimized to increase database  **/
/** performance and a reduction of 80% in CPU was       **/
/** achive over the use of standard sql.                **/
/*********************************************************/

/****** Object:  StoredProcedure [dbo].[add_roster] ******/
/** Add or update user entries in the roster            **/
/*********************************************************/
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
    ( username, 
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
  @Username varchar(200)
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
/** Retrive a list of all users                                  **/
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
