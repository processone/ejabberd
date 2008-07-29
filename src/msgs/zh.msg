% $Id$
% Language: Chinese (中文)
% Author: Shelley Shyan - skylarkbj AT 163 DOT com
% Author: Mike Wang
% Author: Zhan Caibao - zhancaibao AT gmail DOT com

% jlib.hrl
{"No resource provided", "无资源提供"}.

% mod_offline_odbc.erl
{"Your contact offline message queue is full. The message has been discarded.", "您联系人的离线消息队列已满. 消息已被丢弃"}.

% mod_configure.erl
{"Choose storage type of tables", "请选择表格的存储类型"}.
{"RAM copy", "内存(RAM)复制"}.
{"RAM and disc copy", "内存与磁盘复制"}.
{"Disc only copy", "仅复制磁盘"}.
{"Remote copy", "远程复制"}.
{"Stop Modules at ", "要停止的模块位于 "}.
{"Choose modules to stop", "请选择要停止的模块"}.
{"Start Modules at ", "要启动的模块位于 "}.
{"Enter list of {Module, [Options]}", "请输入{模块, [选项]}列表"}.
{"List of modules to start", "要启动的模块列表"}.
{"Backup to File at ", "将文件备份到"}.
{"Enter path to backup file", "请输入备份文件的路径"}.
{"Path to File", "文件路径"}.
{"Restore Backup from File at ", "要恢复的备份文件位于"}.
{"Dump Backup to Text File at ", "转储备份到文本文件于"}.
{"Enter path to text file", "请输入文本文件的路径"}.
{"Import User from File at ", "导入用户的文件位于 "}.
{"Enter path to jabberd1.4 spool file", "请输入jabberd1.4 spool文件的路径"}.
{"Import Users from Dir at ", "导入用户的目录位于 "}.
{"Enter path to jabberd1.4 spool dir", "请输入jabberd1.4 spool目录的路径"}.
{"Path to Dir", "目录的路径"}.
{"Access Control List Configuration", "访问控制列表(ACL)配置"}.
{"Access control lists", "访问控制列表(ACL)"}.
{"Access Configuration", "访问配置"}.
{"Access rules", "访问规则"}.
{"Administration of ", "管理"}.
{"Action on user", "对用户的动作"}.
{"Edit Properties", "编辑属性"}.
{"Remove User", "删除用户"}.
{"Database", "数据库"}.
{"Outgoing s2s Connections", "出站s2s连接"}.
{"Import Users From jabberd 1.4 Spool Files", "从Jabberd 1.4 Spool文件导入用户"}.
{"Database Tables Configuration at ", "数据库表格配置位于"}.
{"Restart Service", "重启服务"}.
{"Shut Down Service", "关闭服务"}.
{"Delete User", "删除用户"}.
{"End User Session", "结束用户会话"}.
{"Get User Password", "获取用户密码"}.
{"Change User Password", "更改用户密码"}.
{"Get User Last Login Time", "获取用户上次登陆时间"}.
{"Get User Statistics", "获取用户统计"}.
{"Get Number of Registered Users", "获取注册用户数"}.
{"Get Number of Online Users", "获取在线用户数"}.
{"User Management", "用户管理"}.
{"Time delay", "时间延迟"}.
{"Password Verification", "确认密码"}.
{"Number of registered users", "注册用户数"}.
{"Number of online users", "在线用户数"}.
{"Last login", "上次登陆"}.
{"Roster size", "花名册大小"}.
{"IP addresses", "IP地址"}.
{"Resources", "资源"}.

% src/ejabberd_c2s.erl
{"Use of STARTTLS required", "要求使用STARTTLS"}.
{"Replaced by new connection", "被新的连接替换"}.

% mod_disco.erl
{"Configuration", "配 置"}.
{"Online Users", "在线用户"}.
{"All Users", "所有用户"}.
{"To ~s", "发送给~s"}.
{"From ~s", "来自~s"}.
{"Running Nodes", "正在运行的节点"}.
{"Stopped Nodes", "已经停止的节点"}.
{"Access Control Lists", "访问控制列表(ACL)"}.
{"Access Rules", "访问规则"}.
{"Modules", "模块"}.
{"Start Modules", "启动模块"}.
{"Stop Modules", "停止模块"}.
{"Backup Management", "备份管理"}.
{"Backup", "备份"}.
{"Restore", "恢复"}.
{"Dump to Text File", "转储到文本文件"}.
{"Import File", "导入文件"}.
{"Import Directory", "导入目录"}.

% mod_register.erl
{"Users are not allowed to register accounts so fast", "不允许用户注册帐户太快"}.
{"Choose a username and password to register with this server", "请选择在此服务器上注册所需的用户名和密码"}.

% src/mod_vcard_ldap.erl
{"Erlang Jabber Server", "Erlang Jabber 服务器"}.
{"ejabberd vCard module", "ejabberd vCard 模块"}.
{"Email", "电子邮件"}.
{"Search Results for ", "搜索结果属于关键词 "}.
{"Jabber ID", "Jabber ID"}.

% mod_vcard.erl
{"You need an x:data capable client to search", "您需要一个兼容x:data的客户端来搜索"}.
{"Search users in ", "要搜索的用户位于 "}.

{"Fill in fields to search for any matching Jabber User", "填充字段以搜索任何匹配的Jabber用户"}.
{"Fill in the form to search for any matching Jabber User (Add * to the end of field to match substring)", "填充表单以搜索任何匹配的Jabber用户(在字段末添加*来匹配子串)"}.

{"User",         "用户"}.
{"Full Name", 	  "全名"}.
{"Name", 	     "名字"}.
{"Middle Name",  "中间名"}.
{"Family Name",  "姓氏"}.
{"Nickname", 	  "昵称"}.
{"Birthday", 	  "出生日期"}.
{"Country", 	  "国家"}.
{"City", 	     "城市"}.
{"Organization Name", "组织名称"}.
{"Organization Unit", "组织单位"}.

% mod_adhoc.erl
{"Commands", "命令"}.
{"Ping", "Ping"}.
{"Pong", "Pong"}.

% mod_announce.erl
{"Really delete message of the day?", "确实要删除当日的消息吗?"}.
{"Subject", "标题"}.
{"Message body", "消息主体"}.
{"No body provided for announce message", "没有人提供通知消息"}.
{"Announcements", "通知"}.
{"Send announcement to all users", "将通知发送给所有用户"}.
{"Send announcement to all online users", "将通知发送给所有在线用户"}.
{"Send announcement to all online users on all hosts", "将通知发送给所有主机的所有用户"}.
{"Set message of the day and send to online users", "设定当日消息并发送给所有在线用户"}.
{"Update message of the day (don't send)", "更新当日的消息(不发送)"}.
{"Delete message of the day", "删除当日的消息"}.
{"Send announcement to all users on all hosts", "给所有主机上的所有用户发送通知"}.
{"Set message of the day on all hosts and send to online users", "设置当日所有主机上 的消息并发送给在线用户"}.
{"Update message of the day on all hosts (don't send)", "更新当日所有主机上 的消息(不发送)"}.
{"Delete message of the day on all hosts", "删除当日所有主机上 的消息"}.

% mod_pubsub/mod_pubsub.erl
{"A friendly name for the node", "该节点的友好名称"}.
{"Roster groups allowed to subscribe", "允许订阅的花名册组"}.
{"Publish-Subscribe", "发布-订阅"}.
{"Deliver payloads with event notifications", "用事件通告传输有效负载"}.
{"Notify subscribers when the node configuration changes", "当节点设置改变时通知订阅人"}.
{"Notify subscribers when the node is deleted", "当节点被删除时通知订阅人"}.
{"Notify subscribers when items are removed from the node", "当从节点删除条款时通知订阅人"}.
{"Persist items to storage", "要存储的持续条款"}.
{"Max # of items to persist", "最多持有条款数"}.
{"Whether to allow subscriptions", "是否允许订阅"}.
{"Specify the publisher model", "指定发布人样式"}.
{"Max payload size in bytes", "最大有效负载比特数"}.
{"Only deliver notifications to available users", "仅将通知发送给可发送的用户"}.
{"ejabberd Publish-Subscribe module", "ejabberd发行-订阅模块"}.
{"PubSub subscriber request", "PubSub订阅人请求"}.
{"Choose whether to approve this entity's subscription.", "选择是否赞成该实体的订阅"}.
{"Node ID", "节点ID"}.
{"Subscriber Address", "订阅人地址"}.
{"Allow this JID to subscribe to this pubsub node?", "允许该JID订阅该pubsub节点?"}.
{"Deliver event notifications", "传递事件通知"}.
{"Specify the access model", "指定访问模式"}.
{"When to send the last published item", "何时发送最新公布的条款"}.

% mod_muc/mod_muc_log.erl
{"has been kicked because of an affiliation change", "已经因联属关系改变而被踢出"}.
{"has been kicked because the room has been changed to members-only", "已经因该房间已被改为只对会员开放而被踢出"}.
{"has been kicked because of a system shutdown", "已经因系统关机而被踢出"}.
{"Chatroom configuration modified", "聊天室配置已修改"}.
{"joins the room", "加入房间"}.
{"leaves the room", "离开房间"}.
{"has been kicked", "已被踢出"}.
{"has been banned", "已被禁止"}.
{"is now known as", "现在称呼为"}.
{"Monday", "星期一"}.
{"Tuesday", "星期二"}.
{"Wednesday", "星期三"}.
{"Thursday", "星期四"}.
{"Friday", "星期五"}.
{"Saturday", "星期六"}.
{"Sunday", "星期天"}.
{"January", "一月"}.
{"February", "二月"}.
{"March", "三月"}.
{"April", "四月"}.
{"May", "五月"}.
{"June", "六月"}.
{"July", "七月"}.
{"August", "八月"}.
{"September", "九月"}.
{"October", "十月"}.
{"November", "十一月"}.
{"December", "十二月"}.
{"Room Configuration", "房间配置"}.

% mod_muc/mod_muc.erl
{"You need an x:data capable client to register nickname", "您需要一个兼容x:data的客户端来注册昵称"}.
{"Nickname Registration at ", "昵称注册于 "}.
{"Enter nickname you want to register", "请输入您想要注册的昵称"}.
{"ejabberd MUC module", "ejabberd MUC模块"}.
{"Only service administrators are allowed to send service messages", "只有服务管理员可以发送服务消息"}.
{"Room creation is denied by service policy", "创建房间被服务政策拒绝"}.
{"Conference room does not exist", "会议室不存在"}.
{"Access denied by service policy", "访问被服务政策拒绝"}.
{"Specified nickname is already registered", "指定的名称已被注册"}.
{"You must fill in field \"Nickname\" in the form", "您必须填充表单中\"昵称\"项"}.
{"Chatrooms", "聊天室"}.

% mod_muc/mod_muc_room.erl
{"It is not allowed to send private messages", "不允许发送隐私消息"}.
{"Visitors are not allowed to change their nicknames in this room", "此房间不允许用户更改其昵称"}.
{"Allow visitors to send status text in presence updates", "更新在线状态时允许用户发送状态文本"}.
{"Allow visitors to change nickname", "允许用户更改昵称"}.
{"Make room moderated", "使房间处于监管状态"}.
{"This participant is kicked from the room because he sent an error message", "该用户由于发生了错误而被踢出了聊天室"}.
{"This participant is kicked from the room because he sent an error message to another participant", "该用户由于给其他人发送了出错信息而被踢出了聊天室"}.
{"This participant is kicked from the room because he sent an error presence", "该用户由于状态信息错误而被踢出了聊天室"}.
{"Traffic rate limit is exceeded", "已经超过传输率限制"}.
{"Maximum Number of Occupants", "占有人最大数"}.
{"No limit", "不限"}.
{"~s invites you to the room ~s", "~s邀请你到~s房间"}.
{"the password is", "密码是"}.
{" has set the subject to: ", "已将标题设置为： "}.
{"You need an x:data capable client to configure room", "您需要一个兼容x:data的客户端来配置房间"}.
{"Configuration for ", "配置 "}.
{"Room title", "房间标题"}.
{"Password", "密码"}.
{"Only moderators and participants are allowed to change subject in this room", "只允许调解人和参与人更改此房间的主题"}.
{"Only moderators are allowed to change subject in this room", "只允许调解人更改此房间的主题"}.
{"Visitors are not allowed to send messages to all occupants", "不允许访客给所有占有者发送消息"}.
{"Only occupants are allowed to send messages to the conference", "只允许占有者向会议发送消息"}.
{"It is not allowed to send private messages to the conference", "不允许向会议发送隐私消息"}.
{"Improper message type", "消息类型不恰当"}.
{"Nickname is already in use by another occupant", "昵称已被另一用户占用"}.
{"Nickname is registered by another person", "昵称已被另一人注册"}.
{"It is not allowed to send private messages of type \"groupchat\"", "\"群组聊天\"类型是不允许发送隐私消息的"}.
{"Recipient is not in the conference room", "接收人不在会议室"}.
{"Only occupants are allowed to send queries to the conference", "只允许占有者向大会询问"}.
{"Queries to the conference members are not allowed in this room", "本房间不允许向大会成员询问"}.
{"You have been banned from this room", "您已被禁止进入该房间"}.
{"Membership required to enter this room", "进入此房间需要会员身份"}.
{"Password required to enter this room", "进入此房间需要密码"}.
{"Incorrect password", "密码不正确"}.
{"Administrator privileges required", "需要管理员权限"}.
{"Moderator privileges required", "需要调解人权限"}.
{"JID ~s is invalid", "JID ~s无效"}.
{"Nickname ~s does not exist in the room", "昵称~s不在该房间"}.
{"Invalid affiliation: ~s", "无效加入: ~s"}.
{"Invalid role: ~s", "无效角色: ~s"}.
{"Owner privileges required", "需要持有人权限"}.
{"private, ", "保密"}.
{"This room is not anonymous", "此房间不是匿名房间"}.
{"Make room persistent", "持续开放房间"}.
{"Make room public searchable", "使房间可被公开搜索"}.
{"Make participants list public", "公开参与人列表"}.
{"Make room password protected", "保护房间密码"}.
{"Make room members-only", "设置房间只接收会员"}.
{"Default users as participants", "将默认用户视为参与人"}.
{"Allow users to change subject", "允许用户更改主题"}.
{"Allow users to send private messages", "允许用户发送隐私消息"}.
{"Allow users to query other users", "允许用户查询其它用户"}.
{"Allow users to send invites", "允许用户发送邀请"}.
{"Enable logging", "允许使用日志"}.
{"Number of occupants", "占用人数"}.
{"Present real JIDs to", "将真实JID显示给"}.
{"moderators only", "仅一般人"}.
{"anyone", "任何人"}.

% mod_irc/mod_irc.erl
{"ejabberd IRC module", "ejabberd IRC 模块"}.
{"You need an x:data capable client to configure mod_irc settings", "您需要一个兼容x:data的客户端来配置mod_irc设置"}.
{"Registration in mod_irc for ", "mod_irc中的注册是为 "}.
{"Enter username and encodings you wish to use for connecting to IRC servers", "请输入您想使用的用来连接到IRC服务器的用户名和编码"}.
{"IRC Username", "IRC用户名"}.
{"If you want to specify different encodings for IRC servers, fill this list with values in format '{\"irc server\", \"encoding\"}'.  By default this service use \"~s\" encoding.", "如果您想为IRC服务器指定不同的编码, 请用'{\"irc 服务器\", \"编码\"}'格式的值填充此表单. 默认情况下此服务使用\"~s\"编码."}.
{"Example: [{\"irc.lucky.net\", \"koi8-r\"}, {\"vendetta.fef.net\", \"iso8859-1\"}].", "例如: [{\"irc.lucky.net\", \"koi8-r\"}, {\"vendetta.fef.net\", \"iso8859-1\"}]."}.
{"Encodings", "编码"}.
{"IRC Transport", "IRC传输"}.

% web/ejabberd_web_admin.erl
{"ejabberd Web Admin", "ejabberd网页管理"}.
{"Administration", "管理"}.
{"Users", "用户"}.
{"Nodes", "节点"}.
{"Statistics", "统计"}.
{"(Raw)", "(原始格式)"}.
{"Submitted", "已提交"}.
{"Bad format", "格式错误"}.
{"Raw", "原始格式"}.
{"Delete Selected", "删除已选内容"}.
{"Submit", "提交"}.
{"~s access rule configuration", "~s访问规则配置"}.
{"Node not found", "没有找到节点"}.
{"Add New", "添加新用户"}.
{"Registered Users", "注册用户"}.
{"Registered Users:", "注册用户："}.
{"Change Password", "更改密码"}.
{"Connected Resources:", "已连接资源"}.
{"Password:", "密码："}.
{"None", "无"}.
{"Node ", "节点 "}.
{"Listened Ports", "监听端口"}.
{"Restart", "重启"}.
{"Stop", "停止"}.
{"RPC Call Error", "RPC调用错误"}.
{"Name", "名称"}.
{"Storage Type", "存储类型"}.
{"Size", "大小"}.
{"Memory", "内存"}.
{"OK", "OK"}.
{"Listened Ports at ", "监听的端口位于 "}.
{"Uptime:", "正常运行时间:"}.
{"CPU Time:", "CPU时间："}.
{"Transactions Commited:", "要提交的事务:"}.
{"Transactions Aborted:", "要取消的事务:"}.
{"Transactions Restarted:", "要重启的事务:"}.
{"Transactions Logged:", "要记入日志的事务:"}.
{"Port",    "端口"}.
{"Module",  "模块"}.
{"Options", "选项"}.
{"Update",  "更新"}.
{"Delete",  "删除"}.
{"Add User", "添加用户"}.
{"Offline Messages", "离线消息"}.
{"Users Last Activity", "用户上次活动"}.
{"Never", "从未"}.
{"~s's Offline Messages Queue", "~s的离线消息队列"}.
{"Time", "时间"}.
{"From", "从"}.
{"To", "到"}.
{"Packet", "数据包"}.
{"Offline Messages:", "离线消息:"}.
{"Roster", "花名册"}.
{"Nickname", "昵称"}.
{"Subscription", "订阅"}.
{"Pending", "待审"}.
{"Groups", "组"}.
{"Remove", "移除"}.
{"Add Jabber ID", "添加Jabber ID"}.
{"User ", "用户 "}.
{"Roster of ", "花名册属于 "}.
{"Online", "在线"}.
{"Validate", "确认"}.
{"Name:", "名称:"}.
{"Description:", "描述:"}.
{"Members:", "会员:"}.
{"Displayed Groups:", "已显示的组:"}.
{"Group ", "组"}.
{"Period: ", "持续时间:"}.
{"Last month", "上个月"}.
{"Last year", "上一年"}.
{"All activity", "所有活动"}.
{"Show Ordinary Table", "显示普通列表"}.
{"Show Integral Table", "显示完整列表"}.
{"Modules at ", "模块位于 "}.
{"Start", "开始"}.
{"Virtual Hosts", "虚拟主机"}.
{"ejabberd virtual hosts", "ejabberd虚拟主机"}.
{"Host", "主机"}.
{"No Data", "没有数据"}.
{"Online Users:", "在线用户:"}.
{"Outgoing s2s Connections:", "出站s2s连接:"}.
{"Outgoing s2s Servers:", "出站s2s服务器"}.
{"Database Tables at ", "数据库列表位于 "}.
{"Backup of ", "备份来源 "}.
{"Remark that these options will only backup the builtin Mnesia database. If you are using the ODBC module, you also need to backup your SQL database separately.", "注意：这些选项仅将备份内置的Mnesia数据库. 如果您在使用ODBC模块, 您还需要分别备份您的数据库."}.
{"Store binary backup:", "存储为二进制备份:"}.
{"Restore binary backup immediately:", "立即恢复二进制备份:"}.
{"Restore binary backup after next ejabberd restart (requires less memory):", "在下次ejabberd重启后恢复二进制备份(需要的内存更少):"}.
{"Store plain text backup:", "存储为普通文本备份:"}.
{"Restore plain text backup immediately:", "立即恢复普通文本备份:"}.
{"Statistics of ~p", "~p的统计"}.
{"Update ", "更新 "}.
{"Update plan", "更新计划"}.
{"Updated modules", "更新模块"}.
{"Update script", "更新脚本"}.
{"Low level update script", "低级别更新脚本"}.
{"Script check", "检查脚本"}.
{"Shared Roster Groups", "共享的花名册组群"}.
{"Last Activity", "上次活动"}.

% mod_proxy65/mod_proxy65_service.erl
{"ejabberd SOCKS5 Bytestreams module", "ejabberd SOCKS5字节流模块"}.

% mod_vcard_odbc.erl
{"vCard User Search", "vCard用户搜索"}.

% Local Variables:
% mode: erlang
% End:
% vim: set filetype=erlang tabstop=8:
