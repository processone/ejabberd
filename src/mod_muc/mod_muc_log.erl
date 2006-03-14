%%%----------------------------------------------------------------------
%%% File    : mod_muc_log.erl
%%% Author  : Badlop
%%% Purpose : MUC room logging
%%% Created : 12 Mar 2006 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_muc_log).
-author('badlop').

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
	 start/2,
	 stop/1,
	 check_access_log/2,
	 add_to_log/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(T(Text), translate:translate(Lang, Text)).
-define(PROCNAME, ejabberd_mod_muc_log).
-record(room, {jid, title, subject, subject_author, config}).


-record(state, {host,
		out_dir,
		dir_type,
		css_file,
		access,
		lang,
		timezone,
		top_link}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [Host, Opts]},
	 temporary,
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).

add_to_log(Host, Type, Data, Room, Opts) ->
    gen_server:cast(get_proc_name(Host),
		    {add_to_log, Type, Data, Room, Opts}).

check_access_log(Host, From) ->
    case catch gen_server:call(get_proc_name(Host),
			       {check_access_log, Host, From}) of
	{'EXIT', _Error} ->
	    false;
	Res ->
	    Res
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    OutDir = gen_mod:get_opt(outdir, Opts, "www/muc"),
    DirType = gen_mod:get_opt(dirtype, Opts, subdirs),
    CSSFile = gen_mod:get_opt(cssfile, Opts, false),
    AccessLog = gen_mod:get_opt(access_log, Opts, muc_admin),
    Timezone = gen_mod:get_opt(timezone, Opts, local),
    Top_link = gen_mod:get_opt(top_link, Opts, {"/", "Home"}),
    Lang = case ejabberd_config:get_local_option({language, Host}) of
	       undefined ->
		   "";
	       L ->
		   L
	   end,
    {ok, #state{host = Host,
		out_dir = OutDir,
		dir_type = DirType,
		css_file = CSSFile,
		access = AccessLog,
		lang = Lang,
		timezone = Timezone,
		top_link = Top_link}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({check_access_log, ServerHost, FromJID}, _From, State) ->
    Reply = acl:match_rule(ServerHost, State#state.access, FromJID),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_to_log, Type, Data, Room, Opts}, State) ->
    case catch add_to_log2(Type, Data, Room, Opts, State) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
add_to_log2(text, {Nick, Packet}, Room, Opts, State) ->
    case {xml:get_subtag(Packet, "subject"), xml:get_subtag(Packet, "body")} of
	{false, false} ->
	    ok;
	{false, SubEl} ->
	    Message = {body, htmlize(xml:get_tag_cdata(SubEl))},
	    add_message_to_log(Nick, Message, Room, Opts, State);
	{SubEl, _} ->
	    Message = {subject, htmlize(xml:get_tag_cdata(SubEl))},
	    add_message_to_log(Nick, Message, Room, Opts, State)
    end;

add_to_log2(roomconfig_change, _, Room, Opts, State) ->
    add_message_to_log("", roomconfig_change, Room, Opts, State);

add_to_log2(nickchange, {OldNick, NewNick}, Room, Opts, State) ->
    add_message_to_log(NewNick, {nickchange, OldNick}, Room, Opts, State);

add_to_log2(join, Nick, Room, Opts, State) ->
    add_message_to_log(Nick, join, Room, Opts, State);

add_to_log2(leave, {Nick, Reason}, Room, Opts, State) ->
    case Reason of
	"" -> add_message_to_log(Nick, leave, Room, Opts, State);
	_ -> add_message_to_log(Nick, {leave, Reason}, Room, Opts, State)
    end;

add_to_log2(kickban, {Nick, Reason, Code}, Room, Opts, State) ->
    add_message_to_log(Nick, {kickban, Code, Reason}, Room, Opts, State).


%%----------------------------------------------------------------------
%% Core

build_filename_string(TimeStamp, OutDir, RoomJID, DirType) ->
    {{Year, Month, Day}, _Time} = TimeStamp,

    % Directory and file names
    {Dir, Filename, Rel} =
	case DirType of
	    subdirs ->
		SYear = lists:flatten(io_lib:format("~4..0w", [Year])),
		SMonth = lists:flatten(io_lib:format("~2..0w", [Month])),
		SDay = lists:flatten(io_lib:format("~2..0w", [Day])),
		{filename:join(SYear, SMonth), SDay, "../.."};
	    plain ->
		Date = lists:flatten(
			 io_lib:format("~4..0w-~2..0w-~2..0w",
				       [Year, Month, Day])),
		{"", Date, "."}
	end,
    Fd = filename:join([OutDir, RoomJID, Dir]),
    Fn = filename:join([Fd, Filename ++ ".html"]),
    Fnrel = filename:join([Rel, Dir, Filename ++ ".html"]),
    {Fd, Fn, Fnrel}.

% calculate day before
get_timestamp_daydiff(TimeStamp, Daydiff) ->
    {Date1, HMS} = TimeStamp,
    Date2 = calendar:gregorian_days_to_date(
	      calendar:date_to_gregorian_days(Date1) + Daydiff),
    {Date2, HMS}.

% Try to close the previous day log, if it exists
close_previous_log(Fn) ->
    case file:read_file_info(Fn) of
	{ok, _} ->
	    {ok, F} = file:open(Fn, [append]),
	    fw(F, "<div class=\"legend\">ejabberd/mod_muc log<span class=\"w3c\"><a href=\"http://validator.w3.org/check?uri=referer\"><img src=\"http://www.w3.org/Icons/valid-xhtml10\" alt=\"Valid XHTML 1.0 Transitional\" height=\"31\" width=\"88\" /></a> <a href=\"http://jigsaw.w3.org/css-validator/\"><img style=\"border:0;width:88px;height:31px\" src=\"http://jigsaw.w3.org/css-validator/images/vcss\" alt=\"Valid CSS!\" /></a></span></div></body></html>"),
	    file:close(F);
	_ -> ok
    end.

add_message_to_log(Nick, Message, RoomJID, Opts, State) ->
    #state{out_dir = OutDir,
	   dir_type = DirType,
	   css_file = CSSFile,
	   lang = Lang,
	   timezone = Timezone,
	   top_link = TopLink} = State,
    Room = get_room_info(RoomJID, Opts),

    TimeStamp = case Timezone of
		    local -> calendar:now_to_local_time(now());
		    universal -> calendar:now_to_universal_time(now())
		end,
    {Fd, Fn, _Dir} = build_filename_string(TimeStamp, OutDir, Room#room.jid, DirType),
    {Date, Time} = TimeStamp,

    % Open file, create if it does not exist, create parent dirs if needed
    case file:read_file_info(Fn) of
	{ok, _} ->
	    {ok, F} = file:open(Fn, [append]);
	{error, enoent} ->
	    make_dir_rec(Fd),
	    {ok, F} = file:open(Fn, [append]),
	    Datestring = get_dateweek(Date, Lang),

	    TimeStampYesterday = get_timestamp_daydiff(TimeStamp, -1),
	    {_FdYesterday, FnYesterday, DatePrev} =
		build_filename_string(
		  TimeStampYesterday, OutDir, Room#room.jid, DirType),

	    TimeStampTomorrow = get_timestamp_daydiff(TimeStamp, 1),
	    {_FdTomorrow, _FnTomorrow, DateNext} =
		build_filename_string(
		  TimeStampTomorrow, OutDir, Room#room.jid, DirType),

	    HourOffset = calc_hour_offset(TimeStamp),
	    put_header(F, Room, Datestring, CSSFile, Lang,
		       HourOffset, DatePrev, DateNext, TopLink),

	    close_previous_log(FnYesterday)
    end,

    % Build message
    Text = case Message of
	       roomconfig_change ->
		   RoomConfig = roomconfig_to_string(Room#room.config, Lang),
		   put_room_config(F, RoomConfig, Lang),
		   io_lib:format("<font class=\"mrcm\">~s</font><br/>", 
				 [?T("Chatroom configuration modified")]);
	       join ->  
		   io_lib:format("<font class=\"mj\">~s ~s</font><br/>", 
				 [Nick, ?T("joins the room")]);
	       leave ->  
		   io_lib:format("<font class=\"ml\">~s ~s</font><br/>", 
				 [Nick, ?T("leaves the room")]);
	       {leave, Reason} ->  
		   io_lib:format("<font class=\"ml\">~s ~s: ~s</font><br/>", 
				 [Nick, ?T("leaves the room"), Reason]);
	       {kickban, "307", ""} ->  
		   io_lib:format("<font class=\"mk\">~s ~s</font><br/>", 
				 [Nick, ?T("has been kicked")]);
	       {kickban, "307", Reason} ->  
		   io_lib:format("<font class=\"mk\">~s ~s: ~s</font><br/>", 
				 [Nick, ?T("has been kicked"), Reason]);
	       {kickban, "301", ""} ->  
		   io_lib:format("<font class=\"mb\">~s ~s</font><br/>", 
				 [Nick, ?T("has been banned")]);
	       {kickban, "301", Reason} ->  
		   io_lib:format("<font class=\"mb\">~s ~s: ~s</font><br/>", 
				 [Nick, ?T("has been banned"), Reason]);
	       {nickchange, OldNick} ->  
		   io_lib:format("<font class=\"mnc\">~s ~s ~s</font><br/>", 
				 [OldNick, ?T("is now known as"), Nick]);
	       {subject, T} ->  
		   io_lib:format("<font class=\"msc\">~s~s~s</font><br/>", 
				 [Nick, ?T(" has set the subject to: "), T]);
	       {body, T} ->  
		   case regexp:first_match(T, "^/me\s") of
		       {match, _, _} ->
			   io_lib:format("<font class=\"mne\">~s ~s</font><br/>", 
					 [Nick, string:substr(T, 5)]);
		       nomatch ->
			   io_lib:format("<font class=\"mn\">&lt;~s&gt;</font> ~s<br/>", 
					 [Nick, T])
		   end
	   end,
    {Hour, Minute, Second} = Time,
    STime = lists:flatten(
	      io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Minute, Second])),

    % Write message
    file:write(F, io_lib:format("<a name=\"~s\" href=\"#~s\" class=\"ts\">[~s]</a> ~s~n", 
				[STime, STime, STime, Text])),

    % Close file
    file:close(F),
    ok.


%%----------------------------------------------------------------------
%% Utilities

get_dateweek(Date, Lang) ->
    Weekday = case calendar:day_of_the_week(Date) of
		  1 -> ?T("Monday");
		  2 -> ?T("Tuesday");
		  3 -> ?T("Wednesday");
		  4 -> ?T("Thursday");
		  5 -> ?T("Friday");
		  6 -> ?T("Saturday");
		  7 -> ?T("Sunday")
	      end,
    {Y, M, D} = Date,
    Month = case M of
		1 -> ?T("January");
		2 -> ?T("February");
		3 -> ?T("March");
		4 -> ?T("April");
		5 -> ?T("May");
		6 -> ?T("June");
		7 -> ?T("July");
		8 -> ?T("August");
		9 -> ?T("September");
		10 -> ?T("October");
		11 -> ?T("November");
		12 -> ?T("December")
	    end,
    case Lang of
	"en" -> io_lib:format("~s, ~s ~w, ~w", [Weekday, Month, D, Y]);
	"es" -> io_lib:format("~s ~w de ~s de ~w", [Weekday, D, Month, Y]);
	_    -> io_lib:format("~s, ~w ~s ~w", [Weekday, D, Month, Y])
    end.

make_dir_rec(Dir) ->
    case file:read_file_info(Dir) of
	{ok, _} ->
	    ok;
	{error, enoent} ->
	    DirS = filename:split(Dir),
	    DirR = lists:sublist(DirS, length(DirS)-1),
	    make_dir_rec(filename:join(DirR)),
	    file:make_dir(Dir)
    end.

fw(F, S, O) -> io:format(F, S ++ "~n", O).
fw(F, S) -> fw(F, S, []).

put_header(F, Room, Date, CSSFile, Lang, Hour_offset, Date_prev, Date_next, Top_link) ->
    fw(F, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"),
    fw(F, "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"~s\" lang=\"~s\">", [Lang, Lang]),
    fw(F, "<head>"),
    fw(F, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />"),
    fw(F, "<title>~s - ~s</title>", [Room#room.title, Date]),
    put_header_css(F, CSSFile),
    put_header_script(F),
    fw(F, "</head>"),
    fw(F, "<body>"),
    {Top_url, Top_text} = Top_link,
    fw(F, "<div style=\"text-align: right;\"><a style=\"color: #AAAAAA; font-family: monospace; text-decoration: none; font-weight: bold;\" href=\"~s\">~s</a></div>", [Top_url, Top_text]),
    fw(F, "<div class=\"roomtitle\"><a class=\"roomtitle\" href=\"xmpp:~s?join\">~s</a></div>", [Room#room.jid, Room#room.title]),
    fw(F, "<a class=\"roomjid\" href=\"xmpp:~s?join\">~s</a>", [Room#room.jid, Room#room.jid]),
    fw(F, "<div class=\"logdate\">~s<span class=\"w3c\"><a class=\"nav\" href=\"~s\">&lt;</a> <a class=\"nav\" href=\".\/\">^</a> <a class=\"nav\" href=\"~s\">&gt;</a></span></div>", [Date, Date_prev, Date_next]),
    case {Room#room.subject_author, Room#room.subject} of
	{"", ""} -> ok;
	{SuA, Su} -> fw(F, "<div class=\"roomsubject\">~s~s~s</div>", [SuA, ?T(" has set the subject to: "), htmlize(Su)])
    end,
    RoomConfig = roomconfig_to_string(Room#room.config, Lang),
    put_room_config(F, RoomConfig, Lang),
    Time_offset_str = case Hour_offset<0 of
			  true -> io_lib:format("~p", [Hour_offset]);
			  false -> io_lib:format("+~p", [Hour_offset])
		      end,
    fw(F, "<br/><a class=\"ts\">GMT~s</a><br/>", [Time_offset_str]).

put_header_css(F, false) ->
    fw(F, "<style type=\"text/css\">"),
    fw(F, "<!--"),
    fw(F, ".ts {color: #AAAAAA; text-decoration: none;}"),
    fw(F, ".mrcm {color: #009900; font-style: italic; font-weight: bold;}"),
    fw(F, ".msc {color: #009900; font-style: italic; font-weight: bold;}"),
    fw(F, ".mj {color: #009900; font-style: italic;}"),
    fw(F, ".ml {color: #009900; font-style: italic;}"),
    fw(F, ".mk {color: #009900; font-style: italic;}"),
    fw(F, ".mb {color: #009900; font-style: italic;}"),
    fw(F, ".mnc {color: #009900; font-style: italic;}"),
    fw(F, ".mn {color: #0000AA;}"),
    fw(F, ".mne {color: #AA0099;}"),
    fw(F, "a.nav {color: #AAAAAA; font-family: monospace; letter-spacing: 3px; text-decoration: none;}"),
    fw(F, "div.roomtitle {border-bottom: #224466 solid 3pt; margin-left: 20pt;}"),
    fw(F, "a.roomtitle {color: #336699; font-size: 24px; font-weight: bold; font-family: sans-serif; letter-spacing: 3px; text-decoration: none;}"),
    fw(F, "a.roomjid {color: #336699; font-size: 24px; font-weight: bold; font-family: sans-serif; letter-spacing: 3px; margin-left: 20pt; text-decoration: none;}"),
    fw(F, "div.logdate {color: #663399; font-size: 20px; font-weight: bold; font-family: sans-serif; letter-spacing: 2px; border-bottom: #224466 solid 1pt; margin-left:80pt; margin-top:20px;}"),
    fw(F, "div.roomsubject {color: #336699; font-size: 18px; font-family: sans-serif; margin-left: 80pt; margin-bottom: 10px;}"),
    fw(F, "div.rc {color: #336699; font-size: 12px; font-family: sans-serif; margin-left: 50%; text-align: right; background: #f3f6f9; border-bottom: 1px solid #336699; border-right: 4px solid #336699;}"),
    fw(F, "div.rct {font-weight: bold; background: #e3e6e9; padding-right: 10px;}"),
    fw(F, "div.rcos {padding-right: 10px;}"),
    fw(F, "div.rcoe {color: green;}"),
    fw(F, "div.rcod {color: red;}"),
    fw(F, "div.rcoe:after {content: \": v\";}"),
    fw(F, "div.rcod:after {content: \": x\";}"),
    fw(F, "div.rcot:after {}"),
    fw(F, ".legend {width: 100%; margin-top: 30px; border-top: #224466 solid 1pt;  padding: 10px 0px 10px 0px; text-align: left; font-family: monospace; letter-spacing: 2px;}"),
    fw(F, ".w3c {position: absolute; right: 10px; width: 60%; text-align: right; font-family: monospace; letter-spacing: 1px;}"),
    fw(F, "//-->"),
    fw(F, "</style>");

put_header_css(F, CSSFile) ->
    fw(F, "<link rel=\"stylesheet\" type=\"text/css\" href=\"~s\" media=\"all\">", [CSSFile]).

put_header_script(F) ->
    fw(F, "<script type=\"text/javascript\">"),
    fw(F, "function sh(e) // Show/Hide an element"),
    fw(F, "{if(document.getElementById(e).style.display=='none')"),
    fw(F, "{document.getElementById(e).style.display='block';}"),
    fw(F, "else {document.getElementById(e).style.display='none';}}"),
    fw(F, "</script>").

put_room_config(F, RoomConfig, Lang) ->
    {_, Now2, _} = now(),
    fw(F, "<div class=\"rc\">"),
    fw(F,   "<div class=\"rct\" onclick=\"sh('a~p');return false;\">~s</div>", [Now2, ?T("Room Configuration")]),
    fw(F,   "<div class=\"rcos\" id=\"a~p\" style=\"display: none;\" ><br/>~s</div>", [Now2, RoomConfig]),
    fw(F, "</div>").

htmlize(S1) ->
    S2_list = string:tokens(S1, "\n"),
    lists:foldl(
      fun(Si, Res) -> 
	      Si2 = htmlize2(Si),
	      case Res of
		  "" -> Si2;
		  _ -> Res ++ "<br/>" ++ Si2
	      end
      end, 
      "",
      S2_list).

htmlize2(S1) ->
    S2 = element(2, regexp:gsub(S1, "<", "\\&lt;")),
    S3 = element(2, regexp:gsub(S2, ">", "\\&gt;")),
    S4 = element(2, regexp:gsub(S3, "(http|ftp)://.[^ ]*", "<a href=\"&\">&</a>")),
    %element(2, regexp:gsub(S4, "  ", "\\&nbsp;")).
    S4.

get_room_info(RoomJID, Opts) ->
    Title =
	case lists:keysearch(title, 1, Opts) of
	    {value, {_, T}} -> T;
	    false -> ""
	end,
    Subject =
	case lists:keysearch(subject, 1, Opts) of
	    {value, {_, S}} -> S;
	    false -> ""
	end,
    SubjectAuthor =
	case lists:keysearch(subject_author, 1, Opts) of
	    {value, {_, SA}} -> SA;
	    false -> ""
	end,
    #room{jid = jlib:jid_to_string(RoomJID),
	  title = Title,
	  subject = Subject,
	  subject_author = SubjectAuthor,
	  config = Opts
	 }.

roomconfig_to_string(Options, Lang) ->
    % Get title, if available
    Title = case lists:keysearch(title, 1, Options) of
		{value, Tuple} -> [Tuple];
		false -> []
	    end,
	
    % Remove title from list
    Os1 = lists:keydelete(title, 1, Options),
	
    % Order list
    Os2 = lists:sort(Os1),
	
    % Add title to ordered list
    Options2 = Title ++ Os2,

    lists:foldl(
      fun({Opt, Val}, R) -> 
	      case get_roomconfig_text(Opt) of
		  undefined ->
		      R;
		  OptT ->
		      OptText = ?T(OptT),
		      R2 = case Val of
			       false -> "<div class=\"rcod\">" ++ OptText ++ "</div>";
			       true -> "<div class=\"rcoe\">" ++ OptText ++ "</div>";
			       "" -> "<div class=\"rcod\">" ++ OptText ++ "</div>";
			       T -> 
				   case Opt of
				       password -> "<div class=\"rcoe\">" ++ OptText ++ "</div>";
				       title -> "<div class=\"rcot\">" ++ ?T("Room title") ++ ": \"" ++ T ++ "\"</div>";
				       _ -> "\"" ++ T ++ "\""
				   end
			   end,
		      R ++ R2
	      end
      end,
      "",
      Options2).

get_roomconfig_text(title) -> "Room title";
get_roomconfig_text(persistent) -> "Make room persistent";
get_roomconfig_text(public) -> "Make room public searchable";
get_roomconfig_text(public_list) -> "Make participants list public";
get_roomconfig_text(password_protected) -> "Make room password protected";
get_roomconfig_text(password) -> "Password";
get_roomconfig_text(anonymous) -> "Make room semianonymous";
get_roomconfig_text(members_only) -> "Make room members-only";
get_roomconfig_text(moderated) -> "Make room moderated";
get_roomconfig_text(members_by_default) -> "Default users as participants";
get_roomconfig_text(allow_change_subj) -> "Allow users to change subject";
get_roomconfig_text(allow_private_messages) -> "Allow users to send private messages";
get_roomconfig_text(allow_query_users) -> "Allow users to query other users";
get_roomconfig_text(allow_user_invites) -> "Allow users to send invites";
get_roomconfig_text(logging) ->  "Enable logging";
get_roomconfig_text(_) -> undefined.

get_proc_name(Host) -> gen_mod:get_module_proc(Host, ?PROCNAME).

calc_hour_offset(TimeHere) ->
	TimeZero = calendar:now_to_universal_time(now()),
	TimeHereHour = calendar:datetime_to_gregorian_seconds(TimeHere) div 3600,
	TimeZeroHour = calendar:datetime_to_gregorian_seconds(TimeZero) div 3600,
	TimeHereHour - TimeZeroHour.
