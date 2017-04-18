%%%----------------------------------------------------------------------
%%% File    : mod_muc_log.erl
%%% Author  : Badlop@process-one.net
%%% Purpose : MUC room logging
%%% Created : 12 Mar 2006 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_log).

-protocol({xep, 334, '0.2'}).

-behaviour(ejabberd_config).

-author('badlop@process-one.net').

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3, transform_module_options/1,
	 check_access_log/2, add_to_log/5]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1, opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("xmpp.hrl").
-include("mod_muc_room.hrl").

-define(T(Text), translate:translate(Lang, Text)).
-record(room, {jid, title, subject, subject_author, config}).

-define(PLAINTEXT_CO, <<"ZZCZZ">>).
-define(PLAINTEXT_IN, <<"ZZIZZ">>).
-define(PLAINTEXT_OUT, <<"ZZOZZ">>).

-record(logstate, {host,
		out_dir,
		dir_type,
		dir_name,
		file_format,
		file_permissions,
		css_file,
		access,
		lang,
		timezone,
		spam_prevention,
		top_link}).

%%====================================================================
%% API
%%====================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, _OldOpts) ->
    Proc = get_proc_name(Host),
    gen_server:cast(Proc, {reload, NewOpts}).

add_to_log(Host, Type, Data, Room, Opts) ->
    gen_server:cast(get_proc_name(Host),
		    {add_to_log, Type, Data, Room, Opts}).

check_access_log(Host, From) ->
    case catch gen_server:call(get_proc_name(Host),
			       {check_access_log, Host, From})
	of
      {'EXIT', _Error} -> deny;
      Res -> Res
    end.

transform_module_options(Opts) ->
    lists:map(
      fun({top_link, {S1, S2}}) ->
              {top_link, [{S1, S2}]};
         (Opt) ->
              Opt
      end, Opts).

depends(_Host, _Opts) ->
    [{mod_muc, hard}].

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    {ok, init_state(Host, Opts)}.

handle_call({check_access_log, ServerHost, FromJID}, _From, State) ->
    Reply = acl:match_rule(ServerHost, State#logstate.access, FromJID),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({reload, Opts}, #logstate{host = Host}) ->
    {noreply, init_state(Host, Opts)};
handle_cast({add_to_log, Type, Data, Room, Opts}, State) ->
    case catch add_to_log2(Type, Data, Room, Opts, State) of
      {'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]);
      _ -> ok
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
init_state(Host, Opts) ->
    OutDir = gen_mod:get_opt(outdir, Opts,
                             fun iolist_to_binary/1,
                             <<"www/muc">>),
    DirType = gen_mod:get_opt(dirtype, Opts,
                              fun(subdirs) -> subdirs;
                                 (plain) -> plain
                              end, subdirs),
    DirName = gen_mod:get_opt(dirname, Opts,
                              fun(room_jid) -> room_jid;
                                 (room_name) -> room_name
                              end, room_jid),
    FileFormat = gen_mod:get_opt(file_format, Opts,
                                 fun(html) -> html;
                                    (plaintext) -> plaintext
                                 end, html),
    FilePermissions = gen_mod:get_opt(file_permissions, Opts,
                                 fun(SubOpts) ->
                                         F = fun({mode, Mode}, {_M, G}) ->
                                                        {Mode, G};
                                                ({group, Group}, {M, _G}) ->
                                                        {M, Group}
                                             end,
                                         lists:foldl(F, {644, 33}, SubOpts)
                                 end, {644, 33}),
    CSSFile = gen_mod:get_opt(cssfile, Opts,
                              fun iolist_to_binary/1,
                              false),
    AccessLog = gen_mod:get_opt(access_log, Opts,
                                fun acl:access_rules_validator/1,
                                muc_admin),
    Timezone = gen_mod:get_opt(timezone, Opts,
                               fun(local) -> local;
                                  (universal) -> universal
                               end, local),
    Top_link = gen_mod:get_opt(top_link, Opts,
                               fun([{S1, S2}]) ->
                                       {iolist_to_binary(S1),
                                        iolist_to_binary(S2)}
                               end, {<<"/">>, <<"Home">>}),
    NoFollow = gen_mod:get_opt(spam_prevention, Opts,
                               fun(B) when is_boolean(B) -> B end,
                               true),
    Lang = ejabberd_config:get_option(
             {language, Host},
             fun iolist_to_binary/1,
             ?MYLANG),
    #logstate{host = Host, out_dir = OutDir,
	      dir_type = DirType, dir_name = DirName,
	      file_format = FileFormat, css_file = CSSFile,
	      file_permissions = FilePermissions,
	      access = AccessLog, lang = Lang, timezone = Timezone,
	      spam_prevention = NoFollow, top_link = Top_link}.

add_to_log2(text, {Nick, Packet}, Room, Opts, State) ->
    case has_no_permanent_store_hint(Packet) of
	false ->
	    case {Packet#message.subject, Packet#message.body} of
		{[], []} -> ok;
		{[], Body} ->
		    Message = {body, xmpp:get_text(Body)},
		    add_message_to_log(Nick, Message, Room, Opts, State);
		{Subj, _} ->
		    Message = {subject, xmpp:get_text(Subj)},
		    add_message_to_log(Nick, Message, Room, Opts, State)
	    end;
	true -> ok
    end;
add_to_log2(roomconfig_change, _Occupants, Room, Opts,
	    State) ->
    add_message_to_log(<<"">>, roomconfig_change, Room,
		       Opts, State);
add_to_log2(roomconfig_change_enabledlogging, Occupants,
	    Room, Opts, State) ->
    add_message_to_log(<<"">>,
		       {roomconfig_change, Occupants}, Room, Opts, State);
add_to_log2(room_existence, NewStatus, Room, Opts,
	    State) ->
    add_message_to_log(<<"">>, {room_existence, NewStatus},
		       Room, Opts, State);
add_to_log2(nickchange, {OldNick, NewNick}, Room, Opts,
	    State) ->
    add_message_to_log(NewNick, {nickchange, OldNick}, Room,
		       Opts, State);
add_to_log2(join, Nick, Room, Opts, State) ->
    add_message_to_log(Nick, join, Room, Opts, State);
add_to_log2(leave, {Nick, Reason}, Room, Opts, State) ->
    case Reason of
      <<"">> ->
	  add_message_to_log(Nick, leave, Room, Opts, State);
      _ ->
	  add_message_to_log(Nick, {leave, Reason}, Room, Opts,
			     State)
    end;
add_to_log2(kickban, {Nick, Reason, Code}, Room, Opts,
	    State) ->
    add_message_to_log(Nick, {kickban, Code, Reason}, Room,
		       Opts, State).

%%----------------------------------------------------------------------
%% Core

build_filename_string(TimeStamp, OutDir, RoomJID,
		      DirType, DirName, FileFormat) ->
    {{Year, Month, Day}, _Time} = TimeStamp,
    {Dir, Filename, Rel} = case DirType of
			     subdirs ->
				 SYear =
				     (str:format("~4..0w",
								    [Year])),
				 SMonth =
				     (str:format("~2..0w",
								    [Month])),
				 SDay = (str:format("~2..0w",
								       [Day])),
				 {fjoin([SYear, SMonth]), SDay,
				  <<"../..">>};
			     plain ->
				 Date =
				     (str:format("~4..0w-~2..0w-~2..0w",
								    [Year,
								     Month,
								     Day])),
				 {<<"">>, Date, <<".">>}
			   end,
    RoomString = case DirName of
		   room_jid -> RoomJID;
		   room_name -> get_room_name(RoomJID)
		 end,
    Extension = case FileFormat of
		  html -> <<".html">>;
		  plaintext -> <<".txt">>
		end,
    Fd = fjoin([OutDir, RoomString, Dir]),
    Fn = fjoin([Fd, <<Filename/binary, Extension/binary>>]),
    Fnrel = fjoin([Rel, Dir, <<Filename/binary, Extension/binary>>]),
    {Fd, Fn, Fnrel}.

get_room_name(RoomJID) ->
    JID = jid:decode(RoomJID), JID#jid.user.

%% calculate day before
get_timestamp_daydiff(TimeStamp, Daydiff) ->
    {Date1, HMS} = TimeStamp,
    Date2 =
	calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date1)
					  + Daydiff),
    {Date2, HMS}.

%% Try to close the previous day log, if it exists
close_previous_log(Fn, Images_dir, FileFormat) ->
    case file:read_file_info(Fn) of
      {ok, _} ->
	  {ok, F} = file:open(Fn, [append]),
	  write_last_lines(F, Images_dir, FileFormat),
	  file:close(F);
      _ -> ok
    end.

write_last_lines(_, _, plaintext) -> ok;
write_last_lines(F, Images_dir, _FileFormat) ->
    fw(F, <<"<div class=\"legend\">">>),
    fw(F,
       <<"  <a href=\"http://www.ejabberd.im\"><img "
	 "style=\"border:0\" src=\"~s/powered-by-ejabbe"
	 "rd.png\" alt=\"Powered by ejabberd - robust, scalable and extensible XMPP server\"/></a>">>,
       [Images_dir]),
    fw(F,
       <<"  <a href=\"http://www.erlang.org/\"><img "
	 "style=\"border:0\" src=\"~s/powered-by-erlang"
	 ".png\" alt=\"Powered by Erlang\"/></a>">>,
       [Images_dir]),
    fw(F, <<"<span class=\"w3c\">">>),
    fw(F,
       <<"  <a href=\"http://validator.w3.org/check?uri"
	 "=referer\"><img style=\"border:0;width:88px;h"
	 "eight:31px\" src=\"~s/valid-xhtml10.png\" "
	 "alt=\"Valid XHTML 1.0 Transitional\" "
	 "/></a>">>,
       [Images_dir]),
    fw(F,
       <<"  <a href=\"http://jigsaw.w3.org/css-validato"
	 "r/\"><img style=\"border:0;width:88px;height:"
	 "31px\" src=\"~s/vcss.png\" alt=\"Valid "
	 "CSS!\"/></a>">>,
       [Images_dir]),
    fw(F, <<"</span></div></body></html>">>).

set_filemode(Fn, {FileMode, FileGroup}) ->
    ok = file:change_mode(Fn, list_to_integer(integer_to_list(FileMode), 8)),
    ok = file:change_group(Fn, FileGroup).

htmlize_nick(Nick1, html) ->
    htmlize(<<"<", Nick1/binary, ">">>, html);
htmlize_nick(Nick1, plaintext) ->
    htmlize(<<?PLAINTEXT_IN/binary, Nick1/binary, ?PLAINTEXT_OUT/binary>>, plaintext).

add_message_to_log(Nick1, Message, RoomJID, Opts,
		   State) ->
    #logstate{out_dir = OutDir, dir_type = DirType,
	      dir_name = DirName, file_format = FileFormat,
	      file_permissions = FilePermissions,
	      css_file = CSSFile, lang = Lang, timezone = Timezone,
	      spam_prevention = NoFollow, top_link = TopLink} =
	State,
    Room = get_room_info(RoomJID, Opts),
    Nick = htmlize(Nick1, FileFormat),
    Nick2 = htmlize_nick(Nick1, FileFormat),
    Now = p1_time_compat:timestamp(),
    TimeStamp = case Timezone of
		  local -> calendar:now_to_local_time(Now);
		  universal -> calendar:now_to_universal_time(Now)
		end,
    {Fd, Fn, _Dir} = build_filename_string(TimeStamp,
					   OutDir, Room#room.jid, DirType,
					   DirName, FileFormat),
    {Date, Time} = TimeStamp,
    case file:read_file_info(Fn) of
      {ok, _} -> {ok, F} = file:open(Fn, [append]);
      {error, enoent} ->
	  make_dir_rec(Fd),
	  {ok, F} = file:open(Fn, [append]),
	  catch set_filemode(Fn, FilePermissions),
	  Datestring = get_dateweek(Date, Lang),
	  TimeStampYesterday = get_timestamp_daydiff(TimeStamp,
						     -1),
	  {_FdYesterday, FnYesterday, DatePrev} =
	      build_filename_string(TimeStampYesterday, OutDir,
				    Room#room.jid, DirType, DirName,
				    FileFormat),
	  TimeStampTomorrow = get_timestamp_daydiff(TimeStamp, 1),
	  {_FdTomorrow, _FnTomorrow, DateNext} =
	      build_filename_string(TimeStampTomorrow, OutDir,
				    Room#room.jid, DirType, DirName,
				    FileFormat),
	  HourOffset = calc_hour_offset(TimeStamp),
	  put_header(F, Room, Datestring, CSSFile, Lang,
		     HourOffset, DatePrev, DateNext, TopLink, FileFormat),
	  Images_dir = fjoin([OutDir, <<"images">>]),
	  file:make_dir(Images_dir),
	  create_image_files(Images_dir),
	  Images_url = case DirType of
			 subdirs -> <<"../../../images">>;
			 plain -> <<"../images">>
		       end,
	  close_previous_log(FnYesterday, Images_url, FileFormat)
    end,
    Text = case Message of
	     roomconfig_change ->
		 RoomConfig = roomconfig_to_string(Room#room.config,
						   Lang, FileFormat),
		 put_room_config(F, RoomConfig, Lang, FileFormat),
		 io_lib:format("<font class=\"mrcm\">~s</font><br/>",
			       [?T(<<"Chatroom configuration modified">>)]);
	     {roomconfig_change, Occupants} ->
		 RoomConfig = roomconfig_to_string(Room#room.config,
						   Lang, FileFormat),
		 put_room_config(F, RoomConfig, Lang, FileFormat),
		 RoomOccupants = roomoccupants_to_string(Occupants,
							 FileFormat),
		 put_room_occupants(F, RoomOccupants, Lang, FileFormat),
		 io_lib:format("<font class=\"mrcm\">~s</font><br/>",
			       [?T(<<"Chatroom configuration modified">>)]);
	     join ->
		 io_lib:format("<font class=\"mj\">~s ~s</font><br/>",
			       [Nick, ?T(<<"joins the room">>)]);
	     leave ->
		 io_lib:format("<font class=\"ml\">~s ~s</font><br/>",
			       [Nick, ?T(<<"leaves the room">>)]);
	     {leave, Reason} ->
		 io_lib:format("<font class=\"ml\">~s ~s: ~s</font><br/>",
			       [Nick, ?T(<<"leaves the room">>),
				htmlize(Reason, NoFollow, FileFormat)]);
	     {kickban, <<"301">>, <<"">>} ->
		 io_lib:format("<font class=\"mb\">~s ~s</font><br/>",
			       [Nick, ?T(<<"has been banned">>)]);
	     {kickban, <<"301">>, Reason} ->
		 io_lib:format("<font class=\"mb\">~s ~s: ~s</font><br/>",
			       [Nick, ?T(<<"has been banned">>),
				htmlize(Reason, FileFormat)]);
	     {kickban, <<"307">>, <<"">>} ->
		 io_lib:format("<font class=\"mk\">~s ~s</font><br/>",
			       [Nick, ?T(<<"has been kicked">>)]);
	     {kickban, <<"307">>, Reason} ->
		 io_lib:format("<font class=\"mk\">~s ~s: ~s</font><br/>",
			       [Nick, ?T(<<"has been kicked">>),
				htmlize(Reason, FileFormat)]);
	     {kickban, <<"321">>, <<"">>} ->
		 io_lib:format("<font class=\"mk\">~s ~s</font><br/>",
			       [Nick,
				?T(<<"has been kicked because of an affiliation "
				     "change">>)]);
	     {kickban, <<"322">>, <<"">>} ->
		 io_lib:format("<font class=\"mk\">~s ~s</font><br/>",
			       [Nick,
				?T(<<"has been kicked because the room has "
				     "been changed to members-only">>)]);
	     {kickban, <<"332">>, <<"">>} ->
		 io_lib:format("<font class=\"mk\">~s ~s</font><br/>",
			       [Nick,
				?T(<<"has been kicked because of a system "
				     "shutdown">>)]);
	     {nickchange, OldNick} ->
		 io_lib:format("<font class=\"mnc\">~s ~s ~s</font><br/>",
			       [htmlize(OldNick, FileFormat),
				?T(<<"is now known as">>), Nick]);
	     {subject, T} ->
		 io_lib:format("<font class=\"msc\">~s~s~s</font><br/>",
			       [Nick, ?T(<<" has set the subject to: ">>),
				htmlize(T, NoFollow, FileFormat)]);
	     {body, T} ->
		 case {ejabberd_regexp:run(T, <<"^/me ">>), Nick} of
		   {_, <<"">>} ->
		       io_lib:format("<font class=\"msm\">~s</font><br/>",
				     [htmlize(T, NoFollow, FileFormat)]);
		   {match, _} ->
		       io_lib:format("<font class=\"mne\">~s ~s</font><br/>",
				     [Nick,
				      str:substr(htmlize(T, FileFormat), 5)]);
		   {nomatch, _} ->
		       io_lib:format("<font class=\"mn\">~s</font> ~s<br/>",
				     [Nick2, htmlize(T, NoFollow, FileFormat)])
		 end;
	     {room_existence, RoomNewExistence} ->
		 io_lib:format("<font class=\"mrcm\">~s</font><br/>",
			       [get_room_existence_string(RoomNewExistence,
							  Lang)])
	   end,
    {Hour, Minute, Second} = Time,
    STime = io_lib:format("~2..0w:~2..0w:~2..0w",
                          [Hour, Minute, Second]),
    {_, _, Microsecs} = Now,
    STimeUnique = io_lib:format("~s.~w",
				[STime, Microsecs]),
    catch fw(F,
       list_to_binary(
         io_lib:format("<a id=\"~s\" name=\"~s\" href=\"#~s\" "
                       "class=\"ts\">[~s]</a> ",
                       [STimeUnique, STimeUnique, STimeUnique, STime])
	 ++ Text),
       FileFormat),
    file:close(F),
    ok.

%%----------------------------------------------------------------------
%% Utilities

get_room_existence_string(created, Lang) ->
    ?T(<<"Chatroom is created">>);
get_room_existence_string(destroyed, Lang) ->
    ?T(<<"Chatroom is destroyed">>);
get_room_existence_string(started, Lang) ->
    ?T(<<"Chatroom is started">>);
get_room_existence_string(stopped, Lang) ->
    ?T(<<"Chatroom is stopped">>).

get_dateweek(Date, Lang) ->
    Weekday = case calendar:day_of_the_week(Date) of
		1 -> ?T(<<"Monday">>);
		2 -> ?T(<<"Tuesday">>);
		3 -> ?T(<<"Wednesday">>);
		4 -> ?T(<<"Thursday">>);
		5 -> ?T(<<"Friday">>);
		6 -> ?T(<<"Saturday">>);
		7 -> ?T(<<"Sunday">>)
	      end,
    {Y, M, D} = Date,
    Month = case M of
	      1 -> ?T(<<"January">>);
	      2 -> ?T(<<"February">>);
	      3 -> ?T(<<"March">>);
	      4 -> ?T(<<"April">>);
	      5 -> ?T(<<"May">>);
	      6 -> ?T(<<"June">>);
	      7 -> ?T(<<"July">>);
	      8 -> ?T(<<"August">>);
	      9 -> ?T(<<"September">>);
	      10 -> ?T(<<"October">>);
	      11 -> ?T(<<"November">>);
	      12 -> ?T(<<"December">>)
	    end,
    list_to_binary(
      case Lang of
          <<"en">> ->
              io_lib:format("~s, ~s ~w, ~w", [Weekday, Month, D, Y]);
          <<"es">> ->
              io_lib:format("~s ~w de ~s de ~w",
                            [Weekday, D, Month, Y]);
          _ ->
              io_lib:format("~s, ~w ~s ~w", [Weekday, D, Month, Y])
      end).

make_dir_rec(Dir) ->
    filelib:ensure_dir(<<Dir/binary, $/>>).

%% {ok, F1}=file:open("valid-xhtml10.png", [read]).
%% {ok, F1b}=file:read(F1, 1000000).
%% c("../../ejabberd/src/jlib.erl").
%% misc:encode_base64(F1b).

image_base64(<<"powered-by-erlang.png">>) ->
    <<"iVBORw0KGgoAAAANSUhEUgAAAGUAAAAfCAYAAAD+xQNoA"
      "AADN0lEQVRo3u1aP0waURz+rjGRRQ+nUyRCYmJyDPTapD"
      "ARaSIbTUjt1gVSh8ZW69aBAR0cWLSxCXWp59LR1jbdqKn"
      "GxoQuRZZrSYyHEVM6iZMbHewROA7u3fHvkr5vOn737vcu"
      "33ffu9/vcQz+gef5Cij6CkmSGABgFEH29r5SVvqIsTEOH"
      "o8HkiQxDBXEOjg9PcHc3BxuUSqsI8jR0REAUFGsCCoKFY"
      "WCBAN6AxyO0Z7cyMXFb6oGqSgAsIrJut9hMQlvdNbUhKW"
      "shLd3HtTF4jihShgVpRaBxKKmIGX5HL920/hz/BM2+zAm"
      "pn2YioQaxnECj0BiEYcrG0Tzzc8/rfudSm02jaVSm9Vr1"
      "MdG8rSKKXlJ7lHrfjouCut2IrC82BDPbe/gc+xlXez7Kx"
      "Ez63H4lmIN473Rh8Si1BKhRY6aEJI8pLmbjSPN0xOnBBI"
      "Lmg5RC6Lg28preKOzsNmHG8R1Bf0o7GdMucUslDy1pJLG"
      "2sndVVG0lq3c9vum4zmBR1kuwiYMN5ybmCYXxQg57ThFO"
      "TYznzpPO+IQi+IK+jXjg/YhuIJ+cIIHg+wQJoJ+2N3jYN"
      "3Olvk4ge/IU98spne+FfGtlslm16nna8fduntfDscoVjG"
      "JqUgIjz686ViFUdjP4N39x9Xq638viZVtlq2tLXKncLf5"
      "ticuZSWU5XOUshJKxxKtfdtdvs4OyNb/68urKvlluYizg"
      "wwu5SLK8jllu1t9ihYOlzdwdpBBKSvh+vKKzHkCj1JW3y"
      "1m+hSj13WjqOiJKK0qpXKhSFxJAYBvKYaZ9TjWRu4SiWi"
      "2LyDtb6wghGmn5HfTml16ILGA/G5al2DW7URYTFYrOU7g"
      "icQ020sYqYDM9CbdgqFd4vzHL03JfvLjk6ZgADAVCSEsJ"
      "vHsdL+utNYrm2ufZDVZSkzPKaQkW8kthpyS297BvRdRzR"
      "6DdTurJbPy9Ov1K6xr3HBPQuIMowR3asegUyDuU9SuUG+"
      "dmIGyZ0b7FBN9St3WunyC5yMsrVv7uXzRP58s/qKn6C4q"
      "lQoVxVIvd4YBwzBUFKs6ZaD27U9hEdcAN98Sx2IxykafI"
      "YrizbfESoB+dd9/KF/d/wX3cJvREzl1vAAAAABJRU5Erk"
      "Jggg==">>;
image_base64(<<"valid-xhtml10.png">>) ->
    <<"iVBORw0KGgoAAAANSUhEUgAAAFgAAAAfCAMAAAEjEcpEA"
      "AACiFBMVEUAAADe5+fOezmtra3ejEKlhELvvWO9WlrehE"
      "LOe3vepaWclHvetVLGc3PerVKcCAj3vVqUjHOUe1JjlL0"
      "xOUpjjL2UAAC91ueMrc7vrVKlvdbW3u+EpcbO3ufO1ucY"
      "WpSMKQi9SiF7e3taWkoQEAiMczkQSoxaUkpzc3O1lEoIC"
      "ACEazEhGAgIAACEYzFra2utjELWcznGnEr/7+9jY2POaz"
      "HOYzGta2NShLVrlL05OUqctdacCADGa2ucAADGpVqUtc6"
      "1ORg5OTmlUikYGAiUezl7YzEYEAiUczkxMTG9nEqtIRDe"
      "3t4AMXu9lEoQCACMazEAKXspKSmljFrW1ta1jELOzs7n7"
      "/fGxsa9pVqEOSkpY5xznL29tZxahLXOpVr/99ZrY1L/79"
      "ZjUiljSikAOYTvxmMAMYScezmchFqUczGtlFp7c2utjFq"
      "UlJStxt73///39/9Ce61CSkq9xsZznMbW5+9Cc62MjIxC"
      "Qkrv9/fv7/fOzsbnlErWjIz/3mtCORhza1IpIRBzWjH/1"
      "mtCMRhzY1L/zmvnvVpSQiHOpVJrUinntVr3zmOEc1L3xm"
      "NaWlq1nFo5QkrGWim1lFoISpRSUlK1zt4hWpwASoz////"
      "///8xa6WUaykAQoxKe61KSkp7nMbWtWPe5+9jWlL39/f3"
      "9/fWrWNCQkLera3nvWPv7+85MRjntWPetVp7c1IxKRCUl"
      "HtKORh7a1IxIRCUjHtaSiHWrVIpIQhzWinvvVpaQiH/1m"
      "PWpVKMe1L/zmP/xmNrUiGErc4YGBj/73PG1ucQWpT/53O"
      "9nFoQUpS1SiEQEBC9zt69vb05c6UISoxSUko5a6UICAhS"
      "SkohUpS1tbXetWMAQoSUgD+kAAAA2HRSTlP/////////i"
      "P9sSf//dP////////////////////////////////////"
      "////////////8M////////////ef/////////////////"
      "/////////////////////////////////////////////"
      "//////////////////////9d/////////////////////"
      "///////////////AP//////////////CP//RP////////"
      "/////////////////////////////////////////////"
      "///////9xPp1gAAAFvUlEQVR42pVWi18URRwfy7vsYUba"
      "iqBRBFmICUQGVKcZckQeaRJQUCLeycMSfKGH0uo5NELpI"
      "vGQGzokvTTA85VHKTpbRoeJnPno/p1+M7t3txj20e/Nzu"
      "7Ofve7v/k9Zg4Vc+wRQMW0eyLx1ZSANeBDxVmxZZSwEUY"
      "kGAewm1eIBOMRvhv1UA+q8KXIVuxGdCelFYwxAnxOrxgb"
      "Y8Ti1t4VA0QHYz4x3FnVC8OVLXv9fkKGSWDoW/4lG6Vbd"
      "tBblesOs+MjmEmzJKNIJWFEfEQTCWNPFKvcKEymjLO1b8"
      "bwYQd1hCiiDCl5KsrDCIlhj4fSuvcpfSpgJmyv6dzeZv+"
      "nMPx3dhbt94II07/JZliEtm1N2RIYPkTYshwYm245a/zk"
      "WjJwcyFh6ZIcYxxmqiaDSYxhOhFUsqngi3Fzcj3ljdYDN"
      "E9uzA1YD/5MhnzW1KRqF7mYG8jFYXLcfLpjOe2LA0fuGq"
      "QrQHl10sdK0sFcFSOSlzF0BgXQH9h3QZDBI0ccNEhftjX"
      "uippBDD2/eMRiETmwwNEYHyqhdDyo22w+3QHuNbdve5a7"
      "eOkHmDVJ0ixNmfbz1h0qo/Q6GuSB2wQJQbpOjOQAl7woW"
      "SRJ0m2ewhvAOUiYYtZtaZL0CZZmtmVOQttLfr/dbveLZo"
      "drfrL7W75wG/JjqkQxoNTtNsTKELQpQL6/D5loaSmyTT8"
      "TUhsmi8iFA0hZiyltf7OiNKdarRm5w2So2lTNdPLuIzR+"
      "AiLj8VTRJaj0LmX4VhJ27f/VJV/yycilWPOrk8NkXi7Qq"
      "mj5bHqVZlJKZIRk1wFzKrt0WUbnXMPJ1fk4TJ5oWBA61p"
      "1V76DeIs0MX+s3GxRlA1vtw83KhgNphc1nyErLO5zcvbO"
      "srq+scbZnpzc6QVFPenLwGxmC+BOfYI+DN55QYddh4Q/N"
      "E/yGYYj4TOGNngQavAZnzzTovEA+kcMJ+247uYexNA+4F"
      "svjmuv662jsWxPZx2xg890bYMYnTgya7bjmCiEY0qgJ0v"
      "MF3c+NoFdPyzxz6V3Uxs3AOWCDchRvOsQtBrbFsrT2fhH"
      "Ec7ByGzu/dA4IO0A3HdfeP9yMqAwP6NPEb6cbwn0PWVU1"
      "7/FDBQh/CPIrbfcg027IZrsAT/Bf3FNWyn9RSR4cvvwn3"
      "e4HFmYPDl/thYcRVi8qPEoXVUWBl6FTBFTtnqmKKg5wnl"
      "F4wZ1yeLv7TiwXKektE+iDBNicWEyLpnFhfDkpJc3q2kh"
      "SPyQBbE0dMJnOoDzTwGsI7cdyMkL5gWqUjCF6Txst/twx"
      "Cv1WzzHoy21ZDQ1xnuDzdPDWR4knr14v0tYn3IxaMFFdi"
      "MOlEOJHw1jOQ4sWt5rQopRkXZhMEi7pmeDCVWBlfUKwhM"
      "Z7rsF6elKsvbwiKxgxIdewa3ErsaYomCVZFYJb0GUu3Jq"
      "GUNoplBxYiYby8vLBFWef+Cri4/I1sbQ/1OtYTrNtdXS+"
      "rSe7kQ52eSObL99/iErCWUjCy5W4JLygmCouGfG9x9fmx"
      "17XhBuDCaOerbt538erta7TFktLvdHghZcCbcPQO33zIJ"
      "G9kxF5hoVXnzTzRz0r5js8oTj6uyPkGRf346HOLcasgFe"
      "xueNUWFPtuFKzjoSFYYedhwVlhsRVYWWJpltv1XPQT1Rl"
      "0bjZIBlb1XujVDzY/Kj4k6Ku3+Z0jo1owjVzDpFTXe1ju"
      "vBSWNFmNWGZy8LvzUl5PN4JCwyNDzbQ0aAj4Zrjz0FatG"
      "JJYhvq4j7mGSpvytGFlZtHf2C4o/28Zu8z7wo7eYPfXys"
      "nF0i9NnPh1t1zR7VBb9GqaOXhtTmHQdgMFXE+Z608cnpO"
      "DdZdjL+TuDY44Q38kJXHhccWLoOd9uv1AwwvO+48uu+fa"
      "CSJPJ1bmy6ThyvpivBmYWgjxPDPAp7JTemY/yGKFEiRt/"
      "jG/2P79s8KCwoLCgoLC/khUBA5F0SfQZ+RYfpNE/4Xosm"
      "q7jsZAJsAAAAASUVORK5CYII=">>;
image_base64(<<"vcss.png">>) ->
    <<"iVBORw0KGgoAAAANSUhEUgAAAFgAAAAfCAMAAABUFvrSA"
      "AABKVBMVEUAAAAjIx8MR51ZVUqAdlmdnZ3ejEWLDAuNjY"
      "1kiMG0n2d9fX19Ghfrp1FtbW3y39+3Ph6lIRNdXV2qJBF"
      "cVUhcVUhPT0/dsmpUfLr57+/u7u4/PDWZAACZAADOp1Gd"
      "GxG+SyTgvnNdSySzk16+mkuxw+BOS0BOS0DOzs7MzMy4T"
      "09RRDwsJBG+vr73wV6fkG6eCQRFcLSurq6/X1+ht9nXfz"
      "5sepHuwV59ZTHetFjQ2+wMCQQ2ZK5tWCsmWajsz8+Sq9N"
      "MPh4hVaY8MRj///////////////////////9MTEyOp9Lu"
      "8vhXU1A8PDyjOSTBz+YLRJ2rLy8sLCwXTaKujEUcHByDn"
      "82dfz7/zGafDw+fDw+zRSlzlMcMDAyNcji1tbXf5vIcFg"
      "vATJOjAAAAY3RSTlP/8/////////////////8A//////P"
      "/////ov//8//////////////z///T//////////+i////"
      "//////////8w/////6IA/xAgMP//////////8////////"
      "/8w0/////////+zehebAAACkUlEQVR42u2VfVPTQBDG19"
      "VqC6LY+lKrRIxFQaFSBPuSvhBPF8SIUZK2J5Yav/+HcO8"
      "uZdLqTCsU/nKnyWwvk1/unnt2D9ZmH+8/cMAaTRFy+ng6"
      "9/yiwC/+gy8R3McGv5zHvGJEGAdR4eBgi1IbZwevIEZE2"
      "4pFtBtzG1Q4AoD5zvw5pEDcJvIQV/TE3/l+H9GnNJwcdA"
      "BS5wAbFQLMqI98/UReoAaOTlaJsp0zaHx7LwZvY0BUR2x"
      "pWTzqam0gzY8KGzG4MhBCNGucha4QbpETy+Yk/BP85nt7"
      "34AjpQLTsE4ZFpf/dnkUCglXVNYB+OfUZJHvAqAoa45Oe"
      "uPgm4+Xjtv7xm4N7PMV4C61+Mrz3H2WImm3ATiWrAiwZR"
      "WcUA5Ej4dgIEMxDv6yxHHcNuAutnjv2HZ1NeuycoVPh0m"
      "wC834zZC9Ao5dkZZKwLVGwT+WdLw0YOZ1saEkUDoT+QGW"
      "KZ0E2xpcrPakVW2KXwyUtYEtlEAj3GXD/fYwrryAdeiyG"
      "qidQSw1eqtJcA8cZq4zXqhPuCBYE1fKJjh/5X6MwRm9c2"
      "xf7WVdLf5oSdt64esVIwVAKC1HJ2oli8vj3L0YzC4zjkM"
      "agt+arDAs6bApbL1RVlWIqrJbreqKZmh4y6VR7rAJeUYD"
      "VRj9VqRXkErpJ9lbEwtE83KlIfeG4p52t7zWIMO1XcaGz"
      "54uUyet+hBM7BXXDS8Xc5+8Gmmbu1xwSoGIokA3oTptQe"
      "cQ4Iimm/Ew7jwbPfMi3TM91T9XVIGo+W9xC8oWpugVCXL"
      "uwXijjxJ3r/6PjX7nlFua8QmyM+TO/Gja2TTc2Z95C5ua"
      "ewGH6cJi6bJO6Z+TY276eH3tbgy+/3ly3Js+rj66osG/A"
      "V5htgaQ9SeRAAAAAElFTkSuQmCC">>;
image_base64(<<"powered-by-ejabberd.png">>) ->
    <<"iVBORw0KGgoAAAANSUhEUgAAAGUAAAAfCAMAAADJG/NaA"
      "AAAw1BMVEUAAAAjBgYtBAM5AwFCAAAYGAJNAABcAABIDQ"
      "5qAAAoJRV7AACFAAAoKSdJHByLAAAwLwk1NQA1MzFJKyo"
      "4NxtDQQBEQT5KSCxSTgBSUBlgQ0JYSEpZWQJPUU5hYABb"
      "W0ZiYClcW1poaCVwbQRpaDhzYWNsakhuZ2VrbFZ8dwCEg"
      "AB3dnd4d2+OjACDhYKcmACJi4iQkpWspgCYmJm5swCmqa"
      "zEwACwsbS4ub3X0QLExsPLyszW1Nnc3ODm5ugMBwAWAwP"
      "Hm1IFAAAAAXRSTlMAQObYZgAAAAFiS0dEAIgFHUgAAAAJ"
      "cEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfVCRQOBA7VB"
      "kCMAAACcElEQVRIx72WjXKiMBSFQalIFbNiy1pdrJZaRV"
      "YR5deGwPs/VRNBSBB2OjvQO0oYjPfj5J6bCcdx8i2Uldx"
      "KcDhk1HbIPwFBF/kHKJfjPSVAyIRHF9rRZ4sUX3EDdWOv"
      "1+u2tESaavpnYTbv9zvd0WwDy3/QcGQXlH5uTxB1l07MJ"
      "lRpsUei0JF6Qi+OHyGK7ijXxPklHe/umIllim3iUBMJDI"
      "EULxxPP0TVWhhKJoN9fUpdmQLteV8aDgEAg9gIcTjL4F4"
      "L+r4WVKEF+rbJdwYYAoQHY+oQjnGootyKwxapoi73WkyF"
      "FySQBv988naEEp4+YMMec5VUCQDJTscEy7Kc0HsLmqNE7"
      "rovDjMpIHHGYeidXn4TQcaxMYqP3RV3C8oCl2WvrlSPaN"
      "pGZadRnmPGCk8ylM2okAJ4i9TEe1KersXxSl6jUt5uayi"
      "IodirtcKLOaWblj50wiyMv1F9lm9TUDArGAD0FmEpvCUs"
      "VoZy6dW81Fg0aDaHogQa36ekAPG5DDGsbdZrGsrzZUnzv"
      "Bo1I2tLmuL69kSitAweyHKN9b3leDfQMnu3nIIKWfmXnq"
      "GVKedJT6QpICbJvf2f8aOsvn68v+k7/cwUQdPoxaMoRTn"
      "KFHNlKsKQphCTOa84u64vpi8bH31CqsbF6lSONRTkTyQG"
      "Arq49/fEvjBwz4eDS2/JpaXRNOoXRD/VmOrDVTJJRIZCT"
      "Lav3VrqbPvP3vdduGEhQJzilncbpSA4F3vsihErO+dayv"
      "/sY5/yRE0GDEXCu2VoNiMlo5i+P2KlgMEvTNk2eYa5XEy"
      "h12Ex17Z8vzQUR3KEPbYd6XG87eC4Ly75RneS5ZYHAAAA"
      "AElFTkSuQmCC">>.

create_image_files(Images_dir) ->
    Filenames = [<<"powered-by-ejabberd.png">>,
		 <<"powered-by-erlang.png">>, <<"valid-xhtml10.png">>,
		 <<"vcss.png">>],
    lists:foreach(fun (Filename) ->
			  Filename_full = fjoin([Images_dir, Filename]),
			  {ok, F} = file:open(Filename_full, [write]),
			  Image = misc:decode_base64(image_base64(Filename)),
			  io:format(F, <<"~s">>, [Image]),
			  file:close(F)
		  end,
		  Filenames),
    ok.

fw(F, S) -> fw(F, S, [], html).

fw(F, S, O) when is_list(O) -> fw(F, S, O, html);
fw(F, S, FileFormat) when is_atom(FileFormat) ->
    fw(F, S, [], FileFormat).

fw(F, S, O, FileFormat) ->
    S1 = (str:format(binary_to_list(S) ++ "~n", O)),
    S2 = case FileFormat of
	     html ->
		 S1;
	     plaintext ->
		 S1a = ejabberd_regexp:greplace(S1, <<"<[^<^>]*>">>, <<"">>),
		 S1x = ejabberd_regexp:greplace(S1a, ?PLAINTEXT_CO, <<"~~">>),
		 S1y = ejabberd_regexp:greplace(S1x, ?PLAINTEXT_IN, <<"<">>),
		 ejabberd_regexp:greplace(S1y, ?PLAINTEXT_OUT, <<">">>)
	 end,
    file:write(F, S2).

put_header(_, _, _, _, _, _, _, _, _, plaintext) -> ok;
put_header(F, Room, Date, CSSFile, Lang, Hour_offset,
	   Date_prev, Date_next, Top_link, FileFormat) ->
    fw(F,
       <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD "
	 "XHTML 1.0 Transitional//EN\" \"http://www.w3."
	 "org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">">>),
    fw(F,
       <<"<html xmlns=\"http://www.w3.org/1999/xhtml\" "
	 "xml:lang=\"~s\" lang=\"~s\">">>,
       [Lang, Lang]),
    fw(F, <<"<head>">>),
    fw(F,
       <<"<meta http-equiv=\"Content-Type\" content=\"t"
	 "ext/html; charset=utf-8\" />">>),
    fw(F, <<"<title>~s - ~s</title>">>,
       [htmlize(Room#room.title), Date]),
    put_header_css(F, CSSFile),
    put_header_script(F),
    fw(F, <<"</head>">>),
    fw(F, <<"<body>">>),
    {Top_url, Top_text} = Top_link,
    fw(F,
       <<"<div style=\"text-align: right;\"><a "
	 "style=\"color: #AAAAAA; font-family: "
	 "monospace; text-decoration: none; font-weight"
	 ": bold;\" href=\"~s\">~s</a></div>">>,
       [Top_url, Top_text]),
    fw(F, <<"<div class=\"roomtitle\">~s</div>">>,
       [htmlize(Room#room.title)]),
    fw(F,
       <<"<a class=\"roomjid\" href=\"xmpp:~s?join\">~s"
	 "</a>">>,
       [Room#room.jid, Room#room.jid]),
    fw(F,
       <<"<div class=\"logdate\">~s<span class=\"w3c\">"
	 "<a class=\"nav\" href=\"~s\">&lt;</a> "
	 "<a class=\"nav\" href=\"./\">^</a> <a "
	 "class=\"nav\" href=\"~s\">&gt;</a></span></di"
	 "v>">>,
       [Date, Date_prev, Date_next]),
    case {htmlize(Room#room.subject_author),
	  htmlize(Room#room.subject)}
	of
      {<<"">>, <<"">>} -> ok;
      {SuA, Su} ->
	  fw(F, <<"<div class=\"roomsubject\">~s~s~s</div>">>,
	     [SuA, ?T(<<" has set the subject to: ">>), Su])
    end,
    RoomConfig = roomconfig_to_string(Room#room.config,
				      Lang, FileFormat),
    put_room_config(F, RoomConfig, Lang, FileFormat),
    Occupants = get_room_occupants(Room#room.jid),
    RoomOccupants = roomoccupants_to_string(Occupants,
					    FileFormat),
    put_room_occupants(F, RoomOccupants, Lang, FileFormat),
    Time_offset_str = case Hour_offset < 0 of
			true -> io_lib:format("~p", [Hour_offset]);
			false -> io_lib:format("+~p", [Hour_offset])
		      end,
    fw(F, <<"<br/><a class=\"ts\">GMT~s</a><br/>">>,
       [Time_offset_str]).

put_header_css(F, false) ->
    fw(F, <<"<style type=\"text/css\">">>),
    fw(F, <<"<!--">>),
    fw(F,
       <<".ts {color: #AAAAAA; text-decoration: "
	 "none;}">>),
    fw(F,
       <<".mrcm {color: #009900; font-style: italic; "
	 "font-weight: bold;}">>),
    fw(F,
       <<".msc {color: #009900; font-style: italic; "
	 "font-weight: bold;}">>),
    fw(F,
       <<".msm {color: #000099; font-style: italic; "
	 "font-weight: bold;}">>),
    fw(F, <<".mj {color: #009900; font-style: italic;}">>),
    fw(F, <<".ml {color: #009900; font-style: italic;}">>),
    fw(F, <<".mk {color: #009900; font-style: italic;}">>),
    fw(F, <<".mb {color: #009900; font-style: italic;}">>),
    fw(F, <<".mnc {color: #009900; font-style: italic;}">>),
    fw(F, <<".mn {color: #0000AA;}">>),
    fw(F, <<".mne {color: #AA0099;}">>),
    fw(F,
       <<"a.nav {color: #AAAAAA; font-family: "
	 "monospace; letter-spacing: 3px; text-decorati"
	 "on: none;}">>),
    fw(F,
       <<"div.roomtitle {border-bottom: #224466 "
	 "solid 3pt; margin-left: 20pt;}">>),
    fw(F,
       <<"div.roomtitle {color: #336699; font-size: "
	 "24px; font-weight: bold; font-family: "
	 "sans-serif; letter-spacing: 3px; text-decorat"
	 "ion: none;}">>),
    fw(F,
       <<"a.roomjid {color: #336699; font-size: "
	 "24px; font-weight: bold; font-family: "
	 "sans-serif; letter-spacing: 3px; margin-left: "
	 "20pt; text-decoration: none;}">>),
    fw(F,
       <<"div.logdate {color: #663399; font-size: "
	 "20px; font-weight: bold; font-family: "
	 "sans-serif; letter-spacing: 2px; border-botto"
	 "m: #224466 solid 1pt; margin-left:80pt; "
	 "margin-top:20px;}">>),
    fw(F,
       <<"div.roomsubject {color: #336699; font-size: "
	 "18px; font-family: sans-serif; margin-left: "
	 "80pt; margin-bottom: 10px;}">>),
    fw(F,
       <<"div.rc {color: #336699; font-size: 12px; "
	 "font-family: sans-serif; margin-left: "
	 "50%; text-align: right; background: "
	 "#f3f6f9; border-bottom: 1px solid #336699; "
	 "border-right: 4px solid #336699;}">>),
    fw(F,
       <<"div.rct {font-weight: bold; background: "
	 "#e3e6e9; padding-right: 10px;}">>),
    fw(F, <<"div.rcos {padding-right: 10px;}">>),
    fw(F, <<"div.rcoe {color: green;}">>),
    fw(F, <<"div.rcod {color: red;}">>),
    fw(F, <<"div.rcoe:after {content: \": v\";}">>),
    fw(F, <<"div.rcod:after {content: \": x\";}">>),
    fw(F, <<"div.rcot:after {}">>),
    fw(F,
       <<".legend {width: 100%; margin-top: 30px; "
	 "border-top: #224466 solid 1pt;  padding: "
	 "10px 0px 10px 0px; text-align: left; "
	 "font-family: monospace; letter-spacing: "
	 "2px;}">>),
    fw(F,
       <<".w3c {position: absolute; right: 10px; "
	 "width: 60%; text-align: right; font-family: "
	 "monospace; letter-spacing: 1px;}">>),
    fw(F, <<"//-->">>),
    fw(F, <<"</style>">>);
put_header_css(F, CSSFile) ->
    fw(F,
       <<"<link rel=\"stylesheet\" type=\"text/css\" "
	 "href=\"~s\" media=\"all\">">>,
       [CSSFile]).

put_header_script(F) ->
    fw(F, <<"<script type=\"text/javascript\">">>),
    fw(F, <<"function sh(e) // Show/Hide an element">>),
    fw(F,
       <<"{if(document.getElementById(e).style.display="
	 "='none')">>),
    fw(F,
       <<"{document.getElementById(e).style.display='bl"
	 "ock';}">>),
    fw(F,
       <<"else {document.getElementById(e).style.displa"
	 "y='none';}}">>),
    fw(F, <<"</script>">>).

put_room_config(_F, _RoomConfig, _Lang, plaintext) ->
    ok;
put_room_config(F, RoomConfig, Lang, _FileFormat) ->
    {_, Now2, _} = p1_time_compat:timestamp(),
    fw(F, <<"<div class=\"rc\">">>),
    fw(F,
       <<"<div class=\"rct\" onclick=\"sh('a~p');return "
	 "false;\">~s</div>">>,
       [Now2, ?T(<<"Room Configuration">>)]),
    fw(F,
       <<"<div class=\"rcos\" id=\"a~p\" style=\"displa"
	 "y: none;\" ><br/>~s</div>">>,
       [Now2, RoomConfig]),
    fw(F, <<"</div>">>).

put_room_occupants(_F, _RoomOccupants, _Lang,
		   plaintext) ->
    ok;
put_room_occupants(F, RoomOccupants, Lang,
		   _FileFormat) ->
    {_, Now2, _} = p1_time_compat:timestamp(),
%% htmlize
%% The default behaviour is to ignore the nofollow spam prevention on links
%% (NoFollow=false)
    fw(F, <<"<div class=\"rc\">">>),
    fw(F,
       <<"<div class=\"rct\" onclick=\"sh('o~p');return "
	 "false;\">~s</div>">>,
       [Now2, ?T(<<"Room Occupants">>)]),
    fw(F,
       <<"<div class=\"rcos\" id=\"o~p\" style=\"displa"
	 "y: none;\" ><br/>~s</div>">>,
       [Now2, RoomOccupants]),
    fw(F, <<"</div>">>).

htmlize(S1) -> htmlize(S1, html).

htmlize(S1, plaintext) ->
    ejabberd_regexp:greplace(S1, <<"~">>, ?PLAINTEXT_CO);
htmlize(S1, FileFormat) ->
    htmlize(S1, false, FileFormat).

%% The NoFollow parameter tell if the spam prevention should be applied to the link found
%% true means 'apply nofollow on links'.
htmlize(S0, _NoFollow, plaintext) ->
    S1  = ejabberd_regexp:greplace(S0, <<"~">>, ?PLAINTEXT_CO),
    S1x = ejabberd_regexp:greplace(S1, <<"<">>, ?PLAINTEXT_IN),
    ejabberd_regexp:greplace(S1x, <<">">>, ?PLAINTEXT_OUT);
htmlize(S1, NoFollow, _FileFormat) ->
    S2_list = str:tokens(S1, <<"\n">>),
    lists:foldl(fun (Si, Res) ->
			Si2 = htmlize2(Si, NoFollow),
			case Res of
			  <<"">> -> Si2;
			  _ -> <<Res/binary, "<br/>", Si2/binary>>
			end
		end,
		<<"">>, S2_list).

htmlize2(S1, NoFollow) ->
%% Regexp link
%% Add the nofollow rel attribute when required
    S2 = ejabberd_regexp:greplace(S1, <<"\\&">>,
				  <<"\\&amp;">>),
    S3 = ejabberd_regexp:greplace(S2, <<"<">>,
				  <<"\\&lt;">>),
    S4 = ejabberd_regexp:greplace(S3, <<">">>,
				  <<"\\&gt;">>),
    S5 = ejabberd_regexp:greplace(S4,
				  <<"((http|https|ftp)://|(mailto|xmpp):)[^] "
				    ")'\"}]+">>,
				  link_regexp(NoFollow)),
    S6 = ejabberd_regexp:greplace(S5, <<"  ">>,
				  <<"\\&nbsp;\\&nbsp;">>),
    S7 = ejabberd_regexp:greplace(S6, <<"\\t">>,
				  <<"\\&nbsp;\\&nbsp;\\&nbsp;\\&nbsp;">>),
    S8 = ejabberd_regexp:greplace(S7, <<"~">>,
				  <<"~~">>),
    ejabberd_regexp:greplace(S8, <<226, 128, 174>>,
			     <<"[RLO]">>).

link_regexp(false) -> <<"<a href=\"&\">&</a>">>;
link_regexp(true) ->
    <<"<a href=\"&\" rel=\"nofollow\">&</a>">>.

get_room_info(RoomJID, Opts) ->
    Title = case lists:keysearch(title, 1, Opts) of
	      {value, {_, T}} -> T;
	      false -> <<"">>
	    end,
    Subject = case lists:keysearch(subject, 1, Opts) of
		{value, {_, S}} -> S;
		false -> <<"">>
	      end,
    SubjectAuthor = case lists:keysearch(subject_author, 1,
					 Opts)
			of
		      {value, {_, SA}} -> SA;
		      false -> <<"">>
		    end,
    #room{jid = jid:encode(RoomJID), title = Title,
	  subject = Subject, subject_author = SubjectAuthor,
	  config = Opts}.

roomconfig_to_string(Options, Lang, FileFormat) ->
    Title = case lists:keysearch(title, 1, Options) of
	      {value, Tuple} -> [Tuple];
	      false -> []
	    end,
    Os1 = lists:keydelete(title, 1, Options),
    Os2 = lists:sort(Os1),
    Options2 = Title ++ Os2,
    lists:foldl(fun ({Opt, Val}, R) ->
			case get_roomconfig_text(Opt) of
			  undefined -> R;
			  OptT ->
			      OptText = (?T(OptT)),
			      R2 = case Val of
				     false ->
					 <<"<div class=\"rcod\">",
					   OptText/binary, "</div>">>;
				     true ->
					 <<"<div class=\"rcoe\">",
					   OptText/binary, "</div>">>;
				     <<"">> ->
					 <<"<div class=\"rcod\">",
					   OptText/binary, "</div>">>;
				     T ->
					 case Opt of
					   password ->
					       <<"<div class=\"rcoe\">",
						 OptText/binary, "</div>">>;
					   max_users ->
					       <<"<div class=\"rcot\">",
						 OptText/binary, ": \"",
						 (htmlize(integer_to_binary(T),
							  FileFormat))/binary,
						 "\"</div>">>;
					   title ->
					       <<"<div class=\"rcot\">",
						 OptText/binary, ": \"",
						 (htmlize(T,
							  FileFormat))/binary,
						 "\"</div>">>;
					   description ->
					       <<"<div class=\"rcot\">",
						 OptText/binary, ": \"",
						 (htmlize(T,
							  FileFormat))/binary,
						 "\"</div>">>;
					   allow_private_messages_from_visitors ->
					       <<"<div class=\"rcot\">",
						 OptText/binary, ": \"",
						 (htmlize(?T(misc:atom_to_binary(T)),
							  FileFormat))/binary,
						 "\"</div>">>;
					   _ -> <<"\"", T/binary, "\"">>
					 end
				   end,
			      <<R/binary, R2/binary>>
			end
		end,
		<<"">>, Options2).

get_roomconfig_text(title) -> <<"Room title">>;
get_roomconfig_text(persistent) ->
    <<"Make room persistent">>;
get_roomconfig_text(public) ->
    <<"Make room public searchable">>;
get_roomconfig_text(public_list) ->
    <<"Make participants list public">>;
get_roomconfig_text(password_protected) ->
    <<"Make room password protected">>;
get_roomconfig_text(password) -> <<"Password">>;
get_roomconfig_text(anonymous) ->
    <<"This room is not anonymous">>;
get_roomconfig_text(members_only) ->
    <<"Make room members-only">>;
get_roomconfig_text(moderated) ->
    <<"Make room moderated">>;
get_roomconfig_text(members_by_default) ->
    <<"Default users as participants">>;
get_roomconfig_text(allow_change_subj) ->
    <<"Allow users to change the subject">>;
get_roomconfig_text(allow_private_messages) ->
    <<"Allow users to send private messages">>;
get_roomconfig_text(allow_private_messages_from_visitors) ->
    <<"Allow visitors to send private messages to">>;
get_roomconfig_text(allow_query_users) ->
    <<"Allow users to query other users">>;
get_roomconfig_text(allow_user_invites) ->
    <<"Allow users to send invites">>;
get_roomconfig_text(logging) -> <<"Enable logging">>;
get_roomconfig_text(allow_visitor_nickchange) ->
    <<"Allow visitors to change nickname">>;
get_roomconfig_text(allow_visitor_status) ->
    <<"Allow visitors to send status text in "
      "presence updates">>;
get_roomconfig_text(captcha_protected) ->
    <<"Make room captcha protected">>;
get_roomconfig_text(description) ->
    <<"Room description">>;
%% get_roomconfig_text(subject) ->  "Subject";
%% get_roomconfig_text(subject_author) ->  "Subject author";
get_roomconfig_text(max_users) ->
    <<"Maximum Number of Occupants">>;
get_roomconfig_text(_) -> undefined.

%% Users = [{JID, Nick, Role}]
roomoccupants_to_string(Users, _FileFormat) ->
    Res = [role_users_to_string(RoleS, Users1)
	   || {RoleS, Users1} <- group_by_role(Users),
	      Users1 /= []],
    iolist_to_binary([<<"<div class=\"rcot\">">>, Res, <<"</div>">>]).

group_by_role(Users) ->
    {Ms, Ps, Vs, Ns} = lists:foldl(fun ({JID, Nick,
					 moderator},
					{Mod, Par, Vis, Non}) ->
					   {[{JID, Nick}] ++ Mod, Par, Vis,
					    Non};
				       ({JID, Nick, participant},
					{Mod, Par, Vis, Non}) ->
					   {Mod, [{JID, Nick}] ++ Par, Vis,
					    Non};
				       ({JID, Nick, visitor},
					{Mod, Par, Vis, Non}) ->
					   {Mod, Par, [{JID, Nick}] ++ Vis,
					    Non};
				       ({JID, Nick, none},
					{Mod, Par, Vis, Non}) ->
					   {Mod, Par, Vis, [{JID, Nick}] ++ Non}
				   end,
				   {[], [], [], []}, Users),
    case Ms of
      [] -> [];
      _ -> [{<<"Moderator">>, Ms}]
    end
      ++
      case Ms of
	[] -> [];
	_ -> [{<<"Participant">>, Ps}]
      end
	++
	case Ms of
	  [] -> [];
	  _ -> [{<<"Visitor">>, Vs}]
	end
	  ++
	  case Ms of
	    [] -> [];
	    _ -> [{<<"None">>, Ns}]
	  end.

role_users_to_string(RoleS, Users) ->
    SortedUsers = lists:keysort(2, Users),
    UsersString = << <<Nick/binary, "<br/>">>
		   || {_JID, Nick} <- SortedUsers >>,
    <<RoleS/binary, ": ", UsersString/binary>>.

get_room_occupants(RoomJIDString) ->
    RoomJID = jid:decode(RoomJIDString),
    RoomName = RoomJID#jid.luser,
    MucService = RoomJID#jid.lserver,
    StateData = get_room_state(RoomName, MucService),
    [{U#user.jid, U#user.nick, U#user.role}
     || {_, U} <- (?DICT):to_list(StateData#state.users)].

-spec get_room_state(binary(), binary()) -> mod_muc_room:state().

get_room_state(RoomName, MucService) ->
    case mod_muc:find_online_room(RoomName, MucService) of
	{ok, RoomPid} ->
	  get_room_state(RoomPid);
	error ->
	    #state{}
    end.

-spec get_room_state(pid()) -> mod_muc_room:state().

get_room_state(RoomPid) ->
    {ok, R} = gen_fsm:sync_send_all_state_event(RoomPid,
						get_state),
    R.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

calc_hour_offset(TimeHere) ->
    TimeZero = calendar:universal_time(),
    TimeHereHour =
	calendar:datetime_to_gregorian_seconds(TimeHere) div
	  3600,
    TimeZeroHour =
	calendar:datetime_to_gregorian_seconds(TimeZero) div
	  3600,
    TimeHereHour - TimeZeroHour.

fjoin(FileList) ->
    list_to_binary(filename:join([binary_to_list(File) || File <- FileList])).

has_no_permanent_store_hint(Packet) ->
    xmpp:has_subtag(Packet, #hint{type = 'no-store'}) orelse
    xmpp:has_subtag(Packet, #hint{type = 'no-storage'}) orelse
    xmpp:has_subtag(Packet, #hint{type = 'no-permanent-store'}) orelse
    xmpp:has_subtag(Packet, #hint{type = 'no-permanent-storage'}).

mod_opt_type(access_log) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(cssfile) -> fun iolist_to_binary/1;
mod_opt_type(dirname) ->
    fun (room_jid) -> room_jid;
	(room_name) -> room_name
    end;
mod_opt_type(dirtype) ->
    fun (subdirs) -> subdirs;
	(plain) -> plain
    end;
mod_opt_type(file_format) ->
    fun (html) -> html;
	(plaintext) -> plaintext
    end;
mod_opt_type(file_permissions) ->
    fun (SubOpts) ->
	    F = fun ({mode, Mode}, {_M, G}) -> {Mode, G};
		    ({group, Group}, {M, _G}) -> {M, Group}
		end,
	    lists:foldl(F, {644, 33}, SubOpts)
    end;
mod_opt_type(outdir) -> fun iolist_to_binary/1;
mod_opt_type(spam_prevention) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(timezone) ->
    fun (local) -> local;
	(universal) -> universal
    end;
mod_opt_type(top_link) ->
    fun ([{S1, S2}]) ->
	    {iolist_to_binary(S1), iolist_to_binary(S2)}
    end;
mod_opt_type(_) ->
    [access_log, cssfile, dirname, dirtype, file_format,
     file_permissions, outdir, spam_prevention, timezone,
     top_link].

opt_type(language) -> fun iolist_to_binary/1;
opt_type(_) -> [language].
