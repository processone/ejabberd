%%%----------------------------------------------------------------------
%%% File    : ejabberd_piefxis.erl
%%% Author  : Pablo Polvorin, Vidal Santiago Martinez, Evgeniy Khramtsov
%%% Purpose : XEP-0227: Portable Import/Export Format for XMPP-IM Servers
%%% Created : 17 Jul 2008 by Pablo Polvorin <pablo.polvorin@process-one.net>
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

%%% Not implemented:
%%% - write mod_piefxis with ejabberdctl commands
%%% - Export from mod_offline_sql.erl
%%% - Export from mod_private_sql.erl
%%% - XEP-227: 6. Security Considerations
%%% - Other schemas of XInclude are not tested, and may not be imported correctly.
%%% - If a host has many users, split that host in XML files with 50 users each.
%%%% Headers

-module(ejabberd_piefxis).

-behaviour(ejabberd_config).

-protocol({xep, 227, '1.0'}).

-export([import_file/1, export_server/1, export_host/2,
	 opt_type/1]).

-define(CHUNK_SIZE, 1024*20). %20k

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include("mod_privacy.hrl").
-include("mod_roster.hrl").

%%-include_lib("exmpp/include/exmpp.hrl").
%%-include_lib("exmpp/include/exmpp_client.hrl").
%% Copied from exmpp header files:
%% Copied from mod_private.erl
%%-define(ERROR_MSG(M,Args),io:format(M,Args)).
%%-define(INFO_MSG(M,Args),ok).
%%%==================================
%%%% Import file
-define(NS_PIE, <<"urn:xmpp:pie:0">>).
-define(NS_PIEFXIS, <<"http://www.xmpp.org/extensions/xep-0227.html#ns">>).
-define(NS_XI, <<"http://www.w3.org/2001/XInclude">>).

-record(state, {xml_stream_state :: fxml_stream:xml_stream_state() | undefined,
                user = <<"">>    :: binary(),
                server = <<"">>  :: binary(),
                fd = self()      :: file:io_device(),
                dir = <<"">>     :: binary()}).

-type state() :: #state{}.

%%File could be large.. we read it in chunks
%%%===================================================================
%%% API
%%%===================================================================
import_file(FileName) ->
    import_file(FileName, #state{}).

-spec import_file(binary(), state()) -> ok | {error, atom()}.
import_file(FileName, State) ->
    case file:open(FileName, [read, binary]) of
	{ok, Fd} ->
            Dir = filename:dirname(FileName),
            XMLStreamState = fxml_stream:new(self(), infinity),
            Res = process(State#state{xml_stream_state = XMLStreamState,
                                      fd = Fd,
                                      dir = Dir}),
            file:close(Fd),
            Res;
	{error, Reason} ->
            ErrTxt = file:format_error(Reason),
            ?ERROR_MSG("Failed to open file '~s': ~s", [FileName, ErrTxt]),
            {error, Reason}
    end.

-spec export_server(binary()) -> any().
export_server(Dir) ->
    export_hosts(?MYHOSTS, Dir).

-spec export_host(binary(), binary()) -> any().
export_host(Dir, Host) ->
    export_hosts([Host], Dir).

%%%===================================================================
%%% Internal functions
%%%===================================================================
export_hosts(Hosts, Dir) ->
    FnT = make_filename_template(),
    DFn = make_main_basefilename(Dir, FnT),
    case file:open(DFn, [raw, write]) of
        {ok, Fd} ->
            print(Fd, make_piefxis_xml_head()),
            print(Fd, make_piefxis_server_head()),
            FilesAndHosts = [{make_host_filename(FnT, Host), Host}
                             || Host <- Hosts],
            lists:foreach(
              fun({FnH, _}) ->
                      print(Fd, make_xinclude(FnH))
              end, FilesAndHosts),
            print(Fd, make_piefxis_server_tail()),
            print(Fd, make_piefxis_xml_tail()),
            file:close(Fd),
            lists:foldl(
              fun({FnH, Host}, ok) ->
                      export_host(Dir, FnH, Host);
                 (_, Err) ->
                      Err
              end, ok, FilesAndHosts);
        {error, Reason} ->
            ErrTxt = file:format_error(Reason),
            ?ERROR_MSG("Failed to open file '~s': ~s", [DFn, ErrTxt]),
            {error, Reason}
    end.

export_host(Dir, FnH, Host) ->
    DFn = make_host_basefilename(Dir, FnH),
    case file:open(DFn, [raw, write]) of
        {ok, Fd} ->
            print(Fd, make_piefxis_xml_head()),
            print(Fd, make_piefxis_host_head(Host)),
            Users = ejabberd_auth:get_vh_registered_users(Host),
            case export_users(Users, Host, Fd) of
                ok ->
                    print(Fd, make_piefxis_host_tail()),
                    print(Fd, make_piefxis_xml_tail()),
                    file:close(Fd),
                    ok;
                Err ->
                    file:close(Fd),
                    file:delete(DFn),
                    Err
            end;
        {error, Reason} ->
            ErrTxt = file:format_error(Reason),
            ?ERROR_MSG("Failed to open file '~s': ~s", [DFn, ErrTxt]),
            {error, Reason}
    end.

export_users([{User, _S}|Users], Server, Fd) ->
    case export_user(User, Server, Fd) of
        ok ->
            export_users(Users, Server, Fd);
        Err ->
            Err
    end;
export_users([], _Server, _Fd) ->
    ok.

export_user(User, Server, Fd) ->
    Password = ejabberd_auth:get_password_s(User, Server),
    LServer = jid:nameprep(Server),
    PasswordFormat = ejabberd_config:get_option({auth_password_format, LServer}, fun(X) -> X end, plain),
    Pass = case Password of
      {_,_,_,_} ->
        case PasswordFormat of
          scram -> format_scram_password(Password);          
          _ -> <<"">>
        end;
      _ -> Password
    end,
    Els = get_offline(User, Server) ++
        get_vcard(User, Server) ++
        get_privacy(User, Server) ++
        get_roster(User, Server) ++
        get_private(User, Server),
    print(Fd, fxml:element_to_binary(
                #xmlel{name = <<"user">>,
                       attrs = [{<<"name">>, User},
                                {<<"password">>, Pass}],
                       children = Els})).

format_scram_password({StoredKey, ServerKey, Salt, IterationCount}) ->
  StoredKeyB64 = base64:encode(StoredKey),
  ServerKeyB64 = base64:encode(ServerKey),
  SaltB64 = base64:encode(Salt),
  IterationCountBin = (integer_to_binary(IterationCount)),
  <<"scram:", StoredKeyB64/binary, ",", ServerKeyB64/binary, ",", SaltB64/binary, ",", IterationCountBin/binary>>.

parse_scram_password(PassData) ->
  Split = binary:split(PassData, <<",">>, [global]),
  [StoredKeyB64, ServerKeyB64, SaltB64, IterationCountBin] = Split,
  #scram{
    storedkey = StoredKeyB64,
    serverkey = ServerKeyB64,
    salt      = SaltB64,
    iterationcount = (binary_to_integer(IterationCountBin))
  }.

-spec get_vcard(binary(), binary()) -> [xmlel()].
get_vcard(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case mod_vcard:get_vcard(LUser, LServer) of
	error -> [];
	Els -> Els
    end.

-spec get_offline(binary(), binary()) -> [xmlel()].
get_offline(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    case mod_offline:get_offline_els(LUser, LServer) of
        [] ->
            [];
        Els ->
            NewEls = lists:map(fun xmpp:encode/1, Els),
            [#xmlel{name = <<"offline-messages">>, children = NewEls}]
    end.

-spec get_privacy(binary(), binary()) -> [xmlel()].
get_privacy(User, Server) ->
    case mod_privacy:get_user_lists(User, Server) of
        {ok, #privacy{default = Default,
                      lists = [_|_] = Lists}} ->
            XLists = lists:map(
                       fun({Name, Items}) ->
                               XItems = lists:map(
					  fun mod_privacy:encode_list_item/1,
					  Items),
			       #privacy_list{name = Name, items = XItems}
                       end, Lists),
	    [xmpp:encode(#privacy_query{default = Default, lists = XLists})];
        _ ->
            []
    end.

-spec get_roster(binary(), binary()) -> [xmlel()].
get_roster(User, Server) ->
    JID = jid:make(User, Server),
    case mod_roster:get_roster(User, Server) of
        [_|_] = Items ->
            Subs =
                lists:flatmap(
                  fun(#roster{ask = Ask,
                              askmessage = Msg} = R)
                        when Ask == in; Ask == both ->
                          Status = if is_binary(Msg) -> (Msg);
                                      true -> <<"">>
                                   end,
			  [xmpp:encode(
			     #presence{from = jid:make(R#roster.jid),
				       to = JID,
				       type = subscribe,
				       status = xmpp:mk_text(Status)})];
                     (_) ->
                          []
                  end, Items),
            Rs = lists:flatmap(
                   fun(#roster{ask = in, subscription = none}) ->
                           [];
                      (R) ->
                           [mod_roster:encode_item(R)]
                   end, Items),
	    [xmpp:encode(#roster_query{items = Rs}) | Subs];
        _ ->
            []
    end.

-spec get_private(binary(), binary()) -> [xmlel()].
get_private(User, Server) ->
    case mod_private:get_data(User, Server) of
        [_|_] = Els ->
	    [xmpp:encode(#private{xml_els = Els})];
        _ ->
            []
    end.

process(#state{xml_stream_state = XMLStreamState, fd = Fd} = State) ->
    case file:read(Fd, ?CHUNK_SIZE) of
        {ok, Data} ->
            NewXMLStreamState = fxml_stream:parse(XMLStreamState, Data),
            case process_els(State#state{xml_stream_state =
                                             NewXMLStreamState}) of
                {ok, NewState} ->
                    process(NewState);
                Err ->
                    fxml_stream:close(NewXMLStreamState),
                    Err
            end;
        eof ->
            fxml_stream:close(XMLStreamState),
            ok
    end.

process_els(State) ->
    receive
        {'$gen_event', El} ->
            case process_el(El, State) of
                {ok, NewState} ->
                    process_els(NewState);
                Err ->
                    Err
            end
    after 0 ->
            {ok, State}
    end.

process_el({xmlstreamstart, <<"server-data">>, Attrs}, State) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_PIEFXIS ->
            {ok, State};
        ?NS_PIE ->
            {ok, State};
        NS ->
            stop("Unknown 'server-data' namespace = ~s", [NS])
    end;
process_el({xmlstreamend, _}, State) ->
    {ok, State};
process_el({xmlstreamcdata, _}, State) ->
    {ok, State};
process_el({xmlstreamelement, #xmlel{name = <<"xi:include">>,
                                     attrs = Attrs}},
           #state{dir = Dir, user = <<"">>} = State) ->
    FileName = fxml:get_attr_s(<<"href">>, Attrs),
    case import_file(filename:join([Dir, FileName]), State) of
        ok ->
            {ok, State};
        Err ->
            Err
    end;
process_el({xmlstreamstart, <<"host">>, Attrs}, State) ->
    process_el({xmlstreamelement, #xmlel{name = <<"host">>,
                                         attrs = Attrs}}, State);
process_el({xmlstreamelement, #xmlel{name = <<"host">>,
                                     attrs = Attrs,
                                     children = Els}}, State) ->
    JIDS = fxml:get_attr_s(<<"jid">>, Attrs),
    try jid:decode(JIDS) of
        #jid{lserver = S} ->
            case ejabberd_router:is_my_host(S) of
                true ->
                    process_users(Els, State#state{server = S});
                false ->
                    stop("Unknown host: ~s", [S])
            end
    catch _:{bad_jid, _} ->
            stop("Invalid 'jid': ~s", [JIDS])
    end;
process_el({xmlstreamstart, <<"user">>, Attrs}, State = #state{server = S})
  when S /= <<"">> ->
    process_el({xmlstreamelement, #xmlel{name = <<"user">>, attrs = Attrs}},
               State);
process_el({xmlstreamelement, #xmlel{name = <<"user">>} = El},
           State = #state{server = S}) when S /= <<"">> ->
    process_user(El, State);
process_el({xmlstreamelement, El}, State = #state{server = S, user = U})
  when S /= <<"">>, U /= <<"">> ->
    process_user_el(El, State);
process_el({xmlstreamelement, El}, _State) ->
    stop("Unexpected tag: ~p", [El]);
process_el({xmlstreamstart, El, Attrs}, _State) ->
    stop("Unexpected payload: ~p", [{El, Attrs}]);
process_el({xmlstreamerror, Err}, _State) ->
    stop("Failed to process element = ~p", [Err]).

process_users([#xmlel{} = El|Els], State) ->
    case process_user(El, State) of
        {ok, NewState} ->
            process_users(Els, NewState);
        Err ->
            Err
    end;
process_users([_|Els], State) ->
    process_users(Els, State);
process_users([], State) ->
    {ok, State}.

process_user(#xmlel{name = <<"user">>, attrs = Attrs, children = Els},
             #state{server = LServer} = State) ->
    Name = fxml:get_attr_s(<<"name">>, Attrs),
    Password = fxml:get_attr_s(<<"password">>, Attrs),
    PasswordFormat = ejabberd_config:get_option({auth_password_format, LServer}, fun(X) -> X end, plain),
    Pass = case PasswordFormat of
      scram ->
        case Password of 
          <<"scram:", PassData/binary>> ->
            parse_scram_password(PassData);
          P -> P
        end;
      _ -> Password
    end,

    case jid:nodeprep(Name) of
        error ->
            stop("Invalid 'user': ~s", [Name]);
        LUser ->
            case ejabberd_auth:try_register(LUser, LServer, Pass) of
                {atomic, _} ->
                    process_user_els(Els, State#state{user = LUser});
                Err ->
                    stop("Failed to create user '~s': ~p", [Name, Err])
            end
    end.

process_user_els([#xmlel{} = El|Els], State) ->
    case process_user_el(El, State) of
        {ok, NewState} ->
            process_user_els(Els, NewState);
        Err ->
            Err
    end;
process_user_els([_|Els], State) ->
    process_user_els(Els, State);
process_user_els([], State) ->
    {ok, State}.

process_user_el(#xmlel{name = Name, attrs = Attrs, children = Els} = El,
                State) ->
    try
	case {Name, fxml:get_attr_s(<<"xmlns">>, Attrs)} of
	    {<<"query">>, ?NS_ROSTER} ->
		process_roster(xmpp:decode(El), State);
	    {<<"query">>, ?NS_PRIVACY} ->
		%% Make sure <list/> elements go before <active/> and <default/>
		process_privacy(xmpp:decode(El), State);
	    {<<"query">>, ?NS_PRIVATE} ->
		process_private(xmpp:decode(El), State);
	    {<<"vCard">>, ?NS_VCARD} ->
		process_vcard(El, State);
	    {<<"offline-messages">>, NS} ->
		Msgs = [xmpp:decode(E, NS, [ignore_els]) || E <- Els],
		process_offline_msgs(Msgs, State);
	    {<<"presence">>, ?NS_CLIENT} ->
		process_presence(xmpp:decode(El, ?NS_CLIENT, [ignore_els]), State);
	    _ ->
		{ok, State}
	end
    catch _:{xmpp_codec, Why} ->
	    ErrTxt = xmpp:format_error(Why),
	    stop("failed to decode XML '~s': ~s",
		 [fxml:element_to_binary(El), ErrTxt])
    end.

-spec process_offline_msgs([stanza()], state()) -> {ok, state()} | {error, _}.
process_offline_msgs([#message{} = Msg|Msgs], State) ->
    case process_offline_msg(Msg, State) of
        {ok, NewState} ->
            process_offline_msgs(Msgs, NewState);
        Err ->
            Err
    end;
process_offline_msgs([_|Msgs], State) ->
    process_offline_msgs(Msgs, State);
process_offline_msgs([], State) ->
    {ok, State}.

-spec process_roster(roster_query(), state()) -> {ok, state()} | {error, _}.
process_roster(RosterQuery, State = #state{user = U, server = S}) ->
    case mod_roster:set_items(U, S, RosterQuery) of
        {atomic, _} ->
            {ok, State};
        Err ->
            stop("Failed to write roster: ~p", [Err])
    end.

-spec process_privacy(privacy_query(), state()) -> {ok, state()} | {error, _}.
process_privacy(#privacy_query{lists = Lists,
			       default = Default,
			       active = Active} = PrivacyQuery,
		State = #state{user = U, server = S}) ->
    JID = jid:make(U, S),
    IQ = #iq{type = set, id = randoms:get_string(),
	     from = JID, to = JID, sub_els = [PrivacyQuery]},
    case mod_privacy:process_iq(IQ) of
	#iq{type = error} = ResIQ ->
	    #stanza_error{reason = Reason} = xmpp:get_error(ResIQ),
	    if Reason == 'item-not-found', Lists == [],
	       Active == undefined, Default /= undefined ->
		    %% Failed to set default list because there is no
		    %% list with such name. We shouldn't stop here.
		    {ok, State};
	       true ->
		    stop("Failed to write privacy: ~p", [Reason])
            end;
        _ ->
            {ok, State}
    end.

-spec process_private(private(), state()) -> {ok, state()} | {error, _}.
process_private(Private, State = #state{user = U, server = S}) ->
    JID = jid:make(U, S),
    IQ = #iq{type = set, id = randoms:get_string(),
	     from = JID, to = JID, sub_els = [Private]},
    case mod_private:process_sm_iq(IQ) of
        #iq{type = result} ->
            {ok, State};
        Err ->
            stop("Failed to write private: ~p", [Err])
    end.

-spec process_vcard(xmlel(), state()) -> {ok, state()} | {error, _}.
process_vcard(El, State = #state{user = U, server = S}) ->
    JID = jid:make(U, S),
    IQ = #iq{type = set, id = randoms:get_string(),
	     from = JID, to = JID, sub_els = [El]},
    case mod_vcard:process_sm_iq(IQ) of
        #iq{type = result} ->
            {ok, State};
        Err ->
            stop("Failed to write vcard: ~p", [Err])
    end.

-spec process_offline_msg(message(), state()) -> {ok, state()} | {error, _}.
process_offline_msg(#message{from = undefined}, _State) ->
    stop("No 'from' attribute found", []);
process_offline_msg(Msg, State = #state{user = U, server = S}) ->
    To = jid:make(U, S),
    ejabberd_hooks:run_fold(
      offline_message_hook, To#jid.lserver, {pass, xmpp:set_to(Msg, To)}, []),
    {ok, State}.

-spec process_presence(presence(), state()) -> {ok, state()} | {error, _}.
process_presence(#presence{from = undefined}, _State) ->
    stop("No 'from' attribute found", []);
process_presence(Pres, #state{user = U, server = S} = State) ->
    To = jid:make(U, S),
    NewPres = xmpp:set_to(Pres, To),
    ejabberd_router:route(NewPres),
    {ok, State}.

stop(Fmt, Args) ->
    ?ERROR_MSG(Fmt, Args),
    {error, import_failed}.

make_filename_template() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    str:format("~4..0w~2..0w~2..0w-~2..0w~2..0w~2..0w",
	       [Year, Month, Day, Hour, Minute, Second]).

make_main_basefilename(Dir, FnT) ->
    Filename2 = <<FnT/binary, ".xml">>,
    filename:join([Dir, Filename2]).

%% @doc Make the filename for the host.
%% Example: ``(<<"20080804-231550">>, <<"jabber.example.org">>) ->
%%             <<"20080804-231550_jabber_example_org.xml">>''
make_host_filename(FnT, Host) ->
    Host2 = str:join(str:tokens(Host, <<".">>), <<"_">>),
    <<FnT/binary, "_", Host2/binary, ".xml">>.

%%%==================================
%%%% PIEFXIS formatting
make_host_basefilename(Dir, FnT) ->
    filename:join([Dir, FnT]).

%% @spec () -> string()
make_piefxis_xml_head() ->
    "<?xml version='1.0' encoding='UTF-8'?>".

%% @spec () -> string()
make_piefxis_xml_tail() ->
    "".

%% @spec () -> string()
make_piefxis_server_head() ->
    io_lib:format("<server-data xmlns='~s' xmlns:xi='~s'>",
                  [?NS_PIE, ?NS_XI]).

%% @spec () -> string()
make_piefxis_server_tail() ->
    "</server-data>".

%% @spec (Host::string()) -> string()
make_piefxis_host_head(Host) ->
    io_lib:format("<host xmlns='~s' xmlns:xi='~s' jid='~s'>",
                  [?NS_PIE, ?NS_XI, Host]).

%% @spec () -> string()
make_piefxis_host_tail() ->
    "</host>".

%% @spec (Fn::string()) -> string()
make_xinclude(Fn) ->
    Base = filename:basename(Fn),
    io_lib:format("<xi:include href='~s'/>", [Base]).

print(Fd, String) ->
    file:write(Fd, String).

opt_type(auth_password_format) -> fun (X) -> X end;
opt_type(_) -> [auth_password_format].

