%%%----------------------------------------------------------------------
%%% File    : ejabberd_piefxis.erl
%%% Author  : Pablo Polvorin, Vidal Santiago Martinez
%%% Purpose : XEP-0227: Portable Import/Export Format for XMPP-IM Servers
%%% Created : 17 Jul 2008 by Pablo Polvorin <pablo.polvorin@process-one.net>
%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2012, Evgeniy Khramtsov
%%% @doc
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% - Export from mod_offline_odbc.erl
%%% - Export from mod_private_odbc.erl
%%% - XEP-227: 6. Security Considerations
%%% - Other schemas of XInclude are not tested, and may not be imported correctly.
%%% - If a host has many users, split that host in XML files with 50 users each.
%%%% Headers

-module(ejabberd_piefxis).

%% API
-export([import_file/1, export_server/1, export_host/2]).

-define(CHUNK_SIZE, 1024*20). %20k

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
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

-record(state, {xml_stream_state :: xml_stream:xml_stream_state(),
                user = <<"">>    :: binary(),
                server = <<"">>  :: binary(),
                fd               :: file:io_device(),
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
            XMLStreamState = xml_stream:new(self(), infinity),
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

%%%==================================
%%%% Process Elements
%%%==================================
%%%% Process Element
%%%==================================
%%%% Add user
%% @spec (El::xmlel(), Domain::string(), User::binary(), Password::binary() | none)
%%       -> ok | {error, ErrorText::string()}
%% @doc Add a new user to the database.
%% If user already exists, it will be only updated.
-spec export_server(binary()) -> any().

%% @spec (User::string(), Password::string(), Domain::string())
%%       -> ok | {atomic, exists} | {error, not_allowed}
%% @doc  Create a new user
export_server(Dir) ->
    export_hosts(?MYHOSTS, Dir).

%%%==================================
%%%% Populate user
%% @spec (User::string(), Domain::string(), El::xml())
%%      -> ok | {error, not_found}
%%
%% @doc  Add a new user from a XML file with a roster list.
%%
%% Example of a file:
%% ```
%% <?xml version='1.0' encoding='UTF-8'?>
%% <server-data xmlns='http://www.xmpp.org/extensions/xep-0227.html#ns'>
%%   <host jid='localhost'>
%%     <user name='juliet' password='s3crEt'>
%%       <query xmlns='jabber:iq:roster'>
%%         <item jid='romeo@montague.net'
%%               name='Romeo'
%%               subscription='both'>
%%           <group>Friends</group>
%%         </item>
%%       </query>
%%     </user>
%%   </host>
%%  </server-data>
%% '''
-spec export_host(binary(), binary()) -> any().

export_host(Dir, Host) ->
    export_hosts([Host], Dir).

%% @spec User   = String with the user name
%%       Domain = String with a domain name
%%       El     = Sub XML element with vCard tags values
%% @ret  ok | {error, not_found}
%% @doc  Read vcards from the XML and send it to the server
%%
%% Example:
%% ```
%% <?xml version='1.0' encoding='UTF-8'?>
%% <server-data xmlns='http://www.xmpp.org/extensions/xep-0227.html#ns'>
%%   <host jid='localhost'>
%%     <user name='admin' password='s3crEt'>
%%       <vCard xmlns='vcard-temp'>
%%         <FN>Admin</FN>
%%       </vCard>
%%     </user>
%%   </host>
%% </server-data>
%% '''
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

%% @spec User   = String with the user name
%%       Domain = String with a domain name
%%       El     = Sub XML element with offline messages values
%% @ret  ok | {error, not_found}
%% @doc  Read off-line message from the XML and send it to the server
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

%% @spec User   = String with the user name
%%       Domain = String with a domain name
%%       El     = Sub XML element with private storage values
%% @ret  ok | {error, not_found}
%% @doc  Private storage parsing
export_users([{User, _S}|Users], Server, Fd) ->
    case export_user(User, Server, Fd) of
        ok ->
            export_users(Users, Server, Fd);
        Err ->
            Err
    end;
export_users([], _Server, _Fd) ->
    ok.

%%%==================================
%%%% Utilities
export_user(User, Server, Fd) ->
    Pass = ejabberd_auth:get_password_s(User, Server),
    Els = get_offline(User, Server) ++
        get_vcard(User, Server) ++
        get_privacy(User, Server) ++
        get_roster(User, Server) ++
        get_private(User, Server),
    print(Fd, xml:element_to_binary(
                #xmlel{name = <<"user">>,
                       attrs = [{<<"name">>, User},
                                {<<"password">>, Pass}],
                       children = Els})).

get_vcard(User, Server) ->
    JID = jlib:make_jid(User, Server, <<>>),
    case mod_vcard:process_sm_iq(JID, JID, #iq{type = get}) of
        #iq{type = result, sub_el = [_|_] = VCardEls} ->
            VCardEls;
        _ ->
            []
    end.

%%%==================================
get_offline(User, Server) ->
    case mod_offline:get_offline_els(User, Server) of
        [] ->
            [];
        Els ->
            NewEls = lists:map(
                       fun(#xmlel{attrs = Attrs} = El) ->
                               NewAttrs = lists:keystore(<<"xmlns">>, 1,
                                                         Attrs,
                                                         {<<"xmlns">>,
                                                          <<"jabber:client">>}),
                               El#xmlel{attrs = NewAttrs}
                       end, Els),
            [#xmlel{name = <<"offline-messages">>, children = NewEls}]
    end.

%%%% Export hosts
get_privacy(User, Server) ->
    case mod_privacy:get_user_lists(User, Server) of
        {ok, #privacy{default = Default,
                      lists = [_|_] = Lists}} ->
            XLists = lists:map(
                       fun({Name, Items}) ->
                               XItems = lists:map(
                                          fun mod_privacy:item_to_xml/1, Items),
                               #xmlel{name = <<"list">>,
                                      attrs = [{<<"name">>, Name}],
                                      children = XItems}
                       end, Lists),
            DefaultEl = case Default of
                            none ->
                                [];
                            _ ->
                                [#xmlel{name = <<"default">>,
                                        attrs = [{<<"name">>, Default}]}]
                        end,
            [#xmlel{name = <<"query">>,
                    attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
                    children = DefaultEl ++ XLists}];
        _ ->
            []
    end.

%% @spec (Dir::string(), Hosts::[string()]) -> ok
get_roster(User, Server) ->
    JID = jlib:make_jid(User, Server, <<>>),
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
                          [#xmlel{name = <<"presence">>,
                                  attrs =
                                      [{<<"from">>,
                                        jlib:jid_to_string(R#roster.jid)},
                                       {<<"to">>, jlib:jid_to_string(JID)},
                                       {<<"xmlns">>, <<"jabber:client">>},
                                       {<<"type">>, <<"subscribe">>}],
                                  children =
                                      [#xmlel{name = <<"status">>,
                                              attrs = [],
                                              children =
                                                  [{xmlcdata, Status}]}]}];
                     (_) ->
                          []
                  end, Items),
            Rs = lists:flatmap(
                   fun(#roster{ask = in, subscription = none}) ->
                           [];
                      (R) ->
                           [mod_roster:item_to_xml(R)]
                   end, Items),
            [#xmlel{name = <<"query">>,
                    attrs = [{<<"xmlns">>, ?NS_ROSTER}],
                    children = Rs} | Subs];
        _ ->
            []
    end.

get_private(User, Server) ->
    case mod_private:get_data(User, Server) of
        [_|_] = Els ->
            [#xmlel{name = <<"query">>,
                    attrs = [{<<"xmlns">>, ?NS_PRIVATE}],
                    children = Els}];
        _ ->
            []
    end.

process(#state{xml_stream_state = XMLStreamState, fd = Fd} = State) ->
    case file:read(Fd, ?CHUNK_SIZE) of
        {ok, Data} ->
            NewXMLStreamState = xml_stream:parse(XMLStreamState, Data),
            case process_els(State#state{xml_stream_state =
                                             NewXMLStreamState}) of
                {ok, NewState} ->
                    process(NewState);
                Err ->
                    xml_stream:close(NewXMLStreamState),
                    Err
            end;
        eof ->
            xml_stream:close(XMLStreamState),
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
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
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
    FileName = xml:get_attr_s(<<"href">>, Attrs),
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
    JIDS = xml:get_attr_s(<<"jid">>, Attrs),
    case jlib:string_to_jid(JIDS) of
        #jid{lserver = S} ->
            case lists:member(S, ?MYHOSTS) of
                true ->
                    process_users(Els, State#state{server = S});
                false ->
                    stop("Unknown host: ~s", [S])
            end;
        error ->
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
    Name = xml:get_attr_s(<<"name">>, Attrs),
    Pass = xml:get_attr_s(<<"password">>, Attrs),
    case jlib:nodeprep(Name) of
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
    case {Name, xml:get_attr_s(<<"xmlns">>, Attrs)} of
        {<<"query">>, ?NS_ROSTER} ->
            process_roster(El, State);
        {<<"query">>, ?NS_PRIVACY} ->
            %% Make sure <list/> elements go before <active/> and <default/>
            NewEls = lists:reverse(lists:keysort(#xmlel.name, Els)),
            process_privacy_el(El#xmlel{children = NewEls}, State);
        {<<"query">>, ?NS_PRIVATE} ->
            process_private(El, State);
        {<<"vCard">>, ?NS_VCARD} ->
            process_vcard(El, State);
        {<<"offline-messages">>, _} ->
            process_offline_msgs(Els, State);
        {<<"presence">>, <<"jabber:client">>} ->
            process_presence(El, State);
        _ ->
            {ok, State}
    end.

process_privacy_el(#xmlel{children = [#xmlel{} = SubEl|SubEls]} = El, State) ->
    case process_privacy(#xmlel{children = [SubEl]}, State) of
        {ok, NewState} ->
            process_privacy_el(El#xmlel{children = SubEls}, NewState);
        Err ->
            Err
    end;
process_privacy_el(#xmlel{children = [_|SubEls]} = El, State) ->
    process_privacy_el(El#xmlel{children = SubEls}, State);
process_privacy_el(#xmlel{children = []}, State) ->
    {ok, State}.

process_offline_msgs([#xmlel{} = El|Els], State) ->
    case process_offline_msg(El, State) of
        {ok, NewState} ->
            process_offline_msgs(Els, NewState);
        Err ->
            Err
    end;
process_offline_msgs([_|Els], State) ->
    process_offline_msgs(Els, State);
process_offline_msgs([], State) ->
    {ok, State}.

process_roster(El, State = #state{user = U, server = S}) ->
    case mod_roster:set_items(U, S, El) of
        {atomic, _} ->
            {ok, State};
        Err ->
            stop("Failed to write roster: ~p", [Err])
    end.

%%%==================================
%%%% Export server
process_privacy(El, State = #state{user = U, server = S}) ->
    JID = jlib:make_jid(U, S, <<"">>),
    case mod_privacy:process_iq_set(
           [], JID, JID, #iq{type = set, sub_el = El}) of
        {error, _} = Err ->
            stop("Failed to write privacy: ~p", [Err]);
        _ ->
            {ok, State}
    end.

%% @spec (Dir::string()) -> ok
process_private(El, State = #state{user = U, server = S}) ->
    JID = jlib:make_jid(U, S, <<"">>),
    case mod_private:process_sm_iq(
           JID, JID, #iq{type = set, sub_el = El}) of
        #iq{type = result} ->
            {ok, State};
        Err ->
            stop("Failed to write private: ~p", [Err])
    end.

%%%==================================
%%%% Export host
process_vcard(El, State = #state{user = U, server = S}) ->
    JID = jlib:make_jid(U, S, <<"">>),
    case mod_vcard:process_sm_iq(
           JID, JID, #iq{type = set, sub_el = El}) of
        #iq{type = result} ->
            {ok, State};
        Err ->
            stop("Failed to write vcard: ~p", [Err])
    end.

%% @spec (Dir::string(), Host::string()) -> ok
process_offline_msg(El, State = #state{user = U, server = S}) ->
    FromS = xml:get_attr_s(<<"from">>, El#xmlel.attrs),
    case jlib:string_to_jid(FromS) of
        #jid{} = From ->
            To = jlib:make_jid(U, S, <<>>),
            NewEl = jlib:replace_from_to(From, To, El),
            case catch mod_offline:store_packet(From, To, NewEl) of
                {'EXIT', _} = Err ->
                    stop("Failed to store offline message: ~p", [Err]);
                _ ->
                    {ok, State}
            end;
        _ ->
            stop("Invalid 'from' = ~s", [FromS])
    end.

%% @spec (Dir::string(), Fn::string(), Host::string()) -> ok
process_presence(El, #state{user = U, server = S} = State) ->
    FromS = xml:get_attr_s(<<"from">>, El#xmlel.attrs),
    case jlib:string_to_jid(FromS) of
        #jid{} = From ->
            To = jlib:make_jid(U, S, <<>>),
            NewEl = jlib:replace_from_to(From, To, El),
            ejabberd_router:route(From, To, NewEl),
            {ok, State};
        _ ->
            stop("Invalid 'from' = ~s", [FromS])
    end.

stop(Fmt, Args) ->
    ?ERROR_MSG(Fmt, Args),
    {error, import_failed}.

make_filename_template() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    list_to_binary(
      io_lib:format("~4..0w~2..0w~2..0w-~2..0w~2..0w~2..0w",
		    [Year, Month, Day, Hour, Minute, Second])).

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

%%%==================================
%%%% Export user
%% @spec (Fd, Username::string(), Host::string()) -> ok
%% @doc Extract user information and print it.
%% @spec (Username::string(), Host::string()) -> string()
%% @spec (InfoName::atom(), Username::string(), Host::string()) -> string()
%%%==================================
%%%% Interface with ejabberd offline storage
%% Copied from mod_offline.erl and customized
%%%==================================
%%%% Interface with ejabberd private storage
%%%==================================
%%%% Disk file access
%% @spec () -> string()
%% @spec (Dir::string(), FnT::string()) -> string()
%% @spec (FnT::string(), Host::string()) -> FnH::string()
%% @doc Make the filename for the host.
%% Example: ``("20080804-231550", "jabber.example.org") -> "20080804-231550_jabber_example_org.xml"''
%% @spec (Fn::string()) -> {ok, Fd}
%% @spec (Fd) -> ok
%% @spec (Fd, String::string()) -> ok
print(Fd, String) ->
%%%==================================
%%% vim: set filetype=erlang tabstop=8 foldmarker=%%%%,%%%= foldmethod=marker:
    file:write(Fd, String).
