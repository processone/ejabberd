%%%----------------------------------------------------------------------
%%% File    : ejabberd_piefxis.erl
%%% Author  : Pablo Polvorin, Vidal Santiago Martinez, Evgeniy Khramtsov
%%% Purpose : XEP-0227: Portable Import/Export Format for XMPP-IM Servers
%%% Created : 17 Jul 2008 by Pablo Polvorin <pablo.polvorin@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
%%% - PEP nodes export/import
%%% - message archives export/import
%%% - write mod_piefxis with ejabberdctl commands
%%% - Other schemas of XInclude are not tested, and may not be imported correctly.
%%% - If a host has many users, split that host in XML files with 50 users each.

-module(ejabberd_piefxis).

-protocol({xep, 227, '1.1', '2.1.0', "partial", ""}).

-export([import_file/1, export_server/1, export_host/2]).

-define(CHUNK_SIZE, 1024 * 20).  %20k

-include_lib("xmpp/include/scram.hrl").

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

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
-define(NS_PIE,     <<"urn:xmpp:pie:0">>).
-define(NS_PIEFXIS, <<"http://www.xmpp.org/extensions/xep-0227.html#ns">>).
-define(NS_XI,      <<"http://www.w3.org/2001/XInclude">>).

%%
%% @efmt:off
%% @indent-begin

-record(state, {xml_stream_state :: fxml_stream:xml_stream_state() | undefined,
                user = <<"">>    :: binary(),
                server = <<"">>  :: binary(),
                fd = self()      :: file:io_device(),
                dir = <<"">>     :: binary()}).

%% @indent-end
%% @efmt:on
           %%

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
            Res = process(State#state{
                            xml_stream_state = XMLStreamState,
                            fd = Fd,
                            dir = Dir
                           }),
            file:close(Fd),
            Res;
        {error, Reason} ->
            ErrTxt = file:format_error(Reason),
            ?ERROR_MSG("Failed to open file '~ts': ~ts", [FileName, ErrTxt]),
            {error, Reason}
    end.


-spec export_server(binary()) -> any().
export_server(Dir) ->
    export_hosts(ejabberd_option:hosts(), Dir).


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
            FilesAndHosts = [ {make_host_filename(FnT, Host), Host}
                              || Host <- Hosts ],
            lists:foreach(
              fun({FnH, _}) ->
                      print(Fd, make_xinclude(FnH))
              end,
              FilesAndHosts),
            print(Fd, make_piefxis_server_tail()),
            print(Fd, make_piefxis_xml_tail()),
            file:close(Fd),
            lists:foldl(
              fun({FnH, Host}, ok) ->
                      export_host(Dir, FnH, Host);
                 (_, Err) ->
                      Err
              end,
              ok,
              FilesAndHosts);
        {error, Reason} ->
            ErrTxt = file:format_error(Reason),
            ?ERROR_MSG("Failed to open file '~ts': ~ts", [DFn, ErrTxt]),
            {error, Reason}
    end.


export_host(Dir, FnH, Host) ->
    DFn = make_host_basefilename(Dir, FnH),
    case file:open(DFn, [raw, write]) of
        {ok, Fd} ->
            print(Fd, make_piefxis_xml_head()),
            print(Fd, make_piefxis_host_head(Host)),
            Users = ejabberd_auth:get_users(Host),
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
            ?ERROR_MSG("Failed to open file '~ts': ~ts", [DFn, ErrTxt]),
            {error, Reason}
    end.


export_users([{User, _S} | Users], Server, Fd) ->
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
    {PassPlain, PassScram} = case ejabberd_auth:password_format(LServer) of
                                 scram -> {[], [format_scram_password(Password)]};
                                 _ when Password == <<"">> -> {[], []};
                                 _ -> {[{<<"password">>, Password}], []}
                             end,
    Els =
        PassScram ++
        get_offline(User, Server) ++
        get_vcard(User, Server) ++
        get_privacy(User, Server) ++
        get_roster(User, Server) ++
        get_private(User, Server),
    print(Fd,
          fxml:element_to_binary(
            #xmlel{
              name = <<"user">>,
              attrs = [{<<"name">>, User} | PassPlain],
              children = Els
             })).


format_scram_password(#scram{
                        hash = Hash,
                        storedkey = StoredKey,
                        serverkey = ServerKey,
                        salt = Salt,
                        iterationcount = IterationCount
                       }) ->
    StoredKeyB64 = base64:encode(StoredKey),
    ServerKeyB64 = base64:encode(ServerKey),
    SaltB64 = base64:encode(Salt),
    IterationCountBin = (integer_to_binary(IterationCount)),
    MechanismB = case Hash of
                     sha -> <<"SCRAM-SHA-1">>;
                     sha256 -> <<"SCRAM-SHA-256">>;
                     sha512 -> <<"SCRAM-SHA-512">>
                 end,
    Children =
        [#xmlel{
           name = <<"iter-count">>,
           children = [{xmlcdata, IterationCountBin}]
          },
         #xmlel{
           name = <<"salt">>,
           children = [{xmlcdata, SaltB64}]
          },
         #xmlel{
           name = <<"server-key">>,
           children = [{xmlcdata, ServerKeyB64}]
          },
         #xmlel{
           name = <<"stored-key">>,
           children = [{xmlcdata, StoredKeyB64}]
          }],
    #xmlel{
      name = <<"scram-credentials">>,
      attrs = [{<<"xmlns">>, <<?NS_PIE/binary, "#scram">>},
               {<<"mechanism">>, MechanismB}],
      children = Children
     }.


parse_scram_password(#xmlel{attrs = Attrs} = El) ->
    Hash = case fxml:get_attr_s(<<"mechanism">>, Attrs) of
               <<"SCRAM-SHA-1">> -> sha;
               <<"SCRAM-SHA-256">> -> sha256;
               <<"SCRAM-SHA-512">> -> sha512
           end,
    StoredKeyB64 = fxml:get_path_s(El, [{elem, <<"stored-key">>}, cdata]),
    ServerKeyB64 = fxml:get_path_s(El, [{elem, <<"server-key">>}, cdata]),
    IterationCountBin = fxml:get_path_s(El, [{elem, <<"iter-count">>}, cdata]),
    SaltB64 = fxml:get_path_s(El, [{elem, <<"salt">>}, cdata]),
    #scram{
      storedkey = base64:decode(StoredKeyB64),
      serverkey = base64:decode(ServerKeyB64),
      salt = base64:decode(SaltB64),
      hash = Hash,
      iterationcount = (binary_to_integer(IterationCountBin))
     };

parse_scram_password(PassData) ->
    Split = binary:split(PassData, <<",">>, [global]),
    [Hash, StoredKeyB64, ServerKeyB64, SaltB64, IterationCountBin] =
        case Split of
            [K1, K2, K3, K4] -> [sha, K1, K2, K3, K4];
            [<<"sha256">>, K1, K2, K3, K4] -> [sha256, K1, K2, K3, K4];
            [<<"sha512">>, K1, K2, K3, K4] -> [sha512, K1, K2, K3, K4]
        end,
    #scram{
      storedkey = base64:decode(StoredKeyB64),
      serverkey = base64:decode(ServerKeyB64),
      salt = base64:decode(SaltB64),
      hash = Hash,
      iterationcount = (binary_to_integer(IterationCountBin))
     }.


-spec get_vcard(binary(), binary()) -> [xmlel()].
get_vcard(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    try mod_vcard:get_vcard(LUser, LServer) of
        error -> [];
        Els -> Els
    catch
        error:{module_not_loaded, _, _} -> []
    end.


-spec get_offline(binary(), binary()) -> [xmlel()].
get_offline(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    try mod_offline:get_offline_els(LUser, LServer) of
        [] ->
            [];
        Els ->
            NewEls = lists:map(fun xmpp:encode/1, Els),
            [#xmlel{name = <<"offline-messages">>, children = NewEls}]
    catch
        error:{module_not_loaded, _, _} -> []
    end.


-spec get_privacy(binary(), binary()) -> [xmlel()].
get_privacy(User, Server) ->
    try mod_privacy:get_user_lists(User, Server) of
        {ok, #privacy{
               default = Default,
               lists = [_ | _] = Lists
              }} ->
            XLists = lists:map(
                       fun({Name, Items}) ->
                               XItems = lists:map(
                                          fun mod_privacy:encode_list_item/1,
                                          Items),
                               #privacy_list{name = Name, items = XItems}
                       end,
                       Lists),
            [xmpp:encode(#privacy_query{default = Default, lists = XLists})];
        _ ->
            []
    catch
        error:{module_not_loaded, _, _} -> []
    end.


-spec get_roster(binary(), binary()) -> [xmlel()].
get_roster(User, Server) ->
    JID = jid:make(User, Server),
    try mod_roster:get_roster(User, Server) of
        [_ | _] = Items ->
            Subs =
                lists:flatmap(
                  fun(#roster{
                        ask = Ask,
                        askmessage = Msg
                       } = R)
                        when Ask == in; Ask == both ->
                          Status = if
                                       is_binary(Msg) -> (Msg);
                                       true -> <<"">>
                                   end,
                          [xmpp:encode(
                             #presence{
                               from = jid:make(R#roster.jid),
                               to = JID,
                               type = subscribe,
                               status = xmpp:mk_text(Status)
                              })];
                     (_) ->
                          []
                  end,
                  Items),
            Rs = lists:flatmap(
                   fun(#roster{ask = in, subscription = none}) ->
                           [];
                      (R) ->
                           [mod_roster:encode_item(R)]
                   end,
                   Items),
            [xmpp:encode(#roster_query{items = Rs}) | Subs];
        _ ->
            []
    catch
        error:{module_not_loaded, _, _} -> []
    end.


-spec get_private(binary(), binary()) -> [xmlel()].
get_private(User, Server) ->
    try mod_private:get_data(User, Server) of
        [_ | _] = Els ->
            [xmpp:encode(#private{sub_els = Els})];
        _ ->
            []
    catch
        error:{module_not_loaded, _, _} -> []
    end.


process(#state{xml_stream_state = XMLStreamState, fd = Fd} = State) ->
    case file:read(Fd, ?CHUNK_SIZE) of
        {ok, Data} ->
            NewXMLStreamState = fxml_stream:parse(XMLStreamState, Data),
            case process_els(State#state{
                               xml_stream_state =
                                   NewXMLStreamState
                              }) of
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
    Els = gather_els(State, []),
    process_els(State, lists:reverse(Els)).


gather_els(State, List) ->
    receive
        {'$gen_event', El} ->
            gather_els(State, [El | List])
    after
        0 ->
            List
    end.


process_els(State, [El | Tail]) ->
    case process_el(El, State) of
        {ok, NewState} ->
            process_els(NewState, Tail);
        Err ->
            Err
    end;
process_els(State, []) ->
    {ok, State}.


process_el({xmlstreamstart, <<"server-data">>, Attrs}, State) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_PIEFXIS ->
            {ok, State};
        ?NS_PIE ->
            {ok, State};
        NS ->
            stop("Unknown 'server-data' namespace = ~ts", [NS])
    end;
process_el({xmlstreamend, _}, State) ->
    {ok, State};
process_el({xmlstreamcdata, _}, State) ->
    {ok, State};
process_el({xmlstreamelement, #xmlel{
                                name = <<"xi:include">>,
                                attrs = Attrs
                               }},
           #state{dir = Dir, user = <<"">>} = State) ->
    FileName = fxml:get_attr_s(<<"href">>, Attrs),
    case import_file(filename:join([Dir, FileName]), State) of
        ok ->
            {ok, State};
        Err ->
            Err
    end;
process_el({xmlstreamstart, <<"host">>, Attrs}, State) ->
    process_el({xmlstreamelement, #xmlel{
                                    name = <<"host">>,
                                    attrs = Attrs
                                   }},
               State);
process_el({xmlstreamelement, #xmlel{
                                name = <<"host">>,
                                attrs = Attrs,
                                children = Els
                               }},
           State) ->
    JIDS = fxml:get_attr_s(<<"jid">>, Attrs),
    try jid:decode(JIDS) of
        #jid{lserver = S} ->
            case ejabberd_router:is_my_host(S) of
                true ->
                    process_users(Els, State#state{server = S});
                false ->
                    stop("Unknown host: ~ts", [S])
            end
    catch
        _:{bad_jid, _} ->
            stop("Invalid 'jid': ~ts", [JIDS])
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


process_users([#xmlel{} = El | Els], State) ->
    case process_user(El, State) of
        {ok, NewState} ->
            process_users(Els, NewState);
        Err ->
            Err
    end;
process_users([_ | Els], State) ->
    process_users(Els, State);
process_users([], State) ->
    {ok, State}.


process_user(#xmlel{name = <<"user">>, attrs = Attrs, children = Els} = El,
             #state{server = LServer} = State) ->
    Name = fxml:get_attr_s(<<"name">>, Attrs),
    Pass = process_password(El, LServer),
    case jid:nodeprep(Name) of
        error ->
            stop("Invalid 'user': ~ts", [Name]);
        LUser ->
            case ejabberd_auth:try_register(LUser, LServer, Pass) of
                ok ->
                    process_user_els(Els, State#state{user = LUser});
                {error, invalid_password} when (Pass == <<>>) ->
                    process_user_els(Els, State#state{user = LUser});
                {error, Err} ->
                    stop("Failed to create user '~ts': ~p", [Name, Err])
            end
    end.


process_password(#xmlel{name = <<"user">>, attrs = Attrs} = El, LServer) ->
    {PassPlain, PassOldScram} = case fxml:get_attr_s(<<"password">>, Attrs) of
                                    <<"scram:", PassData/binary>> -> {<<"">>, PassData};
                                    P -> {P, false}
                                end,
    ScramCred = fxml:get_subtag(El, <<"scram-credentials">>),
    PasswordFormat = ejabberd_auth:password_format(LServer),
    case {PassPlain, PassOldScram, ScramCred, PasswordFormat} of
        {PassPlain, false, false, plain} -> PassPlain;
        {<<"">>, false, ScramCred, plain} -> parse_scram_password(ScramCred);
        {<<"">>, PassOldScram, false, plain} -> parse_scram_password(PassOldScram);
        {PassPlain, false, false, scram} -> PassPlain;
        {<<"">>, false, ScramCred, scram} -> parse_scram_password(ScramCred);
        {<<"">>, PassOldScram, false, scram} -> parse_scram_password(PassOldScram)
    end.


process_user_els([#xmlel{} = El | Els], State) ->
    case process_user_el(El, State) of
        {ok, NewState} ->
            process_user_els(Els, NewState);
        Err ->
            Err
    end;
process_user_els([_ | Els], State) ->
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
                process_vcard(xmpp:decode(El), State);
            {<<"offline-messages">>, NS} ->
                Msgs = [ xmpp:decode(E, NS, [ignore_els]) || E <- Els ],
                process_offline_msgs(Msgs, State);
            {<<"presence">>, ?NS_CLIENT} ->
                process_presence(xmpp:decode(El, ?NS_CLIENT, [ignore_els]), State);
            _ ->
                {ok, State}
        end
    catch
        _:{xmpp_codec, Why} ->
            ErrTxt = xmpp:format_error(Why),
            stop("failed to decode XML '~ts': ~ts",
                 [fxml:element_to_binary(El), ErrTxt])
    end.


-spec process_offline_msgs([stanza()], state()) -> {ok, state()} | {error, _}.
process_offline_msgs([#message{} = Msg | Msgs], State) ->
    case process_offline_msg(Msg, State) of
        {ok, NewState} ->
            process_offline_msgs(Msgs, NewState);
        Err ->
            Err
    end;
process_offline_msgs([_ | Msgs], State) ->
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
process_privacy(#privacy_query{
                  lists = Lists,
                  default = Default,
                  active = Active
                 },
                State = #state{user = U, server = S}) ->
    JID = jid:make(U, S),
    if
        Lists /= undefined ->
            process_privacy2(JID, #privacy_query{lists = Lists});
        true ->
            ok
    end,
    if
        Active /= undefined ->
            process_privacy2(JID, #privacy_query{active = Active});
        true ->
            ok
    end,
    if
        Default /= undefined ->
            process_privacy2(JID, #privacy_query{default = Default});
        true ->
            ok
    end,
    {ok, State}.


process_privacy2(JID, PQ) ->
    case mod_privacy:process_iq(#iq{
                                  type = set,
                                  id = p1_rand:get_string(),
                                  from = JID,
                                  to = JID,
                                  sub_els = [PQ]
                                 }) of
        #iq{type = error} = ResIQ ->
            #stanza_error{reason = Reason} = xmpp:get_error(ResIQ),
            if
                Reason /= 'item-not-found' ->
                    %% Failed to set default list because there is no
                    %% list with such name. We shouldn't stop here.
                    stop("Failed to write default privacy: ~p", [Reason]);
                true ->
                    ok
            end;
        _ ->
            ok
    end.


-spec process_private(private(), state()) -> {ok, state()} | {error, _}.
process_private(Private, State = #state{user = U, server = S}) ->
    JID = jid:make(U, S),
    IQ = #iq{
           type = set,
           id = p1_rand:get_string(),
           from = JID,
           to = JID,
           sub_els = [Private]
          },
    case mod_private:process_sm_iq(IQ) of
        #iq{type = result} ->
            {ok, State};
        Err ->
            stop("Failed to write private: ~p", [Err])
    end.


-spec process_vcard(xmpp_element(), state()) -> {ok, state()} | {error, _}.
process_vcard(El, State = #state{user = U, server = S}) ->
    JID = jid:make(U, S),
    IQ = #iq{
           type = set,
           id = p1_rand:get_string(),
           from = JID,
           to = JID,
           sub_els = [El]
          },
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
%% Example: ``(<<"20080804-231550">>, <<"xmpp.domain.tld">>) ->
%%             <<"20080804-231550_xmpp_domain_tld.xml">>''
make_host_filename(FnT, Host) ->
    Host2 = str:join(str:tokens(Host, <<".">>), <<"_">>),
    <<FnT/binary, "_", Host2/binary, ".xml">>.


%%%==================================
%%%% PIEFXIS formatting
make_host_basefilename(Dir, FnT) ->
    filename:join([Dir, FnT]).


make_piefxis_xml_head() ->
    "<?xml version='1.0' encoding='UTF-8'?>".


make_piefxis_xml_tail() ->
    "".


make_piefxis_server_head() ->
    io_lib:format("<server-data xmlns='~ts' xmlns:xi='~ts'>",
                  [?NS_PIE, ?NS_XI]).


make_piefxis_server_tail() ->
    "</server-data>".


make_piefxis_host_head(Host) ->
    io_lib:format("<host xmlns='~ts' xmlns:xi='~ts' jid='~ts'>",
                  [?NS_PIE, ?NS_XI, Host]).


make_piefxis_host_tail() ->
    "</host>".


make_xinclude(Fn) ->
    Base = filename:basename(Fn),
    io_lib:format("<xi:include href='~ts'/>", [Base]).


print(Fd, String) ->
    file:write(Fd, String).
