%%%----------------------------------------------------------------------
%%% File    : ejabberd_piefxis.erl
%%% Author  : Pablo Polvorin, Vidal Santiago Martinez
%%% Purpose : XEP-0227: Portable Import/Export Format for XMPP-IM Servers
%%% Created : 17 Jul 2008 by Pablo Polvorin <pablo.polvorin@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%%% Not implemented:
%%% - Export from mod_offline_odbc.erl
%%% - Export from mod_private_odbc.erl
%%% - XEP-227: 6. Security Considerations
%%% - Other schemas of XInclude are not tested, and may not be imported correctly.
%%% - If a host has many users, split that host in XML files with 50 users each.

%%%% Headers

-module(ejabberd_piefxis).

-export([import_file/1, export_server/1, export_host/2]).

-record(parsing_state, {parser, host, dir}).

-include("ejabberd.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%% Copied from mod_private.erl
-record(private_storage, {user_host_ns, xml}).

%%-define(ERROR_MSG(M,Args),io:format(M,Args)).
%%-define(INFO_MSG(M,Args),ok).

-define(CHUNK_SIZE,1024*20). %20k

-define(BTL, binary_to_list).
-define(LTB, list_to_binary).

-define(NS_XINCLUDE, 'http://www.w3.org/2001/XInclude').

%%%==================================

%%%% Import file

import_file(FileName) ->
    import_file(FileName, 2).

import_file(FileName, RootDepth) ->
    try_start_exmpp(),
    Dir = filename:dirname(FileName),
    {ok, IO} = try_open_file(FileName),
    Parser = exmpp_xml:start_parser([{max_size,infinity},
				     {root_depth, RootDepth},
				     {emit_endtag,true}]),
    read_chunks(IO, #parsing_state{parser=Parser, dir=Dir}),
    file:close(IO),
    exmpp_xml:stop_parser(Parser).

try_start_exmpp() ->
    try exmpp:start()
    catch
	error:{already_started, exmpp} -> ok;
	  error:undef -> throw({error, exmpp_not_installed})
    end.

try_open_file(FileName) ->
    case file:open(FileName,[read,binary]) of
	{ok, IO} -> {ok, IO};
	{error, enoent} -> throw({error, {file_not_found, FileName}})
    end.

%%File could be large.. we read it in chunks
read_chunks(IO,State) ->
    case file:read(IO,?CHUNK_SIZE) of
        {ok,Chunk} ->
            NewState = process_chunk(Chunk,State),
            read_chunks(IO,NewState);
        eof ->
            ok
    end.

process_chunk(Chunk,S =#parsing_state{parser=Parser}) ->
    case exmpp_xml:parse(Parser,Chunk) of
        continue ->
            S;
        XMLElements ->
            process_elements(XMLElements,S)
    end.

%%%==================================
%%%% Process Elements

process_elements(Elements,State) ->
    lists:foldl(fun process_element/2,State,Elements).

%%%==================================
%%%% Process Element

process_element(El=#xmlel{name=user, ns=_XMLNS},
		State=#parsing_state{host=Host}) ->
    case add_user(El,Host) of
	ok -> ok;
	{error, _Other} -> error
    end,
    State;

process_element(H=#xmlel{name=host},State) ->
    State#parsing_state{host=exmpp_xml:get_attribute(H,<<"jid">>,none)};

process_element(#xmlel{name='server-data'},State) ->
    State;

process_element(El=#xmlel{name=include, ns=?NS_XINCLUDE}, State=#parsing_state{dir=Dir}) ->
    case exmpp_xml:get_attribute(El, <<"href">>, none) of
	none ->
	    ok;
	HrefB ->
	    Href = binary_to_list(HrefB),
	    %%?INFO_MSG("Parse also this file: ~n~p", [Href]),
	    FileName = filename:join([Dir, Href]),
	    import_file(FileName, 1),
	    Href
    end,
    State;

process_element(#xmlcdata{cdata = _CData},State) ->
    State;

process_element(#xmlendtag{ns = _NS, name='server-data'},State) ->
    State;

process_element(#xmlendtag{ns = _NS, name=_Name},State) ->
    State;

process_element(El,State) ->
    io:format("Warning!: unknown element found: ~p ~n",[El]),
    State.

%%%==================================
%%%% Add user

add_user(El, Domain) ->
    User = exmpp_xml:get_attribute(El,<<"name">>,none),
    PasswordFormat = exmpp_xml:get_attribute(El,<<"password-format">>,<<"plaintext">>),
    Password = exmpp_xml:get_attribute(El,<<"password">>,none),
    add_user(El, Domain, User, PasswordFormat, Password).

%% @spec (El::xmlel(), Domain::string(), User::binary(), PasswordFormat::binary(), Password::binary() | none)
%%       -> ok | {error, ErrorText::string()}
%% @doc Add a new user to the database.
%% If user already exists, it will be only updated.
add_user(El, Domain, User, <<"plaintext">>, none) ->
    io:format("Account ~s@~s will not be created, updating it...~n",
	      [User, Domain]),
    io:format(""),
    populate_user_with_elements(El, Domain, User),
    ok;
add_user(El, Domain, User, <<"scram">> = PasswordFormat, Password) ->
    Password2 = prepare_password(PasswordFormat, Password, El),
    case create_user(User,Password2,Domain) of
	ok ->
	    populate_user_with_elements(El, Domain, User),
	    ok;
	{atomic, exists} ->
	    io:format("Account ~s@~s already exists, updating it...~n",
		      [User, Domain]),
	    io:format(""),
	    populate_user_with_elements(El, Domain, User),
	    ok;
	{error, Other} ->
	    ?ERROR_MSG("Error adding user ~s@~s: ~p~n", [User, Domain, Other]),
	    {error, Other}
    end.

prepare_password(<<"plaintext">>, PasswordBinary, _El) ->
    ?BTL(PasswordBinary);
prepare_password(<<"scram">>, none, El) ->
    ScramEl = exmpp_xml:get_element(El, 'scram-hash'),
    #scram{storedkey = base64:decode(exmpp_xml:get_attribute(
					ScramEl, <<"stored-key">>, none)),
	   serverkey = base64:decode(exmpp_xml:get_attribute(
					ScramEl, <<"server-key">>, none)),
	   salt = base64:decode(exmpp_xml:get_attribute(
				  ScramEl, <<"salt">>, none)),
	   iterationcount = list_to_integer(exmpp_xml:get_attribute_as_list(
					       ScramEl, <<"iteration-count">>,
					       ?SCRAM_DEFAULT_ITERATION_COUNT))
	  }.

populate_user_with_elements(El, Domain, User) ->
    exmpp_xml:foreach(
      fun (_,Child) ->
	      populate_user(User,Domain,Child)
      end,
      El).

%% @spec (User::string(), Password::string(), Domain::string())
%%       -> ok | {atomic, exists} | {error, not_allowed}
%% @doc  Create a new user
create_user(User,Password,Domain) ->
    case ejabberd_auth:try_register(?BTL(User),?BTL(Domain),Password) of
	{atomic,ok} -> ok;
	{atomic, exists} -> {atomic, exists};
	{error, not_allowed} -> {error, not_allowed};
	Other -> {error, Other}
    end.

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

populate_user(User,Domain,El=#xmlel{name='query', ns='jabber:iq:roster'}) ->
    io:format("Trying to add/update roster list...",[]),
    case loaded_module(Domain,[mod_roster]) of
	{ok, M} ->
	    case M:set_items(User, Domain, El) of
		{atomic, ok} ->
		    io:format(" DONE.~n",[]),
		    ok;
		_ ->
		    io:format(" ERROR.~n",[]),
		    ?ERROR_MSG("Error trying to add a new user: ~s ~n",
			       [exmpp_xml:document_to_list(El)]),
		    {error, not_found}
	    end;
	E -> io:format(" ERROR: ~p~n",[E]),
	     ?ERROR_MSG("Module not loaded: mod_roster ~s ~n",
			[exmpp_xml:document_to_list(El)]),
	     {error, not_found}
    end;


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

populate_user(User,Domain,El=#xmlel{name='vCard', ns='vcard-temp'}) ->
    io:format("Trying to add/update vCards...",[]),
    case loaded_module(Domain,[mod_vcard]) of
	{ok, M}  ->  FullUser = exmpp_jid:make(User, Domain),
		     IQ = #iq{kind=request, type = set, payload = El},
		     case M:process_sm_iq(FullUser, FullUser , IQ) of
			 {error,_Err} ->
			     io:format(" ERROR.~n",[]),
			     ?ERROR_MSG("Error processing vcard ~s : ~p ~n",
					[exmpp_xml:document_to_list(El), _Err]);
			 _ ->
			     io:format(" DONE.~n",[]), ok
		     end;
	_ ->
	    io:format(" ERROR.~n",[]),
	    ?ERROR_MSG("Module not loaded: mod_vcard ~s ~n",
		       [exmpp_xml:document_to_list(El)]),
	    {error, not_found}
    end;

%% @spec User   = String with the user name
%%       Domain = String with a domain name
%%       El     = Sub XML element with offline messages values
%% @ret  ok | {error, not_found}
%% @doc  Read off-line message from the XML and send it to the server

populate_user(User,Domain,El=#xmlel{name='offline-messages'}) ->
    io:format("Trying to add/update offline-messages...",[]),
    case loaded_module(Domain, [mod_offline]) of
	{ok, M} ->
	    ok = exmpp_xml:foreach(
		   fun (_Element, {xmlcdata, _}) ->
			   ok;
		       (_Element, Child) ->
			   From  = exmpp_xml:get_attribute(Child,<<"from">>,none),
			   FullFrom = exmpp_jid:parse(From),
			   FullUser = exmpp_jid:make(User, Domain),
			   _R = M:store_packet(FullFrom, FullUser, Child)
		   end, El), io:format(" DONE.~n",[]);
	_ ->
	    io:format(" ERROR.~n",[]),
	    ?ERROR_MSG("Module not loaded: mod_offline ~s ~n",
		       [exmpp_xml:document_to_list(El)]),
	    {error, not_found}
    end;

%% @spec User   = String with the user name
%%       Domain = String with a domain name
%%       El     = Sub XML element with private storage values
%% @ret  ok | {error, not_found}
%% @doc  Private storage parsing

populate_user(User,Domain,El=#xmlel{name='query', ns='jabber:iq:private'}) ->
    io:format("Trying to add/update private storage...",[]),
    case loaded_module(Domain,[mod_private]) of
	{ok, M} ->
	    FullUser = exmpp_jid:make(User, Domain),
	    IQ = #iq{type = set,
		     ns = 'jabber:iq:private',
		     kind = request,
		     iq_ns = 'jabberd:client',
		     payload = El},
	    case M:process_sm_iq(FullUser, FullUser, IQ ) of
		{error, _Err} ->
		    io:format(" ERROR.~n",[]),
		    ?ERROR_MSG("Error processing private storage ~s : ~p ~n",
			       [exmpp_xml:document_to_list(El), _Err]);
		_ ->     io:format(" DONE.~n",[]), ok
	    end;
	_ ->
	    io:format(" ERROR.~n",[]),
	    ?ERROR_MSG("Module not loaded: mod_private ~s~n",
		       [exmpp_xml:document_to_list(El)]),
	    {error, not_found}
    end;

populate_user(_User, _Domain, #xmlcdata{cdata = _CData}) ->
    ok;

populate_user(_User, _Domain, _El) ->
    ok.

%%%==================================
%%%% Utilities

loaded_module(Domain,Options) when is_binary(Domain) ->
    loaded_module(?BTL(Domain),Options);
loaded_module(Domain,Options) ->
    LoadedModules = gen_mod:loaded_modules(Domain),
    case lists:filter(fun(Module) ->
			      lists:member(Module, LoadedModules)
                      end, Options) of
        [M|_] -> {ok, M};
        [] -> {error,not_found}
    end.

%%%==================================

%%%% Export hosts

%% @spec (Dir::string(), Hosts::[string()]) -> ok
export_hosts(Dir, Hosts) ->
    try_start_exmpp(),

    FnT = make_filename_template(),
    DFn = make_main_basefilename(Dir, FnT),

    {ok, Fd} = file_open(DFn),
    print(Fd, make_piefxis_xml_head()),
    print(Fd, make_piefxis_server_head()),

    FilesAndHosts = [{make_host_filename(FnT, Host), Host} || Host <- Hosts],
    [print(Fd, make_xinclude(FnH)) || {FnH, _Host} <- FilesAndHosts],

    print(Fd, make_piefxis_server_tail()),
    print(Fd, make_piefxis_xml_tail()),
    file_close(Fd),

    [export_host(Dir, FnH, Host) || {FnH, Host} <- FilesAndHosts],

    ok.

%%%==================================
%%%% Export server

%% @spec (Dir::string()) -> ok
export_server(Dir) ->
    Hosts = ?MYHOSTS,
    export_hosts(Dir, Hosts).

%%%==================================
%%%% Export host

%% @spec (Dir::string(), Host::string()) -> ok
export_host(Dir, Host) ->
    Hosts = [Host],
    export_hosts(Dir, Hosts).

%% @spec (Dir::string(), Fn::string(), Host::string()) -> ok
export_host(Dir, FnH, Host) ->

    DFn = make_host_basefilename(Dir, FnH),

    {ok, Fd} = file_open(DFn),
    print(Fd, make_piefxis_xml_head()),
    print(Fd, make_piefxis_host_head(Host)),

    Users = ejabberd_auth:get_vh_registered_users(Host),
    [export_user(Fd, Username, Host) || {Username, _Host} <- Users],
    timer:sleep(500), % Delay to ensure ERROR_MSG are displayed in the shell

    print(Fd, make_piefxis_host_tail()),
    print(Fd, make_piefxis_xml_tail()),
    file_close(Fd).

%%%==================================
%%%% PIEFXIS formatting

%% @spec () -> string()
make_piefxis_xml_head() ->
    "<?xml version='1.0' encoding='UTF-8'?>".

%% @spec () -> string()
make_piefxis_xml_tail() ->
    "".

%% @spec () -> string()
make_piefxis_server_head() ->
    "<server-data"
	" xmlns='http://www.xmpp.org/extensions/xep-0227.html#ns'"
	" xmlns:xi='http://www.w3.org/2001/XInclude'>".

%% @spec () -> string()
make_piefxis_server_tail() ->
    "</server-data>".

%% @spec (Host::string()) -> string()
make_piefxis_host_head(Host) ->
    NSString =
	" xmlns='http://www.xmpp.org/extensions/xep-0227.html#ns'"
	" xmlns:xi='http://www.w3.org/2001/XInclude'",
    io_lib:format("<host~s jid='~s'>", [NSString, Host]).

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
export_user(Fd, Username, Host) ->
    try extract_user(Username, Host) of
	UserString when is_list(UserString) ->
	    print(Fd, UserString)
    catch
	E1:E2 ->
	    ?ERROR_MSG("The account ~s@~s is not exported because a problem "
		       "was found in it:~n~p: ~p", [Username, Host, E1, E2])
    end.

%% @spec (Username::string(), Host::string()) -> string()
extract_user(Username, Host) ->
    Password = ejabberd_auth:get_password(Username, Host),
    PasswordStr = build_password_string(Password),
    UserInfo = [extract_user_info(InfoName, Username, Host) || InfoName <- [roster, offline, private, vcard]],
    UserInfoString = lists:flatten(UserInfo),
    io_lib:format("<user name='~s' ~s ~s</user>",
		  [Username, PasswordStr, UserInfoString]).

build_password_string({StoredKey, ServerKey, Salt, IterationCount}) ->
    io_lib:format("password-format='scram'>"
		  "<scram-hash stored-key='~s' server-key='~s' "
		  "salt='~s' iteration-count='~w'/> ",
		  [base64:encode_to_string(StoredKey),
		   base64:encode_to_string(ServerKey),
		   base64:encode_to_string(Salt),
		   IterationCount]);
build_password_string(Password) when is_list(Password) ->
    io_lib:format("password-format='plaintext' password='~s'>", [Password]).

%% @spec (InfoName::atom(), Username::string(), Host::string()) -> string()
extract_user_info(roster, Username, Host) ->
    case loaded_module(Host,[mod_roster]) of
	{ok, M} ->
	    From = To = exmpp_jid:make(Username, Host, ""),
	    SubelGet = exmpp_xml:element(?NS_ROSTER, 'query', [], []),
	    IQGet = #iq{kind=request, type=get, ns=?NS_ROSTER, payload=SubelGet},
	    Res = M:process_local_iq(From, To, IQGet),
	    case Res#iq.payload of
		undefined -> "";
		El -> exmpp_xml:document_to_list(El)
	    end;
	_E ->
	    ""
    end;

extract_user_info(offline, Username, Host) ->
    case loaded_module(Host,[mod_offline]) of
	{ok, mod_offline} ->
	    Els = mnesia_pop_offline_messages([], Username, Host),
	    case Els of
		[] -> "";
		Els ->
                    OfEl = #xmlel{name = 'offline-messages',
                                  children = Els},
		    %OfEl = {xmlelement, "offline-messages", [], Els},
		    exmpp_xml:document_to_list(OfEl)
	    end;
	_E ->
	    ""
    end;

extract_user_info(private, Username, Host) ->
    case loaded_module(Host,[mod_private]) of
	{ok, mod_private} ->
	    get_user_private_mnesia(Username, Host);
	_E ->
	    ""
    end;

extract_user_info(vcard, Username, Host) ->
    case loaded_module(Host,[mod_vcard]) of
	{ok, M} ->
	    From = To = exmpp_jid:make(Username, Host, ""),
	    SubelGet = exmpp_xml:element(?NS_VCARD, 'vCard', [], []),
	    IQGet = #iq{kind=request, type=get, ns=?NS_VCARD, payload=SubelGet},
	    Res = M:process_sm_iq(From, To, IQGet),
	    case Res#iq.payload of
		undefined -> "";
		El -> exmpp_xml:document_to_list(El)
	    end;
	_E ->
	    ""
    end.

%%%==================================
%%%% Interface with ejabberd offline storage

%% Copied from mod_offline.erl and customized
-record(offline_msg, {user_host, timestamp, expire, from, to, packet}).
mnesia_pop_offline_messages(Ls, User, Server) ->
    try
	LUser = User,
	LServer = Server,
	US = {LUser, LServer},
	F = fun() ->
		    Rs = mnesia:wread({offline_msg, US}),
		    %% mnesia:delete({offline_msg, US}),
		    Rs
	    end,
	case mnesia:transaction(F) of
	    {atomic, Rs} ->
		TS = make_timestamp(),
		Ls ++ lists:map(
			fun(R) ->
				[Packet] = exmpp_xml:parse_document(R#offline_msg.packet, [names_as_atom]),
				FromString = exmpp_jid:prep_to_list(R#offline_msg.from),
				Packet2 = exmpp_xml:set_attribute(Packet, <<"from">>, FromString),
				Packet3 = Packet2#xmlel{ns = ?NS_JABBER_CLIENT},
				exmpp_xml:append_children(
				  Packet3,
				  [jlib:timestamp_to_xml(
				     calendar:gregorian_seconds_to_datetime(
				       R#offline_msg.timestamp),
				     utc,
				     exmpp_jid:make("", Server, ""),
				     "Offline Storage"),
				   %% TODO: Delete the next three lines once XEP-0091 is Obsolete
				   jlib:timestamp_to_xml(
				     calendar:gregorian_seconds_to_datetime(
				       R#offline_msg.timestamp))]
				 )
			end,
			lists:filter(
			  fun(R) ->
				  case R#offline_msg.expire of
				      0 ->
					  true;
				      TimeStamp ->
					  TS < TimeStamp
				  end
			  end,
			  lists:keysort(#offline_msg.timestamp, Rs)));
	    _ ->
		Ls
	end
    catch
	_ ->
	    Ls
    end.

make_timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    MegaSecs * 1000000 + Secs.

%%%==================================
%%%% Interface with ejabberd private storage

get_user_private_mnesia(Username, Host) ->
    ListNsEl = mnesia:dirty_select(private_storage,
				   [{#private_storage{user_host_ns={?LTB(Username), ?LTB(Host), '$1'}, xml = '$2'},
				     [], ['$$']}]),
    Els = [lists:flatten(exmpp_xml:document_to_list(El)) || [_Ns, El] <- ListNsEl],
    case lists:flatten(Els) of
	"" -> "";
	ElsString ->
	    io_lib:format("<query xmlns='jabber:iq:private'>~s</query>", [ElsString])
    end.

%%%==================================
%%%% Disk file access

%% @spec () -> string()
make_filename_template() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(
      io_lib:format("~4..0w~2..0w~2..0w-~2..0w~2..0w~2..0w",
		    [Year, Month, Day, Hour, Minute, Second])).

%% @spec (Dir::string(), FnT::string()) -> string()
make_main_basefilename(Dir, FnT) ->
    Filename2 = filename:flatten([FnT, ".xml"]),
    filename:join([Dir, Filename2]).

%% @spec (FnT::string(), Host::string()) -> FnH::string()
%% @doc Make the filename for the host.
%% Example: ``("20080804-231550", "jabber.example.org") -> "20080804-231550_jabber_example_org.xml"''
make_host_filename(FnT, Host) ->
    Host2 = string:join(string:tokens(Host, "."), "_"),
    filename:flatten([FnT, "_", Host2, ".xml"]).

make_host_basefilename(Dir, FnT) ->
    filename:join([Dir, FnT]).

%% @spec (Fn::string()) -> {ok, Fd}
file_open(Fn) ->
    file:open(Fn, [write]).

%% @spec (Fd) -> ok
file_close(Fd) ->
    file:close(Fd).

%% @spec (Fd, String::string()) -> ok
print(Fd, String) ->
    io:format(Fd, String, []).

%%%==================================

%%% vim: set filetype=erlang tabstop=8 foldmarker=%%%%,%%%= foldmethod=marker:
