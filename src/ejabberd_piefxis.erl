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
%%% - write mod_piefxis with ejabberdctl commands
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

%%-include_lib("exmpp/include/exmpp.hrl").
%%-include_lib("exmpp/include/exmpp_client.hrl").
%% Copied from exmpp header files:
-define(NS_ROSTER,                   "jabber:iq:roster").
-define(NS_VCARD,                    "vcard-temp").
-record(xmlcdata, {
	  cdata = <<>>
	 }).
-record(xmlattr, {
	  ns = undefined,
	  name,
	  value
	 }).
-record(xmlel, {
	  ns = undefined,
	  declared_ns = [],
	  name,
	  attrs = [],
	  children = []
	 }).
-record(iq, {
	  kind,
	  type,
	  id,
	  ns,
	  payload,
	  error,
	  lang,
	  iq_ns
	 }).
-record(xmlendtag, {
	  ns = undefined,
	  name
	 }).


%% Copied from mod_private.erl
-record(private_storage, {usns, xml}).

%%-define(ERROR_MSG(M,Args),io:format(M,Args)).
%%-define(INFO_MSG(M,Args),ok).

-define(CHUNK_SIZE,1024*20). %20k

-define(BTL, binary_to_list).
-define(LTB, list_to_binary).

-define(NS_XINCLUDE, 'http://www.w3.org/2001/XInclude').

%%%==================================

%%%% Import file

import_file(FileName) ->
    _ = #xmlattr{}, %% this stupid line is only to prevent compilation warning about "recod xmlattr is unused"
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
    State#parsing_state{host=?BTL(exmpp_xml:get_attribute(H, <<"jid">>, none))};

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
    User = exmpp_xml:get_attribute(El, <<"name">>, none),
    PasswordFormat = exmpp_xml:get_attribute(El, <<"password-format">>, <<"plaintext">>),
    Password = exmpp_xml:get_attribute(El, <<"password">>, none),
    add_user(El, Domain, User, PasswordFormat, Password).

%% @spec (El::xmlel(), Domain::string(), User::binary(), Password::binary() | none)
%%       -> ok | {error, ErrorText::string()}
%% @doc Add a new user to the database.
%% If user already exists, it will be only updated.
add_user(El, Domain, UserBinary, <<"plaintext">>, none) ->
    User = ?BTL(UserBinary),
    io:format("Account ~s@~s will not be created, updating it...~n",
	      [User, Domain]),
    io:format(""),
    populate_user_with_elements(El, Domain, User),
    ok;
add_user(El, Domain, UserBinary, PasswordFormat, PasswordBinary) ->
    User = ?BTL(UserBinary),
    Password2 = prepare_password(PasswordFormat, PasswordBinary, El),
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
    case ejabberd_auth:try_register(User,Domain,Password) of
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
    case loaded_module(Domain,[mod_roster_odbc,mod_roster]) of
	{ok, M} ->
	    case M:set_items(User, Domain, exmpp_xml:xmlel_to_xmlelement(El)) of
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
	     ?ERROR_MSG("No modules loaded [mod_roster, mod_roster_odbc] ~s ~n",
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
    case loaded_module(Domain,[mod_vcard,mod_vcard_odbc]) of
	{ok, M}  ->  FullUser = jid_to_old_jid(exmpp_jid:make(User, Domain)),
		     IQ = iq_to_old_iq(#iq{type = set, payload = El}),
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
	    ?ERROR_MSG("No modules loaded [mod_vcard, mod_vcard_odbc] ~s ~n",
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
    case loaded_module(Domain, [mod_offline, mod_offline_odbc]) of
	{ok, M} ->
	    ok = exmpp_xml:foreach(
		   fun (_Element, {xmlcdata, _}) ->
			   ok;
		       (_Element, Child) ->
			   From  = exmpp_xml:get_attribute(Child, <<"from">>,none),
			   FullFrom = jid_to_old_jid(exmpp_jid:parse(From)),
			   FullUser = jid_to_old_jid(exmpp_jid:make(User,
								    Domain)),
			   OldChild = exmpp_xml:xmlel_to_xmlelement(Child),
			   _R = M:store_packet(FullFrom, FullUser, OldChild)
		   end, El), io:format(" DONE.~n",[]);
	_ ->
	    io:format(" ERROR.~n",[]),
	    ?ERROR_MSG("No modules loaded [mod_offline, mod_offline_odbc] ~s ~n",
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
    case loaded_module(Domain,[mod_private_odbc,mod_private]) of
	{ok, M} ->
	    FullUser = jid_to_old_jid(exmpp_jid:make(User, Domain)),
	    IQ = iq_to_old_iq(#iq{type = set,
				  ns = 'jabber:iq:private',
				  kind = request,
				  iq_ns = 'jabberd:client',
				  payload = El}),
	    case M:process_sm_iq(FullUser, FullUser, IQ ) of
		{error, _Err} ->
		    io:format(" ERROR.~n",[]),
		    ?ERROR_MSG("Error processing private storage ~s : ~p ~n",
			       [exmpp_xml:document_to_list(El), _Err]);
		_ ->     io:format(" DONE.~n",[]), ok
	    end;
	_ ->
	    io:format(" ERROR.~n",[]),
	    ?ERROR_MSG("No modules loaded [mod_private, mod_private_odbc] ~s ~n",
		       [exmpp_xml:document_to_list(El)]),
	    {error, not_found}
    end;

populate_user(_User, _Domain, #xmlcdata{cdata = _CData}) ->
    ok;

populate_user(_User, _Domain, _El) ->
    ok.

%%%==================================
%%%% Utilities

loaded_module(Domain,Options) ->
    LoadedModules = gen_mod:loaded_modules(Domain),
    case lists:filter(fun(Module) ->
			      lists:member(Module, LoadedModules)
                      end, Options) of
        [M|_] -> {ok, M};
        [] -> {error,not_found}
    end.

jid_to_old_jid(Jid) ->
    {jid, to_list(exmpp_jid:node_as_list(Jid)),
     to_list(exmpp_jid:domain_as_list(Jid)),
     to_list(exmpp_jid:resource_as_list(Jid)),
     to_list(exmpp_jid:prep_node_as_list(Jid)),
     to_list(exmpp_jid:prep_domain_as_list(Jid)),
     to_list(exmpp_jid:prep_resource_as_list(Jid))}.

iq_to_old_iq(#iq{id = ID, type = Type, lang = Lang, ns= NS, payload = El }) ->
    {iq, to_list(ID), Type, to_list(NS), to_list(Lang),
     exmpp_xml:xmlel_to_xmlelement(El)}.

to_list(L) when is_list(L) -> L;
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(undefined) -> "";
to_list(B) when is_atom(B) -> atom_to_list(B).

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
	UserString ->
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
    case loaded_module(Host,[mod_roster_odbc,mod_roster]) of
	{ok, M} ->
	    From = To = jlib:make_jid(Username, Host, ""),
	    SubelGet = {xmlelement, "query", [{"xmlns",?NS_ROSTER}], []},
	    %%IQGet = #iq{type=get, xmlns=?NS_ROSTER, payload=SubelGet}, % this is for 3.0.0 version
	    IQGet = {iq, "", get, ?NS_ROSTER, "" , SubelGet},
	    Res = M:process_local_iq(From, To, IQGet),
	    %%[El] = Res#iq.payload, % this is for 3.0.0 version
	    {iq, _, result, _, _, Els} = Res,
	    case Els of
		[El] -> exmpp_xml:document_to_list(El);
		[] -> ""
	    end;
	_E ->
	    ""
    end;

extract_user_info(offline, Username, Host) ->
    case loaded_module(Host,[mod_offline,mod_offline_odbc]) of
	{ok, mod_offline} ->
	    Els = mnesia_pop_offline_messages([], Username, Host),
	    case Els of
		[] -> "";
		Els ->
		    OfEl = {xmlelement, "offline-messages", [], Els},
		    exmpp_xml:document_to_list(OfEl)
	    end;
	{ok, mod_offline_odbc} ->
	    "";
	_E ->
	    ""
    end;

extract_user_info(private, Username, Host) ->
    case loaded_module(Host,[mod_private,mod_private_odbc]) of
	{ok, mod_private} ->
	    get_user_private_mnesia(Username, Host);
	{ok, mod_private_odbc} ->
	    "";
	_E ->
	    ""
    end;

extract_user_info(vcard, Username, Host) ->
    case loaded_module(Host,[mod_vcard, mod_vcard_odbc, mod_vcard_odbc]) of
	{ok, M} ->
	    From = To = jlib:make_jid(Username, Host, ""),
	    SubelGet = {xmlelement, "vCard", [{"xmlns",?NS_VCARD}], []},
	    %%IQGet = #iq{type=get, xmlns=?NS_VCARD, payload=SubelGet}, % this is for 3.0.0 version
	    IQGet = {iq, "", get, ?NS_VCARD, "" , SubelGet},
	    Res = M:process_sm_iq(From, To, IQGet),
	    %%[El] = Res#iq.payload, % this is for 3.0.0 version
	    {iq, _, result, _, _, Els} = Res,
	    case Els of
		[El] -> exmpp_xml:document_to_list(El);
		[] -> ""
	    end;
	_E ->
	    ""
    end.

%%%==================================
%%%% Interface with ejabberd offline storage

%% Copied from mod_offline.erl and customized
-record(offline_msg, {us, timestamp, expire, from, to, packet}).
mnesia_pop_offline_messages(Ls, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		Rs = mnesia:wread({offline_msg, US}),
		%%mnesia:delete({offline_msg, US}),
		Rs
	end,
    case mnesia:transaction(F) of
	{atomic, Rs} ->
	    TS = now(),
	    Ls ++ lists:map(
		    fun(R) ->
			    {xmlelement, Name, Attrs, Els} = R#offline_msg.packet,
			    FromString = jlib:jid_to_string(R#offline_msg.from),
			    Attrs2 = lists:keystore("from", 1, Attrs, {"from", FromString}),
			    Attrs3 = lists:keystore("xmlns", 1, Attrs2, {"xmlns", "jabber:client"}),
			    {xmlelement, Name, Attrs3,
			     Els ++
			     [jlib:timestamp_to_xml(
				calendar:now_to_universal_time(
				  R#offline_msg.timestamp))]}
		    end,
		    lists:filter(
		      fun(R) ->
			      case R#offline_msg.expire of
				  never ->
				      true;
				  TimeStamp ->
				      TS < TimeStamp
			      end
		      end,
		      lists:keysort(#offline_msg.timestamp, Rs)));
	_ ->
	    Ls
    end.

%%%==================================
%%%% Interface with ejabberd private storage

get_user_private_mnesia(Username, Host) ->
    ListNsEl = mnesia:dirty_select(private_storage,
				   [{#private_storage{usns={Username, Host, '$1'}, xml = '$2'},
				     [], ['$$']}]),
    Els = [exmpp_xml:document_to_list(El) || [_Ns, El] <- ListNsEl],
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
