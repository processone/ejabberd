%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

-module(ts_jabber_common).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([ get_message/1
         ]).

-include("ts_profile.hrl").
-include("ts_jabber.hrl").


%%----------------------------------------------------------------------
%% Func: get_message/1
%% Args: #jabber record
%% Returns: binary
%% Purpose: Build a message/request from a #jabber record
%%----------------------------------------------------------------------
get_message(Jabber=#jabber{type = 'connect'}) ->
    connect(Jabber);
get_message(#jabber{type = 'close', id=Id,user_server=UserServer}) ->
    ts_user_server:remove_connected(UserServer,Id),
    close();
get_message(#jabber{type = 'presence'}) ->
    presence();
get_message(#jabber{type = 'presence:initial', id=Id,user_server=UserServer}) ->
    ts_user_server:add_to_online(UserServer,Id),
    presence();
get_message(#jabber{type = 'presence:final', id=Id,user_server=UserServer}) ->
    ts_user_server:remove_from_online(UserServer,Id),
    presence(unavailable);
get_message(#jabber{type = 'presence:broadcast', show=Show, status=Status}) ->
    presence(broadcast, Show, Status);
get_message(Jabber=#jabber{type = 'presence:directed', id=Id,
                           show=Show, status=Status,user_server=UserServer}) ->
    case ts_user_server:get_online(UserServer,Id) of
        {ok, Dest} ->
            presence(directed, Dest, Jabber, Show, Status);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;

get_message(Jabber=#jabber{id=Id}) when is_integer(Id)->
    get_message(Jabber#jabber{id=id_to_string(Id)});
get_message(Jabber=#jabber{dest=previous}) ->
    Dest = get(previous),
    get_message(Jabber#jabber{dest=Dest});
get_message(Jabber=#jabber{type = 'presence:roster'}) ->
    presence(roster, Jabber);
get_message(#jabber{type = 'presence:subscribe'}) -> %% must be called AFTER iq:roster:add
    case get(rosterjid) of
        undefined ->
            ?LOG("Warn: no jid set for presence subscribe, skip",?WARN),
            <<>>;
        RosterJid ->
            presence(subscribe, RosterJid)
    end;
get_message(Jabber=#jabber{type = 'chat', id=Id, dest=online,
                           domain=Domain,user_server=UserServer})->
    case ts_user_server:get_online(UserServer,Id) of
        {ok, Dest} ->
            message(Dest, Jabber, Domain);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;

get_message(Jabber=#jabber{type = 'chat', domain = Domain,
                           dest=offline,user_server=UserServer}) ->
    case ts_user_server:get_offline(UserServer) of
        {ok, Dest} ->
            message(Dest, Jabber, Domain);
        {error, no_offline} ->
            ts_mon:add({ count, error_no_offline }),
            << >>
    end;
get_message(Jabber=#jabber{type = 'chat', dest=random, domain=Domain,user_server=UserServer}) ->
    Dest = ts_user_server:get_id(UserServer),
    message(Dest, Jabber, Domain);
get_message(Jabber=#jabber{type = 'chat', dest=unique, domain=Domain,user_server=UserServer})->
    {Dest, _} = ts_user_server:get_first(UserServer),
    message(Dest, Jabber, Domain);
get_message(Jabber=#jabber{type = 'chat', id=_Id, dest = Dest, domain=Domain}) ->
    ?DebugF("~w -> ~w ~n", [_Id,  Dest]),
    message(Dest, Jabber, Domain);
get_message(#jabber{type = 'iq:roster:add', id=Id, dest = online, username=User,
                    domain=Domain, group=Group,user_server=UserServer}) ->
    case ts_user_server:get_online(UserServer,Id) of
        {ok, Dest} ->
            request(roster_add, User, Domain, Dest, Group);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;
get_message(#jabber{type = 'iq:roster:add',dest = offline, username=User,
                    domain=Domain, group=Group, user_server=UserServer})->
    case ts_user_server:get_offline(UserServer) of
        {ok, Dest} ->
            request(roster_add, User, Domain, Dest, Group);
        {error, no_offline} ->
            ts_mon:add({ count, error_no_offline }),
            << >>
    end;
get_message(#jabber{type = 'iq:roster:rename', group=Group})-> %% must be called AFTER iq:roster:add
    case get(rosterjid) of
        undefined ->
            ?LOG("Warn: no jid set for iq:roster:rename msg, skip",?WARN),
            <<>>;
        RosterJid ->
            request(roster_rename, RosterJid, Group)
    end;
get_message(#jabber{type = 'iq:roster:remove'})-> %% must be called AFTER iq:roster:add
    case get(rosterjid) of
        undefined ->
            ?LOG("Warn: no jid set for iq:roster:remove msg, skip",?WARN),
            <<>>;
        RosterJid ->
            request(roster_remove, RosterJid)
    end;
get_message(#jabber{type = 'iq:roster:get', id = Id,username=User,domain=Domain}) ->
    request(roster_get, User, Domain, Id);

get_message(Jabber=#jabber{type = 'raw'}) ->
    raw(Jabber);

%% -- Pubsub benchmark support --
%% For node creation, data contains the pubsub nodename (relative to user
%% hierarchy or absolute, optional)
get_message(#jabber{type = 'pubsub:create', id=Id, username=User,
                    node=Node, node_type=NodeType, pubsub_service = PubSubComponent, domain = Domain}) ->
    Username = username(User,Id),
    create_pubsub_node(Domain, PubSubComponent, Username, Node, NodeType);
%% For node subscription, data contain the pubsub nodename (relative to user
%% hierarchy or absolute)
get_message(#jabber{type = 'pubsub:subscribe', id=Id, username=User, user_server=UserServer,
                    dest=online, node=Node, pubsub_service = PubSubComponent, domain = Domain}) ->
    case ts_user_server:get_online(UserServer,Id) of
        {ok, Dest} ->
            UserFrom = username(User,Id),
            UserTo = username(User, id_to_string(Dest)),
            subscribe_pubsub_node(Domain, PubSubComponent, UserFrom, UserTo, Node);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;
get_message(#jabber{type = 'pubsub:subscribe', id=Id, username=User, user_server=UserServer,
                    dest=offline, node=Node, domain = Domain, pubsub_service = PubSubComponent}) ->
    case ts_user_server:get_offline(UserServer) of
        {ok, Dest} ->
            UserFrom = username(User,Id),
            UserTo = username(User,id_to_string(Dest)),
            subscribe_pubsub_node(Domain, PubSubComponent, UserFrom, UserTo, Node);
        {error, no_offline} ->
            ts_mon:add({ count, error_no_offline }),
            << >>
    end;
get_message(#jabber{type = 'pubsub:subscribe', id=Id, username=User, user_server=UserServer,
                    dest=random, node=Node, domain = Domain, pubsub_service = PubSubComponent}) ->
    Dest = ts_user_server:get_id(UserServer),
    UserFrom = username(User,Id),
    UserTo = username(User,id_to_string(Dest)),
    subscribe_pubsub_node(Domain, PubSubComponent, UserFrom, UserTo, Node);

get_message(#jabber{type = 'pubsub:subscribe', id=Id, username=User,
                    dest=UserTo, node=Node, domain = Domain, pubsub_service = PubSubComponent}) ->
    UserFrom = username(User,Id),
    subscribe_pubsub_node(Domain, PubSubComponent, UserFrom, UserTo, Node);

%% For node publication, data contain the pubsub nodename (relative to user
%% hierarchy or absolute)
get_message(#jabber{type = 'pubsub:publish', size=Size, id=Id,
                    username=User, node=Node, pubsub_service = PubSubComponent, domain = Domain}) ->
    Username = username(User,Id),
    publish_pubsub_node(Domain, PubSubComponent, Username, Node, Size);


%% MUC benchmark support
get_message(#jabber{type = 'muc:join', room = Room, nick = Nick, muc_service = Service }) ->
    muc_join(Room,Nick, Service);
get_message(#jabber{type = 'muc:chat', room = Room, muc_service = Service, size = Size}) ->
    muc_chat(Room, Service, Size);
get_message(#jabber{type = 'muc:nick', room = Room, muc_service = Service, nick = Nick}) ->
    muc_nick(Room, Nick, Service);
get_message(#jabber{type = 'muc:exit', room = Room, muc_service = Service, nick = Nick}) ->
    muc_exit(Room, Nick, Service);


%% Privacy lists benchmark support
get_message(#jabber{type = 'privacy:get_names', username=User, id=Id, domain=Domain}) ->
    privacy_get_names(username(User,Id), Domain);
get_message(#jabber{type = 'privacy:set_active', username=User, id=Id, domain=Domain}) ->
    privacy_set_active(username(User,Id), Domain);


get_message(Jabber=#jabber{username=Name, passwd=Passwd, id=Id}) ->
    FullName = username(Name, Id),
    FullPasswd = password(Passwd,Id),
    get_message2(Jabber#jabber{username=FullName,passwd=FullPasswd}).


%%----------------------------------------------------------------------
%% Func: get_message2/1
%%----------------------------------------------------------------------
get_message2(Jabber=#jabber{type = 'register'}) ->
    registration(Jabber);
get_message2(Jabber=#jabber{type = 'auth_get'}) ->
    auth_get(Jabber);
get_message2(Jabber=#jabber{type = 'auth_set_plain'}) ->
    auth_set_plain(Jabber);
get_message2(Jabber=#jabber{type = 'auth_set_digest', sid=Sid}) ->
    auth_set_digest(Jabber,Sid);
get_message2(Jabber=#jabber{type = 'auth_set_sip', domain=Realm, nonce=Nonce}) ->
    auth_set_sip(Jabber,Nonce,Realm);
get_message2(Jabber=#jabber{type = 'auth_sasl'}) ->
    auth_sasl(Jabber,"PLAIN");
get_message2(Jabber=#jabber{type = 'auth_sasl_anonymous'}) ->
    auth_sasl(Jabber,"ANONYMOUS");
get_message2(Jabber=#jabber{type = 'auth_sasl_bind'}) ->
    auth_sasl_bind(Jabber);
get_message2(Jabber=#jabber{type = 'auth_sasl_session'}) ->
    auth_sasl_session(Jabber).


%%----------------------------------------------------------------------
%% Func: connect/1
%%----------------------------------------------------------------------
connect(#jabber{domain=Domain}) ->
    list_to_binary([
      "<?xml version='1.0'?><stream:stream  id='",
      ts_msg_server:get_id(list),
      "' to='",
      Domain,
      "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>"]).

%%----------------------------------------------------------------------
%% Func: close/0
%% Purpose: close jabber session
%%----------------------------------------------------------------------
close () -> list_to_binary("</stream:stream>").

%%----------------------------------------------------------------------
%% Func: auth_get/1
%%----------------------------------------------------------------------
auth_get(#jabber{username=Name,passwd=Passwd})->
    auth_get(Name, Passwd, "auth").

%%----------------------------------------------------------------------
%% Func: auth_get/3
%%----------------------------------------------------------------------
auth_get(Username, _Passwd, Type) ->
 list_to_binary([
   "<iq id='", ts_msg_server:get_id(list),
   "' type='get' >",
   "<query xmlns='jabber:iq:", Type, "'>",
   "<username>", Username, "</username></query></iq>"]).

%%----------------------------------------------------------------------
%% Func: auth_set_plain/1
%%----------------------------------------------------------------------
auth_set_plain(#jabber{username=Name,passwd=Passwd})->
    auth_set_plain(Name, Passwd, "auth").


%%----------------------------------------------------------------------
%% Func: auth_set_plain/3
%%----------------------------------------------------------------------
auth_set_plain(Username, Passwd, Type) ->
 list_to_binary([
   "<iq id='", ts_msg_server:get_id(list),
   "' type='set' >",
   "<query xmlns='jabber:iq:", Type, "'>",
   "<username>", Username, "</username>",
   "<resource>tsung</resource>",
   "<password>", Passwd, "</password></query></iq>"]).


%%----------------------------------------------------------------------
%% Func: auth_set_digest/2
%%----------------------------------------------------------------------
auth_set_digest(#jabber{username=Name,passwd=Passwd}, Sid)->
        auth_set_digest(Name, Passwd, "auth", Sid).


%%----------------------------------------------------------------------
%% Func: auth_set_digest/4
%%----------------------------------------------------------------------
auth_set_digest(Username, Passwd, Type, Sid) ->
 {Digest} = ts_digest:digest(Sid, Passwd),
 list_to_binary([
   "<iq id='", ts_msg_server:get_id(list),
   "' type='set' >",
   "<query xmlns='jabber:iq:", Type, "'>",
   "<username>", Username, "</username>",
   "<resource>tsung</resource>",
   "<digest>", Digest, "</digest></query></iq>"]).


%%----------------------------------------------------------------------
%% Func: auth_set_sip/3
%%----------------------------------------------------------------------
auth_set_sip(#jabber{username=Name,passwd=Passwd,domain=Domain}, Nonce, Realm)->
        auth_set_sip(Name, Passwd, Domain, "auth", Nonce, Realm).

%%----------------------------------------------------------------------
%% Func: auth_set_sip/6
%%----------------------------------------------------------------------
auth_set_sip(Username, Passwd, Domain, Type, Nonce, Realm) ->
 Jid = Username ++ "@" ++ Realm,
 {SipDigest,Integrity} = ts_digest:sip_digest(Nonce, Jid, Realm, Passwd),
 list_to_binary([
   "<iq id='", ts_msg_server:get_id(list),
   "' type='set' >",
   "<query xmlns='jabber:iq:", Type, "'>",
        "<username>", Jid, "</username>",
        "<resource>tsung</resource>",
        "<x xmlns='xmpp:assert' version='1.0'>",
                "<ContextInfo><ServiceValue><Realm>", Domain,
                "</Realm></ServiceValue></ContextInfo>",
                "<TokenInfo><SubjectValue>",
                        "<Username>", Jid, "</Username>",
                        "<Password type='sip-digest' encoding='hex'>", SipDigest,
                                "</Password>",
                        "<Nonce encoding='hex'>", Nonce, "</Nonce>",
                        "<Integrity encoding='hex'>", Integrity, "</Integrity>",
        "</SubjectValue></TokenInfo></x></query></iq>"]).


%%----------------------------------------------------------------------
%% Func: auth_sasl/1
%%----------------------------------------------------------------------
auth_sasl(_,"ANONYMOUS")->
    list_to_binary(["<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='ANONYMOUS'/>"]);
auth_sasl(#jabber{username=Name,passwd=Passwd},Mechanism)->
        auth_sasl(Name, Passwd, Mechanism).


%%----------------------------------------------------------------------
%% Func: auth_sasl/2
%%----------------------------------------------------------------------
auth_sasl(Username, _Passwd, Mechanism) ->
    S = <<0>>,
    N = list_to_binary(Username),
    list_to_binary(["<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='",Mechanism,"' >",
                    ssl_base64:encode(<<S/binary,N/binary,S/binary,N/binary>>) ,"</auth>"]).


%%----------------------------------------------------------------------
%% Func: auth_sasl_bind/1
%%----------------------------------------------------------------------
auth_sasl_bind(#jabber{username=Name,passwd=Passwd,domain=Domain})->
        auth_sasl_bind(Name, Passwd, Domain).


%%----------------------------------------------------------------------
%% Func: auth_sasl_bind/3
%%----------------------------------------------------------------------
auth_sasl_bind(_Username, _Passwd, _Domain) ->
 list_to_binary(["<iq type='set' id='",ts_msg_server:get_id(list),
"' ><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>tsung</resource></bind></iq>"]).


%%----------------------------------------------------------------------
%% Func: auth_sasl_session/1
%%----------------------------------------------------------------------
auth_sasl_session(#jabber{username=Name,passwd=Passwd,domain=Domain})->
        auth_sasl_session(Name, Passwd, Domain).


%%----------------------------------------------------------------------
%% Func: auth_sasl_session/3
%%----------------------------------------------------------------------
auth_sasl_session(_Username, _Passwd, _Domain) ->
 list_to_binary(["<iq type='set' id='",ts_msg_server:get_id(list),
"' ><session xmlns='urn:ietf:params:xml:ns:xmpp-session' /></iq>"]).

%%----------------------------------------------------------------------
%% Func: registration/1
%% Purpose: register message
%%----------------------------------------------------------------------
registration(#jabber{username=Name,passwd=Passwd})->
    auth_set_plain(Name, Passwd, "register").

%%----------------------------------------------------------------------
%% Func: message/3
%% Purpose: send message to defined user at the Service (aim, ...)
%%----------------------------------------------------------------------
message(Dest, Jabber, Service) when is_integer(Dest) ->
    message(id_to_string(Dest),Jabber, Service);
message(Dest, #jabber{size=Size,data=undefined, username=User}, Service) when is_integer(Size) ->
    put(previous, Dest),
    Username = username(User,Dest),
    list_to_binary([
                    "<message id='",ts_msg_server:get_id(list), "' to='",
                    Username, "@", Service,
                    "'><body>",ts_utils:urandomstr_noflat(Size), "</body></message>"]);
message(Dest, #jabber{data=Data, username=User}, Service) when is_list(Data) ->
    put(previous, Dest),
    Username = username(User,Dest),
    list_to_binary([
                    "<message id='",ts_msg_server:get_id(list), "' to='",
                    Username, "@", Service,
                    "'><body>",Data, "</body></message>"]).

%%----------------------------------------------------------------------
%% Func: presence/0
%%----------------------------------------------------------------------
presence() ->
    list_to_binary([ "<presence id='",ts_msg_server:get_id(list),"' />"]).

%%----------------------------------------------------------------------
%% Func: presence/1
%%----------------------------------------------------------------------
presence(unavailable)->
    list_to_binary([ "<presence type='unavailable'/>"]).

%%----------------------------------------------------------------------
%% Func: presence/2
%%----------------------------------------------------------------------
presence(Type, Jabber=#jabber{dest=Dest}) when is_integer(Dest)->
    presence(Type, Jabber#jabber{dest=integer_to_list(Dest)}) ;
presence(roster, Jabber)->
    presence(subscribed, Jabber);
presence(subscribe, RosterJid)->
     list_to_binary([
           "<presence id='",ts_msg_server:get_id(list),
           "' to='", RosterJid,
           "' type='subscribe'/>"]);
presence(Type, Jabber) when is_atom(Type)->
    presence(atom_to_list(Type), Jabber);
presence(Type, #jabber{dest=Dest, domain=Domain, username=UserName})->
    DestName = username(UserName, Dest),
    list_to_binary([
      "<presence id='",ts_msg_server:get_id(list),
      "' to='", DestName, "@" , Domain,
      "' type='",Type,"'/>"]).

%%----------------------------------------------------------------------
%% Func: presence/3
%%----------------------------------------------------------------------
presence(broadcast, Show, Status) ->
    list_to_binary([ "<presence id='",ts_msg_server:get_id(list),"'>",
        "<show>", Show, "</show><status>", Status, "</status></presence>"]).

%%----------------------------------------------------------------------
%% Func: presence/4
%%----------------------------------------------------------------------
presence(directed, Dest, Jabber, Show, Status) when is_integer(Dest) ->
    presence(directed, integer_to_list(Dest), Jabber, Show, Status);
presence(directed, Dest, #jabber{username=UserName,domain=Domain}, Show, Status) ->
    DestName = username(UserName,Dest),
    list_to_binary([
          "<presence id='",ts_msg_server:get_id(list),
          "' to='", DestName, "@" , Domain , "'>",
          "<show>", Show, "</show><status>", Status, "</status></presence>"]).

%%----------------------------------------------------------------------
%% Func: request/3
%%----------------------------------------------------------------------
request(roster_rename, RosterJid,Group) ->
        list_to_binary([
                "<iq id='" ,ts_msg_server:get_id(list),
                "' type='set'><query xmlns='jabber:iq:roster'><item jid='"
                ,RosterJid,
                "' name='Tsung Testuser'><group>", Group, "</group></item></query></iq>"]).

request(roster_remove, RosterJid) ->
        list_to_binary([
                "<iq id='" ,ts_msg_server:get_id(list),
                "' type='set'><query xmlns='jabber:iq:roster'><item jid='"
                ,RosterJid,
                "' subscription='remove'/></query></iq>"]).
%%----------------------------------------------------------------------
%% Func: request/5
%%----------------------------------------------------------------------
request(roster_add, UserName, Domain, Id, Group) when is_integer(Id)->
    request(roster_add, UserName, Domain, integer_to_list(Id), Group);
request(roster_add, UserName, Domain, Id, Group)->
        Name = username(UserName,Id),
        RosterJid = Name ++ "@" ++ Domain,
        _ = put(rosterjid,RosterJid),
        list_to_binary([
                "<iq id='" ,ts_msg_server:get_id(list),
                "' type='set'>","<query xmlns='jabber:iq:roster'><item jid='",
                RosterJid,
                "' name='",RosterJid,"'><group>",Group,"</group></item></query></iq>"]).
%% Func: request/4
request(roster_get, _UserName, _Domain, _Id)->
    list_to_binary([
      "<iq id='" ,ts_msg_server:get_id(list),
      "' type='get'><query xmlns='jabber:iq:roster'></query></iq>"]).

%%%----------------------------------------------------------------------
%%% Func: raw/1
%%%----------------------------------------------------------------------
raw(#jabber{data=undefined}) ->
    << >>;
raw(#jabber{data=Data}) when is_list(Data) ->
    list_to_binary(Data).

%%%----------------------------------------------------------------------
%%% Func: create_pubsub_node/5
%%% Create a pubsub node: Generate XML packet
%%% If node name is undefined (data attribute), we create a pubsub instant
%%% node.
%%% Nodenames are relative to the User pubsub hierarchy (ejabberd); they are
%%% absolute with leading slash.
%%%----------------------------------------------------------------------
create_pubsub_node(Domain, PubSubComponent,Username, Node, NodeType) ->
    list_to_binary(["<iq to='", PubSubComponent, "' type='set' id='",
            ts_msg_server:get_id(list),"'>"
            "<pubsub xmlns='http://jabber.org/protocol/pubsub'>"
            "<create", pubsub_node_attr(Node, Domain, Username),
                       pubsub_node_type(NodeType),
            "/></pubsub></iq>"]).

%% Generate pubsub node attribute
pubsub_node_attr(undefined, _Domain, _Username) -> " ";
pubsub_node_attr(user_root, Domain, Username) ->
    [" node='/home/", Domain, "/", Username,"'"];
pubsub_node_attr([$/|AbsNode], _Domain, _Username) ->
    [" node='/", AbsNode,"'"];
pubsub_node_attr(Node, Domain, Username) ->
    [" node='/home/", Domain, "/", Username, "/", Node,"'"].

pubsub_node_type(undefined) ->
    "";
pubsub_node_type(Type) when is_list(Type) ->
    [" type='", Type, "' "].

%%%----------------------------------------------------------------------
%%% Func: subscribe_pubsub_node/4
%%% Subscribe to a pubsub node: Generate XML packet
%%% If node name is undefined (data attribute), we subscribe to target user
%%% root node
%%% Nodenames are relative to the User pubsub hierarchy (ejabberd); they are
%%% absolute with leading slash.
%%%----------------------------------------------------------------------
subscribe_pubsub_node(Domain, PubSubComponent, UserFrom, UserTo, undefined) ->
    subscribe_pubsub_node(Domain, PubSubComponent, UserFrom, UserTo, "");
subscribe_pubsub_node(Domain, PubSubComponent, UserFrom, UserTo, Node) ->
    list_to_binary(["<iq to='", PubSubComponent, "' type='set' id='",
            ts_msg_server:get_id(list),"'>"
            "<pubsub xmlns='http://jabber.org/protocol/pubsub'>"
            "<subscribe", pubsub_node_attr(Node, Domain, UserTo),
            " jid='", UserFrom, "@", Domain, "'/>"
            "</pubsub></iq>"]).

%%%----------------------------------------------------------------------
%%% Func: publish_pubsub_node/4
%%% Publish an item to a pubsub node
%%% Nodenames are relative to the User pubsub hierarchy (ejabberd); they are
%%% absolute with leading slash.
%%%----------------------------------------------------------------------
publish_pubsub_node(Domain, PubSubComponent, Username, Node, Size) ->
    Result = list_to_binary(["<iq to='", PubSubComponent, "' type='set' id='",
            ts_msg_server:get_id(list),"'>"
            "<pubsub xmlns='http://jabber.org/protocol/pubsub'>"
            "<publish", pubsub_node_attr(Node, Domain, Username),">"
            "<item><entry>", ts_utils:urandomstr_noflat(Size),"</entry></item></publish>"
            "</pubsub></iq>"]),
    Result.

muc_join(Room,Nick, Service) ->
    list_to_binary(["<presence to='", Room,"@", Service,"/", Nick, "'>",
                             " </presence>"]).

muc_chat(Room, Service, Size) ->
    Result = list_to_binary(["<message type='groupchat' to ='", Room,"@", Service,"'>",
                                "<body>", ts_utils:urandomstr_noflat(Size), "</body>",
                                "</message>"]),
    Result.
muc_nick(Room, Nick, Service) ->
    Result = list_to_binary(["<presence to='", Room,"@", Service,"/", Nick, "'/>"]),
    Result.

muc_exit(Room,Nick, Service) ->
    Result = list_to_binary(["<presence to='", Room,"@", Service,"/", Nick, "' type='unavailable'/>"]),
    Result.


%%%----------------------------------------------------------------------
%%% Func: privacy_get_names/2
%%% Get names of all privacy lists server stores for the user
%%%----------------------------------------------------------------------
privacy_get_names(User, Domain) ->
    Jid = [User,"@",Domain,"/tsung"],
    Req = ["<iq from='", Jid, "' type='get' id='getlist'>",
               "<query xmlns='jabber:iq:privacy'/>",
           "</iq>"],
    list_to_binary(Req).

%%%----------------------------------------------------------------------
%%% Func: privacy_set_active/2
%%% Set the list named according to pattern "<user>@<domain>_list"
%%% as active
%%%----------------------------------------------------------------------
privacy_set_active(User, Domain) ->
    Jid = [User,"@",Domain,"/tsung"],
    List = [User,"@",Domain,"_list"],
    Req = ["<iq from='", Jid, "' type='set' id='active1'>",
              "<query xmlns='jabber:iq:privacy'>",
                  "<active name='", List, "'/>",
              "</query>",
           "</iq>"],
    list_to_binary(Req).


%%%----------------------------------------------------------------------
%%% Func: username/2
%%% Generate the username given a prefix and id
%%%----------------------------------------------------------------------
username(Prefix, Id) ->
    Prefix ++ Id.
%%% Convert Id to string
%%% Change this if you want to have padding
id_to_string(Id) ->
    integer_to_list(Id).

%%%----------------------------------------------------------------------
%%% Func: password/1
%%% Generate password for a given username
%%%----------------------------------------------------------------------
password(Prefix,Id) ->
    Prefix ++ Id.
