%%%
%%%  Copyright © IDEALX S.A.S. 2004
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 20 Apr 2004 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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

-module(ts_config_jabber).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([parse_config/2 ]).

-include("ts_profile.hrl").
-include("ts_jabber.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").


%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% TODO: Dynamic content substitution is not yet supported for Jabber
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=jabber},
             Config=#config{curid= Id, session_tab = Tab,
                            match=MatchRegExp, dynvar=DynVar,
                            subst= SubstFlag, sessions = [CurS |_]}) ->
    TypeStr  = ts_config:getAttr(string,Element#xmlElement.attributes, type, "chat"),
    Ack  = ts_config:getAttr(atom,Element#xmlElement.attributes, ack, no_ack),
    Dest= ts_config:getAttr(atom,Element#xmlElement.attributes, destination,random),

    Size= ts_config:getAttr(integer,Element#xmlElement.attributes, size,0),
    Data= ts_config:getAttr(string,Element#xmlElement.attributes, data,undefined),
    Show= ts_config:getAttr(string,Element#xmlElement.attributes, show, "chat"),
    Status= ts_config:getAttr(string,Element#xmlElement.attributes, status, "Available"),
    Type= list_to_atom(TypeStr),
    Room = ts_config:getAttr(string,Element#xmlElement.attributes, room, undefined),
    Nick = ts_config:getAttr(string,Element#xmlElement.attributes, nick, undefined),
    Group = ts_config:getAttr(string,Element#xmlElement.attributes, group, "Tsung Group"),
    Node = case ts_config:getAttr(string, Element#xmlElement.attributes, 'node', undefined) of
                    "" -> user_root;
                    X -> X
                end,
    NodeType = ts_config:getAttr(string, Element#xmlElement.attributes, 'node_type', undefined),
    %% This specify where the node identified in the 'node' attribute is located.
    %% If node is undefined  (no node attribute)
    %%    -> we don't specify the node, let the server choose one for us.
    %% else
    %%     If node is absolute (starts with "/")
    %%         use that absolute address
    %%     else
    %%        the address is relative. Composed of two variables: user and node
    %%        if node is "" (attribute node="")
    %%            we want the "root" node for that user (/home/domain/user)
    %%        else
    %%            we want a specific child node for that user (/home/domain/user/node)
    %%        in both cases, the user is obtained as:
    %%        if dest == "random"
    %%           random_user()
    %%        if dest == "online"
    %%            online_user()
    %%        if dest == "offline"
    %%            offline_user()
    %%        Otherwise:    (any other string)
    %%          The specified string

    Domain  =ts_config:get_default(Tab, jabber_domain_name, jabber_domain),
    UserName=ts_config:get_default(Tab, jabber_username, jabber_username),
    Passwd  =ts_config:get_default(Tab, jabber_passwd, jabber_passwd),
    MUC_service = ts_config:get_default(Tab, muc_service, muc_service),
    PubSub_service =ts_config:get_default(Tab, pubsub_service, pubsub_service),

    Msg=#ts_request{ack   = Ack,
                    dynvar_specs= DynVar,
                    endpage = true,
                    subst   = SubstFlag,
                    match   = MatchRegExp,
                    param = #jabber{domain = Domain,
                                    username = UserName,
                                    passwd = Passwd,
                                    data   = Data,
                                    type   = Type,
                                    dest   = Dest,
                                    size   = Size,
                                    show   = Show,
                                    status   = Status,
                                    room = Room,
                                    nick = Nick,
                                    group = Group,
                                    muc_service = MUC_service,
                                    pubsub_service = PubSub_service,
                                    node = Node,
                                    node_type = NodeType
                                   }
                   },
    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg}),
    ?LOGF("Insert new request ~p, id is ~p~n",[Msg,Id],?INFO),
    lists:foldl( fun(A,B) -> ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content);
%% Parsing options
parse_config(Element = #xmlElement{name=option}, Conf = #config{session_tab = Tab}) ->
    NewConf = case ts_config:getAttr(Element#xmlElement.attributes, name) of
        "username" ->
            Val = ts_config:getAttr(string,Element#xmlElement.attributes, value,"tsunguser"),
            ets:insert(Tab,{{jabber_username,value}, Val}),
            Conf;
        "passwd" ->
            Val = ts_config:getAttr(string,Element#xmlElement.attributes, value,"sesame"),
            ets:insert(Tab,{{jabber_passwd,value}, Val}),
            Conf;
        "domain" ->
            Val = ts_config:getAttr(string,Element#xmlElement.attributes, value,"erlang-projects.org"),
            ets:insert(Tab,{{jabber_domain_name,value}, {domain,Val}}),
            Conf;
        "vhost_file" ->
            Val = ts_config:getAttr(atom,Element#xmlElement.attributes, value,"vhostfile"),
            ets:insert_new(Tab,{{jabber_domain_name,value}, {vhost,Val}}),
            Conf#config{vhost_file = Val};
        "global_number" ->
            N = ts_config:getAttr(integer,Element#xmlElement.attributes, value, 100),
            ts_timer:config(N),
            ets:insert(Tab,{{jabber_global_number, value}, N}),
            Conf;
        "userid_max" ->
            N = ts_config:getAttr(integer,Element#xmlElement.attributes, value, 10000),
            ts_user_server:reset(N),
            ets:insert(Tab,{{jabber_userid_max,value}, N}),
            Conf#config{user_server_maxuid = N};
        "muc_service" ->
            N = ts_config:getAttr(string,Element#xmlElement.attributes, value, "conference.localhost"),
            ets:insert(Tab,{{muc_service,value}, N}),
            Conf;
        "pubsub_service" ->
            N = ts_config:getAttr(string,Element#xmlElement.attributes, value, "pubsub.localhost"),
            ets:insert(Tab,{{pubsub_service,value}, N}),
            Conf
    end,
    lists:foldl( fun(A,B) -> ts_config:parse(A,B) end, NewConf, Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.


