%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2003 IDEALX
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

%%%----------------------------------------------------------------------
%%% @author Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%% @todo learn how to use xmerl correctly
%%% @doc Read the tsung XML config file. Currently, it
%%%      work by parsing the #xmlElement record by hand !
%%% @end
%%% Created : 3 Dec 2003 by Nicolas Niclausse <nicolas@niclux.org>
%%%----------------------------------------------------------------------


-module(ts_config).
-author('nicolas@niclux.org').
-vc('$Id$ ').

-include("ts_profile.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").

-export([read/2,
         getAttr/2,
         getAttr/3,
         getAttr/4,
         getText/1,
         parse/2,
         get_default/3,
         mark_prev_req/3,
         get_batch_nodes/1
        ]).

%%%----------------------------------------------------------------------
%%% @spec: read(Filename::string(), LogDir::string()) ->
%%%        {ok, Config::#config{} } | {error, Reason::term()}
%%% @doc:  read and parse the xml config file
%%% @end
%%%----------------------------------------------------------------------
read(Filename, LogDir) ->
    case catch xmerl_scan:file(Filename,
                               [{fetch_path,["/usr/share/tsung/","./"]},
                                {validation,true}]) of
        {ok, Root = #xmlElement{}} ->  % xmerl-0.15
            ?LOGF("Reading config file: ~s~n", [Filename], ?NOTICE),
            Table = ets:new(sessiontable, [ordered_set, protected]),
            {ok, parse(Root, #config{session_tab = Table})};
        {Root = #xmlElement{}, _Tail} ->  % xmerl-0.19 and up
            ?LOGF("Reading config file: ~s~n", [Filename], ?NOTICE),
            Table = ets:new(sessiontable, [ordered_set, protected]),
            backup_config(LogDir, Filename, Root),
            {ok, parse(Root, #config{session_tab = Table, proto_opts=#proto_opts{}})};
        {error,Reason} ->
            {error, Reason};
        {'EXIT',Reason} ->
            {error, Reason}
    end.

%%%----------------------------------------------------------------------
%%% Function: parse/2
%%% Purpose:  parse the xmerl structure
%%%----------------------------------------------------------------------
parse(Element = #xmlElement{parents = [], attributes=Attrs}, Conf=#config{}) ->
    Loglevel = getAttr(string, Attrs, loglevel, "notice"),
    Dump     = getAttr(string, Attrs, dumptraffic, "false"),
    BackEnd  = getAttr(atom, Attrs, backend, text),
    DumpType = case Dump of
                   "false" -> none;
                   "true"  -> full;
                   "light" -> light
               end,
    lists:foldl(fun parse/2,
                Conf#config{dump= DumpType, stats_backend=BackEnd,
                            loglevel= ts_utils:level2int(Loglevel)},
                Element#xmlElement.content);


%% parsing the Server elements
parse(Element = #xmlElement{name=server, attributes=Attrs}, Conf=#config{servers=ServerList}) ->
    Server = getAttr(Attrs, host),
    Port   = getAttr(integer, Attrs, port),
    Type = case getAttr(Attrs, type) of
               "ssl" -> ssl;
               "tcp" -> gen_tcp;
               "udp" -> gen_udp;
               "erlang" -> erlang
           end,

    lists:foldl(fun parse/2,
        Conf#config{servers = [#server{host=Server,
                                       port=Port,
                                       type=Type
                                     }|ServerList]},
        Element#xmlElement.content);

%% Parsing the cluster monitoring element (monitor)
parse(Element = #xmlElement{name=monitor, attributes=Attrs},
      Conf = #config{monitor_hosts=MHList}) ->
    Host = getAttr(Attrs, host),
    Type = case getAttr(atom, Attrs, type, erlang) of
               erlang ->
                   {erlang, {}};
               snmp ->
                   case lists:keysearch(snmp,#xmlElement.name,
                                        Element#xmlElement.content) of
                       {value, SnmpEl=#xmlElement{} } ->
                           Port = getAttr(integer,SnmpEl#xmlElement.attributes,
                                                    port, ?config(snmp_port)),
                           Community = getAttr(string,SnmpEl#xmlElement.attributes,
                                                         community, ?config(snmp_community)),
                           Version = getAttr(atom,SnmpEl#xmlElement.attributes,
                                                       version, ?config(snmp_version)),
                           {snmp, {Port, Community, Version}};
                       _ ->
                           {snmp, {?config(snmp_port),
                            ?config(snmp_community),
                            ?config(snmp_version)}}
                   end;
               munin ->
                   case lists:keysearch(munin,#xmlElement.name,
                                        Element#xmlElement.content) of
                       {value, MuninEl=#xmlElement{} } ->
                           Port = getAttr(integer,MuninEl#xmlElement.attributes,
                                                    port, ?config(munin_port)),
                           {munin, {Port}};
                       _ ->
                           {munin, {?config(munin_port) }}
                   end
           end,
    NewMon = case getAttr(atom, Attrs, batch, false) of
                 true ->
                     Nodes = lists:usort(get_batch_nodes(list_to_atom(Host))),
                     lists:map(fun(N)-> {N, Type} end, Nodes);
                 _ ->
                     [{Host, Type}]
             end,
    lists:foldl(fun parse/2,
        Conf#config{monitor_hosts = lists:append(MHList, NewMon)},
        Element#xmlElement.content);

%%
parse(Element = #xmlElement{name=load, attributes=Attrs}, Conf) ->
    Loop     = getAttr(integer, Attrs, loop, 0),
    IDuration = getAttr(integer, Attrs, duration, 0),
    Unit     = getAttr(string,  Attrs, unit, "second"),
    Duration = to_seconds(Unit, IDuration),
    lists:foldl(fun parse/2,    Conf#config{load_loop=Loop,duration=Duration},
                Element#xmlElement.content);


%% Parsing the Client element
parse(Element = #xmlElement{name=client, attributes=Attrs},
      Conf = #config{clients=CList}) ->
    Host     = getAttr(Attrs, host),
    Weight   = getAttr(integer,Attrs, weight,1),
    MaxUsers = getAttr(integer,Attrs, maxusers, 800),
    SingleNode = getAttr(atom, Attrs, use_controller_vm, false) or Conf#config.use_controller_vm,
    NewClients =
        case getAttr(atom, Attrs, type) of
            batch ->
                ?LOG("Get client nodes from batch scheduler~n",?DEB),
                Batch = getAttr(atom, Attrs, batch),
                NodesTmp = get_batch_nodes(Batch),
                case NodesTmp of
                    []->
                        ?LOGF("Warning: empty list of nodes from batch: ~p~n",[NodesTmp],?WARN);
                    _ ->
                        ?LOGF("nodes: ~p~n",[NodesTmp],?DEB)
                end,
                %% remove controller host from list to avoid
                %% overloading the machine running the controller
                {ok, ControllerHost} = ts_utils:node_to_hostname(node()),
                Nodes = lists:delete(ControllerHost, NodesTmp),
                Fun = fun(N)->
                        {ok, IP } = inet:getaddr(N,inet),
                        #client{host=N,weight=Weight,ip=[IP],maxusers=MaxUsers}
                      end,
                lists:map(Fun, Nodes);
            _ ->
                CPU = case {getAttr(integer,Attrs, cpu, 1), SingleNode} of
                          {Val, true} when Val > 1 ->
                              erlang:display("Can't use CPU > 1 when use_controller_vm is true ! Set CPU to 1."),
                              1;
                          {Val, _} -> Val
                      end,
                %% must be hostname and not ip:
                case ts_utils:is_ip(Host) of
                    true ->
                        ?LOGF("ERROR: client config: 'host' attribute must be a hostname, "++
                              "not an IP ! (was ~p)~n",[Host],?EMERG),
                        exit({error, badhostname});
                    false ->
                        %% add a new client for each CPU
                        lists:duplicate(CPU,#client{host     = Host,
                                                    weight   = Weight/CPU,
                                                    maxusers = MaxUsers})
                end
        end,
    lists:foldl(fun parse/2,
                Conf#config{clients = lists:append(NewClients,CList),
                            use_controller_vm = SingleNode},
                Element#xmlElement.content);

%% Parsing the ip element
parse(Element = #xmlElement{name=ip, attributes=Attrs},
      Conf = #config{clients=[CurClient|CList]}) ->
    IPList = CurClient#client.ip,
    ToResolve = case getAttr(Attrs, value) of
             "resolve" ->
                 CurClient#client.host;
             StrIP ->
                 StrIP
         end,
     ?LOGF("resolving host ~p~n",[ToResolve],?WARN),
    {ok, IP } = inet:getaddr(ToResolve,inet),
     ?LOGF("resolved host ~p~n",[IP],?WARN),
    lists:foldl(fun parse/2,
        Conf#config{clients = [CurClient#client{ip = [IP|IPList]}
                               |CList]},
                Element#xmlElement.content);

%% Parsing the arrivalphase element
parse(Element = #xmlElement{name=arrivalphase, attributes=Attrs},
      Conf = #config{arrivalphases=AList}) ->

    Phase     = getAttr(integer,Attrs, phase),
    IDuration  = getAttr(integer, Attrs, duration),
    Unit  = getAttr(string,Attrs, unit, "second"),
    D = to_seconds(Unit, IDuration),
    case lists:keysearch(Phase,#arrivalphase.phase,AList) of
        false ->
            lists:foldl(fun parse/2,
                        Conf#config{arrivalphases = [#arrivalphase{phase=Phase,
                                                                   duration=D
                                                                  }
                                                     |AList]},
                        Element#xmlElement.content);
        _ -> % already existing phase, wrong configuration.
            ?LOGF("Client config error: phase ~p already defined, abort !~n",[Phase],?EMERG),
            exit({error, already_defined_phase})
    end;

%% Parsing the user element
parse(Element = #xmlElement{name=user, attributes=Attrs},
      Conf = #config{static_users=Users}) ->

    Start   = getAttr(float_or_integer,Attrs, start_time),
    Unit    = getAttr(string,Attrs, unit, "second"),
    Session = getAttr(string,Attrs, session),
    Delay   = to_seconds(Unit,Start)*1000,
    NewUsers= Users++[{Delay,Session}],
    lists:foldl(fun parse/2, Conf#config{static_users = NewUsers},
                Element#xmlElement.content);

%% Parsing the users element
parse(Element = #xmlElement{name=users, attributes=Attrs},
      Conf = #config{arrivalphases=[CurA | AList]}) ->

    Max = getAttr(integer,Attrs, maxnumber, infinity),
    ?LOGF("Maximum number of users ~p~n",[Max],?INFO),

    Unit  = getAttr(string,Attrs, unit, "second"),
    Intensity = case {getAttr(float_or_integer,Attrs, interarrival),
                      getAttr(float_or_integer,Attrs, arrivalrate)  } of
                    {[],[]} ->
                        exit({invalid_xml,"arrival or interarrival must be specified"});
                    {[], Rate}  when Rate > 0 ->
                        to_seconds(Unit,Rate) / 1000;
                    {InterArrival,[]} when InterArrival > 0 ->
                        1/(1000 * to_seconds(Unit,InterArrival));
                    {_Value, _Value2} ->
                        exit({invalid_xml,"arrivalrate and interarrival can't be defined simultaneously"})
                end,
    lists:foldl(fun parse/2,
        Conf#config{arrivalphases = [CurA#arrivalphase{maxnumber = Max,
                                                        intensity=Intensity}
                               |AList]},
                Element#xmlElement.content);

%% Parsing the session element
parse(Element = #xmlElement{name=session, attributes=Attrs},
      Conf = #config{curid= PrevReqId, sessions=SList}) ->

    Id = length(SList),
    Type        = getAttr(atom,Attrs, type),

    {Persistent_def, Bidi_def} =
        case Type:session_defaults() of
            {ok, Pdef, Bdef} -> {Pdef, Bdef};
            {ok, Pdef} -> {Pdef, false}
        end,

    Persistent  = getAttr(atom,Attrs, persistent, Persistent_def),
    Bidi        = getAttr(atom,Attrs, bidi, Bidi_def),
    Name        = getAttr(Attrs, name),
    ?LOGF("Session name for id ~p is ~p~n",[Id+1, Name],?NOTICE),
    ?LOGF("Session type: persistent=~p, bidi=~p~n",[Persistent,Bidi],?NOTICE),
    Probability = getAttr(float_or_integer, Attrs, probability),
    NewSList = case SList of
                   [] -> []; % first session
                   [Previous|Tail] ->
                       % set total requests count in previous session
                       [Previous#session{size=PrevReqId,type=Conf#config.main_sess_type}|Tail]
               end,
    lists:foldl(fun parse/2,
                Conf#config{sessions = [#session{id           = Id + 1,
                                                 popularity   = Probability,
                                                 type         = Type,
                                                 name         = Name,
                                                 persistent   = Persistent,
                                                 bidi         = Bidi,
                                                 hibernate    = Conf#config.hibernate,
                                                 proto_opts   = Conf#config.proto_opts
                                                }
                                        |NewSList],
                            main_sess_type = Type,
                            curid=0, cur_req_id=0},% re-initialize request id
                Element#xmlElement.content);

%%%% Parsing the transaction element
parse(Element = #xmlElement{name=transaction, attributes=Attrs},
      Conf = #config{session_tab = Tab, sessions=[CurS|_], curid=Id}) ->

    RawName = getAttr(Attrs, name),
    {ok, [{atom,1,Name}],1} = erl_scan:string("tr_"++RawName),
    ?LOGF("Add start transaction ~p in session ~p as id ~p",
         [Name,CurS#session.id,Id+1],?INFO),
    ets:insert(Tab, {{CurS#session.id, Id+1}, {transaction,start,Name}}),

    NewConf=lists:foldl( fun parse/2,
                 Conf#config{curid=Id+1},
                 Element#xmlElement.content),
    NewId = NewConf#config.curid,
    ?LOGF("Add end transaction ~p in session ~p as id ~p",
         [Name,CurS#session.id,NewId+1],?INFO),
    ets:insert(Tab, {{CurS#session.id, NewId+1}, {transaction,stop,Name}}),
    NewConf#config{curid=NewId+1} ;

%%%% Parsing the 'if' element
parse(_Element = #xmlElement{name='if', attributes=Attrs,content=Content},
      Conf = #config{session_tab = Tab, sessions=[CurS|_], curid=Id}) ->
    VarName = getAttr(atom,Attrs,var),
    {Rel,Value} = case getAttr(string,Attrs,eq,none) of
                none -> {neq,getAttr(string,Attrs,neq)};
                X ->  {eq,X}
            end,
    ?LOGF("Add if_start action in session ~p as id ~p",
          [CurS#session.id,Id+1],?INFO),
    NewConf = lists:foldl(fun parse/2, Conf#config{curid=Id+1}, Content),
    NewId = NewConf#config.curid,
    ?LOGF("endif in session ~p as id ~p",[CurS#session.id,NewId+1],?INFO),
    InitialAction = {ctrl_struct, {if_start, Rel, VarName, Value , NewId+1}},
    %%NewId+1 -> id of the first action after the if
    ets:insert(Tab,{{CurS#session.id,Id+1},InitialAction}),
    NewConf;

%%%% Parsing the 'for' element
parse(_Element = #xmlElement{name=for, attributes=Attrs,content=Content},
      Conf = #config{session_tab = Tab, sessions=[CurS|_], curid=Id}) ->
    VarName = getAttr(atom,Attrs,var),
    InitValue = getAttr(integer,Attrs,from),
    EndValue = getAttr(integer,Attrs,to),
    Increment = getAttr(integer,Attrs,incr,1),
    InitialAction = {ctrl_struct, {for_start, InitValue, VarName}},
    ?LOGF("Add for_start action in session ~p as id ~p",
          [CurS#session.id,Id+1],?INFO),
    ets:insert(Tab,{{CurS#session.id,Id+1},InitialAction}),
    NewConf = lists:foldl(fun parse/2, Conf#config{curid=Id+1}, Content),
    NewId = NewConf#config.curid,
    EndAction= {ctrl_struct,{for_end,VarName,EndValue,Increment,Id+2}},
    %%Id+2 -> id of the first action inside the loop
    %%       (id+1 is the for_start action)
    ?LOGF("Add for_end action in session ~p as id ~p, Jump to:~p",
          [CurS#session.id,NewId+1,Id+2],?INFO),
    ets:insert(Tab, {{CurS#session.id,NewId+1},EndAction}),
    NewConf#config{curid=NewId+1};

%%%% Parsing the 'repeat' element
%%%% Last child element must be either 'while' or 'until'
parse(_Element = #xmlElement{name=repeat,attributes=Attrs,content=Content},
    Conf = #config{session_tab = Tab, sessions=[CurS|_], curid=Id}) ->
    MaxRepeat = getAttr(integer,Attrs,max_repeat,20),
    RepeatName = getAttr(atom,Attrs,name),
    [LastElement|_] = lists:reverse([E || E=#xmlElement{} <- Content]),
    case LastElement of
        #xmlElement{name=While,attributes=WhileAttrs}
        when (While == 'while') or (While == 'until')->
            {Rel,Value} = case getAttr(string,WhileAttrs,eq,none) of
                              none -> {neq,getAttr(string,WhileAttrs,neq)};
                              X ->  {eq,X}
                          end,
                          %either <while .. eq=".."/> or <while ..neq=".."/>
            Var = getAttr(atom,WhileAttrs,var),
            NewConf = lists:foldl(fun parse/2, Conf#config{curid=Id}, Content),
            NewId = NewConf#config.curid,
            EndAction = {ctrl_struct,{repeat,RepeatName, While,Rel,Var,Value,Id+1, MaxRepeat}},
                                 %Id+1 -> id of the first action inside the loop
            ?LOGF("Add repeat action in session ~p as id ~p, Jump to: ~p",
                  [CurS#session.id,NewId+1,Id+1],?INFO),
            ets:insert(Tab,{{CurS#session.id,NewId+1},EndAction}),
            NewConf#config{curid=NewId+1};
        _ -> exit({invalid_xml,"Last element inside a <repeat/> loop must be "
                                "<while/> or <until/>"})
    end;

%%% Parsing the dyn_variable element
parse(#xmlElement{name=dyn_variable, attributes=Attrs},
      Conf=#config{sessions=[CurS|_],dynvar=DynVars}) ->
    StrName  = ts_utils:clean_str(getAttr(Attrs, name)),
    {ok, [{atom,1,Name}],1} = erl_scan:string("'"++StrName++"'"),
    {Type,Expr} = case {getAttr(string,Attrs,regexp,none),
                        getAttr(string,Attrs,pgsql_expr,none),
                        getAttr(string,Attrs,xpath,none),
                       getAttr(string,Attrs,jsonpath,none)} of
                      {none,none,none,none} ->
                          DefaultRegExp = ?DEF_REGEXP_DYNVAR_BEGIN ++ StrName
                              ++?DEF_REGEXP_DYNVAR_END,
                          {regexp,DefaultRegExp};
                      {none,none,XPath,none} ->
                          {xpath,XPath};
                      {none,none,none,JSONPath} ->
                          {jsonpath,JSONPath};
                      {none,PG,none,none} ->
                          {pgsql_expr,PG};
                      {RegExp,_,_,_} ->
                          {regexp,RegExp}
                  end,
    FlattenExpr =lists:flatten(Expr),
    %% precompilation of the exp
    DynVar = case Type of
                 regexp ->
                     ?LOGF("Add new regexp: ~s ~n", [Expr],?INFO),
                     {ok, CompiledRegExp} = gregexp:parse(FlattenExpr),
                     {regexp,Name,CompiledRegExp};
                 xpath ->
                     ?LOGF("Add new xpath: ~s ~n", [Expr],?INFO),
                     CompiledXPathExp = mochiweb_xpath:compile_xpath(FlattenExpr),
                     {xpath,Name,CompiledXPathExp};
                 Other ->
                     ?LOGF("Add ~s ~s ~p ~n", [Type,Name,Expr],?INFO),
                     {Type,Name,Expr}
             end,
    NewDynVar = [DynVar|DynVars],
    ?LOGF("Add new dyn variable=~p in session ~p~n",
          [NewDynVar,CurS#session.id],?INFO),
    Conf#config{ dynvar= NewDynVar };

parse( #xmlElement{name=change_type, attributes=Attrs},
      Conf = #config{sessions=[CurS|Other], curid=Id,session_tab = Tab}) ->

    CType   = getAttr(atom, Attrs, new_type),
    Server  = getAttr(string, Attrs, host),
    Port    = getAttr(integer, Attrs, port),
    Store   = getAttr(atom, Attrs, store, false),
    Restore = getAttr(atom, Attrs, restore, false),
    PType = case getAttr(Attrs, server_type) of
               "ssl" -> ssl;
               "tcp" -> gen_tcp;
               "udp" -> gen_udp;
               "erlang" -> erlang
           end,
    SessType=case Conf#config.main_sess_type == CType of
                 false -> CurS#session.type;
                 true  -> CType % back to the main type
             end,
    ets:insert(Tab,{{CurS#session.id, Id+1}, {change_type, CType, Server, Port, PType, Store, Restore}}),
    ?LOGF("Parse change_type (~p) ~p:~p:~p:~p ~n",[CType, Server,Port,PType,Id],?NOTICE),
    Conf#config{main_sess_type=SessType, curid=Id+1,
                sessions=[CurS#session{type=CType}|Other] };

%%% Parsing the request element
parse(Element = #xmlElement{name=request, attributes=Attrs},
      Conf = #config{sessions=[CurSess|_], curid=Id}) ->

    Type  = CurSess#session.type,
    SubstitutionFlag  = getAttr(atom, Attrs, subst, false),

    lists:foldl( fun(A,B) ->Type:parse_config(A,B) end,
                 Conf#config{curid=Id+1, cur_req_id=Id+1,
                             subst=SubstitutionFlag,
                             match=[]
                            },
                 Element#xmlElement.content);
%%% Match
parse(Element=#xmlElement{name=match,attributes=Attrs},
      Conf=#config{match=Match})->
    Do         = getAttr(atom, Attrs, do, continue),
    When       = getAttr(atom, Attrs, 'when', match),
    Subst      = getAttr(atom, Attrs, subst, false),
    MaxLoop    = getAttr(integer, Attrs, max_loop, 20),
    LoopBack   = getAttr(integer, Attrs, loop_back, 0),
    MaxRestart = getAttr(integer, Attrs, max_restart, 3),
    SleepLoop  = getAttr(integer, Attrs, sleep_loop, 5),
    ValRaw     = getText(Element#xmlElement.content),
    RegExp     = ts_utils:clean_str(ValRaw),
    SkipHeaders = getAttr(atom, Attrs, skip_headers, no),
    ApplyTo = case getAttr(string, Attrs, apply_to_content, undefined) of
                  undefined -> undefined;
                  Data ->
                      {Mod, Fun} = ts_utils:split2(Data,$:),
                      {list_to_atom(Mod), list_to_atom(Fun)}
              end,
    NewMatch   = #match{regexp=RegExp,subst=Subst, do=Do,'when'=When,
                        sleep_loop=SleepLoop * 1000, skip_headers=SkipHeaders,
                        loop_back=LoopBack, max_restart=MaxRestart, max_loop=MaxLoop, apply_to_content=ApplyTo},

    lists:foldl(fun parse/2,
                Conf#config{ match=lists:append(Match, [NewMatch]) },
                Element#xmlElement.content);
%%% Parsing the option element
parse(Element = #xmlElement{name=option, attributes=Attrs},
      Conf = #config{session_tab = Tab}) ->
    case getAttr(atom, Attrs, type) of
        "" ->
            case getAttr(Attrs, name) of
                "thinktime" ->
                    Val = getAttr(float_or_integer,Attrs, value),
                    ets:insert(Tab,{{thinktime, value}, Val}),
                    Random = case { getAttr(integer, Attrs, min),
                                    getAttr(integer, Attrs, max)}  of
                                 {Min, Max } when is_integer(Min), is_integer(Max), Max > 0, Max >= Min ->
                                     {"range", Min, Max};
                                 {"",""} ->
                                     getAttr(string,Attrs, random, ?config(thinktime_random))
                             end,
                    ets:insert(Tab,{{thinktime, random}, Random}),
                    Override = getAttr(string, Attrs, override,
                                       ?config(thinktime_override)),
                    ets:insert(Tab,{{thinktime, override}, Override}),
                    lists:foldl( fun parse/2, Conf, Element#xmlElement.content);
                "ssl_ciphers" ->
                    Cipher = getAttr(string,Attrs, value, negociate),
                    OldProto =  Conf#config.proto_opts,
                    NewProto =  OldProto#proto_opts{ssl_ciphers=Cipher},
                    lists:foldl( fun parse/2, Conf#config{proto_opts=NewProto},
                                 Element#xmlElement.content);
                "seed" ->
                    Seed =  getAttr(integer,Attrs, value, now),
                    lists:foldl( fun parse/2, Conf#config{seed=Seed},
                                 Element#xmlElement.content);
                "hibernate" ->
                    Hibernate = case  getAttr(integer,Attrs, value, 10000 ) of
                                    infinity -> infinity;
                                    Seconds -> Seconds * 1000
                                end,
                    lists:foldl( fun parse/2, Conf#config{hibernate=Hibernate},
                                 Element#xmlElement.content);
                "ports_range" ->
                    Min = getAttr(integer,Attrs, min, 1025),
                    Max = getAttr(integer,Attrs, max, 65534),
                    case getAttr(atom,Attrs, value, on) of
                        off ->
                            lists:foldl( fun parse/2, Conf,Element#xmlElement.content);
                        on ->
                            %%TODO: check if min > 1024 and max < 65536
                            lists:foldl( fun parse/2, Conf#config{ports_range={Min,Max}},
                                         Element#xmlElement.content)
                    end;
                "tcp_rcv_buffer" ->
                    Size = getAttr(integer,Attrs, value, ?config(rcv_size)),
                    OldProto =  Conf#config.proto_opts,
                    NewProto =  OldProto#proto_opts{tcp_rcv_size=Size},
                    lists:foldl( fun parse/2, Conf#config{proto_opts=NewProto},
                                 Element#xmlElement.content);
                "udp_rcv_buffer" ->
                    Size = getAttr(integer,Attrs, value, ?config(rcv_size)),
                    OldProto =  Conf#config.proto_opts,
                    NewProto =  OldProto#proto_opts{udp_rcv_size=Size},
                    lists:foldl( fun parse/2, Conf#config{proto_opts=NewProto},
                                 Element#xmlElement.content);
                "tcp_snd_buffer" ->
                    Size = getAttr(integer,Attrs, value, ?config(snd_size)),
                    OldProto =  Conf#config.proto_opts,
                    NewProto =  OldProto#proto_opts{tcp_snd_size=Size},
                    lists:foldl( fun parse/2, Conf#config{proto_opts=NewProto},
                                 Element#xmlElement.content);
                "udp_snd_buffer" ->
                    Size = getAttr(integer,Attrs, value, ?config(snd_size)),
                    OldProto =  Conf#config.proto_opts,
                    NewProto =  OldProto#proto_opts{udp_snd_size=Size},
                    lists:foldl( fun parse/2, Conf#config{proto_opts=NewProto},
                                 Element#xmlElement.content);
                "tcp_timeout" ->
                    Size = getAttr(integer,Attrs, value, ?config(tcp_timeout)),
                    OldProto =  Conf#config.proto_opts,
                    NewProto =  OldProto#proto_opts{idle_timeout=Size},
                    lists:foldl( fun parse/2, Conf#config{proto_opts=NewProto},
                                 Element#xmlElement.content);
                "retry_timeout" ->
                    Size = getAttr(integer,Attrs, value, ?config(client_retry_timeout)),
                    OldProto =  Conf#config.proto_opts,
                    NewProto =  OldProto#proto_opts{retry_timeout=Size},
                    lists:foldl( fun parse/2, Conf#config{proto_opts=NewProto},
                                 Element#xmlElement.content);
                "file_server" ->
                    FileName = getAttr(Attrs, value),
                    case file:read_file_info(FileName) of
                        {ok, _} ->
                            ok;
                        {error, _Reason} ->
                            exit({error, bad_filename, FileName})
                    end,
                    Id       = getAttr(atom, Attrs, id,default),
                    lists:foldl( fun parse/2,
                                 Conf#config{file_server=[{Id, FileName} | Conf#config.file_server]},
                                 Element#xmlElement.content);
                Other ->
                    ?LOGF("Unknown option ~p !~n",[Other], ?WARN),
                    lists:foldl( fun parse/2, Conf, Element#xmlElement.content)
            end;
        Module ->
            Module:parse_config(Element, Conf)
    end;



%%% Parsing the thinktime element
parse(Element = #xmlElement{name=thinktime, attributes=Attrs},
      Conf = #config{curid=Id, session_tab = Tab, sessions = [CurS |_]}) ->
    DefThink  = get_default(Tab,{thinktime, value},thinktime_value),
    DefRandom = get_default(Tab,{thinktime, random},thinktime_random),
    {Think, Randomize} =
        case get_default(Tab,{thinktime, override},thinktime_override) of
            "true" ->
                {DefThink, DefRandom};
            "false" ->
                case { getAttr(integer, Attrs, min),
                       getAttr(integer, Attrs, max),
                       getAttr(float_or_integer, Attrs, value)}  of
                    {Min, Max, "" } when is_integer(Min), is_integer(Max), Max > 0, Min >0,  Max > Min ->
                        {"", {"range", Min, Max} };
                    {"","",""} ->
                        CurRandom = getAttr(string, Attrs,random,DefRandom),
                        {DefThink, CurRandom};
                    {"","",CurThink} when CurThink > 0 ->
                        CurRandom = getAttr(string, Attrs,random,DefRandom),
                        {CurThink, CurRandom};
                    _ ->
                        exit({error, bad_thinktime})
                end
        end,
    RealThink = case Randomize of
                    "true" ->
                        {random, Think * 1000};
                    {"range", Min2, Max2} ->
                        {range, Min2 * 1000, Max2 * 1000};
                    "false" ->
                        round(Think * 1000)
                end,
    ?LOGF("New thinktime ~p for id (~p:~p)~n",[RealThink, CurS#session.id, Id+1],
          ?INFO),
    ets:insert(Tab,{{CurS#session.id, Id+1}, {thinktime, RealThink}}),
    lists:foldl( fun parse/2, Conf#config{curthink=Think,curid=Id+1},
                 Element#xmlElement.content);


%% Parsing the setdynvars element
parse(Element = #xmlElement{name=setdynvars, attributes=Attrs},
      Conf = #config{session_tab = Tab, sessions=[CurS|_], curid=Id}) ->

    Vars = [ getAttr(atom,Attr,name,none) || #xmlElement{name=var,attributes=Attr} <- Element#xmlElement.content],
    Action = case getAttr(string,Attrs,sourcetype,"erlang") of
                 "erlang" ->
                     [Module,Callback] = string:tokens(getAttr(string,Attrs,callback,none),":"),
                     {setdynvars,erlang,{list_to_atom(Module),list_to_atom(Callback)},Vars};
                 "eval" ->
                     Snippet = getAttr(string,Attrs,code,""),
                     Fun= ts_utils:eval(Snippet),
                     true = is_function(Fun, 1),
                     {setdynvars,code,Fun,Vars};
                 "file"   ->
                     Order = getAttr(atom,Attrs,order,iter),
                     FileId = getAttr(atom,Attrs,fileid,none),
                     case lists:keysearch(FileId,1,Conf#config.file_server) of
                         {value,_Val} ->
                             Delimiter = getAttr(string,Attrs,delimiter,";"),
                             {setdynvars,file,{Order,FileId,Delimiter},Vars};
                         false ->
                             ?LOGF("Unknown_file_id ~p in file setdynvars declaration: you forgot to add a file_server option~n",[FileId],?EMERG),
                             exit({error, unknown_file_id})
                     end;
                 "random_string" ->
                     Length = getAttr(integer,Attrs,length,20),
                     {setdynvars,random,{string,Length},Vars};
                 "urandom_string" ->
                     Length = getAttr(integer,Attrs,length,20),
                     {setdynvars,urandom,{string,Length},Vars};
                 "random_number" ->
                     Start = getAttr(integer,Attrs,start,1),
                     End = getAttr(integer,Attrs,'end',10),
                     {setdynvars,random,{number,Start,End},Vars}
             end,
    ?LOGF("Add setdynvars in session ~p as id ~p",[CurS#session.id,Id+1],?INFO),
    ets:insert(Tab, {{CurS#session.id, Id+1}, Action}),
    Conf#config{curid=Id+1};

%% Parsing other elements
parse(Element = #xmlElement{}, Conf = #config{}) ->
    lists:foldl(fun parse/2, Conf, Element#xmlElement.content);

%% Parsing non #xmlElement elements
parse(_Element, Conf = #config{}) ->
    Conf.


getAttr(Attr, Name) -> getAttr(string, Attr, Name, "").

%%%----------------------------------------------------------------------
%%% @spec getAttr(Type:: 'string'|'list'|'float_or_integer', Attr::list(),
%%%               Name::string()) -> term()
%%% @doc  search the attribute list for the given one
%%% @end
%%%----------------------------------------------------------------------
getAttr(Type, Attr, Name) -> getAttr(Type, Attr, Name, "").

getAttr(Type, [Attr = #xmlAttribute{name=Name}|_], Name, _Default) ->
    case { Attr#xmlAttribute.value, Type}  of
        {[], string } -> "" ;
        {[], list } -> [] ;
        {[], float_or_integer } -> 0 ;
        {A,_}  -> getTypeAttr(Type,A)
    end;

getAttr(Type, [_H|T], Name, Default) ->
    getAttr(Type, T, Name, Default);

getAttr(_Type, [], _Name, Default) ->
    Default.

getTypeAttr(string, String)-> String;
getTypeAttr(list, String)-> String;
getTypeAttr(float_or_integer, String)->
    case erl_scan:string(String) of
        {ok, [{integer,1,I}],1} -> I;
        {ok, [{float,1,F}],1} -> F
    end;
getTypeAttr(Type, String) ->
    {ok, [{Type,1,Val}],1} = erl_scan:string(String),
    Val.


%%%----------------------------------------------------------------------
%%% Function: getText/1
%%% Purpose:  get the text of the XML node
%%%----------------------------------------------------------------------
getText([#xmlText{value=Value}|_]) -> string:strip(Value, both);
getText(_Other) -> "".

%%%----------------------------------------------------------------------
%%% Function: to_seconds/2
%%% Purpose: get the real duration in seconds
%%%----------------------------------------------------------------------
to_seconds("second", Val)-> Val;
to_seconds("minute", Val)-> Val*60;
to_seconds("hour",   Val)-> Val*3600;
to_seconds("millisecond", Val)-> Val/1000.

%%%----------------------------------------------------------------------
%%% Function: get_default/2
%%%----------------------------------------------------------------------
get_default(Tab, Key,ConfigName) when not is_tuple(Key) ->
    get_default(Tab, {Key, value},ConfigName);
get_default(Tab, Key,ConfigName) ->
    case ets:lookup(Tab,Key) of
        [] ->
            ?config(ConfigName);
        [{_, SName}] ->
            SName
    end.

%%%----------------------------------------------------------------------
%%% Function: mark_prev_req/3
%%% Purpose: use to set page marks in requests during parsing ; by
%%%   default, a new request is mark as an endpage; if a new request is
%%%   parse, then the previous one must be set to false, unless there is
%%%   a thinktime between them
%%%----------------------------------------------------------------------
mark_prev_req(0, _, _)  ->
    ok;
mark_prev_req(Id, Tab, CurS) ->
    %% if the previous msg is a #ts_request request, set endpage to
    %% false, we are the current last request of the page
    case ets:lookup(Tab,{CurS#session.id, Id}) of
        [{Key, Msg=#ts_request{}}] ->
            ets:insert(Tab,{Key, Msg#ts_request{endpage=false}});
        [{_, {transaction,_,_}}] ->% transaction, continue to search back
            mark_prev_req(Id-1, Tab, CurS);
        _ -> ok
    end.


get_batch_nodes(pbs) ->
    get_batch_nodes(torque);
get_batch_nodes(lsf)->
    case os:getenv("LSB_HOSTS") of
        false ->
            [];
        Nodes ->
            lists:map(fun shortnames/1, string:tokens(Nodes, " "))
    end;
get_batch_nodes(oar) -> get_batch_nodes2("OAR_NODEFILE");
get_batch_nodes(torque) -> get_batch_nodes2("PBS_NODEFILE").

get_batch_nodes2(Env) ->
    case os:getenv(Env) of
        false ->
            [];
        NodeFile ->
            {ok, Nodes} = ts_utils:file_to_list(NodeFile),
            lists:map(fun shortnames/1, Nodes)
    end.

shortnames(Hostname)->
    [S | _]= string:tokens(Hostname,"."),
    S.

%%----------------------------------------------------------------------
%% @spec: backup_config(Dir::string(), Name::string(), Config::tuple()) ->
%%        ok | {error, Reason::term()}
%% @doc: create a backup copy of the config file in the log directory
%%   This is useful to have an history of all parameters of a test.
%%   Use parsed config file to expand all ENTITY
%% @end
%%----------------------------------------------------------------------
backup_config(Dir, Name, Config) ->
    BaseName = filename:basename(Name),
    {ok,IOF}=file:open(filename:join(Dir,BaseName),[write]),
    Export=xmerl:export_simple([Config],xmerl_xml),
    io:format(IOF,"~s~n",[lists:flatten(Export)]),
    file:close(IOF).
