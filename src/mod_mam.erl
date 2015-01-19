%%%----------------------------------------------------------------------
%%% File    : mod_mam.erl
%%% Author  : Gregor Uhlenheuer <kongo2002@gmail.com>
%%% Purpose : Message Archive Management (XEP-0313)
%%% Created : 29 Jan 2014 by Gregor Uhlenheuer <kongo2002@gmail.com>
%%%
%%% Copyright (C) 2014 Gregor Uhlenheuer
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
%%%----------------------------------------------------------------------

-module(mod_mam).
-author('kongo2002@gmail.com').

-behaviour(gen_server).
-behaviour(gen_mod).


-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").


%% API
-export([start_link/2,
         start/2,
         stop/1,
         send_packet/3,
         receive_packet/4,
         get_disco_features/5,
         process_iq/3,
         process_local_iq/3
        ]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PROCNAME, ejabberd_mod_mam).
-define(POOL_SIZE, 10).
-define(MAX_QUERY_LIMIT, 50).

-define(NS_MAM, <<"urn:xmpp:mam:tmp">>).

-define(MAM_POLICY_VIOLATION(Text),
        #xmlel{name = <<"iq">>,
               attrs = [{<<"type">>, <<"error">>}],
               children =
               [#xmlel{name = <<"error">>,
                       attrs = [{<<"type">>, <<"modify">>}],
                       children = [
                                   #xmlel{name = <<"policy-violation">>,
                                          attrs = [{<<"xmlns">>, ?NS_STANZAS}]},
                                   #xmlel{name = <<"text">>,
                                          attrs = [{<<"xmlns">>, ?NS_STANZAS}],
                                          children = [{xmlcdata, Text}]}
                                  ]
                      }
               ]}).

-record(state, {host = <<"">>        :: binary(),
                ignore_chats = false :: boolean(),
                mongo}).

-record(rsm, {max = none,
              after_item = none,
              before_item = none,
              index = none}).

-record(filter, {start = none :: none | erlang:timestamp(),
                 'end' = none :: none | erlang:timestamp(),
                 jid   = none :: none | jid(),
                 rsm   = none :: none | #rsm{}}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = get_proc(Host),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = get_proc(Host),

    % make sure bson and mongodb are running
    ok = application:ensure_started(bson),
    ok = application:ensure_started(mongodb),

    Child =
        {Proc,
         {?MODULE, start_link, [Host, Opts]},
         permanent,
         1000,
         worker,
         [?MODULE]},

    supervisor:start_child(ejabberd_sup, Child).

stop(Host) ->
    Proc = get_proc(Host),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

send_packet(From, To, Packet) ->
    Host = From#jid.lserver,
    Proc = get_proc(Host),
    gen_server:cast(Proc, {log, to, From#jid.luser, Host, To, Packet}).

receive_packet(_Jid, From, To, Packet) ->
    Host = To#jid.lserver,
    Proc = get_proc(Host),
    gen_server:cast(Proc, {log, from, To#jid.luser, Host, From, Packet}).


%%%-------------------------------------------------------------------
%%% IQ handling callbacks
%%%-------------------------------------------------------------------

process_iq(From, To, IQ) ->
    process_local_iq(From, To, IQ).

process_local_iq(From, To, #iq{sub_el = SubEl} = IQ) ->
    Server = From#jid.lserver,
    case lists:member(Server, ?MYHOSTS) of
        false ->
            % wrong server
            IQ#iq{type=error, sub_el=[SubEl, ?ERR_NOT_ALLOWED]};
        true ->
            case SubEl#xmlel.name of
                <<"query">> ->
                    Proc = get_proc(Server),
                    gen_server:cast(Proc, {process_query, From, To, IQ}),

                    % we have to delay the response IQ until
                    % all messages are sent to the client
                    ignore;
                _ ->
                    % we do not support anything other than 'query'
                    IQ#iq{type = error,
                          sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
            end
    end.


%%%-------------------------------------------------------------------
%%% Service discovery
%%%-------------------------------------------------------------------

get_disco_features(Acc, _From, _To, <<"">>, _Lang) ->
    Features = case Acc of
                   {result, I} -> I;
                   _ -> []
               end,

    {result, Features ++ [?NS_MAM]};

get_disco_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    ?INFO_MSG("Starting mod_mam module of '~s'", [Host]),

    % get options
    IQDisc = gen_mod:get_opt(iqdisc, Opts, false, one_queue),
    IgnoreChats = gen_mod:get_opt(ignore_chats, Opts, false, false),

    % get MongoDB options
    MongoConn = gen_mod:get_opt(mongo, Opts,
                                fun ({H, P}) -> {H, P};
                                    ([{H, P}]) -> {H, P}
                                end,
                                {localhost, 27017}),
    MongoDb = gen_mod:get_opt(mongo_database, Opts,
                              fun (X) when is_atom(X) -> X end, test),
    MongoColl = gen_mod:get_opt(mongo_collection, Opts,
                                fun (X) when is_atom(X) -> X end, ejabberd_mam),

    % hook into send/receive packet
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_packet, 80),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, receive_packet, 80),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, get_disco_features, 99),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_disco_features, 99),

    % hook into IQ stanzas
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM, ?MODULE,
                                  process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MAM, ?MODULE,
                                  process_local_iq, IQDisc),

    % create connection pool
    MPool = get_connection_pool(MongoConn),
    Mongo = {MPool, MongoDb, MongoColl},

    ?INFO_MSG("Using MongoDB at ~p - database '~s' - collection '~s'",
             [MongoConn, MongoDb, MongoColl]),

    {ok, #state{host = Host,
                ignore_chats = IgnoreChats,
                mongo = Mongo}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% handle a 'process_query' request meaning a client request to query
%% for specific messages inside the message archive
handle_cast({process_query, From, To, #iq{sub_el = Query} = IQ}, State) ->
    Children = Query#xmlel.children,
    Filter = lists:foldl(fun process_filter/2, #filter{}, Children),

    case Filter of
        #filter{start = S, 'end' = E, jid = J, rsm = RSM} ->
            ?DEBUG("Filter: ~p", [Filter]),
            User = From#jid.luser,
            Mongo = State#state.mongo,
            QueryId = xml:get_tag_attr_s(<<"queryid">>, Query),
            Fs = [{start, S}, {'end', E}, {jid, J}],

            case find(Mongo, User, Fs, RSM) of
                {error, Error} ->
                    ejabberd_router:route(To, From, Error);
                Ms when is_list(Ms) ->
                    ?DEBUG("Messages: ~p", [Ms]),
                    spawn(
                      fun() ->
                        query_response(Ms, To, From, IQ#iq.id, QueryId, RSM)
                      end)
            end;
        % filter processing failed for some reason
        % immediately return with an error
        {error, E} ->
            Error = IQ#iq{type = error, sub_el = [Query, E]},
            ErrXml = jlib:iq_to_xml(Error),
            ejabberd_router:route(To, From, ErrXml)
    end,

    {noreply, State};

%% handle a 'log' request meaning a message that should be stored
%% in the message archive
handle_cast({log, Dir, LUser, LServer, Jid, Packet}, State) ->
    ?DEBUG("Packet: ~p", [Packet]),

    % TODO: we should add an '<archived/>' tag to the original message
    % TODO: this probably means we have to use the filter_packet hook instead

    case should_store(LUser, LServer) of
        true ->
            IgnoreChats = State#state.ignore_chats,
            case extract_body(Packet, IgnoreChats) of
                ignore -> ok;
                Body ->
                    Mongo = State#state.mongo,
                    Doc = msg_to_bson(Dir, LUser, LServer, Jid, Body, Packet),
                    insert(Mongo, Doc)
            end;
        false -> ok
    end,

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    Host = State#state.host,
    {Pool, _Db, _Coll} = State#state.mongo,

    ?INFO_MSG("Stopping mod_mam module of '~s'", [Host]),

    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, send_packet, 80),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, receive_packet, 80),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_disco_features, 99),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_disco_features, 99),

    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_MAM),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM),

    resource_pool:close(Pool),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

should_store(_User, _Server) ->
    % TODO
    true.

extract_body(#xmlel{name = <<"message">>} = Xml, IgnoreChats) ->
    % archive messages with a body tag only
    case xml:get_subtag(Xml, <<"body">>) of
        false -> ignore;
        Body ->
            case IgnoreChats of
                true ->
                    % do not archive groupchat messages
                    case xml:get_tag_attr(<<"type">>, Xml) of
                        {value, <<"groupchat">>} -> ignore;
                        _ -> xml:get_tag_cdata(Body)
                    end;
                _ -> xml:get_tag_cdata(Body)
            end
    end;

extract_body(_, _) -> ignore.

%% try to extract an integer out of an XML tag content
int_cdata(Tag) ->
    CD = xml:get_tag_cdata(Tag),
    case catch binary_to_integer(CD) of
        I when is_integer(I) -> I;
        _ -> error
    end.

%% try to extract an ObjectId out of an XML tag content
objid_cdata(Tag) ->
    CD = xml:get_tag_cdata(Tag),
    case catch binary_to_objectid(CD) of
        {'EXIT', _} -> error;
        ObjId when is_tuple(ObjId) -> ObjId;
        _ -> error
    end.

is_empty(#xmlel{attrs = [], children = []}) -> true;
is_empty(_) -> false.

%% determine the maximum amount of messages to return
get_limit(none) -> {false, ?MAX_QUERY_LIMIT};
get_limit(#rsm{max = M}) ->
    case M of
        Max when is_integer(Max), Max =< ?MAX_QUERY_LIMIT -> {true, Max};
        _ -> {false, ?MAX_QUERY_LIMIT}
    end.

%% parse RSM instructions according to XEP-0059
parse_rsm(_, error) -> error;
parse_rsm([], RSM) -> RSM;
parse_rsm([#xmlel{name = Name} = C | Cs], RSM) ->
    Result = case Name of
        <<"max">> when RSM#rsm.max == none ->
            case int_cdata(C) of
                error -> error;
                Max -> RSM#rsm{max = Max}
            end;
        <<"after">> when RSM#rsm.after_item == none ->
            case objid_cdata(C) of
                error -> error;
                CD -> RSM#rsm{after_item = CD}
            end;
        <<"before">> when RSM#rsm.before_item == none ->
            % an empty 'before' tag requests the last page of entries
            case is_empty(C) of
                true -> RSM#rsm{before_item = last};
                _ ->
                    case objid_cdata(C) of
                        error -> error;
                        CD -> RSM#rsm{before_item = CD}
                    end
            end;
        <<"index">> when RSM#rsm.index == none ->
            case objid_cdata(C) of
                error -> error;
                CD -> RSM#rsm{index = CD}
            end;
        _ -> error
    end,

    parse_rsm(Cs, Result);
parse_rsm([_ | Cs], RSM) -> parse_rsm(Cs, RSM).

%%%-------------------------------------------------------------------
%%% Process all possible filters
%%%-------------------------------------------------------------------

% propagate errors on filter processing
process_filter(_, {error, _E} = Error) -> Error;

%% start:
%%  filter all messages before a certain date/time
process_filter(#xmlel{name = <<"start">>} = Q, #filter{start = S} = F) ->
    Time = xml:get_tag_cdata(Q),

    % 'start' tag may be defined only once
    case {S, jlib:datetime_string_to_timestamp(Time)} of
        {_, undefined} -> {error, ?ERR_BAD_REQUEST};
        {none, Value}  -> F#filter{start = Value};
        _              -> {error, ?ERR_BAD_REQUEST}
    end;

%% end:
%%  filter all messages after a certain date/time
process_filter(#xmlel{name = <<"end">>} = Q, #filter{'end' = E} = F) ->
    Time = xml:get_tag_cdata(Q),

    % 'end' tag may be defined only once
    case {E, jlib:datetime_string_to_timestamp(Time)} of
        {_, undefined} -> {error, ?ERR_BAD_REQUEST};
        {none, Value}  -> F#filter{'end' = Value};
        _              -> {error, ?ERR_BAD_REQUEST}
    end;

%% with:
%%  match messages that contain a specific JID
process_filter(#xmlel{name = <<"with">>} = Q, #filter{jid = J} = F) ->
    User = xml:get_tag_cdata(Q),

    % 'with' tag may be defined only once
    case {J, jlib:string_to_jid(User)} of
        {_, error}  -> {error, ?ERR_BAD_REQUEST};
        {none, Jid} -> F#filter{jid = Jid};
        _           -> {error, ?ERR_BAD_REQUEST}
    end;

%% set:
%%  limit results via RSM (result set management - XEP-0059)
process_filter(#xmlel{name = <<"set">>} = Q, #filter{} = F) ->
    % search for a RSM (XEP-0059) query statement
    case xml:get_tag_attr_s(<<"xmlns">>, Q) of
        ?NS_RSM ->
            Children = Q#xmlel.children,
            case parse_rsm(Children, #rsm{}) of
                error -> {error, ?ERR_BAD_REQUEST};
                NRSM -> F#filter{rsm = NRSM}
            end;
        _ ->
            % unknown/invalid 'set' statement
            {error, ?ERR_BAD_REQUEST}
    end;

process_filter(_, Filter) -> Filter.

%% construct and send the response to the query request
query_response(Messages, From, To, Id, QueryId, RSM) ->
    Attr = [{<<"to">>, jlib:jid_to_string(To)}],
    Send = fun (Message) ->
                   case bson_to_msg(Message, QueryId) of
                       none -> ok;
                       M ->
                           Xml = #xmlel{name = <<"message">>,
                                        attrs = Attr,
                                        children = [M]},
                           ejabberd_router:route(From, To, Xml)
                   end
           end,
    lists:foreach(Send, Messages),

    IQAttr = [{<<"type">>, <<"result">>}],
    IQAttrs = case Id of
                  <<"">> -> IQAttr;
                  _ -> [{<<"id">>, Id} | IQAttr]
              end,

    % in case the client requested with RSM we have
    % to respond with the appropriate paging information
    Children =
        case RSM of
            none -> [];
            _    -> get_rsm_response(Messages)
        end,

    % terminating IQ stanza
    IQ = #xmlel{name = <<"iq">>, attrs = IQAttrs, children = Children},
    ejabberd_router:route(From, To, IQ).

get_id_ts({'_id', Id, r, _Raw, ts, Ts}) -> {Id, Ts};
get_id_ts({'_id', Id, ts, Ts, r, _Raw}) -> {Id, Ts}.

get_rsm_response([]) -> [];
get_rsm_response(Ms) ->
    % 'Ms' is expected to be sorted by timestamp
    H = hd(Ms),
    L = lists:last(Ms),
    Count = list_to_binary(integer_to_list(length(Ms))),
    {First, Start} = get_id_ts(H),
    {Last, End} = get_id_ts(L),

    % TODO: support 'index' on the 'first' node
    Set = #xmlel{name = <<"set">>, attrs = [{<<"xmlns">>, ?NS_RSM}],
                 children = [#xmlel{name = <<"first">>,
                                    children = [{xmlcdata,
                                                 objectid_to_binary(First)}]},
                             #xmlel{name = <<"last">>,
                                    children = [{xmlcdata,
                                                 objectid_to_binary(Last)}]},
                             #xmlel{name = <<"count">>,
                                    children = [{xmlcdata, Count}]}
                            ]},

    Cs = [#xmlel{name = <<"start">>, children = [{xmlcdata,
                                                  build_timestamp(Start)}]},
          #xmlel{name = <<"end">>,   children = [{xmlcdata,
                                                  build_timestamp(End)}]},
          Set],

    Query = #xmlel{name = <<"query">>,
                   attrs = [{<<"xmlns">>, ?NS_MAM}],
                   children = Cs},
    [Query].

get_proc(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).

%%%-------------------------------------------------------------------
%%% MongoDB functions
%%%-------------------------------------------------------------------

get_connection_pool({_Host, Port} = Conn) when is_integer(Port) ->
    % single node connection pool
    ?INFO_MSG("Connecting to a single node MongoDB at ~p", [Conn]),

    resource_pool:new(mongo:connect_factory(Conn), ?POOL_SIZE);

get_connection_pool({Rs, Nodes} = Conn) when is_list(Nodes) ->
    % replica set connection pool
    ?INFO_MSG("Connecting to replica set '~s' with nodes ~p", [Rs, Nodes]),

    resource_pool:new(mongo:rs_connect_factory(Conn), ?POOL_SIZE).


%% build a BSON document based on the given JID
get_jid_document(Jid) ->
    {U, S, R} = jlib:jid_tolower(Jid),
    case R of
        <<"">> -> {u, U, s, S};  % user + server
        _  -> {u, U, s, S, r, R} % user + server + resource
    end.

%% build a BSON of the given message details
msg_to_bson(Dir, LUser, LServer, Jid, Body, Xml) ->
    { u, LUser,                     % user
      s, LServer,                   % server
      j, get_jid_document(Jid),     % jid
      b, Body,                      % body
      d, Dir,                       % direction
      ts, bson:timenow(),           % timestamp
      r, xml:element_to_binary(Xml) % raw XML
    }.

%% build a XML message that is returned as response of the client
%% query based on a given BSON message record
bson_to_msg(Bson, QueryId) ->
    case Bson of
        {'_id', Id, r, Raw, ts, Ts} ->
            bson_to_msg(Id, Raw, Ts, QueryId);
        {'_id', Id, ts, Ts, r, Raw} ->
            bson_to_msg(Id, Raw, Ts, QueryId);
        _ -> none
    end.

bson_to_msg(Id, Raw, Ts, QueryId) ->
    case xml_stream:parse_element(Raw) of
        {error, _Error} -> none;
        Xml ->
            Attrs = [{<<"xmlns">>, ?NS_MAM},
                     {<<"id">>, objectid_to_binary(Id)}],

            % add 'queryid' if specified
            As = case QueryId of
                     <<"">> -> Attrs;
                     QId -> [{<<"queryid">>, QId} | Attrs]
                 end,

            % build 'delay' node
            Stamp = build_timestamp(Ts),
            Delay = #xmlel{name = <<"delay">>,
                           attrs = [{<<"xmlns">>, ?NS_DELAY},
                                    {<<"stamp">>, Stamp}]},

            #xmlel{name = <<"result">>, attrs = As,
                   children = [ #xmlel{name = <<"forwarded">>,
                                       attrs = [{<<"xmlns">>, <<"urn:xmpp:forward:0">>}],
                                       children = [Delay, Xml]}
                              ]}
    end.

build_timestamp(Ts) ->
    UTC = calendar:now_to_universal_time(Ts),
    {Time, TZ} = jlib:timestamp_to_iso(UTC, utc),
    <<Time/binary, TZ/binary>>.

objectid_to_binary({Id}) -> objectid_to_binary(Id, []).

objectid_to_binary(<<>>, Result) ->
    list_to_binary(lists:reverse(Result));
objectid_to_binary(<<Hex:8, Bin/binary>>, Result) ->
    SL1 = erlang:integer_to_list(Hex, 16),
    SL2 = case erlang:length(SL1) of
        1 -> ["0"|SL1];
        _ -> SL1
    end,
    objectid_to_binary(Bin, [SL2|Result]).

binary_to_objectid(BS) -> binary_to_objectid(BS, []).

binary_to_objectid(<<>>, Result) ->
    {list_to_binary(lists:reverse(Result))};
binary_to_objectid(<<BS:2/binary, Bin/binary>>, Result) ->
    binary_to_objectid(Bin, [erlang:binary_to_integer(BS, 16)|Result]).

to_query(_Key, none)   -> none;
to_query(start, Start) -> {ts, {'$gte', Start}};
to_query('end', End)   -> {ts, {'$lte', End}};
to_query(jid, #jid{luser = U, lserver = S, lresource = R}) ->
    Query = [{'j.u', U}, {'j.s', S}],
    case R of
        <<"">> -> Query;
        Res -> [{'j.r', Res} | Query]
    end;
to_query(_Key, _Value) -> none.

add_to_query({_Key, none}, Query) -> Query;
add_to_query({Key, X}, Query) ->
    case to_query(Key, X) of
        none -> Query;
        Vs when is_list(Vs) -> Vs ++ Query;
        Value -> [Value | Query]
    end.

get_order(none) -> asc;
get_order(#rsm{before_item = B}) ->
    case B of
        last -> desc;
        _    -> asc
    end.

query_order(desc) -> {'_id', -1};
query_order(asc)  -> {'_id', 1}.

%% query the mongo database for specific messages using a query
%% based on optional filters/RSM instructions
find({_Pool, _Db, Coll} = M, User, Filter, RSM) ->
    Base = [{u, User}],
    BaseQuery = bson:document(lists:foldl(fun add_to_query/2, Base, Filter)),
    Order = get_order(RSM),
    QueryOrder = query_order(Order),
    Query = {'$query', BaseQuery, '$orderby', QueryOrder},

    ?DEBUG("Query: ~p", [Query]),

    Proj = {'_id', true, r, true, ts, true},
    Fun = fun () -> mongo:find(Coll, Query, Proj) end,

    case exec(M, Fun) of
        false -> ok;
        none -> ok;
        Cursor ->
            case get_limit(RSM) of
                {true, Max} ->
                    take(Cursor, Max, Order);
                {false, Max} ->
                    Rs = take(Cursor, Max+1, Order),
                    Len = length(Rs),
                    if Len > Max ->
                           E = ?MAM_POLICY_VIOLATION(<<"Too many results">>),
                           {error, E};
                       true -> Rs
                    end
            end
    end.

take(Cursor, Count, Order) ->
    Result = take_inner(Cursor, Count, []),
    mongo:close_cursor(Cursor),
    case Order of
        desc -> Result;
        asc  -> lists:reverse(Result)
    end.

take_inner(Cursor, Count, Acc) when Count > 0 ->
    case mongo:next(Cursor) of
        {}  -> Acc;
        {X} -> take_inner(Cursor, Count-1, [X|Acc])
    end;
take_inner(_Cursor, _Count, Acc) -> Acc.

%% insert a new message document
insert({_Pool, _Db, Coll} = M, Element) ->
    Fun = fun () -> mongo:insert(Coll, Element) end,
    exec(M, Fun, unsafe).

%% execute a mongo command using the specified connection pool
exec(Mongo, Function) ->
    exec(Mongo, Function, safe).

exec({Pool, Db, _Coll}, Function, Mode) ->
    case resource_pool:get(Pool) of
        {ok, Conn} ->
            case mongo:do(Mode, slave_ok, Conn, Db, Function) of
                {ok, {}} -> none;
                {ok, {Found}} -> Found;
                {ok, Cursor} -> Cursor
            end;
        {error, Reason} ->
            ?ERROR_MSG("Error connecting to MongoDB: ~p", [Reason]),
            false
    end.


% vim: set et sw=4 sts=4 tw=80:
