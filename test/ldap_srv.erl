%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 21 Jun 2013 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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

%%%     Simple LDAP server intended for LDAP modules testing

-module(ldap_srv).

-behaviour(gen_server).

%% API
-export([start/1,
         load_ldif/1,
         equalityMatch/3,
         greaterOrEqual/3,
         lessOrEqual/3,
         approxMatch/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ELDAPv3.hrl").

-define(INFO_MSG(Fmt, Args), error_logger:info_msg(Fmt, Args)).
-define(ERROR_MSG(Fmt, Args), error_logger:error_msg(Fmt, Args)).

-define(TCP_SEND_TIMEOUT, 32000).
-define(SERVER, ?MODULE). 

-record(state, {listener = make_ref() :: reference()}).

%%%===================================================================
%%% API
%%%===================================================================
start(LDIFFile) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [LDIFFile], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([LDIFFile]) ->
    case gen_tcp:listen(1389, [binary,
                               {packet, asn1},
                               {active, false},
                               {reuseaddr, true},
                               {nodelay, true},
                               {send_timeout, ?TCP_SEND_TIMEOUT},
                               {send_timeout_close, true},
                               {keepalive, true}]) of
        {ok, ListenSocket} ->
            case load_ldif(LDIFFile) of
                {ok, Tree} ->
                    ?INFO_MSG("LDIF tree loaded, "
                              "ready to accept connections", []),
                    {_Pid, MRef} =
                        spawn_monitor(
                          fun() -> accept(ListenSocket, Tree) end
                         ),
                    {ok, #state{listener = MRef}};
                {error, Reason} ->
                    {stop, Reason}
            end;
        {error, Reason} = Err ->
            ?ERROR_MSG("failed to fetch sockname: ~p", [Err]),
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MRef, _Type, _Object, Info},
            #state{listener = MRef} = State) ->
    ?ERROR_MSG("listener died with reason ~p, terminating",
               [Info]),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
accept(ListenSocket, Tree) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
            spawn(fun() -> process(Socket, Tree) end),
            accept(ListenSocket, Tree);
        Err ->
            ?ERROR_MSG("failed to accept: ~p", [Err]),
	    Err
    end.

process(Socket, Tree) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, B} ->
            case asn1rt:decode('ELDAPv3', 'LDAPMessage', B) of
                {ok, Msg} ->
                    Replies = process_msg(Msg, Tree),
                    Id = Msg#'LDAPMessage'.messageID,
                    lists:foreach(
                      fun(ReplyOp) ->
                              Reply = #'LDAPMessage'{messageID = Id,
                                                     protocolOp = ReplyOp},
                              %%?DEBUG("sent:~n~p", [Reply]),
                              {ok, Bytes} = asn1rt:encode(
                                              'ELDAPv3', 'LDAPMessage', Reply),
                              gen_tcp:send(Socket, Bytes)
                      end, Replies),
                    process(Socket, Tree);
                Err ->
                    ?ERROR_MSG("failed to decode msg: ~p", [Err]),
                    Err
            end;
        Err ->
            Err
    end.

process_msg(#'LDAPMessage'{protocolOp = Op} = _Msg, TopTree) ->
    %%?DEBUG("got:~n~p", [Msg]),
    case Op of
        {bindRequest,
         #'BindRequest'{name = DN}} ->
            ResCode = case find_obj(DN, TopTree) of
                          {ok, _} ->
                              success;
                          error ->
                              invalidCredentials
                              %%success
                      end,
            [{bindResponse,
              #'BindResponse'{resultCode = ResCode,
                              matchedDN = <<"">>,
                              errorMessage = <<"">>}}];
        {searchRequest,
         #'SearchRequest'{baseObject = DN,
                          scope = Scope,
                          filter = Filter,
                          attributes = Attrs}} ->
            DNs = process_dn_filter(DN, Scope, Filter, TopTree),
            Es = lists:map(
                   fun(D) ->
                           make_entry(D, TopTree, Attrs)
                   end, DNs),
            Es ++ [{searchResDone,
                    #'LDAPResult'{resultCode = success,
                                  matchedDN = <<"">>,
                                  errorMessage = <<"">>}}];
        {extendedReq, _} ->
            [{extendedResp,
              #'ExtendedResponse'{matchedDN = <<"">>,
                                  errorMessage = <<"Not Implemented">>,
                                  resultCode = operationsError}}];
        _ ->
            RespOp = case Op of
                         {modifyRequest, _} -> modifyResponse;
                         {addRequest, _} -> addResponse;
                         {delRequest, _} -> delResponse;
                         {modDNRequest, _} -> modDNResponse;
                         {compareRequest, _} -> compareResponse;
                         _ -> undefined
                     end,
            case RespOp of
                undefined ->
                    [];
                _ ->
                    [{RespOp,
                      #'LDAPResult'{matchedDN = <<"">>,
                                    errorMessage = <<"Not implemented">>,
                                    resultCode = operationsError}}]
            end
    end.

make_entry(DN, Tree, Attrs) ->
    KVs = case ets:lookup(Tree, {dn, DN}) of
              [{_, _KVs}|_] ->
                  _KVs;
              _ ->
                  []
          end,
    NewKVs = if Attrs /= [], Attrs /= [<<"*">>] ->
                     lists:filter(
                       fun({A, _V}) ->
                               member(A, Attrs)
                       end, KVs);
                true ->
                     KVs
             end,
    KVs1 = dict:to_list(
             lists:foldl(
               fun({A, V}, D) ->
                       dict:append(A, V, D)
               end, dict:new(), NewKVs)),
    {searchResEntry,
     #'SearchResultEntry'{
       objectName = str:join(DN, <<",">>),
       attributes = [#'PartialAttributeList_SEQOF'{type = T, vals = V}
                     || {T, V} <- KVs1]}}.

process_dn_filter(DN, Level, F, Tree) ->
    DN1 = str:tokens(DN, <<",">>),
    Fun = filter_to_fun(F),
    filter(Fun, DN1, Tree, Level).

filter_to_fun({'and', Fs}) ->
    fun(KVs) ->
            lists:all(
              fun(F) ->
                      (filter_to_fun(F))(KVs)
              end, Fs)
    end;
filter_to_fun({'or', Fs}) ->
    fun(KVs) ->
            lists:any(
              fun(F) ->
                      (filter_to_fun(F))(KVs)
              end, Fs)
    end;
filter_to_fun({present, Attr}) ->
    fun(KVs) -> present(Attr, KVs) end;
filter_to_fun({Tag, #'AttributeValueAssertion'{attributeDesc = Attr,
                                               assertionValue = Val}})
  when Tag == equalityMatch; Tag == greaterOrEqual;
       Tag == lessOrEqual; Tag == approxMatch ->
    fun(KVs) ->
            apply(?MODULE, Tag, [Attr, Val, KVs])
    end;
filter_to_fun({substrings,
               #'SubstringFilter'{type = A, substrings = Ss}}) ->
    Re = substrings_to_regexp(Ss),
    fun(KVs) -> substrings(A, Re, KVs) end;
filter_to_fun({'not', F}) ->
    fun(KVs) -> not (filter_to_fun(F))(KVs) end.

find_obj(DN, Tree) ->
    case ets:lookup(Tree, {dn, str:tokens(DN, <<",">>)}) of
        [{_, Obj}|_] ->
            {ok, Obj};
        [] ->
            error
    end.

present(A, R) ->
    case keyfind(A, R) of
        [] ->
            false;
        _ ->
            true
    end.

equalityMatch(A, V, R) ->
    Vs = keyfind(A, R),
    member(V, Vs).

lessOrEqual(A, V, R) ->
    lists:any(
      fun(X) ->
              str:to_lower(X) =< str:to_lower(V)
      end, keyfind(A, R)).

greaterOrEqual(A, V, R) ->
    lists:any(
      fun(X) ->
              str:to_lower(X) >= str:to_lower(V)
      end, keyfind(A, R)).

approxMatch(A, V, R) ->
    equalityMatch(A, V, R).

substrings(A, Re, R) ->
    lists:any(
      fun(V) ->
              case re:run(str:to_lower(V), Re) of
                  {match, _} ->
                      true;
                  _ ->
                      false
              end
      end, keyfind(A, R)).

substrings_to_regexp(Ss) ->
    ReS = lists:map(
            fun({initial, S}) ->
                    [S, <<".*">>];
               ({any, S}) ->
                    [<<".*">>, S, <<".*">>];
               ({final, S}) ->
                    [<<".*">>, S]
            end, Ss),
    ReS1 = str:to_lower(list_to_binary([$^, ReS, $$])),
    {ok, Re} = re:compile(ReS1),
    Re.

filter(F, BaseDN, Tree, Level) ->
    KVs = case ets:lookup(Tree, {dn, BaseDN}) of
              [{_, _KVs}|_] ->
                  _KVs;
              [] ->
                  []
          end,
    Rest = case Level of
               baseObject ->
                   [];
               _ ->
                   NewLevel = if Level /= wholeSubtree ->
                                      baseObject;
                                 true ->
                                      Level
                              end,
                   lists:flatmap(
                     fun({_, D}) ->
                             NewDN = if BaseDN == [] ->
                                             D;
                                        true ->
                                             [D|BaseDN]
                                     end,
                             filter(F, NewDN, Tree, NewLevel)
                     end, ets:lookup(Tree, BaseDN))
           end,
    if BaseDN == [], Level /= baseObject ->
            Rest;
       true ->
            case F(KVs) of
                true ->
                    [BaseDN|Rest];
                false ->
                    Rest
            end
    end.

keyfind(K, KVs) ->
    keyfind(str:to_lower(K), KVs, []).

keyfind(K, [{K1, V}|T], Acc) ->
    case str:to_lower(K1) of
        K ->
            keyfind(K, T, [V|Acc]);
        _ ->
            keyfind(K, T, Acc)
    end;
keyfind(_, [], Acc) ->
    Acc.

member(E, Es) ->
    member1(str:to_lower(E), Es).

member1(E, [H|T]) ->
    case str:to_lower(H) of
        E ->
            true;
        _ ->
            member1(E, T)
    end;
member1(_, []) ->
    false.

load_ldif(Path) ->
    case file:open(Path, [read, binary]) of
        {ok, Fd} ->
            {ok, resort(format(read_lines(Fd, []), [], []))};
        Err ->
            ?ERROR_MSG("failed to read LDIF file: ~p", [Err]),
            Err
    end.

read_lines(Fd, Acc) ->
    case file:read_line(Fd) of
        {ok, Str} ->
            Line = process_line(str:strip(Str, right, $\n)),
            read_lines(Fd, [Line|Acc]);
        eof ->
            Acc;
        Err ->
            Err
    end.

process_line(<<C, _/binary>> = L) when C/=$ , C/=$\t, C/=$\n ->
    case str:chr(L, $:) of
        0 ->
            <<>>;
        Pos ->
            NewPos = Pos - 1,
            case L of
                <<Val:NewPos/binary, $:, $:, Rest/binary>> ->
                    {Val, base64, str:strip(Rest, left, $ )};
                <<Val:NewPos/binary, $:, Rest/binary>> ->
                    {Val, plain, str:strip(Rest, left, $ )}
            end
    end;
process_line([_|L]) ->
    L;
process_line(_) ->
    <<>>.

format([{Val, Type, L}|T], Ls, Acc) ->
    Str1 = iolist_to_binary([L|Ls]),
    Str2 = case Type of
               plain -> Str1;
               base64 -> base64:decode(Str1)
           end,
    format(T, [], [{Val, Str2}|Acc]);
format([<<"-">>|T], Ls, Acc) ->
    format(T, Ls, Acc);
format([L|T], Ls, Acc) ->
    format(T, [L|Ls], Acc);
format([], _, Acc) ->
    lists:reverse(Acc).

resort(T) ->
    resort(T, [], [], ets:new(ldap_tree, [named_table, public, bag])).

resort([{<<"dn">>, S}|T], Ls, DNs, Tree) ->
    case proplists:get_value(<<"changetype">>, Ls, <<"add">>) of
        <<"add">> ->
            [H|Rest] = DN = str:tokens(S, <<",">>),
            ets:insert(Tree, {{dn, DN}, Ls}),
            ets:insert(Tree, {Rest, H}),
            resort(T, [], [DN|DNs], Tree);
        _ ->
            resort(T, [], DNs, Tree)
    end;
resort([AttrVal|T], Ls, DNs, Acc) ->
    resort(T, [AttrVal|Ls], DNs, Acc);
resort([], _, DNs, Tree) ->
    {_, TopDNs} = lists:foldl(
                    fun(D, {L, Acc}) ->
                            NewL = length(D),
                            if NewL < L ->
                                    {NewL, [D]};
                               NewL == L ->
                                    {L, [D|Acc]};
                               true ->
                                    {L, Acc}
                            end
                    end, {unlimited, []}, DNs),
    Attrs = lists:map(
              fun(TopDN) ->
                      ets:insert(Tree, {[], TopDN}),
                      {<<"namingContexts">>, str:join(TopDN, <<",">>)}
              end, TopDNs),
    Attrs1 = [{<<"supportedLDAPVersion">>, <<"3">>},
              {<<"objectClass">>, <<"top">>}|Attrs],
    ets:insert(Tree, {{dn, []}, Attrs1}),
    Tree.
