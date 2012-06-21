%%
%% Parser of the Redis protocol, see http://redis.io/topics/protocol
%%
%% The idea behind this parser is that we accept any binary data
%% available on the socket. If there is not enough data to parse a
%% complete response, we ask the caller to call us later when there is
%% more data. If there is too much data, we only parse the first
%% response and let the caller call us again with the rest.
%%
%% This approach lets us write a "pure" parser that does not depend on
%% manipulating the socket, which erldis and redis-erl is
%% doing. Instead, we may ask the socket to send us data as fast as
%% possible and parse it continously. The overhead of manipulating the
%% socket when parsing multibulk responses is killing the performance
%% of erldis.
%%
%% Future improvements:
%%  * Instead of building a binary all the time in the continuation,
%%    build an iolist
%%  * When we return a bulk continuation, we also include the size of
%%    the bulk. The caller may use this to explicitly call
%%    gen_tcp:recv/2 with the desired size.

-module(eredis_parser).
-author('knut.nesheim@wooga.com').

-export([init/0, parse/2]).

%% Exported for testing
-export([parse_bulk/1, parse_bulk/2,
         parse_multibulk/1, parse_multibulk/2]).

-include_lib("eredis.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%% API
%%

%% @doc: Initialize the parser
init() ->
    #pstate{}.


-spec parse(State::#pstate{}, Data::binary()) ->
                   {ok, return_value(), NewState::#pstate{}} |
                       {ok, return_value(), Rest::binary(), NewState::#pstate{}} |
                       {error, ErrString::binary(), NewState::#pstate{}} |
                       {error, ErrString::binary(), Rest::binary(), NewState::#pstate{}} |
                       {continue, NewState::#pstate{}}.

%% @doc: Parses the (possibly partial) response from Redis. Returns
%% either {ok, Value, NewState}, {ok, Value, Rest, NewState} or
%% {continue, NewState}. External entry point for parsing.
%%
%% In case {ok, Value, NewState} is returned, Value contains the value
%% returned by Redis. NewState will be an empty parser state.
%%
%% In case {ok, Value, Rest, NewState} is returned, Value contains the
%% most recent value returned by Redis, while Rest contains any extra
%% data that was given, but was not part of the same response. In this
%% case you should immeditely call parse again with Rest as the Data
%% argument and NewState as the State argument.
%%
%% In case {continue, NewState} is returned, more data is needed
%% before a complete value can be returned. As soon as you have more
%% data, call parse again with NewState as the State argument and any
%% new binary data as the Data argument.

%% Parser in initial state, the data we receive will be the beginning
%% of a response
parse(#pstate{state = undefined} = State, NewData) ->
    %% Look at the first byte to get the type of reply
    case NewData of
        %% Status
        <<$+, Data/binary>> ->
            return_result(parse_simple(Data), State, status_continue);

        %% Error
        <<$-, Data/binary>> ->
            return_error(parse_simple(Data), State, status_continue);

        %% Integer reply
        <<$:, Data/binary>> ->
            return_result(parse_simple(Data), State, status_continue);

        %% Multibulk
        <<$*, _Rest/binary>> ->
            return_result(parse_multibulk(NewData), State, multibulk_continue);

        %% Bulk
        <<$$, _Rest/binary>> ->
            return_result(parse_bulk(NewData), State, bulk_continue);

        _ ->
            %% TODO: Handle the case where we start parsing a new
            %% response, but cannot make any sense of it
            {error, unknown_response}
    end;

%% The following clauses all match on different continuation states

parse(#pstate{state = bulk_continue,
              continuation_data = ContinuationData} = State, NewData) ->
    return_result(parse_bulk(ContinuationData, NewData), State, bulk_continue);

parse(#pstate{state = multibulk_continue,
              continuation_data = ContinuationData} = State, NewData) ->
    return_result(parse_multibulk(ContinuationData, NewData), State, multibulk_continue);

parse(#pstate{state = status_continue,
             continuation_data = ContinuationData} = State, NewData) ->
    return_result(parse_simple(ContinuationData, NewData), State, status_continue).

%%
%% MULTIBULK
%%

parse_multibulk(<<$*, _/binary>> = Data) ->
    case get_newline_pos(Data) of
        undefined ->
            {continue, {incomplete_size, Data}};
        NewlinePos ->
            OffsetNewlinePos = NewlinePos - 1,
            <<$*, Size:OffsetNewlinePos/binary, ?NL, Bulk/binary>> = Data,
            IntSize = list_to_integer(binary_to_list(Size)),

            do_parse_multibulk(IntSize, Bulk)
    end.

%% Size of multibulk was incomplete, try again
parse_multibulk({incomplete_size, PartialData}, NewData0) ->
    NewData = <<PartialData/binary, NewData0/binary>>,
    parse_multibulk(NewData);

%% Ran out of data inside do_parse_multibulk in parse_bulk, must
%% continue traversing the bulks
parse_multibulk({in_parsing_bulks, Count, OldData, Acc},
                NewData0) ->
    NewData = <<OldData/binary, NewData0/binary>>,

    %% Continue where we left off
    do_parse_multibulk(Count, NewData, Acc).

%% @doc: Parses the given number of bulks from Data. If Data does not
%% contain enough bulks, {continue, ContinuationData} is returned with
%% enough information to start parsing with the correct count and
%% accumulated data.
do_parse_multibulk(Count, Data) ->
    do_parse_multibulk(Count, Data, []).

do_parse_multibulk(-1, Data, []) ->
    {ok, undefined, Data};
do_parse_multibulk(0, Data, Acc) ->
    {ok, lists:reverse(Acc), Data};
do_parse_multibulk(Count, <<>>, Acc) ->
    {continue, {in_parsing_bulks, Count, <<>>, Acc}};
do_parse_multibulk(Count, Data, Acc) ->
    %% Try parsing the first bulk in Data, if it works, we get the
    %% extra data back that was not part of the bulk which we can
    %% recurse on.  If the bulk does not contain enough data, we
    %% return with a continuation and enough data to pick up where we
    %% left off. In the continuation we will get more data
    %% automagically in Data, so parsing the bulk might work.
    case parse_bulk(Data) of
        {ok, Value, Rest} ->
            do_parse_multibulk(Count - 1, Rest, [Value | Acc]);
        {continue, _} ->
            {continue, {in_parsing_bulks, Count, Data, Acc}}
    end.

%%
%% BULK
%%

parse_bulk(<<$*, _Rest/binary>> = Data) -> parse_multibulk(Data);
parse_bulk(<<$+, Data/binary>>) -> parse_simple(Data);
parse_bulk(<<$-, Data/binary>>) -> parse_simple(Data);
parse_bulk(<<$:, Data/binary>>) -> parse_simple(Data);

%% Bulk, at beginning of response
parse_bulk(<<$$, _/binary>> = Data) ->
    %% Find the position of the first terminator, everything up until
    %% this point contains the size specifier. If we cannot find it,
    %% we received a partial response and need more data
    case get_newline_pos(Data) of
        undefined ->
            {continue, {incomplete_size, Data}};
        NewlinePos ->
            OffsetNewlinePos = NewlinePos - 1, % Take into account the first $
            <<$$, Size:OffsetNewlinePos/binary, Bulk/binary>> = Data,
            IntSize = list_to_integer(binary_to_list(Size)),

            if
                %% Nil response from redis
                IntSize =:= -1 ->
                    <<?NL, Rest/binary>> = Bulk,
                    {ok, undefined, Rest};
                %% We have enough data for the entire bulk
                size(Bulk) - (size(<<?NL>>) * 2) >= IntSize ->
                    <<?NL, Value:IntSize/binary, ?NL, Rest/binary>> = Bulk,
                    {ok, Value, Rest};
                true ->
                    %% Need more data, so we send the bulk without the
                    %% size specifier to our future self
                    {continue, {IntSize, Bulk}}
            end
    end.

%% Bulk, continuation from partial bulk size
parse_bulk({incomplete_size, PartialData}, NewData0) ->
    NewData = <<PartialData/binary, NewData0/binary>>,
    parse_bulk(NewData);

%% Bulk, continuation from partial bulk value
parse_bulk({IntSize, Acc0}, Data) ->
    Acc = <<Acc0/binary, Data/binary>>,

    if
        size(Acc) - (size(<<?NL>>) * 2) >= IntSize ->
            <<?NL, Value:IntSize/binary, ?NL, Rest/binary>> = Acc,
            {ok, Value, Rest};
        true ->
            {continue, {IntSize, Acc}}
    end.


%%
%% SIMPLE REPLIES
%%
%% Handles replies on the following format:
%%   TData\r\n
%% Where T is a type byte, like '+', '-', ':'. Data is terminated by \r\n

%% @doc: Parse simple replies. Data must not contain type
%% identifier. Type must be handled by the caller.
parse_simple(Data) ->
    case get_newline_pos(Data) of
        undefined ->
            {continue, {incomplete_simple, Data}};
        NewlinePos ->
            <<Value:NewlinePos/binary, ?NL, Rest/binary>> = Data,
            {ok, Value, Rest}
    end.

parse_simple({incomplete_simple, OldData}, NewData0) ->
    NewData = <<OldData/binary, NewData0/binary>>,
    parse_simple(NewData).

%%
%% INTERNAL HELPERS
%%
get_newline_pos(B) ->
    case re:run(B, ?NL) of
        {match, [{Pos, _}]} -> Pos;
        nomatch -> undefined
    end.


%% @doc: Helper for handling the result of parsing. Will update the
%% parser state with the continuation of given name if necessary.
return_result({ok, Value, <<>>}, _State, _StateName) ->
    {ok, Value, init()};
return_result({ok, Value, Rest}, _State, _StateName) ->
    {ok, Value, Rest, init()};
return_result({continue, ContinuationData}, State, StateName) ->
    {continue, State#pstate{state = StateName, continuation_data = ContinuationData}}.

%% @doc: Helper for returning an error. Uses return_result/3 and just transforms the {ok, ...} tuple into an error tuple
return_error(Result, State, StateName) ->
    case return_result(Result, State, StateName) of
        {ok, Value, ParserState} ->
            {error, Value, ParserState};
        {ok, Value, Rest, ParserState} ->
            {error, Value, Rest, ParserState};
        Res ->
            Res
    end.
