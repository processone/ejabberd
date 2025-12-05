#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz _build/default/lib/erlydtl/ebin

main([Pattern, OutFile]) ->
    Phrases = sources_parser:parse_pattern([Pattern]),
    Msgs = lists:foldl(
             fun(Phrase, M) ->
                     [MsgId, File, Line] = sources_parser:phrase_info([msgid, file, line], Phrase),
                     L = maps:get(MsgId, M, []),
                     M#{MsgId => [{File, Line} | L]}
             end, #{}, Phrases),
    {ok, Fd} = file:open(OutFile, [write]),
    maps:foreach(
      fun(MsgId, Places) ->
              lists:foreach(
                fun({File, Line}) ->
                        file:write(Fd, io_lib:format("#: ~s:~p~n", [File, Line]))
                end, lists:reverse(Places)),
              file:write(Fd, io_lib:format("msgid ~p~nmsgstr \"\"~n~n", [MsgId]))
      end, Msgs),
    file:close(Fd).
