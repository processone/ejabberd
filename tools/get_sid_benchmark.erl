-module(get_sid_benchmark).

-compile([export_all]).

%% Example results for 1mln of repetitions and data(2):
%%
%%    > get_sid_benchmark:timeit().
%%    exml: 59.420159s 1.0
%%    re  : 5.92969s 0.09979256366513593
%%
%% 1mln of repetitions and data(1):
%%
%%    > get_sid_benchmark:timeit().
%%    exml: 11.557594s 1.0
%%    re  : 3.143657s 0.27199925866923513

re() ->
    {ok, MP} = re:compile("sid=('(\\S+)'|\"(\\S+)\")", [caseless, ungreedy]),
    MP.

-define(NRUNS, 1000 * 1000).
%-define(NRUNS, 1000).
%-define(NRUNS, 10).

exemplar() ->
    [<<"123qwe">> || _ <- lists:seq(1, ?NRUNS)].

data(1) ->
    [exml:to_binary({xmlelement, <<"body">>, [{<<"sid">>, <<"123qwe">>}], []})
     || _ <- lists:seq(1, ?NRUNS)];
data(2) ->
    D = {xmlelement,<<"body">>,
         [{<<"xmlns:xmpp">>,<<"urn:xmpp:xbosh">>},
          {<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},
          {<<"to">>,<<"localhost">>},
          {<<"xml:lang">>,<<"en">>},
          {<<"wait">>,<<"60">>},
          {<<"hold">>,<<"1">>},
          {<<"xmpp:version">>,<<"1.0">>},
          {<<"content">>,<<"text/xml; charset=utf-8">>},
          {<<"rid">>,<<"1363778552433754">>},
          {<<"sid">>,<<"123qwe">>}],
         []},
    [exml:to_binary(D) || _ <- lists:seq(1, ?NRUNS)].

exml_get(B) ->
    {ok, El} = exml:parse(B),
    exml_query:attr(El, <<"sid">>).

mk_re_get() ->
    RE = re(),
    fun(B) ->
            {match, [_,_,{Skip,Take}]} = re:run(B, RE),
            << _:Skip/bytes, Sid:Take/bytes, _/bytes >> = B,
            Sid
    end.

timeit() ->
    Data = data(2),

    Exml = fun() ->
            [exml_get(B) || B <- Data]
    end,
    RE = fun() ->
            F = mk_re_get(),
            [F(B) || B <- Data]
    end,

    {ExmlMS, _} = timer:tc(Exml),
    {REMS, _} = timer:tc(RE),

    ExmlFrac = 1.0,
    REFrac = REMS / ExmlMS,

    io:format("exml: ~ps ~p~n", [ExmlMS / 1000 / 1000, ExmlFrac]),
    io:format("re  : ~ps ~p~n", [REMS / 1000 / 1000, REFrac]).
