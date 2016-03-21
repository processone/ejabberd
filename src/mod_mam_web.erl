%%%-------------------------------------------------------------------
%%% @author Ippolitov Igor (iippolitov@gmail.com)
%%% @doc
%%%      Message Archive Management (XEP-0313) Web representation
%%% @end
%%% Created : 3 September, 2015, Ippolitov Igor (iippolitov@gmail.com)
%%%
%%%
%%% ejabberd, Copyright (C) 2013-2015   ProcessOne
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
%%%-------------------------------------------------------------------
-module(mod_mam_web).
-author('iippolitov@gmail.com').
%-compile([debug_info,export_all]).

-behaviour(gen_mod).

-export([process/2, reload_templates/1, reload_templates/0, get_translations/1]).
-export([start/2, stop/1, mod_opt_type/1, opt_type/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("mod_muc_room.hrl").
-include("ejabberd_http.hrl").
-include("mod_mam.hrl").
-include("ejabberd.hrl").


-record(rd, {muchost, host, dbtype, prefix, lang}).
%%%===================================================================
%%% This is used to do some preparations
%%%===================================================================
start(Host, _Opts) -> 
    reload_templates(Host),
    ok.

stop(_) -> ok.

%%%===================================================================
%%% API
%%%===================================================================
get_translations(Host) ->
    ?DEBUG("Listing strings for trasnaltion in WEB MAM templates for ~p",[Host]),
    L1 = case catch erlang:apply(
                 erlang:binary_to_atom(<<"mam_web_", Host/binary>>, utf8),
                 translatable_strings,
                 []) of
             L when is_list(L) -> L;
             {'EXIT', _Err} -> ?DEBUG("Caught internal server error."
                                     " Got: ~p",[_Err]), [];
             _Err -> ?DEBUG("Internal server error listing translations."
                            " Got: ~p",[_Err]), []
         end,
    L2 = case catch erlang:apply(
                 erlang:binary_to_atom(<<"mam_web_", Host/binary>>, utf8),
                 translated_blocks,
                 []) of
             L5 when is_list(L5) -> L5;
             {'EXIT',_Err2} -> ?DEBUG("Caught internal server error."
                                     " Got: ~p",[_Err2]), [];
             _Err2 -> ?DEBUG("Internal server error rendering."
                            " Got: ~p",[_Err2]), []
         end,
     L1 ++ L2.

mod_opt_type(templates_dir) -> fun erlang:binary_to_list/1;
mod_opt_type(jslinkify) -> fun(B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [templates_dir, jslinkify].
opt_type(templates_dir) -> fun erlang:binary_to_list/1;
opt_type(jslinkify) -> fun(B) when is_boolean(B) -> B end;
opt_type(_) -> [templates_dir, jslinkify].
    
reload_templates() ->
    lists:foreach(fun reload_templates/1, ?MYHOSTS).
reload_templates(Host) ->
    ?DEBUG("Compiling MAM Web templates for ~p",[Host]),
    TemplateDir = gen_mod:get_module_opt(Host, ?MODULE, templates_dir,
                                          fun erlang:binary_to_list/1,
                                           "priv/templates/mam_web/"),
    JS = gen_mod:get_module_opt(Host, ?MODULE, jslinkify,
                               fun(B) when is_boolean(B) -> B end,
                               true),
    erlydtl:compile_dir(TemplateDir, 
                        erlang:binary_to_atom(<<"mam_web_", Host/binary>>, utf8),
                        [{record_info, [{html_msg, record_info(fields, html_msg)},
                                         {muc_room, record_info(fields, muc_room)}]}
                        ,{auto_escape, JS}
                        ] ).
    
process([<<"conferences">>], #request{method = 'GET'} = R) ->
    RD = get_request_details(R),
    Rooms = ets:select(muc_online_room, make_room_match(RD#rd.muchost)),
    render(conference_list, RD, 
           [{rooms, lists:filter(fun muc_logs_enabled/1, Rooms)}]);

process([<<"conferences">>, MUC_Name | T], R=#request{method = 'GET'}) ->
    RD = get_request_details(R),
    MUC = case jlib:string_to_jid(MUC_Name) of 
        #jid{} = A -> 
                    M = make_room_match(A#jid.luser,A#jid.lserver),
                    case catch ets:select(muc_online_room, M) of
                        [#muc_online_room{} = Room] -> Room;
                        _Error -> 
                          ?DEBUG("Error getting room for ~p using ~p: ~p",[MUC_Name, M, _Error]),
                          #muc_online_room{}
                    end;
        _ -> #muc_online_room{}
        end,
    case muc_logs_enabled(MUC) of
        true  -> render_muc_mam(MUC, T, RD);
        false -> ?DEBUG("Logs not found for ~p", [MUC]), http_not_found()
    end;

process([<<"users">> | _], #request{method = 'GET'}) ->
    %TODO: list user logs. AAA
    http_no_data();

%% Some "default behaviour" functions.
process([], #request{method = 'OPTIONS', data = <<>>}) ->
    http_ok(<<>>);
process([], #request{method = 'HEAD'}) ->
    http_ok(<<>>);
process(_Path, _Request) ->
    ?DEBUG("Bad Request at ~p: ~p", [_Path, _Request]),
    http_bad_request().

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_request_details( #request{} = R) ->
    % TODO: configure which host names correspond to which conference hosts
    LServer = case proplists:get_value(<<"Muc-Host">>, R#request.headers) of
        undefined -> H = jlib:nameprep(R#request.host),
                     case lists:member(H, ?MYHOSTS) of
                         true -> H;
                         false -> lists:nth(1, ?MYHOSTS)
                     end;
                H -> jlib:nameprep(H)
    end,
    #rd{muchost=gen_mod:get_module_opt_host(LServer, mod_muc, <<"conference.@HOST@">>),
        host=LServer,
        dbtype={gen_mod:db_type(LServer, mod_mam), LServer},
        prefix=get_path_prefix(R#request.path),
        lang=R#request.lang };
get_request_details ( _ ) -> {}.
    

% Get a room option. If empty room is passed - return default provided
get_muc_config_option(_Option, #muc_online_room{} = MUC, Default)
                           when MUC#muc_online_room.pid == self() -> 
    ?DEBUG("Got self referencing muc, returning Default",[]), Default;

get_muc_config_option(Option, #muc_online_room{} = MUC, Default) ->
    case catch gen_fsm:sync_send_all_state_event(
                       MUC#muc_online_room.pid, get_config) of
        {'EXIT', _Timeout} -> ?DEBUG("Got an error reading room config: ~p",
                                     [_Timeout]),
                               Default;
         {ok, #config{}=C} -> case index_of(Option, 
                                           record_info(fields, config)) of
                                  -1 -> Default;
                                   I -> element(I+1, C)
                              end;
                         _ -> Default
    end.

% some convenience functions.
muc_logs_enabled(#muc_online_room{} = MUC) ->
    get_muc_config_option(mam, MUC, false);
muc_logs_enabled(_) -> false .

muc_title(#muc_online_room{} = MUC) ->
    get_muc_config_option(title, MUC, <<>>);
muc_title(_) -> <<>>.

muc_description(#muc_online_room{} = MUC) ->
    get_muc_config_option(description, MUC, <<>>);
muc_description(_) -> <<>>.

% list of years
render_muc_mam(#muc_online_room{} = MUC, [], RD) -> 
     render(conference_years, RD, [
                    {muc_name, muc_to_string(MUC)},
                    {dates, get_distinct_dates(MUC, years, RD#rd.dbtype)},
                    {year, <<>>},
                    {title, muc_title(MUC)},
                    {description, muc_description(MUC)}
                    ]);

% list of months
render_muc_mam(#muc_online_room{} = MUC, [Year], RD) -> 
     case str:to_integer(Year) of
     {Y,_} when is_integer(Y) ->
         render(conference_years, RD, 
                [ {muc_name, muc_to_string(MUC)},
                  {dates, get_distinct_dates(MUC, {months, Y}, RD#rd.dbtype)},
                  {year, Y},
                  {title, muc_title(MUC)},
                  {description, muc_description(MUC)}
                ]);
     _ -> http_not_found()
     end;

% list of days
render_muc_mam(#muc_online_room{} = MUC, [Y, M], RD) -> 
     Month = lists:keyfind(list_to_integer(binary:bin_to_list(M)), 1,
                  proplists:get_value(list_to_integer(binary:bin_to_list(Y)),
                   get_distinct_dates(MUC, {days,{Y,M}}, RD#rd.dbtype), [])),
     case Month of 
         false -> http_not_found();
             _ -> render(conference_month, RD,  
                   [ {muc_name, muc_to_string(MUC)},
                     {year, Y},
                     {month, Month},
                     {title, muc_title(MUC)},
                     {description, muc_description(MUC)},
                     {prefix, RD#rd.prefix},
                     {locale, RD#rd.lang}
                    ])
     end;

% day messages per day
render_muc_mam(#muc_online_room{} = MUC, [Y, M, D], RD) ->
    case  {str:to_integer(Y), str:to_integer(M), str:to_integer(D)} of
        {{Yi, _}, {Mi, _}, {Di, _}} 
        when is_integer(Yi), is_integer(Mi), is_integer(Di) ->
            case calendar:valid_date(Yi, Mi, Di) of
              false -> http_not_found();
               true -> Msgs = get_messages(MUC, Yi, Mi, Di, RD#rd.dbtype),
                       render(conference_day, RD, [
                               {header, muc_to_string(MUC)},
                               {title, muc_title(MUC)},
                               {description, muc_description(MUC)},
                               {messages, Msgs},
                               {prefix, RD#rd.prefix}])
            end;
         _ -> http_not_found()
    end;

% catch all case
render_muc_mam(_MUC, _Tail, _DB) ->
    ?DEBUG("Got wrong request for ~p, with tail ~p.", [_MUC, _Tail]),
    http_not_found().

render(Template, RD, VarList) ->
    render(Template, RD, 
           [{prefix, RD#rd.prefix}|VarList], % prefix is required everywhere...
           [{locale, RD#rd.lang}
           ,{translation_fun, fun translate/2}
           ]).
render(Template, RD, VarList, Options) ->
    Host = RD#rd.host, 
    case catch erlang:apply(
                 erlang:binary_to_atom(<<"mam_web_", Host/binary>>, utf8),
                 Template,
                 [VarList, Options]) of
             L when is_list(L) -> http_ok(L);
             {'EXIT',_ERR} -> ?DEBUG("Internal server error rendering. Got: ~p",[_ERR]),
                              http_server_error()
         end.



get_messages(MUC, Y, M, D, {odbc, Server}) ->
    Start = datetime_to_now({{Y,M,D},{0,0,0}}, 0),
    End   = datetime_to_now({{Y,M,D},{23,59,59}}, 999999),
    case ejabberd_odbc:sql_query( Server,
            [<<"SELECT timestamp, nick, txt, xml"
            ,  " from archive where username='">>
            ,  ejabberd_odbc:escape(muc_to_string(MUC))
            , <<"' and timestamp >= ">>, jlib:integer_to_binary(now_to_usec(Start))
            , <<"  and timestamp <= ">>, jlib:integer_to_binary(now_to_usec(End))
            , <<";">>]) of
         {selected, _, L} -> lists:map(to_html_message(Server), L);
            {error, R} -> ?DEBUG("An error occured with a query: ~p", [R]), [];
                    _  -> ?DEBUG("Something strange with a query",[]), []
    end;
get_messages(MUC, Y, M, D, {mnesia, Server}) ->
    Seconds = calendar:datetime_to_gregorian_seconds(
                    {{Y,M,D}, {0,0,0}}) - 62167219200,
    Start = {Seconds div 1000000, Seconds rem 1000000, 0},
    EndSeconds = Seconds + 86400,
    End = {EndSeconds div 1000000, EndSeconds rem 1000000, 0},
    MS = make_matchspec(element(1, MUC#muc_online_room.name_host), 
                        element(2, MUC#muc_online_room.name_host),
                        Start, End),
    lists:map(to_html_message(Server), 
                  mnesia:dirty_select(archive_msg, MS)).

store_months_day(YearNum, MonthNum, Day, Dict) ->
    Months = case orddict:is_key(YearNum, Dict) of
        true -> orddict:fetch(YearNum, Dict);
        false -> orddict:new()
    end,
    Days = case orddict:is_key(MonthNum, Months) of
        true -> orddict:fetch(MonthNum, Months);
        false -> ordsets:new()
    end,
    orddict:store(YearNum, orddict:store(MonthNum,ordsets:add_element(Day, Days), Months), Dict).

lod_of_dates_to_lol(LOD) ->
    lists:map(fun({Y,D}) -> {Y,
                  lists:map(
                    fun({M,S}) ->
                      {M, ordsets:to_list(S), {{Y,M,0}, {0,0,0}}}
                    end,
                  orddict:to_list(D)
              )} end,
              orddict:to_list(LOD)).
    
get_distinct_dates(MUC, _, {odbc, Server}) -> 
    Dates_of_messages = fun ([B], S) ->
        case catch erlang:binary_to_integer(B) of
            Ts when is_integer(Ts) -> 
                case calendar:now_to_universal_time(usec_to_now(Ts)) of
                    {{Y,M,D},{0,0,0}} -> store_months_day(Y, M, D, S);
                    _ -> S
                end;
            _ -> S 
        end
    end,
    % select distinct days in form of a timestamp (like now() )
    % strange math is required to get properly rounded value
    case ejabberd_odbc:sql_query( Server, 
            [<<"SELECT DISTINCT((timestamp/1000000/86400*86400*1000000))",
              " from archive where username='">>, 
              ejabberd_odbc:escape(muc_to_string(MUC)), <<"';">>]) of
         {selected, _, L} -> lod_of_dates_to_lol(
                             lists:foldl(Dates_of_messages, orddict:new(), L));
            {error, R} -> ?DEBUG("An error occured with a query: ~p", [R]), [];
                    _  -> ?DEBUG("Something strange with a query",[]), []
    end;

get_distinct_dates(MUC, _, {mnesia, _}) ->
    NH = MUC#muc_online_room.name_host,
    Dates_of_messages = fun (E, S) -> case E of
        #archive_msg{us=NH} -> 
            {{Y,M,D},_} = calendar:now_to_universal_time(E#archive_msg.timestamp),
            store_months_day(Y, M, D, S);
        _ -> S
            end
        end,
    Mod = case mnesia:table_info(archive_msg, storage_type) of
              disc_copies -> ets;
              ram_copies -> ets;
              disc_only_copies -> dets
          end,
    LOD = erlang:apply(Mod, foldl, [Dates_of_messages, orddict:new(), archive_msg]),
    lod_of_dates_to_lol(LOD).
    

to_html_message(Server) ->
    JsLinkify = gen_mod:get_module_opt(Server, ?MODULE, jslinkify,
                               fun(B) when is_boolean(B) -> B end,
                               true),
    fun (Text) -> archive_to_html_message(Text, not JsLinkify) end.

linkify(Text, false) -> 
    Text;
linkify(Text, true) -> 
    S2 = ejabberd_regexp:greplace(Text, <<"\\&">>,
                                  <<"\\&amp;">>),
    S3 = ejabberd_regexp:greplace(S2, <<"<">>,
                                  <<"\\&lt;">>),
    S4 = ejabberd_regexp:greplace(S3, <<">">>,
                                  <<"\\&gt;">>),
    S5 = ejabberd_regexp:greplace(S4,
                                  <<"((http|https|ftp)://|(mailto|xmpp):)"
                                    "i([^] )'\"}]|&(?!(lt;|gt;|amp)))+">>,
                                   <<"<a href=\"&\" rel=\"nofollow\">&</a>">>),
    S6 = ejabberd_regexp:greplace(S5, <<"  ">>,
                                  <<"\\&nbsp;\\&nbsp;">>),
    S7 = ejabberd_regexp:greplace(S6, <<"\\t">>,
                                  <<"\\&nbsp;\\&nbsp;\\&nbsp;\\&nbsp;">>),
    ejabberd_regexp:greplace(S7, <<226, 128, 174>>,
                             <<"[RLO]">>).

% used for odbc, which may have text pre-extracted
archive_to_html_message([TS, Nick, <<>>, XML], _Linkify) ->
    #xmlel{} = El = xml_stream:parse_element(XML),
    Now = usec_to_now(erlang:binary_to_integer(TS)),
    archive_to_html_message(#archive_msg{timestamp = Now,
                                             packet = El,
                                              nick = Nick},
                            false);

archive_to_html_message([TS, Nick, Txt, _], Linkify) ->
    Now = usec_to_now(erlang:binary_to_integer(TS)),
    #html_msg{type = text,
         nick = Nick,
         timestamp = calendar:now_to_local_time(Now),
         ms = element(3, Now),
         text = linkify(Txt,Linkify)
    };
archive_to_html_message(#archive_msg{} = Msg, Linkify) ->
    {Type, Txt} =
      case {fxml:get_subtag_cdata(Msg#archive_msg.packet, <<"subject">>),
            fxml:get_subtag_cdata(Msg#archive_msg.packet, <<"body">>)}
          of
        {<<>>, <<>>}                 -> {none,<<>>};
        {<<>>, <<"/me ", T/binary>>} -> {action, T};
        {<<>>, <<"/me ">>}           -> {action, <<>>};
        {<<>>, <<"/me">>}            -> {action, <<>>};
        {<<>>, T}                    -> {text, T};
        {T, <<>>}                    -> {subject, T};
        {_, _} -> ?DEBUG("Unexpected return from get_subtag_cdata",[]), 
                  {none, <<>>}
      end,
    #html_msg{type = Type,
         nick = Msg#archive_msg.nick,
         timestamp = calendar:now_to_local_time(Msg#archive_msg.timestamp),
         ms = element(3, Msg#archive_msg.timestamp),
         text = linkify(Txt, Linkify)
    }.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% HTTP specific functions: various stand returns
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
http_ct_html() ->      {<<"Content-Type">>, <<"text/html; charset=utf-8">>}.
http_header() -> [http_ct_html()].
http_no_data() -> {204, http_header(), <<>>}.
http_ok(Data) -> {200, http_header(), Data}.
http_bad_request() -> {400, http_header(), #xmlel{name = <<"h1">>,
                 children = [{xmlcdata,<<"400 Bad Request">>}]}}.
%http_access_denied() -> {403, http_header(), #xmlel{name = <<"h1">>,
%                 children = [{xmlcdata,<<"403 Access Denied">>}]}}.
http_not_found() ->  {404, http_header(), #xmlel{name = <<"h1">>,
                 children = [{xmlcdata,<<"404 Not Found">>}]}}.
http_server_error() -> {503, http_header(), #xmlel{name = <<"h1">>,
                 children = [{xmlcdata,<<"503 Internal Server Error">>}]}}.

translate(Msg, {Locale, _Context}) ->
    %% ?DEBUG("translating ~p into ~p, avoid context: ~p", [Msg, Locale, _Context]),
    translate:translate(Locale, Msg);
translate(Msg, Locale) ->
    %% ?DEBUG("translating ~p into ~p", [Msg, Locale]),
    translate:translate(Locale, erlang:list_to_binary(Msg)).


make_room_match(_Host) ->
   ets:fun2ms(fun( E ) when E#muc_online_room.name_host == {'_',_Host} -> E end). 
make_room_match(_Name, _Host) ->
   ets:fun2ms(fun( E ) when E#muc_online_room.name_host == {_Name,_Host} -> E end). 

make_matchspec(LUser, LServer, Start, End) ->
    ets:fun2ms(
      fun(#archive_msg{timestamp = TS,
                       us = US,
                       peer = Peer} = Msg)
            when Start =< TS, End >= TS,
                 US == {LUser, LServer} ->
              Msg
      end).

now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.

usec_to_now(Int) ->
    Secs = Int div 1000000,
    USec = Int rem 1000000,
    MSec = Secs div 1000000,
    Sec = Secs rem 1000000,
    {MSec, Sec, USec}.

datetime_to_now(DateTime, USecs) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime) -
	calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    {Seconds div 1000000, Seconds rem 1000000, USecs}.

% finds an index of an element in a list. Either -1 is returned
index_of(Elem, List) ->
    index_of(Elem, List, 1).
index_of(_Elem, [_Elem|_Tail], Index) -> Index;
index_of(Elem, [_|Tail], Index) -> index_of(Elem, Tail, Index+1);
index_of(_Elem, [], _ ) -> -1.

% jlib lacks jid_to_string({N,S}) function
muc_to_string(#muc_online_room{} = M) ->
    jlib:jid_to_string({element(1, M#muc_online_room.name_host),
                        element(2, M#muc_online_room.name_host),
                        <<>> }).

get_path_prefix(List) -> get_path_prefix(List,["/"]).
get_path_prefix([H | T], Acc) -> get_path_prefix(T, "/"++[H|Acc]);
get_path_prefix([], Acc) -> lists:reverse(Acc).

