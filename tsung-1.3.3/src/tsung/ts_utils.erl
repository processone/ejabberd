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

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

-module(ts_utils).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").

%% to get file_info record definition
-include_lib("kernel/include/file.hrl").

%% user interface
-export([debug/3, debug/4, get_val/1, init_seed/0, chop/1, elapsed/2,
         now_sec/0, node_to_hostname/1, add_time/2, keyumerge/3, key1search/2,
         level2int/1, mkey1search/2, close_socket/2, datestr/0, datestr/1,
         erl_system_args/0, erl_system_args/1, setsubdir/1, export_text/1,
         foreach_parallel/2, spawn_par/3, inet_setopts/3, resolve/2,
         stop_all/2, stop_all/3, stop_all/4, join/2, split/2, split2/2, split2/3,
         make_dir_rec/1, is_ip/1, from_https/1, to_https/1, keymax/2,
         check_sum/3, check_sum/5, clean_str/1, file_to_list/1, term_to_list/1,
         decode_base64/1, encode_base64/1, to_lower/1, release_is_newer_or_eq/1,
         randomstr/1,urandomstr/1,urandomstr_noflat/1, eval/1, list_to_number/1,
         time2sec/1, read_file_raw/1, init_seed/1, jsonpath/2
        ]).

level2int("debug")     -> ?DEB;
level2int("info")      -> ?INFO;
level2int("notice")    -> ?NOTICE;
level2int("warning")   -> ?WARN;
level2int("error")     -> ?ERR;
level2int("critical")  -> ?CRIT;
level2int("emergency") -> ?EMERG.

-define(QUOT,"&quot;").
-define(APOS,"&apos;").
-define(AMP,"&amp;").
-define(GT,"&gt;").
-define(LT,"&lt;").
-define(DUPSTR_SIZE,20).
-define(DUPSTR,"qxvmvtglimieyhemzlxc").

%%----------------------------------------------------------------------
%% Func: get_val/1
%% Purpose: return environnement variable value for the current application
%% Returns: Value | {undef_var, Var}
%%----------------------------------------------------------------------
get_val(Var) ->
    case application:get_env(Var) of
        {ok, Val} ->
            ensure_string(Var, Val);
        undefined -> % undef, application not started, try to get var from stdlib
            case application:get_env(stdlib,Var) of
                undefined -> {undef_var, Var};
                {ok,Val}  -> ensure_string(Var, Val)
            end
    end.


%% ensure atom to string conversion of environnement variable
%% This is intended to fix a problem making tsung run under Windows
%%  I convert parameter that are called from the command-line
ensure_string(log_file, Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
ensure_string(proxy_log_file, Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
ensure_string(config_file, Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
ensure_string(_, Other) ->
    Other.

%%----------------------------------------------------------------------
%% Func: debug/3
%% Purpose: print debug message if level is high enough
%%----------------------------------------------------------------------
debug(From, Message, Level) ->
    debug(From, Message, [], Level).

debug(From, Message, Args, Level) ->
    Debug_level = ?config(debug_level),
    if
        Level =< Debug_level ->
            error_logger:info_msg("~20s:(~p:~p) "++ Message,
                                  [From, Level, self()] ++ Args);
        true ->
            nodebug
    end.

%%----------------------------------------------------------------------
%% Func: elapsed/2
%% Purpose: print elapsed time in milliseconds
%% Returns: integer
%%----------------------------------------------------------------------
elapsed({Before1, Before2, Before3}, {After1, After2, After3}) ->
    After  = After1  * 1000000000  + After2  * 1000 + After3/1000,
    Before = Before1 * 1000000000  + Before2 * 1000 + Before3/1000,
    After - Before.

%%----------------------------------------------------------------------
%% Func: chop/1
%% Purpose: remove trailing "\n"
%%----------------------------------------------------------------------
chop(String) ->
    string:strip(String, right, 10).

%%----------------------------------------------------------------------
%% Func: clean_str/1
%% Purpose: remove "\n" and space at the beginning and at that end of a string
%%----------------------------------------------------------------------
clean_str(String) ->
    Str1 = string:strip(String, both, 10),
    Str2 = string:strip(Str1),
    Str3 = string:strip(Str2, both, 10),
    string:strip(Str3).


%%----------------------------------------------------------------------
%% Func: init_seed/1
%%----------------------------------------------------------------------
init_seed(now)->
    init_seed();
init_seed(A) when is_integer(A)->
    %% in case of a distributed test, we don't want each launcher to
    %% have the same seed, therefore, we need to know the id of the
    %% node to set a reproductible but different seed for each launcher.
    Id=get_node_id(),
    ?LOGF("Seeding with ~p on node ~p~n",[Id,node()],?DEB),
    random:seed(Id,Id,A);
init_seed({A,B,C}) ->
    random:seed(A,B,C).

get_node_id() ->
    case string:tokens(atom_to_list(node()),"@") of
        ["tsung_control"++_,_]    -> 123456;
        ["tsung"++I,_]            -> list_to_integer(I);
        _                         -> 654321
    end.

%%----------------------------------------------------------------------
%% Func: init_seed/0
%%----------------------------------------------------------------------
init_seed()->
    init_seed(now()).

%%----------------------------------------------------------------------
%% Func: now_sec/0
%% Purpose: returns unix like elapsed time in sec
%%----------------------------------------------------------------------
now_sec() ->
    time2sec(now()).

time2sec({MSec, Seconds, _}) ->
    Seconds+1000000*MSec.

%%----------------------------------------------------------------------
%% Func: add_time/2
%% Purpose: add given Seconds to given Time (same format as now())
%%----------------------------------------------------------------------
add_time({MSec, Seconds, MicroSec}, SecToAdd) when is_integer(SecToAdd)->
    NewSec = Seconds +SecToAdd,
    case NewSec < 1000000 of
        true -> {MSec, NewSec, MicroSec};
        false ->{MSec+ (NewSec div 100000), NewSec-1000000, MicroSec}
    end.

node_to_hostname(Node) ->
    [_Nodename, Hostname] = string:tokens( atom_to_list(Node), "@"),
    {ok, Hostname}.

to_lower(String)->
    case check_httpd_old_version() of
        false ->
            string:to_lower(String);
        true  ->
            httpd_util:to_lower(String)
    end.

encode_base64(String)->
    case check_httpd_old_version() of
        false ->
            base64:encode_to_string(String);
        true  ->
            httpd_util:encode_base64(String)
    end.

decode_base64(Base64)->
    case check_httpd_old_version() of
        false ->
            base64:decode_to_string(Base64);
        true ->
            httpd_util:decode_base64(Base64)
    end.

%% check erlang version to know if we need to use the old httpd_utils functions
check_httpd_old_version()-> not release_is_newer_or_eq("5.5.4").

% return true if current version of erlang is newer or equal
release_is_newer_or_eq(Release)->
    erlang:system_info(version) >= Release.


%%----------------------------------------------------------------------
%% Func: key1search/2
%% Purpose: wrapper around httpd_utils module funs (maybe one day
%%          these functions will be added to the stdlib)
%%----------------------------------------------------------------------
key1search(Tuple,String)->
    case release_is_newer_or_eq("5.7") of %% should be removed in R13B
        true ->
            proplists:get_value(String,Tuple);
        false ->
            httpd_util:key1search(Tuple,String)
    end.

%%----------------------------------------------------------------------
%% Func: mkey1search/2
%% Purpose: multiple key1search:
%%  Take as input list of {Key, Value} tuples (length 2).
%%  Return the list of values corresponding to a given key
%%  It is assumed here that there might be several identical keys in the list
%%  unlike the lists:key... functions.
%%----------------------------------------------------------------------
mkey1search(List, Key) ->
    Results = lists:foldl(
                fun({MatchKey, Value}, Acc) when MatchKey == Key ->
                        [Value | Acc];
                   ({_OtherKey, _Value}, Acc) ->
                        Acc
                end,
                [],
                List),
    case Results of
        [] -> undefined;
        Results -> lists:reverse(Results)
    end.

%% close socket if it exists
close_socket(_Protocol, none) -> ok;
close_socket(gen_tcp, Socket)-> gen_tcp:close(Socket);
close_socket(ssl, Socket)    -> ssl:close(Socket);
close_socket(gen_udp, Socket)-> gen_udp:close(Socket).

%%----------------------------------------------------------------------
%% datestr/0
%% Purpose: print date as a string 'YYYY:MM:DD-HH:MM'
%%----------------------------------------------------------------------
datestr()->
    datestr(erlang:universaltime()).

%%----------------------------------------------------------------------
%% datestr/1
%%----------------------------------------------------------------------
datestr({{Y,M,D},{H,Min,_S}})->
    io_lib:format("~w~2.10.0b~2.10.0b-~2.10.0b:~2.10.0b",[Y,M,D,H,Min]).

%%----------------------------------------------------------------------
%% erl_system_args/0
%%----------------------------------------------------------------------
erl_system_args()->
    erl_system_args(extended).
erl_system_args(basic)->
    Rsh = case  init:get_argument(rsh) of
              {ok,[[Value]]}  -> " -rsh " ++ Value;
              _ -> " "
          end,
    lists:append([Rsh, " -detached -setcookie  ",
                  atom_to_list(erlang:get_cookie()) ]);
erl_system_args(extended)->
    BasicArgs  = erl_system_args(basic),
    SetArg = fun(A) -> case init:get_argument(A) of
                           error     -> " ";
                           {ok,[[]]} -> " -" ++atom_to_list(A)++" ";
                           {ok,[[Val|_]]} when is_list(Val)-> " -" ++atom_to_list(A)++" "++Val++" "
                       end
             end,
    Shared = SetArg(shared),
    Hybrid = SetArg(hybrid),
    case  ?config(smp_disable) of
        true ->   Smp = " -smp disable ";
        _    ->   Smp = SetArg(smp)
    end,
    Inet = case init:get_argument(kernel) of
               {ok,[["inetrc",InetRcFile]]} ->
                   ?LOGF("Get inetrc= ~p~n",[InetRcFile],?NOTICE),
                   " -kernel inetrc '"++ InetRcFile ++ "'" ;
               _ -> " "
           end,
    Threads= "+A "++integer_to_list(erlang:system_info(thread_pool_size))++" ",
    ProcessMax="+P "++integer_to_list(erlang:system_info(process_limit))++" ",
    Mea = case  erlang:system_info(version) of
              "5.3" ++ _Tail     -> " +Mea r10b ";
              _ -> " "
          end,
    lists:append([BasicArgs, Shared, Hybrid, Smp, Mea, Inet, Threads,ProcessMax]).

%%----------------------------------------------------------------------
%% setsubdir/1
%% Purpose: all log files are created in a directory whose name is the
%%          start date of the test.
%% ----------------------------------------------------------------------
setsubdir(FileName) ->
    Date = datestr(),
    Path = filename:dirname(FileName),
    Base = filename:basename(FileName),
    Dir  = filename:join(Path, Date),
    case file:make_dir(Dir) of
        ok ->
            {ok, {Dir, Base}};
        {error, eexist} ->
            ?DebugF("Directory ~s already exist~n",[Dir]),
            {ok, {Dir, Base}};
        Err ->
            ?LOGF("Can't create directory ~s (~p)!~n",[Dir, Err],?EMERG),
            {error, Err}
    end.

%%----------------------------------------------------------------------
%% export_text/1
%% Purpose: Escape special characters `<', `&', `'' and `"' flattening
%%          the text.
%%----------------------------------------------------------------------
export_text(T) ->
    export_text(T, []).

export_text(Bin, Cont) when is_binary(Bin) ->
    export_text(binary_to_list(Bin), Cont);
export_text([], Exported) ->
    lists:flatten(lists:reverse(Exported));
export_text([$< | T], Cont) ->
    export_text(T, [?LT | Cont]);
export_text([$> | T], Cont) ->
    export_text(T, [?GT | Cont]);
export_text([$& | T], Cont) ->
    export_text(T, [?AMP | Cont]);
export_text([$' | T], Cont) -> %'
    export_text(T, [?APOS | Cont]);
export_text([$" | T], Cont) -> %"
    export_text(T, [?QUOT | Cont]);
export_text([C | T], Cont) ->
    export_text(T, [C | Cont]).

%%----------------------------------------------------------------------
%% stop_all/2
%%----------------------------------------------------------------------
stop_all(Host, Name) ->
    stop_all(Host, Name, "Tsung").

stop_all([Host],Name,MsgName)  ->
    VoidFun = fun(_A)-> ok end,
    stop_all([Host],Name,MsgName, VoidFun).

stop_all([Host],Name,MsgName,Fun) when is_atom(Host) ->
    _List= net_adm:world_list([Host]),
    global:sync(),
    case global:whereis_name(Name) of
        undefined ->
            Msg = MsgName ++" is not running on " ++ atom_to_list(Host),
            erlang:display(Msg);
        Pid ->
            Controller_Node = node(Pid),
            Fun(Controller_Node),
            slave:stop(Controller_Node)
    end;
stop_all(_,_,_,_)->
    erlang:display("Bad Hostname").

%%----------------------------------------------------------------------
%% make_dir_rec/1
%% Purpose: create directory. Missing parent directories ARE created
%%----------------------------------------------------------------------
make_dir_rec(DirName) when is_list(DirName) ->
    case  file:read_file_info(DirName) of
        {ok, #file_info{type=directory}} ->
            ok;
        {error,enoent} ->
            make_dir_rec("", filename:split(DirName));
        {error, Reason}  ->
            {error,Reason}
    end.

make_dir_rec(_Path, []) ->
    ok;
make_dir_rec(Path, [Parent|Childs]) ->
    CurrentDir=filename:join([Path,Parent]),
    case  file:read_file_info(CurrentDir) of
        {ok, #file_info{type=directory}} ->
            make_dir_rec(CurrentDir, Childs);
        {error,enoent} ->
            case file:make_dir(CurrentDir) of
                ok ->
                    make_dir_rec(CurrentDir, Childs);
                Error ->
                    Error
            end;
        {error, Reason}  ->
            {error,Reason}
    end.

%% check if a string is an IPv4 address (as "192.168.0.1")
is_ip(String) when is_list(String) ->
    EightBit="(2[0-4][0-9]|25[0-5]|1[0-9][0-9]|[0-9][0-9]|[0-9])",
    RegExp = lists:append(["^",EightBit,"\.",EightBit,"\.",EightBit,"\.",EightBit,"$"]), %"
    case regexp:first_match(String, RegExp) of
       {match,_,_} -> true;
       _ -> false
    end;
is_ip(_) -> false.

%%----------------------------------------------------------------------
%% to_https/1
%% Purpose: rewrite https URL, to act as a pure non ssl proxy
%%----------------------------------------------------------------------
to_https({url, "http://-"++Rest})-> "https://" ++ Rest;
to_https({url, URL})-> URL;
to_https({request, {body,Data}}) when is_list(Data) ->
    %% body request, no headers
    {ok,RealBody,_Count} = regexp:gsub(Data,"http://ssl-","https://"),
    {ok, RealBody};
to_https({request, S="CONNECT"++Rest}) -> {ok,S};
to_https({request, String}) when is_list(String) ->
    EndOfHeader = string:str(String, "\r\n\r\n"),
    Header = string:substr(String, 1, EndOfHeader - 1) ++ "\r\n",
    Body = string:substr(String, EndOfHeader + 4),
    {ok,TmpHeader,_} = regexp:gsub(Header,"http://-","https://"),
    {ok,TmpHeader2,_} = regexp:gsub(TmpHeader,"Accept-Encoding: [0-9,a-zA-Z_]+\r\n",""),
    {ok,RealHeader,_} = regexp:gsub(TmpHeader2,"Host: -","Host: "),
    {ok,RealBody,Count} = regexp:gsub(Body,"http://-","https://"),
    RealString = RealHeader ++ "\r\n" ++ RealBody,
    {ok, RealString}.

from_https(String) when is_list(String)->
    %% remove Secure from Set-Cookie (TSUN-120)
    {match, Matches} = regexp:matches(String, "Set-Cookie: [^\r]*\r\n"),
    Fun = fun({0,Length}, {StrAcc, Pos}) ->
                  {StrAcc ++ string:substr(String, Pos), -1};
             ({Start,Length}, {StrAcc, Pos}) ->
                  SetCookie = string:substr(String, Start, Length),
                  {ok, WithoutSecure, _} = regexp:gsub(SetCookie, "; *Secure", ""),
                  {StrAcc ++ string:substr(String, Pos, Start - Pos) ++ WithoutSecure, Start + Length}
          end,
    {TmpString, _} = lists:foldl(Fun, {"", 1}, Matches ++ [{0, 0}]),
    %% if location is defined, don't count it (not included in Content-Length)
    Location = case regexp:first_match(TmpString,"Location: https") of
                   {match,_,_} -> -1;
                   _           ->  0
               end,
    {ok,NewString,RepCount} = regexp:gsub(TmpString,"https://","http://-"),
    {ok, NewString}.


%% A Perl-style join --- concatenates all strings in Strings,
%% separated by Sep.
join(_Sep, []) -> [];
join(Sep, List) when is_list(List)->
    join2(Sep, lists:reverse(List)).
join2(Sep, [First | List]) when is_integer(First)->
    join2(Sep, [integer_to_list(First) | List]);
join2(Sep, [First | List]) when is_float(First)->
    join2(Sep, [float_to_list(First) | List]);
join2(Sep, [First | List]) when is_list(First)->
        lists:foldl(fun(X, Sum) -> X ++ Sep ++ Sum end, First, List).

%% split a string  (at first occurence of char)
split(String,Chr) ->
    {ok, List} = regexp:split(String,Chr),
    List.

%% split a string in 2 (at first occurence of char)
split2(String,Chr) ->
    split2(String,Chr,nostrip).

split2(String,Chr,strip) -> % split and strip blanks
    {A, B} = split2(String,Chr,nostrip),
    {string:strip(A), string:strip(B)};
split2(String,Chr,nostrip) ->
    case string:chr(String, Chr) of
        0   -> {String,[]};
        Pos -> {string:substr(String,1,Pos-1), string:substr(String,Pos+1)}
    end.


foreach_parallel(Fun, List)->
    SpawnFun = fun(A) -> spawn(?MODULE, spawn_par, lists:append([[Fun,self()], [A]])) end,
    lists:foreach(SpawnFun, List),
    wait_pids(length(List)).

wait_pids(0) -> done;
wait_pids(N) ->
    receive
        {ok, _Pid, _Res } ->
            wait_pids(N-1)
    after ?TIMEOUT_PARALLEL_SPAWN ->
            {error, {timout, N}} % N missing answer
    end.

spawn_par(Fun, PidFrom, Args) ->
    Res = Fun(Args),
    PidFrom ! {ok, self(), Res}.

%%----------------------------------------------------------------------
%% Func: inet_setopts/3
%% Purpose: set inet options depending on the protocol (gen_tcp, gen_udp,
%%  ssl)
%%----------------------------------------------------------------------
inet_setopts(_, none, _) -> %socket was closed before
    none;
inet_setopts(ssl, Socket, Opts) ->
    case ssl:setopts(Socket, Opts) of
        ok ->
            Socket;
        {error, closed} ->
            none;
        Error ->
            ?LOGF("Error while setting ssl options ~p ~p ~n", [Opts, Error], ?ERR),
            none
    end;
inet_setopts(_Type, Socket,  Opts)->
    case inet:setopts(Socket, Opts) of
        ok ->
            Socket;
        {error, closed} ->
            none;
        Error ->
            ?LOGF("Error while setting inet options ~p ~p ~n", [Opts, Error], ?ERR),
            none
    end.

%%----------------------------------------------------------------------
%% Func: check_sum/3
%% Purpose: check sum of int equals 100.
%% Args: List of tuples, index of int in tuple, Error msg
%% Returns ok | {error, {bad_sum, Msg}}
%%----------------------------------------------------------------------
check_sum(RecList, Index, ErrorMsg) ->
    %% popularity may be a float number. 5.10-2 precision
    check_sum(RecList, Index, 100, 0.05, ErrorMsg).
check_sum(RecList, Index, Total, Epsilon, ErrorMsg) ->
    %% we use the tuple representation of a record !
    Sum = lists:foldl(fun(X, Sum) -> element(Index,X)+Sum end, 0, RecList),
    Delta = abs(Sum - Total),
    case Delta < Epsilon of
        true -> ok;
        false -> {error, {bad_sum, Sum ,ErrorMsg}}
    end.

%%----------------------------------------------------------------------
%% Func: file_to_list/1
%% Purpose: read a file line by line and put them in a list
%% Args: filename
%% Returns {ok, List} | {error, Reason}
%%----------------------------------------------------------------------
file_to_list(FileName) ->
    case file:open(FileName, [read]) of
        {error, Reason} ->
            {error, Reason};
        {ok , File} ->
            Lines = read_lines(File),
            file:close(File),
            {ok, Lines}
    end.

read_lines(FD) ->read_lines(FD,io:get_line(FD,""),[]).

read_lines(_FD, eof, L) ->
    lists:reverse(L);
read_lines(FD, Line, L) ->
    read_lines(FD, io:get_line(FD,""),[chop(Line)|L]).

%%----------------------------------------------------------------------
%% Func: keyumerge/3
%% Purpose: Same as lists:keymerge, but remove duplicates (use items from A)
%% Returns: List
%%----------------------------------------------------------------------
keyumerge(_N,[],B)->B;
keyumerge(N,[A|Rest],B)->
    Key = element(N,A),
    % remove old values if it exists
    NewB = lists:keydelete(Key, N, B),
    keyumerge(N,Rest, [A|NewB]).

%%----------------------------------------------------------------------
%% Func: keymax/2
%% Purpose: Return Max of Nth element of a list of tuples
%% Returns: Number
%%----------------------------------------------------------------------
keymax(N,[L])-> element(N,L);
keymax(N,[E|Tail])->
    keymax(N,Tail,element(N,E)).

keymax(_N,[],Max)-> Max;
keymax(N,[E|Tail],Max)->
    keymax(N,Tail,lists:max([Max,element(N,E)])).

%%--------------------------------------------------------------------
%% Function: resolve/2
%% Description: return cached hostname or gethostbyaddr for given ip
%%--------------------------------------------------------------------
resolve(Ip, Cache) ->
    case lists:keysearch(Ip, 1, Cache) of
        {value, {Ip, ReverseHostname}} ->
            {ReverseHostname, Cache};
        false ->
            case inet:gethostbyaddr(Ip) of
                {ok, {hostent,ReverseHostname,_,inet,_,_}} ->
                    %% cache dns result and return it
                    ?LOGF("Add ~p -> ~p to DNS cache ~n", [Ip, ReverseHostname],?DEB),
                    {ReverseHostname, [{Ip, ReverseHostname} | Cache]};
                {error, Reason} ->
                    ?LOGF("DNS resolution error on ~p: ~p~n", [Ip, Reason],?WARN),
                    %% cache dns name as IP : {ip, ip} and return Ip
                    NewCache = lists:keymerge(1, Cache, [{Ip, Ip}]),
                    {Ip, NewCache}
            end
    end.

%%----------------------------------------------------------------------
%% @spec urandomstr_noflat(Size::integer()) ->string()
%% @doc generate pseudo-random list of given size. Implemented by
%% duplicating list of fixed size to be faster. unflatten version
%% @end
%%----------------------------------------------------------------------
urandomstr_noflat(Size) when is_integer(Size) , Size >= ?DUPSTR_SIZE ->
    Msg= lists:duplicate(Size div ?DUPSTR_SIZE,?DUPSTR),
    case Size rem ?DUPSTR_SIZE of
        0->
            Msg;
        Rest ->
            lists:append(Msg,urandomstr_noflat(Rest))
    end;
urandomstr_noflat(Size)  when is_integer(Size), Size >= 0 ->
    lists:nthtail(?DUPSTR_SIZE-Size, ?DUPSTR).

%%----------------------------------------------------------------------
%% @spec urandomstr(Size::integer()) ->string()
%% @doc same as urandomstr_noflat/1, but returns a flat list.
%% @end
%%----------------------------------------------------------------------
urandomstr(Size) when is_integer(Size), Size >= 0 ->
    lists:flatten(urandomstr_noflat(Size)).

%%----------------------------------------------------------------------
%% @spec randomstr(Size::integer()) ->string()
%% @doc returns a random string. slow if Size is high.
%% @end
%%----------------------------------------------------------------------
randomstr(Size) when is_integer(Size), Size >= 0 ->
     lists:map(fun (_) -> random:uniform(25) + $a  end, lists:seq(1,Size)).


%%----------------------------------------------------------------------
%% @spec eval(string()) -> term()
%% @doc evaluate strings as Erlang code at runtime
%% @end
%%----------------------------------------------------------------------
eval(Code) ->
    {ok, Scanned, _} = erl_scan:string(lists:flatten(Code)),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Result, _} = erl_eval:exprs(Parsed,  erl_eval:new_bindings()),
    Result.

%%----------------------------------------------------------------------
%% @spec list_to_number(string()) -> integer() | float()
%% @doc  convert a 'number' to either int or float
%% @end
%%----------------------------------------------------------------------
list_to_number(Number) ->
    try list_to_integer(Number) of
        Int -> Int
  catch
      error:_Reason ->
          list_to_float(Number)
  end.

term_to_list(I) when is_integer(I)->
    integer_to_list(I);
term_to_list(I) when is_atom(I)->
    atom_to_list(I);
term_to_list(I) when is_list(I)->
    I;
term_to_list(I) when is_float(I)->
    float_to_list(I);
term_to_list(B) when is_binary(B)->
    binary_to_list(B).

read_file_raw(File) when is_list(File) ->
    case {file:open(File,[read,raw,binary]), file:read_file_info(File)} of
        { {ok,IODev}, {ok,#file_info{size=Size} } } ->
            case file:pread(IODev,0,Size) of
                {ok, Res} ->
                    file:close(IODev),
                    {ok, Res, Size};
                Else ->
                    ?LOGF("pread file ~p of size ~p: ~p~n",[File,Size,Else],?NOTICE),
                    file:close(IODev),
                    Else
            end;
        {{ok,IODev}, {error, Reason} } ->
            file:close(IODev),
            {error,Reason};
        {{error,Reason},_} ->
            {error, Reason}
    end.


%%----------------------------------------------------------------------
%% @spec jsonpath(JSONPath::string(),JSON::iolist()) -> term()
%% @doc  very limited implementation of JSONPath from JSON struct.
%% @end
%%----------------------------------------------------------------------
jsonpath("$."++JSONPath,JSON) ->
    jsonpath(JSONPath,JSON);
jsonpath(JSONPath,JSON) ->
    Fun= fun(A) ->
                case catch list_to_integer(A) of
                    I when is_integer(I) ->
                        I+1;
                    E ->
                        list_to_binary(A)
                end
          end,
    Str=re:replace(JSONPath,"\\[(.*)\\]","\.\\1",[{return,list},global]),
    Keys=lists:map(Fun, string:tokens(Str,".")),
    json_get_bin(Keys,JSON).
json_get_bin([],Val) ->
    Val;
json_get_bin([Key|Keys],undefined) ->
    undefined;
json_get_bin([N|Keys],L) when is_integer(N), N =< length(L) ->
    Val =  lists:nth(N,L),
    json_get_bin(Keys,Val);
json_get_bin([<<"?",Expr/binary>> | Keys],L) when  is_list(L) ->
    case string:tokens(binary_to_list(Expr),"=") of
        [Key,Val] ->
            Fun = fun(S) -> case json_get_bin([list_to_binary(Key)],S) of
                                Int when is_integer(Int) ->
                                    integer_to_list(Int) =:= Val;
                                Other when is_binary(Other)->
                                    binary_to_list(Other) =:= Val
                            end
                  end,
            ?LOG("ok~n",?ERR),
            case lists:filter(Fun,L) of
                [Res|_] -> % keep the first result only
                    json_get_bin(Keys,Res);
                [] ->
                    undefined
            end;
        _ ->
            undefined
    end;
json_get_bin([Key|Keys],{struct,JSON}) when is_list(JSON) ->
    Val =  proplists:get_value(Key,JSON),
    json_get_bin(Keys,Val);
json_get_bin(_,_) ->
    undefined.
