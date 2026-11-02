-module(mod_antispam_util).

-export([for_all_hosts/2,
         try_call_by_host/3,
         sets_equal/2,
         format/2
        ]).

-define(COMMAND_TIMEOUT, timer:seconds(30)).

for_all_hosts(F, A) ->
    try [apply(F, [Host | A]) || Host <- get_spam_filter_hosts()] of
        List ->
            case lists:filter(fun ({error, _}) ->
                                      true;
                                  (_) ->
                                      false
                              end,
                              List)
            of
                [] ->
                    hd(List);
                Errors ->
                    hd(Errors)
            end
    catch
        error:{badmatch, {error, _Reason} = Error} ->
            Error
    end.

try_call_by_host(Host, Module, Call) ->
    LServer = jid:nameprep(Host),
    try Proc = gen_mod:get_module_proc_check(LServer, Module),
        gen_server:call(Proc, Call, ?COMMAND_TIMEOUT)
    of
        Result ->
            Result
    catch
        error:badarg ->
            {error, "Not configured for " ++ binary_to_list(Host)};
        exit:{noproc, _} ->
            {error, "Not configured for " ++ binary_to_list(Host)};
        exit:{timeout, _} ->
            {error, "Timeout while querying ejabberd"}
    end.

-spec get_spam_filter_hosts() -> [binary()].
get_spam_filter_hosts() ->
    [H || H <- ejabberd_option:hosts(), gen_mod:is_loaded(H, mod_antispam)].

-spec sets_equal(sets:set(), sets:set()) -> boolean().
sets_equal(A, B) ->
    sets:is_subset(A, B) andalso sets:is_subset(B, A).

-spec format(io:format(), [term()]) -> binary().
format(Format, Data) ->
    iolist_to_binary(io_lib:format(Format, Data)).
