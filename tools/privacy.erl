-module(privacy).

-compile([export_all]).

-include("../apps/ejabberd/include/mod_privacy.hrl").

-define(MAX_ORDER, 5).
-define(MAX_LIST_SIZE, 5).
-define(LIST_NAME_MAGIC, 100000).
-define(MAX_LISTS, 5).

-define(ensure_type(X), case Type of binary -> list_to_binary(X); string -> X end).
-define(ensure_list(X), if is_binary(X) -> binary_to_list(X);
                           true -> X end).

context_type(X) when is_binary(X) -> binary_to_list(X);
context_type(X)                   -> X.

init() -> random:seed(), ok.

-spec rand(list(A)) -> A.
rand(L) -> lists:nth(random:uniform(length(L)),L).

action() ->
    rand([allow, deny]).

make_jid(Type, UserPrefix, UserHost, UserCount) ->
    make(jid, Type, UserPrefix, UserHost, UserCount).

make_key(Type, UserPrefix, UserHost, UserCount) ->
    make(key, Type, UserPrefix, UserHost, UserCount).

-spec make(jid | key, binary | string, string(), string(), integer()) -> fun().
make(What, Type, UserPrefix, UserHost, UserCount)
    when What =:= jid;
         What =:= key ->
    RandUser = fun() ->
        ?ensure_type(UserPrefix
            ++ integer_to_list(random:uniform(UserCount))) end,
    Host = ?ensure_type(UserHost),
    case What of
        jid ->
            fun() -> { case rand([0,1]) of 0 -> RandUser(); 1 -> ?ensure_type("") end,
                       Host,
                       ?ensure_type("") } end;
        key ->
            fun() -> { RandUser(), Host } end
    end.

%% {listitem,jid,{<<"bob">>,<<"localhost">>,<<>>},deny,1,true,false,false,false,false}
make_list_item(Type, UserPrefix, UserHost, UserCount) ->
    Jid = make_jid(Type, UserPrefix, UserHost, UserCount),
    RandBlockBy = fun() -> % someday maybe: rand([jid,group,subscription])
                         jid end,
    RandValue = fun(BlockBy) ->
        case BlockBy of
            jid -> Jid()
            % maybe someday
            %subscription -> ok;
            %group -> ok
        end
    end,
    RandAction = fun() -> rand([allow,deny]) end,
    RandOrder = fun() -> random:uniform(?MAX_ORDER) end,
    RandBool = fun() -> rand([true,false]) end,
    fun() ->
        MatchAll = RandBool(),
        MatchOther = fun() ->
            case MatchAll of
                true -> false;
                false -> RandBool()
            end end,
        BlockBy = RandBlockBy(),
        {listitem, BlockBy, RandValue(BlockBy), 
            RandAction(),
            RandOrder(),
            MatchAll,
            MatchOther(), MatchOther(), MatchOther(), MatchOther()} end.

randsize_list(Gen, Size) ->
    lists:map(
        fun(_) -> Gen() end,
        lists:seq(1, random:uniform(Size))).

make_list(Type, UserPrefix, UserHost, UserCount) ->
    Item = make_list_item(Type, UserPrefix, UserHost, UserCount),
    fun() -> randsize_list(Item, ?MAX_LIST_SIZE) end.

%% TODO: add a list with name that can be determined from jid
make_privacy(Type, UserPrefix, UserHost, UserCount) ->
    List = make_list(Type, UserPrefix, UserHost, UserCount),
    RandKey = make_key(Type, UserPrefix, UserHost, UserCount),
    ListName = fun({User,Host}) ->
                    ?ensure_type(lists:concat([
                        ?ensure_list(User),
                        "@",
                        ?ensure_list(Host),
                        "_list_",
                        integer_to_list(random:uniform(?LIST_NAME_MAGIC)) ]));
                  ({User,Host,nomagic}) ->
                    ?ensure_type(lists:concat([
                        ?ensure_list(User),
                        "@",
                        ?ensure_list(Host),
                        "_list"])) end,
    fun() ->
        Key = {User,Host} = RandKey(),
        {Lists1, Default} =
            lists:mapfoldr(
                fun(_, _) ->
                    Name = ListName(Key), {{ Name, List() }, Name } end,
                acc0,
                lists:seq(1, random:uniform(?MAX_LISTS))),
        Lists = [ {ListName({User,Host,nomagic}), List()} | Lists1 ],
        #privacy{us=Key, default=Default, lists=Lists} end.

inject_lists(Type, UserPrefix, UserHost, UserCount) ->
    P = make_privacy(Type, UserPrefix, UserHost, UserCount),
    InjectOne = fun(_) -> mnesia:dirty_write(P()) end,
    lists:foreach(InjectOne, lists:seq(1, UserCount)).
