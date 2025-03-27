%%%----------------------------------------------------------------------
%%% File    : mod_adhoc_api.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Frontend for ejabberd API Commands via XEP-0050 Ad-Hoc Commands
%%% Created : 21 Feb 2025 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

%%%% definitions
%% @format-begin

-module(mod_adhoc_api).

-behaviour(gen_mod).

-author('badlop@process-one.net').

%% gen_mod callbacks
-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]).
%% hooks
-export([adhoc_local_commands/4, adhoc_local_items/4, disco_local_features/5,
         disco_local_identity/5, disco_local_items/5]).

-include("ejabberd_commands.hrl").
-include("ejabberd_sm.hrl").
-include("logger.hrl").
-include("translate.hrl").

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(DEFAULT_API_VERSION, 1000000).

%%%==================================
%%%% gen_mod

start(_Host, _Opts) ->
    {ok,
     [{hook, adhoc_local_commands, adhoc_local_commands, 40},
      {hook, adhoc_local_items, adhoc_local_items, 40},
      {hook, disco_local_features, disco_local_features, 40},
      {hook, disco_local_identity, disco_local_identity, 40},
      {hook, disco_local_items, disco_local_items, 40}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

mod_opt_type(default_version) ->
    econf:either(
        econf:int(0, 3),
        econf:and_then(
            econf:binary(),
            fun(Binary) ->
               case binary_to_list(Binary) of
                   F when F >= "24.06" ->
                       2;
                   F when (F > "23.10") and (F < "24.06") ->
                       1;
                   F when F =< "23.10" ->
                       0
               end
            end)).

-spec mod_options(binary()) -> [{default_version, integer()}].
mod_options(_) ->
    [{default_version, ?DEFAULT_API_VERSION}].

depends(_Host, _Opts) ->
    [{mod_adhoc, hard}, {mod_last, soft}].

mod_doc() ->
    #{desc =>
          ?T("Execute https://docs.ejabberd.im/developer/ejabberd-api/[API Commands] "
             "in a XMPP client using "
             "https://xmpp.org/extensions/xep-0050.html[XEP-0050: Ad-Hoc Commands]. "
             "This module requires _`mod_adhoc`_ (to execute the commands), "
             "and recommends _`mod_disco`_ (to discover the commands)."),
      note => "added in 25.03",
      opts =>
          [{default_version,
            #{value => "integer() | string()",
              desc =>
                  ?T("What API version to use. "
                     "If setting an ejabberd version, it will use the latest API "
                     "version that was available in that ejabberd version. "
                     "For example, setting '\"24.06\"' in this option implies '2'. "
                     "The default value is the latest version.")}}],
      example =>
          ["acl:",
           "  admin:",
           "    user: jan@localhost",
           "",
           "api_permissions:",
           "  \"adhoc commands\":",
           "    from: mod_adhoc_api",
           "    who: admin",
           "    what:",
           "      - \"[tag:roster]\"",
           "      - \"[tag:session]\"",
           "      - stats",
           "      - status",
           "",
           "modules:",
           "  mod_adhoc_api:",
           "    default_version: 2"]}.

%%%==================================
%%%% Ad-Hoc Commands (copied from mod_configure)

-define(INFO_IDENTITY(Category, Type, Name, Lang),
        [#identity{category = Category,
                   type = Type,
                   name = tr(Lang, Name)}]).
-define(INFO_COMMAND(Name, Lang),
        ?INFO_IDENTITY(<<"automation">>, <<"command-node">>, Name, Lang)).
-define(NODE(Name, Node),
        #disco_item{jid = jid:make(Server),
                    node = Node,
                    name = tr(Lang, Name)}).

-spec tokenize(binary()) -> [binary()].
tokenize(Node) ->
    str:tokens(Node, <<"/#">>).

-spec tr(binary(), binary()) -> binary().
tr(Lang, Text) ->
    translate:translate(Lang, Text).

%%%==================================
%%%% - disco identity

-spec disco_local_identity([identity()], jid(), jid(), binary(), binary()) ->
                              [identity()].
disco_local_identity(Acc, _From, #jid{lserver = LServer} = _To, Node, Lang) ->
    case tokenize(Node) of
        [<<"api-commands">>] ->
            ?INFO_COMMAND(?T("API Commands"), Lang);
        [<<"api-commands">>, CommandName] ->
            ?INFO_COMMAND(get_api_command_desc(CommandName, LServer), Lang);
        _ ->
            Acc
    end.

get_api_command_desc(NameAtom, Host) ->
    iolist_to_binary((get_api_command(NameAtom, Host))#ejabberd_commands.desc).

%%%==================================
%%%% - disco features

-spec disco_local_features(mod_disco:features_acc(), jid(), jid(), binary(), binary()) ->
                              mod_disco:features_acc().
disco_local_features(Acc, _From, #jid{lserver = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
        false ->
            Acc;
        _ ->
            case tokenize(Node) of
                [<<"api-commands">>] ->
                    {result, []};
                [<<"api-commands">>, _] ->
                    {result, [?NS_COMMANDS]};
                _ ->
                    Acc
            end
    end.

%%%==================================
%%%% - adhoc items

-spec adhoc_local_items(mod_disco:items_acc(), jid(), jid(), binary()) ->
                           mod_disco:items_acc().
adhoc_local_items(Acc, From, #jid{lserver = LServer, server = Server} = To, Lang) ->
    Items =
        case Acc of
            {result, Its} ->
                Its;
            empty ->
                []
        end,
    Nodes = recursively_get_local_items(From, global, LServer, <<"">>, Server, Lang),
    Nodes1 =
        lists:filter(fun(#disco_item{node = Nd}) ->
                        F = disco_local_features(empty, From, To, Nd, Lang),
                        case F of
                            {result, [?NS_COMMANDS]} ->
                                true;
                            _ ->
                                false
                        end
                     end,
                     Nodes),
    {result, Items ++ Nodes1}.

-spec recursively_get_local_items(jid(),
                                  global | vhost,
                                  binary(),
                                  binary(),
                                  binary(),
                                  binary()) ->
                                     [disco_item()].
recursively_get_local_items(From, PermLev, LServer, Node, Server, Lang) ->
    Items =
        case get_local_items2(From, {PermLev, LServer}, tokenize(Node), Server, Lang) of
            {result, Res} ->
                Res;
            {error, _Error} ->
                []
        end,
    lists:flatten(
        lists:map(fun(#disco_item{jid = #jid{server = S}, node = Nd} = Item) ->
                     if (S /= Server) or (Nd == <<"">>) ->
                            [];
                        true ->
                            [Item,
                             recursively_get_local_items(From, PermLev, LServer, Nd, Server, Lang)]
                     end
                  end,
                  Items)).

%%%==================================
%%%% - disco items

-spec disco_local_items(mod_disco:items_acc(), jid(), jid(), binary(), binary()) ->
                           mod_disco:items_acc().
disco_local_items(Acc, From, #jid{lserver = LServer} = To, Node, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
        false ->
            Acc;
        _ ->
            Items =
                case Acc of
                    {result, Its} ->
                        Its;
                    empty ->
                        [];
                    Other ->
                        Other
                end,
            case tokenize(Node) of
                LNode when (LNode == [<<"api-commands">>]) or (LNode == []) ->
                    case get_local_items2(From, {global, LServer}, LNode, jid:encode(To), Lang) of
                        {result, Res} ->
                            {result, Res};
                        {error, Error} ->
                            {error, Error}
                    end;
                _ ->
                    {result, Items}
            end
    end.

%%%==================================
%%%% - get_local_items2

-spec get_local_items2(jid(),
                       {global | vhost, binary()},
                       [binary()],
                       binary(),
                       binary()) ->
                          {result, [disco_item()]} | {error, stanza_error()}.
get_local_items2(_From, _Host, [], Server, Lang) ->
    {result, [?NODE(?T("API Commands"), <<"api-commands">>)]};
get_local_items2(From, {_, Host}, [<<"api-commands">>], _Server, Lang) ->
    {result, get_api_commands(From, Host, Lang)};
get_local_items2(_From, {_, _Host}, [<<"api-commands">>, _], _Server, _Lang) ->
    {result, []};
get_local_items2(_From, _Host, _, _Server, _Lang) ->
    {error, xmpp:err_item_not_found()}.

-spec get_api_commands(jid(), binary(), binary()) -> [disco_item()].
get_api_commands(From, Server, Lang) ->
    ApiVersion = mod_adhoc_api_opt:default_version(Server),
    lists:map(fun({Name, _Args, _Desc}) ->
                 NameBin = list_to_binary(atom_to_list(Name)),
                 ?NODE(NameBin, <<"api-commands/", NameBin/binary>>)
              end,
              ejabberd_commands:list_commands(ApiVersion, get_caller_info(From))).

%%%==================================
%%%% - adhoc commands

-define(COMMANDS_RESULT(LServerOrGlobal, From, To, Request, Lang),
        adhoc_local_commands(From, To, Request)).

-spec adhoc_local_commands(adhoc_command(), jid(), jid(), adhoc_command()) ->
                              adhoc_command() | {error, stanza_error()}.
adhoc_local_commands(Acc, From, To, #adhoc_command{node = Node} = Request) ->
    case tokenize(Node) of
        [<<"api-commands">>, _CommandName] ->
            ?COMMANDS_RESULT(LServer, From, To, Request, Lang);
        _ ->
            Acc
    end.

-spec adhoc_local_commands(jid(), jid(), adhoc_command()) ->
                              adhoc_command() | {error, stanza_error()}.
adhoc_local_commands(From,
                     #jid{lserver = LServer} = _To,
                     #adhoc_command{lang = Lang,
                                    node = Node,
                                    sid = SessionID,
                                    action = Action,
                                    xdata = XData} =
                         Request) ->
    LNode = tokenize(Node),
    ActionIsExecute = Action == execute orelse Action == complete,
    if Action == cancel ->
           #adhoc_command{status = canceled,
                          lang = Lang,
                          node = Node,
                          sid = SessionID};
       XData == undefined, ActionIsExecute ->
           case get_form(LServer, LNode, Lang) of
               {result, Form} ->
                   xmpp_util:make_adhoc_response(Request,
                                                 #adhoc_command{status = executing, xdata = Form});
               {error, Error} ->
                   {error, Error}
           end;
       XData /= undefined, ActionIsExecute ->
           case set_form(From, LServer, LNode, Lang, XData) of
               {result, Res} ->
                   xmpp_util:make_adhoc_response(Request,
                                                 #adhoc_command{xdata = Res, status = completed});
               %%{'EXIT', _} -> {error, xmpp:err_bad_request()};
               {error, Error} ->
                   {error, Error}
           end;
       true ->
           {error, xmpp:err_bad_request(?T("Unexpected action"), Lang)}
    end.

-spec get_form(binary(), [binary()], binary()) ->
                  {result, xdata()} | {error, stanza_error()}.
get_form(Host, [<<"api-commands">>, CommandName], Lang) ->
    get_form_api_command(CommandName, Host, Lang);
get_form(_Host, _, _Lang) ->
    {error, xmpp:err_service_unavailable()}.

-spec set_form(jid(), binary(), [binary()], binary(), xdata()) ->
                  {result, xdata() | undefined} | {error, stanza_error()}.
set_form(From, Host, [<<"api-commands">>, Command], Lang, XData) ->
    set_form_api_command(From, Host, Command, XData, Lang);
set_form(_From, _Host, _, _Lang, _XData) ->
    {error, xmpp:err_service_unavailable()}.

%%%==================================
%%%% API Commands

get_api_command(Name, Host) when is_binary(Name) ->
    get_api_command(binary_to_existing_atom(Name, latin1), Host);
get_api_command(Name, Host) when is_atom(Name) ->
    ApiVersion = mod_adhoc_api_opt:default_version(Host),
    ejabberd_commands:get_command_definition(Name, ApiVersion).

get_caller_info(#jid{user = User, server = Server} = From) ->
    #{tag => <<>>,
      usr => {User, Server, <<"">>},
      caller_server => Server,
      ip => get_ip_address(From),
      caller_module => ?MODULE}.

get_ip_address(#jid{user = User,
                    server = Server,
                    resource = Resource}) ->
    case ejabberd_sm:get_user_ip(User, Server, Resource) of
        {IP, _Port} when is_tuple(IP) ->
            IP;
        _ ->
            error_ip_address
    end.

%%%==================================
%%%% - get form

get_form_api_command(NameBin, Host, _Lang) ->
    Def = get_api_command(NameBin, Host),
    Title = list_to_binary(atom_to_list(Def#ejabberd_commands.name)),
    Instructions = get_instructions(Def),
    FieldsArgs =
        build_fields(Def#ejabberd_commands.args,
                     Def#ejabberd_commands.args_desc,
                     Def#ejabberd_commands.args_example,
                     Def#ejabberd_commands.policy,
                     get_replacements(Host),
                     true),
    FieldsArgsWithHeads =
        case FieldsArgs of
            [] ->
                [];
            _ ->
                [#xdata_field{type = fixed, label = ?T("Arguments")} | FieldsArgs]
        end,
    NodeFields = build_node_fields(),
    {result,
     #xdata{title = Title,
            type = form,
            instructions = Instructions,
            fields = FieldsArgsWithHeads ++ NodeFields}}.

get_replacements(Host) ->
    [{user, <<"">>},
     {localuser, <<"">>},
     {host, Host},
     {localhost, Host},
     {password, <<"">>},
     {newpass, <<"">>},
     {service, mod_muc_admin:find_hosts(Host)}].

build_node_fields() ->
    build_node_fields([node() | nodes()]).

build_node_fields([_ThisNode]) ->
    [];
build_node_fields(AtomNodes) ->
    [ThisNode | _] = Nodes = [atom_to_binary(Atom, latin1) || Atom <- AtomNodes],
    Options = [#xdata_option{label = N, value = N} || N <- Nodes],
    [#xdata_field{type = fixed, label = ?T("Clustering")},
     #xdata_field{type = 'list-single',
                  label = <<"ejabberd node">>,
                  var = <<"mod_adhoc_api_target_node">>,
                  values = [ThisNode],
                  options = Options}].

%%%==================================
%%%% - set form

set_form_api_command(From, Host, CommandNameBin, XData, _Lang) ->
    %% Description
    Def = get_api_command(CommandNameBin, Host),
    Title = list_to_binary(atom_to_list(Def#ejabberd_commands.name)),
    Instructions = get_instructions(Def),

    %% Arguments
    FieldsArgs1 = [Field || Field <- XData#xdata.fields, Field#xdata_field.type /= fixed],

    {Node, FieldsArgs} =
        case lists:keytake(<<"mod_adhoc_api_target_node">>, #xdata_field.var, FieldsArgs1) of
            {value, #xdata_field{values = [TargetNode]}, FAs} ->
                {binary_to_existing_atom(TargetNode, latin1), FAs};
            false ->
                {node(), FieldsArgs1}
        end,

    FieldsArgsWithHeads =
        case FieldsArgs of
            [] ->
                [];
            _ ->
                [#xdata_field{type = fixed, label = ?T("Arguments")} | FieldsArgs]
        end,

    %% Execute
    Arguments = api_extract_fields(FieldsArgs, Def#ejabberd_commands.args),
    ApiVersion = mod_adhoc_api_opt:default_version(Host),
    CallResult =
        ejabberd_cluster:call(Node,
                              mod_http_api,
                              handle,
                              [binary_to_existing_atom(CommandNameBin, latin1),
                               get_caller_info(From),
                               Arguments,
                               ApiVersion]),

    %% Command result
    FieldsResult2 =
        case CallResult of
            {200, RR} ->
                build_fields([Def#ejabberd_commands.result],
                             [Def#ejabberd_commands.result_desc],
                             [RR],
                             restricted,
                             [{host, Host}],
                             false);
            {Code, _ApiErrorCode, MessageBin} ->
                [#xdata_field{type = 'text-single',
                              label = <<"Error ", (integer_to_binary(Code))/binary>>,
                              values = encode(MessageBin, irrelevat_type),
                              var = <<"error">>}];
            {Code, MessageBin} ->
                [#xdata_field{type = 'text-single',
                              label = <<"Error ", (integer_to_binary(Code))/binary>>,
                              values = encode(MessageBin, irrelevat_type),
                              var = <<"error">>}]
        end,
    FieldsResultWithHeads =
        [#xdata_field{type = fixed, label = <<"">>},
         #xdata_field{type = fixed, label = ?T("Result")}
         | FieldsResult2],

    %% Result stanza
    {result,
     #xdata{title = Title,
            type = result,
            instructions = Instructions,
            fields = FieldsArgsWithHeads ++ FieldsResultWithHeads}}.

api_extract_fields(Fields, ArgsDef) ->
    lists:map(fun(#xdata_field{values = Values, var = ANameBin}) ->
                 ArgDef = proplists:get_value(binary_to_existing_atom(ANameBin, latin1), ArgsDef),
                 V = case {Values, ArgDef} of
                         {Values, {list, {_ElementName, {tuple, ElementsDef}}}} ->
                             [parse_tuple(ElementsDef, Value) || Value <- Values];
                         {[Value], {tuple, ElementsDef}} ->
                             parse_tuple(ElementsDef, Value);
                         {[Value], _} ->
                             Value;
                         _ ->
                             Values
                     end,
                 {ANameBin, V}
              end,
              Fields).

parse_tuple(ElementsDef, Value) ->
    Values = str:tokens(Value, <<":">>),
    List1 =
        [{atom_to_binary(Name, latin1), Val}
         || {{Name, _Type}, Val} <- lists:zip(ElementsDef, Values)],
    maps:from_list(List1).

%%%==================================
%%%% - get instructions

get_instructions(Def) ->
    Note2 =
        case Def#ejabberd_commands.note of
            [] ->
                [];
            Note ->
                N = iolist_to_binary(Note),
                [<<"Note: ", N/binary>>]
        end,
    Tags2 =
        case Def#ejabberd_commands.tags of
            [] ->
                [];
            Tags ->
                T = str:join([atom_to_binary(Tag, latin1) || Tag <- Tags], <<", ">>),
                [<<"Tags: ", T/binary>>]
        end,
    Module2 =
        case Def#ejabberd_commands.definer of
            unknown ->
                [];
            DefinerAtom ->
                D = atom_to_binary(DefinerAtom, latin1),
                [<<"Module: ", D/binary>>]
        end,
    Version2 =
        case Def#ejabberd_commands.version of
            0 ->
                [];
            Version ->
                V = integer_to_binary(Version),
                [<<"API version: ", V/binary>>]
        end,
    get_instructions2([Def#ejabberd_commands.desc, Def#ejabberd_commands.longdesc]
                      ++ Note2
                      ++ Tags2
                      ++ Module2
                      ++ Version2).

get_instructions2(ListStrings) ->
    [re:replace(String, "[\t]*[  ]+", " ", [{return, binary}, global])
     || String <- ListStrings, String /= ""].

%%%==================================
%%%% - build fields

build_fields(NameTypes, none, Examples, Policy, Replacements, Required) ->
    build_fields(NameTypes, [], Examples, Policy, Replacements, Required);
build_fields(NameTypes, Descs, none, Policy, Replacements, Required) ->
    build_fields(NameTypes, Descs, [], Policy, Replacements, Required);
build_fields(NameTypes, [none], Examples, Policy, Replacements, Required) ->
    build_fields(NameTypes, [], Examples, Policy, Replacements, Required);
build_fields(NameTypes, Descs, [none], Policy, Replacements, Required) ->
    build_fields(NameTypes, Descs, [], Policy, Replacements, Required);
build_fields(NameTypes, Descs, Examples, Policy, Replacements, Required) ->
    {NameTypes2, Descs2, Examples2} =
        case Policy of
            user ->
                {[{user, binary}, {host, binary} | NameTypes],
                 ["Username", "Server host" | Descs],
                 ["tom", "example.com" | Examples]};
            _ ->
                {NameTypes, Descs, Examples}
        end,
    build_fields2(NameTypes2, Descs2, Examples2, Replacements, Required).

build_fields2([{_ArgName, {list, _ArgNameType}}] = NameTypes,
              Descs,
              Examples,
              _Replacements,
              Required) ->
    Args = lists_zip3_pad(NameTypes, Descs, Examples),
    lists:map(fun({{AName, AType}, ADesc, AExample}) ->
                 ANameBin = list_to_binary(atom_to_list(AName)),
                 #xdata_field{type = 'text-multi',
                              label = ANameBin,
                              desc = list_to_binary(ADesc),
                              values = encode(AExample, AType),
                              required = Required,
                              var = ANameBin}
              end,
              Args);
build_fields2(NameTypes, Descs, Examples, Replacements, Required) ->
    Args = lists_zip3_pad(NameTypes, Descs, Examples),
    lists:map(fun({{AName, AType}, ADesc, AExample}) ->
                 ANameBin = list_to_binary(atom_to_list(AName)),
                 AValue = proplists:get_value(AName, Replacements, AExample),
                 Values = encode(AValue, AType),
                 Type =
                     case {AType, Values} of
                         {{list, _}, _} ->
                             'text-multi';
                         {string, [_, _ | _]} ->
                             'text-multi';
                         _ ->
                             'text-single'
                     end,
                 #xdata_field{type = Type,
                              label = ANameBin,
                              desc = make_desc(ADesc, AValue),
                              values = Values,
                              required = Required,
                              var = ANameBin}
              end,
              Args).

-ifdef(OTP_BELOW_26).

lists_zip3_pad(As, Bs, Cs) ->
    lists_zip3_pad(As, Bs, Cs, []).

lists_zip3_pad([A | As], [B | Bs], [C | Cs], Xs) ->
    lists_zip3_pad(As, Bs, Cs, [{A, B, C} | Xs]);
lists_zip3_pad([A | As], [B | Bs], Nil, Xs) when (Nil == none) or (Nil == []) ->
    lists_zip3_pad(As, Bs, [], [{A, B, ""} | Xs]);
lists_zip3_pad([A | As], Nil, [C | Cs], Xs) when (Nil == none) or (Nil == []) ->
    lists_zip3_pad(As, [], Cs, [{A, "", C} | Xs]);
lists_zip3_pad([A | As], Nil, Nil, Xs) when (Nil == none) or (Nil == []) ->
    lists_zip3_pad(As, [], [], [{A, "", ""} | Xs]);
lists_zip3_pad([], Nil, Nil, Xs) when (Nil == none) or (Nil == []) ->
    lists:reverse(Xs).

-else.

lists_zip3_pad(As, Bs, Cs) ->
    lists:zip3(As, Bs, Cs, {pad, {error_missing_args_def, "", ""}}).

-endif.

make_desc(ADesc, T) when is_tuple(T) ->
    T3 = string:join(tuple_to_list(T), " : "),
    iolist_to_binary([ADesc, " {", T3, "}"]);
make_desc(ADesc, M) when is_map(M) ->
    M2 = [binary_to_list(V) || V <- maps:keys(M)],
    M3 = string:join(M2, " : "),
    iolist_to_binary([ADesc, " {", M3, "}"]);
make_desc(ADesc, _M) ->
    iolist_to_binary(ADesc).

%%%==================================
%%%% - encode

encode({[T | _] = List}, Type) when is_tuple(T) ->
    encode(List, Type);
encode([T | _] = List, Type) when is_tuple(T) ->
    [encode(Element, Type) || Element <- List];
encode(T, _Type) when is_tuple(T) ->
    T2 = [x_to_binary(E) || E <- tuple_to_list(T)],
    T3 = str:join(T2, <<":">>),
    [T3];
encode(M, {tuple, Types}) when is_map(M) ->
    M2 = [x_to_list(maps:get(atom_to_binary(Key, latin1), M))
          || {Key, _ElementType} <- Types],
    M3 = string:join(M2, " : "),
    [iolist_to_binary(M3)];
encode([S | _] = SList, _Type) when is_list(S) ->
    [iolist_to_binary(A) || A <- SList];
encode([B | _] = BList, _Type) when is_binary(B) ->
    BList;
encode(I, _Type) when is_integer(I) ->
    [integer_to_binary(I)];
encode([M | _] = List, {list, {_Name, TupleType}}) when is_map(M) ->
    [encode(M1, TupleType) || M1 <- List];
encode(S, _Type) when is_list(S) ->
    [iolist_to_binary(S)];
encode(B, _Type) when is_binary(B) ->
    str:tokens(B, <<"\n">>).

x_to_list(B) when is_binary(B) ->
    binary_to_list(B);
x_to_list(I) when is_integer(I) ->
    integer_to_list(I);
x_to_list(L) when is_list(L) ->
    L.

x_to_binary(B) when is_binary(B) ->
    B;
x_to_binary(I) when is_integer(I) ->
    integer_to_binary(I);
x_to_binary(L) when is_list(L) ->
    iolist_to_binary(L).

%%%==================================

%%% vim: set foldmethod=marker foldmarker=%%%%,%%%=:
