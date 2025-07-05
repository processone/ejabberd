%% Created automatically by XML generator (fxml_gen.erl)
%% Source: pubsub_serverinfo_codec.spec

-module(pubsub_serverinfo_codec).

-compile(export_all).

decode(El) -> decode(El, <<>>, []).

decode(El, Opts) -> decode(El, <<>>, Opts).

decode({xmlel, Name, Attrs, _} = El, TopXMLNS, Opts) ->
    XMLNS = get_attr(<<"xmlns">>, Attrs, TopXMLNS),
    case get_mod(Name, XMLNS) of
        undefined when XMLNS == <<>> ->
            erlang:error({pubsub_serverinfo_codec,
                          {missing_tag_xmlns, Name}});
        undefined ->
            erlang:error({pubsub_serverinfo_codec,
                          {unknown_tag, Name, XMLNS}});
        Mod -> Mod:do_decode(Name, XMLNS, El, Opts)
    end.

encode(El) -> encode(El, <<>>).

encode({xmlel, _, _, _} = El, _) -> El;
encode({xmlcdata, _} = CData, _) -> CData;
encode(El, TopXMLNS) ->
    Mod = get_mod(El),
    Mod:do_encode(El, TopXMLNS).


get_name(El) ->
    Mod = get_mod(El),
    Mod:do_get_name(El).

get_ns(El) ->
    Mod = get_mod(El),
    Mod:do_get_ns(El).

is_known_tag({xmlel, Name, Attrs, _}, TopXMLNS) ->
    XMLNS = get_attr(<<"xmlns">>, Attrs, TopXMLNS),
    get_mod(Name, XMLNS) /= undefined.

get_els(Term) ->
    Mod = get_mod(Term),
    Mod:get_els(Term).

set_els(Term, Els) ->
    Mod = get_mod(Term),
    Mod:set_els(Term, Els).

do_decode(<<"connection">>, <<"urn:xmpp:serverinfo:0">>,
          El, Opts) ->
    decode_pubsub_serverinfo_connection(<<"urn:xmpp:serverinfo:0">>,
                                        Opts,
                                        El);
do_decode(<<"remote-domain">>,
          <<"urn:xmpp:serverinfo:0">>, El, Opts) ->
    decode_pubsub_serverinfo_remote_domain(<<"urn:xmpp:serverinfo:0">>,
                                           Opts,
                                           El);
do_decode(<<"federation">>, <<"urn:xmpp:serverinfo:0">>,
          El, Opts) ->
    decode_pubsub_serverinfo_federation(<<"urn:xmpp:serverinfo:0">>,
                                        Opts,
                                        El);
do_decode(<<"domain">>, <<"urn:xmpp:serverinfo:0">>, El,
          Opts) ->
    decode_pubsub_serverinfo_domain(<<"urn:xmpp:serverinfo:0">>,
                                    Opts,
                                    El);
do_decode(<<"serverinfo">>, <<"urn:xmpp:serverinfo:0">>,
          El, Opts) ->
    decode_pubsub_serverinfo(<<"urn:xmpp:serverinfo:0">>,
                             Opts,
                             El);
do_decode(Name, <<>>, _, _) ->
    erlang:error({pubsub_serverinfo_codec,
                  {missing_tag_xmlns, Name}});
do_decode(Name, XMLNS, _, _) ->
    erlang:error({pubsub_serverinfo_codec,
                  {unknown_tag, Name, XMLNS}}).

tags() ->
    [{<<"connection">>, <<"urn:xmpp:serverinfo:0">>},
     {<<"remote-domain">>, <<"urn:xmpp:serverinfo:0">>},
     {<<"federation">>, <<"urn:xmpp:serverinfo:0">>},
     {<<"domain">>, <<"urn:xmpp:serverinfo:0">>},
     {<<"serverinfo">>, <<"urn:xmpp:serverinfo:0">>}].

do_encode({pubsub_serverinfo, _} = Serverinfo,
          TopXMLNS) ->
    encode_pubsub_serverinfo(Serverinfo, TopXMLNS);
do_encode({pubsub_serverinfo_domain, _, _} = Domain,
          TopXMLNS) ->
    encode_pubsub_serverinfo_domain(Domain, TopXMLNS);
do_encode({pubsub_serverinfo_remote_domain, _, _} =
              Remote_domain,
          TopXMLNS) ->
    encode_pubsub_serverinfo_remote_domain(Remote_domain,
                                           TopXMLNS).

do_get_name({pubsub_serverinfo, _}) -> <<"serverinfo">>;
do_get_name({pubsub_serverinfo_domain, _, _}) ->
    <<"domain">>;
do_get_name({pubsub_serverinfo_remote_domain, _, _}) ->
    <<"remote-domain">>.

do_get_ns({pubsub_serverinfo, _}) ->
    <<"urn:xmpp:serverinfo:0">>;
do_get_ns({pubsub_serverinfo_domain, _, _}) ->
    <<"urn:xmpp:serverinfo:0">>;
do_get_ns({pubsub_serverinfo_remote_domain, _, _}) ->
    <<"urn:xmpp:serverinfo:0">>.

register_module(Mod) ->
    register_module(Mod, pubsub_serverinfo_codec_external).

unregister_module(Mod) ->
    unregister_module(Mod,
                      pubsub_serverinfo_codec_external).

format_error({bad_attr_value, Attr, Tag, XMLNS}) ->
    <<"Bad value of attribute '", Attr/binary, "' in tag <",
      Tag/binary, "/> qualified by namespace '", XMLNS/binary,
      "'">>;
format_error({bad_cdata_value, <<>>, Tag, XMLNS}) ->
    <<"Bad value of cdata in tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_tag, Tag, XMLNS}) ->
    <<"Missing tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_attr, Attr, Tag, XMLNS}) ->
    <<"Missing attribute '", Attr/binary, "' in tag <",
      Tag/binary, "/> qualified by namespace '", XMLNS/binary,
      "'">>;
format_error({missing_cdata, <<>>, Tag, XMLNS}) ->
    <<"Missing cdata in tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({unknown_tag, Tag, XMLNS}) ->
    <<"Unknown tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_tag_xmlns, Tag}) ->
    <<"Missing namespace for tag <", Tag/binary, "/>">>.

io_format_error({bad_attr_value, Attr, Tag, XMLNS}) ->
    {<<"Bad value of attribute '~s' in tag <~s/> "
       "qualified by namespace '~s'">>,
     [Attr, Tag, XMLNS]};
io_format_error({bad_cdata_value, <<>>, Tag, XMLNS}) ->
    {<<"Bad value of cdata in tag <~s/> qualified "
       "by namespace '~s'">>,
     [Tag, XMLNS]};
io_format_error({missing_tag, Tag, XMLNS}) ->
    {<<"Missing tag <~s/> qualified by namespace "
       "'~s'">>,
     [Tag, XMLNS]};
io_format_error({missing_attr, Attr, Tag, XMLNS}) ->
    {<<"Missing attribute '~s' in tag <~s/> "
       "qualified by namespace '~s'">>,
     [Attr, Tag, XMLNS]};
io_format_error({missing_cdata, <<>>, Tag, XMLNS}) ->
    {<<"Missing cdata in tag <~s/> qualified "
       "by namespace '~s'">>,
     [Tag, XMLNS]};
io_format_error({unknown_tag, Tag, XMLNS}) ->
    {<<"Unknown tag <~s/> qualified by namespace "
       "'~s'">>,
     [Tag, XMLNS]};
io_format_error({missing_tag_xmlns, Tag}) ->
    {<<"Missing namespace for tag <~s/>">>, [Tag]}.

get_attr(Attr, Attrs, Default) ->
    case lists:keyfind(Attr, 1, Attrs) of
        {_, Val} -> Val;
        false -> Default
    end.

enc_xmlns_attrs(XMLNS, XMLNS) -> [];
enc_xmlns_attrs(XMLNS, _) -> [{<<"xmlns">>, XMLNS}].

choose_top_xmlns(<<>>, NSList, TopXMLNS) ->
    case lists:member(TopXMLNS, NSList) of
        true -> TopXMLNS;
        false -> hd(NSList)
    end;
choose_top_xmlns(XMLNS, _, _) -> XMLNS.

register_module(Mod, ResolverMod) ->
    MD5Sum = try Mod:module_info(md5) of
                 Val -> Val
             catch
                 error:badarg ->
                     {ok, {Mod, Val}} = beam_lib:md5(code:which(Mod)),
                     Val
             end,
    case orddict:find(Mod, ResolverMod:modules()) of
        {ok, MD5Sum} -> ok;
        _ ->
            Mods = orddict:store(Mod,
                                 MD5Sum,
                                 ResolverMod:modules()),
            recompile_resolver(Mods, ResolverMod)
    end.

unregister_module(Mod, ResolverMod) ->
    case orddict:find(Mod, ResolverMod:modules()) of
        {ok, _} ->
            Mods = orddict:erase(Mod, ResolverMod:modules()),
            recompile_resolver(Mods, ResolverMod);
        error -> ok
    end.

recompile_resolver(Mods, ResolverMod) ->
    Tags = lists:flatmap(fun (M) ->
                                 [{Name, XMLNS, M} || {Name, XMLNS} <- M:tags()]
                         end,
                         orddict:fetch_keys(Mods)),
    Records = lists:flatmap(fun (M) ->
                                    [{RecName, RecSize, M}
                                     || {RecName, RecSize} <- M:records()]
                            end,
                            orddict:fetch_keys(Mods)),
    Lookup1 = string:join(lists:map(fun ({RecName,
                                          RecSize,
                                          M}) ->
                                            io_lib:format("lookup({~s}) -> '~s'",
                                                          [string:join([io_lib:format("'~s'",
                                                                                      [RecName])
                                                                        | ["_"
                                                                           || _
                                                                                  <- lists:seq(1,
                                                                                               RecSize)]],
                                                                       ","),
                                                           M])
                                    end,
                                    Records)
                              ++
                              ["lookup(Term) -> erlang:error(badarg, "
                               "[Term])."],
                          ";" ++ io_lib:nl()),
    Lookup2 = string:join(lists:map(fun ({Name,
                                          XMLNS,
                                          M}) ->
                                            io_lib:format("lookup(~w, ~w) -> '~s'",
                                                          [Name, XMLNS, M])
                                    end,
                                    Tags)
                              ++ ["lookup(_, _) -> undefined."],
                          ";" ++ io_lib:nl()),
    Modules = io_lib:format("modules() -> [~s].",
                            [string:join([io_lib:format("{'~s', ~w}", [M, S])
                                          || {M, S} <- Mods],
                                         ",")]),
    Module = io_lib:format("-module(~s).", [ResolverMod]),
    Compile = "-compile(export_all).",
    Forms = lists:map(fun (Expr) ->
                              {ok, Tokens, _} =
                                  erl_scan:string(lists:flatten(Expr)),
                              {ok, Form} = erl_parse:parse_form(Tokens),
                              Form
                      end,
                      [Module, Compile, Modules, Lookup1, Lookup2]),
    {ok, Code} = case compile:forms(Forms, []) of
                     {ok, ResolverMod, Bin} -> {ok, Bin};
                     {ok, ResolverMod, Bin, _Warnings} -> {ok, Bin};
                     Error -> Error
                 end,
    {module, ResolverMod} = code:load_binary(ResolverMod,
                                             "nofile",
                                             Code),
    ok.

dec_enum(Val, Enums) ->
    AtomVal = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(AtomVal, Enums) of
        true -> AtomVal
    end.

enc_enum(Atom) -> erlang:atom_to_binary(Atom, utf8).

pp(pubsub_serverinfo, 1) -> [domain];
pp(pubsub_serverinfo_domain, 2) ->
    [name, remote_domain];
pp(pubsub_serverinfo_remote_domain, 2) -> [name, type];
pp(xmlel, 3) -> [name, attrs, children];
pp(Name, Arity) ->
    try get_mod(erlang:make_tuple(Arity + 1,
                                  undefined,
                                  [{1, Name}]))
    of
        Mod -> Mod:pp(Name, Arity)
    catch
        error:badarg -> no
    end.

records() ->
    [{pubsub_serverinfo, 1},
     {pubsub_serverinfo_domain, 2},
     {pubsub_serverinfo_remote_domain, 2}].

get_mod(<<"serverinfo">>,
        <<"urn:xmpp:serverinfo:0">>) ->
    pubsub_serverinfo_codec;
get_mod(<<"remote-domain">>,
        <<"urn:xmpp:serverinfo:0">>) ->
    pubsub_serverinfo_codec;
get_mod(<<"federation">>,
        <<"urn:xmpp:serverinfo:0">>) ->
    pubsub_serverinfo_codec;
get_mod(<<"domain">>, <<"urn:xmpp:serverinfo:0">>) ->
    pubsub_serverinfo_codec;
get_mod(<<"connection">>,
        <<"urn:xmpp:serverinfo:0">>) ->
    pubsub_serverinfo_codec;
get_mod(Name, XMLNS) ->
    pubsub_serverinfo_codec_external:lookup(Name, XMLNS).

get_mod({pubsub_serverinfo, _}) ->
    pubsub_serverinfo_codec;
get_mod({pubsub_serverinfo_domain, _, _}) ->
    pubsub_serverinfo_codec;
get_mod({pubsub_serverinfo_remote_domain, _, _}) ->
    pubsub_serverinfo_codec;
get_mod(Record) ->
    pubsub_serverinfo_codec_external:lookup(Record).

decode_pubsub_serverinfo_connection(__TopXMLNS, __Opts,
                                    {xmlel, <<"connection">>, _attrs, _els}) ->
    Type =
        decode_pubsub_serverinfo_connection_attrs(__TopXMLNS,
                                                  _attrs,
                                                  undefined),
    Type.

decode_pubsub_serverinfo_connection_attrs(__TopXMLNS,
                                          [{<<"type">>, _val} | _attrs],
                                          _Type) ->
    decode_pubsub_serverinfo_connection_attrs(__TopXMLNS,
                                              _attrs,
                                              _val);
decode_pubsub_serverinfo_connection_attrs(__TopXMLNS,
                                          [_ | _attrs], Type) ->
    decode_pubsub_serverinfo_connection_attrs(__TopXMLNS,
                                              _attrs,
                                              Type);
decode_pubsub_serverinfo_connection_attrs(__TopXMLNS,
                                          [], Type) ->
    decode_pubsub_serverinfo_connection_attr_type(__TopXMLNS,
                                                  Type).

encode_pubsub_serverinfo_connection(Type, __TopXMLNS) ->
    __NewTopXMLNS =
        pubsub_serverinfo_codec:choose_top_xmlns(<<"urn:xmpp:serverinfo:0">>,
                                                 [],
                                                 __TopXMLNS),
    _els = [],
    _attrs =
        encode_pubsub_serverinfo_connection_attr_type(Type,
                                                      pubsub_serverinfo_codec:enc_xmlns_attrs(__NewTopXMLNS,
                                                                                              __TopXMLNS)),
    {xmlel, <<"connection">>, _attrs, _els}.

decode_pubsub_serverinfo_connection_attr_type(__TopXMLNS,
                                              undefined) ->
    erlang:error({pubsub_serverinfo_codec,
                  {missing_attr,
                   <<"type">>,
                   <<"connection">>,
                   __TopXMLNS}});
decode_pubsub_serverinfo_connection_attr_type(__TopXMLNS,
                                              _val) ->
    case catch dec_enum(_val, [incoming, outgoing, bidi]) of
        {'EXIT', _} ->
            erlang:error({pubsub_serverinfo_codec,
                          {bad_attr_value,
                           <<"type">>,
                           <<"connection">>,
                           __TopXMLNS}});
        _res -> _res
    end.

encode_pubsub_serverinfo_connection_attr_type(_val,
                                              _acc) ->
    [{<<"type">>, enc_enum(_val)} | _acc].

decode_pubsub_serverinfo_remote_domain(__TopXMLNS,
                                       __Opts,
                                       {xmlel,
                                        <<"remote-domain">>,
                                        _attrs,
                                        _els}) ->
    Type =
        decode_pubsub_serverinfo_remote_domain_els(__TopXMLNS,
                                                   __Opts,
                                                   _els,
                                                   []),
    Name =
        decode_pubsub_serverinfo_remote_domain_attrs(__TopXMLNS,
                                                     _attrs,
                                                     undefined),
    {pubsub_serverinfo_remote_domain, Name, Type}.

decode_pubsub_serverinfo_remote_domain_els(__TopXMLNS,
                                           __Opts, [], Type) ->
    lists:reverse(Type);
decode_pubsub_serverinfo_remote_domain_els(__TopXMLNS,
                                           __Opts,
                                           [{xmlel,
                                             <<"connection">>,
                                             _attrs,
                                             _} =
                                                _el
                                            | _els],
                                           Type) ->
    case pubsub_serverinfo_codec:get_attr(<<"xmlns">>,
                                          _attrs,
                                          __TopXMLNS)
        of
        <<"urn:xmpp:serverinfo:0">> ->
            decode_pubsub_serverinfo_remote_domain_els(__TopXMLNS,
                                                       __Opts,
                                                       _els,
                                                       [decode_pubsub_serverinfo_connection(<<"urn:xmpp:serverinfo:0">>,
                                                                                            __Opts,
                                                                                            _el)
                                                        | Type]);
        _ ->
            decode_pubsub_serverinfo_remote_domain_els(__TopXMLNS,
                                                       __Opts,
                                                       _els,
                                                       Type)
    end;
decode_pubsub_serverinfo_remote_domain_els(__TopXMLNS,
                                           __Opts, [_ | _els], Type) ->
    decode_pubsub_serverinfo_remote_domain_els(__TopXMLNS,
                                               __Opts,
                                               _els,
                                               Type).

decode_pubsub_serverinfo_remote_domain_attrs(__TopXMLNS,
                                             [{<<"name">>, _val} | _attrs],
                                             _Name) ->
    decode_pubsub_serverinfo_remote_domain_attrs(__TopXMLNS,
                                                 _attrs,
                                                 _val);
decode_pubsub_serverinfo_remote_domain_attrs(__TopXMLNS,
                                             [_ | _attrs], Name) ->
    decode_pubsub_serverinfo_remote_domain_attrs(__TopXMLNS,
                                                 _attrs,
                                                 Name);
decode_pubsub_serverinfo_remote_domain_attrs(__TopXMLNS,
                                             [], Name) ->
    decode_pubsub_serverinfo_remote_domain_attr_name(__TopXMLNS,
                                                     Name).

encode_pubsub_serverinfo_remote_domain({pubsub_serverinfo_remote_domain,
                                        Name,
                                        Type},
                                       __TopXMLNS) ->
    __NewTopXMLNS =
        pubsub_serverinfo_codec:choose_top_xmlns(<<"urn:xmpp:serverinfo:0">>,
                                                 [],
                                                 __TopXMLNS),
    _els =
        lists:reverse('encode_pubsub_serverinfo_remote_domain_$type'(Type,
                                                                     __NewTopXMLNS,
                                                                     [])),
    _attrs =
        encode_pubsub_serverinfo_remote_domain_attr_name(Name,
                                                         pubsub_serverinfo_codec:enc_xmlns_attrs(__NewTopXMLNS,
                                                                                                 __TopXMLNS)),
    {xmlel, <<"remote-domain">>, _attrs, _els}.

'encode_pubsub_serverinfo_remote_domain_$type'([],
                                               __TopXMLNS, _acc) ->
    _acc;
'encode_pubsub_serverinfo_remote_domain_$type'([Type
                                                | _els],
                                               __TopXMLNS, _acc) ->
    'encode_pubsub_serverinfo_remote_domain_$type'(_els,
                                                   __TopXMLNS,
                                                   [encode_pubsub_serverinfo_connection(Type,
                                                                                        __TopXMLNS)
                                                    | _acc]).

decode_pubsub_serverinfo_remote_domain_attr_name(__TopXMLNS,
                                                 undefined) ->
    erlang:error({pubsub_serverinfo_codec,
                  {missing_attr,
                   <<"name">>,
                   <<"remote-domain">>,
                   __TopXMLNS}});
decode_pubsub_serverinfo_remote_domain_attr_name(__TopXMLNS,
                                                 _val) ->
    _val.

encode_pubsub_serverinfo_remote_domain_attr_name(_val,
                                                 _acc) ->
    [{<<"name">>, _val} | _acc].

decode_pubsub_serverinfo_federation(__TopXMLNS, __Opts,
                                    {xmlel, <<"federation">>, _attrs, _els}) ->
    Remote_domain =
        decode_pubsub_serverinfo_federation_els(__TopXMLNS,
                                                __Opts,
                                                _els,
                                                []),
    Remote_domain.

decode_pubsub_serverinfo_federation_els(__TopXMLNS,
                                        __Opts, [], Remote_domain) ->
    lists:reverse(Remote_domain);
decode_pubsub_serverinfo_federation_els(__TopXMLNS,
                                        __Opts,
                                        [{xmlel,
                                          <<"remote-domain">>,
                                          _attrs,
                                          _} =
                                             _el
                                         | _els],
                                        Remote_domain) ->
    case pubsub_serverinfo_codec:get_attr(<<"xmlns">>,
                                          _attrs,
                                          __TopXMLNS)
        of
        <<"urn:xmpp:serverinfo:0">> ->
            decode_pubsub_serverinfo_federation_els(__TopXMLNS,
                                                    __Opts,
                                                    _els,
                                                    [decode_pubsub_serverinfo_remote_domain(<<"urn:xmpp:serverinfo:0">>,
                                                                                            __Opts,
                                                                                            _el)
                                                     | Remote_domain]);
        _ ->
            decode_pubsub_serverinfo_federation_els(__TopXMLNS,
                                                    __Opts,
                                                    _els,
                                                    Remote_domain)
    end;
decode_pubsub_serverinfo_federation_els(__TopXMLNS,
                                        __Opts, [_ | _els], Remote_domain) ->
    decode_pubsub_serverinfo_federation_els(__TopXMLNS,
                                            __Opts,
                                            _els,
                                            Remote_domain).

encode_pubsub_serverinfo_federation(Remote_domain,
                                    __TopXMLNS) ->
    __NewTopXMLNS =
        pubsub_serverinfo_codec:choose_top_xmlns(<<"urn:xmpp:serverinfo:0">>,
                                                 [],
                                                 __TopXMLNS),
    _els =
        lists:reverse('encode_pubsub_serverinfo_federation_$remote_domain'(Remote_domain,
                                                                           __NewTopXMLNS,
                                                                           [])),
    _attrs =
        pubsub_serverinfo_codec:enc_xmlns_attrs(__NewTopXMLNS,
                                                __TopXMLNS),
    {xmlel, <<"federation">>, _attrs, _els}.

'encode_pubsub_serverinfo_federation_$remote_domain'([],
                                                     __TopXMLNS, _acc) ->
    _acc;
'encode_pubsub_serverinfo_federation_$remote_domain'([Remote_domain
                                                      | _els],
                                                     __TopXMLNS, _acc) ->
    'encode_pubsub_serverinfo_federation_$remote_domain'(_els,
                                                         __TopXMLNS,
                                                         [encode_pubsub_serverinfo_remote_domain(Remote_domain,
                                                                                                 __TopXMLNS)
                                                          | _acc]).

decode_pubsub_serverinfo_domain(__TopXMLNS, __Opts,
                                {xmlel, <<"domain">>, _attrs, _els}) ->
    Remote_domain =
        decode_pubsub_serverinfo_domain_els(__TopXMLNS,
                                            __Opts,
                                            _els,
                                            undefined),
    Name = decode_pubsub_serverinfo_domain_attrs(__TopXMLNS,
                                                 _attrs,
                                                 undefined),
    {pubsub_serverinfo_domain, Name, Remote_domain}.

decode_pubsub_serverinfo_domain_els(__TopXMLNS, __Opts,
                                    [], Remote_domain) ->
    Remote_domain;
decode_pubsub_serverinfo_domain_els(__TopXMLNS, __Opts,
                                    [{xmlel, <<"federation">>, _attrs, _} = _el
                                     | _els],
                                    Remote_domain) ->
    case pubsub_serverinfo_codec:get_attr(<<"xmlns">>,
                                          _attrs,
                                          __TopXMLNS)
        of
        <<"urn:xmpp:serverinfo:0">> ->
            decode_pubsub_serverinfo_domain_els(__TopXMLNS,
                                                __Opts,
                                                _els,
                                                decode_pubsub_serverinfo_federation(<<"urn:xmpp:serverinfo:0">>,
                                                                                    __Opts,
                                                                                    _el));
        _ ->
            decode_pubsub_serverinfo_domain_els(__TopXMLNS,
                                                __Opts,
                                                _els,
                                                Remote_domain)
    end;
decode_pubsub_serverinfo_domain_els(__TopXMLNS, __Opts,
                                    [_ | _els], Remote_domain) ->
    decode_pubsub_serverinfo_domain_els(__TopXMLNS,
                                        __Opts,
                                        _els,
                                        Remote_domain).

decode_pubsub_serverinfo_domain_attrs(__TopXMLNS,
                                      [{<<"name">>, _val} | _attrs], _Name) ->
    decode_pubsub_serverinfo_domain_attrs(__TopXMLNS,
                                          _attrs,
                                          _val);
decode_pubsub_serverinfo_domain_attrs(__TopXMLNS,
                                      [_ | _attrs], Name) ->
    decode_pubsub_serverinfo_domain_attrs(__TopXMLNS,
                                          _attrs,
                                          Name);
decode_pubsub_serverinfo_domain_attrs(__TopXMLNS, [],
                                      Name) ->
    decode_pubsub_serverinfo_domain_attr_name(__TopXMLNS,
                                              Name).

encode_pubsub_serverinfo_domain({pubsub_serverinfo_domain,
                                 Name,
                                 Remote_domain},
                                __TopXMLNS) ->
    __NewTopXMLNS =
        pubsub_serverinfo_codec:choose_top_xmlns(<<"urn:xmpp:serverinfo:0">>,
                                                 [],
                                                 __TopXMLNS),
    _els =
        lists:reverse('encode_pubsub_serverinfo_domain_$remote_domain'(Remote_domain,
                                                                       __NewTopXMLNS,
                                                                       [])),
    _attrs = encode_pubsub_serverinfo_domain_attr_name(Name,
                                                       pubsub_serverinfo_codec:enc_xmlns_attrs(__NewTopXMLNS,
                                                                                               __TopXMLNS)),
    {xmlel, <<"domain">>, _attrs, _els}.

'encode_pubsub_serverinfo_domain_$remote_domain'(undefined,
                                                 __TopXMLNS, _acc) ->
    _acc;
'encode_pubsub_serverinfo_domain_$remote_domain'(Remote_domain,
                                                 __TopXMLNS, _acc) ->
    [encode_pubsub_serverinfo_federation(Remote_domain,
                                         __TopXMLNS)
     | _acc].

decode_pubsub_serverinfo_domain_attr_name(__TopXMLNS,
                                          undefined) ->
    erlang:error({pubsub_serverinfo_codec,
                  {missing_attr, <<"name">>, <<"domain">>, __TopXMLNS}});
decode_pubsub_serverinfo_domain_attr_name(__TopXMLNS,
                                          _val) ->
    _val.

encode_pubsub_serverinfo_domain_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_pubsub_serverinfo(__TopXMLNS, __Opts,
                         {xmlel, <<"serverinfo">>, _attrs, _els}) ->
    Domain = decode_pubsub_serverinfo_els(__TopXMLNS,
                                          __Opts,
                                          _els,
                                          []),
    {pubsub_serverinfo, Domain}.

decode_pubsub_serverinfo_els(__TopXMLNS, __Opts, [],
                             Domain) ->
    lists:reverse(Domain);
decode_pubsub_serverinfo_els(__TopXMLNS, __Opts,
                             [{xmlel, <<"domain">>, _attrs, _} = _el | _els],
                             Domain) ->
    case pubsub_serverinfo_codec:get_attr(<<"xmlns">>,
                                          _attrs,
                                          __TopXMLNS)
        of
        <<"urn:xmpp:serverinfo:0">> ->
            decode_pubsub_serverinfo_els(__TopXMLNS,
                                         __Opts,
                                         _els,
                                         [decode_pubsub_serverinfo_domain(<<"urn:xmpp:serverinfo:0">>,
                                                                          __Opts,
                                                                          _el)
                                          | Domain]);
        _ ->
            decode_pubsub_serverinfo_els(__TopXMLNS,
                                         __Opts,
                                         _els,
                                         Domain)
    end;
decode_pubsub_serverinfo_els(__TopXMLNS, __Opts,
                             [_ | _els], Domain) ->
    decode_pubsub_serverinfo_els(__TopXMLNS,
                                 __Opts,
                                 _els,
                                 Domain).

encode_pubsub_serverinfo({pubsub_serverinfo, Domain},
                         __TopXMLNS) ->
    __NewTopXMLNS =
        pubsub_serverinfo_codec:choose_top_xmlns(<<"urn:xmpp:serverinfo:0">>,
                                                 [],
                                                 __TopXMLNS),
    _els =
        lists:reverse('encode_pubsub_serverinfo_$domain'(Domain,
                                                         __NewTopXMLNS,
                                                         [])),
    _attrs =
        pubsub_serverinfo_codec:enc_xmlns_attrs(__NewTopXMLNS,
                                                __TopXMLNS),
    {xmlel, <<"serverinfo">>, _attrs, _els}.

'encode_pubsub_serverinfo_$domain'([], __TopXMLNS,
                                   _acc) ->
    _acc;
'encode_pubsub_serverinfo_$domain'([Domain | _els],
                                   __TopXMLNS, _acc) ->
    'encode_pubsub_serverinfo_$domain'(_els,
                                       __TopXMLNS,
                                       [encode_pubsub_serverinfo_domain(Domain,
                                                                        __TopXMLNS)
                                        | _acc]).
