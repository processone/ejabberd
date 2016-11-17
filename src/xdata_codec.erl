%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(xdata_codec).

%% API
-export([compile/1, compile/2]).
-export([dec_int/1, dec_int/3, dec_enum/2, dec_bool/1, not_empty/1,
	 dec_enum_int/2, dec_enum_int/4, enc_int/1, enc_enum/1,
	 enc_bool/1, enc_enum_int/1, format_error/1, enc_jid/1, dec_jid/1]).
-include("xmpp.hrl").

-record(state, {mod_name :: atom(),
		file_name :: string(),
		erl = "" :: string(),
		hrl = "" :: string(),
		dir = "" :: string(),
		ns = <<>> :: binary(),
		doc = <<>> :: binary(),
		erl_dir = "" :: string(),
		hrl_dir = "" :: string(),
		prefix = [] :: [binary()],
		dec_mfas = [] :: [{binary(), mfa()}],
		enc_mfas = [] :: [{binary(), mfa()}],
		specs = [] :: [{binary(), string()}],
		required = [] :: [{binary(), boolean()} | binary()],
		defaults = [] :: [{binary(), any()}]}).

-define(is_multi_type(T),
	((T == 'list-multi') or (T == 'jid-multi') or (T == 'text-multi'))).

-define(is_list_type(T),
	((T == 'list-single') or (T == 'list-multi'))).

%%%===================================================================
%%% API
%%%===================================================================
compile(Path) ->
    compile(Path, []).

compile(Path, Opts) ->
    case filelib:is_dir(Path) of
	true ->
	    filelib:fold_files(
	      Path, ".*.xdata", false,
	      fun(File, ok) ->
		      compile_file(File, Opts);
		 (_, Err) ->
		      Err
	      end, ok);
	false ->
	    compile_file(Path, Opts)
    end.

compile_file(Path, Opts) ->
    try
	ok = application:ensure_started(fast_xml),
	DirName = filename:dirname(Path),
	FileName = filename:basename(Path),
	RootName = filename:rootname(FileName),
	ConfigPath = filename:join(DirName, RootName) ++ ".cfg",
	ModName = list_to_atom(RootName),
	{ok, Data} = file:read_file(Path),
	Config = case file:consult(ConfigPath) of
		     {ok, Terms} -> lists:flatten(Terms);
		     {error, enoent} -> []
		 end,
	State = #state{mod_name = ModName,
		       file_name = FileName,
		       erl = filename:rootname(FileName) ++ ".erl",
		       hrl = filename:rootname(FileName) ++ ".hrl",
		       dir = DirName,
		       prefix = proplists:get_all_values(prefix, Config),
		       erl_dir = proplists:get_value(erl_dir, Opts, DirName),
		       hrl_dir = proplists:get_value(hrl_dir, Opts, DirName),
		       dec_mfas = proplists:get_value(decode, Config, []),
		       enc_mfas = proplists:get_value(encode, Config, []),
		       specs = proplists:get_value(specs, Config, []),
		       required = proplists:get_value(required, Config, []),
		       defaults = proplists:get_value(defaults, Config, [])},
	#xmlel{} = El = fxml_stream:parse_element(Data),
	ok = compile_element(normalize(El), State),
	io:format("Compiled ~s~n", [Path])
    catch _:{badmatch, Err} ->
	    io:format(standard_error, "Failed to compile ~s: ~p~n",
		      [Path, Err]),
	    Err
    end.

emit(Format) ->
    emit(Format, []).

emit(Format, Args) ->
    put(outbuf, get(outbuf) ++ io_lib:format(Format, Args)).

dec_int(Val) ->
    dec_int(Val, infinity, infinity).

dec_int(Val, Min, Max) ->
    case list_to_integer(binary_to_list(Val)) of
        Int when Int =< Max, Min == infinity ->
            Int;
        Int when Int =< Max, Int >= Min ->
            Int
    end.

enc_int(Int) ->
    integer_to_binary(Int).

dec_enum(Val, Enums) ->
    AtomVal = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(AtomVal, Enums) of
        true ->
            AtomVal
    end.

enc_enum(Atom) ->
    erlang:atom_to_binary(Atom, utf8).

dec_enum_int(Val, Enums) ->
    try dec_int(Val)
    catch _:_ -> dec_enum(Val, Enums)
    end.

dec_enum_int(Val, Enums, Min, Max) ->
    try dec_int(Val, Min, Max)
    catch _:_ -> dec_enum(Val, Enums)
    end.

enc_enum_int(Int) when is_integer(Int) ->
    enc_int(Int);
enc_enum_int(Atom) ->
    enc_enum(Atom).

dec_bool(<<"1">>) -> true;
dec_bool(<<"0">>) -> false;
dec_bool(<<"true">>) -> true;
dec_bool(<<"false">>) -> false.

enc_bool(true) -> <<"1">>;
enc_bool(false) -> <<"0">>.

enc_jid(J) -> jid:to_string(J).

dec_jid(Val) ->
    case jid:from_string(Val) of
	error -> erlang:error(badarg);
	J -> J
    end.

not_empty(<<_, _/binary>> = Val) ->
    Val.

format_error({form_type_mismatch, Type}) ->
    <<"FORM_TYPE doesn't match '", Type/binary, "'">>;
format_error({bad_var_value, Var, Type}) ->
    <<"Bad value of field '", Var/binary, "' of type '", Type/binary, "'">>;
format_error({missing_value, Var, Type}) ->
    <<"Missing value of field '", Var/binary, "' of type '", Type/binary, "'">>;
format_error({too_many_values, Var, Type}) ->
    <<"Too many values for field '", Var/binary, "' of type '", Type/binary, "'">>;
format_error({unknown_var, Var, Type}) ->
    <<"Unknown field '", Var/binary, "' of type '", Type/binary, "'">>;
format_error({missing_required_var, Var, Type}) ->
    <<"Missing required field '", Var/binary, "' of type '", Type/binary, "'">>.

%%%===================================================================
%%% Internal functions
%%%===================================================================
compile_element(#xmlel{name = <<"form_type">>, children = Els} = Form,
		#state{erl = OutErl, erl_dir = ErlDir,
		       hrl = OutHrl, hrl_dir = HrlDir} = State0) ->
    try
	Name = fxml:get_subtag_cdata(Form, <<"name">>),
	Doc = fxml:get_subtag_cdata(Form, <<"doc">>),
	X = #xmlel{name = <<"x">>,
		   attrs = [{<<"type">>, <<"form">>},
			    {<<"xmlns">>, <<"jabber:x:data">>}],
		   children = Els},
	State = State0#state{ns = Name, doc = Doc},
	#xdata{fields = Fs} = xmpp_codec:decode(X),
	put(outbuf, []),
	mk_header(State),
	mk_aux_funs(),
	mk_top_decoder(Fs, State),
	mk_top_encoder(Fs, State),
	mk_decoder(Fs, State),
	mk_encoders(Fs, State),
	ErlData = get(outbuf),
	ok = file:write_file(filename:join(ErlDir, OutErl), ErlData),
	ok = erl_tidy:file(filename:join(ErlDir, OutErl), [{backups, false}]),
	put(outbuf, []),
	mk_type_definitions(Fs, State),
	HrlData = get(outbuf),
	ok = file:write_file(filename:join(HrlDir, OutHrl), HrlData)
    catch _:{badmatch, Err} ->
	    Err
    end.

mk_aux_funs() ->
    case get_abstract_code_from_myself() of
        {ok, AbsCode} ->
            AST = lists:filter(
		    fun(T) ->
			    case catch erl_syntax_lib:analyze_function(T) of
				{format_error, 1} -> true;
				{dec_int, 3} -> true;
				{dec_int, 1} -> true;
				{dec_enum, 2} -> true;
				{dec_enum_int, 2} -> true;
				{dec_enum_int, 4} -> true;
				{enc_int, 1} -> true;
				{enc_enum, 1} -> true;
				{enc_enum_int, 1} -> true;
				{not_empty, 1} -> true;
				{dec_bool, 1} -> true;
				{enc_bool, 1} -> true;
				{enc_jid, 1} -> true;
				{dec_jid, 1} -> true;
				_ -> false
			    end
		    end, AbsCode),
	    emit(erl_prettypr:format(erl_syntax:form_list(AST)) ++ io_lib:nl());
        error ->
            erlang:error({no_abstract_code_found, ?MODULE})
    end.

get_abstract_code_from_myself() ->
    {file, File} = code:is_loaded(?MODULE),
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {_, List}} ->
            case lists:keyfind(abstract_code, 1, List) of
                {abstract_code, {raw_abstract_v1, Abstr}} ->
                    {ok, Abstr};
                _ ->
                    error
            end;
        _ ->
            error
    end.

mk_comment_header(#state{file_name = Source, ns = NS, doc = Doc}) ->
    emit("%% Created automatically by xdata generator (xdata_codec.erl)~n"
	 "%% Source: ~s~n"
	 "%% Form type: ~s~n", [Source, NS]),
    if Doc /= <<>> -> emit("%% Document: ~s~n~n", [Doc]);
       true -> emit("~n")
    end.

mk_header(#state{mod_name = Mod, hrl = Include} = State) ->
    mk_comment_header(State),
    emit("~n-module(~s).~n", [Mod]),
    emit("-export([decode/1, decode/2, encode/1, encode/2, format_error/1]).~n"),
    emit("-include(\"xmpp_codec.hrl\").~n"),
    emit("-include(\"~s\").~n", [Include]),
    emit("-export_type([property/0, result/0, form/0]).~n").

mk_type_definitions(Fs, State) ->
    mk_comment_header(State),
    lists:foreach(
      fun(#xdata_field{var = Var} = F) ->
	      Spec = get_typespec(F, State),
	      case is_complex_type(Spec) of
		  true ->
		      emit("-type '~s'() :: ~s.~n",
			   [var_to_rec_field(Var, State), Spec]);
		  false ->
		      ok
	      end
      end, Fs),
    emit("~n-type property() :: "),
    Fields = lists:map(
	       fun(#xdata_field{var = Var} = F) ->
		       RecField = var_to_rec_field(Var, State),
		       [io_lib:format("{'~s', ~s}",
				      [RecField, mk_typespec(F, State)])]
	       end, Fs),
    emit(string:join(Fields, " |~n                    ") ++ ".~n"),
    emit("-type result() :: [property()].~n~n"),
    VarsWithSpec = lists:flatmap(
		     fun(#xdata_field{type = T, var = Var} = F)
			   when ?is_list_type(T) ->
			     RecName = var_to_rec_field(Var, State),
			     Spec0 = get_typespec(F, State),
			     Spec = case is_complex_type(Spec0) of
					true ->
					    io_lib:format("'~s'()", [RecName]);
					false ->
					    Spec0
				    end,
			     [{RecName, mk_typespec(F, State), Spec}];
			(_) ->
			     []
		     end, Fs),
    case VarsWithSpec of
	[] ->
	    emit("-type form() :: [property() | xdata_field()].~n");
	_ ->
	    emit("-type options(T) :: [{binary(), T}].~n"),
	    emit("-type property_with_options() ::~n      "),
	    Options = [io_lib:format("{'~s', ~s, options(~s)}",
				     [Var, Spec1, Spec2])
		       || {Var, Spec1, Spec2} <- VarsWithSpec],
	    emit(string:join(Options, " |~n      ") ++ ".~n"),
	    emit("-type form() :: [property() | property_with_options() | xdata_field()].~n")
    end.

mk_top_decoder(Fs, State) ->
    Required = [Var || #xdata_field{var = Var} <- Fs, is_required(Var, State)],
    emit("decode(Fs) -> decode(Fs, []).~n"),
    emit("decode(Fs, Acc) ->"
	 "  case lists:keyfind(<<\"FORM_TYPE\">>, #xdata_field.var, Fs) of"
	 "    false ->"
	 "      decode(Fs, Acc, ~p);"
	 "    #xdata_field{values = [~p]} ->"
	 "      decode(Fs, Acc, ~p);"
	 "    _ ->"
	 "      erlang:error({?MODULE, {form_type_mismatch, ~p}})~n"
	 "  end.~n",
	 [Required, State#state.ns, Required, State#state.ns]).

mk_top_encoder(Fs, State) ->
    Clauses = string:join(
		lists:map(
		  fun(#xdata_field{var = Var, type = T}) when ?is_list_type(T) ->
			  Field = var_to_rec_field(Var, State),
			  io_lib:format(
			    "{'~s', Val} -> ['encode_~s'(Val, default, Translate)];"
			    "{'~s', Val, Opts} -> ['encode_~s'(Val, Opts, Translate)]",
			    [Field, Field, Field, Field]);
		     (#xdata_field{var = Var}) ->
			  Field = var_to_rec_field(Var, State),
			  io_lib:format(
			    "{'~s', Val} -> ['encode_~s'(Val, Translate)];"
			    "{'~s', _, _} -> erlang:error({badarg, Opt})",
			    [Field, Field, Field])
		  end, Fs) ++ ["#xdata_field{} -> [Opt]; _ -> []"],
		";"),
    emit("encode(Cfg) -> encode(Cfg, fun(Text) -> Text end).~n"),
    emit("encode(List, Translate) when is_list(List) ->"
	 "  Fs = [case Opt of ~s end || Opt <- List],"
	 "  FormType = #xdata_field{var = <<\"FORM_TYPE\">>, type = hidden,"
	 "                          values = [~p]},"
	 "  [FormType|lists:flatten(Fs)].~n",
	 [Clauses, State#state.ns]).

mk_decoder([#xdata_field{var = Var, type = Type} = F|Fs], State) ->
    ValVar = if ?is_multi_type(Type) -> "Values";
		true -> "[Value]"
	     end,
    DecFun = if ?is_multi_type(Type) ->
		     ["[", mk_decoding_fun(F, State), " || Value <- Values]"];
		true ->
		     mk_decoding_fun(F, State)
	     end,
    emit("decode([#xdata_field{var = ~p, values = ~s}|Fs], Acc, Required) ->"
	 "  try ~s of"
	 "    Result -> decode(Fs, [{'~s', Result}|Acc],"
	 "                     lists:delete(~p, Required))"
	 "  catch _:_ ->"
	 "    erlang:error({?MODULE, {bad_var_value, ~p, ~p}})"
	 "  end;",
	 [Var, ValVar, DecFun, var_to_rec_field(Var, State),
	  Var, Var, State#state.ns]),
    if not ?is_multi_type(Type) ->
	    emit("decode([#xdata_field{var = ~p, values = []} = F|Fs],"
		 "       Acc, Required) ->"
		 "  decode([F#xdata_field{var = ~p, values = [<<>>]}|Fs],"
		 "         Acc, Required);",
		 [Var, Var]),
	    emit("decode([#xdata_field{var = ~p}|_], _, _) ->"
		 "  erlang:error({?MODULE, {too_many_values, ~p, ~p}});",
		 [Var, Var, State#state.ns]);
       true ->
	    ok
    end,
    mk_decoder(Fs, State);
mk_decoder([], State) ->
    emit("decode([#xdata_field{var = Var}|Fs], Acc, Required) ->"
	 "  if Var /= <<\"FORM_TYPE\">> ->"
	 "    erlang:error({?MODULE, {unknown_var, Var, ~p}});"
	 "  true ->"
	 "    decode(Fs, Acc, Required)"
	 "  end;",
	 [State#state.ns]),
    emit("decode([], _, [Var|_]) ->"
	 "  erlang:error({?MODULE, {missing_required_var, Var, ~p}});~n",
	 [State#state.ns]),
    emit("decode([], Acc, []) -> Acc.~n").

mk_encoders(Fs, State) ->
    lists:foreach(
      fun(#xdata_field{var = Var, required = IsRequired, desc = Desc,
		       label = Label, type = Type} = F) ->
	      EncVals = mk_encoded_values(F, State),
	      EncOpts = mk_encoded_options(F, State),
	      FieldName = var_to_rec_field(Var, State),
	      DescStr = if Desc == <<>> -> "<<>>";
			   true -> io_lib:format("Translate(~p)", [Desc])
			end,
	      LabelStr = if Label == <<>> -> "<<>>";
			    true -> io_lib:format("Translate(~p)", [Label])
			 end,
	      if ?is_list_type(Type) ->
		      emit("'encode_~s'(Value, Options, Translate) ->", [FieldName]);
		 true ->
		      emit("'encode_~s'(Value, Translate) ->", [FieldName])
	      end,
	      emit("  Values = ~s,"
		   "  Opts = ~s,"
		   "  #xdata_field{var = ~p,"
		   "               values = Values,"
		   "               required = ~p,"
		   "               type = ~p,"
		   "               options = Opts,"
		   "               desc = ~s,"
		   "               label = ~s}.~n",
		   [EncVals, EncOpts, Var, IsRequired, Type, DescStr, LabelStr])
      end, Fs).

mk_encoded_values(#xdata_field{var = Var, type = Type,
			       options = Options}, State) ->
    EncFun =
	case get_enc_fun(Var, Type, Options, State) of
	    {M, Fun, Args} ->
		Mod = if M == undefined -> "";
			 true -> io_lib:format("~s:", [M])
		      end,
		FArgs = [io_lib:format(", ~p", [A]) || A <- Args],
		if ?is_multi_type(Type) ->
			"[" ++ io_lib:format("~s~s(V~s)", [Mod, Fun, FArgs]) ++
			    " || V <- Value]";
		   true ->
			"[" ++ io_lib:format("~s~s(Value~s)", [Mod, Fun, FArgs])
			    ++ "]"
		end;
	    undefined ->
		"[Value]"
	end,
    Default = case get_dec_fun(Var, Type, Options, State) of
		  _ when ?is_multi_type(Type) -> "[]";
		  undefined -> "<<>>";
		  _MFA -> "undefined"
	      end,
    io_lib:format(
      "case Value of"
      "  ~s -> [];~n"
      "  Value -> ~s~n"
      "end",
      [Default, EncFun]).

mk_encoded_options(#xdata_field{var = Var, type = Type,
				options = Options}, State) ->
    EncFun = case get_enc_fun(Var, Type, Options, State) of
		 {M, Fun, Args} ->
		     Mod = if M == undefined -> "";
			      true -> io_lib:format("~s:", [M])
			   end,
		     FArgs = [io_lib:format(", ~p", [A]) || A <- Args],
		     io_lib:format("~s~s(V~s)", [Mod, Fun, FArgs]);
		 undefined ->
		     "V"
	     end,
    EncOpts = string:join(
		[case L of
		     <<>> ->
			 io_lib:format("#xdata_option{value = ~p}", [V]);
		     _ ->
			 io_lib:format(
			   "#xdata_option{label = Translate(~p), value = ~p}",
			   [L, V])
		 end || #xdata_option{label = L, value = V} <- Options],
		","),
    if ?is_list_type(Type) ->
	    io_lib:format(
	      "if Options == default ->"
	      "   [~s];"
	      "true ->"
	      "   [#xdata_option{label = Translate(L), value = ~s}"
	      "    || {L, V} <- Options]"
	      "end",
	      [EncOpts, EncFun]);
       true ->
	    "[]"
    end.

mk_decoding_fun(#xdata_field{var = Var, type = Type,
			     options = Options}, State) ->
    case get_dec_fun(Var, Type, Options, State) of
	{M, Fun, Args} ->
	    Mod = if M == undefined -> "";
		     true -> io_lib:format("~s:", [M])
		  end,
	    FArgs = [io_lib:format(", ~p", [A]) || A <- Args],
	    io_lib:format("~s~s(Value~s)", [Mod, Fun, FArgs]);
	undefined ->
	    "Value"
    end.

var_to_rec_field(Var, #state{prefix = [Prefix|T]} = State) ->
    Size = size(Prefix),
    case Var of
	<<(Prefix):Size/binary, Rest/binary>> ->
	    binary_to_atom(Rest, utf8);
	_ ->
	    var_to_rec_field(Var, State#state{prefix = T})
    end;
var_to_rec_field(Var, #state{prefix = []}) ->
    Var.

get_dec_fun(Var, Type, Options, State) ->
    case lists:keyfind(Var, 1, State#state.dec_mfas) of
	false when Type == 'list-multi'; Type == 'list-single' ->
	    if Options /= [] ->
		    Variants = [binary_to_atom(V, utf8)
				|| #xdata_option{value = V} <- Options],
		    {undefined, dec_enum, [Variants]};
	       true ->
		    undefined
	    end;
	false when Type == 'jid-multi'; Type == 'jid-single' ->
	    {undefined, dec_jid, []};
	false when Type == boolean ->
	    {undefined, dec_bool, []};
	false ->
	    undefined;
	{Var, {M, F, A}} ->
	    {M, F, A};
	{Var, {dec_bool, []}} ->
	    {undefined, dec_bool, []};
	{Var, {not_empty, []}} ->
	    {undefined, not_empty, []};
	{Var, {dec_enum, [Variants]}} ->
	    {undefined, dec_enum, [Variants]};
	{Var, {dec_int, Args}} ->
	    {undefined, dec_int, Args};
	{Var, {dec_enum_int, Args}} ->
	    {undefined, dec_enum_int, Args};
	{Var, {dec_jid, []}} ->
	    {undefined, dec_jid, []}
    end.

get_enc_fun(Var, Type, Options, State) ->
    case get_dec_fun(Var, Type, Options, State) of
	{undefined, dec_enum, _} ->
	    {undefined, enc_enum, []};
	{undefined, dec_bool, _} ->
	    {undefined, enc_bool, []};
	{undefined, dec_int, _} ->
	    {undefined, enc_int, []};
	{undefined, dec_enum_int, _} ->
	    {undefined, enc_enum_int, []};
	{undefined, dec_jid, _} ->
	    {undefined, enc_jid, []};
	_ ->
	    case lists:keyfind(Var, 1, State#state.enc_mfas) of
		false ->
		    undefined;
		{Var, {M, F, A}} ->
		    {M, F, A};
		{Var, {enc_bool, []}} ->
		    {undefined, enc_bool, []};
		{Var, {dec_enum, _}} ->
		    {undefined, enc_enum, []};
		{Var, {enc_int, _}} ->
		    {undefined, enc_int, []};
		{Var, {dec_enum_int, _}} ->
		    {undefined, enc_enum_int, []};
		{Var, {enc_jid, _}} ->
		    {undefined, enc_jid, []}
	    end
    end.

mk_typespec(#xdata_field{type = Type, var = Var} = Field, State) ->
    Spec0 = get_typespec(Field, State),
    Spec1 = case is_complex_type(Spec0) of
		true ->
		    io_lib:format("'~s'()", [var_to_rec_field(Var, State)]);
		false ->
		    Spec0
	    end,
    if ?is_multi_type(Type) -> "[" ++ Spec1 ++ "]";
       true -> Spec1
    end.

get_typespec(#xdata_field{var = Var, type = Type, options = Options}, State) ->
    case lists:keyfind(Var, 1, State#state.specs) of
	false ->
	    case get_dec_fun(Var, Type, Options, State) of
		{undefined, dec_enum, Args} ->
		    enum_spec(Args);
		{undefined, dec_bool, _} ->
		    "boolean()";
		{undefined, dec_jid, _} ->
		    "jid:jid()";
		{undefined, dec_int, Args} ->
		    int_spec(Args);
		{undefined, dec_enum_int, [Variants|T]} ->
		    enum_spec([Variants]) ++ " | " ++ int_spec(T);
		_ ->
		    "binary()"
	    end;
	{Var, Spec} ->
	    Spec
    end.

-spec is_complex_type(string()) -> boolean().
is_complex_type(Spec) ->
    string:chr(Spec, $|) /= 0.

int_spec([]) ->
    "integer()";
int_spec([From, To]) ->
    if From /= infinity, To /= infinity ->
	    io_lib:format("~p..~p", [From, To]);
       From > 0 ->
	    "pos_integer()";
       From == 0 ->
	    "non_neg_integer()";
       true ->
	    "integer()"
    end.

enum_spec([Variants]) ->
    string:join([atom_to_list(V) || V <- Variants], " | ").

is_required(Var, State) ->
    lists:member(Var, State#state.required) orelse
	proplists:get_bool(Var, State#state.required).

normalize(#xmlel{name = Name, attrs = Attrs, children = Els}) ->
    #xmlel{name = Name,
	   attrs = [normalize(Attr) || Attr <- Attrs],
	   children = [normalize(El) || El <- Els]};
normalize({Key, Data}) ->
    {Key, normalize(Data)};
normalize(Txt) when is_binary(Txt) ->
    case re:split(Txt, "[\\s\\r\\n\\t]+", [trim, {return, list}]) of
	[""|T] ->
	    list_to_binary(string:join(T, " "));
	T ->
	    list_to_binary(string:join(T, " "))
    end.
