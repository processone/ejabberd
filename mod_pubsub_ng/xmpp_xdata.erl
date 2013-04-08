-module(xmpp_xdata).


-include("jlib.hrl").
-include("ejabberd.hrl").
-compile(export_all).

xmlcdata(CData) ->
    {xmlcdata, CData}.

xmlattr(Name, Value) ->
%    #xmlattr{name = Name, value = Value}.
    {Name, Value}.

xmlattr_label(Label) ->
    xmlattr(<<"label">>, Label).

xmlattr_type(Type) ->
    xmlattr(<<"type">>, Type).

xmlattr_var(Var) ->
    xmlattr(<<"var">>, Var).


xmlel(NS, Name, Attrs, Children) ->
    #xmlel{name = Name, attrs = [{<<"xmlns">>, NS} | Attrs], children = Children}.

xmlel(Name, Attrs, Children) ->
    xmlel(?NS_XDATA, Name, Attrs, Children).

xmlel_desc(CData) ->
    xmlel(<<"desc">>, [], [xmlcdata(CData)]).

xmlel_value(Value) ->
    xmlel(<<"value">>, [], [xmlcdata(Value)]).

xmlel_option({Value, Label}) ->
    xmlel(<<"option">>, [xmlattr_label(Label)], [xmlel_value(Value)]);
xmlel_option(Value) ->
    xmlel(<<"option">>, [], [xmlel_value(Value)]).

xmlel_field(Var, Type, Values) ->
    xmlel(<<"field">>,
        [xmlattr_var(Var),
         xmlattr_type(Type)],
         [xmlel_value(Value) || Value <- Values]).

xmlel_field(Var, Type, Label, Values, Options) ->
    xmlel(<<"field">>,
        [xmlattr_var(Var),
         xmlattr_label(Label),
         xmlattr_type(Type)],
         [xmlel_option(Option) || Option <- Options]
         ++
         [xmlel_value(Value) || Value <- Values]).

xmlel_field_boolean(Var, Label, Value) ->
    xmlel_field(Var, <<"boolean">>, Label, [Value], []).

%%
xmlel_field_fixed(Var, Label, Value) ->
    xmlel_field(Var, <<"fixed">>, Label, [Value], []).

%%
xmlel_field_hidden(Var, Value) ->
    xmlel_field(Var, <<"hidden">>, [Value]).

%%
xmlel_field_list_single(Var, Label, Value, Options) ->
    xmlel_field(Var, <<"list-single">>, Label, [Value], Options).

%%
xmlel_field_list_multi(Var, Label, Values, Options) ->
    xmlel_field(Var, <<"list-multi">>, Label, Values, Options).

%%
xmlel_field_text_single(Var, Label, undefined = _Text) ->
    xmlel_field(Var, <<"text-single">>, Label, [], []);
%%
xmlel_field_text_single(Var, Label, Text) ->
    xmlel_field(Var, <<"text-single">>, Label, [Text], []).

%%
xmlel_field_text_multi(Var, Label, Values) ->
    xmlel_field(Var, <<"text-multi">>, Label, Values, []).

%%
xmlel_field_jid_multi(Var, Label, Jids) ->
    xmlel_field(Var, <<"jid-multi">>, Label, Jids, []).

%%
xmlel_field_jid_single(Var, Label, Jid) ->
    xmlel_field(Var, <<"jid-single">>, Label, [Jid], []).

%%
xmlel_x(Type, Fields) ->
   xmlel(<<"x">>, [xmlattr_type(Type)], Fields).

%%
xmlel_title(Title) ->
    xmlel(<<"title">>, [], [xmlcdata(Title)]).

%%
xmlel_instructions(Instructions) when is_list(Instructions) ->
    lists:map(fun xmlel_instructions/1, Instructions);
%%
xmlel_instructions(Instruction) when is_binary(Instruction) ->
    xmlel(<<"instructions">>, [], [xmlcdata(Instruction)]).
