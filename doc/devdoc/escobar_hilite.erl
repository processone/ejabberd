%%%-------------------------------------------------------------------
%%% File    : escobar_hiliter.erl
%%% Author  : Mats Cronqvist <mats.cronqvist@gmail.com>
%%% Description :
%%%
%%% Created :  6 Jun 2005 by Mats Cronqvist <mats.cronqvist@gmail.com>
%%%
%%%
%%% Escobar, Copyright (c) 2005-2009 Mats Cronqvist
%%%
%%% MIT License:
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%-------------------------------------------------------------------

-module(escobar_hilite).

-export([file/1,tree/1,string/1,out/2]).

-define(LOG(T),escobar:log(process_info(self()),T)).

-import(erl_syntax, 
	[get_ann/1,add_ann/2,subtrees/1,update_tree/2,type/1,get_pos/1,
	 application/2,application_arguments/1,application_operator/1,
	 arity_qualifier_argument/1,arity_qualifier_body/1,
	 atom_name/1,atom_value/1,
	 attribute/2,attribute_name/1,attribute_arguments/1,
         clause/3,clause_patterns/1,clause_guard/1,clause_body/1,
	 comment/2,comment_text/1,comment_padding/1,
	 function/2,function_name/1,function_clauses/1,function_arity/1,
	 integer_literal/1,
	 list/1,list_elements/1,
	 macro/2,macro_name/1,macro_arguments/1,
	 module_qualifier_body/1, module_qualifier_argument/1,
	 string_value/1,string_literal/1,
	 variable_literal/1,variable_name/1,
	 record_access/3, record_access_argument/1, 
	 record_access_field/1,record_access_type/1,
	 record_expr/3, record_expr_argument/1, 
	 record_expr_fields/1, record_expr_type/1,
	 record_index_expr/2, record_index_expr_field/1,
	 record_index_expr_type/1,
         form_list/1,
	 get_precomments/1,get_postcomments/1,has_comments/1, 
	 copy_comments/2,remove_comments/1]).

-import(prettypr,
	[above/2,follow/2,beside/2,empty/0,
	 null_text/1,break/1,floating/3,text/1,floating/1]).

-import(lists,[flatten/1,duplicate/2,keysearch/3,member/2,usort/1,reverse/1]).

-import(filename,[join/1,basename/1]).

out(File,HtmlString) ->
  {ok,FD}=file:open(File,[write]),
  try
    io:fwrite(FD,"<html>",[]),
    io:fwrite(FD,"<link rel=\"stylesheet\" type=\"text/css\"",[]),
    io:fwrite(FD,"href=\"escobar.css\"></link><body><pre>",[]),
    io:fwrite(FD,"~s",[HtmlString]),
    io:fwrite(FD,"</pre></body></html>",[])
  after 
    file:close(FD)
  end.

string(Str) ->
  tree(form_list(scan_and_parse(Str,1,[]))).

scan_and_parse([],_Line,Forms) -> reverse(Forms);
scan_and_parse(Text,Line,Forms) ->
    {done,{ok,Toks,NLine},Cont} = erl_scan:tokens([],Text,Line),
    {ok,Form} = erl_parse:parse_form(Toks),
    scan_and_parse(Cont,NLine,[Form|Forms]).

file(FileName) ->
  case filelib:is_regular(FileName) of
    true -> 
      case filename:extension(FileName) of
        ".beam"-> tree(get_tree_beam(FileName));
        ".erl" -> tree(get_comm_tree_erl(FileName));
        ".hrl" -> tree(get_comm_tree_erl(FileName));
        X -> erlang:error({unknown_extension,X})
      end;
    false->
      erlang:error({no_file,FileName})
  end.

get_comm_tree_erl(Filename) ->
  Comms = erl_comment_scan:file(Filename),
  Forms = get_forms_erl(Filename),
  erl_recomment:recomment_forms(Forms,Comms).

get_tree_beam(Filename) ->
  case beam_lib:chunks(Filename,["Abst"]) of
    {ok,{_,[{"Abst",AChunk}]}} ->
      {_,Forms} = binary_to_term(AChunk),
      form_list(Forms);
    _ ->
      erlang:error({no_debuginfo,Filename})
  end.

get_forms_erl(Filename) ->
  {ok,Fs} = epp_dodger:parse_file(Filename,[{no_fail, true}]),
  Fs.

%%% # tree
%%% turn a syntax tree into html by annotating and pretty-printing
%%% with a hook function

tree(Tree) -> 
  pout(ann(type(Tree),Tree)).

%%lists:foldl(fun(Form,Acc) -> [pout(ann(Form))|Acc] end, [], Tree).

pout(Form) ->
  erl_prettypr:format(Form,[{hook,fun tag/3},{paper,90},{ribbon,650}]).

%%% ## formatting
%%% ### 'tag' - the format hook function
tag(Node,Ctxt,Cont) ->
  Tags = get_ann(Node),
  case member(has_comment,Tags) of
    true ->
      PreC = get_precomments(Node),
      PostC = get_postcomments(Node),
      Nod = remove_comments(Node),
      Doc0 = tagit(Tags--[has_comment],Cont(Nod,Ctxt)),
      postcomment(precomment(Doc0,PreC),PostC);
    false ->
      Doc0 = Cont(Node,Ctxt),
      tagit(Tags,Doc0)
  end.

tagit([],Doc0) -> 
  Doc0;
tagit(["binary"],{beside,_,{beside,Doc,_}}) -> 
  beside(floating(text("&lt;&lt;")),beside(Doc,floating(text("&gt;&gt;"))));
tagit([Tag],Doc0) -> 
  beside(null_text(Tag),beside(Doc0,null_text(etag(Tag)))).

etag("<"++Tag) -> "</"++hd(string:tokens(Tag," "))++">".
  
%%%### comment stuff
precomment(Doc,PreC) ->
  above(floating(break(stack(PreC)), -1, -1), Doc).
postcomment(Doc,PostC) ->
  beside(Doc, floating(break(stack(PostC)), 1, 0)).

stack([]) -> empty();
stack([Comm|Comms]) ->
  Doc = maybe_pad(stack_comment_lines(comment_text(Comm)),Comm),
  case Comms of 
    [] -> Doc;
    _ -> above(Doc, stack(Comms))
  end.

maybe_pad(Doc,Comm) ->
  case comment_padding(Comm) of
    I when is_integer(I), 0 < I -> beside(text(duplicate(I,$ )), Doc);
    _ -> Doc
  end.

%%% stolen with pride from erl_prettypr
%%% Stack lines of text above each other and prefix each string in
%%% the list with a single `%' character.

stack_comment_lines([S | Ss]) ->
  D = tagit([dehtml('span', [{class,comment}])],text("%"++debracket(S))),
  case Ss of
    [] -> D;
    _ -> above(D, stack_comment_lines(Ss))
  end;
stack_comment_lines([]) ->
  empty().

%%% annotate nodes that should be hilited
%%% the annotation is put on the subtree that should be marked up 
%%% the annotation is; 
%%% has_comments|Markup
%%% if a node already has an annotation the new one is dropped, except
%%% if either the new or the old one is has_comments

ann(binary,Tree) ->
  new_tree(Tree,add_anno("binary",Tree));
ann(application,Tree) ->
  Op = application_operator(Tree),
  Args = application_arguments(Tree),
  new_tree(Tree,application(add_anno(mu(application,Tree),Op),Args));
ann(attribute,Tree) ->
  Name = attribute_name(Tree),
  case atom_value(Name) of
    export ->
      AQs = list_elements(hd(attribute_arguments(Tree))),
      Args = [list([add_anno(mu(export,Tree),AQ) || AQ <- AQs])];
    import ->
      [ImportMod,ImportFAs] = attribute_arguments(Tree),
      AQs = list_elements(ImportFAs),
      Args = [ImportMod,list([add_anno(mu(import,Tree),AQ) || AQ <- AQs])];
    define ->
      [Macro|Rest] = attribute_arguments(Tree),
      case type(Macro) of
        application ->
          Op = application_operator(Macro),
          As = application_arguments(Macro),
          Args = [application(add_anno(mu(macro,Tree),Op),As)|Rest];
        _ ->
          Args = [add_anno(mu(macro,Tree),Macro)|Rest]
      end;
    record ->
      [Rec|Rest] = attribute_arguments(Tree),
      Args = [add_anno(mu(record,Tree),Rec)|Rest];
    _ ->
      Args = attribute_arguments(Tree)
  end,
  new_tree(Tree,attribute(add_anno(mu(attribute,Tree),Name),Args));
ann(record_access,Tree) ->
  Arg = record_access_argument(Tree),
  Type = record_access_type(Tree),
  Field = record_access_field(Tree),
  new_tree(Tree,record_access(Arg,add_anno(mu(record,Tree),Type),Field));
ann(record_expr,Tree) ->
  Arg = record_expr_argument(Tree),
  Type = record_expr_type(Tree),
  Fields = record_expr_fields(Tree),
  new_tree(Tree,record_expr(Arg,add_anno(mu(record,Tree),Type),Fields));
ann(record_index_expr,Tree) ->
  Type = record_index_expr_type(Tree),
  Field = record_index_expr_field(Tree),
  new_tree(Tree,record_index_expr(add_anno(mu(record,Tree),Type),Field));
ann(function,Tree) ->
  Name = function_name(Tree),
  Clauses = function_clauses(Tree),
  new_tree(Tree,function(add_anno(mu(function,Tree),Name),Clauses));
ann(macro,Tree) ->
  Name = macro_name(Tree),
  Args = macro_arguments(Tree),
  new_tree(Tree,macro(add_anno(mu(macro,Tree),Name), Args));
ann(string,OTree) ->
  Tree = erl_syntax:string(debracket(string_value(OTree))),
  new_tree(OTree,add_anno(mu(string,OTree),Tree));
ann(variable,Tree) ->
  new_tree(Tree,add_anno(mu(variable,Tree),Tree));
ann(text,Tree) ->
  new_tree(Tree,add_anno(mu(error,Tree),Tree));
ann(comment,OTree) ->
  Pad = comment_padding(OTree),
  Text = [debracket(S) || S <- comment_text(OTree)],
  Tree = comment(Pad,Text),
  new_tree(OTree,add_anno(mu(comment,OTree),Tree));
ann(_Typ,Tree) ->
  new_tree(Tree,Tree).

new_tree(OTree,NTree) ->
  Tree = 
    case has_comments(OTree) of
      true -> add_ann(has_comment,copy_comments(OTree,NTree));
      false -> NTree
    end,
  SubTrees = subtrees(Tree),
  case [[ann(type(SubT),SubT) || SubT<-Group] || Group<-SubTrees] of
    [] -> Tree;
    NSubtrees -> update_tree(Tree,NSubtrees)
  end.

debracket([]) -> [];
debracket([$>|Str]) -> "&gt;"++debracket(Str);
debracket([$<|Str]) -> "&lt;"++debracket(Str);
debracket([C|Str]) -> [C|debracket(Str)].

add_anno(nil,Tree) -> Tree;
add_anno(Ann,Tree) -> 
  case get_ann(Tree) of
    [] -> add_ann(Ann,Tree);
    [has_comment] -> add_ann(Ann,Tree);
    _OAnn -> Tree
  end.

%%%### the markups
mu(application,Node) ->
  Op = application_operator(Node),
  Ar = length(application_arguments(Node)),
  case type(Op) of
%%     variable ->
%%       dehtml('span', [{class,variable}]);
    atom -> 
      case is_guard_or_builtin(atom_value(Op),Ar) of
        guard -> dehtml('span', [{class,guard}]);
        builtin->dehtml('span', [{class,builtin}]);
        neither->dehtml('span', [{class,call}])
      end;
    module_qualifier -> 
      Mod = module_qualifier_argument(Op),
      Fun = module_qualifier_body(Op),
      case {type(Mod),type(Fun)} of
        {atom,atom} -> 
          case atom_value(Mod) of
            erlang -> dehtml('span', [{class,builtin}]);
            _ -> dehtml('span', [{class,call}])
          end;
        _ ->
          nil
      end;
    _ ->
      nil
  end;
mu(function = Class, {tree, function, _, {function, _, [{tree, clause, _, {clause, Vars, _, _}} | _]}}) ->
  dehtml('span', [{class,Class}, {arity,length(Vars)}]);
mu(Class,_Node) -> 
  dehtml('span', [{class,Class}]).

dehtml(Tag,Atts) ->
  flatten([$<,str(Tag),$ ,[[str(A),"=\"",str(V),"\" "]||{A,V}<-Atts],$>]).

str(I) when is_integer(I) -> integer_to_list(I);
str(A) when is_atom(A) -> atom_to_list(A);
str(L) when is_list(L) -> L.

is_guard_or_builtin(atom,1)      ->guard;
is_guard_or_builtin(binary,1)    ->guard;
is_guard_or_builtin(constant,1)  ->guard;
is_guard_or_builtin(float,1)     ->guard;
is_guard_or_builtin(function,1)  ->guard;
is_guard_or_builtin(function,2)  ->guard;
is_guard_or_builtin(integer,1)   ->guard;
is_guard_or_builtin(list,1)      ->guard;
is_guard_or_builtin(number,1)    ->guard;
is_guard_or_builtin(pid,1)       ->guard;
is_guard_or_builtin(port,1)      ->guard;
is_guard_or_builtin(reference,1) ->guard;
is_guard_or_builtin(tuple,1)     ->guard;
is_guard_or_builtin(record,2)    ->guard;
is_guard_or_builtin(record,3)    ->guard;
is_guard_or_builtin(F,A) ->
  case erlang:function_exported(erlang,F,A) orelse 
    erlang:is_builtin(erlang,F,A) of
    true -> builtin;
    false-> neither
  end.
