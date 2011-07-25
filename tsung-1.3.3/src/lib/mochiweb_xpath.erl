%% mochiweb_html_xpath.erl
%% @author Pablo Polvorin
%% created on <2008-04-29>
%%
%% XPath interpreter, navigate mochiweb's html structs
%% Only a subset of xpath is implemented, see what is supported in test.erl
-module(mochiweb_xpath).

-export([execute/2,execute/3,compile_xpath/1]).

%internal data
-record(ctx, {
        root,
        ctx,
        functions
    }).


%% @spec( string() ) -> compiled_xpath()
compile_xpath(Expr) ->
    mochiweb_xpath_parser:compile_xpath(Expr).
    
%% @doc Execute the given XPath expression against the given document, using
%% the default set of functions. 
%% @spec execute(XPath,Doc) -> Results
%% @type XPath =  compiled_xpath() | string()
%% @type Doc = node()
%% @type Results = [node()] | binary() | boolean() | number()
execute(XPathString,Doc) when is_list(XPathString) ->
    XPath = mochiweb_xpath_parser:compile_xpath(XPathString),
    execute(XPath,Doc);

execute(XPath,Root) ->
    execute(XPath,Root,[]).

%% @doc Execute the given XPath expression against the given document, 
%%      using the default set of functions plus the user-supplied ones. 
%%
%% @see mochiweb_xpath_functions.erl to see how to write functions
%%
%% @spec execute(XPath,Doc,Functions) -> Results
%% @type XPath =  compiled_xpath() | string()
%% @type Doc = node()
%% @type Functions = [FunctionDefinition]
%% @type FunctionDefinition = {FunName,Fun,Signature}
%% @type FunName = atom()
%% @type Fun = fun/2
%% @type Signature = [ArgType]
%% @type ArgType = node_set | string | number | boolean
%% @type Results = [node()] | binary() | boolean() | number()
%% TODO: should pass the user-defined functions when compiling
%%       the xpath expression (compile_xpath/1). Then the 
%%       compiled expression would have all its functions 
%%       resolved, and no function lookup would occur when
%%       the expression is executed
execute(XPathString,Doc,Functions) when is_list(XPathString) ->
    XPath = mochiweb_xpath_parser:compile_xpath(XPathString),
    execute(XPath,Doc,Functions);

execute(XPath,Doc,Functions) ->    
    R = {root,none,[Doc]},
    Funs =  lists:foldl(fun(T={Key,_Fun,_Signature},Prev) ->
                            lists:keystore(Key,1,Prev,T)
            end,mochiweb_xpath_functions:default_functions(),Functions),
    execute_expr(XPath,#ctx{ctx=[R],root=R,functions=Funs}).



execute_expr({path,'abs',Path},Ctx =#ctx{root=Root}) ->
    do_path_expr(Path,Ctx#ctx{ctx=[Root]});

execute_expr({path,'rel',Path},Ctx) ->
    do_path_expr(Path,Ctx);

execute_expr({comp,Comp,A,B},Ctx) ->
    CompFun = comp_fun(Comp),
    L = execute_expr(A,Ctx),
    R = execute_expr(B,Ctx),
    comp(CompFun,L,R);

execute_expr({literal,L},_Ctx) ->
    [L];

execute_expr({number,N},_Ctx) ->
    [N];

execute_expr({function_call,Fun,Args},Ctx=#ctx{functions=Funs}) ->
    RealArgs = lists:map(fun(Arg) ->
                            execute_expr(Arg,Ctx)
                        end,Args),
    case lists:keysearch(Fun,1,Funs) of
        {value,{Fun,F,FormalSignature}} -> 
            TypedArgs = lists:map(fun({Type,Arg}) ->
                                    mochiweb_xpath_utils:convert(Arg,Type)
                        end,lists:zip(FormalSignature,RealArgs)),
            F(Ctx,TypedArgs);
        false -> 
            throw({efun_not_found,Fun})
    end.

do_path_expr({step,{Axis,NodeTest,Predicates}},Ctx=#ctx{ctx=Context}) ->
    NewNodeList = axis(Axis,NodeTest,Context),
    apply_predicates(Predicates,NewNodeList,Ctx);

do_path_expr({refine,Step1,Step2},Ctx) ->
    S1 = do_path_expr(Step1,Ctx),
    do_path_expr(Step2,Ctx#ctx{ctx=S1}).


axis('child',{name,{Tag,_,_}},Context) ->
    F = fun ({Tag2,_,_}) when Tag2 == Tag -> true;
             (_) -> false
        end,
    N = lists:map(fun ({_,_,Childs}) -> 
                       lists:filter(F,Childs) ;
                   (_) -> []
                end, Context),
    lists:flatten(N);


axis('child',{node_type,text},Context) ->
    L = lists:map(fun ({_,_,Childs}) -> 
                     case lists:filter(fun is_binary/1,Childs) of
                            [] -> [];
                            T -> list_to_binary(T)
                     end;
                       (_) -> 
                       []
                    end,Context),
    L;

axis('child',{wildcard,wildcard},Context) ->
   L = lists:map(fun
                ({_,_,Children})-> Children;
                (_) -> []
              end, Context),
   lists:flatten(L);
                    

axis(attribute,{name,{Attr,_Prefix,_Local}},Context) ->
     L = lists:foldl(fun ({_,Attrs,_},Acc) -> 
                     case proplists:get_value(Attr,Attrs) of
                            undefined -> Acc;
                            V -> [V|Acc]
                     end;
                       (_,Acc) -> 
                       Acc
                    end,[],Context),
    lists:reverse(L);

axis('descendant_or_self',{node_type,'node'},Context) ->
    descendant_or_self(Context);

axis('self',{node_type,'node'},Context) ->
    Context.

%%FIXME:The order of the result is wrong, it doesn't return the nodes in
%%      document order. Actually the problem isn't here, but in
%%      axis('child',{Tag,_,_},Ctx). We may need to find a better strategy
%%      to implement the child axis if document order is important
descendant_or_self(Ctx) ->
    L = descendant_or_self(Ctx,[]),
    lists:reverse(L).

descendant_or_self([],Acc) ->
    Acc;

descendant_or_self([E={_,_,Children}|Rest],Acc) ->
    N = descendant_or_self(Children,[E|Acc]),
    descendant_or_self(Rest,N);
   
%% text() nodes aren't included
descendant_or_self([_|Rest],Acc) ->
    descendant_or_self(Rest,Acc).


apply_predicates(Predicates,NodeList,Ctx) ->
    lists:foldl(fun(Pred,Nodes) -> 
                 apply_predicate(Pred,Nodes,Ctx) 
                end, NodeList,Predicates).

% special case: indexing
apply_predicate({pred,{number,N}},NodeList,_Ctx) when length(NodeList) >= N ->
    [lists:nth(N,NodeList)];

apply_predicate({pred,Pred},NodeList,Ctx) ->
    Filter = fun(Node) ->
                mochiweb_xpath_utils:boolean_value(
                        execute_expr(Pred,Ctx#ctx{ctx=[Node]}))
              end,
    L = lists:filter(Filter,NodeList),
    L.


%% @see http://www.w3.org/TR/1999/REC-xpath-19991116 , section 3.4 
comp(CompFun,L,R) when is_list(L), is_list(R) ->
    lists:any(fun(LeftValue) ->
                     lists:any(fun(RightValue)->
                                 CompFun(LeftValue,RightValue) 
                               end, R)
              end, L);
comp(CompFun,L,R) when is_list(L) ->
    lists:any(fun(LeftValue) -> CompFun(LeftValue,R) end,L);
comp(CompFun,L,R) when is_list(R) ->
    lists:any(fun(RightValue) -> CompFun(L,RightValue) end,R);
comp(CompFun,L,R) ->
    CompFun(L,R).

comp_fun('=') -> 
    fun 
        (A,B) when is_number(A) -> A == mochiweb_xpath_utils:number_value(B);
        (A,B) when is_number(B) -> mochiweb_xpath_utils:number_value(A) == B;
        (A,B) when is_boolean(A) -> A == mochiweb_xpath_utils:boolean_value(B);
        (A,B) when is_boolean(B) -> mochiweb_xpath_utils:boolean_value(A) == B;
        (A,B) -> mochiweb_xpath_utils:string_value(A) == mochiweb_xpath_utils:string_value(B)
    end;

comp_fun('!=') ->
    fun(A,B) -> F = comp_fun('='),
                not F(A,B) 
    end;

comp_fun('>') ->
  fun(A,B) -> 
    mochiweb_xpath_utils:number_value(A) > mochiweb_xpath_utils:number_value(B) 
  end;
comp_fun('<') ->
  fun(A,B) -> 
    mochiweb_xpath_utils:number_value(A) < mochiweb_xpath_utils:number_value(B)
   end;
comp_fun('<=') ->
  fun(A,B) -> 
    mochiweb_xpath_utils:number_value(A) =< mochiweb_xpath_utils:number_value(B) 
  end;
comp_fun('>=') ->
  fun(A,B) -> 
    mochiweb_xpath_utils:number_value(A) >= mochiweb_xpath_utils:number_value(B) 
  end.







