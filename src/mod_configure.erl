%%%----------------------------------------------------------------------
%%% File    : mod_configure.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 19 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_configure).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0,
	 process_local_iq/3]).

-include("ejabberd.hrl").
-include("namespaces.hrl").


start() ->
    ejabberd_local:register_iq_handler(?NS_XDATA,
				       ?MODULE, process_local_iq).


process_local_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case acl:match_rule(configure, From) of
	deny ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	allow ->
	    Lang = xml:get_tag_attr_s("xml:lang", SubEl),
	    case Type of
		set ->
		    case xml:get_tag_attr_s("type", SubEl) of
			"cancel" ->
			    {iq, ID, result, XMLNS,
			     [{xmlelement, "query", [{"xmlns", XMLNS}], []}]};
			"submit" ->
			    XData = jlib:parse_xdata_submit(SubEl),
			    case XData of
				invalid ->
				    {iq, ID, error, XMLNS,
				     [SubEl, {xmlelement, "error",
					      [{"code", "400"}],
					      [{xmlcdata, "Bad Request"}]}]};
				_ ->
				    Node =
					string:tokens(
					  xml:get_tag_attr_s("node", SubEl),
					  "/"),
				    case set_form(Node, Lang, XData) of
					{result, Res} ->
					    {iq, ID, result, XMLNS,
					     [{xmlelement, "query",
					       [{"xmlns", XMLNS}],
					       Res
					      }]};
					{error, Code, Desc} ->
					    {iq, ID, error, XMLNS,
					     [SubEl, {xmlelement, "error",
						      [{"code", Code}],
						      [{xmlcdata, Desc}]}]}
				    end
			    end;
			_ ->
			    {iq, ID, error, XMLNS,
			     [SubEl, {xmlelement, "error",
				      [{"code", "405"}],
				      [{xmlcdata, "Not Allowed"}]}]}
		    end;
		get ->
		    Node =
			string:tokens(xml:get_tag_attr_s("node", SubEl), "/"),
		    case get_form(Node, Lang) of
			{result, Res} ->
			    {iq, ID, result, XMLNS,
			     [{xmlelement, "query", [{"xmlns", XMLNS}],
			       Res
			      }]};
			{error, Code, Desc} ->
			    {iq, ID, error, XMLNS,
			     [SubEl, {xmlelement, "error",
				      [{"code", Code}],
				      [{xmlcdata, Desc}]}]}
		    end
	    end
    end.

-define(TLFIELD(Type, Label, Var),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).

-define(TABLEFIELD(Table, Val),
	{xmlelement, "field", [{"type", "list-single"},
			       {"label", atom_to_list(Table)},
			       {"var", atom_to_list(Table)}],
	 [{xmlelement, "value", [], [{xmlcdata, atom_to_list(Val)}]},
	  {xmlelement, "option", [{"label",
				   translate:translate(Lang, "RAM copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "ram_copies"}]}]},
	  {xmlelement, "option", [{"label",
				   translate:translate(Lang,
						       "RAM and disc copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "disc_copies"}]}]},
	  {xmlelement, "option", [{"label",
				   translate:translate(Lang, "Disk copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "disc_only_copies"}]}]},
	  {xmlelement, "option", [{"label",
				   translate:translate(Lang, "Remote copy")}],
	   [{xmlelement, "value", [], [{xmlcdata, "unknown"}]}]}
	 ]}).



get_form(["running nodes", ENode, "DB"], Lang) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    case rpc:call(Node, mnesia, system_info, [tables]) of
		{badrpc, Reason} ->
		    {error, "500", "Internal Server Error"};
		Tables ->
		    {result, [{xmlelement, "title", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang, "DB Tables Configuration")}]},
			      {xmlelement, "instructions", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang, "Choose storage type of tables")}]} |
			      lists:map(
				fun(Table) ->
					case rpc:call(Node,
						      mnesia,
						      table_info,
						      [Table, storage_type]) of
					    {badrpc, _} ->
						?TABLEFIELD(Table, unknown);
					    Type ->
						?TABLEFIELD(Table, Type)
					end
				end, Tables)
			     ]}
	    end
    end;

get_form(_, Lang) ->
    {error, "503", "Service Unavailable"}.



set_form(["running nodes", ENode, "DB"], Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    lists:foreach(
	      fun({SVar, SVals}) ->
		      % We believe that this allowed only for good peoples
		      Table = list_to_atom(SVar),
		      Type = case SVals of
				 ["unknown"] -> unknown;
				 ["ram_copies"] -> ram_copies;
				 ["disc_copies"] -> disc_copies;
				 ["disc_only_copies"] -> disc_only_copies;
				 _ -> false
			     end,
		      if
			  Type == false ->
			      ok;
			  Type == unknown ->
			      mnesia:del_table_copy(Table, Node);
			  true ->
			      case mnesia:add_table_copy(Table, Node, Type) of
				  {aborted, _} ->
				      mnesia:change_table_copy_type(
					Table, Node, Type);
				  _ ->
				      ok
			      end
		      end
	      end, XData),
	    {result, []}
    end;

set_form(_, Lang, XData) ->
    {error, "503", "Service Unavailable"}.



search_running_node(SNode) ->
    search_running_node(SNode, mnesia:system_info(running_db_nodes)).

search_running_node(_, []) ->
    false;
search_running_node(SNode, [Node | Nodes]) ->
    case atom_to_list(Node) of
	SNode ->
	    Node;
	_ ->
	    search_running_node(SNode, Nodes)
    end.


