%%%----------------------------------------------------------------------
%%% File    : mod_configure.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Support for online configuration of ejabberd via x:data
%%% Created : 19 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_configure).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1,
	 stop/0,
	 process_local_iq/3,
	 process_sm_iq/3]).

-include("ejabberd.hrl").
-include("namespaces.hrl").


start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_XDATA,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_XDATA,
				  ?MODULE, process_sm_iq, IQDisc),
    ok.

stop() ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_XDATA),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ?NS_XDATA).


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

-define(XFIELD(Type, Label, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

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
				   translate:translate(Lang,
						       "Disc only copy")}],
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
		    STables = lists:sort(Tables),
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
				end, STables)
			     ]}
	    end
    end;

get_form(["running nodes", ENode, "modules", "stop"], Lang) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    case rpc:call(Node, gen_mod, loaded_modules, []) of
		{badrpc, Reason} ->
		    {error, "500", "Internal Server Error"};
		Modules ->
		    SModules = lists:sort(Modules),
		    {result, [{xmlelement, "title", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang, "Stop Modules")}]},
			      {xmlelement, "instructions", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang, "Choose modules to stop")}]} |
			      lists:map(fun(M) ->
						S = atom_to_list(M),
						?XFIELD("boolean", S, S, "0")
					end, SModules)
			     ]}
	    end
    end;

get_form(["running nodes", ENode, "modules", "start"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Start Modules")}]},
	      {xmlelement, "instructions", [],
	       [{xmlcdata,
	         translate:translate(
	           Lang, "Enter list of {Module, [Options]}")}]},
	      {xmlelement, "field", [{"type", "text-multi"},
				     {"label",
				      translate:translate(
					Lang, "List of modules to start")},
				     {"var", "modules"}],
	       [{xmlelement, "value", [], [{xmlcdata, "[]."}]}]
	      }
	     ]};

get_form(["running nodes", ENode, "backup", "backup"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Backup to File")}]},
	      {xmlelement, "instructions", [],
	       [{xmlcdata,
	         translate:translate(
	           Lang, "Enter path to backup file")}]},
	      {xmlelement, "field", [{"type", "text-single"},
				     {"label",
				      translate:translate(
					Lang, "Path to File")},
				     {"var", "path"}],
	       [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	      }
	     ]};

get_form(["running nodes", ENode, "backup", "restore"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Restore Backup from File")}]},
	      {xmlelement, "instructions", [],
	       [{xmlcdata,
	         translate:translate(
	           Lang, "Enter path to backup file")}]},
	      {xmlelement, "field", [{"type", "text-single"},
				     {"label",
				      translate:translate(
					Lang, "Path to File")},
				     {"var", "path"}],
	       [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	      }
	     ]};

get_form(["running nodes", ENode, "backup", "textfile"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Dump Backup to Text File")}]},
	      {xmlelement, "instructions", [],
	       [{xmlcdata,
	         translate:translate(
	           Lang, "Enter path to text file")}]},
	      {xmlelement, "field", [{"type", "text-single"},
				     {"label",
				      translate:translate(
					Lang, "Path to File")},
				     {"var", "path"}],
	       [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	      }
	     ]};

get_form(["running nodes", ENode, "import", "file"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Import User from File")}]},
	      {xmlelement, "instructions", [],
	       [{xmlcdata,
	         translate:translate(
	           Lang, "Enter path to jabberd1.4 spool file")}]},
	      {xmlelement, "field", [{"type", "text-single"},
				     {"label",
				      translate:translate(
					Lang, "Path to File")},
				     {"var", "path"}],
	       [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	      }
	     ]};

get_form(["running nodes", ENode, "import", "dir"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Import User from Dir")}]},
	      {xmlelement, "instructions", [],
	       [{xmlcdata,
	         translate:translate(
	           Lang, "Enter path to jabberd1.4 spool dir")}]},
	      {xmlelement, "field", [{"type", "text-single"},
				     {"label",
				      translate:translate(
					Lang, "Path to Dir")},
				     {"var", "path"}],
	       [{xmlelement, "value", [], [{xmlcdata, ""}]}]
	      }
	     ]};

get_form(["config", "hostname"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Hostname Configuration")}]},
	      {xmlelement, "instructions", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Choose host name")}]},
	      {xmlelement, "field", [{"type", "text-single"},
				     {"label",
				      translate:translate(Lang, "Host name")},
				     {"var", "hostname"}],
	       [{xmlelement, "value", [], [{xmlcdata, ?MYNAME}]}]}
	     ]};

get_form(["config", "acls"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "ACLs Configuration")}]},
	      %{xmlelement, "instructions", [],
	      % [{xmlcdata,
	      %   translate:translate(
	      %     Lang, "")}]},
	      {xmlelement, "field", [{"type", "text-multi"},
				     {"label",
				      translate:translate(Lang, "ACLs")},
				     {"var", "acls"}],
	       lists:map(fun(S) ->
				 {xmlelement, "value", [], [{xmlcdata, S}]}
			 end,
			 string:tokens(
			   lists:flatten(io_lib:format("~p.",
						       [ets:tab2list(acl)])),
			   "\n"))
	      }
	     ]};

get_form(["config", "access"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Access Configuration")}]},
	      %{xmlelement, "instructions", [],
	      % [{xmlcdata,
	      %   translate:translate(
	      %     Lang, "")}]},
	      {xmlelement, "field", [{"type", "text-multi"},
				     {"label",
				      translate:translate(
					Lang, "Access Rules")},
				     {"var", "access"}],
	       lists:map(fun(S) ->
				 {xmlelement, "value", [], [{xmlcdata, S}]}
			 end,
			 string:tokens(
			   lists:flatten(
			     io_lib:format(
			       "~p.",
			       [ets:select(config,
					   [{{config, {access, '$1'}, '$2'},
					     [],
					     [{{access, '$1', '$2'}}]}])
			       ])),
			   "\n"))
	      }
	     ]};

get_form(["config", "remusers"], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Remove Users")}]},
	      {xmlelement, "instructions", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Choose users to remove")}]}] ++
     case catch ejabberd_auth:dirty_get_registered_users() of
	 {'EXIT', Reason} ->
	     [];
	 Users ->
	     lists:map(fun(U) ->
			       ?XFIELD("boolean", U, U, "0")
		       end, lists:sort(Users))
     end
    };

get_form(_, Lang) ->
    {error, "503", "Service Unavailable"}.



set_form(["running nodes", ENode, "DB"], Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    lists:foreach(
	      fun({SVar, SVals}) ->
		      % We believe that this is allowed only for good peoples
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

set_form(["running nodes", ENode, "modules", "stop"], Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    lists:foreach(
	      fun({Var, Vals}) ->
		      case Vals of
			  ["1"] ->
			      Module = list_to_atom(Var),
			      rpc:call(Node, gen_mod, stop_module, [Module]);
			  _ ->
			      ok
		      end
	      end, XData),
	    {result, []}
    end;

set_form(["running nodes", ENode, "modules", "start"], Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    case lists:keysearch("modules", 1, XData) of
		false ->
		    {error, "406", "Not Acceptable"};
		{value, {_, Strings}} ->
		    String = lists:foldl(fun(S, Res) ->
						 Res ++ S ++ "\n"
					 end, "", Strings),
		    case erl_scan:string(String) of
			{ok, Tokens, _} ->
			    case erl_parse:parse_term(Tokens) of
				{ok, Modules} ->
				    lists:foreach(
				      fun({Module, Args}) ->
					      rpc:call(Node,
						       gen_mod,
						       start_module,
						       [Module, Args])
				      end, Modules),
				    {result, []};
				_ ->
				    {error, "406", "Not Acceptable"}
			    end;
			_ ->
			    {error, "406", "Not Acceptable"}
		    end;
		_ ->
		    {error, "406", "Not Acceptable"}
	    end
    end;


set_form(["running nodes", ENode, "backup", "backup"], Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, "406", "Not Acceptable"};
		{value, {_, [String]}} ->
		    case rpc:call(Node, mnesia, backup, [String]) of
			{badrpc, Reason} ->
			    {error, "500", "Internal Server Error"};
			{error, Reason} ->
			    {error, "500", "Internal Server Error"};
			_ ->
			    {result, []}
			end;
		_ ->
		    {error, "406", "Not Acceptable"}
	    end
    end;


set_form(["running nodes", ENode, "backup", "restore"], Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, "406", "Not Acceptable"};
		{value, {_, [String]}} ->
		    case rpc:call(Node, mnesia, restore,
				  [String, [{default_op, keep_tables}]]) of
			{badrpc, Reason} ->
			    {error, "500", "Internal Server Error"};
			{error, Reason} ->
			    {error, "500", "Internal Server Error"};
			_ ->
			    {result, []}
			end;
		_ ->
		    {error, "406", "Not Acceptable"}
	    end
    end;


set_form(["running nodes", ENode, "backup", "textfile"], Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, "406", "Not Acceptable"};
		{value, {_, [String]}} ->
		    case rpc:call(Node, mnesia, dump_to_textfile, [String]) of
			{badrpc, Reason} ->
			    {error, "500", "Internal Server Error"};
			{error, Reason} ->
			    {error, "500", "Internal Server Error"};
			_ ->
			    {result, []}
			end;
		_ ->
		    {error, "406", "Not Acceptable"}
	    end
    end;


set_form(["running nodes", ENode, "import", "file"], Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, "406", "Not Acceptable"};
		{value, {_, [String]}} ->
		    rpc:call(Node, jd2ejd, import_file, [String]),
		    {result, []};
		_ ->
		    {error, "406", "Not Acceptable"}
	    end
    end;


set_form(["running nodes", ENode, "import", "dir"], Lang, XData) ->
    case search_running_node(ENode) of
	false ->
	    {error, "404", "Not Found"};
	Node ->
	    case lists:keysearch("path", 1, XData) of
		false ->
		    {error, "406", "Not Acceptable"};
		{value, {_, [String]}} ->
		    rpc:call(Node, jd2ejd, import_dir, [String]);
		_ ->
		    {error, "406", "Not Acceptable"}
	    end
    end;


set_form(["config", "hostname"], Lang, XData) ->
    case lists:keysearch("hostname", 1, XData) of
	false ->
	    {error, "406", "Not Acceptable"};
	{value, {_, [""]}} ->
	    {error, "406", "Not Acceptable"};
	{value, {_, [NewName]}} ->
	    ejabberd_config:add_global_option(hostname, NewName),
	    {result, []};
	_ ->
	    {error, "406", "Not Acceptable"}
    end;

set_form(["config", "acls"], Lang, XData) ->
    case lists:keysearch("acls", 1, XData) of
	{value, {_, Strings}} ->
	    String = lists:foldl(fun(S, Res) ->
					 Res ++ S ++ "\n"
				 end, "", Strings),
	    case erl_scan:string(String) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, ACLs} ->
			    case acl:add_list(ACLs, true) of
				ok ->
				    {result, []};
				_ ->
				    {error, "406", "Not Acceptable"}
			    end;
			_ ->
			    {error, "406", "Not Acceptable"}
		    end;
		_ ->
		    {error, "406", "Not Acceptable"}
	    end;
	_ ->
	    {error, "406", "Not Acceptable"}
    end;

set_form(["config", "access"], Lang, XData) ->
    SetAccess =
	fun(Rs) ->
		mnesia:transaction(
		  fun() ->
			  Os = mnesia:select(config,
					     [{{config, {access, '$1'}, '$2'},
					       [],
					       ['$_']}]),
			  lists:foreach(fun(O) ->
						mnesia:delete_object(O)
					end, Os),
			  lists:foreach(
			    fun({access, Name, Rules}) ->
				    mnesia:write({config,
						  {access, Name},
						  Rules})
			    end, Rs)
		  end)
	end,
    case lists:keysearch("access", 1, XData) of
	{value, {_, Strings}} ->
	    String = lists:foldl(fun(S, Res) ->
					 Res ++ S ++ "\n"
				 end, "", Strings),
	    case erl_scan:string(String) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, Rs} ->
			    case SetAccess(Rs) of
				{atomic, _} ->
				    {result, []};
				E ->
				    io:format("A: ~p~n", [E]),
				    {error, "406", "Not Acceptable"}
			    end;
			_ ->
			    {error, "406", "Not Acceptable"}
		    end;
		_ ->
		    {error, "406", "Not Acceptable"}
	    end;
	_ ->
	    {error, "406", "Not Acceptable"}
    end;

set_form(["config", "remusers"], Lang, XData) ->
    lists:foreach(
      fun({Var, Vals}) ->
	      case Vals of
		  ["1"] ->
		      ejabberd_sm ! {route, {"", "", ""}, {Var, "", ""},
				     {xmlelement, "broadcast", [],
				      [{exit, "User removed"}]}},
		      catch ejabberd_auth:remove_user(Var),
		      catch mod_roster:remove_user(Var),
		      catch mod_offline:remove_user(Var),
		      catch mod_vcard:remove_user(Var);
		  _ ->
		      ok
	      end
      end, XData),
    {result, []};

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_sm_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case acl:match_rule(configure, From) of
	deny ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	allow ->
	    {User, _, _} = To,
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
				    case set_sm_form(
					   User, Node, Lang, XData) of
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
		    case get_sm_form(User, Node, Lang) of
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


get_sm_form(User, [], Lang) ->
    {result, [{xmlelement, "title", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "Administration of " ++ User)}]},
	      %{xmlelement, "instructions", [],
	      % [{xmlcdata,
	      %   translate:translate(
	      %     Lang, "Choose host name")}]},
	      {xmlelement, "field",
	       [{"type", "list-single"},
		{"label", translate:translate(Lang, "Action on user")},
		{"var", "action"}],
	       [{xmlelement, "value", [], [{xmlcdata, "edit"}]},
		{xmlelement, "option",
		 [{"label", translate:translate(Lang, "Edit Properties")}],
		 [{xmlelement, "value", [], [{xmlcdata, "edit"}]}]},
		{xmlelement, "option",
		 [{"label", translate:translate(Lang, "Remove User")}],
		 [{xmlelement, "value", [], [{xmlcdata, "remove"}]}]}
	       ]},
	      ?XFIELD("text-private", "Password", "password",
		      ejabberd_auth:get_password_s(User))
	      %{xmlelement, "field", [{"type", "text-single"},
	      %  		     {"label",
	      %  		      translate:translate(Lang, "Host name")},
	      %  		     {"var", "hostname"}],
	      % [{xmlelement, "value", [], [{xmlcdata, ?MYNAME}]}]}
	     ]};

get_sm_form(_, _, Lang) ->
    {error, "503", "Service Unavailable"}.


set_sm_form(_, _, Lang, XData) ->
    {error, "503", "Service Unavailable"}.
