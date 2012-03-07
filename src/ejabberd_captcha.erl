%%%-------------------------------------------------------------------
%%% File    : ejabberd_captcha.erl
%%% Author  : Evgeniy Khramtsov <xramtsov@gmail.com>
%%% Purpose : CAPTCHA processing.
%%% Created : 26 Apr 2008 by Evgeniy Khramtsov <xramtsov@gmail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

%%% Database schema (version / storage / table)
%%%
%%% 2.1.x / mnesia / captcha
%%%  id = string()
%%%  pid = pid()
%%%  key = string()
%%%  tref = any()
%%%  args = any()
%%%
%%% 3.0.0-alpha / ets / captcha
%%%  id = string()
%%%  pid = pid()
%%%  key = string()
%%%  tref = any()
%%%  args = any()

-module(ejabberd_captcha).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([create_captcha/5, build_captcha_html/2, check_captcha/2,
	 process_reply/1, process/2, is_feature_available/0,
	 create_captcha_x/4, create_captcha_x/5]).

-include_lib("exmpp/include/exmpp.hrl").

-include("jlib.hrl").
-include("ejabberd.hrl").
-include("web/ejabberd_http.hrl").

-define(VFIELD(Type, Var, Value),
  #xmlel{name = 'field',
         attrs = [
           #xmlattr{name = <<"type">>,
             value = Type
           },
           #xmlattr{name = <<"var">>,
             value = Var
           }
         ],
         children = [
           #xmlel{name = 'value',
             children = [Value]
           }
         ]}).

-define(CAPTCHA_TEXT(Lang), list_to_binary(translate:translate(Lang, "Enter the text you see"))).
-define(CAPTCHA_LIFETIME, 120000). % two minutes
-define(RPC_TIMEOUT, 5000).

-record(state, {}).
-record(captcha, {id, pid, key, tref, args}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_captcha(SID, From, To, Lang, Args)
  when is_binary(Lang), is_binary(SID) ->
    case create_image() of
	{ok, Type, Key, Image} ->
	    Id = randoms:get_string() ++ "-" ++ ejabberd_cluster:node_id(),
	    B64Image = list_to_binary(jlib:encode_base64(binary_to_list(Image))),
	    JID = exmpp_jid:to_list(From),
	    CID = list_to_binary(["sha1+", sha:sha(Image), "@bob.xmpp.org"]),
%	    Data = {xmlelement, "data",
%		    [{"xmlns", ?NS_BOB}, {"cid", CID},
%		     {"max-age", "0"}, {"type", Type}],
%		    [{xmlcdata, B64Image}]},
	    Data =
	      #xmlel{
	        name = 'data',
	        ns = ?NS_BOB,
	        attrs = [
	          #xmlattr{name = <<"cid">>,
	            value = CID
	          },
	          #xmlattr{name = <<"max-age">>,
	            value = <<"0">>
	          },
	          #xmlattr{name = <<"type">>,
	            value = Type	                    
	          }
	        ],
	        children = [#xmlcdata{cdata = B64Image}]},
%		{xmlelement, "captcha", [{"xmlns", ?NS_CAPTCHA}],
%		 %% ?NS_DATA_FORMS is 'jabber:x:data'
%		 [{xmlelement, "x", [{"xmlns", "jabber:x:data"}, {"type", "form"}],
%		   [?VFIELD("hidden", "FORM_TYPE", {xmlcdata, ?NS_CAPTCHA}),
%		    ?VFIELD("hidden", "from", {xmlcdata, exmpp_jid:to_list(To)}),
%		    ?VFIELD("hidden", "challenge", {xmlcdata, Id}),
%		    ?VFIELD("hidden", "sid", {xmlcdata, SID}),
%		    {xmlelement, "field", [{"var", "ocr"}, {"label", ?CAPTCHA_TEXT(Lang)}],
%		     [{xmlelement, "media", [{"xmlns", ?NS_DATA_FORMS_MEDIA_s}],
%		       [{xmlelement, "uri", [{"type", Type}],
%			 [{xmlcdata, "cid:" ++ CID}]}]}]}]}]},
%% TODO : kael : write exmpp_captcha.erl
	    Captcha =
	  #xmlel{
	    name = 'captcha',
	    ns = ?NS_CAPTCHA,
	    children = [
	      #xmlel{name = 'x',
	        ns = ?NS_DATA_FORMS_s,
	        attrs = [
	          #xmlattr{name = <<"type">>,
	            value = <<"form">>
	          }
	        ],
	        children = [
	          ?VFIELD(<<"hidden">>, <<"FORM_TYPE">>, #xmlcdata{cdata = ?NS_CAPTCHA_b}),
	          ?VFIELD(<<"hidden">>, <<"from">>, #xmlcdata{cdata = exmpp_jid:to_binary(To)}),
	          ?VFIELD(<<"hidden">>, <<"challenge">>, #xmlcdata{cdata = list_to_binary(Id)}),
	          ?VFIELD(<<"hidden">>, <<"sid">>, #xmlcdata{cdata = SID}),
	          #xmlel{name = 'field',
	            attrs = [
	              #xmlattr{name = <<"var">>,
	                value = <<"ocr">>
	              },
	              #xmlattr{name = <<"label">>,
	                value = ?CAPTCHA_TEXT(Lang)
	              }	                      
	            ],
	            children = [
					#xmlel{ns = ?NS_DATA_FORMS, name = 'required'},
	              #xmlel{name = 'media',
	                ns = ?NS_DATA_FORMS_MEDIA_s,
	                children = [
	                  #xmlel{name = 'uri',
	                    attrs = [
	                      #xmlattr{name = <<"type">>,
	                        value = Type
	                      }
	                    ],
	                    children = [
	                      #xmlcdata{cdata = list_to_binary(["cid:", CID])}
	                    ]
	                  }
	                ]
	              }
	            ]
	          }
	        ]
	      }
	    ]
	  },
	    BodyString1 = translate:translate(Lang, "Your messages to ~s are being blocked. To unblock them, visit ~s"),
	    BodyString = io_lib:format(BodyString1, [JID, get_url(Id)]),
      Body =
        #xmlel{name = 'body',
          children = [
            #xmlcdata{cdata = list_to_binary(BodyString)
            }
          ]
        },
      OOB =
        #xmlel{name = 'x',
          ns = ?NS_OOBD_X_s,
          children = [
            #xmlel{name = 'url',
              children = [
                #xmlcdata{cdata = list_to_binary(get_url(Id))}
              ]
            }
          ]
        },
	    %Body = {xmlelement, "body", [],
		  %  [{xmlcdata, BodyString}]},
	    %OOB = {xmlelement, "x", [{"xmlns", ?NS_OOBD_X_s}],
		  % [{xmlelement, "url", [], [{xmlcdata, get_url(Id)}]}]},
	    Tref = erlang:send_after(?CAPTCHA_LIFETIME, ?MODULE, {remove_id, Id}),
	    ets:insert(captcha, #captcha{id=Id, pid=self(), key=Key,
					 tref=Tref, args=Args}),
	    {ok, Id, [Body, OOB, Captcha, Data]};
	_Err ->
	    error
    end.

create_captcha_x(SID, To, Lang, HeadEls) ->
    create_captcha_x(SID, To, Lang, HeadEls, []).

create_captcha_x(SID, To, Lang, HeadEls, TailEls) ->
    case create_image() of
	{ok, Type, Key, Image} ->
	    Id = randoms:get_string() ++ "-" ++ ejabberd_cluster:node_id(),
	    B64Image = list_to_binary(jlib:encode_base64(binary_to_list(Image))),
	    CID = list_to_binary(["sha1+", sha:sha(Image), "@bob.xmpp.org"]),
	    Data =
	      #xmlel{
	        name = 'data',
	        ns = ?NS_BOB,
	        attrs = [
	          #xmlattr{name = <<"cid">>,
	            value = CID
	          },
	          #xmlattr{name = <<"max-age">>,
	            value = <<"0">>
	          },
	          #xmlattr{name = <<"type">>,
	            value = Type
	          }
	        ],
	        children = [#xmlcdata{cdata = B64Image}]},
            HelpTxt = translate:translate(
                                Lang,
                                 "If you don't see the CAPTCHA image here, "
                                 "visit the web page."),
           Imageurl = list_to_binary(get_url(Id ++ "/image")),
	    Captcha =
	      #xmlel{name = 'x',
	        ns = ?NS_DATA_FORMS_s,
	        attrs = [
	          #xmlattr{name = <<"type">>,
	            value = <<"form">>
	          }
	        ],
	        children = [
				?VFIELD(<<"hidden">>, <<"FORM_TYPE">>, #xmlcdata{cdata = ?NS_CAPTCHA_b}) | HeadEls] ++ [
            #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
             [?XMLATTR(<<"type">>, <<"fixed">>), ?XMLATTR(<<"label">>, HelpTxt)]},
	          ?VFIELD(<<"hidden">>, <<"captchahidden">>, #xmlcdata{cdata = <<"workaround-for-psi">>}),

  #xmlel{name = 'field',
         attrs = [
           #xmlattr{name = <<"type">>,
             value = <<"text-single">>
           },
           #xmlattr{name = <<"label">>,
			   value = list_to_binary(translate:translate(Lang, "CAPTCHA web page"))
           },
           #xmlattr{name = <<"var">>,
             value = <<"url">>
           }
         ],
         children = [
           #xmlel{name = 'value',
			   children = [#xmlcdata{cdata = Imageurl}]
           }
         ]},
	          ?VFIELD(<<"hidden">>, <<"from">>, #xmlcdata{cdata = exmpp_jid:to_binary(To)}),
	          ?VFIELD(<<"hidden">>, <<"challenge">>, #xmlcdata{cdata = list_to_binary(Id)}),
	          ?VFIELD(<<"hidden">>, <<"sid">>, #xmlcdata{cdata = SID}),
	          #xmlel{name = 'field',
	            attrs = [
	              #xmlattr{name = <<"var">>,
	                value = <<"ocr">>
	              },
	              #xmlattr{name = <<"label">>,
	                value = ?CAPTCHA_TEXT(Lang)
	              }
	            ],
	            children = [
					#xmlel{ns = ?NS_DATA_FORMS, name = 'required'},
	              #xmlel{name = 'media',
	                ns = ?NS_DATA_FORMS_MEDIA_s,
	                children = [
	                  #xmlel{name = 'uri',
	                    attrs = [
	                      #xmlattr{name = <<"type">>,
	                        value = Type
	                      }
	                    ],
	                    children = [
	                      #xmlcdata{cdata = list_to_binary(["cid:", CID])}
	                    ]
	                  }
	                ]
	              }
	            ]
	          }
	        ] ++ TailEls
	      },
	    Tref = erlang:send_after(?CAPTCHA_LIFETIME, ?MODULE, {remove_id, Id}),
	    ets:insert(captcha, #captcha{id=Id, key=Key, tref=Tref}),
	    {ok, [Captcha, Data]};
	_ ->
	    error
    end.

%% @spec (Id::string(), Lang::string()) -> {FormEl, {ImgEl, TextEl, IdEl, KeyEl}} | captcha_not_found
%% where FormEl = xmlelement()
%%       ImgEl = xmlelement()
%%       TextEl = xmlelement()
%%       IdEl = xmlelement()
%%       KeyEl = xmlelement()
build_captcha_html(Id, Lang) ->
    case lookup_captcha(Id) of
	{ok, _} ->
	    %ImgEl = {xmlelement, "img", [{"src", get_url(Id ++ "/image")}], []},
	    ImgEl =
	      #xmlel{name = 'img',
          attrs = [
            #xmlattr{name = <<"src">>,
              value = list_to_binary(get_url(Id ++ "/image"))
            }
          ]
        },
	    %TextEl = {xmlcdata, ?CAPTCHA_TEXT(Lang)},
	    TextEl = #xmlcdata{cdata = ?CAPTCHA_TEXT(Lang)},
	    %IdEl = {xmlelement, "input", [{"type", "hidden"},
			%		  {"name", "id"},
			%		  {"value", Id}], []},
	    IdEl = 
	      #xmlel{name = 'input',
	        attrs = [
	          #xmlattr{name = <<"type">>,
	            value = <<"hidden">>
	          },
	          #xmlattr{name = <<"name">>,
	            value = <<"id">>
	          },
	          #xmlattr{name = <<"value">>,
	            value = list_to_binary(Id)
	          }
	        ]
	      },
	    %KeyEl = {xmlelement, "input", [{"type", "text"},
			%		   {"name", "key"},
			%		   {"size", "10"}], []},
			KeyEl =
			  #xmlel{name = 'input',
			    attrs = [
			      #xmlattr{name = <<"type">>,
			        value = <<"text">>
			      },
			      #xmlattr{name = <<"name">>,
			        value = <<"key">>
			      },
			      #xmlattr{name = <<"size">>,
			        value = <<"10">>
			      }
			    ]
			  },
	    %FormEl = {xmlelement, "form", [{"action", get_url(Id)},
			%		   {"name", "captcha"},
			%		   {"method", "POST"}],
		  %    [ImgEl,
		  %     {xmlelement, "br", [], []},
		  %     TextEl,
		  %     {xmlelement, "br", [], []},
		  %     IdEl,
		  %     KeyEl,
		  %     {xmlelement, "br", [], []},
		  %     {xmlelement, "input", [{"type", "submit"},
			%		      {"name", "enter"},
			%		      {"value", "OK"}], []}
		  %    ]},
			FormEl = 
			  #xmlel{name = 'form',
			    attrs = [
			      #xmlattr{name = <<"action">>,
			        value = list_to_binary(get_url(Id))
			      },
			      #xmlattr{name = <<"name">>,
			        value = <<"captcha">>
			      },
			      #xmlattr{name = <<"method">>,
			        value = <<"POST">>
			      }
			    ],
			    children = [
			      ImgEl,
			      #xmlel{name = 'br'
			      },
			      TextEl,
			      #xmlel{name = 'br'
			      },
			      IdEl,
			      KeyEl,
			      #xmlel{name = 'br'
			      },
			      #xmlel{name = 'input',
			        attrs = [
			          #xmlattr{name = <<"type">>,
			            value = <<"submit">>
			          },
			          #xmlattr{name = <<"name">>,
			            value = <<"enter">>
			          },
			          #xmlattr{name = <<"value">>,
			            value = <<"OK">>
			          }
			        ]
			      }
			    ]
			  },
	    {FormEl, {ImgEl, TextEl, IdEl, KeyEl}};
	_ ->
	    captcha_not_found
    end.

%% @spec (Id::string(), ProvidedKey::string()) -> captcha_valid | captcha_non_valid | captcha_not_found
check_captcha(Id, ProvidedKey) ->
    case string:tokens(Id, "-") of
	[_, NodeID] ->
	    case ejabberd_cluster:get_node_by_id(NodeID) of
		Node when Node == node() ->
		    do_check_captcha(Id, ProvidedKey);
		Node ->
		    case catch rpc:call(Node, ?MODULE, check_captcha,
					[Id, ProvidedKey], ?RPC_TIMEOUT) of
			{'EXIT', _} ->
			    captcha_not_found;
			{badrpc, _} ->
			    captcha_not_found;
			Res ->
			    Res
		    end
	    end;
	_ ->
	    captcha_not_found
    end.

process_reply(El) ->
    case exmpp_xml:get_element(El, x) of
	undefined ->
	    {error, malformed};
	Xdata ->
	    Fields = jlib:parse_xdata_submit(Xdata),
	    case catch {proplists:get_value("challenge", Fields),
			proplists:get_value("ocr", Fields)} of
		{[Id|_], [OCR|_]} ->
		    case check_captcha(Id, OCR) of
			captcha_valid ->
			    ok;
			captcha_non_valid ->
			    {error, bad_match};
			captcha_not_found ->
			    {error, not_found}
		    end;
		_ ->
		    {error, malformed}
	    end
    end.


process(_Handlers, #request{method='GET', lang=Lang, path=[_, Id]}) ->
    case build_captcha_html(Id, Lang) of
	{FormEl, CaptchaTuple} when is_tuple(CaptchaTuple) ->
	    Form =
		%{xmlelement, "div", [{"align", "center"}],
		 %[FormEl]},
	  #xmlel{name = 'div',
	    attrs = [
	      #xmlattr{name = <<"align">>,
	        value = <<"center">>
	      }
	    ],
	    children = [FormEl]
	  },
	    ejabberd_web:make_xhtml([Form]);
	captcha_not_found ->
	    ejabberd_web:error(not_found)
    end;

process(_Handlers, #request{method='GET', path=[_, Id, "image"]}) ->
    case lookup_captcha(Id) of
	{ok, #captcha{key=Key}} ->
	    case create_image(Key) of
		{ok, Type, _, Img} ->
		    {200,
		     [{"Content-Type", Type},
		      {"Cache-Control", "no-cache"},
		      {"Last-Modified", httpd_util:rfc1123_date()}],
		     Img};
		_ ->
		    ejabberd_web:error(not_found)
	    end;
	_ ->
	    ejabberd_web:error(not_found)
    end;

process(_Handlers, #request{method='POST', q=Q, lang=Lang, path=[_, Id]}) ->
    ProvidedKey = proplists:get_value("key", Q, none),
    case check_captcha(Id, ProvidedKey) of
	captcha_valid ->
	    Form =
		%{xmlelement, "p", [],
		% [{xmlcdata,
		%   translate:translate(Lang, "The captcha is valid.")
		%  }]},
	  #xmlel{name = 'p',
	    children = [
			#xmlcdata{cdata = list_to_binary(translate:translate(Lang, "The captcha is valid."))}
	    ]
	  },
	    ejabberd_web:make_xhtml([Form]);
	captcha_non_valid ->
	    ejabberd_web:error(not_allowed);
	captcha_not_found ->
	    ejabberd_web:error(not_found)
    end;

process(_Handlers, _Request) ->
    ejabberd_web:error(not_found).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    mnesia:delete_table(captcha),
    ets:new(captcha, [named_table, public, {keypos, #captcha.id}]),
    check_captcha_setup(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({remove_id, Id}, State) ->
    ?DEBUG("captcha ~p timed out", [Id]),
    case ets:lookup(captcha, Id) of
	[#captcha{args=Args, pid=Pid}] ->
	    if is_pid(Pid) ->
		    Pid ! {captcha_failed, Args};
	       true ->
		    ok
	    end,
	    ets:delete(captcha, Id);
	_ ->
	    ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: create_image() -> {ok, Type, Key, Image} | {error, Reason}
%% Type = "image/png" | "image/jpeg" | "image/gif"
%% Key = binary()
%% Image = binary()
%% Reason = atom()
%%--------------------------------------------------------------------
create_image() ->
    %% Six numbers from 1 to 9.
    Key = string:substr(randoms:get_string(), 1, 6),
    create_image(Key).

create_image(Key) ->
    FileName = get_prog_name(),
    Cmd = lists:flatten(io_lib:format("~s ~s", [FileName, Key])),
    case cmd(Cmd) of
	{ok, <<16#89, $P, $N, $G, $\r, $\n, 16#1a, $\n, _/binary>> = Img} ->
	    {ok, <<"image/png">>, Key, Img};
	{ok, <<16#ff, 16#d8, _/binary>> = Img} ->
	    {ok, <<"image/jpeg">>, Key, Img};
	{ok, <<$G, $I, $F, $8, X, $a, _/binary>> = Img} when X==$7; X==$9 ->
	    {ok, <<"image/gif">>, Key, Img};
	{error, enodata = Reason} ->
	    ?ERROR_MSG("Failed to process output from \"~s\". "
		       "Maybe ImageMagick's Convert program is not installed.",
		       [Cmd]),
	    {error, Reason};
	{error, Reason} ->
	    ?ERROR_MSG("Failed to process an output from \"~s\": ~p",
		       [Cmd, Reason]),
	    {error, Reason};
	_ ->
	    Reason = malformed_image,
	    ?ERROR_MSG("Failed to process an output from \"~s\": ~p",
		       [Cmd, Reason]),
	    {error, Reason}
    end.

get_prog_name() ->
    case ejabberd_config:get_local_option(captcha_cmd) of
	FileName when is_list(FileName) ->
	    FileName;
	_ ->
	    ?DEBUG("The option captcha_cmd is not configured, but some "
			  "module wants to use the CAPTCHA feature.", []),
	    throw({error, option_not_configured_captcha_cmd})
    end.

%% @doc (Str::string()) -> string()
get_url(Str) ->
    CaptchaHost = ejabberd_config:get_local_option(captcha_host),
    case string:tokens(CaptchaHost, ":") of
	[Host] ->
	    "http://" ++ Host ++ "/captcha/" ++ Str;
	["http"++_ = TransferProt, Host] ->
	    TransferProt ++ ":" ++ Host ++ "/captcha/" ++ Str;
	[Host, PortString] ->
	    TransferProt = atom_to_list(get_transfer_protocol(PortString)),
	    TransferProt ++ "://" ++ Host ++ ":" ++ PortString ++ "/captcha/" ++ Str;
	[TransferProt, Host, PortString] ->
	    TransferProt ++ ":" ++ Host ++ ":" ++ PortString ++ "/captcha/" ++ Str;
	_ ->
	    "http://" ++ ?MYNAME ++ "/captcha/" ++ Str
    end.

get_transfer_protocol(PortString) ->
    PortNumber = list_to_integer(PortString),
    PortListeners = get_port_listeners(PortNumber),
    get_captcha_transfer_protocol(PortListeners).

get_port_listeners(PortNumber) ->
    AllListeners = ejabberd_config:get_local_option(listen),
    lists:filter(
      fun({{Port, _Ip, _Netp}, _Module1, _Opts1}) when Port == PortNumber ->
	      true;
	 (_) ->
	      false
      end,
      AllListeners).

get_captcha_transfer_protocol([]) ->
    throw("The port number mentioned in captcha_host is not "
	  "a ejabberd_http listener with 'captcha' option. "
	  "Change the port number or specify http:// in that option.");
get_captcha_transfer_protocol([{{_Port, _Ip, tcp}, ejabberd_http, Opts}
			       | Listeners]) ->
    case lists:member(captcha, Opts) of
	true ->
	    case lists:member(tls, Opts) of
		true ->
		    https;
		false ->
		    http
	    end;
	false ->
	    get_captcha_transfer_protocol(Listeners)
    end;
get_captcha_transfer_protocol([_ | Listeners]) ->
    get_captcha_transfer_protocol(Listeners).

%%--------------------------------------------------------------------
%% Function: cmd(Cmd) -> Data | {error, Reason}
%% Cmd = string()
%% Data = binary()
%% Description: os:cmd/1 replacement
%%--------------------------------------------------------------------
-define(CMD_TIMEOUT, 5000).
-define(MAX_FILE_SIZE, 64*1024).

cmd(Cmd) ->
    Port = open_port({spawn, Cmd}, [stream, eof, binary]),
    TRef = erlang:start_timer(?CMD_TIMEOUT, self(), timeout),
    recv_data(Port, TRef, <<>>).

recv_data(Port, TRef, Buf) ->
    receive
	{Port, {data, Bytes}} ->
	    NewBuf = <<Buf/binary, Bytes/binary>>,
	    if size(NewBuf) > ?MAX_FILE_SIZE ->
		    return(Port, TRef, {error, efbig});
	       true ->
		    recv_data(Port, TRef, NewBuf)
	    end;
	{Port, {data, _}} ->
	    return(Port, TRef, {error, efbig});
	{Port, eof} when Buf /= <<>> ->
	    return(Port, TRef, {ok, Buf});
	{Port, eof} ->
	    return(Port, TRef, {error, enodata});
	{timeout, TRef, _} ->
	    return(Port, TRef, {error, timeout})
    end.

return(Port, TRef, Result) ->
    case erlang:cancel_timer(TRef) of
	false ->
	    receive
		{timeout, TRef, _} ->
		    ok
	    after 0 ->
		    ok
	    end;
	_ ->
	    ok
    end,
    catch port_close(Port),
    Result.

is_feature_enabled() ->
    try get_prog_name() of
	Prog when is_list(Prog) -> true
    catch 
	_:_ -> false
    end.

is_feature_available() ->
    case is_feature_enabled() of
	false -> false;
	true ->
	    case create_image() of
		{ok, _, _, _} -> true;
		_Error -> false
	    end
    end.

check_captcha_setup() ->
    case is_feature_enabled() andalso not is_feature_available() of
	true ->
	    ?CRITICAL_MSG("Captcha is enabled in the option captcha_cmd, "
			  "but it can't generate images.", []);
	false ->
	    ok
    end.

lookup_captcha(Id) ->
    case string:tokens(Id, "-") of
	[_, NodeID] ->
	    case ejabberd_cluster:get_node_by_id(NodeID) of
		Node when Node == node() ->
		    case ets:lookup(captcha, Id) of
			[C] ->
			    {ok, C};
			_ ->
			    {error, enoent}
		    end;
		Node ->
		    case catch rpc:call(Node, ets, lookup,
					[captcha, Id], ?RPC_TIMEOUT) of
			[C] ->
			    {ok, C};
			_ ->
			    {error, enoent}
		    end
	    end;
	_ ->
	    {error, enoent}
    end.

do_check_captcha(Id, ProvidedKey) ->
    case ets:lookup(captcha, Id) of
	[#captcha{pid = Pid, args = Args, key = ValidKey, tref = Tref}] ->
	    ets:delete(captcha, Id),
	    erlang:cancel_timer(Tref),
	    if ValidKey == ProvidedKey ->
		    if is_pid(Pid) ->
			    Pid ! {captcha_succeed, Args};
		       true ->
			    ok
		    end,
		    captcha_valid;
	       true ->
		    if is_pid(Pid) ->
			    Pid ! {captcha_failed, Args};
		       true ->
			    ok
		    end,
		    captcha_non_valid
	    end;
	_ ->
	    captcha_not_found
    end.
