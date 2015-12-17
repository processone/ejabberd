%%%-------------------------------------------------------------------
%%% File    : ejabberd_captcha.erl
%%% Author  : Evgeniy Khramtsov <xramtsov@gmail.com>
%%% Purpose : CAPTCHA processing.
%%% Created : 26 Apr 2008 by Evgeniy Khramtsov <xramtsov@gmail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%%-------------------------------------------------------------------

-module(ejabberd_captcha).

-behaviour(ejabberd_config).

-protocol({xep, 158, '1.0'}).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-export([create_captcha/6, build_captcha_html/2,
	 check_captcha/2, process_reply/1, process/2,
	 is_feature_available/0, create_captcha_x/5,
	 create_captcha_x/6, opt_type/1]).

-include("jlib.hrl").

-include("ejabberd.hrl").
-include("logger.hrl").

-include("ejabberd_http.hrl").

-define(VFIELD(Type, Var, Value),
	#xmlel{name = <<"field">>,
	       attrs = [{<<"type">>, Type}, {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [Value]}]}).

-define(CAPTCHA_TEXT(Lang),
	translate:translate(Lang,
			    <<"Enter the text you see">>)).

-define(CAPTCHA_LIFETIME, 120000).

-define(LIMIT_PERIOD, 60*1000*1000).

-type error() :: efbig | enodata | limit | malformed_image | timeout.

-record(state, {limits = treap:empty() :: treap:treap()}).

-record(captcha, {id :: binary(),
                  pid :: pid(),
                  key :: binary(),
                  tref :: reference(),
                  args :: any()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec create_captcha(binary(), jid(), jid(),
                     binary(), any(), any()) -> {error, error()} |
                                                {ok, binary(), [xmlel()]}.

create_captcha(SID, From, To, Lang, Limiter, Args) ->
    case create_image(Limiter) of
      {ok, Type, Key, Image} ->
	  Id = <<(randoms:get_string())/binary>>,
	  B64Image = jlib:encode_base64((Image)),
	  JID = jid:to_string(From),
	  CID = <<"sha1+", (p1_sha:sha(Image))/binary,
		  "@bob.xmpp.org">>,
	  Data = #xmlel{name = <<"data">>,
			attrs =
			    [{<<"xmlns">>, ?NS_BOB}, {<<"cid">>, CID},
			     {<<"max-age">>, <<"0">>}, {<<"type">>, Type}],
			children = [{xmlcdata, B64Image}]},
	  Captcha = #xmlel{name = <<"captcha">>,
			   attrs = [{<<"xmlns">>, ?NS_CAPTCHA}],
			   children =
			       [#xmlel{name = <<"x">>,
				       attrs =
					   [{<<"xmlns">>, ?NS_XDATA},
					    {<<"type">>, <<"form">>}],
				       children =
					   [?VFIELD(<<"hidden">>,
						    <<"FORM_TYPE">>,
						    {xmlcdata, ?NS_CAPTCHA}),
					    ?VFIELD(<<"hidden">>, <<"from">>,
						    {xmlcdata,
						     jid:to_string(To)}),
					    ?VFIELD(<<"hidden">>,
						    <<"challenge">>,
						    {xmlcdata, Id}),
					    ?VFIELD(<<"hidden">>, <<"sid">>,
						    {xmlcdata, SID}),
					    #xmlel{name = <<"field">>,
						   attrs =
						       [{<<"var">>, <<"ocr">>},
							{<<"label">>,
							 ?CAPTCHA_TEXT(Lang)}],
						   children =
						       [#xmlel{name =
								   <<"required">>,
							       attrs = [],
							       children = []},
							#xmlel{name =
								   <<"media">>,
							       attrs =
								   [{<<"xmlns">>,
								     ?NS_MEDIA}],
							       children =
								   [#xmlel{name
									       =
									       <<"uri">>,
									   attrs
									       =
									       [{<<"type">>,
										 Type}],
									   children
									       =
									       [{xmlcdata,
										 <<"cid:",
										   CID/binary>>}]}]}]}]}]},
	  BodyString1 = translate:translate(Lang,
					    <<"Your messages to ~s are being blocked. "
					      "To unblock them, visit ~s">>),
	  BodyString = iolist_to_binary(io_lib:format(BodyString1,
                                                      [JID, get_url(Id)])),
	  Body = #xmlel{name = <<"body">>, attrs = [],
			children = [{xmlcdata, BodyString}]},
	  OOB = #xmlel{name = <<"x">>,
		       attrs = [{<<"xmlns">>, ?NS_OOB}],
		       children =
			   [#xmlel{name = <<"url">>, attrs = [],
				   children = [{xmlcdata, get_url(Id)}]}]},
	  Tref = erlang:send_after(?CAPTCHA_LIFETIME, ?MODULE,
				   {remove_id, Id}),
	  ets:insert(captcha,
		     #captcha{id = Id, pid = self(), key = Key, tref = Tref,
			      args = Args}),
	  {ok, Id, [Body, OOB, Captcha, Data]};
      Err -> Err
    end.

-spec create_captcha_x(binary(), jid(), binary(),
                       any(), [xmlel()]) -> {ok, [xmlel()]} |
                                            {error, error()}.

create_captcha_x(SID, To, Lang, Limiter, HeadEls) ->
    create_captcha_x(SID, To, Lang, Limiter, HeadEls, []).

-spec create_captcha_x(binary(), jid(), binary(),
                       any(), [xmlel()], [xmlel()]) -> {ok, [xmlel()]} |
                                                       {error, error()}.

create_captcha_x(SID, To, Lang, Limiter, HeadEls,
		 TailEls) ->
    case create_image(Limiter) of
      {ok, Type, Key, Image} ->
	  Id = <<(randoms:get_string())/binary>>,
	  B64Image = jlib:encode_base64((Image)),
	  CID = <<"sha1+", (p1_sha:sha(Image))/binary,
		  "@bob.xmpp.org">>,
	  Data = #xmlel{name = <<"data">>,
			attrs =
			    [{<<"xmlns">>, ?NS_BOB}, {<<"cid">>, CID},
			     {<<"max-age">>, <<"0">>}, {<<"type">>, Type}],
			children = [{xmlcdata, B64Image}]},
	  HelpTxt = translate:translate(Lang,
					<<"If you don't see the CAPTCHA image here, "
					  "visit the web page.">>),
	  Imageurl = get_url(<<Id/binary, "/image">>),
	  Captcha = #xmlel{name = <<"x">>,
			   attrs =
			       [{<<"xmlns">>, ?NS_XDATA},
				{<<"type">>, <<"form">>}],
			   children =
			       [?VFIELD(<<"hidden">>, <<"FORM_TYPE">>,
					{xmlcdata, ?NS_CAPTCHA})
				| HeadEls]
				 ++
				 [#xmlel{name = <<"field">>,
					 attrs = [{<<"type">>, <<"fixed">>}],
					 children =
					     [#xmlel{name = <<"value">>,
						     attrs = [],
						     children =
							 [{xmlcdata,
							   HelpTxt}]}]},
				  #xmlel{name = <<"field">>,
					 attrs =
					     [{<<"type">>, <<"hidden">>},
					      {<<"var">>, <<"captchahidden">>}],
					 children =
					     [#xmlel{name = <<"value">>,
						     attrs = [],
						     children =
							 [{xmlcdata,
							   <<"workaround-for-psi">>}]}]},
				  #xmlel{name = <<"field">>,
					 attrs =
					     [{<<"type">>, <<"text-single">>},
					      {<<"label">>,
					       translate:translate(Lang,
								   <<"CAPTCHA web page">>)},
					      {<<"var">>, <<"url">>}],
					 children =
					     [#xmlel{name = <<"value">>,
						     attrs = [],
						     children =
							 [{xmlcdata,
							   Imageurl}]}]},
				  ?VFIELD(<<"hidden">>, <<"from">>,
					  {xmlcdata, jid:to_string(To)}),
				  ?VFIELD(<<"hidden">>, <<"challenge">>,
					  {xmlcdata, Id}),
				  ?VFIELD(<<"hidden">>, <<"sid">>,
					  {xmlcdata, SID}),
				  #xmlel{name = <<"field">>,
					 attrs =
					     [{<<"var">>, <<"ocr">>},
					      {<<"label">>,
					       ?CAPTCHA_TEXT(Lang)}],
					 children =
					     [#xmlel{name = <<"required">>,
						     attrs = [], children = []},
					      #xmlel{name = <<"media">>,
						     attrs =
							 [{<<"xmlns">>,
							   ?NS_MEDIA}],
						     children =
							 [#xmlel{name =
								     <<"uri">>,
								 attrs =
								     [{<<"type">>,
								       Type}],
								 children =
								     [{xmlcdata,
								       <<"cid:",
									 CID/binary>>}]}]}]}]
				   ++ TailEls},
	  Tref = erlang:send_after(?CAPTCHA_LIFETIME, ?MODULE,
				   {remove_id, Id}),
	  ets:insert(captcha,
		     #captcha{id = Id, key = Key, tref = Tref}),
	  {ok, [Captcha, Data]};
      Err -> Err
    end.

-spec build_captcha_html(binary(), binary()) -> captcha_not_found |
                                                {xmlel(),
                                                 {xmlel(), xmlel(),
                                                  xmlel(), xmlel()}}.

build_captcha_html(Id, Lang) ->
    case lookup_captcha(Id) of
      {ok, _} ->
	  ImgEl = #xmlel{name = <<"img">>,
			 attrs =
			     [{<<"src">>, get_url(<<Id/binary, "/image">>)}],
			 children = []},
	  TextEl = {xmlcdata, ?CAPTCHA_TEXT(Lang)},
	  IdEl = #xmlel{name = <<"input">>,
			attrs =
			    [{<<"type">>, <<"hidden">>}, {<<"name">>, <<"id">>},
			     {<<"value">>, Id}],
			children = []},
	  KeyEl = #xmlel{name = <<"input">>,
			 attrs =
			     [{<<"type">>, <<"text">>}, {<<"name">>, <<"key">>},
			      {<<"size">>, <<"10">>}],
			 children = []},
	  FormEl = #xmlel{name = <<"form">>,
			  attrs =
			      [{<<"action">>, get_url(Id)},
			       {<<"name">>, <<"captcha">>},
			       {<<"method">>, <<"POST">>}],
			  children =
			      [ImgEl,
			       #xmlel{name = <<"br">>, attrs = [],
				      children = []},
			       TextEl,
			       #xmlel{name = <<"br">>, attrs = [],
				      children = []},
			       IdEl, KeyEl,
			       #xmlel{name = <<"br">>, attrs = [],
				      children = []},
			       #xmlel{name = <<"input">>,
				      attrs =
					  [{<<"type">>, <<"submit">>},
					   {<<"name">>, <<"enter">>},
					   {<<"value">>, <<"OK">>}],
				      children = []}]},
	  {FormEl, {ImgEl, TextEl, IdEl, KeyEl}};
      _ -> captcha_not_found
    end.

-spec process_reply(xmlel()) -> ok | {error, bad_match | not_found | malformed}.

process_reply(#xmlel{} = El) ->
    case xml:get_subtag(El, <<"x">>) of
      false -> {error, malformed};
      Xdata ->
	  Fields = jlib:parse_xdata_submit(Xdata),
	  case catch {proplists:get_value(<<"challenge">>,
					  Fields),
		      proplists:get_value(<<"ocr">>, Fields)}
	      of
	    {[Id | _], [OCR | _]} ->
		case check_captcha(Id, OCR) of
		  captcha_valid -> ok;
		  captcha_non_valid -> {error, bad_match};
		  captcha_not_found -> {error, not_found}
		end;
	    _ -> {error, malformed}
	  end
    end;
process_reply(_) -> {error, malformed}.

process(_Handlers,
	#request{method = 'GET', lang = Lang,
		 path = [_, Id]}) ->
    case build_captcha_html(Id, Lang) of
      {FormEl, _} when is_tuple(FormEl) ->
	  Form = #xmlel{name = <<"div">>,
			attrs = [{<<"align">>, <<"center">>}],
			children = [FormEl]},
	  ejabberd_web:make_xhtml([Form]);
      captcha_not_found -> ejabberd_web:error(not_found)
    end;
process(_Handlers,
	#request{method = 'GET', path = [_, Id, <<"image">>],
		 ip = IP}) ->
    {Addr, _Port} = IP,
    case lookup_captcha(Id) of
      {ok, #captcha{key = Key}} ->
	  case create_image(Addr, Key) of
	    {ok, Type, _, Img} ->
		{200,
		 [{<<"Content-Type">>, Type},
		  {<<"Cache-Control">>, <<"no-cache">>},
		  {<<"Last-Modified">>, list_to_binary(httpd_util:rfc1123_date())}],
		 Img};
	    {error, limit} -> ejabberd_web:error(not_allowed);
	    _ -> ejabberd_web:error(not_found)
	  end;
      _ -> ejabberd_web:error(not_found)
    end;
process(_Handlers,
	#request{method = 'POST', q = Q, lang = Lang,
		 path = [_, Id]}) ->
    ProvidedKey = proplists:get_value(<<"key">>, Q, none),
    case check_captcha(Id, ProvidedKey) of
      captcha_valid ->
	  Form = #xmlel{name = <<"p">>, attrs = [],
			children =
			    [{xmlcdata,
			      translate:translate(Lang,
						  <<"The CAPTCHA is valid.">>)}]},
	  ejabberd_web:make_xhtml([Form]);
      captcha_non_valid -> ejabberd_web:error(not_allowed);
      captcha_not_found -> ejabberd_web:error(not_found)
    end;
process(_Handlers, _Request) ->
    ejabberd_web:error(not_found).

init([]) ->
    mnesia:delete_table(captcha),
    ets:new(captcha,
	    [named_table, public, {keypos, #captcha.id}]),
    check_captcha_setup(),
    {ok, #state{}}.

handle_call({is_limited, Limiter, RateLimit}, _From,
	    State) ->
    NowPriority = now_priority(),
    CleanPriority = NowPriority + (?LIMIT_PERIOD),
    Limits = clean_treap(State#state.limits, CleanPriority),
    case treap:lookup(Limiter, Limits) of
      {ok, _, Rate} when Rate >= RateLimit ->
	  {reply, true, State#state{limits = Limits}};
      {ok, Priority, Rate} ->
	  NewLimits = treap:insert(Limiter, Priority, Rate + 1,
				   Limits),
	  {reply, false, State#state{limits = NewLimits}};
      _ ->
	  NewLimits = treap:insert(Limiter, NowPriority, 1,
				   Limits),
	  {reply, false, State#state{limits = NewLimits}}
    end;
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({remove_id, Id}, State) ->
    ?DEBUG("captcha ~p timed out", [Id]),
    case ets:lookup(captcha, Id) of
      [#captcha{args = Args, pid = Pid}] ->
	  if is_pid(Pid) -> Pid ! {captcha_failed, Args};
	     true -> ok
	  end,
	  ets:delete(captcha, Id);
      _ -> ok
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

create_image() -> create_image(undefined).

create_image(Limiter) ->
    Key = str:substr(randoms:get_string(), 1, 6),
    create_image(Limiter, Key).

create_image(Limiter, Key) ->
    case is_limited(Limiter) of
      true -> {error, limit};
      false -> do_create_image(Key)
    end.

do_create_image(Key) ->
    FileName = get_prog_name(),
    Cmd = lists:flatten(io_lib:format("~s ~s", [FileName, Key])),
    case cmd(Cmd) of
      {ok,
       <<137, $P, $N, $G, $\r, $\n, 26, $\n, _/binary>> =
	   Img} ->
	  {ok, <<"image/png">>, Key, Img};
      {ok, <<255, 216, _/binary>> = Img} ->
	  {ok, <<"image/jpeg">>, Key, Img};
      {ok, <<$G, $I, $F, $8, X, $a, _/binary>> = Img}
	  when X == $7; X == $9 ->
	  {ok, <<"image/gif">>, Key, Img};
      {error, enodata = Reason} ->
	  ?ERROR_MSG("Failed to process output from \"~s\". "
		     "Maybe ImageMagick's Convert program "
		     "is not installed.",
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
    case ejabberd_config:get_option(
           captcha_cmd,
           fun(FileName) ->
                   F = iolist_to_binary(FileName),
                   if F /= <<"">> -> F end
           end) of
        undefined ->
            ?DEBUG("The option captcha_cmd is not configured, "
                   "but some module wants to use the CAPTCHA "
                   "feature.",
                   []),
            false;
        FileName ->
            FileName
    end.

get_url(Str) ->
    CaptchaHost = ejabberd_config:get_option(
                    captcha_host,
                    fun iolist_to_binary/1,
                    <<"">>),
    case str:tokens(CaptchaHost, <<":">>) of
      [Host] ->
	  <<"http://", Host/binary, "/captcha/", Str/binary>>;
      [<<"http", _/binary>> = TransferProt, Host] ->
	  <<TransferProt/binary, ":", Host/binary, "/captcha/",
	    Str/binary>>;
      [Host, PortString] ->
	  TransferProt =
	      iolist_to_binary(atom_to_list(get_transfer_protocol(PortString))),
	  <<TransferProt/binary, "://", Host/binary, ":",
	    PortString/binary, "/captcha/", Str/binary>>;
      [TransferProt, Host, PortString] ->
	  <<TransferProt/binary, ":", Host/binary, ":",
	    PortString/binary, "/captcha/", Str/binary>>;
      _ ->
	  <<"http://", (?MYNAME)/binary, "/captcha/", Str/binary>>
    end.

get_transfer_protocol(PortString) ->
    PortNumber = jlib:binary_to_integer(PortString),
    PortListeners = get_port_listeners(PortNumber),
    get_captcha_transfer_protocol(PortListeners).

get_port_listeners(PortNumber) ->
    AllListeners = ejabberd_config:get_option(listen, fun(V) -> V end),
    lists:filter(fun (Listener) when is_list(Listener) ->
			 case proplists:get_value(port, Listener) of
			   PortNumber -> true;
			   _ -> false
			 end;
		     (_) -> false
		 end,
		 AllListeners).

get_captcha_transfer_protocol([]) ->
    throw(<<"The port number mentioned in captcha_host "
	    "is not a ejabberd_http listener with "
	    "'captcha' option. Change the port number "
	    "or specify http:// in that option.">>);
get_captcha_transfer_protocol([Listener | Listeners]) when is_list(Listener) ->
    case proplists:get_value(module, Listener) == ejabberd_http andalso
	   proplists:get_bool(captcha, Listener) of
      true ->
	  case proplists:get_bool(tls, Listener) of
	    true -> https;
	    false -> http
	  end;
      false -> get_captcha_transfer_protocol(Listeners)
    end;
get_captcha_transfer_protocol([_ | Listeners]) ->
    get_captcha_transfer_protocol(Listeners).

is_limited(undefined) -> false;
is_limited(Limiter) ->
    case ejabberd_config:get_option(
           captcha_limit,
           fun(I) when is_integer(I), I > 0 -> I end) of
      undefined -> false;
      Int ->
	  case catch gen_server:call(?MODULE,
				     {is_limited, Limiter, Int}, 5000)
	      of
	    true -> true;
	    false -> false;
	    Err -> ?ERROR_MSG("Call failed: ~p", [Err]), false
	  end
    end.

-define(CMD_TIMEOUT, 5000).

-define(MAX_FILE_SIZE, 64 * 1024).

cmd(Cmd) ->
    Port = open_port({spawn, Cmd}, [stream, eof, binary]),
    TRef = erlang:start_timer(?CMD_TIMEOUT, self(),
			      timeout),
    recv_data(Port, TRef, <<>>).

recv_data(Port, TRef, Buf) ->
    receive
      {Port, {data, Bytes}} ->
	  NewBuf = <<Buf/binary, Bytes/binary>>,
	  if byte_size(NewBuf) > (?MAX_FILE_SIZE) ->
		 return(Port, TRef, {error, efbig});
	     true -> recv_data(Port, TRef, NewBuf)
	  end;
      {Port, {data, _}} -> return(Port, TRef, {error, efbig});
      {Port, eof} when Buf /= <<>> ->
	  return(Port, TRef, {ok, Buf});
      {Port, eof} -> return(Port, TRef, {error, enodata});
      {timeout, TRef, _} ->
	  return(Port, TRef, {error, timeout})
    end.

return(Port, TRef, Result) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end,
    catch port_close(Port),
    Result.

is_feature_available() ->
    case get_prog_name() of
      Prog when is_binary(Prog) -> true;
      false -> false
    end.

check_captcha_setup() ->
    case is_feature_available() of
      true ->
	  case create_image() of
	    {ok, _, _, _} -> ok;
	    _Err ->
		?CRITICAL_MSG("Captcha is enabled in the option captcha_cmd, "
			      "but it can't generate images.",
			      []),
		throw({error, captcha_cmd_enabled_but_fails})
	  end;
      false -> ok
    end.

lookup_captcha(Id) ->
    case ets:lookup(captcha, Id) of
	[C] -> {ok, C};
	_ -> {error, enoent}
    end.

-spec check_captcha(binary(), binary()) -> captcha_not_found |
                                           captcha_valid |
                                           captcha_non_valid.

check_captcha(Id, ProvidedKey) ->
    case ets:lookup(captcha, Id) of
      [#captcha{pid = Pid, args = Args, key = ValidKey,
		tref = Tref}] ->
	  ets:delete(captcha, Id),
	  erlang:cancel_timer(Tref),
	  if ValidKey == ProvidedKey ->
		 if is_pid(Pid) -> Pid ! {captcha_succeed, Args};
		    true -> ok
		 end,
		 captcha_valid;
	     true ->
		 if is_pid(Pid) -> Pid ! {captcha_failed, Args};
		    true -> ok
		 end,
		 captcha_non_valid
	  end;
      _ -> captcha_not_found
    end.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
      true -> Treap;
      false ->
	  {_Key, Priority, _Value} = treap:get_root(Treap),
	  if Priority > CleanPriority ->
		 clean_treap(treap:delete_root(Treap), CleanPriority);
	     true -> Treap
	  end
    end.

now_priority() ->
    -p1_time_compat:system_time(micro_seconds).

opt_type(captcha_cmd) ->
    fun (FileName) ->
	    F = iolist_to_binary(FileName), if F /= <<"">> -> F end
    end;
opt_type(captcha_host) -> fun iolist_to_binary/1;
opt_type(captcha_limit) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(listen) -> fun (V) -> V end;
opt_type(_) ->
    [captcha_cmd, captcha_host, captcha_limit, listen].
