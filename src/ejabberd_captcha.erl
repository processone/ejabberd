%%%-------------------------------------------------------------------
%%% File    : ejabberd_captcha.erl
%%% Author  : Evgeniy Khramtsov <xramtsov@gmail.com>
%%% Purpose : CAPTCHA processing.
%%% Created : 26 Apr 2008 by Evgeniy Khramtsov <xramtsov@gmail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
	 opt_type/1]).

-include("xmpp.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-define(CAPTCHA_LIFETIME, 120000).
-define(LIMIT_PERIOD, 60*1000*1000).

-type image_error() :: efbig | enodata | limit | malformed_image | timeout.

-record(state, {limits = treap:empty() :: treap:treap()}).

-record(captcha, {id :: binary(),
                  pid :: pid() | undefined,
                  key :: binary(),
                  tref :: reference(),
                  args :: any()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec captcha_text(undefined | binary()) -> binary().
captcha_text(Lang) ->
    translate:translate(Lang, <<"Enter the text you see">>).

-spec mk_ocr_field(binary() | undefined, binary(), binary()) -> xdata_field().
mk_ocr_field(Lang, CID, Type) ->
    URI = #media_uri{type = Type, uri = <<"cid:", CID/binary>>},
    #xdata_field{var = <<"ocr">>,
		 type = 'text-single',
		 label = captcha_text(Lang),
		 required = true,
		 sub_els = [#media{uri = [URI]}]}.

mk_field(Type, Var, Value) ->
    #xdata_field{type = Type, var = Var, values = [Value]}.

-spec create_captcha(binary(), jid(), jid(),
                     binary(), any(), any()) -> {error, image_error()} |
                                                {ok, binary(), [text()], [xmlel()]}.

create_captcha(SID, From, To, Lang, Limiter, Args) ->
    case create_image(Limiter) of
      {ok, Type, Key, Image} ->
	  Id = <<(randoms:get_string())/binary>>,
	  JID = jid:encode(From),
	  CID = <<"sha1+", (str:sha(Image))/binary, "@bob.xmpp.org">>,
	  Data = #bob_data{cid = CID, 'max-age' = 0, type = Type,
			   data = Image},
	  Fs = [mk_field(hidden, <<"FORM_TYPE">>, ?NS_CAPTCHA),
		mk_field(hidden, <<"from">>, jid:encode(To)),
		mk_field(hidden, <<"challenge">>, Id),
		mk_field(hidden, <<"sid">>, SID),
		mk_ocr_field(Lang, CID, Type)],
	  X = #xdata{type = form, fields = Fs},
	  Captcha = #xcaptcha{xdata = X},
	  BodyString = {<<"Your messages to ~s are being blocked. "
			  "To unblock them, visit ~s">>, [JID, get_url(Id)]},
	  Body = xmpp:mk_text(BodyString, Lang),
	  OOB = #oob_x{url = get_url(Id)},
	  Tref = erlang:send_after(?CAPTCHA_LIFETIME, ?MODULE,
				   {remove_id, Id}),
	  ets:insert(captcha,
		     #captcha{id = Id, pid = self(), key = Key, tref = Tref,
			      args = Args}),
	  {ok, Id, Body, [OOB, Captcha, Data]};
      Err -> Err
    end.

-spec create_captcha_x(binary(), jid(), binary(), any(), xdata()) ->
			      {ok, xdata()} | {error, image_error()}.

create_captcha_x(SID, To, Lang, Limiter, #xdata{fields = Fs} = X) ->
    case create_image(Limiter) of
      {ok, Type, Key, Image} ->
	  Id = <<(randoms:get_string())/binary>>,
	  CID = <<"sha1+", (str:sha(Image))/binary, "@bob.xmpp.org">>,
	  Data = #bob_data{cid = CID, 'max-age' = 0, type = Type, data = Image},
	  HelpTxt = translate:translate(Lang,
					<<"If you don't see the CAPTCHA image here, "
					  "visit the web page.">>),
	  Imageurl = get_url(<<Id/binary, "/image">>),
	  NewFs = [mk_field(hidden, <<"FORM_TYPE">>, ?NS_CAPTCHA)|Fs] ++
		[#xdata_field{type = fixed, values = [HelpTxt]},
		 #xdata_field{type = hidden, var = <<"captchahidden">>,
			      values = [<<"workaround-for-psi">>]},
		 #xdata_field{type = 'text-single', var = <<"url">>,
			      label = translate:translate(
					Lang, <<"CAPTCHA web page">>),
			      values = [Imageurl]},
		 mk_field(hidden, <<"from">>, jid:encode(To)),
		 mk_field(hidden, <<"challenge">>, Id),
		 mk_field(hidden, <<"sid">>, SID),
		 mk_ocr_field(Lang, CID, Type)],
	  Captcha = X#xdata{type = form, fields = NewFs},
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
	  TextEl = {xmlcdata, captcha_text(Lang)},
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

-spec process_reply(xmpp_element()) -> ok | {error, bad_match | not_found | malformed}.

process_reply(#xdata{} = X) ->
    case {xmpp_util:get_xdata_values(<<"challenge">>, X),
	  xmpp_util:get_xdata_values(<<"ocr">>, X)} of
	{[Id], [OCR]} ->
	    case check_captcha(Id, OCR) of
		captcha_valid -> ok;
		captcha_non_valid -> {error, bad_match};
		captcha_not_found -> {error, not_found}
	    end;
	_ ->
	    {error, malformed}
    end;
process_reply(#xcaptcha{xdata = #xdata{} = X}) ->
    process_reply(X);
process_reply(_) ->
    {error, malformed}.

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
    PortNumber = binary_to_integer(PortString),
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
