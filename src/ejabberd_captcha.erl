%%%-------------------------------------------------------------------
%%% File    : ejabberd_captcha.erl
%%% Author  : Evgeniy Khramtsov <xramtsov@gmail.com>
%%% Purpose : CAPTCHA processing.
%%% Created : 26 Apr 2008 by Evgeniy Khramtsov <xramtsov@gmail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-protocol({xep, 158, '1.0', '2.1.0', "complete", ""}).
-protocol({xep, 231, '1.0', '2.1.0', "complete", ""}).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-export([create_captcha/6, build_captcha_html/2,
	 check_captcha/2, process_reply/1, process/2,
	 is_feature_available/0, create_captcha_x/5,
	 host_up/1, host_down/1,
	 config_reloaded/0, process_iq/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("translate.hrl").

-define(CAPTCHA_LIFETIME, 120000).
-define(LIMIT_PERIOD, 60*1000*1000).

-type image_error() :: efbig | enodata | limit | malformed_image | timeout.
-type priority() :: neg_integer().
-type callback() :: fun((captcha_succeed | captcha_failed) -> any()).

-record(state, {limits = treap:empty() :: treap:treap(),
		enabled = false :: boolean()}).

-record(captcha, {id :: binary(),
                  pid :: pid() | undefined,
                  key :: binary(),
                  tref :: reference(),
                  args :: any()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec captcha_text(binary()) -> binary().
captcha_text(Lang) ->
    translate:translate(Lang, ?T("Enter the text you see")).

-spec mk_ocr_field(binary(), binary(), binary()) -> xdata_field().
mk_ocr_field(Lang, CID, Type) ->
    URI = #media_uri{type = Type, uri = <<"cid:", CID/binary>>},
    [_, F] = captcha_form:encode([{ocr, <<>>}], Lang, [ocr]),
    xmpp:set_els(F, [#media{uri = [URI]}]).

update_captcha_key(_Id, Key, Key) ->
    ok;
update_captcha_key(Id, _Key, Key2) ->
    true = ets:update_element(captcha, Id, [{4, Key2}]).

-spec create_captcha(binary(), jid(), jid(),
                     binary(), any(),
		     callback() | term()) -> {error, image_error()} |
					     {ok, binary(), [text()], [xmpp_element()]}.
create_captcha(SID, From, To, Lang, Limiter, Args) ->
    case create_image(Limiter) of
      {ok, Type, Key, Image} ->
	    Id = <<(p1_rand:get_string())/binary>>,
	    JID = jid:encode(From),
	    CID = <<"sha1+", (str:sha(Image))/binary, "@bob.xmpp.org">>,
	    Data = #bob_data{cid = CID, 'max-age' = 0, type = Type, data = Image},
	    Fs = captcha_form:encode(
		   [{from, To}, {challenge, Id}, {sid, SID},
		    mk_ocr_field(Lang, CID, Type)],
		   Lang, [challenge]),
	    X = #xdata{type = form, fields = Fs},
	    Captcha = #xcaptcha{xdata = X},
	    BodyString = {?T("Your subscription request and/or messages to ~s have been blocked. "
			     "To unblock your subscription request, visit ~s"), [JID, get_url(Id)]},
	    Body = xmpp:mk_text(BodyString, Lang),
	    OOB = #oob_x{url = get_url(Id)},
	    Hint = #hint{type = 'no-store'},
	    Tref = erlang:send_after(?CAPTCHA_LIFETIME, ?MODULE, {remove_id, Id}),
	    ets:insert(captcha,
		       #captcha{id = Id, pid = self(), key = Key, tref = Tref,
				args = Args}),
	    {ok, Id, Body, [Hint, OOB, Captcha, Data]};
	Err -> Err
    end.

-spec create_captcha_x(binary(), jid(), binary(), any(), xdata()) ->
			      {ok, [xmpp_element()]} | {error, image_error()}.
create_captcha_x(SID, To, Lang, Limiter, #xdata{fields = Fs} = X) ->
    case create_image(Limiter) of
      {ok, Type, Key, Image} ->
	    Id = <<(p1_rand:get_string())/binary>>,
	    CID = <<"sha1+", (str:sha(Image))/binary, "@bob.xmpp.org">>,
	    Data = #bob_data{cid = CID, 'max-age' = 0, type = Type, data = Image},
	    HelpTxt = translate:translate(
			Lang, ?T("If you don't see the CAPTCHA image here, visit the web page.")),
	    Imageurl = get_url(<<Id/binary, "/image">>),
	    [H|T] = captcha_form:encode(
		      [{'captcha-fallback-text', HelpTxt},
		       {'captcha-fallback-url', Imageurl},
		       {from, To}, {challenge, Id}, {sid, SID},
		       mk_ocr_field(Lang, CID, Type)],
		      Lang, [challenge]),
	    Captcha = X#xdata{type = form, fields = [H|Fs ++ T]},
	    Tref = erlang:send_after(?CAPTCHA_LIFETIME, ?MODULE, {remove_id, Id}),
	    ets:insert(captcha, #captcha{id = Id, key = Key, tref = Tref}),
	    {ok, [Captcha, Data]};
	Err -> Err
    end.

-spec build_captcha_html(binary(), binary()) -> captcha_not_found |
                                                {xmlel(),
                                                 {xmlel(), cdata(),
                                                  xmlel(), xmlel()}}.

build_captcha_html(Id, Lang) ->
    case lookup_captcha(Id) of
      {ok, _} ->
	  ImgEl = #xmlel{name = <<"img">>,
			 attrs =
			     [{<<"src">>, get_url(<<Id/binary, "/image">>)}],
			 children = []},
	  Text = {xmlcdata, captcha_text(Lang)},
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
			       Text,
			       #xmlel{name = <<"br">>, attrs = [],
				      children = []},
			       IdEl, KeyEl,
			       #xmlel{name = <<"br">>, attrs = [],
				      children = []},
			       #xmlel{name = <<"input">>,
				      attrs =
					  [{<<"type">>, <<"submit">>},
					   {<<"name">>, <<"enter">>},
					   {<<"value">>, ?T("OK")}],
				      children = []}]},
	  {FormEl, {ImgEl, Text, IdEl, KeyEl}};
      _ -> captcha_not_found
    end.

-spec process_reply(xmpp_element()) -> ok | {error, bad_match | not_found | malformed}.

process_reply(#xdata{} = X) ->
    Required = [<<"challenge">>, <<"ocr">>],
    Fs = lists:filter(
	   fun(#xdata_field{var = Var}) ->
		   lists:member(Var, [<<"FORM_TYPE">>|Required])
	   end, X#xdata.fields),
    try captcha_form:decode(Fs, [?NS_CAPTCHA], Required) of
	Props ->
	    Id = proplists:get_value(challenge, Props),
	    OCR = proplists:get_value(ocr, Props),
	    case check_captcha(Id, OCR) of
		captcha_valid -> ok;
		captcha_non_valid -> {error, bad_match};
		captcha_not_found -> {error, not_found}
	    end
    catch _:{captcha_form, Why} ->
	    ?WARNING_MSG("Malformed CAPTCHA form: ~ts",
			 [captcha_form:format_error(Why)]),
	    {error, malformed}
    end;
process_reply(#xcaptcha{xdata = #xdata{} = X}) ->
    process_reply(X);
process_reply(_) ->
    {error, malformed}.

-spec process_iq(iq()) -> iq().
process_iq(#iq{type = set, lang = Lang, sub_els = [#xcaptcha{} = El]} = IQ) ->
    case process_reply(El) of
	ok ->
	    xmpp:make_iq_result(IQ);
	{error, malformed} ->
	    Txt = ?T("Incorrect CAPTCHA submit"),
	    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang));
	{error, _} ->
	    Txt = ?T("The CAPTCHA verification has failed"),
	    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang))
    end;
process_iq(#iq{type = get, lang = Lang} = IQ) ->
    Txt = ?T("Value 'get' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_iq(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).

process(_Handlers,
	#request{method = 'GET', lang = Lang,
		 path = [_, Id]}) ->
    case build_captcha_html(Id, Lang) of
      {FormEl, _} ->
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
	    {ok, Type, Key2, Img} ->
		update_captcha_key(Id, Key, Key2),
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
						  ?T("The CAPTCHA is valid."))}]},
	  ejabberd_web:make_xhtml([Form]);
      captcha_non_valid -> ejabberd_web:error(not_allowed);
      captcha_not_found -> ejabberd_web:error(not_found)
    end;
process(_Handlers, _Request) ->
    ejabberd_web:error(not_found).

host_up(Host) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CAPTCHA,
				  ?MODULE, process_iq).

host_down(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_CAPTCHA).

config_reloaded() ->
    gen_server:call(?MODULE, config_reloaded, timer:minutes(1)).

init([]) ->
    _ = mnesia:delete_table(captcha),
    _ = ets:new(captcha, [named_table, public, {keypos, #captcha.id}]),
    case check_captcha_setup() of
	true ->
	    register_handlers(),
	    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 70),
	    {ok, #state{enabled = true}};
	false ->
	    {ok, #state{enabled = false}};
	{error, Reason} ->
	    {stop, Reason}
    end.

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
handle_call(config_reloaded, _From, #state{enabled = Enabled} = State) ->
    State1 = case is_feature_available() of
		 true when not Enabled ->
		     case check_captcha_setup() of
			 true ->
			     register_handlers(),
			     State#state{enabled = true};
			 _ ->
			     State
		     end;
		 false when Enabled ->
		     unregister_handlers(),
		     State#state{enabled = false};
		 _ ->
		     State
	     end,
    {reply, ok, State1};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({remove_id, Id}, State) ->
    ?DEBUG("CAPTCHA ~p timed out", [Id]),
    case ets:lookup(captcha, Id) of
	[#captcha{args = Args, pid = Pid}] ->
	    callback(captcha_failed, Pid, Args),
	    ets:delete(captcha, Id);
	_ -> ok
    end,
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{enabled = Enabled}) ->
    if Enabled -> unregister_handlers();
       true -> ok
    end,
    ejabberd_hooks:delete(config_reloaded, ?MODULE, config_reloaded, 70).

register_handlers() ->
    ejabberd_hooks:add(host_up, ?MODULE, host_up, 50),
    ejabberd_hooks:add(host_down, ?MODULE, host_down, 50),
    lists:foreach(fun host_up/1, ejabberd_option:hosts()).

unregister_handlers() ->
    ejabberd_hooks:delete(host_up, ?MODULE, host_up, 50),
    ejabberd_hooks:delete(host_down, ?MODULE, host_down, 50),
    lists:foreach(fun host_down/1, ejabberd_option:hosts()).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec create_image() -> {ok, binary(), binary(), binary()} |
			{error, image_error()}.
create_image() ->
    create_image(undefined).

-spec create_image(term()) -> {ok, binary(), binary(), binary()} |
			      {error, image_error()}.
create_image(Limiter) ->
    Key = str:substr(p1_rand:get_string(), 1, 6),
    create_image(Limiter, Key).

-spec create_image(term(), binary()) -> {ok, binary(), binary(), binary()} |
					{error, image_error()}.
create_image(Limiter, Key) ->
    case is_limited(Limiter) of
	true -> {error, limit};
	false -> do_create_image(Key)
    end.

-spec do_create_image(binary()) -> {ok, binary(), binary(), binary()} |
				   {error, image_error()}.
do_create_image(Key) ->
    FileName = get_prog_name(),
    case length(binary:split(FileName, <<"/">>)) == 1 of
        true ->
            do_create_image(Key, misc:binary_to_atom(FileName));
        false ->
            do_create_image(Key, FileName)
    end.

do_create_image(Key, Module) when is_atom(Module) ->
    Function = create_image,
    erlang:apply(Module, Function, [Key]);

do_create_image(Key, FileName) when is_binary(FileName) ->
    Cmd = lists:flatten(io_lib:format("~ts ~ts", [FileName, Key])),
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
	  ?ERROR_MSG("Failed to process output from \"~ts\". "
		     "Maybe ImageMagick's Convert program "
		     "is not installed.",
		     [Cmd]),
	  {error, Reason};
      {error, Reason} ->
	  ?ERROR_MSG("Failed to process an output from \"~ts\": ~p",
		     [Cmd, Reason]),
	  {error, Reason};
      _ ->
	  Reason = malformed_image,
	  ?ERROR_MSG("Failed to process an output from \"~ts\": ~p",
		     [Cmd, Reason]),
	  {error, Reason}
    end.

get_prog_name() ->
    case ejabberd_option:captcha_cmd() of
        undefined ->
            ?WARNING_MSG("The option captcha_cmd is not configured, "
                   "but some module wants to use the CAPTCHA "
                   "feature.",
                   []),
            false;
        FileName ->
            maybe_warning_norequesthandler(),
            FileName
    end.

maybe_warning_norequesthandler() ->
    AutoURL = get_url(),
    ManualURL = ejabberd_option:captcha_url(),
    case (AutoURL == undefined) and not is_binary(ManualURL) of
        true ->
            ?CRITICAL_MSG("The option captcha_cmd is configured "
                   "and captcha_url is set to auto, "
                   "but I couldn't find a request_handler in listen option "
                   "configured with ejabberd_captcha and integer port. "
                   "Please setup the URL with option captcha_url, see "
                   "https://docs.ejabberd.im/admin/configuration/basic/#captcha",
                   []);
        _ ->
            ok
    end.

-spec get_url(binary()) -> binary().
get_url(Str) ->
    case ejabberd_option:captcha_url() of
	auto ->
            URL = get_url(),
            <<URL/binary, $/, Str/binary>>;
	undefined ->
	    URL = parse_captcha_host(),
	    <<URL/binary, "/captcha/", Str/binary>>;
	URL ->
	    <<URL/binary, $/, Str/binary>>
    end.

get_url() ->
    case ejabberd_http:get_auto_url(any, ?MODULE) of
        undefined ->
            undefined;
        Url ->
            Host = ejabberd_config:get_myname(),
            misc:expand_keyword(<<"@HOST@">>, Url, Host)
    end.

-spec parse_captcha_host() -> binary().
parse_captcha_host() ->
    CaptchaHost = ejabberd_option:captcha_host(),
    case str:tokens(CaptchaHost, <<":">>) of
	[Host] ->
	    <<"http://", Host/binary>>;
	[<<"http", _/binary>> = TransferProt, Host] ->
	    <<TransferProt/binary, ":", Host/binary>>;
	[Host, PortString] ->
	    TransferProt = atom_to_binary(get_transfer_protocol(PortString), latin1),
	    <<TransferProt/binary, "://", Host/binary, ":", PortString/binary>>;
	[TransferProt, Host, PortString] ->
	    <<TransferProt/binary, ":", Host/binary, ":", PortString/binary>>;
      _ ->
	    <<"http://", (ejabberd_config:get_myname())/binary>>
    end.

get_transfer_protocol(PortString) ->
    PortNumber = binary_to_integer(PortString),
    PortListeners = get_port_listeners(PortNumber),
    get_captcha_transfer_protocol(PortListeners).

get_port_listeners(PortNumber) ->
    AllListeners = ejabberd_option:listen(),
    lists:filter(
      fun({{Port, _IP, _Transport}, _Module, _Opts}) ->
	      Port == PortNumber
      end, AllListeners).

get_captcha_transfer_protocol([]) ->
    throw(<<"The port number mentioned in captcha_host "
	    "is not a ejabberd_http listener with "
	    "'captcha' option. Change the port number "
	    "or specify http:// in that option.">>);
get_captcha_transfer_protocol([{_, ejabberd_http, Opts} | Listeners]) ->
    Handlers = maps:get(request_handlers, Opts, []),
    case lists:any(
	   fun({_, ?MODULE}) -> true;
	      ({_, _}) -> false
	   end, Handlers) of
	true ->
	    case maps:get(tls, Opts) of
		true -> https;
		false -> http
	    end;
	false ->
	    get_captcha_transfer_protocol(Listeners)
    end;
get_captcha_transfer_protocol([_ | Listeners]) ->
    get_captcha_transfer_protocol(Listeners).

is_limited(undefined) -> false;
is_limited(Limiter) ->
    case ejabberd_option:captcha_limit() of
      infinity -> false;
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

-spec cmd(string()) -> {ok, binary()} | {error, image_error()}.
cmd(Cmd) ->
    Port = open_port({spawn, Cmd}, [stream, eof, binary]),
    TRef = erlang:start_timer(?CMD_TIMEOUT, self(),
			      timeout),
    recv_data(Port, TRef, <<>>).

-spec recv_data(port(), reference(), binary()) -> {ok, binary()} | {error, image_error()}.
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

-spec return(port(), reference(), {ok, binary()} | {error, image_error()}) ->
		    {ok, binary()} | {error, image_error()}.
return(Port, TRef, Result) ->
    misc:cancel_timer(TRef),
    catch port_close(Port),
    Result.

is_feature_available() ->
    case get_prog_name() of
      PathOrModule when is_binary(PathOrModule) -> true;
      false -> false
    end.

check_captcha_setup() ->
    case is_feature_available() of
	true ->
	    case create_image() of
		{ok, _, _, _} ->
		    true;
		Err ->
		    ?CRITICAL_MSG("Captcha is enabled in the option captcha_cmd, "
				  "but it can't generate images.",
				  []),
		    Err
	    end;
	false ->
	    false
    end.

-spec lookup_captcha(binary()) -> {ok, #captcha{}} | {error, enoent}.
lookup_captcha(Id) ->
    case ets:lookup(captcha, Id) of
	[C] -> {ok, C};
	[] -> {error, enoent}
    end.

-spec check_captcha(binary(), binary()) -> captcha_not_found |
                                           captcha_valid |
                                           captcha_non_valid.

check_captcha(Id, ProvidedKey) ->
    case lookup_captcha(Id) of
	{ok, #captcha{pid = Pid, args = Args, key = ValidKey, tref = Tref}} ->
	    ets:delete(captcha, Id),
	    misc:cancel_timer(Tref),
	    if ValidKey == ProvidedKey ->
		    callback(captcha_succeed, Pid, Args),
		    captcha_valid;
	       true ->
		    callback(captcha_failed, Pid, Args),
		    captcha_non_valid
	    end;
	{error, _} ->
	    captcha_not_found
    end.

-spec clean_treap(treap:treap(), priority()) -> treap:treap().
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

-spec callback(captcha_succeed | captcha_failed,
	       pid() | undefined,
	       callback() | term()) -> any().
callback(Result, _Pid, F) when is_function(F) ->
    F(Result);
callback(Result, Pid, Args) when is_pid(Pid) ->
    Pid ! {Result, Args};
callback(_, _, _) ->
    ok.

-spec now_priority() -> priority().
now_priority() ->
    -erlang:system_time(microsecond).
