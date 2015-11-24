%%%-------------------------------------------------------------------
%%% File    : mod_register_web.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Web page to register account and related tasks
%%% Created :  4 May 2008 by Badlop <badlop@process-one.net>
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
%%%----------------------------------------------------------------------

%%% IDEAS:
%%%
%%% * Implement those options, already present in mod_register:
%%%   + access
%%%   + captcha_protected
%%%   + password_strength
%%%   + welcome_message
%%%   + registration_timeout
%%%
%%% * Improve this module to allow each virtual host to have different
%%%   options. See http://support.process-one.net/browse/EJAB-561
%%%
%%% * Check that all the text is translatable.
%%%
%%% * Add option to use a custom CSS file, or custom CSS lines.
%%%
%%% * Don't hardcode the "register" path in URL.
%%%
%%% * Allow private email during register, and store in custom table.
%%% * Optionally require private email to register.
%%% * Optionally require email confirmation to register.
%%% * Allow to set a private email address anytime.
%%% * Allow to recover password using private email to confirm (mod_passrecover)
%%% * Optionally require invitation
%%% * Optionally register request is forwarded to admin, no account created.

-module(mod_register_web).

-author('badlop@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process/2, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    %% case gen_mod:get_opt(docroot, Opts, fun(A) -> A end, undefined) of
    ok.

stop(_Host) -> ok.

%%%----------------------------------------------------------------------
%%% HTTP handlers
%%%----------------------------------------------------------------------

process([], #request{method = 'GET', lang = Lang}) ->
    index_page(Lang);
process([<<"register.css">>],
	#request{method = 'GET'}) ->
    serve_css();
process([<<"new">>],
	#request{method = 'GET', lang = Lang, host = Host,
		 ip = IP}) ->
    {Addr, _Port} = IP, form_new_get(Host, Lang, Addr);
process([<<"delete">>],
	#request{method = 'GET', lang = Lang, host = Host}) ->
    form_del_get(Host, Lang);
process([<<"change_password">>],
	#request{method = 'GET', lang = Lang, host = Host}) ->
    form_changepass_get(Host, Lang);
process([<<"new">>],
	#request{method = 'POST', q = Q, ip = {Ip, _Port},
		 lang = Lang, host = _HTTPHost}) ->
    case form_new_post(Q) of
      {success, ok, {Username, Host, _Password}} ->
	  Jid = jid:make(Username, Host, <<"">>),
          mod_register:send_registration_notifications(?MODULE, Jid, Ip),
	  Text = (?T(<<"Your Jabber account was successfully "
		       "created.">>)),
	  {200, [], Text};
      Error ->
	  ErrorText =
                list_to_binary([?T(<<"There was an error creating the account: ">>),
                                ?T(get_error_text(Error))]),
	  {404, [], ErrorText}
    end;
process([<<"delete">>],
	#request{method = 'POST', q = Q, lang = Lang,
		 host = Host}) ->
    case form_del_post(Q, Host) of
      {atomic, ok} ->
	  Text = (?T(<<"Your Jabber account was successfully "
		       "deleted.">>)),
	  {200, [], Text};
      Error ->
	  ErrorText =
                list_to_binary([?T(<<"There was an error deleting the account: ">>),
                                ?T(get_error_text(Error))]),
	  {404, [], ErrorText}
    end;
%% TODO: Currently only the first vhost is usable. The web request record
%% should include the host where the POST was sent.
process([<<"change_password">>],
	#request{method = 'POST', q = Q, lang = Lang,
		 host = Host}) ->
    case form_changepass_post(Q, Host) of
      {atomic, ok} ->
	  Text = (?T(<<"The password of your Jabber account "
		       "was successfully changed.">>)),
	  {200, [], Text};
      Error ->
	  ErrorText =
                list_to_binary([?T(<<"There was an error changing the password: ">>),
                                ?T(get_error_text(Error))]),
	  {404, [], ErrorText}
    end;

process(_Path, _Request) ->
    {404, [], "Not Found"}.

%%%----------------------------------------------------------------------
%%% CSS
%%%----------------------------------------------------------------------

serve_css() ->
    {200,
     [{<<"Content-Type">>, <<"text/css">>}, last_modified(),
      cache_control_public()],
     css()}.

last_modified() ->
    {<<"Last-Modified">>,
     <<"Mon, 25 Feb 2008 13:23:30 GMT">>}.

cache_control_public() ->
    {<<"Cache-Control">>, <<"public">>}.

css() ->
    <<"html,body {\nbackground: white;\nmargin: "
      "0;\npadding: 0;\nheight: 100%;\n}">>.

%%%----------------------------------------------------------------------
%%% Index page
%%%----------------------------------------------------------------------

index_page(Lang) ->
    HeadEls = [?XCT(<<"title">>,
		    <<"Jabber Account Registration">>),
	       ?XA(<<"link">>,
		   [{<<"href">>, <<"/register/register.css">>},
		    {<<"type">>, <<"text/css">>},
		    {<<"rel">>, <<"stylesheet">>}])],
    Els = [?XACT(<<"h1">>,
		 [{<<"class">>, <<"title">>},
		  {<<"style">>, <<"text-align:center;">>}],
		 <<"Jabber Account Registration">>),
	   ?XE(<<"ul">>,
	       [?XE(<<"li">>,
		    [?ACT(<<"new">>, <<"Register a Jabber account">>)]),
		?XE(<<"li">>,
		    [?ACT(<<"change_password">>, <<"Change Password">>)]),
		?XE(<<"li">>,
		    [?ACT(<<"delete">>,
			  <<"Unregister a Jabber account">>)])])],
    {200,
     [{<<"Server">>, <<"ejabberd">>},
      {<<"Content-Type">>, <<"text/html">>}],
     ejabberd_web:make_xhtml(HeadEls, Els)}.

%%%----------------------------------------------------------------------
%%% Formulary new account GET
%%%----------------------------------------------------------------------

form_new_get(Host, Lang, IP) ->
    CaptchaEls = build_captcha_li_list(Lang, IP),
    HeadEls = [?XCT(<<"title">>,
		    <<"Register a Jabber account">>),
	       ?XA(<<"link">>,
		   [{<<"href">>, <<"/register/register.css">>},
		    {<<"type">>, <<"text/css">>},
		    {<<"rel">>, <<"stylesheet">>}])],
    Els = [?XACT(<<"h1">>,
		 [{<<"class">>, <<"title">>},
		  {<<"style">>, <<"text-align:center;">>}],
		 <<"Register a Jabber account">>),
	   ?XCT(<<"p">>,
		<<"This page allows to create a Jabber "
		  "account in this Jabber server. Your "
		  "JID (Jabber IDentifier) will be of the "
		  "form: username@server. Please read carefully "
		  "the instructions to fill correctly the "
		  "fields.">>),
	   ?XAE(<<"form">>,
		[{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
		[?XE(<<"ol">>,
		     ([?XE(<<"li">>,
			   [?CT(<<"Username:">>), ?C(<<" ">>),
			    ?INPUTS(<<"text">>, <<"username">>, <<"">>,
				    <<"20">>),
			    ?BR,
			    ?XE(<<"ul">>,
				[?XCT(<<"li">>,
				      <<"This is case insensitive: macbeth is "
					"the same that MacBeth and Macbeth.">>),
				 ?XC(<<"li">>,
				     <<(?T(<<"Characters not allowed:">>))/binary,
				       " \" & ' / : < > @ ">>)])]),
		       ?XE(<<"li">>,
			   [?CT(<<"Server:">>), ?C(<<" ">>),
			    ?INPUTS(<<"text">>, <<"host">>, Host, <<"20">>)]),
		       ?XE(<<"li">>,
			   [?CT(<<"Password:">>), ?C(<<" ">>),
			    ?INPUTS(<<"password">>, <<"password">>, <<"">>,
				    <<"20">>),
			    ?BR,
			    ?XE(<<"ul">>,
				[?XCT(<<"li">>,
				      <<"Don't tell your password to anybody, "
					"not even the administrators of the Jabber "
					"server.">>),
				 ?XCT(<<"li">>,
				      <<"You can later change your password using "
					"a Jabber client.">>),
				 ?XCT(<<"li">>,
				      <<"Some Jabber clients can store your password "
					"in your computer. Use that feature only "
					"if you trust your computer is safe.">>),
				 ?XCT(<<"li">>,
				      <<"Memorize your password, or write it "
					"in a paper placed in a safe place. In "
					"Jabber there isn't an automated way "
					"to recover your password if you forget "
					"it.">>)])]),
		       ?XE(<<"li">>,
			   [?CT(<<"Password Verification:">>), ?C(<<" ">>),
			    ?INPUTS(<<"password">>, <<"password2">>, <<"">>,
				    <<"20">>)])]
			++
			CaptchaEls ++
			  [?XE(<<"li">>,
			       [?INPUTT(<<"submit">>, <<"register">>,
					<<"Register">>)])]))])],
    {200,
     [{<<"Server">>, <<"ejabberd">>},
      {<<"Content-Type">>, <<"text/html">>}],
     ejabberd_web:make_xhtml(HeadEls, Els)}.

%% Copied from mod_register.erl
%% Function copied from ejabberd_logger_h.erl and customized
%%%----------------------------------------------------------------------
%%% Formulary new POST
%%%----------------------------------------------------------------------

form_new_post(Q) ->
    case catch get_register_parameters(Q) of
      [Username, Host, Password, Password, Id, Key] ->
	  form_new_post(Username, Host, Password, {Id, Key});
      [_Username, _Password, _Password2, false, false] ->
	  {error, passwords_not_identical};
      [_Username, _Password, _Password2, Id, Key] ->
	  ejabberd_captcha:check_captcha(Id, Key),
	  {error, passwords_not_identical};
      _ -> {error, wrong_parameters}
    end.

get_register_parameters(Q) ->
    lists:map(fun (Key) ->
		      case lists:keysearch(Key, 1, Q) of
			{value, {_Key, Value}} -> Value;
			false -> false
		      end
	      end,
	      [<<"username">>, <<"host">>, <<"password">>, <<"password2">>,
	       <<"id">>, <<"key">>]).

form_new_post(Username, Host, Password,
	      {false, false}) ->
    register_account(Username, Host, Password);
form_new_post(Username, Host, Password, {Id, Key}) ->
    case ejabberd_captcha:check_captcha(Id, Key) of
      captcha_valid ->
	  register_account(Username, Host, Password);
      captcha_non_valid -> {error, captcha_non_valid};
      captcha_not_found -> {error, captcha_non_valid}
    end.

%%%----------------------------------------------------------------------
%%% Formulary Captcha support for new GET/POST
%%%----------------------------------------------------------------------

build_captcha_li_list(Lang, IP) ->
    case ejabberd_captcha:is_feature_available() of
      true -> build_captcha_li_list2(Lang, IP);
      false -> []
    end.

build_captcha_li_list2(Lang, IP) ->
    SID = <<"">>,
    From = #jid{user = <<"">>, server = <<"test">>,
		resource = <<"">>},
    To = #jid{user = <<"">>, server = <<"test">>,
	      resource = <<"">>},
    Args = [],
    case ejabberd_captcha:create_captcha(SID, From, To,
					 Lang, IP, Args)
	of
      {ok, Id, _} ->
	  {_, {CImg, CText, CId, CKey}} =
	      ejabberd_captcha:build_captcha_html(Id, Lang),
	  [?XE(<<"li">>,
	       [CText, ?C(<<" ">>), CId, CKey, ?BR, CImg])];
      _ -> []
    end.

%%%----------------------------------------------------------------------
%%% Formulary change password GET
%%%----------------------------------------------------------------------

form_changepass_get(Host, Lang) ->
    HeadEls = [?XCT(<<"title">>, <<"Change Password">>),
	       ?XA(<<"link">>,
		   [{<<"href">>, <<"/register/register.css">>},
		    {<<"type">>, <<"text/css">>},
		    {<<"rel">>, <<"stylesheet">>}])],
    Els = [?XACT(<<"h1">>,
		 [{<<"class">>, <<"title">>},
		  {<<"style">>, <<"text-align:center;">>}],
		 <<"Change Password">>),
	   ?XAE(<<"form">>,
		[{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
		[?XE(<<"ol">>,
		     [?XE(<<"li">>,
			  [?CT(<<"Username:">>), ?C(<<" ">>),
			   ?INPUTS(<<"text">>, <<"username">>, <<"">>,
				   <<"20">>)]),
		      ?XE(<<"li">>,
			  [?CT(<<"Server:">>), ?C(<<" ">>), ?C(Host)]),
		      ?XE(<<"li">>,
			  [?CT(<<"Old Password:">>), ?C(<<" ">>),
			   ?INPUTS(<<"password">>, <<"passwordold">>, <<"">>,
				   <<"20">>)]),
		      ?XE(<<"li">>,
			  [?CT(<<"New Password:">>), ?C(<<" ">>),
			   ?INPUTS(<<"password">>, <<"password">>, <<"">>,
				   <<"20">>)]),
		      ?XE(<<"li">>,
			  [?CT(<<"Password Verification:">>), ?C(<<" ">>),
			   ?INPUTS(<<"password">>, <<"password2">>, <<"">>,
				   <<"20">>)]),
		      ?XE(<<"li">>,
			  [?INPUTT(<<"submit">>, <<"changepass">>,
				   <<"Change Password">>)])])])],
    {200,
     [{<<"Server">>, <<"ejabberd">>},
      {<<"Content-Type">>, <<"text/html">>}],
     ejabberd_web:make_xhtml(HeadEls, Els)}.

%%%----------------------------------------------------------------------
%%% Formulary change password POST
%%%----------------------------------------------------------------------

form_changepass_post(Q, Host) ->
    case catch get_changepass_parameters(Q) of
      [Username, PasswordOld, Password, Password] ->
	  try_change_password(Username, Host, PasswordOld,
			      Password);
      [_Username, _PasswordOld, _Password, _Password2] ->
	  {error, passwords_not_identical};
      _ -> {error, wrong_parameters}
    end.

get_changepass_parameters(Q) ->
%% @spec(Username,Host,PasswordOld,Password) -> {atomic, ok} |
%%                                              {error, account_doesnt_exist} |
%%                                              {error, password_not_changed} |
%%                                              {error, password_incorrect}
    lists:map(fun (Key) ->
		      {value, {_Key, Value}} = lists:keysearch(Key, 1, Q),
		      Value
	      end,
	      [<<"username">>, <<"passwordold">>, <<"password">>,
	       <<"password2">>]).

try_change_password(Username, Host, PasswordOld,
		    Password) ->
    try change_password(Username, Host, PasswordOld,
			Password)
    of
      {atomic, ok} -> {atomic, ok}
    catch
      error:{badmatch, Error} -> {error, Error}
    end.

change_password(Username, Host, PasswordOld,
		Password) ->
    account_exists = check_account_exists(Username, Host),
    password_correct = check_password(Username, Host,
				      PasswordOld),
    ok = ejabberd_auth:set_password(Username, Host,
				    Password),
    case check_password(Username, Host, Password) of
      password_correct -> {atomic, ok};
      password_incorrect -> {error, password_not_changed}
    end.

check_account_exists(Username, Host) ->
    case ejabberd_auth:is_user_exists(Username, Host) of
      true -> account_exists;
      false -> account_doesnt_exist
    end.

check_password(Username, Host, Password) ->
    case ejabberd_auth:check_password(Username, Host,
				      Password)
	of
      true -> password_correct;
      false -> password_incorrect
    end.

%%%----------------------------------------------------------------------
%%% Formulary delete account GET
%%%----------------------------------------------------------------------

form_del_get(Host, Lang) ->
    HeadEls = [?XCT(<<"title">>,
		    <<"Unregister a Jabber account">>),
	       ?XA(<<"link">>,
		   [{<<"href">>, <<"/register/register.css">>},
		    {<<"type">>, <<"text/css">>},
		    {<<"rel">>, <<"stylesheet">>}])],
    Els = [?XACT(<<"h1">>,
		 [{<<"class">>, <<"title">>},
		  {<<"style">>, <<"text-align:center;">>}],
		 <<"Unregister a Jabber account">>),
	   ?XCT(<<"p">>,
		<<"This page allows to unregister a Jabber "
		  "account in this Jabber server.">>),
	   ?XAE(<<"form">>,
		[{<<"action">>, <<"">>}, {<<"method">>, <<"post">>}],
		[?XE(<<"ol">>,
		     [?XE(<<"li">>,
			  [?CT(<<"Username:">>), ?C(<<" ">>),
			   ?INPUTS(<<"text">>, <<"username">>, <<"">>,
				   <<"20">>)]),
		      ?XE(<<"li">>,
			  [?CT(<<"Server:">>), ?C(<<" ">>), ?C(Host)]),
		      ?XE(<<"li">>,
			  [?CT(<<"Password:">>), ?C(<<" ">>),
			   ?INPUTS(<<"password">>, <<"password">>, <<"">>,
				   <<"20">>)]),
		      ?XE(<<"li">>,
			  [?INPUTT(<<"submit">>, <<"unregister">>,
				   <<"Unregister">>)])])])],
    {200,
     [{<<"Server">>, <<"ejabberd">>},
      {<<"Content-Type">>, <<"text/html">>}],
     ejabberd_web:make_xhtml(HeadEls, Els)}.

%% @spec(Username, Host, Password) -> {success, ok, {Username, Host, Password} |
%%                                    {success, exists, {Username, Host, Password}} |
%%                                    {error, not_allowed} |
%%                                    {error, invalid_jid}
register_account(Username, Host, Password) ->
    Access = gen_mod:get_module_opt(Host, mod_register, access,
                                    fun(A) when is_atom(A) -> A end,
                                    all),
    case jid:make(Username, Host, <<"">>) of
      error -> {error, invalid_jid};
      JID ->
        case acl:match_rule(Host, Access, JID) of
          deny -> {error, not_allowed};
          allow -> register_account2(Username, Host, Password)
        end
    end.

register_account2(Username, Host, Password) ->
    case ejabberd_auth:try_register(Username, Host,
				    Password)
	of
      {atomic, Res} ->
	  {success, Res, {Username, Host, Password}};
      Other -> Other
    end.

%%%----------------------------------------------------------------------
%%% Formulary delete POST
%%%----------------------------------------------------------------------

form_del_post(Q, Host) ->
    case catch get_unregister_parameters(Q) of
      [Username, Password] ->
	  try_unregister_account(Username, Host, Password);
      _ -> {error, wrong_parameters}
    end.

get_unregister_parameters(Q) ->
%% @spec(Username, Host, Password) -> {atomic, ok} |
%%                                    {error, account_doesnt_exist} |
%%                                    {error, account_exists} |
%%                                    {error, password_incorrect}
    lists:map(fun (Key) ->
		      {value, {_Key, Value}} = lists:keysearch(Key, 1, Q),
		      Value
	      end,
	      [<<"username">>, <<"password">>]).

try_unregister_account(Username, Host, Password) ->
    try unregister_account(Username, Host, Password) of
      {atomic, ok} -> {atomic, ok}
    catch
      error:{badmatch, Error} -> {error, Error}
    end.

unregister_account(Username, Host, Password) ->
    account_exists = check_account_exists(Username, Host),
    password_correct = check_password(Username, Host,
				      Password),
    ok = ejabberd_auth:remove_user(Username, Host,
				   Password),
    account_doesnt_exist = check_account_exists(Username,
						Host),
    {atomic, ok}.

%%%----------------------------------------------------------------------
%%% Error texts
%%%----------------------------------------------------------------------

get_error_text({error, captcha_non_valid}) ->
    <<"The captcha you entered is wrong">>;
get_error_text({success, exists, _}) ->
    get_error_text({atomic, exists});
get_error_text({atomic, exists}) ->
    <<"The account already exists">>;
get_error_text({error, password_incorrect}) ->
    <<"Incorrect password">>;
get_error_text({error, invalid_jid}) ->
    <<"The username is not valid">>;
get_error_text({error, not_allowed}) ->
    <<"Not allowed">>;
get_error_text({error, account_doesnt_exist}) ->
    <<"Account doesn't exist">>;
get_error_text({error, account_exists}) ->
    <<"The account was not deleted">>;
get_error_text({error, password_not_changed}) ->
    <<"The password was not changed">>;
get_error_text({error, passwords_not_identical}) ->
    <<"The passwords are different">>;
get_error_text({error, wrong_parameters}) ->
    <<"Wrong parameters in the web formulary">>.

mod_opt_type(_) -> [].
