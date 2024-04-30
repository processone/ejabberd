%%%----------------------------------------------------------------------
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
-module(ejabberd_acme).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([default_directory_url/0]).
%% HTTP API
-export([process/2]).
%% Hooks
-export([ejabberd_started/0, register_certfiles/0, cert_expired/2]).
%% ejabberd commands
-export([get_commands_spec/0, request_certificate/1,
         revoke_certificate/1, list_certificates/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").
-include("ejabberd_commands.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(CALL_TIMEOUT, timer:minutes(10)).

-record(state, {}).

-type state() :: #state{}.
-type priv_key() :: public_key:private_key().
-type cert() :: #'OTPCertificate'{}.
-type cert_type() :: ec | rsa.
-type io_error() :: file:posix().
-type issue_result() :: ok | p1_acme:issue_return() |
			{error, {file, io_error()} |
			        {idna_failed, binary()}}.

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_certfiles() -> ok.
register_certfiles() ->
    lists:foreach(fun ejabberd_pkix:add_certfile/1,
		  list_certfiles()).

-spec process([binary()], _) -> {integer(), [{binary(), binary()}], binary()}.
process([Token], _) ->
    ?DEBUG("Received ACME challenge request for token: ~ts", [Token]),
    try ets:lookup_element(acme_challenge, Token, 2) of
	Key -> {200, [{<<"Content-Type">>,
		       <<"application/octet-stream">>}],
		Key}
    catch _:_ ->
	    {404, [], <<>>}
    end;
process(_, _) ->
    {404, [], <<>>}.

-spec cert_expired(_, pkix:cert_info()) -> ok | stop.
cert_expired(_, #{domains := Domains, files := Files}) ->
    CertFiles = list_certfiles(),
    case lists:any(
	   fun({File, _}) ->
		   lists:member(File, CertFiles)
	   end, Files) of
	true ->
	    gen_server:cast(?MODULE, {request, Domains}),
	    stop;
	false ->
	    ok
    end.

-spec ejabberd_started() -> ok.
ejabberd_started() ->
    gen_server:cast(?MODULE, ejabberd_started).

default_directory_url() ->
    <<"https://acme-v02.api.letsencrypt.org/directory">>.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(acme_challenge, [named_table, public]),
    process_flag(trap_exit, true),
    ejabberd:start_app(p1_acme),
    delete_obsolete_data(),
    ejabberd_hooks:add(cert_expired, ?MODULE, cert_expired, 60),
    ejabberd_hooks:add(config_reloaded, ?MODULE, register_certfiles, 40),
    ejabberd_hooks:add(ejabberd_started, ?MODULE, ejabberd_started, 110),
    ejabberd_hooks:add(config_reloaded, ?MODULE, ejabberd_started, 110),
    ejabberd_commands:register_commands(get_commands_spec()),
    register_certfiles(),
    {ok, #state{}}.

handle_call({request, [_|_] = Domains}, _From, State) ->
    ?INFO_MSG("Requesting new certificate for ~ts from ~ts",
	      [misc:format_hosts_list(Domains), directory_url()]),
    {Ret, State1} = issue_request(State, Domains),
    {reply, Ret, State1};
handle_call({revoke, Cert, Key, Path}, _From, State) ->
    ?INFO_MSG("Revoking certificate from file ~ts", [Path]),
    {Ret, State1} = revoke_request(State, Cert, Key, Path),
    {reply, Ret, State1};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(ejabberd_started, State) ->
    case request_on_start() of
	{true, Domains} ->
	    ?INFO_MSG("Requesting new certificate for ~ts from ~ts",
		      [misc:format_hosts_list(Domains), directory_url()]),
	    {_, State1} = issue_request(State, Domains),
	    {noreply, State1};
	false ->
	    {noreply, State}
    end;
handle_cast({request, [_|_] = Domains}, State) ->
    ?INFO_MSG("Requesting renewal of certificate for ~ts from ~ts",
	      [misc:format_hosts_list(Domains), directory_url()]),
    {_, State1} = issue_request(State, Domains),
    {noreply, State1};
handle_cast(Request, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_hooks:delete(cert_expired, ?MODULE, cert_expired, 60),
    ejabberd_hooks:delete(config_reloaded, ?MODULE, register_certfiles, 40),
    ejabberd_hooks:delete(ejabberd_started, ?MODULE, ejabberd_started, 110),
    ejabberd_hooks:delete(config_reloaded, ?MODULE, ejabberd_started, 110),
    ejabberd_commands:unregister_commands(get_commands_spec()).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
%%% Challenge callback
%%%===================================================================
-spec register_challenge(p1_acme:challenge_data(), reference()) -> true.
register_challenge(Auth, Ref) ->
    ?DEBUG("Registering ACME challenge ~p -> ~p", [Ref, Auth]),
    ejabberd_hooks:run(acme_challenge, [{start, Auth, Ref}]),
    ets:insert(
      acme_challenge,
      lists:map(
	fun(#{token := Token, key := Key}) ->
		{Token, Key, Ref}
	end, Auth)).

-spec unregister_challenge(reference()) -> non_neg_integer().
unregister_challenge(Ref) ->
    ?DEBUG("Unregistering ACME challenge ~p", [Ref]),
    ejabberd_hooks:run(acme_challenge, [{stop, Ref}]),
    ets:select_delete(
      acme_challenge,
      ets:fun2ms(
	fun({_, _, Ref1}) ->
		Ref1 == Ref
	end)).

%%%===================================================================
%%% Issuance
%%%===================================================================
-spec issue_request(state(), [binary(),...]) -> {issue_result(), state()}.
issue_request(State, Domains) ->
    case check_idna(Domains) of
	{ok, AsciiDomains} ->
	    case read_account_key() of
		{ok, AccKey} ->
		    Config = ejabberd_option:acme(),
		    DirURL = maps:get(ca_url, Config, default_directory_url()),
		    Contact = maps:get(contact, Config, []),
		    CertType = maps:get(cert_type, Config, rsa),
		    issue_request(State, DirURL, Domains, AsciiDomains, AccKey, CertType, Contact);
		{error, Reason} = Err ->
		    ?ERROR_MSG("Failed to request certificate for ~ts: ~ts",
			       [misc:format_hosts_list(Domains),
				format_error(Reason)]),
		    {Err, State}
	    end;
	{error, Reason} = Err ->
	    ?ERROR_MSG("Failed to request certificate for ~ts: ~ts",
		       [misc:format_hosts_list(Domains),
			format_error(Reason)]),
	    {Err, State}
    end.

-spec issue_request(state(), binary(), [binary(),...], [string(), ...], priv_key(),
		    cert_type(), [binary()]) -> {issue_result(), state()}.
issue_request(State, DirURL, Domains, AsciiDomains, AccKey, CertType, Contact) ->
    Ref = make_ref(),
    ChallengeFun = fun(Auth) -> register_challenge(Auth, Ref) end,
    Ret = case p1_acme:issue(DirURL, AsciiDomains, AccKey,
			  [{cert_type, CertType},
			   {contact, Contact},
			   {debug_fun, debug_fun()},
			   {challenge_fun, ChallengeFun}]) of
	      {ok, #{cert_key := CertKey,
		     cert_chain := Certs}} ->
		  case store_cert(CertKey, Certs, CertType, Domains) of
		      {ok, Path} ->
			  ejabberd_pkix:add_certfile(Path),
			  ejabberd_pkix:commit(),
			  ?INFO_MSG("Certificate for ~ts has been received, "
				    "stored and loaded successfully",
				    [misc:format_hosts_list(Domains)]),
			  {ok, State};
		      {error, Reason} = Err ->
			  ?ERROR_MSG("Failed to store certificate for ~ts: ~ts",
				     [misc:format_hosts_list(Domains),
				      format_error(Reason)]),
			  {Err, State}
		  end;
	      {error, Reason} = Err ->
		  ?ERROR_MSG("Failed to request certificate for ~ts: ~ts",
			     [misc:format_hosts_list(Domains),
			      format_error(Reason)]),
		  {Err, State}
	  end,
    unregister_challenge(Ref),
    Ret.

%%%===================================================================
%%% Revocation
%%%===================================================================
revoke_request(State, Cert, Key, Path) ->
    case p1_acme:revoke(directory_url(), Cert, Key,
		     [{debug_fun, debug_fun()}]) of
	ok ->
	    ?INFO_MSG("Certificate from file ~ts has been "
		      "revoked successfully", [Path]),
	    case delete_file(Path) of
		ok ->
		    ejabberd_pkix:del_certfile(Path),
		    ejabberd_pkix:commit(),
		    {ok, State};
		Err ->
		    {Err, State}
	    end;
	{error, Reason} = Err ->
	    ?ERROR_MSG("Failed to revoke certificate from file ~ts: ~ts",
		       [Path, format_error(Reason)]),
	    {Err, State}
    end.

%%%===================================================================
%%% File management
%%%===================================================================
-spec acme_dir() -> file:filename_all().
acme_dir() ->
    MnesiaDir = mnesia:system_info(directory),
    filename:join(MnesiaDir, "acme").

-spec acme_certs_dir(atom()) -> file:filename_all().
acme_certs_dir(Tag) ->
    filename:join(acme_dir(), Tag).

-spec account_file() -> file:filename_all().
account_file() ->
    filename:join(acme_dir(), "account.key").

-spec cert_file(cert_type(), [binary()]) -> file:filename_all().
cert_file(CertType, Domains) ->
    L = [erlang:atom_to_binary(CertType, latin1)|Domains],
    Hash = str:sha(str:join(L, <<0>>)),
    filename:join(acme_certs_dir(live), Hash).

-spec prep_path(file:filename_all()) -> binary().
prep_path(Path) ->
    unicode:characters_to_binary(Path).

-spec list_certfiles() -> [binary()].
list_certfiles() ->
    filelib:fold_files(
      acme_certs_dir(live), "^[0-9a-f]{40}$", false,
      fun(F, Fs) -> [prep_path(F)|Fs] end, []).

-spec read_account_key() -> {ok, #'ECPrivateKey'{}} | {error, {file, io_error()}}.
read_account_key() ->
    Path = account_file(),
    case pkix:read_file(Path) of
	{ok, _, KeyMap} ->
	    case maps:keys(KeyMap) of
		[#'ECPrivateKey'{} = Key|_] -> {ok, Key};
		_ ->
		    ?WARNING_MSG("File ~ts doesn't contain ACME account key. "
				 "Trying to create a new one...",
				 [Path]),
		    create_account_key()
	    end;
	{error, enoent} ->
	    create_account_key();
	{error, {bad_cert, _, _} = Reason} ->
	    ?WARNING_MSG("ACME account key from '~ts' is corrupted: ~ts. "
			 "Trying to create a new one...",
			 [Path, pkix:format_error(Reason)]),
	    create_account_key();
	{error, Reason} ->
	    ?ERROR_MSG("Failed to read ACME account from ~ts: ~ts. "
		       "Try to fix permissions or delete the file completely",
		       [Path, pkix:format_error(Reason)]),
	    {error, {file, Reason}}
    end.

-spec create_account_key() -> {ok, #'ECPrivateKey'{}} | {error, {file, io_error()}}.
create_account_key() ->
    Path = account_file(),
    ?DEBUG("Creating ACME account key in ~ts", [Path]),
    Key = p1_acme:generate_key(ec),
    DER = public_key:der_encode(element(1, Key), Key),
    PEM = public_key:pem_encode([{element(1, Key), DER, not_encrypted}]),
    case write_file(Path, PEM) of
	ok ->
	    ?DEBUG("ACME account key has been created successfully in ~ts",
		   [Path]),
	    {ok, Key};
	{error, Reason} ->
	    {error, {file, Reason}}
    end.

-spec store_cert(priv_key(), [cert()], cert_type(), [binary()]) -> {ok, file:filename_all()} |
								   {error, {file, io_error()}}.
store_cert(Key, Chain, CertType, Domains) ->
    DerKey = public_key:der_encode(element(1, Key), Key),
    PemKey = [{element(1, Key), DerKey, not_encrypted}],
    PemChain = lists:map(
		 fun(Cert) ->
			 DerCert = public_key:pkix_encode(
				     element(1, Cert), Cert, otp),
			 {'Certificate', DerCert, not_encrypted}
		 end, Chain),
    PEM = public_key:pem_encode(PemChain ++ PemKey),
    Path = cert_file(CertType, Domains),
    ?DEBUG("Storing certificate for ~ts in ~ts",
	   [misc:format_hosts_list(Domains), Path]),
    case write_file(Path, PEM) of
	ok ->
	    {ok, Path};
	{error, Reason} ->
	    {error, {file, Reason}}
    end.

-spec read_cert(file:filename_all()) -> {ok, [cert()], priv_key()} |
					{error, {file, io_error()} |
					        {bad_cert, _, _} |
					        unexpected_certfile}.
read_cert(Path) ->
    ?DEBUG("Reading certificate from ~ts", [Path]),
    case pkix:read_file(Path) of
	{ok, CertsMap, KeysMap} ->
	    case {maps:to_list(CertsMap), maps:keys(KeysMap)} of
		{[_|_] = Certs, [CertKey]} ->
		    {ok, [Cert || {Cert, _} <- lists:keysort(2, Certs)], CertKey};
		_ ->
		    {error, unexpected_certfile}
	    end;
	{error, Why} when is_atom(Why) ->
	    {error, {file, Why}};
	{error, _} = Err ->
	    Err
    end.

-spec write_file(file:filename_all(), iodata()) -> ok | {error, io_error()}.
write_file(Path, Data) ->
    case ensure_dir(Path) of
	ok ->
	    case file:write_file(Path, Data) of
		ok ->
		    case file:change_mode(Path, 8#600) of
			ok -> ok;
			{error, Why} ->
			    ?WARNING_MSG("Failed to change permissions of ~ts: ~ts",
					 [Path, file:format_error(Why)])
		    end;
		{error, Why} = Err ->
		    ?ERROR_MSG("Failed to write file ~ts: ~ts",
			       [Path, file:format_error(Why)]),
		    Err
	    end;
	Err ->
	    Err
    end.

-spec delete_file(file:filename_all()) -> ok | {error, io_error()}.
delete_file(Path) ->
    case file:delete(Path) of
	ok -> ok;
	{error, Why} = Err ->
	    ?WARNING_MSG("Failed to delete file ~ts: ~ts",
			 [Path, file:format_error(Why)]),
	    Err
    end.

-spec ensure_dir(file:filename_all()) -> ok | {error, io_error()}.
ensure_dir(Path) ->
    case filelib:ensure_dir(Path) of
	ok -> ok;
	{error, Why} = Err ->
	    ?ERROR_MSG("Failed to create directory ~ts: ~ts",
		       [filename:dirname(Path),
			file:format_error(Why)]),
	    Err
    end.

-spec delete_obsolete_data() -> ok.
delete_obsolete_data() ->
    Path = filename:join(ejabberd_pkix:certs_dir(), "acme"),
    case filelib:is_dir(Path) of
	true ->
	    ?INFO_MSG("Deleting obsolete directory ~ts", [Path]),
	    _ = misc:delete_dir(Path),
	    ok;
	false ->
	    ok
    end.

%%%===================================================================
%%% ejabberd commands
%%%===================================================================
get_commands_spec() ->
    [#ejabberd_commands{name = request_certificate, tags = [acme],
			desc = "Requests certificates for all or some domains",
			longdesc = "Domains can be `all`, or a list of domains separared with comma characters",
			module = ?MODULE, function = request_certificate,
			args_desc = ["Domains for which to acquire a certificate"],
			args_example = ["example.com,domain.tld,conference.domain.tld"],
			args = [{domains, string}],
			result = {res, restuple}},
     #ejabberd_commands{name = list_certificates, tags = [acme],
			desc = "Lists all ACME certificates",
			module = ?MODULE, function = list_certificates,
			args = [],
			result = {certificates,
				  {list, {certificate,
					  {tuple, [{domain, string},
						   {file, string},
						   {used, string}]}}}}},
     #ejabberd_commands{name = revoke_certificate, tags = [acme],
			desc = "Revokes the selected ACME certificate",
			module = ?MODULE, function = revoke_certificate,
			args_desc = ["Filename of the certificate"],
			args = [{file, string}],
			result = {res, restuple}}].

-spec request_certificate(iodata()) -> {ok | error, string()}.
request_certificate(Arg) ->
    Ret = case lists:filter(
		 fun(S) -> S /= <<>> end,
		 re:split(Arg, "[\\h,;]+", [{return, binary}])) of
	      [<<"all">>] ->
		  case auto_domains() of
		      [] -> {error, no_auto_hosts};
		      Domains ->
			  gen_server:call(?MODULE, {request, Domains}, ?CALL_TIMEOUT)
		  end;
	      [_|_] = Domains ->
		  case lists:dropwhile(
			 fun(D) ->
				 try ejabberd_router:is_my_route(D) of
				     true -> not is_ip_or_localhost(D);
				     false -> false
				 catch _:{invalid_domain, _} -> false
				 end
			 end, Domains) of
		      [Bad|_] ->
			  {error, {invalid_host, Bad}};
		      [] ->
			  gen_server:call(?MODULE, {request, Domains}, ?CALL_TIMEOUT)
		  end;
	      [] ->
		  {error, invalid_argument}
	  end,
    case Ret of
	ok -> {ok, ""};
	{error, Why} -> {error, format_error(Why)}
    end.

-spec revoke_certificate(iodata()) -> {ok | error, string()}.
revoke_certificate(Path0) ->
    Path = prep_path(Path0),
    Ret = case read_cert(Path) of
	      {ok, [Cert|_], Key} ->
		  gen_server:call(?MODULE, {revoke, Cert, Key, Path}, ?CALL_TIMEOUT);
	      {error, _} = Err ->
		  Err
	  end,
    case Ret of
	ok -> {ok, ""};
	{error, Reason} -> {error, format_error(Reason)}
    end.

-spec list_certificates() -> [{binary(), binary(), boolean()}].
list_certificates() ->
    Known = lists:flatmap(
	      fun(Path) ->
		      try
			  {ok, [Cert|_], _} = read_cert(Path),
			  Domains = pkix:extract_domains(Cert),
			  [{Domain, Path} || Domain <- Domains]
		      catch _:{badmatch, _} ->
			      []
		      end
	      end, list_certfiles()),
    Used = lists:foldl(
	     fun(Domain, S) ->
		     try
			 {ok, Path} = ejabberd_pkix:get_certfile_no_default(Domain),
			 {ok, [Cert|_], _} = read_cert(Path),
			 {ok, #{files := Files}} = pkix:get_cert_info(Cert),
			 lists:foldl(fun sets:add_element/2,
				     S, [{Domain, File} || {File, _} <- Files])
		     catch _:{badmatch, _} ->
			     S
		     end
	     end, sets:new(), all_domains()),
    lists:sort(
      lists:map(
	fun({Domain, Path} = E) ->
		{Domain, Path, sets:is_element(E, Used)}
	end, Known)).

%%%===================================================================
%%% Other stuff
%%%===================================================================
-spec all_domains() -> [binary(),...].
all_domains() ->
    ejabberd_option:hosts() ++ ejabberd_router:get_all_routes().

-spec auto_domains() -> [binary()].
auto_domains() ->
    lists:filter(
      fun(Host) ->
	      not is_ip_or_localhost(Host)
      end, all_domains()).

-spec directory_url() -> binary().
directory_url() ->
    maps:get(ca_url, ejabberd_option:acme(), default_directory_url()).

-spec debug_fun() -> fun((string(), list()) -> ok).
debug_fun() ->
    fun(Fmt, Args) -> ?DEBUG(Fmt, Args) end.

-spec request_on_start() -> false | {true, [binary()]}.
request_on_start() ->
    Config = ejabberd_option:acme(),
    case maps:get(auto, Config, true) of
	false -> false;
	true ->
	    case ejabberd_listener:tls_listeners() of
		[] -> false;
		_ ->
		    case lists:filter(
			   fun(Host) ->
				   not (have_cert_for_domain(Host)
					orelse is_ip_or_localhost(Host))
			   end, auto_domains()) of
			[] -> false;
			Hosts ->
			    case have_acme_listener() of
				true -> {true, Hosts};
				false ->
				    ?WARNING_MSG(
				       "No HTTP listeners for ACME challenges "
				       "are configured, automatic "
				       "certificate requests are aborted. Hint: "
				       "configure the listener and restart/reload "
				       "ejabberd. Or set acme->auto option to "
				       "`false` to suppress this warning.",
				       []),
				    false
			    end
		    end
	    end
    end.

well_known() ->
    [<<".well-known">>, <<"acme-challenge">>].

-spec have_cert_for_domain(binary()) -> boolean().
have_cert_for_domain(Host) ->
    ejabberd_pkix:get_certfile_no_default(Host) /= error.

-spec is_ip_or_localhost(binary()) -> boolean().
is_ip_or_localhost(Host) ->
    Parts = binary:split(Host, <<".">>),
    TLD = binary_to_list(lists:last(Parts)),
    case inet:parse_address(TLD) of
	{ok, _} -> true;
	_ -> TLD == "localhost"
    end.

-spec have_acme_listener() -> boolean().
have_acme_listener() ->
    lists:any(
      fun({_, ejabberd_http, #{tls := false,
			       request_handlers := Handlers}}) ->
	      lists:keymember(well_known(), 1, Handlers);
	 (_) ->
	      false
      end, ejabberd_option:listen()).

-spec check_idna([binary()]) -> {ok, [string()]} | {error, {idna_failed, binary()}}.
check_idna(Domains) ->
    lists:foldl(
      fun(D, {ok, Ds}) ->
	      try {ok, [idna:utf8_to_ascii(D)|Ds]}
	      catch _:_ -> {error, {idna_failed, D}}
	      end;
	 (_, Err) ->
	      Err
      end, {ok, []}, Domains).

-spec format_error(term()) -> string().
format_error({file, Reason}) ->
    "I/O error: " ++ file:format_error(Reason);
format_error({invalid_host, Domain}) ->
    "Unknown or unacceptable virtual host: " ++ binary_to_list(Domain);
format_error(no_auto_hosts) ->
    "You have no virtual hosts acceptable for ACME certification";
format_error(invalid_argument) ->
    "Invalid argument";
format_error(unexpected_certfile) ->
    "The certificate file was not obtained using ACME";
format_error({idna_failed, Domain}) ->
    "Not an IDN hostname: " ++ binary_to_list(Domain);
format_error({bad_cert, _, _} = Reason) ->
    "Malformed certificate file: " ++ pkix:format_error(Reason);
format_error(Reason) ->
    p1_acme:format_error(Reason).
