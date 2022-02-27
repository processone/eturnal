%%% eturnal STUN/TURN server.
%%%
%%% Copyright (c) 2020-2022 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2020-2022 ProcessOne, SARL.
%%% All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(eturnal).
-author('holger@zedat.fu-berlin.de').
-behaviour(gen_server).
-export([start/0,
         stop/0]).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([run_hook/2,
         get_password/2,
         get_opt/1,
         abort/1]).
-export_type([transport/0]).

-include_lib("kernel/include/logger.hrl").
-define(PEM_FILE_NAME, "cert.pem").

-record(eturnal_state,
        {listeners :: listeners(),
         modules :: modules()}).

-type transport() :: udp | tcp | tls | auto.
-type listeners() :: [{inet:ip_address(), inet:port_number(), transport()}].
-type modules() :: [module()].
-type option() :: atom().
-type value() :: term().
-type config_changes() :: {[{option(), value()}],
                           [{option(), value()}],
                           [option()]}.
-type state() :: #eturnal_state{}.

%% API: non-release startup and shutdown (used by test suite).

-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(eturnal) of
        {ok, _Started} ->
            ok;
        {error, _Reason} = Err ->
            Err
    end.

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(eturnal).

%% API: supervisor callback.

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API: gen_server callbacks.

-spec init(any()) -> {ok, state()} | no_return().
init(_Opts) ->
    process_flag(trap_exit, true),
    ok = eturnal_module:init(),
    ok = log_control_listener(),
    case turn_enabled() of
        true ->
            ok = check_turn_config(got_relay_addr());
        false ->
            ?LOG_DEBUG("TURN is disabled")
    end,
    case ensure_run_dir() of
        ok ->
            ok;
        error -> % Has been logged.
            abort(run_dir_failure)
    end,
    case check_proxy_config() of
        ok ->
            ok;
        error -> % Has been logged.
            abort(proxy_config_failure)
    end,
    case check_pem_file() of
        Result when Result =:= ok;
                    Result =:= unmodified ->
            ok;
        error -> % Has been logged.
            abort(certificate_failure)
    end,
    Ms = case start_modules() of
             {ok, Modules} ->
                 ?LOG_DEBUG("Started ~B modules", [length(Modules)]),
                 Modules;
             {error, Reason1} ->
                 ?LOG_DEBUG("Failed to start modules: ~p", [Reason1]),
                 abort(module_failure)
         end,
    Ls = case start_listeners() of
             {ok, Listeners} ->
                 ?LOG_DEBUG("Started ~B listeners", [length(Listeners)]),
                 Listeners;
             {error, Reason2} ->
                 ?LOG_DEBUG("Failed to start listeners: ~p", [Reason2]),
                 abort(listener_failure)
         end,
    {ok, #eturnal_state{listeners = Ls, modules = Ms}}.

-spec handle_call(reload | get_info | get_version | get_loglevel |
                  {set_loglevel, eturnal_logger:level()} |
                  {get_password, binary()} | term(),
                  {pid(), term()}, state())
      -> {reply, ok | {ok, term()} | {error, term()}, state()}.
handle_call(reload, _From, State) ->
    case conf:reload_file() of
        ok ->
            case check_pem_file() of
                ok ->
                    ok = fast_tls:clear_cache(),
                    ?LOG_INFO("Using new TLS certificate");
                unmodified ->
                    ?LOG_DEBUG("TLS certificate unchanged");
                error -> % Has been logged.
                    abort(certificate_failure)
            end,
            ?LOG_DEBUG("Reloaded configuration"),
            {reply, ok, State};
        {error, Reason} = Err ->
            ?LOG_ERROR("Cannot reload configuration: ~ts",
                       [conf:format_error(Reason)]),
            {reply, Err, State}
    end;
handle_call(get_info, _From, State) ->
    Info = eturnal_misc:info(),
    {reply, {ok, Info}, State};
handle_call(get_version, _From, State) ->
    Version = eturnal_misc:version(),
    {reply, {ok, Version}, State};
handle_call(get_loglevel, _From, State) ->
    Level = eturnal_logger:get_level(),
    {reply, {ok, Level}, State};
handle_call({set_loglevel, Level}, _From, State) ->
    try
        ok = eturnal_logger:set_level(Level),
        {reply, ok, State}
    catch error:{badmatch, {error, _Reason} = Err} ->
        {reply, Err, State}
    end;
handle_call({get_password, Username}, _From, State) ->
    case get_opt(secret) of
        [Secret | _Secrets] ->
            Password = derive_password(Username, [Secret]),
            {reply, {ok, Password}, State};
        undefined ->
            {reply, {error, no_secret}, State}
    end;
handle_call(Request, From, State) ->
    ?LOG_ERROR("Got unexpected request from ~p: ~p", [From, Request]),
    {reply, {error, badarg}, State}.

-spec handle_cast({config_change, config_changes(),
                   fun(() -> ok), fun(() -> ok)} | term(), state())
      -> {noreply, state()} | no_return().
handle_cast({config_change, Changes, BeginFun, EndFun}, State) ->
    ok = BeginFun(),
    State1 = apply_config_changes(State, Changes),
    ok = EndFun(),
    {noreply, State1};
handle_cast(Msg, State) ->
    ?LOG_ERROR("Got unexpected message: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    ?LOG_ERROR("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> ok.
terminate(Reason, State) ->
    ?LOG_DEBUG("Terminating ~s (~p)", [?MODULE, Reason]),
    _ = stop_listeners(State),
    _ = stop_modules(State),
    _ = clean_run_dir(),
    _ = eturnal_module:terminate(),
    ok.

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    ?LOG_INFO("Got code change request"),
    {ok, State}.

%% API: stun callbacks.

-spec run_hook(eturnal_module:event(), eturnal_module:info()) -> ok.
run_hook(Event, Info) ->
    eturnal_module:handle_event(Event, Info).

-spec get_password(binary(), binary()) -> binary() | [binary()].
get_password(Username, _Realm) ->
    [Expiration | _Suffix] = binary:split(Username, <<$:>>),
    try binary_to_integer(Expiration) of
        ExpireTime ->
            case erlang:system_time(second) of
                Now when Now < ExpireTime ->
                    ?LOG_DEBUG("Looking up password for: ~ts", [Username]),
                    derive_password(Username, get_opt(secret));
                Now when Now >= ExpireTime ->
                    case get_opt(strict_expiry) of
                        true ->
                            ?LOG_INFO("Credentials expired: ~ts", [Username]),
                            <<>>;
                        false ->
                            ?LOG_DEBUG("Credentials expired: ~ts", [Username]),
                            {expired,
                             derive_password( Username, get_opt(secret))}
                    end
            end
    catch _:badarg ->
            ?LOG_INFO("Non-numeric expiration field: ~ts", [Username]),
            <<>>
    end.

%% API: retrieve option value.

-spec get_opt(option()) -> value().
get_opt(Opt) ->
    {ok, Val} = application:get_env(eturnal, Opt),
    Val.

%% API: abnormal termination.

-spec abort(term()) -> no_return().
abort(Reason) ->
    case application:get_env(eturnal, on_fail, halt) of
        exit ->
            ?LOG_ALERT("Stopping eturnal: ~s", [format_error(Reason)]),
            exit(Reason);
        _Halt ->
            ?LOG_ALERT("Aborting eturnal: ~s", [format_error(Reason)]),
            eturnal_logger:stop(), % Flush logs.
            halt(1)
    end.

%% Internal functions: authentication.

-spec derive_password(binary(), [binary()]) -> binary() | [binary()].
-ifdef(old_crypto).
derive_password(Username, [Secret]) ->
    base64:encode(crypto:hmac(sha, Secret, Username));
derive_password(Username, Secrets) when is_list(Secrets) ->
    [derive_password(Username, [Secret]) || Secret <- Secrets].
-else.
derive_password(Username, [Secret]) ->
    base64:encode(crypto:mac(hmac, sha, Secret, Username));
derive_password(Username, Secrets) when is_list(Secrets) ->
    [derive_password(Username, [Secret]) || Secret <- Secrets].
-endif.

%% Internal functions: log distribution listener port.

-spec log_control_listener() -> ok.
-dialyzer({[no_fail_call, no_match], log_control_listener/0}). % OTP 21/22.
log_control_listener() ->
    [Name, Host] = string:split(atom_to_list(node()), "@"),
    % The 'catch' calms Dialyzer on OTP 21 (even though we don't match 'EXIT').
    case catch erl_epmd:port_please(Name, Host, 10000) of
        {port, Port, Version} ->
            ?LOG_INFO("Listening on ~s:~B (tcp) (Erlang protocol version ~B)",
                      [Host, Port, Version]);
        {error, Reason} ->
            ?LOG_INFO("Cannot determine control query port: ~p", [Reason]);
        Reason when is_atom(Reason) ->
            ?LOG_INFO("Cannot determine control query port: ~s", [Reason])
    end.

%% Internal functions: module startup/shutdown.

-spec start_modules() -> {ok, modules()} | {error, term()}.
start_modules() ->
    try lists:map(
          fun({Mod, _Opts}) ->
                  case eturnal_module:start(Mod) of
                      ok ->
                          ?LOG_INFO("Started ~s", [Mod]),
                          Mod;
                      {error, Reason} = Err ->
                          ?LOG_CRITICAL("Failed to start ~s: ~p",
                                        [Mod, Reason]),
                          throw(Err)
                  end
          end, maps:to_list(get_opt(modules))) of
        Modules ->
            {ok, Modules}
    catch throw:{error, Reason} ->
            {error, Reason}
    end.

-spec stop_modules(state()) -> ok | {error, term()}.
stop_modules(#eturnal_state{modules = Modules}) ->
    try lists:foreach(
          fun(Mod) ->
                  case eturnal_module:stop(Mod) of
                      ok ->
                          ?LOG_INFO("Stopped ~s", [Mod]);
                      {error, Reason} = Err ->
                          ?LOG_CRITICAL("Failed to stop ~s: ~p", [Mod, Reason]),
                      throw(Err)
                  end
          end, Modules)
    catch throw:{error, Reason} ->
            {error, Reason}
    end.

%% Internal functions: listener startup/shutdown.

-spec start_listeners() -> {ok, listeners()} | {error, term()}.
start_listeners() ->
    Opts = lists:filtermap(
             fun({InKey, OutKey}) ->
                     opt_filter({OutKey, get_opt(InKey)})
             end, opt_map()) ++ [{auth_fun, fun ?MODULE:get_password/2},
                                 {hook_fun, fun ?MODULE:run_hook/2}],
    try lists:map(
          fun({IP, Port, Transport, ProxyProtocol, EnableTURN}) ->
                  Opts1 = tls_opts(Transport) ++ Opts,
                  Opts2 = turn_opts(EnableTURN) ++ Opts1,
                  Opts3 = proxy_opts(ProxyProtocol) ++ Opts2,
                  ?LOG_DEBUG("Starting listener ~s (~s) with options:~n~p",
                             [eturnal_misc:addr_to_str(IP, Port),
                              Transport, Opts3]),
                  case stun_listener:add_listener(IP, Port, Transport, Opts3) of
                      ok ->
                          ?LOG_INFO("Listening on ~s (~s) (~s)",
                                    [eturnal_misc:addr_to_str(IP, Port),
                                     Transport, describe_listener(EnableTURN)]);
                      {error, Reason} = Err ->
                          ?LOG_CRITICAL("Cannot listen on ~s (~s): ~p",
                                        [eturnal_misc:addr_to_str(IP, Port),
                                         Transport, Reason]),
                          throw(Err)
                  end,
                  {IP, Port, Transport}
          end, get_opt(listen)) of
        Listeners ->
            {ok, Listeners}
    catch throw:{error, Reason} ->
            {error, Reason}
    end.

-spec stop_listeners(state()) -> ok | {error, term()}.
stop_listeners(#eturnal_state{listeners = Listeners}) ->
    try lists:foreach(
          fun({IP, Port, Transport}) ->
                  case stun_listener:del_listener(IP, Port, Transport) of
                      ok ->
                          ?LOG_INFO("Stopped listening on ~s (~s)",
                                    [eturnal_misc:addr_to_str(IP, Port),
                                     Transport]);
                      {error, Reason} = Err ->
                          ?LOG_CRITICAL("Cannot stop listening on ~s (~s): ~p",
                                        [eturnal_misc:addr_to_str(IP, Port),
                                         Transport, Reason]),
                      throw(Err)
                  end
          end, Listeners)
    catch throw:{error, Reason} ->
            {error, Reason}
    end.

-spec describe_listener(boolean()) -> binary().
describe_listener(_EnableTURN = true) ->
    <<"STUN/TURN">>;
describe_listener(_EnableTURN = false) ->
    <<"STUN only">>.

-spec opt_map() -> [{atom(), atom()}].
opt_map() ->
    [{relay_ipv4_addr, turn_ipv4_address},
     {relay_ipv6_addr, turn_ipv6_address},
     {relay_min_port, turn_min_port},
     {relay_max_port, turn_max_port},
     {max_allocations, turn_max_allocations},
     {max_permissions, turn_max_permissions},
     {max_bps, shaper},
     {blacklist, turn_blacklist},
     {whitelist, turn_whitelist},
     {realm, auth_realm},
     {software_name, server_name}].

-spec opt_filter(Opt) -> {true, Opt} | false when Opt :: {option(), value()}.
opt_filter({relay_ipv6_addr, undefined}) ->
    false; % The 'stun' application currently wouldn't accept 'undefined'.
opt_filter(Opt) ->
    {true, Opt}.

-spec turn_opts(boolean()) -> proplists:proplist().
turn_opts(EnableTURN) ->
    case {EnableTURN, got_secret(), got_relay_addr()} of
        {true, true, true} ->
            [{use_turn, true},
             {auth_type, user}];
        {_, _, _} ->
            [{use_turn, false},
             {auth_type, anonymous}]
    end.

-spec proxy_opts(boolean()) -> proplists:proplist().
proxy_opts(true = _ProxyProtocol) ->
    [proxy_protocol];
proxy_opts(false = _ProxyProtocol) ->
    [].

-spec tls_opts(transport()) -> proplists:proplist().
-ifdef(old_inet_backend).
tls_opts(tls) ->
    [{tls, true} | extra_tls_opts()];
tls_opts(auto) ->
    ?LOG_CRITICAL("Setting 'transport: auto' requires Erlang/OTP 23 or later"),
    abort(listener_failure);
tls_opts(_) ->
    [].
-else.
tls_opts(tls) ->
    [{tls, true} | extra_tls_opts()];
tls_opts(auto) ->
    [{tls, optional} | extra_tls_opts()];
tls_opts(_) ->
    [].
-endif.

-spec extra_tls_opts() -> proplists:proplist().
extra_tls_opts() ->
    Opts = [{certfile, get_pem_file_path()},
            {ciphers, get_opt(tls_ciphers)},
            {protocol_options, get_opt(tls_options)}],
    case get_opt(tls_dh_file) of
        Path when is_binary(Path) ->
            [{dhfile, Path} | Opts];
        none ->
            Opts
    end.

%% Internal functions: configuration parsing.

-spec tls_enabled() -> boolean().
tls_enabled() ->
    lists:any(fun({_IP, _Port, Transport, _ProxyProtocol, _EnableTURN}) ->
                      (Transport =:= tls) or (Transport =:= auto)
              end, get_opt(listen)).

-spec turn_enabled() -> boolean().
turn_enabled() ->
    lists:any(fun({_IP, _Port, _Transport, _ProxyProtocol, EnableTURN}) ->
                      EnableTURN =:= true
              end, get_opt(listen)).

-spec got_secret() -> boolean().
got_secret() ->
    case get_opt(secret) of
        Secrets when is_list(Secrets) ->
            lists:all(fun(Secret) ->
                              is_binary(Secret) and (byte_size(Secret) > 0)
                      end, Secrets);
        Secret when is_binary(Secret), byte_size(Secret) > 0 ->
            true;
        undefined ->
            false
    end.

-spec got_relay_addr() -> boolean().
got_relay_addr() ->
    case get_opt(relay_ipv4_addr) of
        undefined ->
            false;
        {127, _, _, _} ->
            false;
        {0, 0, 0, 0} ->
            false;
        {_, _, _, _} ->
            true
    end.

-spec check_turn_config(boolean()) -> ok.
check_turn_config(_GotAddr = true) ->
    ?LOG_DEBUG("TURN configuration seems fine");
check_turn_config(_GotAddr = false) ->
    ?LOG_WARNING("Specify a 'relay_ipv4_addr' to enable TURN").

-spec check_proxy_config() -> ok | error.
check_proxy_config() ->
    case lists:any(
           fun({_IP, _Port, Transport, ProxyProtocol, _EnableTURN}) ->
                   (Transport =:= udp) and (ProxyProtocol =:= true)
           end, get_opt(listen)) of
        true ->
            ?LOG_CRITICAL("The 'proxy_protocol' ist not supported for 'udp'"),
            error;
        false ->
            ok
    end.

%% Internal functions: configuration reload.

-spec logging_config_changed(config_changes()) -> boolean().
logging_config_changed({Changed, New, Removed}) ->
    ModifiedKeys = proplists:get_keys(Changed ++ New ++ Removed),
    LoggingKeys = [log_dir,
                   log_level,
                   log_rotate_size,
                   log_rotate_count],
    lists:any(fun(Key) -> lists:member(Key, ModifiedKeys) end, LoggingKeys).

-spec listener_config_changed(config_changes()) -> boolean().
listener_config_changed({Changed, New, Removed}) ->
    ModifiedKeys = proplists:get_keys(Changed ++ New ++ Removed),
    ListenerKeys = [listen,
                    relay_ipv4_addr,
                    relay_ipv6_addr,
                    relay_min_port,
                    relay_max_port,
                    max_allocations,
                    max_permissions,
                    max_bps,
                    blacklist,
                    whitelist,
                    realm,
                    software_name,
                    tls_options,
                    tls_ciphers,
                    tls_dh_file],
    lists:any(fun(Key) -> lists:member(Key, ModifiedKeys) end, ListenerKeys).

-spec module_config_changed(config_changes()) -> boolean().
module_config_changed({Changed, New, Removed}) ->
    ModifiedKeys = proplists:get_keys(Changed ++ New ++ Removed),
    ModuleKeys = [modules],
    lists:any(fun(Key) -> lists:member(Key, ModifiedKeys) end, ModuleKeys).

-spec apply_config_changes(state(), config_changes()) -> state() | no_return().
apply_config_changes(State, {Changed, New, Removed} = ConfigChanges) ->
    if length(Changed) > 0 ->
            ?LOG_DEBUG("Changed options: ~p", [Changed]);
       length(Changed) =:= 0 ->
            ?LOG_DEBUG("No changed options")
    end,
    if length(Removed) > 0 ->
            ?LOG_DEBUG("Removed options: ~p", [Removed]);
       length(Removed) =:= 0 ->
            ?LOG_DEBUG("No removed options")
    end,
    if length(New) > 0 ->
            ?LOG_DEBUG("New options: ~p", [New]);
       length(New) =:= 0 ->
            ?LOG_DEBUG("No new options")
    end,
    case logging_config_changed(ConfigChanges) of
        true ->
            ok = eturnal_logger:reconfigure(),
            ?LOG_INFO("Applied new logging configuration");
        false ->
            ?LOG_DEBUG("Logging configuration unchanged")
    end,
    State1 = case module_config_changed(ConfigChanges) of
                 true ->
                     case {stop_modules(State), start_modules()} of
                         {ok, {ok, Modules}} ->
                             ?LOG_INFO("Applied new module configuration"),
                             State#eturnal_state{modules = Modules};
                         {_, _} -> % Error has been logged.
                             abort(module_failure)
                     end;
                 false ->
                     ?LOG_DEBUG("Module configuration unchanged"),
                     State
             end,
    State2 = case listener_config_changed(ConfigChanges) of
                 true ->
                     case {stop_listeners(State), timer:sleep(500),
                           start_listeners()} of
                         {ok, ok, {ok, Listeners}} ->
                             ?LOG_INFO("Applied new listen configuration"),
                             State1#eturnal_state{listeners = Listeners};
                         {_, ok, _} -> % Error has been logged.
                             abort(listener_failure)
                     end;
                 false ->
                     ?LOG_DEBUG("Listen configuration unchanged"),
                     State1
             end,
    State2.

%% Internal functions: PEM file handling.

-spec get_pem_file_path() -> file:filename_all().
get_pem_file_path() ->
    filename:join(get_opt(run_dir), <<?PEM_FILE_NAME>>).

-spec check_pem_file() -> ok | unmodified | error.
check_pem_file() ->
    case tls_enabled() of
        true ->
            OutFile = get_pem_file_path(),
            case {get_opt(tls_crt_file), filelib:last_modified(OutFile)} of
                {none, OutTime} when OutTime =/= 0 ->
                    ?LOG_DEBUG("Keeping PEM file (~ts)", [OutFile]),
                    unmodified;
                {none, OutTime} when OutTime =:= 0 ->
                    ?LOG_WARNING("TLS enabled without 'tls_crt_file', creating "
                                 "self-signed certificate"),
                    create_self_signed(OutFile);
                {CrtFile, OutTime} ->
                    case filelib:last_modified(CrtFile) of
                        CrtTime when CrtTime =< OutTime ->
                            ?LOG_DEBUG("Keeping PEM file (~ts)", [OutFile]),
                            unmodified;
                        CrtTime when CrtTime =/= 0 -> % Assert to be true.
                            ?LOG_DEBUG("Updating PEM file (~ts)", [OutFile]),
                            import_pem_file(CrtFile, OutFile)
                    end
            end;
        false ->
            ?LOG_DEBUG("TLS not enabled, ignoring certificate configuration"),
            unmodified
    end.

-spec import_pem_file(binary(), file:filename_all()) -> ok | error.
import_pem_file(CrtFile, OutFile) ->
    try
        Read = [read, binary, raw],
        Write = [write, binary, raw],
        Append = [append, binary, raw],
        ok = touch(OutFile),
        case get_opt(tls_key_file) of
            KeyFile when is_binary(KeyFile) ->
                {ok, _} = file:copy({KeyFile, Read}, {OutFile, Write}),
                ?LOG_DEBUG("Copied ~ts into ~ts", [KeyFile, OutFile]);
            none ->
                ?LOG_INFO("No 'tls_key_file' specified, assuming key in ~ts",
                          [CrtFile])
        end,
        {ok, _} = file:copy({CrtFile, Read}, {OutFile, Append}),
        ?LOG_DEBUG("Copied ~ts into ~ts", [CrtFile, OutFile]),
        ok
    catch
        error:{badarg, {error, Reason}} ->
            ?LOG_CRITICAL("Cannot create ~ts: ~ts",
                          [OutFile, file:format_error(Reason)]),
            error
    end.

-spec create_self_signed(file:filename_all()) -> ok | error.
create_self_signed(File) ->
    try
        PEM = eturnal_cert:create(get_opt(realm)),
        ok = touch(File),
        ok = file:write_file(File, PEM, [raw])
    catch
        error:{badarg, {error, Reason}} ->
            ?LOG_CRITICAL("Cannot create ~ts: ~ts",
                          [File, file:format_error(Reason)]),
            error
    end.

-spec touch(file:filename_all()) -> ok.
touch(File) ->
    {ok, Fd} = file:open(File, [append, binary, raw]),
    ok = file:close(Fd),
    ok = file:change_mode(File, 8#00600).

%% Internal functions: run directory.

-spec ensure_run_dir() -> ok | error.
ensure_run_dir() ->
    RunDir = get_opt(run_dir),
    case filelib:ensure_dir(filename:join(RunDir, <<"state.dat">>)) of
        ok ->
            ?LOG_DEBUG("Using run directory ~ts", [RunDir]),
            ok;
        {error, Reason} ->
            ?LOG_CRITICAL("Cannot create run directory ~ts: ~ts",
                          [RunDir, file:format_error(Reason)]),
            error
    end.

-spec clean_run_dir() -> ok | {error, term()}.
clean_run_dir() ->
    PEMFile = get_pem_file_path(),
    case filelib:is_regular(PEMFile) of
        true ->
            case file:delete(PEMFile) of
                ok ->
                    ?LOG_DEBUG("Removed ~ts", [PEMFile]),
                    ok;
                {error, Reason} = Err ->
                    ?LOG_WARNING("Cannot remove ~ts: ~ts",
                                 [PEMFile, file:format_error(Reason)]),
                    Err
            end;
        false ->
            ?LOG_DEBUG("PEM file doesn't exist: ~ts", [PEMFile])
    end.

%% Internal functions: error message formatting.

-spec format_error(atom()) -> binary().
format_error(certificate_failure) ->
    <<"PEM file failure">>;
format_error(dependency_failure) ->
    <<"Dependency failure">>;
format_error(listener_failure) ->
    <<"Listener startup failure">>;
format_error(module_failure) ->
    <<"Module startup failure">>;
format_error(proxy_config_failure) ->
    <<"Proxy protocol configuration failure">>;
format_error(run_dir_failure) ->
    <<"Run directory failure">>;
format_error(_Unknown) ->
    <<"Unknown error">>.
