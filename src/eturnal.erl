%%% eturnal STUN/TURN server.
%%%
%%% Copyright (c) 2020 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2020 ProcessOne, SARL.
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
         start_link/0,
         get_opt/1,
         get_password/2,
         run_hook/2,
         stop/0,
         abort/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").
-define(PEM_FILE_NAME, "cert.pem").

-record(eturnal_state,
        {listeners :: listeners(),
         modules :: modules()}).

-type transport() :: udp | tcp | tls.
-type listeners() :: [{inet:port_number(), transport()}].
-type modules() :: [module()].
-type option() :: atom().
-type value() :: term().
-type config_changes() :: {[{option(), value()}],
                           [{option(), value()}],
                           [option()]}.
-type state() :: #eturnal_state{}.

%% API.

-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(?MODULE, permanent) of
        {ok, _Started} ->
            ok;
        {error, _Reason} = Err ->
            Err
    end.

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_opt(option()) -> value().
get_opt(Opt) ->
    {ok, Val} = application:get_env(eturnal, Opt),
    Val.

-spec get_password(binary(), binary()) -> binary().
get_password(Username, _Realm) ->
    [Expiration | _Suffix] = binary:split(Username, <<$:>>),
    try binary_to_integer(Expiration) of
        ExpireTime ->
            case erlang:system_time(second) of
                Now when Now < ExpireTime ->
                    ?LOG_DEBUG("Looking up password for: ~ts", [Username]),
                    derive_password(Username, get_opt(secret));
                Now when Now >= ExpireTime ->
                    ?LOG_INFO("Credentials expired: ~ts", [Username]),
                    <<>>
            end
    catch _:badarg ->
            ?LOG_INFO("Non-numeric expiration field: ~ts", [Username]),
            <<>>
    end.

-spec run_hook(eturnal_module:event(), eturnal_module:info()) -> ok.
run_hook(Event, Info) ->
    eturnal_module:handle_event(Event, Info).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(?MODULE).

-spec abort(term()) -> no_return().
abort(Reason) ->
    case application:get_env(eturnal, on_fail, halt) of
        exit ->
            ?LOG_ALERT("Stopping eturnal STUN/TURN server (~p)", [Reason]),
            exit(Reason);
        _Halt ->
            ?LOG_ALERT("Aborting eturnal STUN/TURN server (~p)", [Reason]),
            eturnal_logger:flush(),
            halt(1)
    end.

%% Behaviour callbacks.

-spec init(any()) -> {ok, state()} | no_return().
init(_Opts) ->
    process_flag(trap_exit, true),
    case ensure_run_dir() of
        ok ->
            ok;
        error -> % Has been logged.
            abort(run_dir_failure)
    end,
    case {turn_enabled(), got_secret(), got_relay_addr()} of
        {false, _, _} ->
            ?LOG_DEBUG("TURN is disabled");
        {true, true, true} ->
            ?LOG_DEBUG("TURN configuration seems fine");
        {true, false, _} ->
            ?LOG_CRITICAL("Please specify an authentication 'secret'"),
            abort(auth_secret_failure);
        {true, _, false} ->
            ?LOG_CRITICAL("Please specify your external 'relay_ipv4_addr'"),
            abort(relay_address_failure)
    end,
    case tls_enabled() of
        true ->
            case update_pem_file() of
                Result when Result =:= ok;
                            Result =:= unmodified ->
                    ?LOG_DEBUG("Certificate configuration seems fine");
                error -> % Has been logged.
                    abort(certificate_failure)
            end;
        false ->
            ?LOG_DEBUG("TLS not enabled, ignoring certificate configuration")
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

-spec handle_call(reload | get_version | get_loglevel |
                  {set_loglevel, eturnal_logger:level()} | term(),
                  {pid(), term()}, state())
      -> {reply, ok | {ok, term()} | {error, term()}, state()}.
handle_call(reload, _From, State) ->
    case conf:reload_file() of
        ok ->
            ?LOG_DEBUG("Reloaded configuration"),
            {reply, ok, State};
        {error, Reason} = Err ->
            ?LOG_ERROR("Cannot reload configuration: ~ts",
                       [conf:format_error(Reason)]),
            {reply, Err, State}
    end;
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
    ok.

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    ?LOG_INFO("Got code change request"),
    {ok, State}.

%% Internal functions.

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

-spec start_listeners() -> {ok, listeners()} | {error, term()}.
start_listeners() ->
    Opts = lists:filtermap(
             fun({InKey, OutKey}) ->
                     opt_filter({OutKey, get_opt(InKey)})
             end, opt_map()) ++ [{auth_fun, fun ?MODULE:get_password/2},
                                 {hook_fun, fun ?MODULE:run_hook/2}],
    try lists:map(
          fun({IP, Port, Transport, EnableTURN}) ->
                  Opts1 = [{use_turn, EnableTURN} | Opts],
                  Opts2 = case Transport of
                              tls ->
                                  [{tls, true},
                                   {certfile, get_pem_file_path()} | Opts1];
                              _ ->
                                  Opts1
                          end,
                  ?LOG_DEBUG("Starting listener ~s (~s) with options:~n~p",
                             [eturnal_misc:addr_to_str(IP, Port),
                              Transport, Opts2]),
                  case stun_listener:add_listener(IP, Port, Transport, Opts2) of
                      ok ->
                          Type = case EnableTURN of
                                     true ->
                                         <<"STUN/TURN">>;
                                     false ->
                                         <<"STUN only">>
                                 end,
                          ?LOG_INFO("Listening on ~s (~s) (~s)",
                                    [eturnal_misc:addr_to_str(IP, Port),
                                     Transport, Type]);
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

-spec tls_enabled() -> boolean().
tls_enabled() ->
    lists:any(fun({_IP, _Port, Transport, _EnableTURN}) ->
                      Transport =:= tls
              end, get_opt(listen)).

-spec turn_enabled() -> boolean().
turn_enabled() ->
    lists:any(fun({_IP, _Port, _Transport, EnableTURN}) ->
                      EnableTURN =:= true
              end, get_opt(listen)).

-spec got_secret() -> boolean().
got_secret() ->
    case get_opt(secret) of
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
                    realm,
                    software_name],
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
    case tls_enabled() of
        true ->
            case update_pem_file() of
                ok ->
                    ok = fast_tls:clear_cache(),
                    ?LOG_INFO("Using new TLS certificate");
                unmodified ->
                    ?LOG_DEBUG("TLS certificate unchanged");
                error -> % Has been logged.
                    abort(certificate_failure)
            end;
        false ->
            ?LOG_DEBUG("TLS not enabled, ignoring certificate configuration")
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

-spec get_pem_file_path() -> file:filename_all().
get_pem_file_path() ->
    filename:join(get_opt(run_dir), <<?PEM_FILE_NAME>>).

-spec update_pem_file() -> ok | unmodified | error.
update_pem_file() ->
    OutFile = get_pem_file_path(),
    case {get_opt(tls_crt_file), filelib:last_modified(OutFile)} of
        {none, OutTime} when OutTime =/= 0 ->
            ?LOG_DEBUG("Using existing PEM file (~ts)", [OutFile]),
            unmodified;
        {none, OutTime} when OutTime =:= 0 ->
            ?LOG_WARNING("TLS enabled without 'tls_crt_file', creating "
                         "self-signed certificate"),
            create_self_signed(OutFile);
        {CrtFile, OutTime} ->
            case filelib:last_modified(CrtFile) of
                CrtTime when CrtTime =< OutTime ->
                    ?LOG_DEBUG("Using existing PEM file (~ts)", [OutFile]),
                    unmodified;
                CrtTime when CrtTime =/= 0 -> % Assert to be true.
                    ?LOG_DEBUG("Updating PEM file (~ts)", [OutFile]),
                    import_cert(CrtFile, OutFile)
            end
    end.

-spec import_cert(binary(), file:filename_all()) -> ok | error.
import_cert(CrtFile, OutFile) ->
    try
        Read = [read, binary, raw],
        Write = [write, binary, raw],
        Append = [append, binary, raw],
        {ok, Fd} = file:open(OutFile, Write),
        ok = file:close(Fd),
        ok = file:change_mode(OutFile, 8#00600),
        case get_opt(tls_key_file) of
            KeyFile when is_binary(KeyFile) ->
                {ok, _} = file:copy({KeyFile, Read}, {OutFile, Write}),
                ?LOG_DEBUG("Copied ~ts into ~ts", [KeyFile, OutFile]);
            undefined ->
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
create_self_signed(OutFile) ->
    Cmd = io_lib:format("openssl req -x509 -batch -nodes -newkey rsa:4096 "
                        "-keyout ~ts -subj /CN=eturnal.net -days 3650",
                        [OutFile]),
    Output = os:cmd(Cmd),
    case string:find(Output, "-----BEGIN CERTIFICATE-----") of
        Cert when is_list(Cert) ->
            case file:write_file(OutFile, Cert, [append, raw]) of
                ok ->
                    ?LOG_DEBUG("Created PEM file: ~ts", [OutFile]),
                    ok;
                {error, Reason} ->
                    ?LOG_CRITICAL("Cannot store PEM file ~ts: ~ts",
                                  [OutFile, file:format_error(Reason)]),
                    error
            end;
        nomatch ->
            Err = string:trim(Output),
            Txt = if length(Err) > 0 ->
                          Err;
                     length(Err) =:= 0 ->
                          "openssl req -x509 [...] failed"
                  end,
            ?LOG_CRITICAL("Cannot create ~ts: ~ts", [OutFile, Txt]),
            error
    end.

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

-spec derive_password(binary(), binary()) -> binary().
-ifdef(old_crypto).
derive_password(Username, Secret) ->
    base64:encode(crypto:hmac(sha, Secret, Username)).
-else.
derive_password(Username, Secret) ->
    base64:encode(crypto:mac(hmac, sha, Secret, Username)).
-endif.

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
     {realm, auth_realm},
     {software_name, server_name}].

-spec opt_filter(Opt) -> {true, Opt} | false when Opt :: {option(), value()}.
opt_filter({relay_ipv6_addr, undefined}) ->
    false; % The 'stun' application currently wouldn't accept 'undefined'.
opt_filter(Opt) ->
    {true, Opt}.
