%%% eturnal STUN/TURN server module.
%%%
%%% Copyright (c) 2022-2023 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2022-2023 ProcessOne, SARL.
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

%%% This module exports STUN/TURN metrics to Prometheus.

-module(mod_stats_prometheus).
-behaviour(eturnal_module).
-export([start/0,
         stop/0,
         handle_event/2,
         options/0]).
-import(yval, [either/2, ip/0, port/0, bool/0, file/1]).

-include_lib("kernel/include/logger.hrl").
-define(PEM_FILE_NAME, "cert.pem").
-define(SIZE_BUCKETS,
        [1024 * 4,
         1024 * 32,
         1024 * 256,
         1024 * 1024,
         1024 * 1024 * 4,
         1024 * 1024 * 16,
         1024 * 1024 * 64,
         1024 * 1024 * 256,
         1024 * 1024 * 1024]).
-define(TIME_BUCKETS,
        [timer:minutes(1) div 1000,
         timer:minutes(5) div 1000,
         timer:minutes(15) div 1000,
         timer:minutes(30) div 1000,
         timer:hours(1) div 1000,
         timer:hours(2) div 1000,
         timer:hours(6) div 1000,
         timer:hours(12) div 1000,
         timer:hours(24) div 1000]).

%% API.

-spec start() -> {ok, eturnal_module:events()}.
start() ->
    ?LOG_DEBUG("Starting ~s", [?MODULE]),
    Opt = eturnal_module:get_opt(?MODULE, vm_metrics),
    case {check_vm_metrics_opt(Opt), Opt} of
        {ok, true} ->
            ok;
        {ok, false} ->
            % See: prometheus.erl/src/prometheus_collector.erl
            Collectors = [prometheus_boolean,
                          prometheus_counter,
                          prometheus_gauge,
                          prometheus_histogram,
                          prometheus_summary],
            ok = application:set_env(
                   prometheus, collectors, Collectors, [{persistent, true}]);
        {modified, _Opt} ->
            % The 'prometheus' application doesn't support updating the list of
            % collectors on configuration reload, and we cannot easily restart
            % it, as the application controller might be blocking on our own
            % startup.
            ?LOG_ERROR("New 'vm_metrics' setting requires restart")
    end,
    ok = eturnal_module:ensure_deps(?MODULE, get_deps()),
    ok = declare_metrics(),
    Addr = eturnal_module:get_opt(?MODULE, ip),
    Port = eturnal_module:get_opt(?MODULE, port),
    Root = get_document_root(), % Empty and unused.
    case inets:start(httpd, [socket_opts(),
                             {bind_address, Addr},
                             {port, Port},
                             {server_root, Root},
                             {document_root, Root},
                             {server_name, "Prometheus Exporter"},
                             {modules, [prometheus_httpd]}]) of
        {ok, _PID} ->
            ok;
        {error, {already_started, _PID}} ->
            ok;
        {error, Reason2} ->
            exit(Reason2)
    end,
    {ok, [stun_query, turn_session_start, turn_session_stop, protocol_error]}.

-spec handle_event(eturnal_module:event(), eturnal_module:info()) -> ok.
handle_event(stun_query, Info) ->
    on_stun_query(Info);
handle_event(turn_session_start, Info) ->
    on_turn_session_start(Info);
handle_event(turn_session_stop, Info) ->
    on_turn_session_stop(Info);
handle_event(protocol_error, Info) ->
    on_protocol_error(Info).

-spec stop() -> ok.
stop() ->
    ?LOG_DEBUG("Stopping ~s", [?MODULE]),
    AddrPort = {eturnal_module:get_opt(?MODULE, ip),
                eturnal_module:get_opt(?MODULE, port)},
    ok = inets:stop(httpd, AddrPort).

-spec options() -> eturnal_module:options().
options() ->
    {#{ip => either(any, ip()),
       port => port(),
       tls => bool(),
       tls_crt_file => file(read),
       tls_key_file => file(read),
       vm_metrics => bool()},
     [{defaults,
       #{ip => any,
         port => 8081,
         tls => false,
         tls_crt_file => none,
         tls_key_file => none,
         vm_metrics => true}}]}.

%% Internal functions.

-spec declare_metrics() -> ok.
declare_metrics() ->
    SizeSpec = [{labels, [transport]}, {buckets, ?SIZE_BUCKETS}],
    TimeSpec = [{labels, [transport]}, {buckets, ?TIME_BUCKETS}],
    _ = prometheus_http_impl:setup(),
    _ = prometheus_counter:declare(
          [{name, eturnal_stun_requests_total},
           {labels, [transport]},
           {help, "STUN request count"}]),
    _ = prometheus_counter:declare(
          [{name, eturnal_turn_sessions_total},
           {labels, [transport]},
           {help, "TURN session count"}]),
    _ = prometheus_gauge:declare(
          [{name, eturnal_turn_open_sessions},
           {help, "Number of currently active TURN sessions"} | SizeSpec]),
    _ = prometheus_histogram:declare(
          [{name, eturnal_turn_relay_sent_bytes},
           {help, "Number of bytes sent to TURN peers"} | SizeSpec]),
    _ = prometheus_histogram:declare(
          [{name, eturnal_turn_relay_rcvd_bytes},
           {help, "Number of bytes received from TURN peers"} | SizeSpec]),
    _ = prometheus_histogram:declare(
          [{name, eturnal_turn_session_duration_seconds},
           {help, "Duration of TURN session in seconds"} | TimeSpec]),
    _ = prometheus_counter:declare(
          [{name, eturnal_protocol_error_total},
           {labels, [transport, reason]},
           {help, "STUN/TURN protocol error count"}]),
    ok.

-spec on_stun_query(eturnal_module:info()) -> ok.
on_stun_query(#{transport := Transport}) ->
    ?LOG_DEBUG("Observing STUN query for Prometheus"),
    _ = prometheus_counter:inc(
          eturnal_stun_requests_total, [Transport]),
    ok.

-spec on_turn_session_start(eturnal_module:info()) -> ok.
on_turn_session_start(#{id := ID,
                        transport := Transport}) ->
    ?LOG_DEBUG("Observing started TURN session ~s for Prometheus", [ID]),
    _ = prometheus_gauge:inc(
          eturnal_turn_open_sessions, [Transport]),
    _ = prometheus_counter:inc(
          eturnal_turn_sessions_total, [Transport]),
    ok.

-spec on_turn_session_stop(eturnal_module:info()) -> ok.
on_turn_session_stop(#{id := ID,
                       transport := Transport,
                       sent_bytes := SentBytes,
                       rcvd_bytes := RcvdBytes,
                       duration := Duration}) ->
    ?LOG_DEBUG("Observing stopped TURN session ~s for Prometheus", [ID]),
    _ = prometheus_gauge:dec(
          eturnal_turn_open_sessions, [Transport]),
    _ = prometheus_histogram:observe(
          eturnal_turn_session_duration_seconds, [Transport], Duration),
    _ = prometheus_histogram:observe(
          eturnal_turn_relay_rcvd_bytes, [Transport], RcvdBytes),
    _ = prometheus_histogram:observe(
          eturnal_turn_relay_sent_bytes, [Transport], SentBytes),
    ok.

-spec on_protocol_error(eturnal_module:info()) -> ok.
on_protocol_error(#{transport := Transport,
                    reason := {_Code, Text}}) ->
    ?LOG_DEBUG("Observing protocol error for Prometheus: ~s", [Text]),
    _ = prometheus_counter:inc(
          eturnal_protocol_error_total, [Transport, Text]),
    ok.

-spec socket_opts() -> {socket_type, ip_comm | {essl, proplists:proplist()}}.
socket_opts() ->
    case eturnal_module:get_opt(?MODULE, tls) of
        true ->
            {CrtFile, KeyFile} = get_pem_files(),
            {socket_type, {essl, [{certfile, CrtFile}, {keyfile, KeyFile}]}};
        false ->
            {socket_type, ip_comm}
    end.

-spec get_deps() -> [eturnal_module:dep()].
get_deps() ->
    case eturnal_module:get_opt(?MODULE, tls) of
        true ->
            [ssl, prometheus_httpd];
        false ->
            [prometheus_httpd]
    end.

-spec get_pem_files() -> {file:filename(), file:filename()}.
get_pem_files() ->
    case get_module_or_global_opt(tls_crt_file) of
        CrtFile when is_binary(CrtFile) ->
            case get_module_or_global_opt(tls_key_file) of
                KeyFile when is_binary(KeyFile) ->
                    {CrtFile, KeyFile};
                none ->
                    {CrtFile, CrtFile}
            end;
        none ->
            ?LOG_WARNING("TLS enabled for ~s without 'tls_crt_file', creating "
                         "self-signed certificate", [?MODULE]),
            File = get_pem_file_path(),
            ok = eturnal:create_self_signed(File),
            {File, File}
    end.

-spec get_pem_file_path() -> file:filename().
get_pem_file_path() ->
    Path = filename:join(get_module_run_dir(), <<?PEM_FILE_NAME>>),
    unicode:characters_to_list(Path).

-spec get_document_root() -> file:filename().
get_document_root() ->
    DocDir = filename:join(get_module_run_dir(), <<"doc">>),
    ok = filelib:ensure_dir(filename:join(DocDir, <<"file">>)),
    unicode:characters_to_list(DocDir).

-spec get_module_run_dir() -> file:filename().
get_module_run_dir() ->
    RunDir = filename:join(eturnal:get_opt(run_dir), ?MODULE),
    ok = filelib:ensure_dir(filename:join(RunDir, <<"file">>)),
    unicode:characters_to_list(RunDir).

-spec get_module_or_global_opt(eturnal_module:option()) -> term().
get_module_or_global_opt(Opt) ->
    case eturnal_module:get_opt(?MODULE, Opt) of
        Value when Value =:= undefined;
                   Value =:= none ->
            eturnal:get_opt(Opt);
        Value ->
            Value
    end.

-spec check_vm_metrics_opt(boolean()) -> ok | modified.
check_vm_metrics_opt(NewValue) ->
    PID = whereis(prometheus_sup),
    OldValue = application:get_env(prometheus, collectors),
    if is_pid(PID),
       ((NewValue =:= true) and (OldValue =/= undefined)) or
        (NewValue =/= true) and (OldValue =:= undefined) ->
            modified;
       true ->
            ok
    end.
