%%% eturnal STUN/TURN server.
%%%
%%% Copyright (c) 2020-2023 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2020-2023 ProcessOne, SARL.
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

-module(eturnal_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

-type info() :: ct_suite:ct_info().
-type config() :: ct_suite:ct_config().
-type test_def() :: ct_suite:ct_test_def().
-type test_name() :: ct_suite:ct_testname().
-type group_def() :: ct_suite:ct_group_def().
-type group_name() :: ct_suite:ct_groupname().

%% API.

-spec suite() -> [info()].
suite() ->
    [{require, {server, [address, udp_port, tcp_port, tls_port, auto_port]}},
     {timetrap, {seconds, 120}}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Server = ct:get_config(server),
    Address = proplists:get_value(address, Server),
    UdpPort = proplists:get_value(udp_port, Server),
    TcpPort = proplists:get_value(tcp_port, Server),
    TlsPort = proplists:get_value(tls_port, Server),
    AutoPort = proplists:get_value(auto_port, Server),
    [{address, Address},
     {udp_port, UdpPort},
     {tcp_port, TcpPort},
     {tls_port, TlsPort},
     {auto_port, AutoPort} | Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_GroupName, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> ok.
end_per_group(_GroupName, _Config) ->
    ok.

-spec init_per_testcase(test_name(), config()) -> config().
-ifdef(old_inet_backend).
init_per_testcase(start_eturnal, Config) ->
    set_eturnal_env("eturnal-old-otp.yml", Config);
init_per_testcase(stun_tcp_auto, _Config) ->
    {skip, otp_version_unsupported};
init_per_testcase(stun_tls_auto, _Config) ->
    {skip, otp_version_unsupported};
init_per_testcase(_TestCase, Config) ->
    Config.
-else.
init_per_testcase(start_eturnal, Config) ->
    set_eturnal_env("eturnal-new-otp.yml", Config);
init_per_testcase(_TestCase, Config) ->
    Config.
-endif.

-spec end_per_testcase(test_name(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
    ok.

-spec groups() -> [group_def()].
groups() ->
    [].

-spec all() -> [test_def()] | {skip, term()}.
all() ->
    [start_eturnal,
     check_status,
     check_info,
     check_all_sessions,
     check_user_sessions,
     check_disconnect,
     check_credentials,
     check_loglevel,
     check_version,
     reload,
     connect_tcp,
     connect_tls,
     turn_udp,
     stun_udp,
     stun_tcp,
     stun_tls,
     stun_tcp_auto,
     stun_tls_auto,
     stop_eturnal].

-spec start_eturnal(config()) -> any().
start_eturnal(_Config) ->
    ct:pal("Starting up eturnal"),
    ok = eturnal:start().

-spec check_status(config()) -> any().
check_status(_Config) ->
    ct:pal("Checking eturnal status"),
    {ok, Status} = eturnal_ctl:get_status(),
    ct:pal("Got eturnal status: ~p", [Status]),
    true = is_list(Status).

-spec check_info(config()) -> any().
check_info(_Config) ->
    ct:pal("Checking eturnal statistics"),
    {ok, Info} = eturnal_ctl:get_info(),
    ct:pal("Got eturnal statistics: ~p", [Info]),
    true = is_list(Info).

-spec check_all_sessions(config()) -> any().
check_all_sessions(_Config) ->
    ct:pal("Checking active TURN sessions"),
    {ok, Sessions} = eturnal_ctl:get_sessions(),
    ct:pal("Got active TURN sessions: ~p", [Sessions]),
    true = is_list(Sessions).

-spec check_user_sessions(config()) -> any().
check_user_sessions(_Config) ->
    ct:pal("Checking active TURN sessions of a user"),
    {ok, Sessions1} = eturnal_ctl:get_sessions("alice"),
    ct:pal("Got active TURN sessions of a user: ~p", [Sessions1]),
    true = is_list(Sessions1),
    ct:pal("Checking active TURN sessions of another user"),
    {ok, Sessions2} = eturnal_ctl:get_sessions("1256900400:alice"),
    ct:pal("Got active TURN sessions of another user: ~p", [Sessions2]),
    true = is_list(Sessions2),
    ct:pal("Checking active TURN sessions of invalid user"),
    {error, Reason} = eturnal_ctl:disconnect(alice),
    ct:pal("Got an error, as expected: ~p", [Reason]).

-spec check_disconnect(config()) -> any().
check_disconnect(_Config) ->
    ct:pal("Checking disconnection of TURN user"),
    {ok, Msg} = eturnal_ctl:disconnect("alice"),
    ct:pal("Got result for disconnecting TURN user: ~p", [Msg]),
    true = is_list(Msg),
    ct:pal("Checking disconnection of invalid TURN user"),
    {error, Reason} = eturnal_ctl:disconnect(alice),
    ct:pal("Got an error, as expected: ~p", [Reason]).

-spec check_credentials(config()) -> any().
check_credentials(_Config) ->
    Timestamp = "2009-10-30 11:00:00Z",
    Username = 1256900400,
    Password = "uEKlpcME7MNMMVRV8rUFPCTIFEs=",
    ct:pal("Checking credentials valid until ~s", [Timestamp]),
    {ok, Credentials} = eturnal_ctl:get_credentials(Timestamp, []),
    {ok, [Username, Password], []} =
        io_lib:fread("Username: ~u~~nPassword: ~s", Credentials),
    lists:foreach(
      fun(Lifetime) ->
              ct:pal("Checking credentials valid for ~s", [Lifetime]),
              {ok, Creds} = eturnal_ctl:get_credentials(Lifetime, "alice"),
              {ok, [Time, Pass], []} =
                  io_lib:fread("Username: ~u:alice~~nPassword: ~s", Creds),
              {ok, Pass} =
                  eturnal_ctl:get_password(integer_to_list(Time) ++ ":alice"),
              true = erlang:system_time(second) + 86400 - Time < 5
      end, ["86400", "86400s", "1440m", "24h", "1d"]),
    ct:pal("Checking invalid expiry"),
    lists:foreach(
      fun(Invalid) ->
              {error, _Reason1} = eturnal_ctl:get_password(Invalid),
              {error, _Reason2} = eturnal_ctl:get_credentials(Invalid, [])
      end, ["Invalid", invalid, [invalid]]),
    ct:pal("Checking invalid suffix"),
    {error, _Reason} = eturnal_ctl:get_credentials(Username, invalid),
    ct:pal("Checking static credentials"),
    {ok, "l0vesBob"} = eturnal_ctl:get_password("alice").

-spec check_loglevel(config()) -> any().
check_loglevel(_Config) ->
    Level = "debug",
    ct:pal("Setting log level to ~s", [Level]),
    ok = eturnal_ctl:set_loglevel(list_to_atom(Level)),
    ct:pal("Checking whether log level is set to ~s", [Level]),
    {ok, Level} = eturnal_ctl:get_loglevel(),
    ct:pal("Setting invalid log level"),
    {error, _Reason} = eturnal_ctl:set_loglevel(invalid),
    ct:pal("Checking whether log level is still set to ~s", [Level]),
    {ok, Level} = eturnal_ctl:get_loglevel().

-spec check_version(config()) -> any().
check_version(_Config) ->
    ct:pal("Checking eturnal version"),
    {ok, Version} = eturnal_ctl:get_version(),
    ct:pal("Got eturnal version: ~p", [Version]),
    match = re:run(Version, "^[0-9]+\\.[0-9]+\\.[0-9](\\+[0-9]+)?",
                   [{capture, none}]).

-spec reload(config()) -> any().
reload(_Config) ->
    CertFile = <<"run/cert.pem">>,
    ct:pal("Deleting TLS certificate"),
    ok = file:delete(CertFile),
    ct:pal("Reloading eturnal"),
    ok = eturnal_ctl:reload(),
    ct:pal("Checking whether new TLS certificate was created"),
    {ok, _} = file:read_file_info(CertFile),
    ct:pal("Reloading eturnal without changes"),
    ok = eturnal_ctl:reload().

-spec connect_tcp(config()) -> any().
connect_tcp(Config) ->
    Addr = ?config(address, Config),
    Port = ?config(tcp_port, Config),
    ct:pal("Connecting to 127.0.0.1:~B (TCP)", [Port]),
    {ok, Sock} = gen_tcp:connect(Addr, Port, []),
    ok = gen_tcp:close(Sock).

-spec connect_tls(config()) -> any().
connect_tls(Config) ->
    Addr = ?config(address, Config),
    Port = ?config(tls_port, Config),
    ct:pal("Connecting to 127.0.0.1:~B (TLS)", [Port]),
    {ok, TCPSock} = gen_tcp:connect(Addr, Port, []),
    {ok, TLSSock} = fast_tls:tcp_to_tls(TCPSock, [connect]),
    ok = fast_tls:close(TLSSock).

-spec turn_udp(config()) -> any().
turn_udp(Config) ->
    Addr = ?config(address, Config),
    Port = ?config(udp_port, Config),
    Username1 = <<"alice">>,
    Password1 = <<"l0vesBob">>,
    Username2 = <<"2145913200">>,
    Password2 = <<"cLwpKS2/9bWHf+agUImD47PIXNE=">>,
    Realm = <<"eturnal.net">>,
    ct:pal("Allocating TURN relay on 127.0.0.1:~B (UDP)", [Port]),
    ok = stun_test:allocate_udp(Addr, Port, Username1, Realm, Password1),
    ok = stun_test:allocate_udp(Addr, Port, Username2, Realm, Password2).

-spec stun_udp(config()) -> any().
stun_udp(Config) ->
    Addr = ?config(address, Config),
    Port = ?config(udp_port, Config),
    ct:pal("Performing STUN query against 127.0.0.1:~B (UDP)", [Port]),
    Result = stun_test:bind_udp(Addr, Port),
    ct:pal("Got query result: ~p", [Result]),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stun_tcp(config()) -> any().
stun_tcp(Config) ->
    Addr = ?config(address, Config),
    Port = ?config(tcp_port, Config),
    ct:pal("Performing STUN query against 127.0.0.1:~B (TCP)", [Port]),
    Result = stun_test:bind_tcp(Addr, Port),
    ct:pal("Got query result: ~p", [Result]),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stun_tls(config()) -> any().
stun_tls(Config) ->
    Addr = ?config(address, Config),
    Port = ?config(tls_port, Config),
    ct:pal("Performing STUN query against 127.0.0.1:~B (TLS)", [Port]),
    Result = stun_test:bind_tls(Addr, Port),
    ct:pal("Got query result: ~p", [Result]),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stun_tcp_auto(config()) -> any().
stun_tcp_auto(Config) ->
    Addr = ?config(address, Config),
    Port = ?config(auto_port, Config),
    ct:pal("Performing STUN query against 127.0.0.1:~B (TCP)", [Port]),
    Result = stun_test:bind_tcp(Addr, Port),
    ct:pal("Got query result: ~p", [Result]),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stun_tls_auto(config()) -> any().
stun_tls_auto(Config) ->
    Addr = ?config(address, Config),
    Port = ?config(auto_port, Config),
    ct:pal("Performing STUN query against 127.0.0.1:~B (TLS)", [Port]),
    Result = stun_test:bind_tls(Addr, Port),
    ct:pal("Got query result: ~p", [Result]),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stop_eturnal(config()) -> any().
stop_eturnal(_Config) ->
    ct:pal("Stopping eturnal"),
    ok = eturnal:stop().

%% Internal functions.

-spec set_eturnal_env(file:filename_all(), config()) -> config().
set_eturnal_env(ConfName, Config) ->
    DataDir = ?config(data_dir, Config),
    ConfFile = filename:join(DataDir, ConfName),
    ok = application:set_env(conf, file, ConfFile),
    ok = application:set_env(conf, on_fail, stop),
    ok = application:set_env(eturnal, on_fail, exit),
    Config.
