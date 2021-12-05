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
    [{timetrap, {seconds, 30}}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_GroupName, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> ok.
end_per_group(_GroupName, _Config) ->
    ok.

-ifdef(old_inet_backend).
-spec init_per_testcase(test_name(), config()) -> config().
init_per_testcase(start_eturnal, Config) ->
    set_eturnal_env("eturnal-old-otp.yml", Config);
init_per_testcase(stun_tcp_auto, _Config) ->
    {skip, otp_version_unsupported};
init_per_testcase(stun_tls_auto, _Config) ->
    {skip, otp_version_unsupported};
init_per_testcase(_TestCase, Config) ->
    Config.
-else.
-spec init_per_testcase(test_name(), config()) -> config().
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
     check_info,
     check_sessions,
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
    {ok, _} = application:ensure_all_started(eturnal).

-spec check_info(config()) -> any().
check_info(_Config) ->
    ct:pal("Checking eturnal statistics"),
    {ok, Info} = eturnal_ctl:get_info(),
    true = is_list(Info).

-spec check_sessions(config()) -> any().
check_sessions(_Config) ->
    ct:pal("Checking active TURN sessions"),
    {ok, Sessions} = eturnal_ctl:get_sessions(),
    true = is_list(Sessions).

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
      end, ["86400", "86400s", "1440m", "24h", "1d"]).

-spec check_loglevel(config()) -> any().
check_loglevel(_Config) ->
    Level = "debug",
    ct:pal("Setting log level to ~s", [Level]),
    ok = eturnal_ctl:set_loglevel(list_to_atom(Level)),
    ct:pal("Checking whether log level is set to ~s", [Level]),
    {ok, Level} = eturnal_ctl:get_loglevel().

-spec check_version(config()) -> any().
check_version(_Config) ->
    ct:pal("Checking eturnal version"),
    {ok, Version} = eturnal_ctl:get_version(),
    match = re:run(Version, "^[0-9]+\\.[0-9]+\\.[0-9](\\+[0-9]+)?",
                   [{capture, none}]).

-spec reload(config()) -> any().
reload(_Config) ->
    ct:pal("Reloading eturnal"),
    ok = eturnal_ctl:reload().

-spec connect_tcp(config()) -> any().
connect_tcp(_Config) ->
    Port = 34780,
    ct:pal("Connecting to 127.0.0.1:~B (TCP)", [Port]),
    {ok, Addr} = inet:parse_address("127.0.0.1"),
    {ok, Sock} = gen_tcp:connect(Addr, Port, []),
    ok = gen_tcp:close(Sock).

-spec connect_tls(config()) -> any().
connect_tls(_Config) ->
    Port = 53490,
    ct:pal("Connecting to 127.0.0.1:~B (TLS)", [Port]),
    {ok, Addr} = inet:parse_address("127.0.0.1"),
    {ok, TCPSock} = gen_tcp:connect(Addr, Port, []),
    {ok, TLSSock} = fast_tls:tcp_to_tls(TCPSock, [connect]),
    ok = fast_tls:close(TLSSock).

-spec turn_udp(config()) -> any().
turn_udp(_Config) ->
    Port = 34780,
    Username = <<"2145913200">>,
    Password = <<"cLwpKS2/9bWHf+agUImD47PIXNE=">>,
    Realm = <<"eturnal.net">>,
    ct:pal("Allocating TURN relay on 127.0.0.1:~B (UDP)", [Port]),
    {ok, Addr} = inet:parse_address("127.0.0.1"),
    ok = stun_test:allocate_udp(Addr, Port, Username, Realm, Password).

-spec stun_udp(config()) -> any().
stun_udp(_Config) ->
    Port = 34780,
    ct:pal("Performing STUN query against 127.0.0.1:~B (UDP)", [Port]),
    {ok, Addr} = inet:parse_address("127.0.0.1"),
    Result = stun_test:bind_udp(Addr, Port),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stun_tcp(config()) -> any().
stun_tcp(_Config) ->
    Port = 34780,
    ct:pal("Performing STUN query against 127.0.0.1:~B (TCP)", [Port]),
    {ok, Addr} = inet:parse_address("127.0.0.1"),
    Result = stun_test:bind_tcp(Addr, Port),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stun_tls(config()) -> any().
stun_tls(_Config) ->
    Port = 53490,
    ct:pal("Performing STUN query against 127.0.0.1:~B (TLS)", [Port]),
    {ok, Addr} = inet:parse_address("127.0.0.1"),
    Result = stun_test:bind_tls(Addr, Port),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stun_tcp_auto(config()) -> any().
stun_tcp_auto(_Config) ->
    Port = 34781,
    ct:pal("Performing STUN query against 127.0.0.1:~B (TCP)", [Port]),
    {ok, Addr} = inet:parse_address("127.0.0.1"),
    Result = stun_test:bind_tcp(Addr, Port),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stun_tls_auto(config()) -> any().
stun_tls_auto(_Config) ->
    Port = 34781,
    ct:pal("Performing STUN query against 127.0.0.1:~B (TLS)", [Port]),
    {ok, Addr} = inet:parse_address("127.0.0.1"),
    Result = stun_test:bind_tls(Addr, Port),
    true = is_tuple(Result),
    true = element(1, Result) =:= stun.

-spec stop_eturnal(config()) -> any().
stop_eturnal(_Config) ->
    ct:pal("Stopping eturnal"),
    ok = application:stop(eturnal).

%% Internal functions.

-spec set_eturnal_env(file:filename_all(), config()) -> config().
set_eturnal_env(ConfName, Config) ->
    DataDir = ?config(data_dir, Config),
    ConfFile = filename:join(DataDir, ConfName),
    ok = application:set_env(conf, file, ConfFile),
    ok = application:set_env(conf, on_fail, stop),
    ok = application:set_env(eturnal, on_fail, exit),
    Config.
