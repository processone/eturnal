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

%% API.

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(start_eturnal, Config) ->
    DataDir = ?config(data_dir, Config),
    ConfFile = filename:join(DataDir, "eturnal.yml"),
    ok = application:set_env(conf, file, ConfFile),
    ok = application:set_env(conf, on_fail, stop),
    ok = application:set_env(eturnal, on_fail, stop),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [start_eturnal,
     check_version,
     connect_ipv4,
     connect_ipv6,
     stop_eturnal].

start_eturnal(_Config) ->
    {ok, _} = application:ensure_all_started(eturnal).

check_version(_Config) ->
    {ok, _Version} = eturnal_ctl:get_version().

connect_ipv4(_Config) ->
    {ok, Addr} = inet:parse_address("127.0.0.1"),
    ok = connect(Addr).

connect_ipv6(_Config) ->
    {ok, Addr} = inet:parse_address("::1"),
    ok = connect(Addr).

stop_eturnal(_Config) ->
    ok = application:stop(eturnal).

%% Internal functions.

connect(Addr) ->
    {ok, Sock} = gen_tcp:connect(Addr, 34780, []),
    ok = gen_tcp:close(Sock).
