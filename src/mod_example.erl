%%% eturnal STUN/TURN server module.
%%%
%%% Copyright (c) 2020 Alice Wonder <alice@example.com>.
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

%%% This example module logs the duration and some metadata of TURN sessions.

%%% See: https://eturnal.net/documentation/code/eturnal_module.html

-module(mod_example).
-author('alice@example.com').
-behaviour(eturnal_module).
-export([start/0,
         stop/0,
         handle_event/2,
         options/0]).
-import(yval, [enum/1]).

-include_lib("kernel/include/logger.hrl").

%% API.

-spec start() -> ok | {ok, eturnal_module:events()}.
start() ->
    ?LOG_DEBUG("Starting ~s", [?MODULE]),
    {ok, [turn_session_stop]}. % Subscribe to this list of events.

-spec handle_event(eturnal_module:event(), eturnal_module:info()) -> ok.
handle_event(turn_session_stop, Info) ->
    ?LOG_DEBUG("Handling 'turn_session_stop' event: ~p", [Info]),
    on_turn_session_stop(Info).

-spec stop() -> ok.
stop() ->
    ?LOG_DEBUG("Stopping ~s", [?MODULE]),
    ok.

%% Internal functions.

-spec on_turn_session_stop(eturnal_module:info()) -> ok.
on_turn_session_stop(#{id := ID,
                       user := User,
                       client := AddrPort,
                       transport := Transport,
                       sent_bytes := Sent,
                       rcvd_bytes := Rcvd,
                       duration := Duration0}) ->
    KiB = round((Sent + Rcvd) / 1024),
    Unit = eturnal_module:get_opt(?MODULE, time_unit),
    Duration = erlang:convert_time_unit(Duration0, native, Unit),
    Client = eturnal_misc:addr_to_str(AddrPort),
    ?LOG_NOTICE("Relayed ~B KiB in ~B ~ss [~s, session ~s, user ~s, client ~s]",
                [KiB, Duration, Unit, Transport, ID, User, Client]),
    ok.

-spec options() -> eturnal_module:options().
options() -> % See: https://github.com/processone/yval/blob/master/src/yval.erl
    {#{time_unit => enum([second, millisecond, microsecond, nanosecond])},
     [{required, []}, % List of required options.
      {defaults,
       #{time_unit => second}}]}.
