%%% eturnal STUN/TURN server module.
%%%
%%% Copyright (c) 2020 Marc Schink <dev@zapb.de>.
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

%%% This module logs STUN/TURN events with some metadata into an InfluxDB
%%% database.

-module(mod_stats_influx).
-behaviour(eturnal_module).
-export([start/0,
         stop/0,
         handle_event/2,
         options/0]).
-import(yval, [either/2, ip/0, non_empty/1, port/0, string/0]).

-include_lib("kernel/include/logger.hrl").
-define(INFLUX_POOL, stats_influx_pool).

%% API.

-spec start() -> {ok, eturnal_module:events()}.
start() ->
    ?LOG_DEBUG("Starting ~s", [?MODULE]),
    ok = eturnal_module:ensure_deps(?MODULE, [influx_udp]),
    Host = eturnal_module:get_opt(?MODULE, host),
    Port = eturnal_module:get_opt(?MODULE, port),
    Events = [turn_session_stop, stun_query],
    case influx_udp:start_pool(?INFLUX_POOL, #{host => Host,
                                               port => Port,
                                               pool_size => 1}) of
        {ok, _} ->
            {ok, Events};
        {error, {already_started, _}} ->
            {ok, Events};
        {error, Reason} ->
            exit(Reason)
    end.

-spec handle_event(eturnal_module:event(), eturnal_module:info()) -> ok.
handle_event(stun_query, Info) ->
    on_stun_query(Info);
handle_event(turn_session_stop, Info) ->
    on_turn_session_stop(Info).

-spec stop() -> ok.
stop() ->
    ?LOG_DEBUG("Stopping ~s", [?MODULE]),
    ok.

-spec options() -> eturnal_module:options().
options() ->
    {#{host => either(ip(), non_empty(string())),
       port => port()},
     [{defaults,
       #{host => "localhost",
         port => 8089}}]}.

%% Internal functions.

-spec on_stun_query(eturnal_module:info()) -> ok.
on_stun_query(#{transport := Transport}) ->
    ?LOG_DEBUG("Writing STUN query event to InfluxDB"),
    Points = [{type, <<"stun">>},
              {transport, string:lowercase(Transport)}],
    ok = influx_udp:write_to(?INFLUX_POOL, <<"events">>, Points).

-spec on_turn_session_stop(eturnal_module:info()) -> ok.
on_turn_session_stop(#{id := ID,
                       transport := Transport,
                       sent_bytes := Sent,
                       rcvd_bytes := Rcvd,
                       duration := Duration0}) ->
    ?LOG_DEBUG("Writing stats of TURN session ~s to InfluxDB", [ID]),
    Duration = erlang:convert_time_unit(Duration0, native, microsecond),
    Points = [{type, <<"turn">>},
              {transport, string:lowercase(Transport)},
              {duration, Duration},
              {sent_bytes, Sent},
              {rcvd_bytes, Rcvd}],
    ok = influx_udp:write_to(?INFLUX_POOL, <<"events">>, Points).
