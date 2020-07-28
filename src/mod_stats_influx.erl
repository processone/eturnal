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

%%% This module logs STUN/TURN events with some metadata into an InfluxDB database.
%%% The module is enabled by adding the following settings to the eturnal.yml
%%% file:
%%%
%%% eturnal:
%%%   modules:
%%%     mod_stats_influx:
%%%      host: "localhost"
%%%      port: 8089

-module(mod_stats_influx).
-author('dev@zapb.de').
-behaviour(eturnal_module).
-export([start/0,
         stop/1,
         handle_event/3,
         options/0]).
-import(yval, [port/0, string/0]).

-include_lib("kernel/include/logger.hrl").
-define(INFLUX_POOL, events_influx_pool).

-record(events_influx_state,
        {sessions = #{} :: #{id() => integer()}}).

-type id() :: binary().
-type ret() :: ok | {ok, state()}.
-type state() :: #events_influx_state{}.

%% API.

-spec start() -> ret().
start() ->
    ?LOG_DEBUG("Starting ~s", [?MODULE]),
    eturnal_module:ensure_deps(?MODULE, [influx_udp]),
    Host = eturnal_module:get_opt(?MODULE, host),
    Port = eturnal_module:get_opt(?MODULE, port),
    {ok, _} = influx_udp:start_pool(?INFLUX_POOL, #{host => Host, port => Port, pool_size => 1}),
    {ok, #events_influx_state{}}.

-spec handle_event(eturnal_module:event(), eturnal_module:info(), state())
      -> ret().
handle_event(turn_session_start, Info, State) ->
    on_turn_session_start(Info, State);
handle_event(turn_session_stop, Info, State) ->
    on_turn_session_stop(Info, State);
handle_event(stun_query, Info, State) ->
    on_stun_query(Info, State);
handle_event(_Event, _Info, State) ->
    {ok, State}.

-spec stop(state()) -> ok.
stop(_State) ->
    ?LOG_DEBUG("Stopping ~s", [?MODULE]),
    ok.

%% Internal functions.

-spec on_turn_session_start(eturnal_module:info(), state()) -> ret().
on_turn_session_start(#{id := Id},
                      #events_influx_state{sessions = Sessions0} = State) ->
    Sessions = Sessions0#{Id => erlang:system_time(microsecond)},
    {ok, State#events_influx_state{sessions = Sessions}}.

-spec on_turn_session_stop(eturnal_module:info(), state()) -> ret().
on_turn_session_stop(#{id := Id,
                       transport := Transport,
                       sent_bytes := Sent,
                       rcvd_bytes := Rcvd},
                     #events_influx_state{sessions = Sessions} = State) ->
    case Sessions of
        #{Id := Start} ->
            Duration = erlang:system_time(microsecond) - Start,
	    Points = [{type, "turn"}, {transport, string:lowercase(Transport)}, {duration, Duration},
		      {sent_bytes, Sent}, {rcvd_bytes, Rcvd}],
            ok = influx_udp:write_to(?INFLUX_POOL, "events", Points),
            {ok, State#events_influx_state{sessions = maps:remove(Id, Sessions)}};
        #{} ->
            ?LOG_WARNING("Got stop event for unknown TURN session ~s", [Id]),
            {ok, State}
    end.

-spec on_stun_query(eturnal_module:info(), state()) -> ret().
on_stun_query(#{transport := Transport}, State) ->
    ok = influx_udp:write_to(?INFLUX_POOL, "events", [{type, "stun"}, {transport, string:lowercase(Transport)}]),
    {ok, State}.

-spec options() -> eturnal_module:options().
options() ->
    {#{host => string(),
       port => port()},
     [{required, [host, port]},
      {defaults, #{}}]}.
