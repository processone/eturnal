%%% eturnal STUN/TURN server module.
%%%
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

%%% Note that returning some state() from the callback functions is optional,
%%% and so is exporting start/0 and/or stop/1 functions.

-module(mod_example).
-author('alice@example.com').
-behaviour(eturnal_module).
-export([start/0,
         stop/1,
         handle_event/3,
         options/0]).
-import(yval, [enum/1]).

-include_lib("kernel/include/logger.hrl").

-record(example_state,
        {sessions = #{} :: #{id() => integer()}}).

-type id() :: binary().
-type ret() :: ok | {ok, state()}.
-type state() :: #example_state{}.

%% API.

-spec start() -> ret().
start() ->
    ?LOG_DEBUG("Starting ~s", [?MODULE]),
    {ok, #example_state{}}.

-spec handle_event(eturnal_module:event(), eturnal_module:info(), state())
      -> ret().
handle_event(turn_session_start, Info, State) ->
    ?LOG_DEBUG("Handling 'turn_session_start' event: ~p", [Info]),
    on_turn_session_start(Info, State);
handle_event(turn_session_stop, Info, State) ->
    ?LOG_DEBUG("Handling 'turn_session_stop' event: ~p", [Info]),
    on_turn_session_stop(Info, State);
handle_event(Event, Info, State) -> % Ignore 'stun_query' events.
    ?LOG_DEBUG("Ignoring '~s' event: ~p", [Event, Info]),
    {ok, State}.

-spec stop(state()) -> ok.
stop(_State) ->
    ?LOG_DEBUG("Stopping ~s", [?MODULE]),
    ok.

%% Internal functions.

-spec on_turn_session_start(eturnal_module:info(), state()) -> ret().
on_turn_session_start(#{id := ID},
                      #example_state{sessions = Sessions0} = State) ->
    Unit = eturnal_module:get_opt(?MODULE, time_unit),
    Sessions = Sessions0#{ID => erlang:monotonic_time(Unit)},
    {ok, State#example_state{sessions = Sessions}}.

-spec on_turn_session_stop(eturnal_module:info(), state()) -> ret().
on_turn_session_stop(#{id := ID,
                       user := User,
                       client := AddrPort,
                       transport := Transport,
                       sent_bytes := Sent,
                       rcvd_bytes := Rcvd},
                     #example_state{sessions = Sessions} = State) ->
    case Sessions of
        #{ID := Start} ->
            KiB = round((Sent + Rcvd) / 1024),
            Unit = eturnal_module:get_opt(?MODULE, time_unit),
            Duration = erlang:monotonic_time(Unit) - Start,
            Client = eturnal_misc:addr_to_str(AddrPort),
            ?LOG_NOTICE("Relayed ~B KiB in ~B ~ss "
                        "[~s, session ~s, user ~s, client ~s]",
                        [KiB, Duration, Unit, Transport, ID, User, Client]),
            {ok, State#example_state{sessions = maps:remove(ID, Sessions)}};
        #{} ->
            ?LOG_WARNING("Got stop event for unknown TURN session ~s", [ID]),
            {ok, State}
    end.

-spec options() -> eturnal_module:options().
options() -> % See: https://github.com/processone/yval/blob/master/src/yval.erl
    {#{time_unit => enum([second, millisecond, microsecond, nanosecond])},
     [{required, []}, % List of required options.
      {defaults,
       #{time_unit => second}}]}.
