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

-module(eturnal_systemd).
-behaviour(gen_server).
-export([start_link/0,
         ready/0,
         reloading/0,
         stopping/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(systemd_state,
        {socket :: gen_udp:socket() | undefined,
         destination :: inet:local_address() | undefined,
         interval :: pos_integer() | undefined,
         last_ping :: integer() | undefined}).

-type watchdog_timeout() :: pos_integer() | hibernate.
-type state() :: #systemd_state{}.

%% API.

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec ready() -> ok.
ready() ->
    cast_notification(ready).

-spec reloading() -> ok.
reloading() ->
    cast_notification(reloading).

-spec stopping() -> ok.
stopping() ->
    cast_notification(stopping).

%% Behaviour callbacks.

-spec init(any())
      -> {ok, state()} | {ok, state(), watchdog_timeout()} | {error, term()}.
init(_Opts) ->
    process_flag(trap_exit, true),
    case os:getenv("NOTIFY_SOCKET") of
        [$@ | _Abstract] ->
            ?LOG_CRITICAL("Abstract NOTIFY_SOCKET not supported"),
            {error, esocktnosupport};
        Path when is_list(Path), length(Path) > 0 ->
            ?LOG_DEBUG("Got NOTIFY_SOCKET: ~s", [Path]),
            Destination = {local, Path},
            case gen_udp:open(0, [local]) of
                {ok, Socket} ->
                    Interval = get_watchdog_interval(),
                    State = #systemd_state{socket = Socket,
                                           destination = Destination,
                                           interval = Interval},
                    if is_integer(Interval), Interval > 0 ->
                            ?LOG_INFO("Watchdog notifications enabled"),
                            {ok, set_last_ping(State), Interval};
                       true ->
                            ?LOG_INFO("Watchdog notifications disabled"),
                            {ok, State}
                    end;
                {error, Reason} = Err ->
                    ?LOG_CRITICAL("Cannot open IPC socket: ~p", [Reason]),
                    Err
            end;
        _ ->
            ?LOG_INFO("Got no NOTIFY_SOCKET, notifications disabled"),
            {ok, #systemd_state{}}
    end.

-spec handle_call(term(), {pid(), term()}, state())
      -> {reply, {error, badarg}, state(), watchdog_timeout()}.
handle_call(Request, From, State) ->
    ?LOG_ERROR("Got unexpected request from ~p: ~p", [From, Request]),
    {reply, {error, badarg}, State, get_timeout(State)}.

-spec handle_cast({notify, binary()} | term(), state())
      -> {noreply, state(), watchdog_timeout()}.
handle_cast({notify, Notification},
            #systemd_state{destination = undefined} = State) ->
    ?LOG_DEBUG("No NOTIFY_SOCKET, dropping '~s' notification", [Notification]),
    {noreply, State, get_timeout(State)};
handle_cast({notify, Notification}, State) ->
    try notify(State, Notification)
    catch _:Err ->
            ?LOG_ERROR("Cannot send '~s' notification: ~p", [Notification, Err])
    end,
    {noreply, State, get_timeout(State)};
handle_cast(Msg, State) ->
    ?LOG_ERROR("Got unexpected message: ~p", [Msg]),
    {noreply, State, get_timeout(State)}.

-spec handle_info(timeout | term(), state())
      -> {noreply, state(), watchdog_timeout()}.
handle_info(timeout, #systemd_state{interval = Interval} = State)
  when is_integer(Interval), Interval > 0 ->
    try notify(State, watchdog)
    catch _:Err ->
            ?LOG_ERROR("Cannot ping watchdog: ~p", [Err])
    end,
    {noreply, set_last_ping(State), Interval};
handle_info(Info, State) ->
    ?LOG_ERROR("Got unexpected info: ~p", [Info]),
    {noreply, State, get_timeout(State)}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> ok.
terminate(Reason, #systemd_state{socket = undefined}) ->
    ?LOG_DEBUG("Terminating ~s (~p)", [?MODULE, Reason]),
    ok;
terminate(Reason, #systemd_state{socket = Socket}) ->
    ?LOG_DEBUG("Closing socket and terminating ~s (~p)", [?MODULE, Reason]),
    ok = gen_udp:close(Socket).

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    ?LOG_INFO("Got code change request"),
    {ok, State}.

%% Internal functions.

-spec get_watchdog_interval() -> integer() | undefined.
get_watchdog_interval() ->
    case os:getenv("WATCHDOG_USEC") of
        WatchdogUSec when is_list(WatchdogUSec), length(WatchdogUSec) > 0 ->
            Interval = erlang:round(0.5 * list_to_integer(WatchdogUSec)),
            ?LOG_DEBUG("Watchdog interval: ~B microseconds", [Interval]),
            erlang:convert_time_unit(Interval, microsecond, millisecond);
        _ ->
            undefined
    end.

-spec get_timeout(state()) -> watchdog_timeout().
get_timeout(#systemd_state{interval = undefined}) ->
    hibernate;
get_timeout(#systemd_state{interval = Interval, last_ping = LastPing}) ->
    case Interval - LastPing + erlang:monotonic_time(millisecond) of
        Timeout when Timeout > 0 ->
            ?LOG_DEBUG("Calculated new timeout value: ~B", [Timeout]),
            Timeout;
        _ ->
            ?LOG_DEBUG("Calculated new timeout value: 1"),
            1
    end.

-spec set_last_ping(state()) -> state().
set_last_ping(State) ->
    LastPing = erlang:monotonic_time(millisecond),
    State#systemd_state{last_ping = LastPing}.

-spec notify(state(), ready | reloading | stopping | watchdog | binary()) -> ok.
notify(State, ready) ->
    notify(State, <<"READY=1">>);
notify(State, reloading) ->
    notify(State, <<"RELOADING=1">>);
notify(State, stopping) ->
    notify(State, <<"STOPPING=1">>);
notify(State, watchdog) ->
    notify(State, <<"WATCHDOG=1">>);
notify(#systemd_state{socket = Socket, destination = Destination}, Notification)
  when is_binary(Notification) ->
    ?LOG_DEBUG("Notifying systemd: ~s", [Notification]),
    ok = gen_udp:send(Socket, Destination, 0, Notification).

-spec cast_notification(ready | reloading | stopping) -> ok.
cast_notification(Notification) ->
    gen_server:cast(?MODULE, {notify, Notification}).
