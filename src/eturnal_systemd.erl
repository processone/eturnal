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

-module(eturnal_systemd).
-behaviour(gen_server).
-export([ready/0,
         reloading/0,
         stopping/0]).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export_type([state/0]).

-include_lib("kernel/include/logger.hrl").

-record(systemd_state,
        {socket :: gen_udp:socket() | undefined,
         destination :: inet:local_address() | undefined,
         interval :: pos_integer() | undefined,
         timer :: reference() | undefined}).

-opaque state() :: #systemd_state{}.

%% API: send systemd notifications.

-spec ready() -> ok.
ready() ->
    cast_notification(<<"READY=1">>).

-spec reloading() -> ok.
reloading() ->
    cast_notification(<<"RELOADING=1">>).

-spec stopping() -> ok.
stopping() ->
    cast_notification(<<"STOPPING=1">>).

%% API: supervisor callback.

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API: gen_server callbacks.

-spec init(any()) -> {ok, state()} | {stop, term()}.
init(_Opts) ->
    process_flag(trap_exit, true),
    case os:getenv("NOTIFY_SOCKET") of
        [$@ | _Abstract] ->
            ?LOG_CRITICAL("Abstract NOTIFY_SOCKET not supported"),
            {stop, esocktnosupport};
        Path when is_list(Path), length(Path) > 0 ->
            ?LOG_DEBUG("Got NOTIFY_SOCKET: ~s", [Path]),
            Destination = {local, Path},
            case gen_udp:open(0, [local]) of
                {ok, Socket} ->
                    State = #systemd_state{socket = Socket,
                                           destination = Destination,
                                           interval = get_watchdog_interval()},
                    {ok, maybe_start_timer(State)};
                {error, Reason} ->
                    ?LOG_CRITICAL("Cannot open IPC socket: ~p", [Reason]),
                    {stop, Reason}
            end;
        _ ->
            ?LOG_INFO("Got no NOTIFY_SOCKET, notifications disabled"),
            {ok, #systemd_state{}}
    end.

-spec handle_call(term(), {pid(), term()}, state())
      -> {reply, {error, badarg}, state()}.
handle_call(Request, From, State) ->
    ?LOG_ERROR("Got unexpected request from ~p: ~p", [From, Request]),
    {reply, {error, badarg}, State}.

-spec handle_cast({notify, binary()} | term(), state()) -> {noreply, state()}.
handle_cast({notify, Notification},
            #systemd_state{destination = undefined} = State) ->
    ?LOG_DEBUG("No NOTIFY_SOCKET, dropping ~s notification", [Notification]),
    {noreply, State};
handle_cast({notify, Notification}, State) ->
    try notify(State, Notification)
    catch _:Err ->
            ?LOG_ERROR("Cannot send ~s notification: ~p", [Notification, Err])
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    ?LOG_ERROR("Got unexpected message: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(ping_watchdog | term(), state()) -> {noreply, state()}.
handle_info(ping_watchdog, #systemd_state{interval = Interval} = State)
  when is_integer(Interval), Interval > 0 ->
    try notify(State, <<"WATCHDOG=1">>)
    catch _:Err ->
            ?LOG_ERROR("Cannot ping watchdog: ~p", [Err])
    end,
    {noreply, start_timer(State)};
handle_info(Info, State) ->
    ?LOG_ERROR("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> ok.
terminate(Reason, State) ->
    ?LOG_DEBUG("Terminating ~s (~p)", [?MODULE, Reason]),
    cancel_timer(State),
    close_socket(State).

-spec code_change({down, term()} | term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    ?LOG_INFO("Got code change request"),
    {ok, State}.

%% Internal functions.

-spec get_watchdog_interval() -> integer() | undefined.
get_watchdog_interval() ->
    case os:getenv("WATCHDOG_USEC") of
        WatchdogUSec when is_list(WatchdogUSec), length(WatchdogUSec) > 0 ->
            Interval = round(0.5 * list_to_integer(WatchdogUSec)),
            ?LOG_DEBUG("Watchdog interval: ~B microseconds", [Interval]),
            erlang:convert_time_unit(Interval, microsecond, millisecond);
        _ ->
            undefined
    end.

-spec maybe_start_timer(state()) -> state().
maybe_start_timer(#systemd_state{interval = Interval} = State)
  when is_integer(Interval), Interval > 0 ->
    ?LOG_INFO("Watchdog notifications enabled"),
    start_timer(State);
maybe_start_timer(State) ->
    ?LOG_INFO("Watchdog notifications disabled"),
    State.

-spec start_timer(state()) -> state().
start_timer(#systemd_state{interval = Interval} = State) ->
    ?LOG_DEBUG("Pinging watchdog in ~B milliseconds", [Interval]),
    Timer = erlang:send_after(Interval, self(), ping_watchdog),
    State#systemd_state{timer = Timer}.

-spec cancel_timer(state()) -> ok.
cancel_timer(#systemd_state{timer = undefined}) ->
    ok;
cancel_timer(#systemd_state{timer = Timer}) ->
    ?LOG_DEBUG("Cancelling watchdog timer"),
    ok = erlang:cancel_timer(Timer, [{info, false}]).

-spec notify(state(), binary()) -> ok.
notify(#systemd_state{socket = Socket, destination = Destination},
       Notification) ->
    ?LOG_DEBUG("Notifying systemd: ~s", [Notification]),
    ok = gen_udp:send(Socket, Destination, 0, Notification).

-spec cast_notification(binary()) -> ok.
cast_notification(Notification) ->
    ok = gen_server:cast(?MODULE, {notify, Notification}).

-spec close_socket(state()) -> ok.
close_socket(#systemd_state{socket = undefined}) ->
    ok;
close_socket(#systemd_state{socket = Socket}) ->
    ?LOG_DEBUG("Closing NOTIFY_SOCKET"),
    ok = gen_udp:close(Socket).
