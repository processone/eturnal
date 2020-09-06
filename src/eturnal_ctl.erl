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

-module(eturnal_ctl).
-author('holger@zedat.fu-berlin.de').
-export([get_sessions/0,
         get_info/0,
         get_version/0,
         get_loglevel/0,
         set_loglevel/1,
         reload/0]).

-include_lib("kernel/include/logger.hrl").

-type sock_mod() :: gen_udp | gen_tcp | fast_tls.
-type addr() :: inet:ip_address().
-type addr_port() :: {inet:ip_address(), inet:port_number()}.
-type session() :: {binary(), sock_mod(), addr_port(), addr_port(), [addr()],
                    [addr_port()], non_neg_integer(), non_neg_integer(),
                    non_neg_integer(), non_neg_integer(), integer()}.
-type node_info() :: {binary(), {string(), string()}, non_neg_integer(),
                      non_neg_integer(), non_neg_integer(), non_neg_integer(),
                      non_neg_integer(), non_neg_integer()}.

%% API.

-spec get_sessions() -> {ok, string()} | {error, string()}.
get_sessions() ->
    ?LOG_DEBUG("Handling API call: get_sessions()"),
    case query_sessions() of
        [_ | _] = Sessions ->
            Header = io_lib:format("~B active TURN sessions:",
                                   [length(Sessions)]),
            Output = lists:join([nl(), nl()],
                                [Header | format_sessions(Sessions)]),
            {ok, unicode:characters_to_list(Output)};
        [] ->
            {ok, "No active TURN sessions"}
    end.

-spec get_info() -> {ok, string()} | {error, string()}.
get_info() ->
    ?LOG_DEBUG("Handling API call: get_info()"),
    case call(get_info) of
        {ok, Info} ->
            {ok, unicode:characters_to_list(format_info(Info))};
        {error, timeout} ->
            {error, "Querying eturnal timed out"}
    end.

-spec get_version() -> {ok, string()} | {error, string()}.
get_version() ->
    ?LOG_DEBUG("Handling API call: get_version()"),
    case call(get_version) of
        {ok, Version} ->
            {ok, unicode:characters_to_list(Version)};
        {error, timeout} ->
            {error, "Querying eturnal timed out"}
    end.

-spec get_loglevel() -> {ok, string()} | {error, string()}.
get_loglevel() ->
    ?LOG_DEBUG("Handling API call: get_loglevel()"),
    case call(get_loglevel) of
        {ok, Level} ->
            {ok, atom_to_list(Level)};
        {error, timeout} ->
            {error, "Querying eturnal timed out"}
    end.

-spec set_loglevel(term()) -> ok | {error, string()}.
set_loglevel(Level) when is_atom(Level) ->
    ?LOG_DEBUG("Handling API call: set_loglevel(~s)", [Level]),
    case eturnal_logger:is_valid_level(Level) of
        true ->
            case call({set_loglevel, Level}) of
                ok ->
                    ok;
                {error, timeout} ->
                    {error, "Querying eturnal timed out"}
            end;
        false ->
            {error, "Not a valid log level: " ++ atom_to_list(Level)}
    end;
set_loglevel(Level) ->
    ?LOG_DEBUG("Invalid API call: set_loglevel(~p)", [Level]),
    {error, "Log level must be specified as an 'atom'"}.

-spec reload() -> ok | {error, string()}.
reload() ->
    ?LOG_DEBUG("Handling API call: reload()"),
    case call(reload) of
        ok ->
            ok;
        {error, timeout} ->
            {error, "Querying eturnal timed out"};
        {error, Reason} ->
            {error, conf:format_error(Reason)}
    end.

%% Internal functions.

-spec query_state(pid()) -> tuple().
query_state(PID) -> % Until we add a proper API to 'stun'.
    {value, State} = lists:search(
                       fun(E) ->
                               is_tuple(E) andalso element(1, E) =:= state
                       end, sys:get_state(PID)),
    State.

-spec query_sessions() -> [session()].
query_sessions() ->
    lists:map(
      fun({_, PID, worker, _}) ->
              State = query_state(PID),
              User = element(6, State),
              SockMod = element(2, State),
              ClientAddr = element(4, State),
              RelayAddr = element(18, State),
              PermMap = element(12, State),
              PeerMap = element(10, State),
              SentBytes = element(29, State),
              SentPkts = element(30, State),
              RcvdBytes = element(27, State),
              RcvdPkts = element(28, State),
              Start = element(31, State),
              {User, SockMod, ClientAddr, RelayAddr, maps:keys(PermMap),
               maps:keys(PeerMap), SentBytes, SentPkts, RcvdBytes, RcvdPkts,
               Start}
      end, supervisor:which_children(turn_tmp_sup)).

-spec format_sessions([session()]) -> iolist().
format_sessions(Sessions) ->
    lists:map(
      fun({User, SockMod, ClientAddr, RelayAddr, PermAddrs, PeerAddrs,
           SentBytes, SentPkts, RcvdBytes, RcvdPkts, Start}) ->
              Duration0 = erlang:monotonic_time() - Start,
              Duration = erlang:convert_time_unit(Duration0, native, second),
              Transport = format_transport(SockMod),
              Client = eturnal_misc:addr_to_str(ClientAddr),
              Relay = eturnal_misc:addr_to_str(RelayAddr),
              Peers = format_addrs(PeerAddrs),
              Perms = format_addrs(PermAddrs),
              io_lib:format(
                "-- TURN session of ~s --~s"
                "          Client: ~s (~s)~s"
                "           Relay: ~s (UDP)~s"
                "   Permission(s): ~s (UDP)~s"
                "         Peer(s): ~s (UDP)~s"
                "            Sent: ~B KiB (~B packets)~s"
                "        Received: ~B KiB (~B packets)~s"
                "     Running for: ~B seconds",
                [User, nl(), Client, Transport, nl(),
                 Relay, nl(), Perms, nl(), Peers, nl(),
                 round(SentBytes / 1024), SentPkts, nl(),
                 round(RcvdBytes / 1024), RcvdPkts, nl(), Duration])
      end, Sessions).

-spec format_info(node_info()) -> io_lib:chars().
format_info({EturnalVsn, {OtpVsn, ErtsVsn}, Uptime, Sessions, Procs, QueueLen,
             Reductions, Memory}) ->
    MiB = round(Memory / 1024 / 1024),
    Seconds = erlang:convert_time_unit(Uptime, millisecond,  second),
    {Ds, {Hs, Ms, Ss}} = calendar:seconds_to_daystime(Seconds),
    io_lib:format(
      "eturnal ~s on Erlang/OTP ~s (ERTS ~s)~s"
      "Uptime: ~B days, ~B hours, ~B minutes, ~B seconds~s"
      "Active TURN sessions: ~B~s"
      "Processes: ~B~s"
      "Total length of run queues: ~B~s"
      "Total CPU usage (reductions): ~B~s"
      "Allocated memory (MiB): ~B",
      [EturnalVsn, OtpVsn, ErtsVsn, nl(), Ds, Hs, Ms, Ss, nl(), Sessions, nl(),
       Procs, nl(), QueueLen, nl(), Reductions, nl(), MiB]).

-spec format_transport(sock_mod()) -> binary().
format_transport(gen_udp) ->
    <<"UDP">>;
format_transport(gen_tcp) ->
    <<"TCP">>;
format_transport(fast_tls) ->
    <<"TLS">>.

-spec format_addrs([addr() | addr_port()]) -> iolist() | binary().
format_addrs([]) ->
    <<"none">>;
format_addrs(PeerAddrs) ->
    lists:join(", ", lists:map(fun eturnal_misc:addr_to_str/1, PeerAddrs)).

-spec nl() -> string().
nl() ->
    [$~, $n]. % Let the caller convert "~n"s to actual newline characters.

-spec call(term()) -> ok | {error, term()}.
call(Request) ->
    try gen_server:call(eturnal, Request) of
        ok ->
            ?LOG_DEBUG("eturnal call (~p) returned ok", [Request]),
            ok;
        {ok, _Value} = Result ->
            ?LOG_DEBUG("eturnal call (~p) returned ~p", [Request, Result]),
            Result;
        {error, _Reason} = Err ->
            ?LOG_DEBUG("eturnal call (~p) returned ~p", [Request, Err]),
            Err
    catch exit:{timeout, _} ->
            ?LOG_DEBUG("eturnal call (~p) timed out", [Request]),
            {error, timeout}
    end.
