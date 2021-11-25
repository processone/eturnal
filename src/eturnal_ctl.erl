%%% eturnal STUN/TURN server.
%%%
%%% Copyright (c) 2020, 2021 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2020, 2021 ProcessOne, SARL.
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
-export([get_credentials/0,
         get_credentials/1,
         get_credentials/2,
         get_password/1,
         get_sessions/0,
         get_info/0,
         get_version/0,
         get_loglevel/0,
         set_loglevel/1,
         reload/0]).

-include_lib("kernel/include/logger.hrl").
-include("eturnal.hrl").
-define(DEFAULT_DURATION, "1d"). % For get_credentials/0.

-type sock_mod() :: gen_udp | gen_tcp | fast_tls.
-type addr() :: inet:ip_address().
-type addr_port() :: {inet:ip_address(), inet:port_number()}.

-record(session,
        {user :: binary(),
         sock_mod :: sock_mod(),
         client_addr :: addr_port(),
         relay_addr :: addr_port(),
         perm_addrs :: [addr()],
         peer_addrs :: [addr_port()],
         sent_bytes :: non_neg_integer(),
         sent_pkts :: non_neg_integer(),
         rcvd_bytes :: non_neg_integer(),
         rcvd_pkts :: non_neg_integer(),
         start_time :: integer()}).

-type session() :: #session{}.

%% API.

-spec get_credentials() -> {ok, string()} | {error, string()}.
get_credentials() ->
    ?LOG_DEBUG("Handling API call: get_credentials()"),
    {ok, Username} = make_username(?DEFAULT_DURATION),
    case call({get_password, Username}) of
        {ok, Password} ->
            Credentials = format_credentials(Username, Password),
            {ok, unicode:characters_to_list(Credentials)};
        {error, no_secret} ->
            {error, "No shared secret"};
        {error, timeout} ->
            {error, "Querying eturnal timed out"}
    end.

-spec get_credentials(term()) -> {ok, string()} | {error, string()}.
get_credentials(Expiry) when is_list(Expiry) ->
    ?LOG_DEBUG("Handling API call: get_credentials(~p)", [Expiry]),
    case make_username(Expiry) of
        {ok, Username} ->
            case call({get_password, Username}) of
                {ok, Password} ->
                    Credentials = format_credentials(Username, Password),
                    {ok, unicode:characters_to_list(Credentials)};
                {error, no_secret} ->
                    {error, "No shared secret"};
                {error, timeout} ->
                    {error, "Querying eturnal timed out"}
            end;
        {error, badarg} ->
            ?LOG_DEBUG("Invalid expiry: ~p", [Expiry]),
            {error, "Invalid expiry"}
    end;
get_credentials(Expiry) ->
    ?LOG_DEBUG("Invalid API call: get_credentials(~p)", [Expiry]),
    {error, "Expiry must be specified as a string"}.

-spec get_credentials(term(), term()) -> {ok, string()} | {error, string()}.
get_credentials(Expiry, Suffix) when is_list(Expiry), is_list(Suffix) ->
    ?LOG_DEBUG("Handling API call: get_credentials(~p, ~p)", [Expiry, Suffix]),
    case make_username(Expiry, Suffix) of
        {ok, Username} ->
            case call({get_password, Username}) of
                {ok, Password} ->
                    Credentials = format_credentials(Username, Password),
                    {ok, unicode:characters_to_list(Credentials)};
                {error, no_secret} ->
                    {error, "No shared secret"};
                {error, timeout} ->
                    {error, "Querying eturnal timed out"}
            end;
        {error, badarg} ->
            ?LOG_DEBUG("Invalid argument(s): ~p:~p", [Expiry, Suffix]),
            {error, "Invalid expiry or suffix"}
    end;
get_credentials(Expiry, Suffix) ->
    ?LOG_DEBUG("Invalid API call: get_credentials(~p, ~p)", [Expiry, Suffix]),
    {error, "Expiry and suffix must be specified as strings"}.

-spec get_password(term()) -> {ok, string()} | {error, string()}.
get_password(Username0) when is_list(Username0) ->
    ?LOG_DEBUG("Handling API call: get_password(~p)", [Username0]),
    case unicode:characters_to_binary(Username0) of
        Username when is_binary(Username) ->
            case is_valid_username(Username) of
                true ->
                    case call({get_password, Username}) of
                        {ok, Password} ->
                            {ok, unicode:characters_to_list(Password)};
                        {error, no_secret} ->
                            {error, "No shared secret"};
                        {error, timeout} ->
                            {error, "Querying eturnal timed out"}
                    end;
                false ->
                    ?LOG_DEBUG("Invalid user name: ~s", [Username]),
                    {error, "Invalid user name: " ++ Username0}
            end;
        {_, _, _} ->
            ?LOG_DEBUG("Cannot convert user name to binary: ~p", [Username0]),
            {error, "User name must be specified as a string"}
    end;
get_password(Username) ->
    ?LOG_DEBUG("Invalid API call: get_password(~p)", [Username]),
    {error, "User name must be specified as a string"}.

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
            ?LOG_DEBUG("Invalid log level: ~s", [Level]),
            {error, "Not a valid log level: " ++ atom_to_list(Level)}
    end;
set_loglevel(Level) ->
    ?LOG_DEBUG("Invalid API call: set_loglevel(~p)", [Level]),
    {error, "Log level must be specified as an atom"}.

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

-spec is_valid_username(binary()) -> boolean().
is_valid_username(Username) ->
    case string:to_integer(Username) of
        {N, <<":", _Rest/binary>>} when is_integer(N), N > 0 ->
            true;
        {N, <<>>} when is_integer(N), N > 0 ->
            true;
        {_, _} ->
            false
    end.

-spec make_username(string()) -> {ok, binary()} | {error, badarg}.
make_username(Expiry) ->
    make_username(Expiry, []).

-spec make_username(string(), string())
      -> {ok, binary()} | {error, badarg}.
make_username(Expiry, Suffix) ->
    try calendar:rfc3339_to_system_time(Expiry) of
        Time ->
            username_from_timestamp(Time, Suffix)
    catch _:{badmatch, _} ->
            username_from_expiry(Expiry, Suffix)
    end.

-spec username_from_timestamp(integer(), string())
      -> {ok, binary()} | {error, badarg}.
username_from_timestamp(Time, [])  ->
    Username = integer_to_binary(Time),
    {ok, Username};
username_from_timestamp(Time, Suffix) ->
    Username = io_lib:format("~B:~s", [Time, Suffix]),
    {ok, unicode:characters_to_binary(Username)}.

-spec username_from_expiry(string(), string())
      -> {ok, binary()} | {error, badarg}.
username_from_expiry(Expiry0, Suffix) ->
    case {unicode:characters_to_binary(Expiry0),
          io_lib:printable_unicode_list(Suffix)} of
        {Expiry, true} when is_binary(Expiry) ->
            case parse_expiry(Expiry) of
                {ok, ExpirySecs} ->
                    Time = erlang:system_time(second) + ExpirySecs,
                    username_from_timestamp(Time, Suffix);
                {error, badarg} = Err ->
                    Err
            end;
        {_, _} ->
            {error, badarg}
    end.

-spec parse_expiry(binary()) -> {ok, pos_integer()} | {error, badarg}.
parse_expiry(Expiry) ->
    case string:to_integer(string:trim(Expiry)) of
        {N, <<>>} when is_integer(N), N > 0 ->
            {ok, N};
        {N, <<"s">>} when is_integer(N), N > 0 ->
            {ok, N};
        {N, <<"m">>} when is_integer(N), N > 0 ->
            {ok, N * 60};
        {N, <<"h">>} when is_integer(N), N > 0 ->
            {ok, N * 3600};
        {N, <<"d">>} when is_integer(N), N > 0 ->
            {ok, N * 86400};
        {_, _} ->
            {error, badarg}
    end.

-spec query_state(pid()) -> tuple().
query_state(PID) -> % Until we add a proper API to 'stun'.
    {value, State} = lists:search(
                       fun(E) ->
                               is_tuple(E) andalso element(1, E) =:= state
                       end, sys:get_state(PID)),
    State.

-spec query_sessions() -> [session()].
query_sessions() ->
    lists:filtermap(
      fun({_, PID, worker, _}) ->
              try query_state(PID) of
                  State ->
                      Session = #session{
                                   user = element(6, State),
                                   sock_mod = element(2, State),
                                   client_addr = element(4, State),
                                   relay_addr = element(18, State),
                                   perm_addrs = maps:keys(element(12, State)),
                                   peer_addrs = maps:keys(element(10, State)),
                                   sent_bytes = element(29, State),
                                   sent_pkts = element(30, State),
                                   rcvd_bytes = element(27, State),
                                   rcvd_pkts = element(28, State),
                                   start_time = element(31, State)},
                      {true, Session}
              catch exit:{Reason, _} when Reason =:= noproc;
                                          Reason =:= normal;
                                          Reason =:= shutdown;
                                          Reason =:= killed;
                                          Reason =:= timeout ->
                      ?LOG_DEBUG("Cannot query TURN session ~p: ~s",
                                 [PID, Reason]),
                      false
              end
      end, supervisor:which_children(turn_tmp_sup)).

-spec format_sessions([session()]) -> iolist().
format_sessions(Sessions) ->
    lists:map(
      fun(#session{user = User,
                   sock_mod = SockMod,
                   client_addr = ClientAddr,
                   relay_addr = RelayAddr,
                   perm_addrs = PermAddrs,
                   peer_addrs = PeerAddrs,
                   sent_bytes = SentBytes,
                   sent_pkts = SentPkts,
                   rcvd_bytes = RcvdBytes,
                   rcvd_pkts = RcvdPkts,
                   start_time = StartTime}) ->
              Duration0 = erlang:monotonic_time() - StartTime,
              Duration = erlang:convert_time_unit(Duration0, native, second),
              Transport = format_transport(SockMod),
              Client = eturnal_misc:addr_to_str(ClientAddr),
              Relay = eturnal_misc:addr_to_str(RelayAddr),
              Peers = format_addrs(PeerAddrs),
              Perms = format_addrs(PermAddrs),
              io_lib:format(
                "-- TURN session of ~ts --~s"
                "          Client: ~s (~s)~s"
                "           Relay: ~s (UDP)~s"
                "   Permission(s): ~s~s"
                "         Peer(s): ~s~s"
                "            Sent: ~B KiB (~B packets)~s"
                "        Received: ~B KiB (~B packets)~s"
                "     Running for: ~B seconds",
                [User, nl(),
                 Client, Transport, nl(),
                 Relay, nl(),
                 Perms, nl(),
                 Peers, nl(),
                 round(SentBytes / 1024), SentPkts, nl(),
                 round(RcvdBytes / 1024), RcvdPkts, nl(),
                 Duration])
      end, Sessions).

-spec format_info(eturnal_node_info()) -> io_lib:chars().
format_info(#eturnal_node_info{
               eturnal_vsn = EturnalVsn,
               otp_vsn = {OtpVsn, ErtsVsn},
               uptime = Uptime,
               num_sessions = Sessions,
               num_processes = Procs,
               num_reductions = Reductions,
               total_queue_len = QueueLen,
               total_memory = Memory}) ->
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
      [EturnalVsn, OtpVsn, ErtsVsn, nl(),
       Ds, Hs, Ms, Ss, nl(),
       Sessions, nl(),
       Procs, nl(),
       QueueLen, nl(),
       Reductions, nl(),
       MiB]).

-spec format_transport(sock_mod()) -> binary().
format_transport(gen_udp) ->
    <<"UDP">>;
format_transport(gen_tcp) ->
    <<"TCP">>;
format_transport(fast_tls) ->
    <<"TLS">>.

-spec format_addrs([addr() | addr_port()]) -> iodata().
format_addrs([]) ->
    <<"none">>;
format_addrs(PeerAddrs) ->
    [lists:join(", ", lists:map(fun eturnal_misc:addr_to_str/1, PeerAddrs)),
     <<" (UDP)">>].

-spec format_credentials(binary(), binary()) -> iodata().
format_credentials(Username, Password) ->
    io_lib:format("Username: ~s~s"
                  "Password: ~s",
                  [Username, nl(),
                   Password]).

-spec nl() -> string().
nl() ->
    [$~, $n]. % Let the caller convert "~n"s to actual newline characters.

-spec call(term()) -> ok | {ok | error, term()}.
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
