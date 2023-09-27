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

-module(eturnal_ctl).
-export([get_credentials/2,
         get_password/1,
         get_sessions/0,
         get_sessions/1,
         get_info/0,
         get_version/0,
         get_loglevel/0,
         set_loglevel/1,
         disconnect/1,
         reload/0]).

-include_lib("kernel/include/logger.hrl").
-include("eturnal.hrl").

-type sock_mod() :: gen_udp | gen_tcp | fast_tls.
-type addr() :: inet:ip_address().
-type addr_port() :: {inet:ip_address(), inet:port_number()}.

-record(session,
        {pid :: pid(),
         sid :: binary(),
         user :: binary(),
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

-spec get_credentials(term(), term()) -> {ok, string()} | {error, string()}.
get_credentials(Expiry, Suffix) ->
    ?LOG_DEBUG("Handling API call: get_credentials(~p, ~p)", [Expiry, Suffix]),
    try make_username(Expiry, Suffix) of
        Username ->
            case call({get_password, Username}) of
                {ok, Password} ->
                    Credentials = format_credentials(Username, Password),
                    {ok, unicode:characters_to_list(Credentials)};
                {error, no_credentials} ->
                    {error, "No shared secret and no credentials"};
                {error, timeout} ->
                    {error, "Querying eturnal timed out"}
            end
    catch _:badarg ->
            ?LOG_DEBUG("Invalid argument(s): ~p:~p", [Expiry, Suffix]),
            {error, "Invalid expiry or suffix"}
    end.

-spec get_password(term()) -> {ok, string()} | {error, string()}.
get_password(Username0) ->
    ?LOG_DEBUG("Handling API call: get_password(~p)", [Username0]),
    try unicode:characters_to_binary(Username0) of
        Username when is_binary(Username) ->
            case call({get_password, Username}) of
                {ok, Password} ->
                    {ok, unicode:characters_to_list(Password)};
                {error, no_credentials} ->
                    {error, "No shared secret and no credentials"};
                {error, timeout} ->
                    {error, "Querying eturnal timed out"}
            end;
        {_, _, _} ->
            ?LOG_DEBUG("Cannot convert user name to binary: ~p", [Username0]),
            {error, "User name must be specified as a string"}
    catch _:badarg ->
            ?LOG_DEBUG("Cannot convert user name to binary: ~p", [Username0]),
            {error, "User name must be specified as a string"}
    end.

-spec get_sessions() -> {ok, string()} | {error, string()}.
get_sessions() ->
    ?LOG_DEBUG("Handling API call: get_sessions()"),
    case query_all_sessions() of
        [_ | _] = Sessions ->
            {ok, unicode:characters_to_list(format_sessions(Sessions))};
        [] ->
            {ok, "No active TURN sessions"}
    end.

-spec get_sessions(term()) -> {ok, string()} | {error, string()}.
get_sessions(Username0) ->
    ?LOG_DEBUG("Handling API call: get_sessions(~p)", [Username0]),
    try unicode:characters_to_binary(Username0) of
        Username when is_binary(Username) ->
            case query_user_sessions(Username) of
                [_ | _] = Sessions ->
                    {ok, unicode:characters_to_list(format_sessions(Sessions))};
                [] ->
                    {ok, "No active TURN sessions"}
            end;
        {_, _, _} ->
            ?LOG_DEBUG("Cannot convert user name to binary: ~p", [Username0]),
            {error, "User name must be specified as a string"}
    catch _:badarg ->
            ?LOG_DEBUG("Cannot convert user name to binary: ~p", [Username0]),
            {error, "User name must be specified as a string"}
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

-spec disconnect(term()) -> {ok, string()} | {error, string()}.
disconnect(Username0) ->
    ?LOG_DEBUG("Handling API call: disconnect(~p)", [Username0]),
    try unicode:characters_to_binary(Username0) of
        Username when is_binary(Username) ->
            N = disconnect_user(Username),
            Msg = io_lib:format("Disconnected ~B TURN session(s)", [N]),
            {ok, unicode:characters_to_list(Msg)};
        {_, _, _} ->
            ?LOG_DEBUG("Cannot convert user name to binary: ~p", [Username0]),
            {error, "User name must be specified as a string"}
    catch _:badarg ->
            ?LOG_DEBUG("Cannot convert user name to binary: ~p", [Username0]),
            {error, "User name must be specified as a string"}
    end.

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

-spec make_username(string(), string()) -> binary().
make_username(Expiry0, Suffix) ->
    Expiry = try string:trim(Expiry0) of
                 Trimmed ->
                     Trimmed
             catch _:function_clause ->
                     erlang:error(badarg)
             end,
    try calendar:rfc3339_to_system_time(Expiry) of
        Time ->
            username_from_timestamp(Time, Suffix)
    catch
        _:{badmatch, _} ->
            username_from_expiry(Expiry, Suffix);
        _:badarg -> % Erlang/OTP < 21.3.
            username_from_expiry(Expiry, Suffix)
    end.

-spec username_from_timestamp(integer(), string()) -> binary().
username_from_timestamp(Time, [])  ->
    integer_to_binary(Time);
username_from_timestamp(Time, Suffix) ->
    Username = io_lib:format("~B:~s", [Time, Suffix]),
    unicode:characters_to_binary(Username).

-spec username_from_expiry(string(), string()) -> binary().
username_from_expiry(Expiry0, Suffix) ->
    case {unicode:characters_to_binary(Expiry0),
          io_lib:printable_unicode_list(Suffix)} of
        {Expiry, true} when is_binary(Expiry) ->
            Time = erlang:system_time(second) + parse_expiry(Expiry),
            username_from_timestamp(Time, Suffix);
        {_, _} ->
            erlang:error(badarg)
    end.

-spec parse_expiry(binary()) -> pos_integer().
parse_expiry(Expiry) ->
    case string:to_integer(Expiry) of
        {N, <<>>} when is_integer(N), N > 0 ->
            N;
        {N, <<"s">>} when is_integer(N), N > 0 ->
            N;
        {N, <<"m">>} when is_integer(N), N > 0 ->
            N * 60;
        {N, <<"h">>} when is_integer(N), N > 0 ->
            N * 3600;
        {N, <<"d">>} when is_integer(N), N > 0 ->
            N * 86400;
        {_, _} ->
            erlang:error(badarg)
    end.

-spec filter_sessions(fun((session()) -> boolean())) -> [session()].
filter_sessions(Pred) ->
    lists:filtermap(
      fun({_, PID, worker, _}) ->
              try query_state(PID) of
                  State ->
                      Session = #session{
                                   pid = PID,
                                   sid = element(27, State),
                                   user = element(6, State),
                                   sock_mod = element(2, State),
                                   client_addr = element(4, State),
                                   relay_addr = element(18, State),
                                   perm_addrs = maps:keys(element(12, State)),
                                   peer_addrs = maps:keys(element(10, State)),
                                   sent_bytes = element(32, State),
                                   sent_pkts = element(33, State),
                                   rcvd_bytes = element(30, State),
                                   rcvd_pkts = element(31, State),
                                   start_time = element(34, State)},
                      case Pred(Session) of
                          true ->
                              {true, Session};
                          false ->
                              false
                      end
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

-spec query_user_sessions(binary()) -> [session()].
query_user_sessions(Username) ->
    Pred = fun(#session{user = User}) when User =:= Username ->
                   true;
              (#session{user = User}) -> % Match 1256900400:Username.
                   case binary:split(User, <<$:>>) of
                       [_Expiration, Username] ->
                           true;
                       _ ->
                           false
                   end
           end,
    filter_sessions(Pred).

-spec query_all_sessions() -> [session()].
query_all_sessions() ->
    filter_sessions(fun(_Session) -> true end).

-spec query_state(pid()) -> tuple().
query_state(PID) -> % Until we add a proper API to 'stun'.
    {value, State} = lists:search(
                       fun(E) ->
                               is_tuple(E) andalso element(1, E) =:= state
                       end, sys:get_state(PID)),
    State.

-spec disconnect_user(binary()) -> non_neg_integer().
disconnect_user(User) ->
    lists:foldl(fun(#session{user = U, pid = PID, sid = SID}, N) ->
                        ?LOG_DEBUG("Disconnecting session ~s of ~s", [SID, U]),
                        _ = supervisor:terminate_child(turn_tmp_sup, PID),
                        N + 1
                end, 0, query_user_sessions(User)).

-spec format_sessions([session()]) -> io_lib:chars().
format_sessions(Sessions) ->
    H = io_lib:format("~B active TURN sessions:", [length(Sessions)]),
    T = lists:map(
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
                  Duration = erlang:convert_time_unit(
                               Duration0, native, second),
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
          end, Sessions),
    lists:join([nl(), nl()], [H | T]).

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
    Seconds = erlang:convert_time_unit(Uptime, millisecond, second),
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
    try gen_server:call(eturnal, Request, timer:minutes(1)) of
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
