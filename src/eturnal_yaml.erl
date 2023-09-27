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

-module(eturnal_yaml).
-behaviour(conf).
-export([validator/0]).
-import(yval, [and_then/2, any/0, beam/1, binary/0, bool/0, directory/1,
               either/2, enum/1, file/1, int/2, ip/0, ipv4/0, ipv6/0, ip_mask/0,
               list/1, list/2, list_or_single/1, map/3, non_empty/1,
               non_neg_int/0, options/1, options/2, port/0, pos_int/1]).

-include_lib("kernel/include/logger.hrl").

-define(RECOMMENDED_BLACKLIST,
        [{{127, 0, 0, 0}, 8},
         {{10, 0, 0, 0}, 8},
         {{100, 64, 0, 0}, 10},
         {{169, 254, 0, 0}, 16},
         {{172, 16, 0, 0}, 12},
         {{192, 0, 0, 0}, 24},
         {{192, 0, 2, 0}, 24},
         {{192, 88, 99, 0}, 24},
         {{192, 168, 0, 0}, 16},
         {{198, 18, 0, 0}, 15},
         {{198, 51, 100, 0}, 24},
         {{203, 0, 113, 0}, 24},
         {{224, 0, 0, 0}, 4},
         {{240, 0, 0, 0}, 4},
         {{0, 0, 0, 0, 0, 0, 0, 1}, 128},
         {{100, 65435, 0, 0, 0, 0, 0, 0}, 96},
         {{256, 0, 0, 0, 0, 0, 0, 0}, 64},
         {{64512, 0, 0, 0, 0, 0, 0, 0}, 7},
         {{65152, 0, 0, 0, 0, 0, 0, 0}, 10},
         {{65280, 0, 0, 0, 0, 0, 0, 0}, 8}]).

-type listener() :: {inet:ip_address(), inet:port_number(), eturnal:transport(),
                     boolean(), boolean()}.
-type family() :: ipv4 | ipv6.
-type boundary() :: min | max.

%% API.

-spec validator() -> yval:validator().
validator() ->
    options(
      #{secret => list_or_single(non_empty(binary())),
        listen => listen_validator(),
        relay_ipv4_addr => and_then(ipv4(), fun check_relay_addr/1),
        relay_ipv6_addr => and_then(ipv6(), fun check_relay_addr/1),
        relay_min_port => port(),
        relay_max_port => port(),
        tls_crt_file => file(read),
        tls_key_file => file(read),
        tls_dh_file => file(read),
        tls_options => openssl_list($|),
        tls_ciphers => openssl_list($:),
        max_allocations => pos_int(unlimited),
        max_permissions => pos_int(unlimited),
        max_bps => and_then(pos_int(unlimited),
                            fun(unlimited) -> none;
                               (I) -> I
                            end),
        blacklist => blacklist_validator(),
        whitelist => list_or_single(ip_mask()),
        blacklist_clients => list_or_single(ip_mask()),
        whitelist_clients => list_or_single(ip_mask()),
        blacklist_peers => blacklist_validator(),
        whitelist_peers => list_or_single(ip_mask()),
        strict_expiry => bool(),
        credentials => map(binary(), binary(), [unique, {return, map}]),
        realm => non_empty(binary()),
        software_name => non_empty(binary()),
        run_dir => directory(write),
        log_dir => either(stdout, directory(write)),
        log_level => enum([critical, error, warning, notice, info, debug]),
        log_rotate_size => pos_int(infinity),
        log_rotate_count => non_neg_int(),
        modules => module_validator()},
      [unique,
       {required, []},
       {defaults,
        #{listen => [{{0, 0, 0, 0, 0, 0, 0, 0}, 3478, udp, false, true},
                     {{0, 0, 0, 0, 0, 0, 0, 0}, 3478, tcp, false, true}],
          relay_ipv4_addr => get_default_addr(ipv4),
          relay_ipv6_addr => get_default_addr(ipv6),
          relay_min_port => get_default_port(min, 49152),
          relay_max_port => get_default_port(max, 65535),
          tls_crt_file => none,
          tls_key_file => none,
          tls_dh_file => none,
          tls_options => <<"cipher_server_preference">>,
          tls_ciphers => <<"HIGH:!aNULL:@STRENGTH">>,
          max_allocations => 10,
          max_permissions => 10,
          max_bps => none,
          blacklist => [],
          whitelist => [],
          blacklist_clients => [],
          whitelist_clients => [],
          blacklist_peers => ?RECOMMENDED_BLACKLIST,
          whitelist_peers => [],
          strict_expiry => false,
          credentials => #{},
          realm => <<"eturnal.net">>,
          secret => [get_default(secret, make_random_secret())],
          software_name => <<"eturnal">>,
          run_dir => get_default("RUNTIME_DIRECTORY", <<"run">>),
          log_dir => get_default("LOGS_DIRECTORY", <<"log">>),
          log_level => info,
          log_rotate_size => infinity,
          log_rotate_count => 10,
          modules => #{}}}]).

%% Internal functions.

-spec blacklist_validator() -> yval:validator().
blacklist_validator() ->
    and_then(
      list_or_single(either(recommended, ip_mask())),
      fun(L) ->
              lists:usort(
                lists:flatmap(
                  fun(recommended) ->
                          ?RECOMMENDED_BLACKLIST;
                     (Network) ->
                          [Network]
                  end, L))
      end).

-spec module_validator() -> yval:validator().
module_validator() ->
    and_then(
      map(
        beam([{handle_event, 2}, {options, 0}]),
        options(#{'_' => any()}),
        [unique]),
      fun(L) ->
              lists:foldl(
                fun({Mod, Opts}, Acc) ->
                        {Validators,
                         ValidatorOpts0} = eturnal_module:options(Mod),
                        ValidatorOpts = [unique,
                                         {return, map} | ValidatorOpts0],
                        Acc#{Mod => (options(Validators, ValidatorOpts))(Opts)}
                end, #{}, L)
      end).

-spec listen_validator() -> yval:validator().
listen_validator() ->
    and_then(
      list(
        and_then(
          options(
            #{ip => ip(),
              port => int(0, 65535),
              transport => enum([tcp, udp, tls, auto]),
              proxy_protocol => bool(),
              enable_turn => bool()}),
          fun(Opts) ->
                  DefP = fun(udp) -> 3478;
                            (tcp) -> 3478;
                            (tls) -> 5349;
                            (auto) -> 3478
                         end,
                  I = proplists:get_value(ip, Opts, {0, 0, 0, 0, 0, 0, 0, 0}),
                  T = proplists:get_value(transport, Opts, udp),
                  P = proplists:get_value(port, Opts, DefP(T)),
                  X = proplists:get_value(proxy_protocol, Opts, false),
                  E = proplists:get_value(enable_turn, Opts, true),
                  {I, P, T, X, E}
          end)),
      fun check_overlapping_listeners/1).

-spec check_overlapping_listeners([listener()]) -> [listener()].
check_overlapping_listeners(Listeners) ->
    ok = check_overlapping_listeners(Listeners, fun(L) -> L end),
    ok = check_overlapping_listeners(Listeners, fun lists:reverse/1),
    Listeners.

-spec check_overlapping_listeners([listener()],
                                  fun(([listener()]) -> [listener()]))
      -> ok.
check_overlapping_listeners(Listeners, PrepareFun) ->
    _ = lists:foldl(
          fun({IP, Port, Transport, _ProxyProtocol, _EnableTURN} = Listener,
              Acc) ->
                  Key = case Transport of
                            udp ->
                                {IP, Port, udp};
                            _ ->
                                {IP, Port, tcp}
                        end,
                  case lists:member(Key, Acc) of
                      true ->
                          fail({duplicated_value,
                                format_listener(Listener)});
                      false ->
                          % With dual-stack sockets, we won't detect conflicts
                          % of IPv4 addresses with "::".
                          AnyIP = case tuple_size(IP) of
                                       8 -> {0, 0, 0, 0, 0, 0, 0, 0};
                                       4 -> {0, 0, 0, 0}
                                   end,
                          Key1 = {AnyIP, Port, Transport},
                          case lists:member(Key1, Acc) of
                              true ->
                                  fail({duplicated_value,
                                        format_listener(Listener)});
                              false ->
                                  [Key | Acc]
                          end
                  end
          end, [], PrepareFun(Listeners)),
    ok.

-spec format_listener(listener()) -> binary().
format_listener({IP, Port, Transport, _ProxyProtocol, _EnableTURN}) ->
    Addr = eturnal_misc:addr_to_str(IP, Port),
    list_to_binary(io_lib:format("~s (~s)", [Addr, Transport])).

-spec check_relay_addr(inet:ip_address()) -> inet:ip_address().
check_relay_addr({0, 0, 0, 0} = Addr) ->
    fail({bad_ip, inet:ntoa(Addr)});
check_relay_addr({_, _, _, _} = Addr) ->
    Addr;
check_relay_addr({0, 0, 0, 0, 0, 0, 0, 0} = Addr) ->
    fail({bad_ip, inet:ntoa(Addr)});
check_relay_addr({_, _, _, _, _, _, _, _} = Addr) ->
    Addr.

-spec get_default_addr(family()) -> inet:ip_address() | undefined.
get_default_addr(Family) ->
    {Vsn, Opt, ParseAddr, MyAddr} =
        case Family of
            ipv4 ->
                {4, relay_ipv4_addr,
                 fun inet:parse_ipv4strict_address/1,
                 fun eturnal_misc:my_ipv4_addr/0};
            ipv6 ->
                {6, relay_ipv6_addr,
                 fun inet:parse_ipv6strict_address/1,
                 fun eturnal_misc:my_ipv6_addr/0}
        end,
    case get_default(Opt, undefined) of
        RelayAddr when is_binary(RelayAddr) ->
            try
                {ok, Addr} = ParseAddr(binary_to_list(RelayAddr)),
                check_relay_addr(Addr)
            catch error:_ ->
                    abort("Bad ETURNAL_RELAY_IPV~B_ADDR: ~s", [Vsn, RelayAddr])
            end;
        undefined ->
            MyAddr()
    end.

-spec get_default_port(boundary(), inet:port_number()) -> inet:port_number().
get_default_port(MinMax, Default) ->
    MinMaxStr = from_atom(MinMax),
    Opt = to_atom(<<"relay_", MinMaxStr/binary, "_port">>),
    case get_default(Opt, Default) of
        Bin when is_binary(Bin) ->
            try
                Port = binary_to_integer(Bin),
                true = (Port >= 1),
                true = (Port =< 65535),
                Port
            catch error:_ ->
                    abort("Bad ETURNAL_RELAY_~s_PORT: ~s",
                          [string:uppercase(MinMaxStr), Bin])
            end;
        Default ->
            Default
    end.

-spec get_default(atom() | string(), Term) -> binary() | Term.
get_default(Opt, Default) when is_atom(Opt) ->
    get_default(get_env_name(Opt), Default);
get_default(Var, Default) ->
    case os:getenv(Var) of
        Val when is_list(Val), length(Val) > 0 ->
            unicode:characters_to_binary(Val);
        _ ->
            Default
    end.

-spec get_env_name(atom()) -> string().
get_env_name(Opt) ->
    "ETURNAL_" ++ string:uppercase(atom_to_list(Opt)).

-spec make_random_secret() -> binary().
-ifdef(old_rand).
make_random_secret() ->
    <<(rand:uniform(1 bsl 127)):128>>.
-else.
make_random_secret() ->
    rand:bytes(16).
-endif.

-spec openssl_list(char()) -> fun((binary() | [binary()]) -> binary()).
openssl_list(Sep) ->
    fun(L) when is_list(L) ->
            (and_then(list(binary(), [unique]), join(Sep)))(L);
       (B) ->
            (binary())(B)
    end.

-spec join(char()) -> fun(([binary()]) -> binary()).
join(Sep) ->
    fun(Opts) -> unicode:characters_to_binary(lists:join(<<Sep>>, Opts)) end.

-spec from_atom(atom()) -> binary().
-spec to_atom(binary()) -> atom().
-ifdef(old_atom_conversion). % Erlang/OTP < 23.0.
from_atom(A) -> atom_to_binary(A, utf8).
to_atom(S) -> list_to_existing_atom(binary_to_list(S)).
-else.
from_atom(A) -> atom_to_binary(A).
to_atom(S) -> binary_to_existing_atom(S).
-endif.

-spec fail({atom(), term()}) -> no_return().
fail(Reason) ->
    yval:fail(yval, Reason).

-spec abort(io:format(), [term()]) -> no_return().
abort(Format, Data) ->
    ?LOG_CRITICAL(Format, Data),
    eturnal_logger:flush(),
    halt(2).
