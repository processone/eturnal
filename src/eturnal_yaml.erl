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

-module(eturnal_yaml).
-author('holger@zedat.fu-berlin.de').
-behaviour(conf).
-export([validator/0]).
-import(yval, [and_then/2, any/0, beam/1, binary/0, bool/0, directory/1,
               either/2, enum/1, file/1, int/2, ip/0, ipv4/0, ipv6/0, ip_mask/0,
               list/1, list_or_single/1, map/3, non_empty/1, non_neg_int/0,
               options/1, options/2, port/0, pos_int/1]).

-type transport() :: udp | tcp | tls | mixed.
-type listener() :: {inet:ip_address(), inet:port_number(), transport(),
                     boolean()}.

-define(BLACKLIST, [{{127, 0, 0, 0}, 8},               % IPv4 loopback.
                    {{0, 0, 0, 0, 0, 0, 0, 1}, 128}]). % IPv6 loopback.

%% API.

-spec validator() -> yval:validator().
validator() ->
    options(
      #{secret => list_or_single(non_empty(binary())),
        listen => listen_validator(),
        relay_ipv4_addr => ipv4(),
        relay_ipv6_addr => ipv6(),
        relay_min_port => port(),
        relay_max_port => port(),
        tls_crt_file => file(read),
        tls_key_file => file(read),
        max_allocations => pos_int(unlimited),
        max_permissions => pos_int(unlimited),
        max_bps => and_then(pos_int(unlimited),
                            fun(unlimited) -> none;
                               (I) -> I
                            end),
        blacklist => list_or_single(ip_mask()),
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
        #{listen => [{{0, 0, 0, 0, 0, 0, 0, 0}, 3478, udp, true},
                     {{0, 0, 0, 0, 0, 0, 0, 0}, 3478, tcp, true}],
          relay_ipv4_addr => eturnal_misc:my_ipv4_addr(),
          relay_ipv6_addr => eturnal_misc:my_ipv6_addr(),
          relay_min_port => 49152,
          relay_max_port => 65535,
          tls_crt_file => none,
          tls_key_file => none,
          max_allocations => 10,
          max_permissions => 10,
          max_bps => none,
          blacklist => ?BLACKLIST,
          realm => <<"eturnal.net">>,
          secret => get_default(secret, undefined),
          software_name => <<"eturnal">>,
          run_dir => get_default("RUNTIME_DIRECTORY", <<"run">>),
          log_dir => get_default("LOGS_DIRECTORY", <<"log">>),
          log_level => info,
          log_rotate_size => infinity,
          log_rotate_count => 10,
          modules => #{}}}]).

%% Internal functions.

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
                        ValidatorOpts = [unique | ValidatorOpts0],
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
              transport => enum([tcp, udp, tls, mixed]),
              enable_turn => bool()},
            [unique,
             {required, [ip]}]),
          fun(Opts) ->
                  DefP = fun(udp) -> 3478;
                            (tcp) -> 3478;
                            (tls) -> 5349
                         end,
                  I = proplists:get_value(ip, Opts),
                  T = proplists:get_value(transport, Opts, udp),
                  P = proplists:get_value(port, Opts, DefP(T)),
                  E = proplists:get_value(enable_turn, Opts, true),
                  {I, P, T, E}
          end)),
      fun check_overlapping_listeners/1).

-spec check_overlapping_listeners([listener()]) -> [listener()].
check_overlapping_listeners(Listeners) ->
    ok = check_overlapping_listeners(Listeners, fun(L) -> L end),
    ok = check_overlapping_listeners(Listeners, fun lists:reverse/1),
    Listeners.

-spec check_overlapping_listeners([listener()],
                                  fun(([listener()]) -> [listener()]))
      -> ok | no_return().
check_overlapping_listeners(Listeners, PrepareFun) ->
    _ = lists:foldl(
          fun({IP, Port, Transport, _EnableTURN} = Listener, Acc) ->
                  Key = {IP, Port, Transport},
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
format_listener({IP, Port, Transport, _EnableTURN}) ->
    Addr = eturnal_misc:addr_to_str(IP, Port),
    list_to_binary(io_lib:format("~s (~s)", [Addr, Transport])).

-spec get_env_name(atom()) -> string().
get_env_name(Opt) ->
    "ETURNAL_" ++ string:uppercase(atom_to_list(Opt)).

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

-spec fail({atom(), term()}) -> no_return().
fail(Reason) ->
   yval:fail(yval, Reason).
