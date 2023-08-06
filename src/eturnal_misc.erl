%%% eturnal STUN/TURN server.
%%%
%%% Copyright (c) 2020-2022 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2020-2022 ProcessOne, SARL.
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

-module(eturnal_misc).
-export([my_ipv4_addr/0,
         my_ipv6_addr/0,
         addr_to_str/1,
         addr_to_str/2,
         version/0,
         info/0]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("eturnal.hrl").

%% API.

-spec my_ipv4_addr() -> inet:ip4_address() | undefined.
my_ipv4_addr() ->
    case my_host_addr(inet) of
        {_, _, _, _} = Addr ->
            Addr;
        undefined ->
            my_if_addr(inet)
    end.

-spec my_ipv6_addr() -> inet:ip6_address() | undefined.
my_ipv6_addr() ->
    case my_host_addr(inet6) of
        {_, _, _, _, _, _, _, _} = Addr ->
            Addr;
        undefined ->
            my_if_addr(inet6)
    end.

-spec addr_to_str(inet:ip_address(), inet:port_number()) -> iolist().
addr_to_str(Addr, Port) ->
    addr_to_str({Addr, Port}).

-spec addr_to_str({inet:ip_address(), inet:port_number()} | inet:ip_address())
      -> iolist().
addr_to_str({Addr, Port}) when is_tuple(Addr) ->
    [addr_to_str(Addr), [$: | integer_to_list(Port)]];
addr_to_str({0, 0, 0, 0, 0, 16#FFFF, D7, D8}) ->
    addr_to_str({D7 bsr 8, D7 band 255, D8 bsr 8, D8 band 255});
addr_to_str({_, _, _, _, _, _, _, _} = Addr) ->
    [$[, inet:ntoa(Addr), $]];
addr_to_str(Addr) ->
    inet:ntoa(Addr).

-spec version() -> binary().
version() ->
    {ok, Version} = application:get_key(vsn),
    unicode:characters_to_binary(Version).

-spec info() -> eturnal_node_info().
info() ->
    #eturnal_node_info{
       eturnal_vsn = version(),
       otp_vsn = {erlang:system_info(otp_release), erlang:system_info(version)},
       uptime = element(1, erlang:statistics(wall_clock)),
       num_sessions = length(supervisor:which_children(turn_tmp_sup)),
       num_processes = erlang:system_info(process_count),
       num_reductions = element(1, erlang:statistics(reductions)),
       total_queue_len = erlang:statistics(total_run_queue_lengths),
       total_memory = erlang:memory(total)}.

%% Internal functions.

-spec my_host_addr(inet) -> inet:ip4_address() | undefined;
                  (inet6) -> inet:ip6_address() | undefined.
my_host_addr(Family) ->
    {ok, Name} = inet:gethostname(),
    case inet:getaddr(Name, Family) of
        {ok, Addr} ->
            case is_public_addr(Addr) of
                true ->
                    Addr;
                false ->
                    undefined
            end;
        {error, _} ->
            undefined
    end.

-spec my_if_addr(inet) -> inet:ip4_address() | undefined;
                (inet6) -> inet:ip6_address() | undefined.
my_if_addr(Family) ->
    %
    % net:getifaddrs/1 is more convenient, but depends on Erlang/OTP 22.3+ with
    % 'socket' support (and therefore wouldn't work on Windows, for example):
    %
    % net:getifaddrs(#{family => Family, flags => [up]}).
    %
    {ok, Interfaces} = inet:getifaddrs(),
    Addrs = lists:filtermap(fun({_Name, Opts}) ->
                                    filter_interface(Family, Opts)
                            end, Interfaces),
    case Addrs of
        [Addr | _] ->
            Addr;
        [] ->
            undefined
    end.

-spec filter_interface(inet, [{atom(), tuple()}])
      -> {true, inet:ip4_address()} | false;
                      (inet6, [{atom(), tuple()}])
      -> {true, inet:ip6_address()} | false.
filter_interface(Family, Opts) ->
    Flags = lists:flatten([Fs || {flags, Fs} <- Opts]),
    case lists:member(up, Flags) of
        true ->
            Addrs = [Addr || {addr, Addr} <- Opts,
                             family_matches(Addr, Family),
                             is_public_addr(Addr)],
            case Addrs of
                [Addr | _] ->
                    {true, Addr};
                [] ->
                    false
            end;
        false ->
            false
    end.

-spec is_public_addr(inet:ip_address()) -> boolean().
is_public_addr({0, 0, 0, 0}) ->
    false;
is_public_addr({127, _, _, _}) ->
    false;
is_public_addr({169, 254, _, _}) ->
    false;
is_public_addr({10, _, _, _}) ->
    false;
is_public_addr({172, D2, _, _}) when D2 >= 16, D2 =< 31 ->
    false;
is_public_addr({192, 168, _, _}) ->
    false;
is_public_addr({0, 0, 0, 0, 0, 0, 0, 0}) ->
    false;
is_public_addr({0, 0, 0, 0, 0, 0, 0, 1}) ->
    false;
is_public_addr({65152, _, _, _, _, _, _, _}) ->
    false;
is_public_addr({D1, _, _, _, _, _, _, _}) when D1 >= 64512, D1 =< 65023 ->
    false;
is_public_addr({_, _, _, _}) ->
    true;
is_public_addr({_, _, _, _, _, _, _, _}) ->
    true.

-spec family_matches(inet:ip_address(), inet | inet6) -> boolean().
family_matches({_, _, _, _}, inet) ->
    true;
family_matches({_, _, _, _}, inet6) ->
    false;
family_matches({_, _, _, _, _, _, _, _}, inet6) ->
    true;
family_matches({_, _, _, _, _, _, _, _}, inet) ->
    false.

%% EUnit tests.

-ifdef(EUNIT).
-define(PRIVATE_IPV4ADDR, {172, 20, 0, 2}).
-define(PUBLIC_IPV4ADDR, {203, 0, 113, 2}).
-define(PRIVATE_IPV6ADDR, {65000, 0, 0, 0, 0, 0, 0, 2}).
-define(PUBLIC_IPV6ADDR, {8193, 3512, 0, 0, 0, 0, 0, 2}).

addr_test_() ->
    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
     ?_assert(family_matches(?PUBLIC_IPV4ADDR, inet)),
     ?_assert(family_matches(?PUBLIC_IPV6ADDR, inet6)),
     ?_assertNot(is_public_addr(?PRIVATE_IPV4ADDR)),
     ?_assertNot(is_public_addr(?PRIVATE_IPV6ADDR)),
     ?_assertNot(family_matches(?PRIVATE_IPV4ADDR, inet6)),
     ?_assertNot(family_matches(?PRIVATE_IPV6ADDR, inet)),
     ?_test(my_if_addr(inet)),
     ?_test(my_if_addr(inet6))].
-endif.
