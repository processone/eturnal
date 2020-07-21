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

-module(eturnal_misc).
-export([my_ipv4_addr/0, my_ipv6_addr/0, addr_to_str/1, addr_to_str/2,
         version/0]).

-include_lib("kernel/include/logger.hrl").

%% API.

-spec my_ipv4_addr() -> inet:ip4_address() | undefined.
my_ipv4_addr() ->
    {ok, MyHostName} = inet:gethostname(),
    case inet:getaddr(MyHostName, inet) of
        {ok, Addr} -> Addr;
        {error, _} -> undefined
    end.

-spec my_ipv6_addr() -> inet:ip6_address() | undefined.
my_ipv6_addr() ->
    {ok, MyHostName} = inet:gethostname(),
    case inet:getaddr(MyHostName, inet6) of
        {ok, Addr} -> Addr;
        {error, _} -> undefined
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
