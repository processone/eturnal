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
-export([to_string/1, my_ipv4_addr/0, my_ipv6_addr/0]).

-include_lib("kernel/include/logger.hrl").

%% API.

-spec to_string(binary() | iolist()) -> string().
to_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_string(IoList) when is_list(IoList) ->
    lists:flatten(IoList).

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
