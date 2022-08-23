%%% Simple STUN client (UDP-only).
%%%
%%% Copyright (c) 2022 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2022 ProcessOne, SARL.
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

-module(stun).
-export([main/1]).

-include_lib("stun/include/stun.hrl").
-define(STUN_TIMEOUT, timer:seconds(5)).
-define(STUN_PORT, "3478").
-define(STUN_FAMILY, inet).

-spec main([string()]) -> any().
main(["-4", Server, Port]) ->
    query(Server, Port, inet);
main(["-6", Server, Port]) ->
    query(Server, Port, inet6);
main(["-4", Server]) ->
    query(Server, ?STUN_PORT, inet);
main(["-6", Server]) ->
    query(Server, ?STUN_PORT, inet6);
main([Server, Port]) ->
    query(Server, Port, ?STUN_FAMILY);
main([Server]) ->
    query(Server, ?STUN_PORT, ?STUN_FAMILY);
main(_Args) ->
    abort("Usage: stun [-4|-6] <server> [<port>]").

-spec query(inet:hostname(), string(), inet:family()) -> any().
query(Server0, Port0, Family) ->
    try
        {ok, Server} = inet:getaddr(Server0, Family),
        Port = list_to_integer(Port0),
        TrID = rand:uniform(1 bsl 96),
        Msg = #stun{method = ?STUN_METHOD_BINDING,
                    class = request,
                    trid = TrID},
        {ok, Sock} = gen_udp:open(0, [Family, binary, {active, false}]),
        PktOut = stun_codec:encode(Msg),
        ok = gen_udp:send(Sock, Server, Port, PktOut),
        {ok, {_, _, PktIn}} = gen_udp:recv(Sock, 0, ?STUN_TIMEOUT),
        {ok, #stun{trid = TrID,
                   'XOR-MAPPED-ADDRESS' = {Addr, _}}} =
            stun_codec:decode(PktIn, datagram),
        ok = gen_udp:close(Sock),
        ok = io:put_chars(inet:ntoa(Addr)),
        ok = io:nl()
    catch _:Err ->
            abort("Cannot query ~s:~s: ~s", [Server0, Port0, format_error(Err)])
    end.

-spec format_error(any()) -> string().
format_error({_, {error, timeout}}) ->
    "request timed out";
format_error({_, {error, Reason}}) ->
    inet:format_error(Reason);
format_error(Err) ->
    unicode:characters_to_list(io_lib:format("~p", [Err])).

-spec abort(iolist() | binary() | atom()) -> no_return().
abort(Msg) ->
    abort("~s", [Msg]).

-spec abort(io:format(), [term()]) -> no_return().
abort(Format, Data) ->
    ok = io:format(standard_error, Format, Data),
    ok = io:nl(standard_error),
    halt(1).
