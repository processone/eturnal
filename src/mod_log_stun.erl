%%% eturnal STUN/TURN server module.
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

%%% This module logs STUN queries (or rather, responses).

-module(mod_log_stun).
-behaviour(eturnal_module).
-export([start/0,
         stop/0,
         handle_event/2,
         options/0]).
-import(yval, [enum/1]).

-include_lib("kernel/include/logger.hrl").
-ifndef(LOG). % Erlang/OTP 21.0.
-define(LOG(Level, Format, Args), logger:log(Level, Format, Args)).
-endif.

%% API.

-spec start() -> {ok, eturnal_module:events()}.
start() ->
    ?LOG_DEBUG("Starting ~s", [?MODULE]),
    {ok, [stun_query]}.

-spec handle_event(eturnal_module:event(), eturnal_module:info()) -> ok.
handle_event(stun_query, #{version := Version}) ->
    Level = eturnal_module:get_opt(?MODULE, level),
    ?LOG(Level, "Responding to ~s request", [format_protocol(Version)]),
    ok.

-spec stop() -> ok.
stop() ->
    ?LOG_DEBUG("Stopping ~s", [?MODULE]),
    ok.

-spec options() -> eturnal_module:options().
options() ->
    {#{level => enum([critical, error, warning, notice, info, debug])},
     [{defaults,
       #{level => info}}]}.

%% Internal functions.

-spec format_protocol(old | new) -> binary().
format_protocol(old) -> <<"'classic' STUN">>;
format_protocol(new) -> <<"STUN">>.
