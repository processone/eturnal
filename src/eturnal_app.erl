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

-module(eturnal_app).
-behaviour(application).
-export([start/2, stop/1, config_change/3]).

-include_lib("kernel/include/logger.hrl").

%% API

-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    eturnal_logger:start(),
    ?LOG_NOTICE("Starting eturnal STUN/TURN server"),
    eturnal_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ?LOG_NOTICE("Stopping eturnal STUN/TURN server"),
    ok.

-spec config_change([{atom(), term()}], [{atom(), term()}], [atom()]) -> ok.
config_change(Changed, New, Removed) ->
    gen_server:cast(eturnal, {config_change, {Changed, New, Removed}}).
