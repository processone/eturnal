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

-module(eturnal_app).
-behaviour(application).
-export([start/2,
         prep_stop/1,
         stop/1,
         config_change/3]).

-include_lib("kernel/include/logger.hrl").

%% API.

-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    ok = eturnal:init_config(),
    ok = eturnal_logger:start(),
    ?LOG_NOTICE("Starting eturnal ~s on Erlang/OTP ~s (ERTS ~s)",
                [eturnal_misc:version(),
                 erlang:system_info(otp_release),
                 erlang:system_info(version)]),
    case eturnal_sup:start_link() of
        {ok, _PID} = Result ->
            ok = eturnal_systemd:ready(),
            Result;
        {error, _Reason} = Err ->
            Err
    end.

-spec prep_stop(term()) -> term().
prep_stop(State) ->
    ok = eturnal_systemd:stopping(),
    State.

-spec stop(term()) -> ok.
stop(_State) ->
    ?LOG_NOTICE("Stopping eturnal ~s on Erlang/OTP ~s (ERTS ~s)",
                [eturnal_misc:version(),
                 erlang:system_info(otp_release),
                 erlang:system_info(version)]),
    ok = eturnal_logger:stop().

-spec config_change([{atom(), term()}], [{atom(), term()}], [atom()]) -> ok.
config_change(Changed, New, Removed) ->
    case eturnal:config_is_loaded() of
        true ->
            ?LOG_DEBUG("Got configuration change event"),
            ok = eturnal:reload({Changed, New, Removed},
                                fun eturnal_systemd:reloading/0,
                                fun eturnal_systemd:ready/0);
        false ->
            ?LOG_DEBUG("Got configuration change event after release upgrade")
    end.
