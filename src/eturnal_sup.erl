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

-module(eturnal_sup).
-behaviour(supervisor).
-export([start_link/0,
         init/1]).

-include_lib("kernel/include/logger.hrl").
-define(SERVER, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    ?LOG_DEBUG("Starting supervisor: ~s", [?SERVER]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{},
    ChildSpecs = [#{id => systemd, start => {eturnal_systemd, start_link, []}},
                  #{id => eturnal, start => {eturnal, start_link, []}}],
    ?LOG_DEBUG("Configuring ~s supervisor: ~p", [?MODULE, ChildSpecs]),
    {ok, {SupFlags, ChildSpecs}}.
