%%% eturnal STUN/TURN server.
%%%
%%% Copyright (c) 2021 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2021 ProcessOne, SARL.
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

-record(eturnal_node_info,
        {eturnal_vsn :: binary(),
         otp_vsn :: {string(), string()},
         uptime :: non_neg_integer(),
         num_sessions :: non_neg_integer(),
         num_processes :: non_neg_integer(),
         num_reductions :: non_neg_integer(),
         total_queue_len :: non_neg_integer(),
         total_memory :: non_neg_integer()}).

-type eturnal_node_info() :: #eturnal_node_info{}.
