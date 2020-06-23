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

-module(eturnal_ctl).
-export([reload/0, get_loglevel/0, set_loglevel/1]).

-include_lib("kernel/include/logger.hrl").

%% API.

-spec reload() -> ok | {error, string()}.
reload() ->
    ?LOG_DEBUG("Handling API call: reload()"),
    case call(reload) of
        ok ->
            ok;
        {error, timeout} ->
            {error, "Querying eturnal timed out"};
        {error, Reason} ->
            {error, conf:format_error(Reason)}
    end.

-spec get_loglevel() -> {ok, string()} | {error, string()}.
get_loglevel() ->
    ?LOG_DEBUG("Handling API call: get_loglevel()"),
    case call(get_loglevel) of
        {ok, Level} ->
            {ok, atom_to_list(Level)};
        {error, timeout} ->
            {error, "Querying eturnal timed out"}
    end.

-spec set_loglevel(term()) -> ok | {error, string()}.
set_loglevel(Level) when is_atom(Level) ->
    ?LOG_DEBUG("Handling API call: set_loglevel(~s)", [Level]),
    case eturnal_logger:is_valid_level(Level) of
        true ->
            case call({set_loglevel, Level}) of
                ok ->
                    ok;
                {error, timeout} ->
                    {error, "Querying eturnal timed out"}
            end;
        false ->
            ?LOG_ERROR("set_loglevel(~s) argument is invalid", [Level]),
            {error, "Not a valid log level: " ++ atom_to_list(Level)}
    end;
set_loglevel(Level) ->
    ?LOG_ERROR("set_loglevel(~p) argument is not an atom", [Level]),
    {error, "Log level must be specified as an 'atom'"}.

%% Internal functions.

-spec call(term()) -> ok | {error, term()}.
call(Request) ->
    try gen_server:call(eturnal, Request) of
        ok ->
            ?LOG_DEBUG("eturnal call (~p) returned ok", [Request]),
            ok;
        {ok, _Value} = Result ->
            ?LOG_DEBUG("eturnal call (~p) returned ~p", [Request, Result]),
            Result;
        {error, _Reason} = Err ->
            ?LOG_DEBUG("eturnal call (~p) returned ~p", [Request, Err]),
            Err
    catch exit:{timeout, _} ->
            ?LOG_WARNING("eturnal call (~p) timed out", [Request]),
            {error, timeout}
    end.
