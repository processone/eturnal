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

-module(eturnal_module).
-author('holger@zedat.fu-berlin.de').
-export([start/1,
         stop/2,
         handle_event/4,
         options/1,
         get_opt/2]).
-export_type([state/0,
              event/0,
              info/0,
              options/0]).

-type state() :: term().
-type event() :: atom().
-type info() :: #{atom() => term()}.
-type option() :: atom().
-type options() :: {yval:validators(), proplists:proplist()}.
-type module_ret() :: ok | {ok, state()}.

-callback start() -> module_ret().
-callback stop(state()) -> ok.
-callback handle_event(event(), info(), state()) -> module_ret().
-callback options() -> options().

-optional_callbacks([start/0, stop/1]).

-include_lib("kernel/include/logger.hrl").

%% API.

-spec start(module()) -> {ok, state()} | {error, term()}.
start(Mod) ->
    case erlang:function_exported(Mod, start, 0) of
        true ->
            ?LOG_DEBUG("Calling ~s:start/0", [Mod]),
            try
                case Mod:start() of
                    ok ->
                        {ok, undefined};
                    {ok, State} ->
                        {ok, State}
                end
            catch _:Err ->
                    ?LOG_DEBUG("Module ~s failed at starting: ~p", [Mod, Err]),
                    {error, Err}
            end;
        false ->
            ?LOG_DEBUG("Module ~s doesn't export start/0", [Mod]),
            {ok, undefined}
    end.

-spec stop(module(), state()) -> ok | {error, term()}.
stop(Mod, State) ->
    case erlang:function_exported(Mod, stop, 1) of
        true ->
            ?LOG_DEBUG("Calling ~s:stop/1", [Mod]),
            try ok = Mod:stop(State)
            catch _:Err ->
                    ?LOG_DEBUG("Module ~s failed at stopping: ~p", [Mod, Err]),
                    {error, Err}
            end;
        false ->
            ?LOG_DEBUG("Module ~s doesn't export stop/1", [Mod]),
            ok
    end.

-spec handle_event(module(), event(), info(), state())
      -> {ok, state()} | {error, term()}.
handle_event(Mod, Event, Info, State) ->
    ?LOG_DEBUG("Calling ~s:handle_event/3", [Mod]),
    try
        case Mod:handle_event(Event, Info, State) of
            ok ->
                {ok, State};
            {ok, State1} ->
                {ok, State1}
        end
    catch _:Err ->
            ?LOG_DEBUG("Module ~s failed at handling '~s' event: ~p",
                       [Mod, Event, Err]),
            {error, Err}
    end.

-spec options(module()) -> options().
options(Mod) ->
    Mod:options().

-spec get_opt(module(), option()) -> term().
get_opt(Mod, Opt) ->
    {ok, #{Mod := Opts}} = application:get_env(modules),
    {Opt, Val} = proplists:lookup(Opt, Opts),
    Val.
