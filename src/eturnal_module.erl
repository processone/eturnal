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
         stop/1,
         handle_event/2,
         options/1,
         get_opt/2,
         ensure_deps/2]).
-export_type([event/0,
              events/0,
              info/0,
              options/0]).

-type event() :: atom().
-type events() :: [atom()].
-type info() :: #{atom() => term()}.
-type option() :: atom().
-type options() :: {yval:validators(), proplists:proplist()}.
-type dep() :: atom().

-callback start() -> ok | {ok, [event()]}.
-callback stop() -> ok.
-callback handle_event(event(), info()) -> ok.
-callback options() -> options().

-optional_callbacks([start/0, stop/0]).

-include_lib("kernel/include/logger.hrl").
-define(m(Name), {?MODULE, m, Name}).
-define(e(Name), {?MODULE, e, Name}).

%% API.

-spec start(module()) -> ok | {error, term()}.
start(Mod) ->
    case erlang:function_exported(Mod, start, 0) of
        true ->
            ?LOG_DEBUG("Calling ~s:start/0", [Mod]),
            try
                case Mod:start() of
                    ok ->
                        ok = subscribe_events(all, Mod);
                    {ok, Events} ->
                        ok = subscribe_events(Events, Mod)
                end
            catch _:Err ->
                    ?LOG_DEBUG("Module ~s failed at starting: ~p", [Mod, Err]),
                    {error, Err}
            end;
        false ->
            ?LOG_DEBUG("Module ~s doesn't export start/0", [Mod]),
            ok = subscribe_events(all, Mod)
    end.

-spec stop(module()) -> ok | {error, term()}.
stop(Mod) ->
    ok = unsubscribe_events(Mod),
    case erlang:function_exported(Mod, stop, 1) of
        true ->
            ?LOG_DEBUG("Calling ~s:stop/1", [Mod]),
            try ok = Mod:stop()
            catch _:Err ->
                    ?LOG_DEBUG("Module ~s failed at stopping: ~p", [Mod, Err]),
                    {error, Err}
            end;
        false ->
            ?LOG_DEBUG("Module ~s doesn't export stop/1", [Mod]),
            ok
    end.

-spec handle_event(event(), info()) -> ok.
handle_event(Event, Info) ->
    ?LOG_DEBUG("Got '~s' event", [Event]),
    Mods1 = persistent_term:get(?e(Event), ordsets:new()),
    Mods2 = persistent_term:get(?e(all), ordsets:new()),
    ok = lists:foreach(
           fun(Mod) ->
                   ?LOG_DEBUG("Calling ~s:handle_event/2", [Mod]),
                   try ok = Mod:handle_event(Event, Info)
                   catch _:Err ->
                           ?LOG_ERROR("Module ~s failed at handling '~s': ~p",
                                      [Mod, Event, Err])
                   end
           end, ordsets:union(Mods1, Mods2)).

-spec options(module()) -> options().
options(Mod) ->
    Mod:options().

-spec get_opt(module(), option()) -> term().
get_opt(Mod, Opt) ->
    #{Mod := Opts} = eturnal:get_opt(modules),
    {Opt, Val} = proplists:lookup(Opt, Opts),
    Val.

-spec ensure_deps(module(), [dep()]) -> ok | no_return().
ensure_deps(Mod, Deps) ->
    lists:foreach(fun(Dep) -> ok = ensure_dep(Mod, Dep) end, Deps).

%% Internal functions.

-spec subscribe_events(event() | [event()], module()) -> ok.
subscribe_events(Event, Mod) when is_atom(Event) ->
    ok = subscribe_events([Event], Mod);
subscribe_events(Events, Mod) ->
    ok = persistent_term:put(?m(Mod), Events),
    ok = lists:foreach(
           fun(Event) ->
                   Ms = persistent_term:get(?e(Event), ordsets:new()),
                   ok = persistent_term:put(?e(Event),
                                            ordsets:add_element(Mod, Ms))
           end, Events).

-spec unsubscribe_events(module()) -> ok.
unsubscribe_events(Mod) ->
    Es = persistent_term:get(?m(Mod), []),
    _R = persistent_term:erase(?m(Mod)),
    ok = lists:foreach(
           fun(Event) ->
                   Ms = persistent_term:get(?e(Event), ordsets:new()),
                   ok = persistent_term:put(?e(Event),
                                            ordsets:del_element(Mod, Ms))
           end, Es).

-spec ensure_dep(module(), dep()) -> ok | no_return().
ensure_dep(Mod, Dep) ->
    case application:ensure_all_started(Dep) of
        {ok, _Apps} ->
            ?LOG_DEBUG("Dependency ~s was available already", [Dep]),
            ok;
        {error, _Reason1} ->
            ?LOG_DEBUG("Dependency ~s isn't started, loading it", [Dep]),
            case start_app(Dep) of
                ok ->
                    ?LOG_INFO("Dependency ~s is available", [Dep]),
                    ok;
                {error, _Reason2} ->
                    ?LOG_CRITICAL(
                      "Dependency ~s is missing; install it below ~s, or "
                      "point ERL_LIBS to it, or disable ~s",
                      [Dep, code:lib_dir(), Mod]),
                    eturnal:abort(dependency_failure)
            end
    end.

-spec start_app(dep()) -> ok | {error, term()}.
start_app(App) ->
    case load_app(App) of
        ok ->
            ?LOG_DEBUG("Loaded ~s, trying to start it", [App]),
            case application:ensure_started(App) of
                ok ->
                    ok;
                {error, {not_started, Dep}} ->
                    ?LOG_DEBUG("~s depends on ~s, loading it", [App, Dep]),
                    case start_app(Dep) of
                        ok ->
                            start_app(App);
                        {error, _Reason} = Err ->
                            Err
                    end;
                {error, Reason} = Err ->
                    ?LOG_DEBUG("Cannot start ~s: ~p", [App, Reason]),
                    Err
            end;
        {error, Reason} = Err ->
            ?LOG_DEBUG("Cannot load ~s: ~p", [App, Reason]),
            Err
    end.

-spec load_app(dep()) -> ok | {error, term()}.
load_app(App) ->
    try
        LibDir = code:lib_dir(),
        AppDir = lists:max(filelib:wildcard([App, $*], LibDir)),
        EbinDir = filename:join([LibDir, AppDir, "ebin"]),
        BeamFiles = filelib:wildcard("*.beam", EbinDir),
        Mods = lists:map(
                 fun(File) ->
                         list_to_atom(
                           unicode:characters_to_list(
                             string:replace(File, ".beam", "", trailing)))
                 end, BeamFiles),
        true = code:add_path(EbinDir),
        case lists:any(fun(Mod) ->
                               code:module_status(Mod) =:= not_loaded
                       end, Mods) of
            true ->
                ?LOG_DEBUG("Loading modules: ~p", [Mods]),
                ok = code:atomic_load(Mods);
            false ->
                ?LOG_DEBUG("Modules loaded already: ~p", [Mods]),
                ok
        end
    catch _:Err ->
              {error, Err}
    end.
