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

%%% @doc An eturnal module adds functionality to the eturnal server. It is to be
%%% named `mod_foo', where `foo' describes the added functionality. The module
%%% must export `handle_event/2' and `options/0' callbacks, and may optionally
%%% export `start/0' and `stop/0' functions.
%%%
%%% The optional `start/0' function must return `ok' or `{ok, Events}', where
%%% `Events' is the list of events the module is interested in. Currently, the
%%% following events may be triggered: `stun_query', `turn_session_start', and
%%% `turn_session_stop'. If the `start/0' function doesn't return a list of
%%% events (or returns `{ok, [all]}'), the `handle_event/2' callback will be
%%% called for all events.
%%%
%%% The `handle_event/2' function is called with the event name as the first
%%% argument and a map with metadata related to the event as the second. The
%%% contents of that map depend on the event. Note that the `handle_event/2'
%%% callback is executed by the process handling the STUN/TURN session, so it
%%% should never block. If it might, and/or if it needs some `#state{}', one or
%%% more handler processes must be created.
%%%
%%% The `options/0' callback returns an {@type options()} tuple with two
%%% elements. The first is a map of configuration options, where the keys are
%%% the {@type option} names specified as {@type atom()}s, and the values are
%%% functions that validate the option values. Those functions are returned by
%%% the <a
%%% href="https://github.com/processone/yval/blob/master/src/yval.erl">yval</a>
%%% library. The second tuple element is a list of optional tuples to specify
%%% `{required, [Options]}' and/or `{defaults, #{Option => Value}}'. For
%%% example:
%%%
%%% ```
%%% options() ->
%%%     {#{threshold => yval:pos_int()},
%%%      [{defaults,
%%%       #{threshold => 42}}]}.
%%% '''
%%%
%%% The option values are queried by calling {@link eturnal_module:get_opt/2}
%%% with the `?MODULE' name as the first and the option name as the second
%%% argument. Note that the lookup is very efficient, so there's no point in
%%% saving option values into some `#state{}'.
%%%
%%% The optional `stop/0' function must return `ok'. Note that the `start/0' and
%%% `stop/0' functions might not just be called on eturnal startup and shutdown,
%%% but also on configuration reloads.
%%%
%%% If the module depends on other applications, those must be added to the
%%% `rebar.config' file, but not to the app file. They are to be started by
%%% calling {@link eturnal_module:ensure_deps/2}, where the first argument is
%%% the `?MODULE' name and the second is the dependency's name.
%%%
%%% The module is enabled by adding its configuration to the `modules' section
%%% of eturnal's configuration file as described in `doc/overview.edoc'. The
%%% module configuration options are to be documented in that file as well.

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
-type options() :: {yval:validators(), yval:validator_option()}.
-type dep() :: atom().

-callback start() -> ok | {ok, [event()]}.
-callback stop() -> ok.
-callback handle_event(event(), info()) -> ok.
-callback options() -> options().

-optional_callbacks([start/0, stop/0]).

-include_lib("kernel/include/logger.hrl").
-ifdef(old_persistent_term).
-define(m(Name), {m, Name}).
-define(e(Name), {e, Name}).
-else.
-define(m(Name), {?MODULE, m, Name}).
-define(e(Name), {?MODULE, e, Name}).
-endif.

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
    ok = lists:foreach(
           fun(Mod) ->
                   ?LOG_DEBUG("Calling ~s:handle_event/2", [Mod]),
                   try ok = Mod:handle_event(Event, Info)
                   catch _:Err ->
                           ?LOG_ERROR("Module ~s failed at handling '~s': ~p",
                                      [Mod, Event, Err])
                   end
           end, get_subscribers(Event)).

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
-spec unsubscribe_events(module()) -> ok.
-spec get_subscribers(event()) -> [module()].
-ifdef(old_persistent_term).
subscribe_events(Event, Mod) when is_atom(Event) ->
    ok = subscribe_events([Event], Mod);
subscribe_events(Events, Mod) ->
    case ets:whereis(events) of
        undefined ->
            events = ets:new(events, [named_table, {read_concurrency, true}]);
        _TID ->
            ok
    end,
    Entries = lists:map(
                fun(Event) ->
                        case ets:lookup(events, ?e(Event)) of
                            [] ->
                                {?e(Event), [Mod]};
                            [{_, Ms}] ->
                                {?e(Event), ordsets:add_element(Mod, Ms)}
                        end
                end, Events),
    true = ets:insert(events, [{?m(Mod), Events} | Entries]),
    ok.

unsubscribe_events(Mod) ->
    case ets:lookup(events, ?m(Mod)) of
        [] ->
            ok;
        [{?m(Mod), Es}] ->
            Entries = lists:map(
                        fun(Event) ->
                                [{_, Ms}] = ets:lookup(events, ?e(Event)),
                                {?e(Event), ordsets:del_element(Mod, Ms)}
                        end, Es),
            true = ets:insert(events, Entries),
            true = ets:delete(?m(Mod)),
            ok
    end.

get_subscribers(Event) ->
    Mods = lists:map(
             fun(E) ->
                     case ets:lookup(events, ?e(E)) of
                         [] ->
                             ordsets:new();
                         [{_, Ms}] ->
                             Ms
                     end
             end, [Event, all]),
    ordsets:union(Mods).
-else.
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
unsubscribe_events(Mod) ->
    Es = persistent_term:get(?m(Mod), []),
    _R = persistent_term:erase(?m(Mod)),
    ok = lists:foreach(
           fun(Event) ->
                   Ms = persistent_term:get(?e(Event)),
                   ok = persistent_term:put(?e(Event),
                                            ordsets:del_element(Mod, Ms))
           end, Es).

get_subscribers(Event) ->
    Mods1 = persistent_term:get(?e(Event), ordsets:new()),
    Mods2 = persistent_term:get(?e(all), ordsets:new()),
    ordsets:union(Mods1, Mods2).
-endif.

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
