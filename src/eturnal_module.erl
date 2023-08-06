%%% eturnal STUN/TURN server.
%%%
%%% Copyright (c) 2020-2023 Holger Weiss <holger@zedat.fu-berlin.de>.
%%% Copyright (c) 2020-2023 ProcessOne, SARL.
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
%%% may export `start/0', `stop/0', `handle_event/2', and `options/0' functions.
%%%
%%% If a `start/0' callback is exported, it must return `ok' or `{ok, Events}',
%%% where `Events' is either a single {@link event()} or a list of {@link
%%% events()} the module is interested in. Currently, the following events may
%%% be triggered: `stun_query', `turn_session_start', `turn_session_stop', and
%%% `protocol_error'.
%%%
%%% If a `start/0' function is exported and subscribes to one or more events, a
%%% `handle_event/2' callback <em>must</em> be exported as well. It is called
%%% with the {@link event()} name as the first argument and an {@link info()}
%%% map with related data as the second. The contents of that map depend on the
%%% event. Note that the `handle_event/2' function is executed in the context of
%%% the process handling the STUN/TURN session, so it should never block. If it
%%% might, and/or if it needs some state, one or more handler processes must be
%%% created.
%%%
%%% The `options/0' callback returns an {@link options()} tuple with two
%%% elements. The first is a map of module configuration options, where the keys
%%% are the {@link option()} names and the values are functions that validate
%%% the option values. Those functions are returned by the <a
%%% href="https://hex.pm/packages/yval">yval</a> library, see the documentation
%%% for the list of <a href="https://hexdocs.pm/yval/yval.html#index">available
%%% validators</a>. The second element is a list of optional tuples to specify
%%% any `{required, [Options]}' and/or `{defaults, #{Option => Value}}'. For
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
%%% with the module name as the first and the {@link option()} name as the
%%% second argument. Note that the lookup is very efficient, so there's no point
%%% in saving option values into some state. If the module has no configuration
%%% options, the `options/0' function may be omitted.
%%%
%%% The optional `stop/0' callback must return `ok'. Note that the `start/0' and
%%% `stop/0' functions might not just be called on eturnal startup and shutdown,
%%% but also on configuration reloads.
%%%
%%% If the module depends on other applications, those must be added to the
%%% `rebar.config' file, but not to the app file. They are to be started by
%%% calling {@link eturnal_module:ensure_deps/2}, where the first argument is
%%% the module name and the second is a list of dependency names. Note that
%%% there's no need to list transitive dependencies.
%%%
%%% The module is enabled by adding its configuration to the `modules' section
%%% of eturnal's configuration file as described in `doc/overview.edoc'. The
%%% module configuration options are to be documented in that file as well.

-module(eturnal_module).
-export([init/0,
         terminate/0,
         start/1,
         stop/1,
         handle_event/2,
         options/1,
         get_opt/2,
         ensure_deps/2]).
-export_type([dep/0,
              event/0,
              events/0,
              info/0,
              option/0,
              options/0]).

-type dep() :: atom().
-type event() :: atom().
-type events() :: [event()].
-type info() :: #{atom() => term()}.
-type option() :: atom().
-type options() :: {yval:validators(), [yval:validator_option()]}.

-callback start() -> ok | {ok, event() | [event()]}.
-callback stop() -> ok.
-callback handle_event(event(), info()) -> ok.
-callback options() -> options().

-optional_callbacks([start/0, stop/0, handle_event/2, options/0]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-ifdef(old_persistent_term).
-define(m(Name), {m, Name}).
-define(e(Name), {e, Name}).
-else.
-define(m(Name), {?MODULE, m, Name}).
-define(e(Name), {?MODULE, e, Name}).
-endif.

%% API.

-spec init() -> ok.
-ifdef(old_persistent_term).
init() ->
    events = ets:new(events, [named_table, {read_concurrency, true}]),
    ok.
-else.
init() ->
    ok.
-endif.

-spec terminate() -> ok.
-ifdef(old_persistent_term).
terminate() ->
    true = ets:delete(events),
    ok.
-else.
terminate() ->
    ok.
-endif.

-spec start(module()) -> ok | {error, term()}.
start(Mod) ->
    case erlang:function_exported(Mod, start, 0) of
        true ->
            ?LOG_DEBUG("Calling ~s:start/0", [Mod]),
            try Mod:start() of
                ok ->
                    ok;
                {ok, Events} ->
                    ok = subscribe_events(Events, Mod)
            catch _:Err:Stack ->
                    ?LOG_DEBUG("Module ~s failed at starting:~n~p",
                               [Mod, Stack]),
                    {error, Err}
            end;
        false ->
            ?LOG_DEBUG("Module ~s doesn't export start/0", [Mod])
    end.

-spec stop(module()) -> ok | {error, term()}.
stop(Mod) ->
    ok = unsubscribe_events(Mod),
    case erlang:function_exported(Mod, stop, 0) of
        true ->
            ?LOG_DEBUG("Calling ~s:stop/1", [Mod]),
            try ok = Mod:stop()
            catch _:Err:Stack ->
                    ?LOG_DEBUG("Module ~s failed at stopping:~n~p",
                               [Mod, Stack]),
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
                   catch _:_Err:Stack ->
                           ?LOG_ERROR("Module ~s failed at handling '~s':~n~p",
                                      [Mod, Event, Stack])
                   end
           end, get_subscribers(Event)).

-spec options(module()) -> options().
options(Mod) ->
    case erlang:function_exported(Mod, options, 0) of
        true ->
            ?LOG_DEBUG("Calling ~s:options/0", [Mod]),
            Mod:options();
        false ->
            ?LOG_DEBUG("Module ~s doesn't export options/1", [Mod]),
            {#{}, []}
    end.

-spec get_opt(module(), option()) -> term().
get_opt(Mod, Opt) ->
    #{Mod := #{Opt := Val}} = eturnal:get_opt(modules),
    Val.

-spec ensure_deps(module(), [dep()]) -> ok.
ensure_deps(Mod, Deps) ->
    lists:foreach(fun(Dep) -> ok = ensure_dep(Mod, Dep) end, Deps).

%% Internal functions.

-ifdef(old_persistent_term).
-spec subscribe_events(event() | [event()], module()) -> ok.
subscribe_events(Event, Mod) when is_atom(Event) ->
    ok = subscribe_events([Event], Mod);
subscribe_events(Events, Mod) ->
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

-spec unsubscribe_events(module()) -> ok.
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
            true = ets:delete(events, ?m(Mod)),
            ok
    end.

-spec get_subscribers(event()) -> [module()].
get_subscribers(Event) ->
    case ets:lookup(events, ?e(Event)) of
        [] ->
            [];
        [{_, Ms}] ->
            Ms
    end.
-else.
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
                   Ms = persistent_term:get(?e(Event)),
                   ok = persistent_term:put(?e(Event),
                                            ordsets:del_element(Mod, Ms))
           end, Es).

-spec get_subscribers(event()) -> [module()].
get_subscribers(Event) ->
    persistent_term:get(?e(Event), []).
-endif.

-spec ensure_dep(module(), dep()) -> ok.
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
                    eturnal:abort({dependency_failure, Mod, Dep})
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
        AppDir = lists:max(filelib:wildcard([App, "{,-*}"], LibDir)),
        EbinDir = filename:join([LibDir, AppDir, "ebin"]),
        AppFile = filename:join(EbinDir, [App, ".app"]),
        {ok, [{application, App, Props}]} = file:consult(AppFile),
        Mods = proplists:get_value(modules, Props),
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

%% EUnit tests.

-ifdef(EUNIT).
load_test_() ->
    [?_assertEqual(ok, start_app(eunit)),
     ?_assertMatch({error, _}, start_app(nonexistent))].
-endif.
