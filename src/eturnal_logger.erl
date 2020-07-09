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

-module(eturnal_logger).
-export([start/0, progress_filter/2, reconfigure/0, is_valid_level/1,
         get_level/0, set_level/1, flush/0]).
-export_type([level/0]).

-include_lib("kernel/include/logger.hrl").
-define(LOG_FILE_NAME, "eturnal.log").
-define(VALID_LEVELS, [critical, error, warning, notice, info, debug]).

% Subset of logger:level():
-type level() :: critical | error | warning | notice | info | debug.

% Currently not exported by logger_formatter:
-type metakey() :: atom() | [atom()].
-type template() :: [metakey() | {metakey(), template(), template()} |
                     string()].

%% API.

-spec start() -> ok.
start() ->
    case get_config() of
        {ok, Config} ->
            init(Config);
        none -> % Logging disabled.
            ok
    end,
    ok = configure_default_handler().

-spec progress_filter(logger:log_event(), any()) -> logger:filter_return().
progress_filter(#{level := info,
                  msg := {report, #{label := {_, progress}}}} = Event,
                _Extra) ->
    case get_level() of
        debug ->
            logger_filters:progress(Event#{level => debug}, log);
        _ ->
            stop
    end;
progress_filter(_Event, _Extra) ->
    ignore.

-spec reconfigure() -> ok.
reconfigure() ->
    case get_config() of
        {ok, Config} ->
            case logger:get_handler_config(eturnal_log) of
                {ok, _OldConfig} ->
                    case logger:set_handler_config(eturnal_log, config,
                                                   Config) of
                        ok ->
                            ok;
                        {error, {illegal_config_change, _, _, _}} ->
                            ?LOG_ERROR("New logging settings require restart")
                    end,
                    ok = set_level();
                {error, {not_found, _}} ->
                    ok = init(Config)
            end;
        none ->
            case logger:remove_handler(eturnal_log) of
                ok ->
                    ok;
                {error, {not_found, _}} ->
                    ok
            end
    end,
    ok = configure_default_handler().

-spec is_valid_level(atom()) -> boolean().
is_valid_level(Level) ->
    lists:member(Level, ?VALID_LEVELS).

-spec get_level() -> logger:level().
get_level() ->
    #{level := Level} = logger:get_primary_config(),
    Level.

-spec set_level(level()) -> ok.
set_level(Level) ->
    ok = logger:set_primary_config(level, Level).

-spec flush() -> ok.
flush() ->
    lists:foreach(
      fun(#{id := HandlerId, module := logger_std_h}) ->
              logger_std_h:filesync(HandlerId);
         (_) ->
              ok
      end, logger:get_handler_config()).

%% Internal functions.

-spec init(map()) -> ok.
init(Config) ->
    FmtConfig = #{time_designator => $\s,
                  max_size => 100 * 1024,
                  single_line => false,
                  template => format_template()},
    case logger:add_primary_filter(progress_report,
                                   {fun ?MODULE:progress_filter/2, stop}) of
        ok ->
            ok;
        {error, {already_exist, _}} ->
            ok
    end,
    case logger:add_handler(eturnal_log, logger_std_h,
                            #{level => all,
                              config => Config,
                              formatter => {logger_formatter, FmtConfig}}) of
        ok ->
            ok;
        {error, {already_exist, _}} ->
            ok
    end,
    set_level().

-spec get_config() -> {ok, map()} | none.
-ifdef(old_logger). % Erlang/OTP < 21.3.
get_config() ->
    Config = #{sync_mode_qlen => 1000,
               drop_mode_qlen => 1000, % Never switch to synchronous mode.
               flush_qlen => 5000},
    case get_log_file() of
        {ok, stdout} ->
            {ok, Config};
        {ok, LogFile} ->
            case application:get_env(log_rotate_size) of
                {ok, infinity} ->
                    ok;
                {ok, Size} when is_integer(Size) ->
                    ?LOG_WARNING("Log rotation requires newer Erlang/OTP "
                                 "version, ignoring 'log_rotate_*' options")
            end,
            {ok, Config#{type => {file, LogFile}}};
        none ->
            none
    end.
-else.
get_config() ->
    Config = #{sync_mode_qlen => 1000,
               drop_mode_qlen => 1000, % Never switch to synchronous mode.
               flush_qlen => 5000},
    case get_log_file() of
        {ok, stdout} ->
            {ok, Config};
        {ok, LogFile} ->
            {ok, MaxNoBytes} = application:get_env(log_rotate_size),
            {ok, MaxNoFiles} = application:get_env(log_rotate_count),
            {ok, Config#{file_check => 1000,
                         file => LogFile,
                         max_no_bytes => MaxNoBytes,
                         max_no_files => MaxNoFiles}};
        none ->
            none
    end.
-endif.

-spec get_log_file() -> {ok, file:filename() | stdout} | none.
get_log_file() ->
    case application:get_env(log_dir) of
        {ok, LogDir} when is_binary(LogDir) ->
            LogFile = filename:join(LogDir, <<?LOG_FILE_NAME>>),
            {ok, unicode:characters_to_list(LogFile)};
        {ok, stdout} ->
            {ok, stdout};
        {ok, none} ->
            none
    end.

-spec set_level() -> ok.
set_level() ->
    {ok, Level} = application:get_env(log_level),
    ok = set_level(Level).

-spec format_template() -> template().
format_template() ->
    [time, " [", level, "] ",
     % For progress reports:
     {logger_formatter, [[logger_formatter, title], ":", io_lib:nl()], []},
     % The actual log message:
     msg,
     % Append ("<PID>" and, if available, "@Module:Function/Arity:Line"):
     " (", pid, {mfa, ["@", mfa, {line, [":", line], []}], []}, ")",
     io_lib:nl()].

-spec configure_default_handler() -> ok.
configure_default_handler() ->
    case application:get_env(log_dir) of
        {ok, stdout} ->
            ok = logger:set_handler_config(default, level, none);
        {ok, _Dir} ->
            ok = logger:set_handler_config(default, level, warning);
        none ->
            ok = logger:set_handler_config(default, level, critical)
    end.
