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

-module(eturnal_logger).
-export([start/0,
         stop/0,
         progress_filter/2,
         reconfigure/0,
         is_valid_level/1,
         get_level/0,
         set_level/1,
         flush/0]).
-export_type([level/0]).

-include_lib("kernel/include/logger.hrl").
-define(ETURNAL_HANDLER, eturnal_log).
-define(LOG_FILE_NAME, "eturnal.log").
-define(VALID_LEVELS, [critical, error, warning, notice, info, debug]).

-type logger_config() :: #{file => file:filename(),
                           file_check => non_neg_integer(),
                           max_no_bytes => pos_integer() | infinity,
                           max_no_files => non_neg_integer(),
                           flush_qlen => pos_integer(),
                           sync_mode_qlen => non_neg_integer(),
                           drop_mode_qlen => pos_integer()}.

% Subset of logger:level():
-type level() :: critical | error | warning | notice | info | debug.

% Currently not exported by logger_formatter:
-type metakey() :: atom() | [atom()].
-type template() :: [metakey() | {metakey(), template(), template()} |
                     string()].

%% API.

-spec start() -> ok.
start() ->
    ok = init(get_config()),
    ok = configure_default_handler().

-spec stop() -> ok.
stop() ->
    ok = terminate().

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
    Config = get_config(),
    case logger:get_handler_config(?ETURNAL_HANDLER) of
        {ok, _OldConfig} ->
            case logger:set_handler_config(?ETURNAL_HANDLER, config, Config) of
                ok ->
                    ok;
                {error, {illegal_config_change, _, _, _}} ->
                    ?LOG_ERROR("New logging settings require restart")
            end,
            ok = set_level();
        {error, {not_found, _}} ->
            ok = init(Config)
    end,
    ok = configure_default_handler().

-spec is_valid_level(atom()) -> boolean().
is_valid_level(Level) ->
    lists:member(Level, ?VALID_LEVELS).

-spec get_level() -> logger:level() | all | none.
get_level() ->
    #{level := Level} = logger:get_primary_config(),
    Level.

-spec set_level(level()) -> ok.
set_level(Level) ->
    ok = logger:set_primary_config(level, Level),
    ok = logger:update_formatter_config(
           ?ETURNAL_HANDLER, template, format_template()).

%% Internal functions.

-spec init(logger_config()) -> ok.
init(Config) ->
    FmtConfig = #{time_designator => $\s,
                  max_size => 100 * 1024,
                  single_line => false},
    case logger:add_primary_filter(progress_report,
                                   {fun ?MODULE:progress_filter/2, stop}) of
        ok ->
            ok;
        {error, {already_exist, _}} ->
            ok
    end,
    case logger:add_handler(?ETURNAL_HANDLER, logger_std_h,
                            #{level => all,
                              config => Config,
                              formatter => {logger_formatter, FmtConfig}}) of
        ok ->
            ok;
        {error, {already_exist, _}} ->
            ok
    end,
    set_level().

-spec get_config() -> logger_config().
-ifdef(old_logger). % Erlang/OTP < 21.3.
get_config() ->
    Config = #{sync_mode_qlen => 1000,
               drop_mode_qlen => 1000, % Never switch to synchronous mode.
               flush_qlen => 5000},
    case get_log_file() of
        LogFile when is_list(LogFile) ->
            case eturnal:get_opt(log_rotate_size) of
                Size when is_integer(Size) ->
                    ?LOG_WARNING("Log rotation requires newer Erlang/OTP "
                                 "version, ignoring 'log_rotate_*' options");
                infinity ->
                    ok
            end,
            Config#{type => {file, LogFile}};
        stdout ->
            Config
    end.
-else.
get_config() ->
    Config = #{sync_mode_qlen => 1000,
               drop_mode_qlen => 1000, % Never switch to synchronous mode.
               flush_qlen => 5000},
    case get_log_file() of
        LogFile when is_list(LogFile) ->
            Config#{file => LogFile,
                    file_check => 1000,
                    max_no_bytes => eturnal:get_opt(log_rotate_size),
                    max_no_files => eturnal:get_opt(log_rotate_count)};
        stdout ->
            Config
    end.
-endif.

-spec get_log_file() -> file:filename() | stdout.
get_log_file() ->
    case eturnal:get_opt(log_dir) of
        LogDir when is_binary(LogDir) ->
            LogFile = filename:join(LogDir, <<?LOG_FILE_NAME>>),
            unicode:characters_to_list(LogFile);
        stdout ->
            stdout
    end.

-spec set_level() -> ok.
set_level() ->
    ok = set_level(eturnal:get_opt(log_level)),
    ok = logger:update_formatter_config(
           ?ETURNAL_HANDLER, template, format_template()).

-spec logging_to_journal() -> boolean().
logging_to_journal() ->
    (eturnal:get_opt(log_dir) =:= stdout) and
        (os:getenv("JOURNAL_STREAM") =/= false).

-spec format_template() -> template().
format_template() ->
    format_prefix() ++ format_message() ++ format_suffix().

-spec format_prefix() -> template().
format_prefix() ->
    case logging_to_journal() of
        true ->
            ["[", level, "] "];
        false ->
            [time, " [", level, "] "]
    end.

-spec format_message() -> template().
format_message() ->
    % For progress reports:
    [{logger_formatter, [[logger_formatter, title], ":", io_lib:nl()], []},
    % The actual log message:
     msg].

-spec format_suffix() -> template().
format_suffix() ->
    case get_level() of
        debug ->
            % Append (Module:Function/Arity and maybe :Line), if available:
            [{mfa, [" (", mfa, {line, [":", line], []}, ")"], []},
             io_lib:nl()];
        _Level ->
            [io_lib:nl()]
    end.

-spec configure_default_handler() -> ok.
configure_default_handler() ->
    case eturnal:get_opt(log_dir) of
        LogDir when is_binary(LogDir) ->
            ok = logger:set_handler_config(default, level, warning);
        stdout ->
            ok = logger:set_handler_config(default, level, none)
    end.

-spec flush() -> ok.
flush() ->
    lists:foreach(
      fun(#{id := HandlerID, module := logger_std_h}) ->
              logger_std_h:filesync(HandlerID);
         (_) ->
              ok
      end, logger:get_handler_config()).

-spec terminate() -> ok.
terminate() ->
    ok = flush(),
    ok = logger:set_handler_config(default, level, notice),
    ok = logger:remove_primary_filter(progress_report),
    ok = logger:remove_handler(?ETURNAL_HANDLER).
