# Changelog

All notable changes to this project will be documented in this file. This
project adheres to [Semantic Versioning][SemVer].

## [Unreleased]
### Added
- Add `mod_log_stun` for logging STUN requests. Without this module, they will
  now only show up in the debug log output.

### Changed
- Always log reason for TCP/TLS connection termination (at info level).

### Fixed
- Make the `eturnalctl sessions` command work with recent versions of the `stun`
  application.

## [1.3.0] - 2020-08-26
### Added
- Add `eturnalctl info` command, which prints some details regarding the running
  eturnal instance.
- Add the TURN session duration to the `eturnalctl sessions` output.
- Document the module API for developers.

### Changed
- Refactor the module API to avoid bottlenecks.

## [1.2.1] - 2020-08-16
### Fixed
- Strip the BEAM files shipped with the binary release. Due to a bug in the
  build tooling, this didn't happen for the previous release.

## [1.2.0] - 2020-08-16
### Added
- Add experimental support for modules and include a `mod_example` with the
  source code. The APIs aren't documented yet and may change in the future.
- Include `mod_stats_influx`, a module for logging STUN/TURN events/statistics
  to InfluxDB, contributed by Marc Schink.

## [1.1.0] - 2020-07-22
### Added
- Add `eturnalctl session` command, which lists some details about the currently
  active TURN sessions.

### Changed
- Append session ID, transport, username, and client IP addresses/ports to
  STUN/TURN log messages.
- Append relay/peer IP addresses/ports to TURN log messages.
- Log amount of relayed traffic per TURN session.
- Log plain STUN (Binding) responses.
- Log more info level messages during TURN sessions.
- Log error responses sent to STUN/TURN clients.

### Fixed
- Make configuration reloads performed after changing the `listen` configuration
  more robust against timing issues.
- Let eturnalctl commands that query the running node fail gracefully if eturnal
  isn't running.

## [1.0.0] - 2020-07-13
### Added
- Allow for setting the `log_dir` option to the special value `stdout`, which
  tells eturnal to print log messages to the standard output rather than logging
  to a file.
- Publish DEB and RPM packages, and adjust the documentation accordingly.

### Changed
- Allow for binding to privileged ports (if started via systemd).
- Disable TURN support in the example configuration file.
- If the distribution provides an `epmd.service`, make sure eturnal uses it
  rather than starting its own epmd instance.
- Don't bind epmd to 127.0.0.1 by default.

### Fixed
- Only signal readiness to systemd if eturnal's startup actually was successful.

## [0.8.0] - 2020-07-08
### Added
- Support systemd's `notify` startup type.
- Support systemd's service watchdog feature.

### Changed
- Remove `max_allocations` option from the documentation and from the example
  configuration. The `stun` application currently ignores this option, and it's
  not all that useful with ephemeral TURN credentials anyway.

### Fixed
- Don't ignore the `log_level` option when the configuration is reloaded.

## [0.7.0] - 2020-07-07
### Added
- Ship documentation and license with binary release archive.
- Add reference documentation which can be built by calling `rebar3 edoc` within
  the source directory.
- Allow for starting up eturnal without release boot file by calling a command
  such as `erl -conf file '"/etc/eturnal.yml"' -s eturnal` (assuming the BEAM
  files are in the code path).

### Changed
- Refuse TURN relaying from/to loopback addresses by default.

## [0.6.0] - 2020-07-02
### Added
- Include an example init script for non-systemd platforms.

### Changed
- Log more (and improved) info and debug level messages.
- Allow for starting up eturnal without configured secret if TURN is disabled.

## [0.5.0] - 2020-06-30
### Added
- Let `eturnalctl version` print the version string of the running release.
- Add an initial version of a test suite.

### Changed
- Allow non-root users to run the eturnalctl script if they have eturnal's
  Erlang cookie.
- Make the release directory freely relocatable.

## [0.4.0] - 2020-06-28
### Fixed
- Fix TURN authentication on Erlang/OTP versions older than 22.1.

## [0.3.0] - 2020-06-28
### Changed
- Change systemd service type in order to support systemd versions older than
  240.

### Fixed
- Make sure the eturnalctl script can be invoked by the superuser.
- Fix compatibility with Erlang/OTP 21.0, 21.1, and 21.2.

## [0.2.0] - 2020-06-25
### Changed
- Add Erlang process ID to log messages.

## [0.1.0] - 2020-06-24
### Changed
- Allow for configuring the same (port, transport) combination on different IP
  addresses.

### Fixed
- Fix parameter expansion in eturnalctl script which prevented eturnal from
  starting up.
- In the README section that describes building from source, don't forget to
  mention that rebar3 needs to be made executable.

## [0.0.1] - 2020-06-23
### Added
- Initial (pre-)release of the eturnal STUN/TURN server.

[Unreleased]: https://github.com/processone/eturnal/compare/1.3.0...HEAD
[1.3.0]: https://github.com/processone/eturnal/releases/tag/1.3.0
[1.2.1]: https://github.com/processone/eturnal/releases/tag/1.2.1
[1.2.0]: https://github.com/processone/eturnal/releases/tag/1.2.0
[1.1.0]: https://github.com/processone/eturnal/releases/tag/1.1.0
[1.0.0]: https://github.com/processone/eturnal/releases/tag/1.0.0
[0.8.0]: https://github.com/processone/eturnal/releases/tag/0.8.0
[0.7.0]: https://github.com/processone/eturnal/releases/tag/0.7.0
[0.6.0]: https://github.com/processone/eturnal/releases/tag/0.6.0
[0.5.0]: https://github.com/processone/eturnal/releases/tag/0.5.0
[0.4.0]: https://github.com/processone/eturnal/releases/tag/0.4.0
[0.3.0]: https://github.com/processone/eturnal/releases/tag/0.3.0
[0.2.0]: https://github.com/processone/eturnal/releases/tag/0.2.0
[0.1.0]: https://github.com/processone/eturnal/releases/tag/0.1.0
[0.0.1]: https://github.com/processone/eturnal/releases/tag/0.0.1
[SemVer]: https://semver.org/spec/v2.0.0.html
