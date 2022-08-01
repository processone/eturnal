# Changelog

All notable changes to this project will be documented in this file. This
project adheres to [Semantic Versioning][SemVer].

## [Unreleased]
### Added
- Improve TCP/TLS performance if no traffic shaper is configured (using the
  `max_bps` option).
- `mod_stats_prometheus`: Add a counter for STUN/TURN protocol errors, bucketed
  by transport and error condition.
- `build.config`: Add `code_loading` option to specify whether code is loaded
  statically during eturnal startup or dynamically on demand. The latter may be
  desirable for (distribution) builds that use separately packaged Erlang
  dependencies, as it avoids hard-coding dependency versions at build time.
- Docker: Allow to define a different external STUN service for the IPv4 address
  lookup by adding the container-image-specific environment variable
  `STUN_SERVICE`, defaulting to: `STUN_SERVICE="stun.conversations.im 3478"`.

### Changed
- `build.config`: Rename the `eturnal_bin_prefix` option to `eturnal_prefix`.
- `build.config`: Remove the `eturnal_etc_prefix` option.
- Binary release: Reduce code size by omitting an unused transitive dependency
  (which had slipped back into the previous release).

### Fixed
- Fix dynamic loading of `mod_stats_prometheus` dependencies (for distribution
  builds).
- Docker: Keep list of installed packages, so that image scanners like Trivy can
  check the image for vulnerabilities.

## [1.10.0] - 2022-07-27
### Added
- Include `mod_stats_prometheus`, a module for exporting metrics to Prometheus.
- Include an example configuration for logrotate.
- Include an example OpenRC init (and configuration) file.

### Changed
- If an EPMD process was spawned during eturnal startup, stop it on shutdown,
  unless it's used by other Erlang nodes.

### Fixed
- Avoid permission issues in the case where `eturnalctl` was invoked by root
  from a directory the user running eturnal isn't permitted to change into.
- Make sure `eturnalctl daemon` won't hang on the very first startup when using
  Erlang/OTP 23 or newer.

## [1.9.1] - 2022-07-17
### Added
- Allow for adding the special keywords `default` or `recommended` to the
  `blacklist`. The former expands to the addresses blocked by default, the
  latter includes the former and additionally expands to a number of networks
  [recommended](https://rtcsec.com/article/cve-2020-26262-bypass-of-coturns-access-control-protection/#further-concerns-what-else)
  to be blocked.
- Fall back to reading the relay port range boundaries from environment
  variables when `relay_min_port` and/or `relay_max_port` aren't specified.
- Docker: Adjust image `ENTRYPOINT` to provide a way to autodetect (in most
  cases) the Docker host's IPv4 address during container startup within isolated
  network environments, without explicitly defining the IPv4 address (with an
  `ENV` variable or a configuration file).

### Changed
- If an EPMD process is spawned during eturnal startup, let it listen on
  `localhost` only (#9). (Note that our Linux packages and container images are
  [configured](https://eturnal.net/documentation/#ERL_DIST_PORT) to _not_ start
  an EPMD process.)
- Omit the code location from log messages, except when debug logging is
  enabled.
- Apply other minor logging improvements.
- Docker: Reduce image size. IMPORTANT: A custom `eturnal.yml` configuration
  file should be mounted to the default path `/etc/eturnal.yml` or to a custom
  path defined with `ETURNAL_ETC_PREFIX`, as mounting it to
  `/opt/eturnal/etc/eturnal.yml` will prevent the container to start up
  successfully.
- Binary release: Update Erlang/OTP from 25.0.2 to 25.0.3.
- Windows: Update to LibYAML 0.2.5.
- Windows: Update to OpenSSL 3.0.5.

## [1.9.0] - 2022-07-07
### Added
- Publish Docker images and provide configuration examples for Docker/Kubernetes
  (many thanks to Saarko) (#20).
- Fall back to reading the relay IP addresses from environment variables when
  `relay_ipv4_address` and/or `relay_ipv6_address` aren't specified (#24).

### Changed
- Binary release: Update Erlang/OTP from 24.3.4 to 25.0.2.
- Binary release: Update Rebar3 from 3.18.0 to 3.19.0.
- Binary release: Update OpenSSL from 1.1.1m to 1.1.1q.
- Binary release: Update minimum glibc version from 2.17 to 2.19.
- Binary release: Reduce code size by omitting an unused transitive dependency.

### Fixed
- Avoid crashes in the case where no `secret` is configured in the `eturnal.yml`
  file (#21).
- Don't log misleading complaints about `proxy_protocol` option.
- Gracefully handle errors while receiving UDP data (#23).
- Restart listeners on failure.
- Reduce log level for network issues that may occur during normal operation.
- Windows: Support custom installation path (#22).

## [1.8.3] - 2022-05-12
### Changed
- Specifying an `ip` address for `listen` entries is no longer mandatory. The
  default value is now `"::"`.
- Make sure eturnal's `log_dir` is used for the additional log files created by
  `eturnalctl daemon`.
- Keep TURN session IDs unique across eturnal restarts.
- Binary release: Update Erlang/OTP from 24.2.2 to 24.3.4.
- Binary release: Update OpenSSL from 1.1.1m to 1.1.1o.
- Binary release: Update zlib from 1.2.11 to 1.2.12.
- Binary release: Use new (GCC-11.2-based) version of build toolchain.
- Binary release: Provide self-extracting installer for non-DEB/RPM systems.

### Fixed
- Windows: Don't fail to start up after reboot.

## [1.8.2] - 2022-03-02
### Changed
- Use a (pseudo)random `secret` by default.
- Improve autodetection of relay IP addresses used by default if the
  `relay_ipv4_addr` and/or `relay_ipv6_addr` options aren't specified.
- Binary release: Update Erlang/OTP from 24.2 to 24.2.2.

### Fixed
- Don't crash without explicit `listen` configuration. This bug was introduced
  with version 1.7.0.
- Don't crash if the configuration file is empty (i.e., has no `eturnal`
  section).
- Don't crash if TURN is enabled without a public IPv6 relay address being
  available.

## [1.8.1] - 2022-01-10
### Fixed
- Don't fail to handle the `$user` argument of the `eturnalctl sessions` and
  `eturnalctl disconnect` calls.

## [1.8.0] - 2022-01-10
### Added
- Allow for configuring TLS connection properties using the new `tls_options`,
  `tls_ciphers`, and `tls_dh_file` options (#6).
- Allow for specifying a `whitelist` of IP addresses/subnets which will be
  accepted even if they would otherwise be rejected due to being matched by a
  `blacklist` (#12).
- Don't close active TURN sessions when ephemeral credentials expire, by
  default. The new `strict_expiry` option allows for enabling the previous
  behavior.
- Add `eturnalctl disconnect $user` command for closing any TURN session(s) of
  the specified `$user` name.
- Let the `eturnalctl sessions` command accept an optional `$user` argument to
  list only the TURN session(s) of the specified `$user` name.
- Support running eturnal [without](https://blog.erlware.org/epmdlessless/) the
  Erlang Port Mapper Daemon (EPMD) by specifying the environment variable
  `ERL_DIST_PORT` (requires at least Erlang/OTP 23.1 and Rebar3 3.18.0).

### Changed
- Binary release: Run eturnal without EPMD (as described above).

### Fixed
- Don't log bogus error messages if no eturnal modules are enabled when using
  Erlang/OTP version 21.0, 21.1, or 21.2.
- Binary release: Don't let Erlang/OTP link against libnsl.so.1, which is no
  longer shipped by default on RedHat-based distributions, and isn't actually
  needed (#19).

## [1.7.0] - 2021-12-15
### Added
- Introduce the `listen` option `proxy_protocol` for enabling HAproxy protocol
  (version 1 and 2) support (#18).

### Changed
- Binary release: Update Erlang/OTP from 24.1.7 to 24.2.
- Binary release: Update OpenSSL from 1.1.1l to 1.1.1m.
- Binary release: Link `asn1` and `crypto` NIFs statically into BEAM.
- Binary release: Reduce size by a few MiB by omitting a test suite file.
- Binary release: Don't forget to strip ERTS binaries.

### Fixed
- Don't crash when multiple `secret`s are configured on Erlang/OTP 23 or later.

## [1.6.0] - 2021-12-04
### Added
- Add `eturnalctl credentials` and `eturnalctl password` commands for generating
  ephemeral TURN credentials.
- Support the `listen` option `transport: auto` for accepting unencrypted TCP
  and TLS connections on the same port (thanks to Annika Hannig). Requires
  Erlang/OTP 23 or later.

### Changed
- Binary release: Update Erlang/OTP from 24.1.4 to 24.1.7.

## [1.5.0] - 2021-11-02
### Added
- Allow for specifying a list of shared secrets in order to facilitate key
  rollover (#16).
- Improve UDP receive performance.
- Reduce risk of UDP packet loss.

### Changed
- Binary release: Update Erlang/OTP from 24.1.2 to 24.1.4.

### Fixed
- Handle the case where a `tls_crt_file` but no `tls_key_file` is specified (by
  assuming the `tls_crt_file` includes both the certificate and the key).
- Don't forget to check for new PEM files on reload if the configuration wasn't
  modified (#17).

## [1.4.6] - 2021-10-11
### Changed
- Don't abort (but log an appropriate warning) if TURN is enabled without a
  shared secret.
- Drop the runtime dependency on the `openssl` command for generating
  self-signed certificates.
- Binary release: Update Erlang/OTP from 23.2 to 24.1.2.
- Binary release: Update OpenSSL from 1.1.1i to 1.1.1l.

### Removed
- Drop the `mod_example` module.

## [1.4.5] - 2021-01-28
### Changed
- Don't include timestamp when logging to the systemd journal.

### Fixed
- Let `eturnalctl sessions` cope with non-latin characters in user names.
- Binary release: Let `eturnalctl remote_console` actually connect to the
  running eturnal instance.

## [1.4.4] - 2021-01-21
### Changed
- Reject Teredo and 6to4 peers unconditionally.
- Reject 0.0.0.0/8 and ::/128 peers unconditionally.

### Fixed
- Never request certificates from TLS clients.

## [1.4.3] - 2020-12-16
### Changed
- Binary release: Update Erlang/OTP from 22.2 to 23.2.
- Binary release: Update OpenSSL from 1.1.1g to 1.1.1i.

### Fixed
- Don't log stack traces if clients attempt authentication while TURN is
  disabled.

## [1.4.2] - 2020-11-04
### Changed
- Make sure the `eturnal.yml` file isn't installed world-readable, as it might
  contain the shared TURN secret (#10).

## [1.4.1] - 2020-09-09
### Fixed
- Fix systemd watchdog interval recalculation during configuration reloads.

## [1.4.0] - 2020-09-06
### Added
- Add `mod_log_stun` for logging STUN requests. Without this module, they will
  now only show up in the debug log output.
- Add list of TURN permissions to the `eturnalctl sessions` output.

### Changed
- Always log reason for TCP/TLS connection termination (at info level).
- Omit Erlang process ID from log messages (now that a session ID is logged).

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
  to InfluxDB (contributed by Marc Schink).

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
  rather than starting its own EPMD instance.
- Don't bind EPMD to 127.0.0.1 by default.

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

[Unreleased]: https://github.com/processone/eturnal/compare/1.10.0...HEAD
[1.10.0]: https://github.com/processone/eturnal/releases/tag/1.10.0
[1.9.1]: https://github.com/processone/eturnal/releases/tag/1.9.1
[1.9.0]: https://github.com/processone/eturnal/releases/tag/1.9.0
[1.8.3]: https://github.com/processone/eturnal/releases/tag/1.8.3
[1.8.2]: https://github.com/processone/eturnal/releases/tag/1.8.2
[1.8.1]: https://github.com/processone/eturnal/releases/tag/1.8.1
[1.8.0]: https://github.com/processone/eturnal/releases/tag/1.8.0
[1.7.0]: https://github.com/processone/eturnal/releases/tag/1.7.0
[1.6.0]: https://github.com/processone/eturnal/releases/tag/1.6.0
[1.5.0]: https://github.com/processone/eturnal/releases/tag/1.5.0
[1.4.6]: https://github.com/processone/eturnal/releases/tag/1.4.6
[1.4.5]: https://github.com/processone/eturnal/releases/tag/1.4.5
[1.4.4]: https://github.com/processone/eturnal/releases/tag/1.4.4
[1.4.3]: https://github.com/processone/eturnal/releases/tag/1.4.3
[1.4.2]: https://github.com/processone/eturnal/releases/tag/1.4.2
[1.4.1]: https://github.com/processone/eturnal/releases/tag/1.4.1
[1.4.0]: https://github.com/processone/eturnal/releases/tag/1.4.0
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
