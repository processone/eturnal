# eturnal STUN/TURN Server

[![Build Status](https://travis-ci.com/weiss/eturnal.svg?branch=master)][1]

[eturnal][2] is a modern, straightforward STUN and TURN server with full IPv6
support. For TURN authentication, the mechanism described in the [REST API for
Access to TURN Services specification][3] is implemented.

On Linux/x64 systems, you can [install the binary
release](#installation-on-linuxx64-systems). On SUSE Linux Enterprise and
openSUSE systems, [distribution repositories][4] can be used instead. On other
platforms, eturnal is [built from source][5].

## Installation on Linux/x64 Systems

On **DEB-based** Linux/x64 distributions, run:

    $ curl -O https://eturnal.net/download/package/eturnal_1.4.5-1_amd64.deb
    $ sudo dpkg -i eturnal_1.4.5-1_amd64.deb

On **RPM-based** Linux/x64 distributions, run:

    $ curl -O https://eturnal.net/download/package/eturnal-1.4.5-1.x86_64.rpm
    $ sudo rpm -i eturnal-1.4.5-1.x86_64.rpm
    $ sudo systemctl daemon-reload
    $ sudo systemctl --now enable eturnal

On other Linux/x64 systems, the binary release tarball can be installed as
[described][6] in the reference documentation.

## Configuration

The eturnal server is configured by editing the `/etc/eturnal.yml` file. This
file uses the (indentation-sensitive!) [YAML][7] format. A commented [example
configuration][8] is shipped with the eturnal server. However, for TURN relaying
to work, you'll have to specify the [shared authentication][3] `secret` and
probably also the `relay_ipv4_addr` option (which should be set to the server's
external IPv4 address). Then, either remove the `enable_turn: false` lines
within the `listen` section or remove the `listen` section altogether. As an
example, a minimal configuration for offering STUN and TURN services on port
3478 (UDP and TCP) might look like this:

```yaml
eturnal:
  secret: "long-and-cryptic"     # Shared secret, CHANGE THIS.
  relay_ipv4_addr: "203.0.113.4" # The server's public IPv4 address.
  relay_ipv6_addr: "2001:db8::4" # The server's public IPv6 address (optional).
```

## Running eturnal

On Linux systems, the eturnal server is usually invoked by [systemd][9]. For
non-systemd platforms, an [example init script][10] is shipped in the
`etc/init.d` directory.

For controlling eturnal, the `eturnalctl` command can be used; see:

    $ eturnalctl help

## Logging

If eturnal was started by systemd, log files are written into the
`/var/log/eturnal` directory by default. In order to log to the [journal][11]
instead, the `log_dir` option can be set to `stdout` in the configuration file.

## Documentation

For a detailed description of eturnal's configuration options and the
`eturnalctl` tool, see the [reference documentation][12]. For notable changes
between eturnal releases, see the [change log][13].

## Feedback/Support

Please use [our issue tracker][14] for bug reports and feature requests. Feel
free to (ab)use it for usage questions as well. If you happen to be using
[XMPP][15], you could also join our public channel
`eturnal@conference.process-one.net`.

 [1]: https://app.travis-ci.com/github/weiss/eturnal
 [2]: https://eturnal.net/
 [3]: https://tools.ietf.org/html/draft-uberti-behave-turn-rest-00
 [4]: https://software.opensuse.org/download/?package=eturnal&project=devel:languages:erlang
 [5]: https://github.com/processone/eturnal/blob/1.4.5/INSTALL.md
 [6]: https://eturnal.net/documentation/#Installation
 [7]: https://en.wikipedia.org/wiki/YAML
 [8]: https://github.com/processone/eturnal/blob/1.4.5/config/eturnal.yml
 [9]: https://www.freedesktop.org/software/systemd/man/systemctl.html
[10]: https://github.com/processone/eturnal/blob/1.4.5/scripts/eturnal.init
[11]: https://www.freedesktop.org/software/systemd/man/systemd-journald.service.html
[12]: https://eturnal.net/documentation/
[13]: https://github.com/processone/eturnal/blob/1.4.5/CHANGELOG.md
[14]: https://github.com/processone/eturnal/issues
[15]: https://xmpp.org
