# eturnal STUN/TURN Server

[![Build Status](https://travis-ci.org/processone/eturnal.svg?branch=master)][1]

[eturnal][2] is a modern, straightforward STUN/TURN server with full IPv6
support. For TURN authentication, the mechanism described in the [REST API for
Access to TURN Services specification][3] is implemented.

On Linux/x64 systems, you can [install the binary
release](#installation-on-linuxx64-systems). On other platforms, eturnal is
[built from source](#building-from-source).

## Installation on Linux/x64 Systems

On **DEB-based** Linux/x64 distributions, run:

    $ curl -O https://eturnal.net/download/package/eturnal_1.3.0-1_amd64.deb
    $ sudo dpkg -i eturnal_1.3.0-1_amd64.deb

On **RPM-based** Linux/x64 distributions, run:

    $ curl -O https://eturnal.net/download/package/eturnal-1.3.0-1.x86_64.rpm
    $ sudo rpm -i eturnal-1.3.0-1.x86_64.rpm
    $ sudo systemctl daemon-reload
    $ sudo systemctl --now enable eturnal

On other Linux/x64 systems, the binary release tarball can be installed as
[described][4] in the reference documentation.

## Building From Source

### Requirements

- [Erlang/OTP][5] (21.0 or newer).
- [LibYAML][6] (0.1.4 or newer).
- [OpenSSL][7] (1.3.0 or newer).
- [GCC][8] (other C compilers might work as well).

Note that you need the development headers of the libraries as well. Linux
distributions often put those into separate `*-dev` or `*-devel` packages. For
example, on DEB-based distributions you'd typically install `libyaml-dev` and
`libssl-dev`, on RPM-based distributions you'll probably need `libyaml-devel`
and `openssl-devel`.

### Compilation

> _Note:_ If you build directly from the Git repository rather than using the
> official source tarball, you must [download rebar3][9] and make it executable
> (`chmod +x rebar3`), first.

    $ curl https://eturnal.net/download/eturnal-1.3.0.tar.gz | tar -C /tmp -xzf -
    $ cd /tmp/eturnal-1.3.0
    $ ./rebar3 as prod tar

This generates the archive file `_build/prod/rel/eturnal/eturnal-1.3.0.tar.gz`.
The default installation prefix is set to `/opt/eturnal`, and it's assumed the
server will be executed by a user named `eturnal`. To change these defaults,
edit the [build.config][10] file, re-run `./rebar3 as prod tar`, and adapt the
following installation instructions accordingly.

### Installation

You'll need root privileges for the following commands. Therefore, call `su -`
or `sudo -i`, first.

1.  Create a user for running eturnal. This step is of course only required if
    you're installing eturnal for the first time:

        # useradd -r -m -d /opt/eturnal eturnal

    Otherwise, **create a backup** of the old installation, first:

        # tar -czf /opt/eturnal-$(date '+%F').tar.gz /opt/eturnal

2.  Extract the archive generated [above](#compilation):

        # cd /opt/eturnal
        # tar -xzf /tmp/eturnal-1.3.0/_build/prod/rel/eturnal/eturnal-1.3.0.tar.gz

3.  Copy the `eturnal.yml` file to `/etc` (optional):

        # cp -i /opt/eturnal/etc/eturnal.yml /etc/

4.  Start the systemd service:

        # cp /opt/eturnal/etc/systemd/system/eturnal.service /etc/systemd/system/
        # systemctl daemon-reload
        # systemctl --now enable eturnal

## Configuration

The eturnal server is configured by editing the `/etc/eturnal.yml` file. This
file uses the (indentation-sensitive!) [YAML][11] format. A commented [example
configuration][12] is shipped with the eturnal server. However, for TURN
relaying to work, you'll have to specify the [shared authentication][3] `secret`
and probably also the `relay_ipv4_addr` option (which should be set to the
server's external IPv4 address). Then, either remove the `enable_turn: false`
lines within the `listen` section or remove the `listen` section altogether. As
an example, a minimal configuration for offering STUN and TURN services on port
3478 (UDP and TCP) might look like this:

```yaml
eturnal:
  secret: "long-and-cryptic"     # Shared secret, CHANGE THIS.
  relay_ipv4_addr: "203.0.113.4" # The server's public IPv4 address.
  relay_ipv6_addr: "2001:db8::4" # The server's public IPv6 address (optional).
```

## Running eturnal

On Linux systems, the eturnal server is usually invoked by [systemd][13]. For
non-systemd platforms, an [example init script][14] is shipped in the
`etc/init.d` directory.

For controlling eturnal, the `eturnalctl` command can be used; see:

    $ eturnalctl help

## Logging

If eturnal was started by systemd, log files are written into the
`/var/log/eturnal` directory by default. In order to log to the [journal][15]
instead, the `log_dir` option can be set to `stdout` in the configuration file.

## Documentation

For a detailed description of eturnal's configuration options and the
`eturnalctl` tool, see the [reference documentation][16]. For notable changes
between eturnal releases, see the [change log][17].

## Feedback/Support

Please use [our issue tracker][18] for bug reports and feature requests. Feel
free to (ab)use it for usage questions as well. If you happen to be using
[XMPP][19], you could also join our public channel
`eturnal@conference.process-one.net`.

 [1]: https://travis-ci.org/processone/eturnal
 [2]: https://eturnal.net/
 [3]: https://tools.ietf.org/html/draft-uberti-behave-turn-rest-00
 [4]: https://eturnal.net/documentation/#Installation
 [5]: https://www.erlang.org
 [6]: https://pyyaml.org/wiki/LibYAML
 [7]: https://www.openssl.org
 [8]: https://gcc.gnu.org
 [9]: https://s3.amazonaws.com/rebar3/rebar3
[10]: https://github.com/processone/eturnal/blob/1.3.0/build.config
[11]: https://en.wikipedia.org/wiki/YAML
[12]: https://github.com/processone/eturnal/blob/1.3.0/config/eturnal.yml
[13]: https://www.freedesktop.org/software/systemd/man/systemctl.html
[14]: https://github.com/processone/eturnal/blob/1.3.0/scripts/eturnal.init
[15]: https://www.freedesktop.org/software/systemd/man/systemd-journald.service.html
[16]: https://eturnal.net/documentation/
[17]: https://github.com/processone/eturnal/blob/1.3.0/CHANGELOG.md
[18]: https://github.com/processone/eturnal/issues
[19]: https://xmpp.org
