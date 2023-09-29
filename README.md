# eturnal TURN Server

<p align="left"><img src="https://eturnal.net/hello.png" height="180"></p>

[![CI](https://github.com/processone/eturnal/actions/workflows/ci.yml/badge.svg)][1]

[eturnal][2] is a modern, straightforward STUN and TURN server. For
authentication, the mechanism described in the [REST API for Access to TURN
Services][3] specification is implemented. The server can easily be tested [in a
Linux shell][4] or [using Docker][5]. For a persistent installation, see the
following section.

## Installation

> _Note:_ Running eturnal in **container environments** such as Docker or
> Kubernetes is described on a [separate documentation][6] page.

On **APT-based** Linux distributions, run:

```shell
sudo apt install extrepo
sudo extrepo enable eturnal
sudo apt update
sudo apt install eturnal
```

On **DNF-based** Linux distributions, run:

```shell
sudo dnf config-manager --add-repo https://eturnal.net/eturnal.repo
sudo dnf install eturnal
sudo systemctl --now enable eturnal
```

On **YUM-based** Linux distributions, run:

```shell
sudo yum-config-manager --add-repo https://eturnal.net/eturnal.repo
sudo yum install eturnal
sudo systemctl --now enable eturnal
```

On **SUSE** Linux Enterprise and openSUSE systems, [distribution
repositories][7] can be used instead. There's also an official **Alpine**
[package][8]. On **other Linux systems**, the binary release can be installed as
[described][9] in the reference documentation. For **Windows**, an installer is
[available][10].

On **macOS**, homebrew can be used like this:

```shell
brew tap processone/eturnal https://github.com/processone/eturnal
brew install processone/eturnal/eturnal
```

On **other platforms**, eturnal is [built from source][11].

## Configuration

The eturnal server is configured by editing the `/etc/eturnal.yml` file. This
file uses the (indentation-sensitive!) [YAML][12] format. For TURN relaying to
work, you'll have to specify the [shared authentication][3] `secret` and (if
autodetection fails) also the `relay_ipv4_addr` option, which should be set to
the server's _external_ IPv4 address. As an example, a configuration for
offering STUN and TURN services on port 3478 (UDP and TCP) might look like
this:

```yaml
eturnal:
  secret: "long-and-cryptic"     # Shared secret, CHANGE THIS.
  relay_ipv4_addr: "203.0.113.4" # The server's public IPv4 address.
  relay_ipv6_addr: "2001:db8::4" # The server's public IPv6 address (optional).
```

A more detailed, commented [example configuration][13] is shipped with the
eturnal server.

## Running eturnal

On Linux systems, the eturnal server is usually invoked by [systemd][14]. For
non-systemd platforms, example [init][15] and [OpenRC][16] scripts are shipped
below the `etc` directory.

For controlling eturnal, the `eturnalctl` command can be used; see:

```shell
eturnalctl help
```

## Logging

If eturnal was started by systemd, log files are written into the
`/var/log/eturnal` directory by default. In order to log to the [journal][17]
instead, the `log_dir` option can be set to `stdout` in the configuration file.

## Documentation

For a detailed description of eturnal's configuration options and the
`eturnalctl` tool, see the [reference documentation][18]. For notable changes
between eturnal releases, see the [change log][19].

## Feedback/Support

Please use [our issue tracker][20] for bug reports and feature requests. Feel
free to (ab)use it for usage questions as well. If you happen to be using
[XMPP][21], you could also join our public room
`eturnal@conference.process-one.net`. Otherwise, just use the [web client][22].
As an alternative, there's an `#eturnal` channel [on Slack][23].

 [1]: https://github.com/processone/eturnal/actions/workflows/ci.yml
 [2]: https://eturnal.net/
 [3]: https://tools.ietf.org/html/draft-uberti-behave-turn-rest-00
 [4]: https://eturnal.net/doc/quick-test.html
 [5]: https://eturnal.net/doc/container-quick-test.html
 [6]: https://eturnal.net/doc/container.html
 [7]: https://software.opensuse.org/download/?package=eturnal&project=devel:languages:erlang
 [8]: https://pkgs.alpinelinux.org/packages?name=eturnal
 [9]: https://eturnal.net/doc/#Installation
[10]: https://eturnal.net/windows/
[11]: https://eturnal.net/doc/install.html
[12]: https://en.wikipedia.org/wiki/YAML
[13]: https://github.com/processone/eturnal/blob/1.12.0/config/eturnal.yml
[14]: https://www.freedesktop.org/software/systemd/man/systemctl.html
[15]: https://github.com/processone/eturnal/blob/1.12.0/overlay/init/sysv/eturnal
[16]: https://github.com/processone/eturnal/blob/1.12.0/overlay/init/openrc/eturnal.initd
[17]: https://www.freedesktop.org/software/systemd/man/systemd-journald.service.html
[18]: https://eturnal.net/doc/
[19]: https://github.com/processone/eturnal/blob/1.12.0/CHANGELOG.md
[20]: https://github.com/processone/eturnal/issues
[21]: https://xmpp.org
[22]: https://eturnal.net/chat/
[23]: https://erlef.org/slack-invite/erlanger
