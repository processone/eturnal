# eturnal STUN/TURN Server

[![CI](https://github.com/processone/eturnal/actions/workflows/ci.yml/badge.svg)][1]

[eturnal][2] is a modern, straightforward STUN and TURN server. For
authentication, the mechanism described in the [REST API for Access to TURN
Services][3] specification is implemented.

## Installation

> _Note:_ Running eturnal in **container environments** such as Docker or
> Kubernetes is described in a [separate documentation][4] page.

On **APT-based** Linux distributions, run:

    sudo apt install extrepo
    sudo extrepo enable eturnal
    sudo apt update
    sudo apt install eturnal

On **DNF-based** Linux distributions, run:

    sudo dnf config-manager --add-repo https://eturnal.net/eturnal.repo
    sudo dnf install eturnal
    sudo systemctl --now enable eturnal

On **YUM-based** Linux distributions, run:

    sudo yum-config-manager --add-repo https://eturnal.net/eturnal.repo
    sudo yum install eturnal
    sudo systemctl --now enable eturnal

On SUSE Linux Enterprise and openSUSE systems, [distribution repositories][5]
can be used instead. On other Linux systems, the binary release can be installed
as [described][6] in the reference documentation. For Windows, an installer is
[available][7]. On other platforms, eturnal is [built from source][8].

## Configuration

The eturnal server is configured by editing the `/etc/eturnal.yml` file. This
file uses the (indentation-sensitive!) [YAML][9] format. For TURN relaying to
work, you'll have to specify the [shared authentication][3] `secret` and (at
least if the server is running behind NAT) also the `relay_ipv4_addr` option,
which should be set to the server's _external_ IPv4 address. As an example, a
minimal configuration for offering STUN and TURN services on port 3478 (UDP and
TCP) might look like this:

```yaml
eturnal:
  secret: "long-and-cryptic"     # Shared secret, CHANGE THIS.
  relay_ipv4_addr: "203.0.113.4" # The server's public IPv4 address.
  relay_ipv6_addr: "2001:db8::4" # The server's public IPv6 address (optional).
```

A more detailed, commented [example configuration][10] is shipped with the
eturnal server.

## Running eturnal

On Linux systems, the eturnal server is usually invoked by [systemd][11]. For
non-systemd platforms, an [example init script][12] is shipped in the
`etc/init.d` directory.

For controlling eturnal, the `eturnalctl` command can be used; see:

    eturnalctl help

## Logging

If eturnal was started by systemd, log files are written into the
`/var/log/eturnal` directory by default. In order to log to the [journal][13]
instead, the `log_dir` option can be set to `stdout` in the configuration file.

## Documentation

For a detailed description of eturnal's configuration options and the
`eturnalctl` tool, see the [reference documentation][14]. For notable changes
between eturnal releases, see the [change log][15].

## Feedback/Support

Please use [our issue tracker][16] for bug reports and feature requests. Feel
free to (ab)use it for usage questions as well. If you happen to be using
[XMPP][17], you could also join our public channel
`eturnal@conference.process-one.net`.

 [1]: https://github.com/processone/eturnal/actions/workflows/ci.yml
 [2]: https://eturnal.net/
 [3]: https://tools.ietf.org/html/draft-uberti-behave-turn-rest-00
 [4]: https://github.com/processone/eturnal/tree/master/docker-k8s
 [5]: https://software.opensuse.org/download/?package=eturnal&project=devel:languages:erlang
 [6]: https://eturnal.net/documentation/#Installation
 [7]: https://eturnal.net/windows/
 [8]: https://github.com/processone/eturnal/blob/1.8.3/INSTALL.md
 [9]: https://en.wikipedia.org/wiki/YAML
[10]: https://github.com/processone/eturnal/blob/1.8.3/config/eturnal.yml
[11]: https://www.freedesktop.org/software/systemd/man/systemctl.html
[12]: https://github.com/processone/eturnal/blob/1.8.3/scripts/eturnal.init
[13]: https://www.freedesktop.org/software/systemd/man/systemd-journald.service.html
[14]: https://eturnal.net/documentation/
[15]: https://github.com/processone/eturnal/blob/1.8.3/CHANGELOG.md
[16]: https://github.com/processone/eturnal/issues
[17]: https://xmpp.org
