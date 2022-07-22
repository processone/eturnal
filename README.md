# eturnal STUN/TURN Server

[![CI](https://github.com/processone/eturnal/actions/workflows/ci.yml/badge.svg)][1]

[eturnal][2] is a modern, straightforward STUN and TURN server. For
authentication, the mechanism described in the [REST API for Access to TURN
Services][3] specification is implemented. The server can [easily][4] be tested.
For a persistent installation, see the following section.

## Installation

> _Note:_ Running eturnal in **container environments** such as Docker or
> Kubernetes is described on a [separate documentation][5] page.

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

On SUSE Linux Enterprise and openSUSE systems, [distribution repositories][6]
can be used instead. On other Linux systems, the binary release can be installed
as [described][7] in the reference documentation. For Windows, an installer is
[available][8]. On other platforms, eturnal is [built from source][9].

## Configuration

The eturnal server is configured by editing the `/etc/eturnal.yml` file. This
file uses the (indentation-sensitive!) [YAML][10] format. For TURN relaying to
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

A more detailed, commented [example configuration][11] is shipped with the
eturnal server.

## Running eturnal

On Linux systems, the eturnal server is usually invoked by [systemd][12]. For
non-systemd platforms, example [init][13] and [OpenRC][14] scripts are shipped
below the `etc` directory.

For controlling eturnal, the `eturnalctl` command can be used; see:

    eturnalctl help

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

 [1]: https://github.com/processone/eturnal/actions/workflows/ci.yml
 [2]: https://eturnal.net/
 [3]: https://tools.ietf.org/html/draft-uberti-behave-turn-rest-00
 [4]: https://github.com/processone/eturnal/blob/master/QUICK-TEST.md
 [5]: https://eturnal.net/documentation/code/docker.html
 [6]: https://software.opensuse.org/download/?package=eturnal&project=devel:languages:erlang
 [7]: https://eturnal.net/documentation/#Installation
 [8]: https://eturnal.net/windows/
 [9]: https://github.com/processone/eturnal/blob/1.9.1/INSTALL.md
[10]: https://en.wikipedia.org/wiki/YAML
[11]: https://github.com/processone/eturnal/blob/1.9.1/config/eturnal.yml
[12]: https://www.freedesktop.org/software/systemd/man/systemctl.html
[13]: https://github.com/processone/eturnal/blob/1.9.1/scripts/eturnal.init
[14]: https://github.com/processone/eturnal/blob/1.9.1/scripts/eturnal.openrc
[15]: https://www.freedesktop.org/software/systemd/man/systemd-journald.service.html
[16]: https://eturnal.net/documentation/
[17]: https://github.com/processone/eturnal/blob/1.9.1/CHANGELOG.md
[18]: https://github.com/processone/eturnal/issues
[19]: https://xmpp.org
