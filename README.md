# eturnal STUN/TURN Server

eturnal is a modern, straightforward STUN/TURN server with full IPv6 support.
For TURN authentication, the mechanism described in the [REST API for Access to
TURN Services specification][1] is implemented.

On Linux/x64 systems, you can [install the binary
release](#persistent-installation). On other platforms, you must [build eturnal
from source](#building-from-source).

## Installing the Linux/x64 Binaries

### Quick Test

The following two commands give you a STUN/TURN server listening on port 3478
(UDP/TCP) and port 5349 (TLS) using the specified shared secret for [TURN
authentication][1] (no root privileges required):

    $ curl -L https://eturnal.net/download/eturnal-0.2.0-linux-x64.tar.gz | tar -C /tmp -xzf -
    $ ETURNAL_SECRET='crypt1c' /tmp/eturnal/bin/eturnal foreground

To stop the server, press `<Ctrl>+C`. To remove it, run `rm -rf /tmp/eturnal`.

### Persistent Installation

You'll need root privileges for the following commands. Therefore, call `su -`
or `sudo -i`, first.

1.  Create user. This step is of course only required if you're installing
    eturnal for the first time:

        # useradd -r -m -d /opt/eturnal eturnal

    Otherwise, **create a backup** of the old installation, first:

        # mv /opt/eturnal /opt/eturnal-$(date '+%F')

2.  Download and extract binary release:

        # curl -L https://eturnal.net/download/eturnal-0.2.0-linux-x64.tar.gz | tar -C /opt -xzf -

3.  Configure the shared secret, your server's IP address(es), and optionally
    other settings:

        # vi /opt/eturnal/etc/eturnal.yml

4.  Start the systemd service:

        # cp /opt/eturnal/etc/systemd/system/eturnal.service /etc/systemd/system/
        # systemctl daemon-reload
        # systemctl enable eturnal
        # systemctl start eturnal

If you'd like to use a different user and/or installation prefix, you must edit
the following two files accordingly:

- `$PREFIX/bin/eturnalctl`
- `/etc/systemd/system/eturnal.service`

## Building From Source

### Requirements

- [Erlang/OTP][2] (21.0 or newer).
- [LibYAML][3] (0.1.4 or newer).
- [OpenSSL][4] (1.0.0 or newer).
- [GCC][5] (other C compilers might work as well).

Note that you need the development headers of the libraries as well. Linux
distributions often put those into separate `*-dev` or `*-devel` packages. For
example, on DEB-based distributions you'd typically install `libyaml-dev` and
`libssl-dev`, on RPM-based distributions you'll probably need `libyaml-devel`
and `openssl-devel`.

### Compilation

> _Note:_ If you build directly from the Git repository rather than using the
> official source tarball, you must [download rebar3][6] and make it executable
> (`chmod +x rebar3`), first.

    $ mkdir -p "$HOME/src"
    $ cd "$HOME/src"
    $ curl https://eturnal.net/download/eturnal-0.2.0.tar.gz | tar -xzf -
    $ cd eturnal-0.2.0
    $ ./rebar3 as prod tar

This generates the archive file `_build/prod/rel/eturnal/eturnal-0.2.0.tar.gz`.
The default installation prefix is set to `/opt/eturnal`, and it's assumed the
server will be executed by a user named `eturnal`. To change these defaults,
edit the `build.config` file and re-run `./rebar3 as prod tar`.

### Quick Test

The following command gives you a STUN/TURN server listening on port 3478
(UDP/TCP) and port 5349 (TLS) using the specified shared secret for [TURN
authentication][1]:

    $ ETURNAL_SECRET='crypt1c' ./rebar3 run

To stop the server, press `<Ctrl>+C`.

### Persistent Installation

The generated archive file holds the _contents_ of the installation prefix.
Therefore, you'd follow the [binary installation
instructions](#persistent-installation) above, but adapt step 2 by extracting
the archive _into_ the `/opt/eturnal` directory:

    # cd /opt/eturnal
    # tar -xzf "$HOME/src/eturnal/_build/prod/rel/eturnal/eturnal-0.2.0.tar.gz"

## Configuring eturnal

The eturnal server is configured by editing the `eturnal.yml` file. This file is
written using the (indentation-sensitive!) YAML format. A commented example
configuration with sane default settings is shipped with the eturnal server.
However, for TURN relaying to work, you'll have to specify the [shared
authentication][1] `secret`, and probably also the `relay_ipv4_addr` option,
which should be set to the server's external IPv4 address.

## Running eturnal

On Linux systems, the eturnal server is usually controlled by systemd:

    # systemctl start eturnal
    # systemctl restart eturnal
    # systemctl reload eturnal
    # systemctl stop eturnal

On non-systemd platforms, the `eturnalctl` command can be used, see:

    # eturnalctl help

## Logging

If eturnal was started by systemd, log files are written into the
`/var/log/eturnal` directory by default. To use an external log rotation
utility, remove the `log_rotate_*` options from your `eturnal.yml` configuration
file and run `systemctl reload eturnal`. eturnal will detect external rotation
automatically, so there's no need to send a `HUP` signal after log rotation.

## Feedback/Support

Please use [our issue tracker][7] for bug reports and feature requests. Feel
free to (ab)use it for usage questions as well.

[1]: https://tools.ietf.org/html/draft-uberti-behave-turn-rest-00
[2]: https://www.erlang.org
[3]: https://pyyaml.org/wiki/LibYAML
[4]: https://www.openssl.org
[5]: https://gcc.gnu.org
[6]: https://github.com/erlang/rebar3/releases/download/3.14.0-rc2/rebar3
[7]: https://github.com/processone/eturnal/issues
