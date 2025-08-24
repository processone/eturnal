# Building eturnal From Source

## Requirements

- [Erlang/OTP][1] (26.0 or newer).
- [LibYAML][2] (0.1.4 or newer).
- [OpenSSL][3] (1.0.0 or newer).
- [GCC][4] (other C compilers might work as well).

Note that you need the development headers of the libraries as well. Linux
distributions often put those into separate `*-dev` or `*-devel` packages. For
example, on DEB-based distributions you'd typically install `libyaml-dev` and
`libssl-dev`, on RPM-based distributions you'll probably need `libyaml-devel`
and `openssl-devel`.

## Compilation

> _Note:_ If you build directly from the Git repository rather than using the
> official source tarball, you must [download Rebar3][5] and make it executable
> (`chmod +x rebar3`), first. [Depending on][6] the Erlang/OTP release in use,
> you might need an [older Rebar3][7] version.

```shell
curl https://eturnal.net/eturnal-1.12.2.tar.gz | tar -C /tmp -xzf -
cd /tmp/eturnal-1.12.2
./rebar3 as prod tar
```

This generates the archive file `_build/prod/rel/eturnal/eturnal-1.12.2.tar.gz`.
The default installation prefix is set to `/opt/eturnal`, and it's assumed the
server will be executed by a user named `eturnal`. To change these defaults,
edit the [build.config][8] file or override the settings using environment
variables (of the same name, but upper-case), re-run `./rebar3 as prod tar`, and
adapt the following installation instructions accordingly.

## Quick Test

The following command starts the server in an Erlang shell, [using][9] the
configuration in [config/eturnal.yml][10]:

```shell
./rebar3 shell
```

To stop the server, enter `q().` (including the trailing dot).

## Installation

You'll need root privileges for the following commands. Therefore, call `su -`
or `sudo -i`, first.

1.  Create a user for running eturnal. This step is of course only required if
    you're installing eturnal for the first time:

    ```shell
    useradd -r -m -d /opt/eturnal eturnal
    ```

    Otherwise, **create a backup** of the old installation, first:

    ```shell
    tar -czf /opt/eturnal-$(date '+%F').tar.gz /opt/eturnal
    ```

2.  Extract the archive generated [above](#compilation):

    ```shell
    cd /opt/eturnal
    tar -xzf /tmp/eturnal-1.12.2/_build/prod/rel/eturnal/eturnal-1.12.2.tar.gz
    chown eturnal /opt/eturnal/etc/eturnal.yml
    ```

3.  Copy the `eturnal.yml` file to `/etc` (optional):

    ```shell
    cp -p -i /opt/eturnal/etc/eturnal.yml /etc/
    ```

4.  Start the systemd service:

    ```shell
    cp /opt/eturnal/etc/systemd/system/eturnal.service /etc/systemd/system/
    systemctl daemon-reload
    systemctl --now enable eturnal
    ```

## Configuration and Usage

See the [README.md][11] file and the [reference documentation][12] for
configuration and usage instructions.

 [1]: https://www.erlang.org
 [2]: https://pyyaml.org/wiki/LibYAML
 [3]: https://www.openssl.org
 [4]: https://gcc.gnu.org
 [5]: https://s3.amazonaws.com/rebar3/rebar3
 [6]: https://github.com/erlang/rebar3#compatibility-between-rebar3-and-erlangotp
 [7]: https://github.com/erlang/rebar3/releases
 [8]: https://github.com/processone/eturnal/blob/1.12.2/build.config
 [9]: https://github.com/processone/eturnal/blob/1.12.2/config/shell.config
[10]: https://github.com/processone/eturnal/blob/1.12.2/config/eturnal.yml
[11]: https://github.com/processone/eturnal/blob/1.12.2/README.md
[12]: https://eturnal.net/doc/
