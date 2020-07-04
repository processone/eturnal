Packaging eturnal
=================

> The following notes might be interesting to distribution packagers.

The eturnal server is built as a so-called [Erlang/OTP release][1] (using the
[rebar3][2] and [relx][3] tools). An Erlang/OTP release is a stand-alone
directory that contains eturnal and (usually) all Erlang dependencies including
the required parts of the Erlang VM itself. Erlang/OTP releases are freely
relocatable within the file system and offer deployment features such as
in-place upgrades of the software. However, the release concept was not designed
with distribution packaging in mind, esp. regarding dependency management and
the release directory structure.

Dependency Management
---------------------

Besides the dependencies mentioned in the [README.md][4] file, eturnal depends
on the Erlang libraries referenced as `deps` in the [rebar.config][5] file,
_except_ for `recon` (which is totally optional and just included with source
builds as a convenience for debugging purposes). The complete list of direct and
transitive Erlang dependencies is:

- [stun](https://github.com/processone/stun)
- [conf](https://github.com/processone/conf)
- [yval](https://github.com/processone/yval)
- [fast\_yaml](https://github.com/processone/fast_yaml)
- [fast\_tls](https://github.com/processone/fast_tls)
- [p1\_utils](https://github.com/processone/p1_utils)

Assuming those dependencies are installed into the distribution's Erlang/OTP
tree, [rebar3][2]'s built-in dependency management can be skipped by building
eturnal (from the official source tarball) as follows:

    $ SKIP_DEPS=true ./rebar3 as distro release

This will yield a `_build/distro/rel/eturnal` directory containing the eturnal
files to be installed.

Directory Structure
-------------------

The eturnal server is started (and controlled) by calling the `eturnalctl`
script, which is a small wrapper around the `eturnal` command. The latter
expects a few files and directories to be available in the following locations:

- The `bin` directory the `eturnal` command itself is installed to (see the
  output of `ls -R _build/distro/rel/eturnal/bin`).

- The `bin/../releases` directory (see the output of
  `ls -R _build/distro/rel/eturnal/releases`).

Therefore, distributions might want to install the directories
`_build/distro/rel/eturnal/bin` and `_build/distro/rel/eturnal/releases` into
(for example) some private `/usr/lib/eturnal` directory and adjust the path to
the `eturnal` command within the `eturnalctl` script accordingly. The
`eturnalctl` script itself can then be installed elsewhere (e.g., into
`/usr/sbin`).

Note that the [build.config][6] file can be patched before calling `./rebar3` in
order to specify a different system user for running `eturnal`, and for
specifing a different path to the `eturnalctl` script. Those `build.config`
settings are (only) used while generating the `eturnalctl` script, the systemd
unit file, and the init script shipped with eturnal. The prefix to eturnal's
`etc` directory can also be specified in the `build.config` file. If the
`eturnal.yml` file should be placed into some directory _not_ called `etc`,
(e.g., `/etc/eturnal/eturnal.yml`) the `file` option of the `conf` library must
be edited in the `sys.config` file located in the
`_build/prod/rel/eturnal/releases/$version` directory after building eturnal.

[1]: https://erlang.org/doc/design_principles/release_structure.html
[2]: https://www.rebar3.org
[3]: https://erlware.github.io/relx/
[4]: https://github.com/processone/eturnal/blob/master/README.md
[5]: https://github.com/processone/eturnal/blob/master/rebar.config
[6]: https://github.com/processone/eturnal/blob/master/build.config
