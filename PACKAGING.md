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

Besides the dependencies [mentioned][4] in the [README.md][5] file, eturnal
depends on the Erlang libraries referenced as `deps` in the [rebar.config][6]
file, _except_ for `recon` (which is totally optional and just included with
source builds as a convenience for debugging purposes). The complete list of
(direct and transitive) Erlang dependencies is:

- [stun](https://github.com/processone/stun)
- [conf](https://github.com/processone/conf)
- [yval](https://github.com/processone/yval)
- [fast\_yaml](https://github.com/processone/fast_yaml)
- [fast\_tls](https://github.com/processone/fast_tls)
- [p1\_utils](https://github.com/processone/p1_utils)

Assuming those dependencies are installed into the distribution's Erlang/OTP
tree, [rebar3][2]'s built-in dependency management can be skipped by building
eturnal (from the [official source tarball][7]) as follows:

    $ rm -f rebar.lock
    $ SKIP_DEPS=true ./rebar3 as distro release

This will yield a `_build/distro/rel/eturnal` directory containing the eturnal
files to be installed.

Directory Structure
-------------------

The eturnal server is started (and controlled) by calling the [eturnalctl][8]
script, which is a small wrapper around the `eturnal` command. The latter
expects a few files and directories to be available in the following locations:

- The `bin` directory the `eturnal` command itself is installed to (see
  `ls -R _build/distro/rel/eturnal/bin`).
- The `bin/../releases` directory (see
  `ls -R _build/distro/rel/eturnal/releases`).

Therefore, distributions might want to install the directories
`_build/distro/rel/eturnal/bin` and `_build/distro/rel/eturnal/releases` into
(for example) some private `/usr/lib/eturnal` directory and adjust the path to
the `eturnal` command within the [eturnalctl][8] script accordingly. The
[eturnalctl][8] script itself can then be installed elsewhere (e.g., into
`/usr/sbin`).

Note that the [build.config][9] file can be patched before calling `./rebar3` in
order to specify a different system user for running `eturnal`, and for
specifing a different path to the [eturnalctl][8] script. Those
[build.config][9] settings are (only) used while generating the [eturnalctl][8]
script, the [systemd unit file][10], and the [init script][11] shipped with
eturnal. The prefix to eturnal's `etc` directory can also be specified in the
[build.config][9] file. If the [eturnal.yml][12] file should be placed into some
(sub)directory _not_ called `etc` (e.g., `/etc/eturnal/eturnal.yml`), the `file`
option of the `conf` library must be edited in the [sys.config][13] file located
in the `_build/prod/rel/eturnal/releases/$version` directory after building
eturnal. As an alternative, `-conf file '"/path/to/eturnal.yml"'` (note the
double quoting) can be appended to the `eturnalctl` command line used for
starting up eturnal.

 [1]: https://erlang.org/doc/design_principles/release_structure.html
 [2]: https://www.rebar3.org
 [3]: https://erlware.github.io/relx/
 [4]: https://github.com/processone/eturnal/blob/master/README.md#requirements
 [5]: https://github.com/processone/eturnal/blob/master/README.md
 [6]: https://github.com/processone/eturnal/blob/master/rebar.config
 [7]: https://eturnal.net/download/
 [8]: https://github.com/processone/eturnal/blob/master/scripts/eturnalctl
 [9]: https://github.com/processone/eturnal/blob/master/build.config
[10]: https://github.com/processone/eturnal/blob/master/config/eturnal.service
[11]: https://github.com/processone/eturnal/blob/master/scripts/eturnal.init
[12]: https://github.com/processone/eturnal/blob/master/config/eturnal.yml
[13]: https://github.com/processone/eturnal/blob/master/config/sys.config
